"""Grade the fixed 12-model cap x counts x opponent-relative experiment.

Usage: python cap_relative_report.py OUTPUT_DIRECTORY
Reuses the existing cohort/ATS grader without changing that source file.
"""

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

import numpy as np
import margin_cap_report as grading


CAPS = ("uncapped", "cap_24", "cap_42")
MODELS = tuple(cap + suffix for cap in CAPS for suffix in
               ("", "_relative", "_no_counts", "_no_counts_relative"))
PRIMARY = tuple((cap + suffix, "uncapped" + suffix)
                for suffix in ("_relative", "_no_counts_relative") for cap in CAPS[1:])
CAP_ONLY = tuple((cap + suffix, "uncapped" + suffix)
                 for suffix in ("", "_no_counts") for cap in CAPS[1:])
RELATIVE = tuple((cap + suffix + "_relative", cap + suffix)
                 for suffix in ("", "_no_counts") for cap in CAPS)
COMBINED = tuple((candidate, "uncapped") for candidate, _ in RELATIVE)
CONTRASTS = tuple(dict.fromkeys(PRIMARY + CAP_ONLY + RELATIVE + COMBINED))


def read_variants(path):
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        required = {"model", "cap", "count_predictors", "relative_feature", "feature_count"}
        if not required <= set(reader.fieldnames or ()):
            raise ValueError("variants.csv is missing required columns")
        rows = list(reader)
    if len(rows) != len(MODELS) or {row["model"] for row in rows} != set(MODELS):
        raise ValueError("Expected each of the 12 model variants exactly once")
    for row in rows:
        name = row["model"]
        counts = "_no_counts" not in name
        relative = name.endswith("_relative")
        expected_cap = "uncapped" if name.startswith("uncapped") else name.split("_")[1]
        actual_cap = row["cap"].strip().lower()
        if expected_cap == "uncapped":
            if actual_cap not in ("uncapped", "none", "inf", "infinity"):
                raise ValueError(f"Unexpected uncapped encoding for {name}: {actual_cap}")
        elif float(actual_cap) != float(expected_cap):
            raise ValueError(f"Cap disagrees with model label: {name}")
        if grading.boolean(row["count_predictors"]) != counts or grading.boolean(row["relative_feature"]) != relative:
            raise ValueError(f"Feature flags disagree with model label: {name}")
        if float(row["feature_count"]) != 18 + 2 * counts + relative:
            raise ValueError(f"Unexpected feature count for {name}")
    return rows


def counts_label(model):
    return "Removed" if "_no_counts" in model else "Kept"


def cap_label(model):
    return "None" if model.startswith("uncapped") else model.split("_")[1]


def paired(candidate, reference, data, name, mask, primary=False):
    row = grading.comparison(candidate, data[candidate], data[reference], name, mask)
    row["reference"] = reference
    row["primary_contrast"] = primary
    row["count_predictors"] = "_no_counts" not in candidate
    row["relative_feature"] = candidate.endswith("_relative")
    return row


def interaction(cap, counts, data, name, mask):
    suffix = "" if counts else "_no_counts"
    capped = data[cap + suffix]
    capped_relative = data[cap + suffix + "_relative"]
    control = data["uncapped" + suffix]
    control_relative = data["uncapped" + suffix + "_relative"]
    off = capped["absolute_error"][mask] - control["absolute_error"][mask]
    on = capped_relative["absolute_error"][mask] - control_relative["absolute_error"][mask]
    difference = on - off
    seasons = control["season"][mask]
    low, high, _, _ = grading.cluster_intervals(difference, seasons)
    return dict(cap=cap_label(cap), count_predictors=counts, slice=name, games=int(mask.sum()),
                season_clusters=len(np.unique(seasons)), cap_effect_without_relative=grading.mean(off),
                cap_effect_with_relative=grading.mean(on), interaction_mae_change=grading.mean(difference),
                interaction_ci95_low=low, interaction_ci95_high=high,
                interpretation="Negative means the relative feature makes the cap effect more favorable; descriptive interaction.")


def main(directory):
    directory = directory.resolve(strict=True)
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8-sig"))
    if manifest.get("status") != "fitting_complete" or manifest.get("source_inputs_unchanged") is not True:
        raise ValueError("Experiment must be complete with unchanged protected inputs")
    if "control_replay_max_error" in manifest:
        value = float(manifest["control_replay_max_error"])
        if not math.isfinite(value) or value > 1e-8:
            raise ValueError("Control replay failed")
    read_variants(directory / "variants.csv")
    grading.MODELS = MODELS
    data, input_count, excluded = grading.load_predictions(directory / "predictions.csv")
    cohorts = grading.slices(data["uncapped"])
    for year in (2022, 2023, 2024, 2025):
        if not cohorts[f"season_{year}_week_5_plus"].any():
            raise ValueError(f"Missing primary games for {year}")
    metrics = [grading.metrics(model, data[model], name, mask)
               for name, mask in cohorts.items() for model in MODELS]
    comparisons = [paired(candidate, reference, data, name, mask, (candidate, reference) in PRIMARY)
                   for name, mask in cohorts.items() for candidate, reference in CONTRASTS]
    omissions = []
    for candidate, reference in PRIMARY:
        mask = cohorts["week_5_plus"]
        for year in np.unique(data[reference]["season"][mask]):
            row = paired(candidate, reference, data, "week_5_plus",
                         mask & (data[reference]["season"] != year), True)
            row["omitted_season"] = int(year)
            omissions.append(row)
    interactions = [interaction(cap, counts, data, name, mask)
                    for name, mask in cohorts.items() for counts in (True, False) for cap in CAPS[1:]]
    for filename, rows in (("metrics.csv", metrics), ("paired_comparisons.csv", comparisons),
                           ("leave_one_season_out.csv", omissions), ("interactions.csv", interactions)):
        grading.write_csv(directory / filename, rows)
    metric = {(row["model"], row["slice"]): row for row in metrics}
    pair = {(row["model"], row["reference"], row["slice"]): row for row in comparisons}
    development = "development_2022_2024_week_5_plus"
    confirmation = "confirmation_2025_week_5_plus"
    selections = []
    for counts in (True, False):
        suffix = "_relative" if counts else "_no_counts_relative"
        # min preserves the preregistered tie preference: uncapped, then 24, then 42.
        selected = min((cap + suffix for cap in CAPS), key=lambda model: metric[model, development]["margin_mae"])
        reference = "uncapped" + suffix
        selections.append(dict(count_predictors=counts, selected_model=selected,
                               selected_cap=cap_label(selected), reference=reference,
                               development_mae=metric[selected, development]["margin_mae"],
                               development_mae_change=metric[selected, development]["margin_mae"]-metric[reference, development]["margin_mae"],
                               year_2025_mae=metric[selected, confirmation]["margin_mae"],
                               year_2025_mae_change=metric[selected, confirmation]["margin_mae"]-metric[reference, confirmation]["margin_mae"],
                               tie_preference="uncapped, 24, 42"))
    grading.write_csv(directory / "development_selection.csv", selections)
    fmt, pct = grading.fmt, grading.pct
    primary = "week_5_plus"
    lines = ["# Margin caps with opponent-relative performance", "",
             "Twelve matched ridge fits test caps of none, 24 and 42 points, with direct count predictors "
             "kept or removed, and the opponent-relative feature off or on. Primary scoring uses "
             "regular-season Week 5+ FBS-vs-FBS games in 2022–2025.", "",
             "The cap changes only the power fit and its dependent coaching inputs. "
             "The opponent-relative feature retains its original raw, uncapped margins in every variant.", ""]

    def add_results(relative):
        lines.extend(["## " + ("With the opponent-relative feature" if relative else "Matched controls without the relative feature"), "",
                      "| Direct counts | Power cap | MAE | RMSE | Straight up | ATS W–L–P | ATS | Δ MAE vs same-count uncapped |",
                      "|---|---|---:|---:|---:|---:|---:|---:|"])
        for counts in (True, False):
            suffix = ("" if counts else "_no_counts") + ("_relative" if relative else "")
            control = metric["uncapped" + suffix, primary]
            for cap in CAPS:
                model = cap + suffix
                row = metric[model, primary]
                delta = "—" if cap == "uncapped" else fmt(row["margin_mae"] - control["margin_mae"])
                lines.append(f"| {counts_label(model)} | {cap_label(model)} | {fmt(row['margin_mae'])} | "
                             f"{fmt(row['margin_rmse'])} | {pct(row['su_accuracy'])} | "
                             f"{row['wins']}–{row['losses']}–{row['pushes']} | {pct(row['ats_accuracy'])} | {delta} |")
        lines.append("")

    add_results(True)
    add_results(False)
    lines += ["## Four primary cap comparisons", "",
              f"Each comparison holds count predictors and the relative feature fixed. All use "
              f"{metric['uncapped', primary]['games']:,} identical games. Negative MAE change favors the cap.", "",
              "| Counts | Cap | Δ MAE | 95% season interval | 98.75% adjusted interval | Δ SU (pp) | Paired Δ ATS (pp) |",
              "|---|---|---:|---|---|---:|---:|"]
    for candidate, reference in PRIMARY:
        row = pair[candidate, reference, primary]
        lines.append(f"| {counts_label(candidate)} | {cap_label(candidate)} | {fmt(row['mae_change'])} | "
                     f"[{fmt(row['mae_ci95_low'])}, {fmt(row['mae_ci95_high'])}] | "
                     f"[{fmt(row['mae_ci9875_low'])}, {fmt(row['mae_ci9875_high'])}] | "
                     f"{fmt(row['su_change_pp'], 2)} | {fmt(row['ats_paired_change_pp'], 2)} |")
    lines += ["", "## Development and 2025", "",
              "| Counts | Cap | 2022 Δ MAE | 2023 | 2024 | 2025 | 2022–2024 development |",
              "|---|---|---:|---:|---:|---:|---:|"]
    for candidate, reference in PRIMARY:
        years = [pair[candidate, reference, f"season_{year}_week_5_plus"]["mae_change"] for year in (2022, 2023, 2024, 2025)]
        dev = pair[candidate, reference, "development_2022_2024_week_5_plus"]["mae_change"]
        lines.append(f"| {counts_label(candidate)} | {cap_label(candidate)} | " +
                     " | ".join(fmt(value) for value in years) + f" | {fmt(dev)} |")
    lines += ["", "Selection uses development MAE only, separately within each count setting, "
              "with the relative feature present. Ties prefer uncapped, then 24, then 42.", ""]
    for row in selections:
        outcome = ("No cap beats the uncapped development baseline."
                   if row["selected_cap"] == "None" else
                   f"Its separate 2025 Δ MAE is {fmt(row['year_2025_mae_change'])} versus the matched uncapped model.")
        lines.append(f"- Counts {'kept' if row['count_predictors'] else 'removed'}: "
                     f"**{'uncapped' if row['selected_cap'] == 'None' else 'cap ' + row['selected_cap']}** "
                     f"has the lowest development MAE, {fmt(row['development_mae'])}. {outcome}")
    lines += ["", "Interpretation:", ""]
    for candidate, reference in PRIMARY:
        dev = pair[candidate, reference, "development_2022_2024_week_5_plus"]["mae_change"]
        last = pair[candidate, reference, "confirmation_2025_week_5_plus"]["mae_change"]
        full = pair[candidate, reference, primary]
        loso = [row["mae_change"] for row in omissions if row["model"] == candidate]
        if dev < 0 and last < 0:
            verdict = "The development improvement persists in 2025; this remains exploratory evidence."
        elif dev < 0:
            verdict = "The development improvement reverses in 2025."
        elif last < 0:
            verdict = "Only the separate 2025 result improves; development does not."
        else:
            verdict = "Neither development nor 2025 improves MAE."
        lines.append(f"- Counts {counts_label(candidate).lower()}, cap {cap_label(candidate)}: "
                     f"overall Δ MAE {fmt(full['mae_change'])}. {verdict} "
                     f"The MAE change stays negative under {sum(value < 0 for value in loso)}/{len(loso)} one-season omissions.")
    lines += ["", "## Does the relative feature change the cap's effect?", "",
              "Interaction = cap effect with the relative feature minus cap effect without it, "
              "calculated on paired per-game absolute errors. Negative values mean the relative feature "
              "makes capping more favorable; this does not by itself mean the capped model improves on its control.", "",
              "| Counts | Cap | Cap effect without relative | Cap effect with relative | Interaction Δ MAE | Descriptive 95% interval |",
              "|---|---|---:|---:|---:|---|"]
    for row in interactions:
        if row["slice"] != primary:
            continue
        lines.append(f"| {'Kept' if row['count_predictors'] else 'Removed'} | {row['cap']} | "
                     f"{fmt(row['cap_effect_without_relative'])} | {fmt(row['cap_effect_with_relative'])} | "
                     f"{fmt(row['interaction_mae_change'])} | "
                     f"[{fmt(row['interaction_ci95_low'])}, {fmt(row['interaction_ci95_high'])}] |")
    lines += ["", "## Secondary all-FBS comparison", "",
              "| Counts | Cap | With relative Δ MAE | Without relative Δ MAE |",
              "|---|---|---:|---:|"]
    for candidate, reference in PRIMARY:
        a = pair[candidate, reference, "all_fbs"]
        b = pair[candidate.removesuffix("_relative"), reference.removesuffix("_relative"), "all_fbs"]
        lines.append(f"| {counts_label(candidate)} | {cap_label(candidate)} | {fmt(a['mae_change'])} | {fmt(b['mae_change'])} |")
    lines += ["", "## Secondary incremental and combined effects", "",
              "The feature-addition comparison holds cap and count setting fixed. The combined comparison "
              "uses the original published control, so it may include both count removal and capping.", "",
              "| Counts | Cap | Adding relative: Δ MAE | Δ SU (pp) | Paired Δ ATS (pp) | Combined vs published: Δ MAE |",
              "|---|---|---:|---:|---:|---:|"]
    for candidate, reference in RELATIVE:
        row = pair[candidate, reference, primary]
        combined = pair[candidate, "uncapped", primary]
        lines.append(f"| {counts_label(candidate)} | {cap_label(candidate)} | {fmt(row['mae_change'])} | "
                     f"{fmt(row['su_change_pp'], 2)} | {fmt(row['ats_paired_change_pp'], 2)} | "
                     f"{fmt(combined['mae_change'])} |")
    lines += ["", "## Method and limits", "",
              f"- Exact IDs and scoring metadata matched for all {input_count:,} input predictions per model; "
              f"{excluded:,} non-FBS or non-CFP-bowl games were excluded. All-FBS scoring retains "
              f"{metric['uncapped', 'all_fbs']['games']:,} games. Detailed slices are in `metrics.csv`.",
              "- The same prior-season folds, fixed ridge penalties, coaching shares, raw targets, weights and closing lines are used throughout. There is no additional tuning or search over shrinkage.",
              "- All variants are refitted. The cap is applied to observed power-fit margins before home-field subtraction; dependent coaching inputs are recalculated. It does not clip EPA, actual outcomes, regression targets or the new relative feature.",
              "- Opponent-relative performance uses strictly earlier same-season information and the original fixed n/(n+3) shrinkage. Its frozen feature is identical across caps and count settings. Count removal refers only to the two direct predictors, not removal of sample-size weighting.",
              "- Missing-line games remain in MAE/RMSE/SU. ATS accuracy is wins/(wins+losses); pushes and exactly zero-edge passes are separate CSV counts. Paired ATS differences use the same non-push selections for both models. Saved closing lines do not represent Friday execution or ROI.",
              "- Whole-season paired bootstrap: 10,000 draws, seed 20260927. Primary 98.75% MAE intervals account for four cap comparisons. ATS, control comparisons, interactions and additional slices are descriptive secondary results.",
              "- Only four season clusters exist, so the intervals are fragile. The development seasons and 2025 have all appeared in prior research; 2025 is not a pristine holdout. Small changes or a bootstrap interval alone do not establish a reliable betting edge.",
              "- This report makes no production change, ranking refresh or automatic promotion.", "",
              "Artifacts: `variants.csv`, `manifest.json`, `predictions.csv`, `metrics.csv`, "
              "`paired_comparisons.csv`, `leave_one_season_out.csv`, `development_selection.csv`, and `interactions.csv`.", ""]
    for name in ("predictions.csv", "variants.csv", "manifest.json", "fold_choices.csv"):
        path = directory / name
        if not path.is_file():
            raise ValueError(f"Required provenance input missing: {name}")
        lines.append(f"- SHA-256 `{name}`: `{hashlib.sha256(path.read_bytes()).hexdigest()}`")
    output = directory / "report.md"
    output.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(output)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    main(parser.parse_args().output_directory)
