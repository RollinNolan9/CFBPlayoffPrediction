"""Grade the fixed opponent-relative feature experiment using the cap report's shared grader."""
import argparse
import hashlib
import json
from pathlib import Path

import numpy as np
import margin_cap_report as grading


MODELS = ("uncapped", "opponent_relative", "no_counts", "no_counts_opponent_relative")
LABELS = {"uncapped": "Published control", "opponent_relative": "Control + opponent-relative",
          "no_counts": "No direct count predictors", "no_counts_opponent_relative": "No counts + opponent-relative"}
CONTRASTS = (("opponent_relative", "uncapped", True),
             ("no_counts_opponent_relative", "no_counts", True),
             ("no_counts", "uncapped", False),
             ("no_counts_opponent_relative", "uncapped", False))


def adjusted_mae_interval(candidate, control, mask):
    delta = candidate["absolute_error"][mask] - control["absolute_error"][mask]
    seasons = candidate["season"][mask]
    years = np.unique(seasons)
    if len(years) < 2:
        return float("nan"), float("nan")
    sums = np.array([delta[seasons == year].sum() for year in years])
    counts = np.array([(seasons == year).sum() for year in years])
    draws = np.random.default_rng(grading.SEED).integers(0, len(years), (grading.DRAWS, len(years)))
    values = sums[draws].sum(axis=1) / counts[draws].sum(axis=1)
    return tuple(float(value) for value in np.quantile(values, [.0125, .9875]))


def main(directory):
    directory = directory.resolve(strict=True)
    manifest = json.loads((directory / "manifest.json").read_text(encoding="utf-8"))
    if manifest["status"] != "fitting_complete" or not manifest["source_inputs_unchanged"]:
        raise ValueError("Experiment is incomplete or protected inputs changed")
    if manifest["control_replay_max_error"] > 1e-8:
        raise ValueError("Control replay failed")
    # Reuse exact cohort/ATS semantics without editing the earlier experiment.
    grading.MODELS = MODELS
    data, input_count, excluded = grading.load_predictions(directory / "predictions.csv")
    cohorts = grading.slices(data["uncapped"])
    metrics = [grading.metrics(model, data[model], name, mask)
               for name, mask in cohorts.items() for model in MODELS]
    comparisons = []
    for name, mask in cohorts.items():
        for candidate, reference, primary in CONTRASTS:
            row = grading.comparison(candidate, data[candidate], data[reference], name, mask)
            row["reference"] = reference
            row["primary_contrast"] = primary
            row["mae_ci975_low"], row["mae_ci975_high"] = adjusted_mae_interval(data[candidate], data[reference], mask)
            comparisons.append(row)
    omissions = []
    for name in ("week_5_plus", "development_2022_2024_week_5_plus"):
        mask = cohorts[name]
        for year in np.unique(data["uncapped"]["season"][mask]):
            for candidate, reference, primary in CONTRASTS[:2]:
                reduced = mask & (data["uncapped"]["season"] != year)
                row = grading.comparison(candidate, data[candidate], data[reference], name, reduced)
                row["reference"] = reference
                row["omitted_season"] = int(year)
                omissions.append(row)
    grading.write_csv(directory / "metrics.csv", metrics)
    grading.write_csv(directory / "paired_comparisons.csv", comparisons)
    grading.write_csv(directory / "leave_one_season_out.csv", omissions)
    metric = {(row["model"], row["slice"]): row for row in metrics}
    pair = {(row["model"], row["reference"], row["slice"]): row for row in comparisons}
    fmt, pct = grading.fmt, grading.pct
    primary = "week_5_plus"
    lines = ["# Opponent-relative margin experiment", "",
        "Relative performance = a team's actual margin plus its opponent's prior average signed margin. "
        "For example, winning by 14 against an opponent that previously lost by 3 on average is +11. "
        "Every opponent baseline is frozen before that game. The team feature averages earlier usable "
        "performances in the same season and shrinks by n/(n+3).", "",
        "All variants use uncapped power inputs. Primary cohort: regular-season Week 5+ FBS games, "
        "2022-2025. Earlier-season training, original final-score targets and identical frozen closing lines.", "",
        "## Primary results", "",
        "| Model | Games | MAE | RMSE | Straight up | ATS W-L-P | ATS |",
        "|---|---:|---:|---:|---:|---:|---:|"]
    for model in MODELS:
        row = metric[model, primary]
        lines.append(f"| {LABELS[model]} | {row['games']} | {fmt(row['margin_mae'])} | {fmt(row['margin_rmse'])} | "
                     f"{pct(row['su_accuracy'])} | {row['wins']}-{row['losses']}-{row['pushes']} | {pct(row['ats_accuracy'])} |")
    lines += ["", "## Does the new feature add value?", "",
        "Negative MAE changes favor the added feature. These two paired comparisons isolate its effect.", "",
        "| Base model | Added-feature MAE change | 95% season interval | 97.5% adjusted interval | SU change (pp) | ATS change (pp) |",
        "|---|---:|---|---|---:|---:|"]
    for candidate, reference, _ in CONTRASTS[:2]:
        row = pair[candidate, reference, primary]
        lines.append(f"| {LABELS[reference]} | {fmt(row['mae_change'])} | [{fmt(row['mae_ci95_low'])}, {fmt(row['mae_ci95_high'])}] | "
                     f"[{fmt(row['mae_ci975_low'])}, {fmt(row['mae_ci975_high'])}] | {fmt(row['su_change_pp'], 2)} | {fmt(row['ats_paired_change_pp'], 2)} |")
    lines += ["", "## Season-by-season incremental MAE", "",
        "| Base model | 2022 | 2023 | 2024 | 2025 | 2022-2024 development |",
        "|---|---:|---:|---:|---:|---:|"]
    for candidate, reference, _ in CONTRASTS[:2]:
        values = [pair[candidate, reference, f"season_{year}_week_5_plus"]["mae_change"] for year in (2022, 2023, 2024, 2025)]
        dev = pair[candidate, reference, "development_2022_2024_week_5_plus"]["mae_change"]
        lines.append(f"| {LABELS[reference]} | " + " | ".join(fmt(value) for value in values) + f" | {fmt(dev)} |")
    lines += ["", "## Interpretation", ""]
    for candidate, reference, _ in CONTRASTS[:2]:
        full = pair[candidate, reference, primary]
        dev = pair[candidate, reference, "development_2022_2024_week_5_plus"]
        last = pair[candidate, reference, "confirmation_2025_week_5_plus"]
        if dev["mae_change"] < 0 and last["mae_change"] < 0:
            verdict = "The development MAE improvement also appears in 2025; this is a candidate for further validation."
        elif dev["mae_change"] < 0:
            verdict = "The development MAE improvement reverses in 2025; this does not support promotion."
        else:
            verdict = "It does not improve development MAE; these results do not support adding it on this basis."
        lines.append(f"- **{LABELS[reference]}:** overall MAE change {fmt(full['mae_change'])}; "
                     f"2025 change {fmt(last['mae_change'])}. {verdict}")
    lines += ["", "## Secondary cohorts", "",
        "| Cohort | Games | Control MAE | + relative | No counts MAE | No counts + relative |",
        "|---|---:|---:|---:|---:|---:|"]
    for name in ("all_fbs", "early_weeks_0_4", "postseason", "spread_over_21"):
        lines.append(f"| {name} | {metric['uncapped',name]['games']} | " +
                     " | ".join(fmt(metric[model,name]['margin_mae']) for model in MODELS) + " |")
    lines += ["", "## Method and limits", "",
        f"- All {input_count} prediction IDs and scoring fields matched across variants before filtering; {excluded} were excluded under the existing FBS/non-CFP-bowl rule.",
        f"- Published control replay maximum difference: {manifest['control_replay_max_error']:.3g} points.",
        "- No full-season opponent averages, target-game outcomes or future results enter the new predictor. Zero usable history remains a neutral zero and does not remove a game from evaluation.",
        "- The feature uses current-season FBS-vs-FBS results, without venue adjustment. It omits games whose opponent has no prior eligible results. The fixed three-pseudogame shrinkage tempers small samples but is not a fitted optimal choice.",
        "- Removing direct count predictors does not eliminate sample-size weighting. The new feature uses its count only for shrinkage, not as a separate predictor.",
        "- The existing power model already adjusts for opponents, and coaching features use above-expectation results. This is an incremental-value test against those existing predictors.",
        "- MAE/RMSE/SU include missing-line games. ATS uses saved closing lines, zero-edge passes and pushes exactly as the original grader; accuracy is wins/(wins+losses). Paired ATS uses common non-push decisions. No Friday execution or ROI claim is made.",
        "- Whole-season bootstrap: 10,000 paired draws, seed 20260927. 97.5% MAE intervals account for the two primary feature-addition comparisons; secondary tests remain exploratory. The shared CSV also retains the earlier helper's more conservative 98.75% descriptive intervals.",
        "- Only four season clusters exist. These intervals are fragile, and 2025 has been examined in earlier research. This is not pristine holdout validation.",
        "- Same frozen ridge penalties, coaching shares and training weights for every variant. No hyperparameter search, production change or ranking promotion.",
        "", "Artifacts: `PROTOCOL.md`, `manifest.json`, `feature_qa.json`, `opponent_relative_features.csv`, "
        "`predictions.csv`, `metrics.csv`, `paired_comparisons.csv`, and `leave_one_season_out.csv`.", ""]
    for name in ("predictions.csv", "manifest.json", "fold_choices.csv"):
        lines.append(f"- SHA-256 `{name}`: `{hashlib.sha256((directory/name).read_bytes()).hexdigest()}`")
    output = directory / "report.md"
    output.write_text("\n".join(lines)+"\n",encoding="utf-8")
    print(output)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory",type=Path)
    main(parser.parse_args().output_directory)
