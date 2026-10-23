"""Report frozen chronological margin-cap predictions; never fit or promote a model.

Usage: python margin_cap_report.py OUTPUT_DIRECTORY
Inputs: predictions.csv, fold_choices.csv, manifest.json. Outputs stay in that directory.
"""

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

import numpy as np


MODELS = ("uncapped", "cap_24", "cap_25", "cap_35", "cap_42")
NUMERIC = ("season", "week", "actual_margin", "expected_margin", "closing_home_spread")
BOOLEAN = ("is_cfp", "neutral_site")
METADATA = ("game_id", "season", "week", "postseason_type", "home_level", "away_level",
            "home", "away", "actual_margin", "closing_home_spread", "is_cfp", "neutral_site")
REQUIRED = {"model", "expected_margin", *METADATA}
SEED = 20260927
DRAWS = 10000


def number(value):
    return math.nan if value.strip().lower() in ("", "na", "nan", "null", "n/a") else float(value)


def boolean(value):
    text = value.strip().lower()
    if text in ("true", "t", "1"):
        return True
    if text in ("false", "f", "0", "", "na", "null"):
        return False
    raise ValueError(f"Unrecognized boolean: {value!r}")


def equal_metadata(a, b):
    if isinstance(a, float) and isinstance(b, float):
        return (math.isnan(a) and math.isnan(b)) or a == b
    return a == b


def load_predictions(path):
    groups = {model: {} for model in MODELS}
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        missing = REQUIRED - set(reader.fieldnames or ())
        if missing:
            raise ValueError(f"Missing prediction columns: {sorted(missing)}")
        for line, source in enumerate(reader, 2):
            row = {key: source[key].strip() for key in REQUIRED}
            for key in NUMERIC:
                row[key] = number(row[key])
            for key in BOOLEAN:
                row[key] = boolean(row[key])
            model = row["model"]
            if model not in groups:
                raise ValueError(f"Unexpected model {model!r} at line {line}")
            if not row["game_id"] or row["game_id"] in groups[model]:
                raise ValueError(f"Empty or duplicate game ID for {model} at line {line}")
            if not all(math.isfinite(row[key]) for key in ("season", "week", "actual_margin", "expected_margin")):
                raise ValueError(f"Nonfinite prediction/target/time at line {line}")
            if row["season"] not in (2022, 2023, 2024, 2025) or row["week"] != int(row["week"]):
                raise ValueError(f"Unexpected evaluation season or week at line {line}")
            if math.isinf(row["closing_home_spread"]):
                raise ValueError(f"Infinite closing line at line {line}")
            groups[model][row["game_id"]] = row
    reference = groups["uncapped"]
    if not reference:
        raise ValueError("No uncapped control predictions")
    for model in MODELS[1:]:
        if set(groups[model]) != set(reference):
            raise ValueError(f"{model} game IDs differ from uncapped; no intersection-only scoring allowed")
        for game_id, row in reference.items():
            for field in METADATA:
                if not equal_metadata(row[field], groups[model][game_id][field]):
                    raise ValueError(f"Scoring metadata mismatch: {model}, {game_id}, {field}")
    ids = sorted(reference, key=lambda game_id: (reference[game_id]["season"], reference[game_id]["week"], game_id))
    keep = [game_id for game_id in ids if eligible(reference[game_id])]
    if not keep:
        raise ValueError("No eligible FBS evaluation games")
    data = {}
    for model in MODELS:
        rows = [groups[model][game_id] for game_id in keep]
        data[model] = {key: np.asarray([row[key] for row in rows]) for key in REQUIRED}
        grade(data[model])
    return data, len(ids), len(ids) - len(keep)


def eligible(row):
    """Mirror raw_history_eligible: FBS/FBS, excluding non-CFP bowls."""
    return (row["home_level"].lower() == "fbs" and row["away_level"].lower() == "fbs"
            and not (row["postseason_type"].lower() in ("bowl", "non_cfp_bowl") and not row["is_cfp"]))


def grade(data):
    error = data["expected_margin"] - data["actual_margin"]
    edge = data["expected_margin"] + data["closing_home_spread"]
    result = data["actual_margin"] + data["closing_home_spread"]
    lined = np.isfinite(data["closing_home_spread"])
    selected = lined & (edge != 0)
    push = selected & (result == 0)
    win = selected & ~push & (np.sign(edge) == np.sign(result))
    loss = selected & ~push & ~win
    data.update(error=error, absolute_error=np.abs(error), lined=lined, selected=selected,
                push=push, win=win, loss=loss, zero_edge_pass=lined & (edge == 0),
                su_correct=np.sign(data["actual_margin"]) == np.sign(data["expected_margin"]),
                ats_correct=np.where(win, 1.0, np.where(loss, 0.0, np.nan)))


def slices(reference):
    regular = reference["postseason_type"] == "regular"
    primary = regular & (reference["week"] >= 5)
    result = {
        "week_5_plus": primary,
        "all_fbs": np.ones(len(primary), dtype=bool),
        "early_weeks_0_4": regular & (reference["week"] <= 4),
        "postseason": ~regular,
        "cfp": reference["is_cfp"],
        "spread_over_21": np.abs(reference["closing_home_spread"]) > 21,
        "development_2022_2024_week_5_plus": primary & (reference["season"] <= 2024),
        "development_2022_2024_all_fbs": reference["season"] <= 2024,
        "confirmation_2025_week_5_plus": primary & (reference["season"] == 2025),
        "confirmation_2025_all_fbs": reference["season"] == 2025,
    }
    for season in (2022, 2023, 2024, 2025):
        result[f"season_{season}_all_fbs"] = reference["season"] == season
        result[f"season_{season}_week_5_plus"] = primary & (reference["season"] == season)
    return result


def mean(values):
    return float(np.mean(values)) if len(values) else math.nan


def metrics(model, data, name, mask):
    n = int(mask.sum())
    count = lambda key: int(data[key][mask].sum())
    wins, losses, pushes = count("win"), count("loss"), count("push")
    error = data["error"][mask]
    return dict(model=model, slice=name, games=n, season_clusters=len(set(data["season"][mask])),
                margin_mae=mean(np.abs(error)), margin_rmse=math.sqrt(mean(error ** 2)),
                margin_bias=mean(error), su_wins=count("su_correct"), su_accuracy=mean(data["su_correct"][mask]),
                lined_games=count("lined"), missing_lines=n-count("lined"),
                wins=wins, losses=losses, pushes=pushes, zero_edge_passes=count("zero_edge_pass"),
                no_selection=n-count("selected"), ats_decisions=wins+losses,
                ats_accuracy=wins/(wins+losses) if wins+losses else math.nan)


def cluster_intervals(delta, seasons):
    """Resample whole seasons, preserving game counts and pairing within each draw."""
    keep = np.isfinite(delta)
    delta, seasons = delta[keep], seasons[keep]
    clusters = np.unique(seasons)
    if len(clusters) < 2:
        return (math.nan,) * 4
    sums = np.asarray([delta[seasons == season].sum() for season in clusters])
    counts = np.asarray([(seasons == season).sum() for season in clusters])
    draws = np.random.default_rng(SEED).integers(0, len(clusters), (DRAWS, len(clusters)))
    samples = sums[draws].sum(axis=1) / counts[draws].sum(axis=1)
    return tuple(float(x) for x in np.quantile(samples, [.025, .975, .00625, .99375]))


def comparison(model, data, control, name, mask):
    ae_delta = data["absolute_error"][mask] - control["absolute_error"][mask]
    su_delta = 100 * (data["su_correct"][mask].astype(float) - control["su_correct"][mask].astype(float))
    ats_delta = 100 * (data["ats_correct"][mask] - control["ats_correct"][mask])
    seasons = data["season"][mask]
    common = np.isfinite(ats_delta)
    a = metrics(model, data, name, mask)
    b = metrics("uncapped", control, name, mask)
    row = dict(model=model, reference="uncapped", slice=name, games=len(ae_delta),
               season_clusters=len(set(seasons)), control_mae=b["margin_mae"], candidate_mae=a["margin_mae"],
               mae_change=mean(ae_delta), su_change_pp=mean(su_delta),
               paired_ats_games=int(common.sum()), ats_paired_change_pp=mean(ats_delta[common]),
               ats_full_rate_change_pp=100*(a["ats_accuracy"]-b["ats_accuracy"]),
               ats_paired_control_accuracy=mean(control["ats_correct"][mask][common]),
               ats_paired_candidate_accuracy=mean(data["ats_correct"][mask][common]))
    for key, values in (("mae", ae_delta), ("su_pp", su_delta), ("ats_paired_pp", ats_delta)):
        bounds = cluster_intervals(values, seasons)
        for suffix, value in zip(("ci95_low", "ci95_high", "ci9875_low", "ci9875_high"), bounds):
            row[f"{key}_{suffix}"] = value
    return row


def write_csv(path, rows):
    if not rows:
        raise ValueError(f"No rows for {path.name}")
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        for row in rows:
            writer.writerow({key: "" if isinstance(value, float) and not math.isfinite(value) else value
                             for key, value in row.items()})


def fmt(value, digits=3):
    return f"{value:.{digits}f}" if math.isfinite(value) else "—"


def pct(value):
    return f"{100*value:.1f}%" if math.isfinite(value) else "—"


def report(directory, metric_rows, paired, omissions, input_count, excluded, input_hashes):
    metric = {(row["model"], row["slice"]): row for row in metric_rows}
    pair = {(row["model"], row["slice"]): row for row in paired}
    primary = "week_5_plus"
    development = "development_2022_2024_week_5_plus"
    confirmation = "confirmation_2025_week_5_plus"
    ordered = sorted(MODELS[1:], key=lambda model: (metric[model, development]["margin_mae"], int(model[4:])))
    selected = ordered[0]
    dev = pair[selected, development]
    test = pair[selected, confirmation]
    overall = pair[selected, primary]
    improvement = dev["mae_change"] < 0
    confirmation_improvement = test["mae_change"] < 0
    omitted_primary = [row for row in omissions if row["model"] == selected and row["slice"] == primary]
    consistent = all(row["mae_change"] < 0 for row in omitted_primary)
    lines = ["# Historical power-margin cap comparison", "",
             "Primary cohort: regular-season FBS-vs-FBS games in Week 5 or later. "
             "Final margins and grading targets remain uncapped. Negative MAE changes favor the cap.", "",
             "## Primary results", "",
             "| Model | Games | MAE | RMSE | SU | ATS W–L–P | ATS | Missing lines | Zero-edge passes |",
             "|---|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for model in MODELS:
        row = metric[model, primary]
        lines.append(f"| {model} | {row['games']} | {fmt(row['margin_mae'])} | {fmt(row['margin_rmse'])} | "
                     f"{pct(row['su_accuracy'])} | {row['wins']}–{row['losses']}–{row['pushes']} | "
                     f"{pct(row['ats_accuracy'])} | {row['missing_lines']} | {row['zero_edge_passes']} |")
    lines += ["", "| Cap vs uncapped | Δ MAE | Season bootstrap 95% | Adjusted 98.75% | Δ SU (pp) | Paired Δ ATS (pp) |",
              "|---|---:|---|---|---:|---:|"]
    for model in MODELS[1:]:
        row = pair[model, primary]
        lines.append(f"| {model} | {fmt(row['mae_change'])} | [{fmt(row['mae_ci95_low'])}, {fmt(row['mae_ci95_high'])}] | "
                     f"[{fmt(row['mae_ci9875_low'])}, {fmt(row['mae_ci9875_high'])}] | "
                     f"{fmt(row['su_change_pp'], 2)} | {fmt(row['ats_paired_change_pp'], 2)} |")
    lines += ["", "## Development choice and 2025 check", "",
              f"The lowest 2022–2024 Week 5+ MAE among the four caps belongs to **{selected}** "
              f"(deterministic tie-break: smaller cap). Its development Δ MAE is **{fmt(dev['mae_change'])}**, "
              f"and its separate 2025 Δ MAE is **{fmt(test['mae_change'])}**.", ""]
    if not improvement:
        lines.append("No cap beats the uncapped control on the development primary metric. The least-bad capped candidate is not a reason to replace the control.")
    elif not confirmation_improvement:
        lines.append("The development advantage does not persist in 2025. These results do not support replacing the control.")
    else:
        lines.append("The direction of the development MAE advantage persists in 2025. This is exploratory evidence, not a promotion decision or a reliable ATS-edge claim.")
    lines += ["", f"For {selected}, primary Δ MAE is {fmt(overall['mae_change'])} across 2022–2025; "
              f"it {'does' if consistent else 'does not'} remain negative under every one-season omission.", "",
              "| Model | 2022 Δ MAE | 2023 Δ MAE | 2024 Δ MAE | 2025 Δ MAE | 2022–2024 Δ MAE |",
              "|---|---:|---:|---:|---:|---:|"]
    for model in MODELS[1:]:
        changes = [fmt(pair[model, f"season_{year}_week_5_plus"]["mae_change"]) for year in (2022, 2023, 2024, 2025)]
        lines.append(f"| {model} | " + " | ".join(changes) + f" | {fmt(pair[model, development]['mae_change'])} |")
    lines += ["", "## Secondary cohorts", "",
              "| Cohort | Games | Uncapped MAE | cap_24 Δ MAE | cap_25 Δ MAE | cap_35 Δ MAE | cap_42 Δ MAE |",
              "|---|---:|---:|---:|---:|---:|---:|"]
    for name in ("all_fbs", "early_weeks_0_4", "postseason", "cfp", "spread_over_21"):
        row = metric["uncapped", name]
        lines.append(f"| {name} | {row['games']} | {fmt(row['margin_mae'])} | " +
                     " | ".join(fmt(pair[model, name]["mae_change"]) for model in MODELS[1:]) + " |")
    lines += ["", "## Interpretation and audit", "",
              f"- Exact cap/control game IDs and all scoring metadata matched before filtering. "
              f"There were {input_count} input games per model; {excluded} were excluded as FCS/non-FBS games or non-CFP bowls.",
              "- MAE, RMSE and SU retain games without a line. SU uses the sign of the predicted and actual margin, matching the existing experiment grader; a zero prediction does not count as a correct non-tied winner.",
              "- ATS selects the sign of predicted home margin plus saved closing home spread. An exactly zero edge is a pass; an exactly zero actual cover margin is a push only when a side was selected. ATS accuracy is wins / (wins + losses).",
              "- Each model's full ATS record is reported independently. Paired ATS changes and intervals use only the same games where both models made non-push selections. Full-rate changes, which can have different denominators, are separate CSV columns.",
              "- Historical closing lines are frozen across models. These are not Friday publication execution results or ROI estimates.",
              f"- Intervals resample whole seasons with replacement ({DRAWS:,} draws, seed {SEED}), preserving paired games and each season's game count. Adjusted 98.75% intervals account for four cap comparisons. Intervals with fewer than two season clusters are left blank.",
              "- Only four season clusters exist (three for development); the bootstrap is descriptive and fragile. Previously researched seasons, including 2025, are not pristine holdouts. Secondary slices and their intervals are exploratory and not multiplicity-adjusted across slices.",
              "- Foundation ridge only; the same frozen prior-only fold choices and training weights apply to every cap. Model fitting, chronology, baseline parity, and protected-file checks belong to the runner's manifest and validation artifacts; this reporting helper does not independently prove them.",
              "- No model is promoted and no production artifact is changed by this report.", "",
              "Artifacts: `metrics.csv`, `paired_comparisons.csv`, `leave_one_season_out.csv`, "
              "`predictions.csv`, `fold_choices.csv`, and `manifest.json`.", "",
              "Input SHA-256 hashes:", ""]
    for name, digest in input_hashes.items():
        lines.append(f"- `{name}`: `{digest}`")
    (directory / "report.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
    return selected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    args = parser.parse_args()
    directory = args.output_directory.resolve(strict=True)
    files = [directory / name for name in ("predictions.csv", "fold_choices.csv", "manifest.json")]
    for path in files:
        if not path.is_file():
            raise ValueError(f"Required input missing: {path}")
    # Confirm valid JSON without assuming the fitting runner's manifest schema.
    json.loads((directory / "manifest.json").read_text(encoding="utf-8-sig"))
    input_hashes = {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in files}
    data, input_count, excluded = load_predictions(directory / "predictions.csv")
    control = data["uncapped"]
    cohorts = slices(control)
    for year in (2022, 2023, 2024, 2025):
        if not cohorts[f"season_{year}_week_5_plus"].any():
            raise ValueError(f"Missing primary evaluation games in {year}")
    metric_rows = [metrics(model, data[model], name, mask)
                   for name, mask in cohorts.items() for model in MODELS]
    paired = [comparison(model, data[model], control, name, mask)
              for name, mask in cohorts.items() for model in MODELS[1:]]
    omissions = []
    for name in ("week_5_plus", "all_fbs", "development_2022_2024_week_5_plus"):
        mask = cohorts[name]
        for year in sorted(set(control["season"][mask])):
            for model in MODELS[1:]:
                row = comparison(model, data[model], control, name, mask & (control["season"] != year))
                row["omitted_season"] = int(year)
                omissions.append(row)
    write_csv(directory / "metrics.csv", metric_rows)
    write_csv(directory / "paired_comparisons.csv", paired)
    write_csv(directory / "leave_one_season_out.csv", omissions)
    selected = report(directory, metric_rows, paired, omissions, input_count, excluded, input_hashes)
    print(f"Report: {directory / 'report.md'}")
    print(f"Development primary-MAE candidate: {selected}; no automatic promotion.")


if __name__ == "__main__":
    main()
