"""Report frozen history-decay experiments; no fitting or automatic promotion.

Usage: python history_decay_report.py OUTPUT_DIRECTORY
"""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

import numpy as np
import margin_cap_report as shared

MODELS = ("baseline", "early_week5", "smooth_moderate", "smooth_fast", "current_only_week5")
PRIMARY = "regular_weeks_2_7"
DEVELOPMENT = "development_2022_2024_weeks_2_7"
CONFIRMATION = "confirmation_2025_weeks_2_7"
SEED, DRAWS = 20260927, 10000
REQUIRED = shared.REQUIRED | {"model_week"}


def load_predictions(path):
    groups = {model: {} for model in MODELS}
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        if not REQUIRED <= set(reader.fieldnames or ()):
            raise ValueError("Missing prediction columns")
        for line, source in enumerate(reader, 2):
            row = {key: source[key].strip() for key in REQUIRED}
            for key in (*shared.NUMERIC, "model_week"):
                row[key] = shared.number(row[key])
            for key in shared.BOOLEAN:
                row[key] = shared.boolean(row[key])
            model = row["model"]
            if model not in groups or not row["game_id"] or row["game_id"] in groups[model]:
                raise ValueError(f"Unknown model or empty/duplicate game ID at line {line}")
            if not all(math.isfinite(row[key]) for key in ("season", "week", "model_week", "actual_margin", "expected_margin")):
                raise ValueError(f"Nonfinite prediction, target or time at line {line}")
            if row["season"] not in (2022, 2023, 2024, 2025) or row["week"] != int(row["week"]) or row["model_week"] != int(row["model_week"]):
                raise ValueError(f"Unexpected evaluation season/week at line {line}")
            if math.isinf(row["closing_home_spread"]):
                raise ValueError(f"Infinite closing line at line {line}")
            groups[model][row["game_id"]] = row
    control = groups["baseline"]
    if not control:
        raise ValueError("No baseline predictions")
    for model in MODELS[1:]:
        if set(groups[model]) != set(control):
            raise ValueError(f"{model} cohort differs from baseline; intersection-only scoring is forbidden")
        for game_id, row in control.items():
            for key in (*shared.METADATA, "model_week"):
                if not shared.equal_metadata(row[key], groups[model][game_id][key]):
                    raise ValueError(f"Changed scoring metadata: {model}, {game_id}, {key}")
    ids = sorted(control, key=lambda key: (control[key]["season"], control[key]["week"], key))
    keep = [key for key in ids if shared.eligible(control[key])]
    if not keep:
        raise ValueError("No eligible FBS games")
    data = {}
    for model in MODELS:
        rows = [groups[model][key] for key in keep]
        data[model] = {key: np.asarray([row[key] for row in rows]) for key in REQUIRED}
        shared.grade(data[model])
    return data, len(ids), len(ids) - len(keep)


def slices(reference):
    regular = reference["postseason_type"] == "regular"
    primary = regular & (reference["week"] >= 2) & (reference["week"] <= 7)
    masks = {
        PRIMARY: primary,
        DEVELOPMENT: primary & (reference["season"] <= 2024),
        CONFIRMATION: primary & (reference["season"] == 2025),
        "regular_weeks_0_1": regular & (reference["week"] <= 1),
        "regular_weeks_2_4": regular & (reference["week"] >= 2) & (reference["week"] <= 4),
        "regular_weeks_5_7": regular & (reference["week"] >= 5) & (reference["week"] <= 7),
        "regular_week_8_plus": regular & (reference["week"] >= 8),
        "all_regular": regular,
        "all_fbs": np.ones(len(primary), dtype=bool),
    }
    for year in (2022, 2023, 2024, 2025):
        masks[f"season_{year}_weeks_2_7"] = primary & (reference["season"] == year)
    return masks


def cluster_intervals(delta, labels):
    keep = np.isfinite(delta)
    delta, labels = np.asarray(delta)[keep], np.asarray(labels)[keep]
    clusters = np.unique(labels)
    if len(clusters) < 2:
        return (math.nan,) * 4
    sums = np.asarray([delta[labels == cluster].sum() for cluster in clusters])
    counts = np.asarray([(labels == cluster).sum() for cluster in clusters])
    draws = np.random.default_rng(SEED).integers(0, len(clusters), (DRAWS, len(clusters)))
    samples = sums[draws].sum(axis=1) / counts[draws].sum(axis=1)
    return tuple(float(value) for value in np.quantile(samples, [.025, .975, .00625, .99375]))


def chronological_labels(data, mask):
    return np.asarray([f"{int(year)}-{int(week)}" for year, week in
                       zip(data["season"][mask], data["model_week"][mask])])


def paired(model, candidate, control, name, mask):
    row = shared.comparison(model, candidate, control, name, mask)
    row["reference"] = "baseline"
    labels = chronological_labels(candidate, mask)
    row["season_week_clusters"] = len(np.unique(labels))
    row["interval_cluster"] = "season_model_week"
    for key, values in (
        ("mae", candidate["absolute_error"][mask] - control["absolute_error"][mask]),
        ("su_pp", 100 * (candidate["su_correct"][mask].astype(float) - control["su_correct"][mask].astype(float))),
        ("ats_paired_pp", 100 * (candidate["ats_correct"][mask] - control["ats_correct"][mask])),
    ):
        row[f"{key}_season_ci95_low"] = row[f"{key}_ci95_low"]
        row[f"{key}_season_ci95_high"] = row[f"{key}_ci95_high"]
        for suffix, bound in zip(("ci95_low", "ci95_high", "ci9875_low", "ci9875_high"), cluster_intervals(values, labels)):
            row[f"{key}_{suffix}"] = bound
    return row


def self_check():
    sample = {"expected_margin": np.asarray([7., 1., 0., -3.]),
              "actual_margin": np.asarray([6., 2., 4., -7.]),
              "closing_home_spread": np.asarray([-6., -3., 0., math.nan])}
    shared.grade(sample)
    assert sample["push"].tolist() == [True, False, False, False]
    assert sample["win"].tolist() == [False, True, False, False]
    assert sample["zero_edge_pass"].tolist() == [False, False, True, False]
    assert sample["selected"].tolist() == [True, True, False, False]
    assert cluster_intervals(np.asarray([-1., -1., -1.]), np.asarray(["a", "b", "c"])) == (-1.,) * 4
    chronology = {"season": np.asarray([2022, 2022, 2022]), "week": np.asarray([1, 1, 2]),
                  "model_week": np.asarray([1, 2, 2])}
    assert chronological_labels(chronology, np.asarray([True, True, True])).tolist() == ["2022-1", "2022-2", "2022-2"]


def clean(value):
    if isinstance(value, dict):
        return {key: clean(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [clean(item) for item in value]
    if isinstance(value, (np.bool_,)):
        return bool(value)
    if isinstance(value, (np.integer,)):
        return int(value)
    if isinstance(value, (float, np.floating)):
        return float(value) if math.isfinite(value) else None
    return value


def ranking_context(directory):
    files = [directory / f"{model}_rankings.csv" for model in MODELS]
    if not any(path.exists() for path in files):
        return [], []
    if not all(path.exists() for path in files):
        raise ValueError("Incomplete current ranking variants")
    tables = {}
    for model, path in zip(MODELS, files):
        with path.open(encoding="utf-8-sig", newline="") as handle:
            rows = list(csv.DictReader(handle))
        if len({row["team"] for row in rows}) != len(rows):
            raise ValueError(f"Duplicate team in {path.name}")
        if sorted(int(row["rank"]) for row in rows) != list(range(1, len(rows) + 1)):
            raise ValueError(f"Nonconsecutive ranks in {path.name}")
        if any(not math.isfinite(float(row["rating"])) for row in rows):
            raise ValueError(f"Nonfinite rating in {path.name}")
        tables[model] = {row["team"]: row for row in rows}
    baseline = tables["baseline"]
    for model, table in tables.items():
        if set(table) != set(baseline):
            raise ValueError(f"Ranking team sets differ for {model}")
        for team, row in table.items():
            if int(row["baseline_rank"]) != int(baseline[team]["rank"]):
                raise ValueError(f"Incorrect baseline rank for {model}: {team}")
    summary = []
    for team in ("Oklahoma State", "Kentucky", "James Madison", "Penn State", "Wake Forest"):
        if team not in baseline:
            raise ValueError(f"Missing diagnostic team: {team}")
        for model in MODELS:
            row = tables[model][team]
            summary.append(dict(team=team, model=model, rank=int(row["rank"]),
                                rating=float(row["rating"]), baseline_rank=int(row["baseline_rank"])))
    return summary, files


def main(directory):
    self_check()
    manifest_path = directory / "manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8-sig"))
    if manifest.get("status") != "fitting_complete" or manifest.get("source_inputs_unchanged") is not True:
        raise ValueError("Require completed fitting and unchanged source inputs")
    replay = manifest.get("baseline_prediction_replay_error")
    if not isinstance(replay, (int, float)) or not math.isfinite(replay) or replay > 1e-8:
        raise ValueError("Baseline replay must pass within 1e-8")
    data, input_count, excluded = load_predictions(directory / "predictions.csv")
    masks = slices(data["baseline"])
    if any(not mask.any() for name, mask in masks.items() if name.startswith("season_")):
        raise ValueError("Every evaluation season needs primary games")
    ranks, ranking_files = ranking_context(directory)
    protected = [manifest_path, directory / "predictions.csv", *ranking_files]
    source_hashes = {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in protected}
    metric_rows = [shared.metrics(model, data[model], name, mask)
                   for model in MODELS for name, mask in masks.items()]
    pairs = [paired(model, data[model], data["baseline"], name, mask)
             for model in MODELS[1:] for name, mask in masks.items()]
    metric = {(row["model"], row["slice"]): row for row in metric_rows}
    pair = {(row["model"], row["slice"]): row for row in pairs}
    ordered = sorted(MODELS, key=lambda model: (metric[model, DEVELOPMENT]["margin_mae"], MODELS.index(model)))
    candidate = next(model for model in ordered if model != "baseline")
    dev, confirm = pair[candidate, DEVELOPMENT], pair[candidate, CONFIRMATION]
    all_regular = pair[candidate, "all_regular"]
    improved_years = [year for year in (2022, 2023, 2024, 2025)
                      if pair[candidate, f"season_{year}_weeks_2_7"]["mae_change"] < 0]
    gates = {
        "development_mae_gain_at_least_0_10": dev["mae_change"] <= -.10,
        "confirmation_mae_not_worse": confirm["mae_change"] <= 0,
        "all_regular_mae_not_worse": all_regular["mae_change"] <= 0,
        "at_least_three_of_four_seasons_improved": len(improved_years) >= 3,
        "development_adjusted_ci_upper_below_zero": dev["mae_ci9875_high"] < 0,
    }
    omissions = []
    for model in MODELS[1:]:
        for year in (2022, 2023, 2024, 2025):
            mask = masks[PRIMARY] & (data["baseline"]["season"] != year)
            row = paired(model, data[model], data["baseline"], PRIMARY, mask)
            row["omitted_season"] = year
            omissions.append(row)
    selection = dict(candidate=candidate, development_order=ordered,
                     development_best_including_baseline=ordered[0], selection_slice=DEVELOPMENT,
                     confirmation_slice=CONFIRMATION, adjusted_interval_gate_slice=DEVELOPMENT,
                     gates=gates, all_evidence_gates_passed=all(gates.values()),
                     improved_primary_seasons=improved_years, automatic_promotion=False,
                     primary_interval_cluster="season_model_week", secondary_interval_cluster="season",
                     bootstrap_seed=SEED, bootstrap_draws=DRAWS, primary_comparisons=4,
                     input_predictions_per_model=input_count, excluded_non_fbs_or_non_cfp_bowls=excluded,
                     source_sha256=source_hashes,
                     limitation="All historical seasons were previously researched. This power/direct-prior ablation holds coach features fixed; it is not a full rebuild of history-dependent coaching expectations.")
    shared.write_csv(directory / "metrics.csv", metric_rows)
    shared.write_csv(directory / "paired.csv", pairs)
    shared.write_csv(directory / "leave_one_season_out.csv", omissions)
    if ranks:
        shared.write_csv(directory / "ranking_diagnostics.csv", ranks)
    (directory / "selection.json").write_text(json.dumps(clean(selection), indent=2, allow_nan=False) + "\n", encoding="utf-8")
    fmt, pct = shared.fmt, shared.pct
    lines = ["# Earlier and smoother historical decay", "",
             "Development selection uses 2022–2024 regular-season FBS games in Weeks 2–7. "
             "2025 is reported separately and was already used in prior research. Negative MAE changes favor the candidate.", "",
             "Each chronological fold refits the same 19-feature uncapped opponent-relative ridge model on earlier seasons only. "
             "Direct game-count predictors remain excluded. Frozen penalties are 8/32/32/32 for 2022/2023/2024/2025; coach split is 0.65. "
             "Only score-power and explicit prior/trailing-history inputs change. Coach features are held fixed.", "",
             "## Development selection and separate 2025 check", "",
             "| Model | Development games | Development MAE | Change | 2025 MAE | 2025 change | All regular change |",
             "|---|---:|---:|---:|---:|---:|---:|"]
    for model in MODELS:
        d, c = metric[model, DEVELOPMENT], metric[model, CONFIRMATION]
        lines.append(f"| {model} | {d['games']} | {fmt(d['margin_mae'])} | "
                     f"{fmt(d['margin_mae']-metric['baseline', DEVELOPMENT]['margin_mae'])} | "
                     f"{fmt(c['margin_mae'])} | {fmt(c['margin_mae']-metric['baseline', CONFIRMATION]['margin_mae'])} | "
                     f"{fmt(metric[model, 'all_regular']['margin_mae']-metric['baseline', 'all_regular']['margin_mae'])} |")
    lines += ["", f"The best challenger by development MAE is **{candidate}**. "
              f"The lowest development MAE including baseline belongs to **{ordered[0]}**. "
              "Ties follow the declared model order, with baseline preferred.", "",
              f"Evidence gates: **{'all passed' if all(gates.values()) else 'not all passed'}**. No model is automatically promoted.", "",
              "| Gate | Result |", "|---|---|"]
    for gate, passed in gates.items():
        lines.append(f"| {gate.replace('_', ' ')} | {'Pass' if passed else 'Fail'} |")
    lines += ["", "## Development uncertainty", "",
              "| Candidate vs baseline | Change in MAE | Season-week 95% | Adjusted 98.75% | Season 95% |",
              "|---|---:|---|---|---|"]
    for model in MODELS[1:]:
        p = pair[model, DEVELOPMENT]
        lines.append(f"| {model} | {fmt(p['mae_change'])} | [{fmt(p['mae_ci95_low'])}, {fmt(p['mae_ci95_high'])}] | "
                     f"[{fmt(p['mae_ci9875_low'])}, {fmt(p['mae_ci9875_high'])}] | "
                     f"[{fmt(p['mae_season_ci95_low'])}, {fmt(p['mae_season_ci95_high'])}] |")
    lines += ["", "## All four seasons: Weeks 2–7", "",
              "| Model | Games | MAE | RMSE | SU | ATS W–L–P | ATS | Missing lines | Zero-edge passes |",
              "|---|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for model in MODELS:
        m = metric[model, PRIMARY]
        lines.append(f"| {model} | {m['games']} | {fmt(m['margin_mae'])} | {fmt(m['margin_rmse'])} | "
                     f"{pct(m['su_accuracy'])} | {m['wins']}–{m['losses']}–{m['pushes']} | "
                     f"{pct(m['ats_accuracy'])} | {m['missing_lines']} | {m['zero_edge_passes']} |")
    lines += ["", "## Seasonal consistency", "",
              "| Candidate | 2022 change | 2023 change | 2024 change | 2025 change | LOSO change range |",
              "|---|---:|---:|---:|---:|---|"]
    for model in MODELS[1:]:
        effects = [pair[model, f"season_{year}_weeks_2_7"]["mae_change"] for year in (2022, 2023, 2024, 2025)]
        omitted = [row["mae_change"] for row in omissions if row["model"] == model]
        lines.append(f"| {model} | " + " | ".join(fmt(value) for value in effects) +
                     f" | [{fmt(min(omitted))}, {fmt(max(omitted))}] |")
    lines += ["", "## Secondary windows", "",
              "| Candidate | Weeks 0–1 change | Weeks 2–4 change | Weeks 5–7 change | Week 8+ change | All regular change |",
              "|---|---:|---:|---:|---:|---:|"]
    for model in MODELS[1:]:
        values = [pair[model, name]["mae_change"] for name in
                  ("regular_weeks_0_1", "regular_weeks_2_4", "regular_weeks_5_7", "regular_week_8_plus", "all_regular")]
        lines.append(f"| {model} | " + " | ".join(fmt(value) for value in values) + " |")
    if ranks:
        lookup = {(row["team"], row["model"]): row for row in ranks}
        lines += ["", "## Current ranking sensitivity", "",
                  "These rank changes are descriptive and do not select or validate a decay rule. "
                  "The current_only_week5 variant zeros only power/direct history from Week 5; coaching retains historical information.", "",
                  "| Team | Baseline | Early Week 5 | Smooth moderate | Smooth fast | Zero power/direct history from Week 5 |",
                  "|---|---:|---:|---:|---:|---:|"]
        for team in ("Oklahoma State", "Kentucky", "James Madison", "Penn State", "Wake Forest"):
            lines.append(f"| {team} | " + " | ".join(
                f"#{lookup[team, model]['rank']} ({lookup[team, model]['rating']:+.1f})" for model in MODELS) + " |")
    lines += ["", "## Checks and limits", "",
              f"- Baseline historical prediction replay error: {replay:.3g}; exact game cohorts, scores and grading lines match across all five models.",
              f"- {input_count:,} source predictions per model; {excluded:,} ineligible FCS/non-CFP-bowl rows excluded uniformly.",
              "- Five Labor Day Monday Week 1 games share a chronological snapshot with Week 2. Their baseline Week 1 side-power inputs are preserved explicitly, and the FBS-transition bridge is reapplied. These preflight corrections preceded challenger metrics.",
              "- MAE and SU keep missing-line games. ATS uses saved closing lines, excludes pushes from accuracy and records zero-edge passes. This is not a Friday execution or ROI backtest.",
              f"- Paired bootstrap: {DRAWS:,} draws, seed {SEED}. Primary intervals cluster by season and chronological model_week; 98.75% intervals adjust for four candidate comparisons. Season-cluster 95% intervals and leave-one-season-out results are secondary dependence checks.",
              "- Season-week clustering preserves within-week dependence but does not fully capture dependence across weeks involving the same teams. Only four seasons, and three development seasons, are available; season intervals are fragile.",
              "- Choosing among multiple variants and reusing previously researched seasons can overstate evidence. No automatic production change follows these results.",
              "- Coaching residuals and context depend on score-power in the full architecture; this controlled ablation deliberately holds those coach features fixed. A future full-pipeline promotion must address that difference.", ""]
    (directory / "report.md").write_text("\n".join(lines), encoding="utf-8")
    assert source_hashes == {path.name: hashlib.sha256(path.read_bytes()).hexdigest() for path in protected}
    print(json.dumps(clean({"candidate": candidate, "development_best": ordered[0],
                           "all_evidence_gates_passed": all(gates.values()), "report": str(directory / "report.md")})))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("directory", type=Path)
    main(parser.parse_args().directory)
