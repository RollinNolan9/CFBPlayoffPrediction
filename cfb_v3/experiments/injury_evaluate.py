"""Paired evaluation of precomputed, audited SEC RB injury cases.

Feature construction and evidence selection must happen before invoking this file.
No fitting, network access, production changes, or full-season player predictors.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]


def metrics(rows: pd.DataFrame, margin: str) -> dict:
    error = (rows[margin] - rows.actual_margin).abs()
    valid = rows.reference_home_spread.notna()
    actual_cover = rows.actual_margin + rows.reference_home_spread
    predicted_cover = rows[margin] + rows.reference_home_spread
    pushes = valid & np.isclose(actual_cover, 0)
    abstain = valid & ~pushes & np.isclose(predicted_cover, 0)
    decided = valid & ~pushes & ~abstain
    wins = decided & (np.sign(actual_cover) == np.sign(predicted_cover))
    losses = decided & ~wins
    return {
        "games": len(rows), "margin_mae": float(error.mean()) if len(rows) else None,
        "su_correct": int((np.sign(rows[margin]) == np.sign(rows.actual_margin)).sum()),
        "ats_wins": int(wins.sum()), "ats_losses": int(losses.sum()),
        "ats_pushes": int(pushes.sum()), "ats_abstentions": int(abstain.sum()),
        "ats_missing_lines": int((~valid).sum()),
        "ats_pct": float(wins.sum() / decided.sum() * 100) if decided.sum() else None,
    }


def validate_cases(cases: pd.DataFrame, baseline: pd.DataFrame) -> pd.DataFrame:
    required = ["game_id", "team", "player_id", "player_name", "status", "position",
                "report_published_at", "source_url", "prior_games", "expected_involvements",
                "player_ppa", "replacement_ppa", "raw_loss_points", "latest_history_kickoff",
                "played_latest_game", "team_report_complete"]
    missing = set(required) - set(cases)
    if missing:
        raise ValueError(f"Missing case fields: {sorted(missing)}")
    if cases.duplicated(["game_id", "team", "player_id"]).any():
        raise ValueError("Duplicate player case")
    if not set(cases.game_id).issubset(set(baseline.game_id)):
        raise ValueError("Case outside baseline cohort")
    joined = cases.merge(baseline[["game_id", "home", "away", "cutoff_utc", "season"]],
                         on="game_id", validate="many_to_one")
    for field in ["report_published_at", "latest_history_kickoff", "cutoff_utc"]:
        joined[field] = pd.to_datetime(joined[field], utc=True, errors="raise")
    finite_fields = ["prior_games", "expected_involvements", "player_ppa",
                     "replacement_ppa", "raw_loss_points"]
    if not np.isfinite(joined[finite_fields].to_numpy(dtype=float)).all():
        raise ValueError("Nonfinite injury inputs")
    truth = lambda col: joined[col].astype(str).str.lower().isin(["true", "1"])
    eligible = (
        joined.status.str.lower().eq("out") & joined.position.eq("RB")
        & (joined.prior_games >= 2) & (joined.expected_involvements >= 8)
        & truth("played_latest_game") & truth("rb_report_complete" if "rb_report_complete" in joined else "team_report_complete")
        & joined.source_url.notna() & joined.source_url.astype(str).str.startswith("https://")
        & (joined.report_published_at <= joined.cutoff_utc)
        & (joined.latest_history_kickoff < joined.cutoff_utc)
        & ((joined.team == joined.home) | (joined.team == joined.away))
    )
    if not eligible.all():
        raise ValueError(f"Ineligible or future-informed cases: {joined.loc[~eligible, 'player_name'].tolist()}")
    formula = joined.expected_involvements * (joined.player_ppa - joined.replacement_ppa)
    if not np.allclose(formula, joined.raw_loss_points, rtol=1e-9, atol=1e-8):
        raise ValueError("Case impact does not reproduce locked formula")
    joined["home_margin_shift"] = np.where(joined.team == joined.home, -1, 1) * joined.raw_loss_points
    return joined


def evaluate(baseline: pd.DataFrame, cases: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame, float | None]:
    if baseline.game_id.duplicated().any():
        raise ValueError("Duplicate baseline game")
    if not np.isfinite(baseline[["expected_margin", "actual_margin"]].to_numpy(dtype=float)).all():
        raise ValueError("Nonfinite baseline prediction or outcome")
    if not set(baseline.season).issubset({2024, 2025}):
        raise ValueError("Baseline outside locked 2024/2025 seasons")
    checked = validate_cases(cases, baseline)
    shifts = checked.groupby("game_id").home_margin_shift.sum()
    rows = baseline.copy()
    rows["raw_home_shift"] = rows.game_id.map(shifts).fillna(0)
    rows["affected"] = rows.game_id.isin(checked.game_id)
    output = []
    for scale in [0., .5, 1.]:
        rows[f"margin_{scale:g}"] = rows.expected_margin + scale * rows.raw_home_shift
        for season in [2024, 2025]:
            subset = rows[rows.season.eq(season) & rows.affected]
            result = metrics(subset, f"margin_{scale:g}")
            result.update(season=season, scale=scale, slice="affected_games")
            output.append(result)
    results = pd.DataFrame(output)
    dev = results[(results.season == 2024) & results.margin_mae.notna()]
    selected = None if dev.empty else float(dev.sort_values(["margin_mae", "scale"]).iloc[0].scale)
    for scale in [0., .5, 1.]:
        rows[f"mae_change_{scale:g}"] = ((rows[f"margin_{scale:g}"] - rows.actual_margin).abs()
                                          - (rows.expected_margin - rows.actual_margin).abs())
        rows[f"ats_side_changed_{scale:g}"] = (
            np.sign(rows[f"margin_{scale:g}"] + rows.reference_home_spread)
            != np.sign(rows.expected_margin + rows.reference_home_spread)) & rows.reference_home_spread.notna()
    assert np.allclose(rows["margin_0"], rows.expected_margin)
    assert np.allclose(rows.loc[~rows.affected, "margin_1"], rows.loc[~rows.affected, "expected_margin"])
    return rows, results, selected


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--cases", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--coverage", type=Path,
                        help="Report coverage only; does not assert complete player histories")
    args = parser.parse_args()
    sources = [args.baseline, args.cases, Path(__file__), Path(__file__).with_name("INJURY_PILOT_PROTOCOL.md")]
    if args.coverage:
        sources.append(args.coverage)
    feature_audits = [args.cases.parent / name for name in ["case_eligibility.csv", "report_selection.csv", "feature_sources.json"]]
    sources.extend(path for path in feature_audits if path.exists())
    sha = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
    fingerprints = {str(path.resolve()): sha(path) for path in sources}
    baseline = pd.read_csv(args.baseline, dtype={"game_id": str}).rename(columns={
        "baseline_expected_margin": "expected_margin", "prediction_cutoff_utc": "cutoff_utc"})
    cases = pd.read_csv(args.cases, dtype={"game_id": str, "player_id": str})
    if baseline.game_id.duplicated().any():
        raise ValueError("Duplicate baseline game")
    rows, results, selected = evaluate(baseline, cases)
    if args.coverage:
        coverage = pd.read_csv(args.coverage, dtype={"game_id": str})
        if coverage.game_id.duplicated().any() or set(coverage.game_id) != set(baseline.game_id):
            raise ValueError("Coverage does not match baseline population")
        covered = coverage.loc[coverage.both_rb_reports_covered.astype(str).str.lower().isin(["true", "1"]), "game_id"]
        rows["both_rb_reports_recovered"] = rows.game_id.isin(covered)
        secondary = []
        for scale in [0., .5, 1.]:
            for season in [2024, 2025]:
                subset = rows[rows.season.eq(season) & rows.both_rb_reports_recovered]
                result = metrics(subset, f"margin_{scale:g}")
                result.update(season=season, scale=scale, slice="report_covered_partial_history")
                secondary.append(result)
        results = pd.concat([results, pd.DataFrame(secondary)], ignore_index=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    destination = args.output / stamp
    destination.mkdir(parents=True, exist_ok=False)
    rows.to_csv(destination / "paired_predictions.csv", index=False)
    results.to_csv(destination / "metrics.csv", index=False)
    for source in [args.cases, *feature_audits, *([args.coverage] if args.coverage else [])]:
        if source.exists():
            shutil.copy2(source, destination / source.name)
    assert fingerprints == {str(path.resolve()): sha(path) for path in sources}
    manifest = {"created_at": stamp, "selected_on_2024_scale": selected,
                "cutoff": "Friday12:00 America/Los_Angeles", "source_sha256": fingerprints,
                "affected_games": int(rows.affected.sum()),
                "line_caveat": "PBP reference lines; no verified Friday bookmaker timestamps",
                "coverage_caveat": "Report-covered slate uses known case adjustments only; uncollected player histories are unknown, not verified healthy. No complete covered-slate injury estimate is available.",
                "production_changed": False}
    (destination / "manifest.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")
    print(results.to_string(index=False))
    print(f"Output: {destination}")


if __name__ == "__main__":
    main()
