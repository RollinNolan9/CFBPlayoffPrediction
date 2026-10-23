"""Build injury features from dated reports and strictly prior player histories.

Reads outcome-free schedule.csv only. Never reads target-game scores/predictions.
History specifications are a JSON list of game_id, team and history_dir entries.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import unicodedata
from pathlib import Path

import numpy as np
import pandas as pd


def person_key(value: str) -> str:
    text = unicodedata.normalize("NFKD", str(value)).encode("ascii", "ignore").decode().lower()
    text = re.sub(r"\b(jr|sr|ii|iii|iv)\.?\s*$", "", text).strip()
    return re.sub(r"[^a-z0-9]", "", text)


def truth(series: pd.Series) -> pd.Series:
    return series.astype(str).str.lower().isin(["true", "1"])


def select_reports(reports: pd.DataFrame, statuses: pd.DataFrame) -> tuple[pd.DataFrame, list[dict]]:
    """Select latest pre-cutoff evidence before checking completeness.

    A newer partial update blocks the older full report: it may clear an Out
    designation without establishing the replacement pool. Simultaneous copies
    must agree, including listed-player omissions, before either can be used.
    """
    if reports.report_id.duplicated().any():
        raise ValueError("Duplicate report identity")
    scope = "rb_report_complete" if "rb_report_complete" in reports else "team_report_complete"
    usable_time = reports.published.notna() & (reports.published <= reports.cutoff)
    selected, ledger = [], []
    for (game_id, team), group in reports[usable_time].groupby(["game_id", "team"], sort=True):
        latest = group[group.published.eq(group.published.max())]
        reason = "selected_latest_report"
        if "content_update_after_cutoff" in latest and truth(latest.content_update_after_cutoff).any():
            reason = "latest_report_content_updated_after_cutoff"
        elif not truth(latest[scope]).all():
            reason = "latest_report_incomplete"
        else:
            snapshots = []
            for report in latest.itertuples():
                entries = statuses[statuses.report_id.eq(report.report_id)].copy()
                if "game_id" in entries and not entries.game_id.eq(game_id).all():
                    raise ValueError(f"Status game differs from report {report.report_id}")
                if "team" in entries and not entries.team.eq(team).all():
                    raise ValueError(f"Status team differs from report {report.report_id}")
                entries["normalized_status"] = entries.status.astype(str).str.strip().str.lower()
                if entries.groupby("person_key").normalized_status.nunique().gt(1).any():
                    reason = "conflicting_statuses_in_latest_report"
                    break
                snapshots.append(frozenset(zip(entries.person_key, entries.normalized_status)))
            if reason == "selected_latest_report" and len(set(snapshots)) > 1:
                reason = "conflicting_latest_reports_same_timestamp"
        accepted = reason == "selected_latest_report"
        ledger.append({"game_id": game_id, "team": team,
                       "report_ids": "|".join(sorted(latest.report_id)),
                       "report_published_at": latest.published.iloc[0].isoformat(),
                       "eligible": accepted, "reason": reason})
        if accepted:
            # A deterministic choice is safe only after the content agrees.
            selected.append(latest.sort_values("report_id").index[-1])
    return reports.loc[selected].copy(), ledger


def validate_history_identity(manifest: dict, report: pd.Series,
                              scheduled: pd.Series, specification: dict) -> None:
    canonical = lambda value: "Mississippi" if str(value) in {"Ole Miss", "Mississippi"} else str(value)
    expected_team = canonical(report.team)
    for label, value in [("manifest", manifest.get("team")), ("specification", specification.get("team"))]:
        if value is not None and canonical(value) != expected_team:
            raise ValueError(f"History {label} team differs from case team")
    if {"home", "away"}.issubset(scheduled.index) and expected_team not in {
            canonical(scheduled.home), canonical(scheduled.away)}:
        raise ValueError("Case team is not in the scheduled game")
    seasons = []
    for source in (manifest, report, scheduled, specification):
        value = source.get("season")
        if value is not None and not pd.isna(value) and str(value).strip():
            number = float(value)
            if not np.isfinite(number) or not number.is_integer():
                raise ValueError("Invalid history/case season")
            seasons.append(int(number))
    if len(set(seasons)) > 1:
        raise ValueError("History manifest/specification season differs from case/schedule season")


CASE_COLUMNS = ["game_id", "team", "player_id", "player_name", "status", "position",
    "report_id", "report_published_at", "timestamp_kind", "source_url", "evidence_grade",
    "team_report_complete", "rb_report_complete", "prior_games", "expected_involvements", "player_ppa",
    "replacement_ppa", "raw_loss_points", "latest_history_kickoff", "played_latest_game",
    "prior_involvements", "prior_total_ppa", "replacement_involvements", "replacement_players",
    "history_dir"]


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--schedule", type=Path, required=True)
    parser.add_argument("--reports", type=Path, required=True)
    parser.add_argument("--statuses", type=Path, required=True)
    parser.add_argument("--histories", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    sources = [args.schedule, args.reports, args.statuses, args.histories, Path(__file__)]
    sha = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
    hashes = {str(path.resolve()): sha(path) for path in sources}
    schedule = pd.read_csv(args.schedule, dtype={"game_id": str}).set_index("game_id")
    if not schedule.index.is_unique:
        raise ValueError("Duplicate schedule game")
    forbidden = {"actual_margin", "home_points", "away_points", "expected_margin", "baseline_expected_margin"}
    if forbidden.intersection(schedule.columns):
        raise ValueError("Feature construction requires an outcome-free schedule")
    reports = pd.read_csv(args.reports, dtype={"game_id": str}).fillna("")
    statuses = pd.read_csv(args.statuses, dtype={"game_id": str}).fillna("")
    statuses["person_key"] = statuses.player_name.map(person_key)
    history_specs = json.loads(args.histories.read_text(encoding="utf-8-sig"))
    specs = {(str(row["game_id"]), row["team"]): row for row in history_specs}
    if len(specs) != len(history_specs):
        raise ValueError("Duplicate game/team history specification")
    reports["published"] = pd.to_datetime(reports.report_published_at, utc=True, errors="coerce")
    reports["cutoff"] = reports.game_id.map(schedule.prediction_cutoff_utc)
    reports["cutoff"] = pd.to_datetime(reports.cutoff, utc=True, errors="coerce")
    reports, report_selection = select_reports(reports, statuses)
    cases, ledger = [], []
    for _, report in reports.iterrows():
        entries = statuses[statuses.report_id.eq(report.report_id)].copy()
        if entries.groupby("person_key").status.nunique().max() > 1:
            raise ValueError(f"Conflicting statuses in {report.report_id}")
        specification = specs.get((report.game_id, report.team))
        directory = None if specification is None else Path(specification["history_dir"])
        history = None
        history_error = "history_not_collected_for_team"
        if directory is not None:
            paths = [directory / "manifest.json", directory / "player_game_history.csv", directory / "prior_team_games.csv"]
            for path in paths:
                hashes[str(path.resolve())] = sha(path)
            manifest = json.loads(paths[0].read_text(encoding="utf-8-sig"))
            validate_history_identity(manifest, report, schedule.loc[report.game_id], specification)
            if manifest.get("history_complete"):
                history = pd.read_csv(paths[1], dtype={"game_id": str, "athlete_id": str})
                prior = pd.read_csv(paths[2], dtype={"game_id": str})
                if (pd.to_datetime(prior.kickoff, utc=True) >= report.cutoff).any():
                    raise ValueError("Future player game in history")
                if not set(history.game_id).issubset(set(prior.game_id)):
                    raise ValueError("Unknown player-history game")
                if pd.Timestamp(manifest["prediction_cutoff_utc"]) != report.cutoff:
                    raise ValueError("History cutoff differs from report/game cutoff")
                history["person_key"] = history.player_name.map(person_key)
                if history.groupby("person_key").athlete_id.nunique().max() > 1:
                    raise ValueError("Ambiguous player names; requires manual identity resolution")
                # Each athlete/game contributes only once, even if multiple stat roles exist.
                if history.duplicated(["game_id", "athlete_id"]).any():
                    raise ValueError("Duplicate player-game history")
                history_error = ""
            else:
                history_error = "incomplete_or_unreconciled_player_history"
        known_rb = set() if history is None else set(history.person_key)
        position = entries["resolved_position"] if "resolved_position" in entries else entries.position
        candidates = entries[entries.status.str.lower().eq("out") &
                             (position.eq("RB") | entries.person_key.isin(known_rb))]
        for _, player in candidates.iterrows():
            record = {"game_id": report.game_id, "team": report.team,
                      "player_name": player.player_name, "report_id": report.report_id,
                      "eligible": False, "reason": history_error}
            if history is None:
                ledger.append(record)
                continue
            counts = history.groupby(["athlete_id", "person_key", "player_name"], as_index=False).agg(
                opportunities=("ppa_involvements", "sum"), total_ppa=("total_ppa", "sum"))
            selected = counts[counts.person_key.eq(player.person_key)]
            if len(selected) != 1:
                record["reason"] = "no_unique_prior_player_history"
                ledger.append(record)
                continue
            selected = selected.iloc[0]
            n_games = len(prior)
            last = prior.loc[pd.to_datetime(prior.kickoff, utc=True).idxmax()]
            recent = history[history.game_id.eq(last.game_id) & history.athlete_id.eq(selected.athlete_id)]
            played = bool(recent.ppa_involvements.sum() > 0)
            expected = selected.opportunities / n_games if n_games else 0
            record.update(prior_games=n_games, expected_involvements=expected, played_latest_game=played)
            if n_games < 2 or expected < 8 or not played:
                record["reason"] = ("fewer_than_two_prior_games" if n_games < 2 else
                                    "below_eight_prior_involvements_per_team_game" if expected < 8 else
                                    "already_absent_from_latest_game")
                ledger.append(record)
                continue
            unavailable = set(entries.loc[entries.status.str.lower().eq("out"), "person_key"])
            backups = counts[~counts.person_key.isin(unavailable | {player.person_key}) & counts.opportunities.gt(0)]
            if backups.empty:
                record["reason"] = "no_usable_replacement_history"
                ledger.append(record)
                continue
            replacement = backups.total_ppa.sum() / backups.opportunities.sum()
            player_ppa = selected.total_ppa / selected.opportunities
            backup_list = [{"player_id": row.athlete_id, "name": row.player_name,
                "weight": row.opportunities / backups.opportunities.sum(),
                "ppa": row.total_ppa / row.opportunities,
                "reported_status": (entries.loc[entries.person_key.eq(row.person_key), "status"].iloc[0]
                                    if entries.person_key.eq(row.person_key).any() else "not_listed")}
                           for row in backups.itertuples()]
            case = {"game_id": report.game_id, "team": report.team, "player_id": selected.athlete_id,
                "player_name": selected.player_name, "status": "Out", "position": "RB",
                "report_id": report.report_id, "report_published_at": report.report_published_at,
                "timestamp_kind": report.timestamp_kind, "source_url": report.source_url,
                "evidence_grade": report.evidence_grade, "team_report_complete": bool(truth(pd.Series([report.team_report_complete])).iloc[0]),
                "rb_report_complete": True,
                "prior_games": n_games, "expected_involvements": expected,
                "player_ppa": player_ppa, "replacement_ppa": replacement,
                "raw_loss_points": expected * (player_ppa - replacement),
                "latest_history_kickoff": last.kickoff, "played_latest_game": played,
                "prior_involvements": selected.opportunities, "prior_total_ppa": selected.total_ppa,
                "replacement_involvements": int(backups.opportunities.sum()),
                "replacement_players": json.dumps(backup_list), "history_dir": str(directory.resolve())}
            if not np.isfinite([player_ppa, replacement, case["raw_loss_points"]]).all():
                raise ValueError("Nonfinite injury feature")
            cases.append(case)
            record.update(eligible=True, reason="eligible", raw_loss_points=case["raw_loss_points"])
            ledger.append(record)
    args.output.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(cases, columns=CASE_COLUMNS).to_csv(args.output / "case_features.csv", index=False)
    pd.DataFrame(ledger).to_csv(args.output / "case_eligibility.csv", index=False)
    pd.DataFrame(report_selection, columns=["game_id", "team", "report_ids", "report_published_at",
        "eligible", "reason"]).to_csv(args.output / "report_selection.csv", index=False)
    for filename, wanted in hashes.items():
        if sha(Path(filename)) != wanted:
            raise ValueError(f"Source changed during feature build: {filename}")
    (args.output / "feature_sources.json").write_text(json.dumps(hashes, indent=2), encoding="utf-8")
    print(f"Eligible cases: {len(cases)}; candidate rows: {len(ledger)}")
    if ledger:
        print(pd.DataFrame(ledger).to_string(index=False))


if __name__ == "__main__":
    main()
