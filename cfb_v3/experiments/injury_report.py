"""Render an auditable SEC RB pilot evaluation as local HTML and Markdown.

Usage: python cfb_v3/experiments/injury_report.py --evaluation PATH
No network requests, model changes, or new performance calculations are fitted.
"""
from __future__ import annotations

import argparse
import csv
import html
import json
import math
import os
from collections import Counter
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import quote, urlparse

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_PILOT = ROOT / "cfb_v3/output/experiments/injury_pilot"


def read_csv(path: Path, required: bool = True) -> list[dict]:
    if not path.exists() and not required:
        return []
    with path.open(encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def number(value):
    try:
        result = float(value)
        return result if math.isfinite(result) else None
    except (ValueError, TypeError):
        return None


def yes(value):
    return str(value).strip().lower() in {"true", "1", "1.0"}


def fmt(value, digits=2, signed=False):
    value = number(value)
    if value is None:
        return "—"
    return format(value, f"{'+' if signed else ''}.{digits}f")


def percentage(numerator, denominator):
    return "—" if not denominator else f"{100 * numerator / denominator:.1f}%"


def display_team(name):
    return "Ole Miss" if name == "Mississippi" else name


@dataclass
class Link:
    label: str
    url: str


def h(value):
    if isinstance(value, Link):
        return f'<a href="{html.escape(value.url, quote=True)}">{html.escape(value.label)}</a>'
    return html.escape(str(value))


def md(value):
    if isinstance(value, Link):
        return f"[{value.label.replace('[', '').replace(']', '')}]({value.url})"
    return str(value).replace("|", "\\|").replace("\n", " ")


class Document:
    def __init__(self):
        self.web, self.markdown = [], []

    def heading(self, text, level=2):
        self.web.append(f"<h{level}>{h(text)}</h{level}>")
        self.markdown.append(f"{'#' * level} {text}\n")

    def paragraph(self, text, callout=False):
        self.web.append(f'<p class="{"callout" if callout else ""}">{h(text)}</p>')
        self.markdown.append(text + "\n")

    def table(self, headers, rows, caption=""):
        if not rows:
            self.paragraph("No rows are available for this section.")
            return
        cap = f"<caption>{h(caption)}</caption>" if caption else ""
        head = "".join(f'<th scope="col">{h(cell)}</th>' for cell in headers)
        body = "".join("<tr>" + "".join(f"<td>{h(cell)}</td>" for cell in row) + "</tr>" for row in rows)
        self.web.append(f'<div class="table-wrap"><table>{cap}<thead><tr>{head}</tr></thead><tbody>{body}</tbody></table></div>')
        if caption:
            self.markdown.append(caption + "\n")
        self.markdown.append("| " + " | ".join(map(md, headers)) + " |\n| " + " | ".join("---" for _ in headers) + " |\n" +
                             "\n".join("| " + " | ".join(map(md, row)) + " |" for row in rows) + "\n")


CSS = """
:root{color-scheme:light;--ink:#17212b;--muted:#566372;--line:#dce2e7;--accent:#174c65}
*{box-sizing:border-box}body{margin:0;background:#f3f5f6;color:var(--ink);font:16px/1.6 system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
main{max-width:1240px;margin:32px auto;padding:42px 48px;background:#fff;border:1px solid var(--line);border-radius:8px}
.eyebrow{text-transform:uppercase;letter-spacing:.12em;color:var(--accent);font-size:12px;font-weight:700}
h1{font-size:clamp(29px,4vw,42px);line-height:1.15;letter-spacing:-.03em;margin:10px 0 22px}h2{font-size:23px;margin:40px 0 10px;line-height:1.3;border-top:1px solid var(--line);padding-top:24px}
h3{font-size:18px;margin-top:26px}p{max-width:100ch;margin:12px 0;color:#344250}.callout{border-left:4px solid var(--accent);background:#edf4f7;padding:15px 18px;color:var(--ink)}
.table-wrap{overflow:auto;margin:18px 0 26px}table{width:100%;border-collapse:collapse;font-size:14px;font-variant-numeric:tabular-nums}caption{text-align:left;color:var(--muted);padding-bottom:8px;font-size:13px}
th{background:#eef2f4;text-align:left;font-size:12px;line-height:1.4}th,td{padding:11px 12px;border-bottom:1px solid var(--line);vertical-align:top}tbody tr:nth-child(even){background:#fafbfc}td{min-width:60px}td:first-child{font-weight:600}
a{color:var(--accent);text-underline-offset:3px;overflow-wrap:anywhere}footer{margin-top:36px;color:var(--muted);font-size:12px;border-top:1px solid var(--line);padding-top:15px}
@media(max-width:720px){main{margin:0;padding:25px 18px;border:0;border-radius:0}th,td{padding:9px}h2{margin-top:30px}}
@media print{body{background:white}main{margin:0;border:0;padding:0;max-width:none}.table-wrap{overflow:visible}h2{break-after:avoid}tr{break-inside:avoid}a{color:inherit}table{font-size:10px}th,td{padding:6px}}
"""


def create_report(evaluation: Path, pilot: Path):
    evaluation, pilot = evaluation.resolve(), pilot.resolve()
    paired = read_csv(evaluation / "paired_predictions.csv")
    metrics = read_csv(evaluation / "metrics.csv")
    manifest = json.loads((evaluation / "manifest.json").read_text(encoding="utf-8-sig"))
    locations = {}

    def snapshot(name, fallback, required=True):
        path = evaluation / name
        if not path.exists():
            path = pilot / fallback
        locations[name] = path
        return read_csv(path, required)

    cases = snapshot("case_features.csv", "features/case_features.csv")
    eligibility = snapshot("case_eligibility.csv", "features/case_eligibility.csv", False)
    coverage = snapshot("coverage.csv", "source_data/coverage.csv")
    sources = snapshot("reports.csv", "source_data/reports.csv", False)
    by_game = {row["game_id"]: row for row in paired}
    if len(by_game) != len(paired):
        raise ValueError("Paired predictions contain duplicate game IDs")
    affected = [row for row in paired if yes(row.get("affected"))]
    affected_ids = {row["game_id"] for row in affected}
    if {case["game_id"] for case in cases} != affected_ids:
        raise ValueError("Feature cases and evaluated affected games disagree; use matching snapshots")
    if manifest.get("affected_games") is not None and int(manifest["affected_games"]) != len(affected_ids):
        raise ValueError("Manifest affected-game count does not match paired predictions")

    def local_link(path, label):
        return Link(label, quote(os.path.relpath(path, evaluation).replace("\\", "/"), safe="/._-"))

    def external(url, label):
        return Link(label, url) if urlparse(url).scheme in {"https", "http"} else label

    def measure(season, scale, slice_name="affected_games"):
        matches = [r for r in metrics if number(r.get("season")) == season and number(r.get("scale")) == scale and r.get("slice", "affected_games") == slice_name]
        if len(matches) > 1:
            raise ValueError("Duplicate season/scale/slice metrics")
        return matches[0] if matches else {}

    report_covered = [r for r in coverage if yes(r.get("both_rb_reports_covered", r.get("both_teams_covered")))]
    uncollected = [r for r in eligibility if "history_not_collected" in r.get("reason", "")]
    doc = Document()
    doc.heading("SEC running-back injury pilot", 1)
    doc.paragraph(f"Friday noon Pacific · Historical 2024–2025 evaluation · {len(affected_ids)} affected games", False)
    doc.paragraph(f"Dated RB reports were recovered before the cutoff for {len(report_covered)} of {len(coverage)} target games ({percentage(len(report_covered), len(coverage))}). "
                  f"The evaluated adjustment covers {len(cases)} eligible player absences in {len(affected_ids)} games. "
                  "This selected, incomplete pilot does not establish general predictive improvement or justify a production model change.", True)

    cover_rows = []
    for season in sorted({int(r["season"]) for r in coverage}):
        target = [r for r in coverage if int(r["season"]) == season]
        covered = [r for r in report_covered if int(r["season"]) == season]
        changed = [r for r in affected if int(r["season"]) == season]
        cover_rows.append([season, len(target), len(covered), percentage(len(covered), len(target)), len(changed)])
    doc.table(["Season", "Target games", "Reports before cutoff", "Coverage", "Affected games"], cover_rows)
    doc.paragraph("Report coverage means a recovered complete RB status list at an eligible time. It does not establish that every later update was recovered, or that all player histories were reconstructed. "
                  "No report or no player history is missing evidence, not evidence of a healthy roster. Unknown injuries remain unadjusted.")

    selected = number(manifest.get("selected_on_2024_scale"))
    doc.heading("Paired results on affected games")
    if selected is None:
        doc.paragraph("The evaluation did not select an adjustment multiplier on 2024. A selected-multiplier 2025 effect is unavailable.")
    else:
        dev = measure(2024, selected)
        test_base, test_selected = measure(2025, 0), measure(2025, selected)
        doc.paragraph(f"The 2024 affected-game comparison selected multiplier {selected:g} from 0, 0.5 and 1.0 by lowest margin MAE, with ties going to the smaller multiplier. "
                      f"The selection used {int(number(dev.get('games')) or 0)} games and was then held fixed for 2025.")
        before, after = number(test_base.get("margin_mae")), number(test_selected.get("margin_mae"))
        if before is not None and after is not None:
            delta = after - before
            direction = "lower" if delta < -1e-9 else "higher" if delta > 1e-9 else "unchanged"
            test_n = int(number(test_selected.get('games')) or 0)
            doc.paragraph(f"On {test_n} affected 2025 {'game' if test_n == 1 else 'games'}, margin MAE was {before:.2f} points at baseline and {after:.2f} with the selected multiplier: "
                          f"{fmt(delta, signed=True)} points, {direction}. This is a descriptive result on the recovered cases, not an estimate of performance across all SEC games.", True)
        else:
            doc.paragraph("No paired 2025 affected-game result is available for the selected multiplier.")

    def metric_table(slice_name):
        table = []
        for r in sorted([r for r in metrics if r.get("slice", "affected_games") == slice_name], key=lambda x: (float(x["season"]), float(x["scale"]))):
            season, scale = int(float(r["season"])), float(r["scale"])
            base_mae, mae = number(measure(season, 0, slice_name).get("margin_mae")), number(r.get("margin_mae"))
            n, correct = int(number(r.get("games")) or 0), int(number(r.get("su_correct")) or 0)
            ats = "–".join(str(int(number(r.get(k)) or 0)) for k in ("ats_wins", "ats_losses", "ats_pushes"))
            scope_ids = affected_ids if slice_name == "affected_games" else {g["game_id"] for g in report_covered}
            changed_picks = sum(yes(g.get(f"ats_side_changed_{scale:g}")) for g in paired if int(g["season"]) == season and g["game_id"] in scope_ids)
            table.append([season, f"{scale:g}" + (" · selected" if selected == scale else ""), n, fmt(mae),
                          fmt(None if base_mae is None or mae is None else mae - base_mae, signed=True),
                          f"{correct}/{n} ({percentage(correct, n)})", ats,
                          fmt(r.get("ats_pct"), 1) + ("%" if number(r.get("ats_pct")) is not None else ""),
                          changed_picks, int(number(r.get("ats_abstentions")) or 0), int(number(r.get("ats_missing_lines")) or 0)])
        doc.table(["Season", "Scale", "Games", "Margin MAE", "Δ MAE vs 0", "Straight-up", "ATS W–L–P", "ATS win %", "ATS pick changes", "ATS passes", "Missing lines"], table)

    metric_table("affected_games")
    doc.paragraph("Lower MAE is better; a negative Δ MAE is an improvement. ATS percentages exclude pushes, exact predicted-cover ties and missing lines. "
                  "ATS uses saved PBP reference lines, whose Friday bookmaker timestamps were not verified. These are reference-line diagnostics, not executable Friday bets or a profit record.")

    doc.heading("Each evaluated absence")
    doc.paragraph("Every margin below is from the named injured team's perspective: positive means that team is favored or won; negative means its opponent is favored or won. "
                  "Full adjusted uses multiplier 1.0 and all collected qualifying adjustments for the game. Raw loss is the signed player-versus-replacement estimate, before scaling; the adjustment subtracts it from that team's expected margin.")
    case_rows = []
    for case in sorted(cases, key=lambda x: (int(by_game[x["game_id"]]["season"]), by_game[x["game_id"]].get("game_date_pacific", ""), x["game_id"], x["player_name"])):
        game = by_game[case["game_id"]]
        if case["team"] not in {game["home"], game["away"]}:
            raise ValueError("Case team is not in its evaluated game")
        sign = 1 if case["team"] == game["home"] else -1
        vals = {key: number(game.get(key)) for key in ("margin_0", "margin_1", "actual_margin")}
        if any(value is None for value in vals.values()):
            raise ValueError("Nonfinite case margins in evaluation")
        change = abs(vals["margin_1"] - vals["actual_margin"]) - abs(vals["margin_0"] - vals["actual_margin"])
        case_rows.append([f"{game['season']} · {display_team(game['away'])} at {display_team(game['home'])}",
                          f"{case['player_name']} · {display_team(case['team'])}", fmt(case["raw_loss_points"], signed=True),
                          fmt(sign * vals["margin_0"], signed=True), fmt(sign * vals["margin_1"], signed=True),
                          fmt(sign * vals["actual_margin"], signed=True), fmt(change, signed=True)])
    doc.table(["Game", "Injured player / team", "Raw loss", "Baseline margin", "Full adjusted margin", "Actual margin", "Δ absolute error"], case_rows)
    negative = [case for case in cases if (number(case.get("raw_loss_points")) or 0) < 0]
    if negative:
        names = "; ".join(f"{r['player_name']} ({fmt(r['raw_loss_points'], signed=True)} points)" for r in negative)
        doc.paragraph(f"Negative raw losses are preserved: {names}. The formula therefore raises the team's forecast when that player is absent because the sampled replacements had higher raw PPA. "
                      "This counterintuitive signed estimate reflects the method and sample, not proof that an injury helps a team.", True)

    doc.heading("Inputs and replacement assumptions")
    doc.paragraph("Raw loss = prior involvements per team game × (absent player's raw involvement PPA − weighted replacement raw involvement PPA). "
                  "Counts and means use the same finite-PPA cohort. Replacement weights come from prior involvement counts; absent players must be explicitly Out and have played in the most recent prior game. "
                  "Eligibility requires at least two prior team games and eight finite-PPA involvements per team game. "
                  "Questionable and Doubtful replacements are retained under the preset rule excluding only Out players; this is an uncertainty, not a claim that every backup was healthy.")
    input_rows, backup_rows = [], []
    for case in cases:
        input_rows.append([case["player_name"], int(float(case["prior_games"])), fmt(case["expected_involvements"]),
                           fmt(case["player_ppa"], 3), fmt(case["replacement_ppa"], 3), case.get("replacement_involvements", "—")])
        for backup in json.loads(case.get("replacement_players") or "[]"):
            backup_rows.append([case["player_name"], backup.get("name", "—"), fmt(100 * float(backup["weight"]), 1) + "%",
                                fmt(backup.get("ppa"), 3), backup.get("reported_status", "unknown")])
    doc.table(["Absent player", "Prior team games", "Involvements/game", "Player PPA", "Replacement PPA", "Replacement involvements"], input_rows)
    doc.table(["Replaces", "Backup", "Weight", "Raw PPA", "Reported status"], backup_rows)

    partial = [r for r in metrics if r.get("slice") == "report_covered_partial_history"]
    if partial:
        doc.heading("Secondary: reports-covered slate with partial player history")
        doc.paragraph("This slice includes games with recovered RB availability reports but applies only the collected qualifying adjustments. "
                      "Uncollected injuries remain unchanged. It is not a complete injury evaluation of the covered slate, and unchanged games must not be counted as additional injury evidence.", True)
        metric_table("report_covered_partial_history")

    doc.heading("Missing evidence and exclusions")
    doc.paragraph(f"The eligibility ledger identifies {len(uncollected)} reported absences whose team histories were not collected. "
                  "An opposing team's uncollected RB absence may therefore remain embedded only in the baseline; this pilot does not claim to isolate every injury in an affected matchup.")
    excluded = [r for r in eligibility if not yes(r.get("eligible"))]
    doc.table(["Game ID", "Team", "Player", "Exclusion / missing evidence"],
              [[r.get("game_id", "—"), display_team(r.get("team", "—")), r.get("player_name", "—"), r.get("reason", "—").replace("_", " ")] for r in excluded])
    reasons = Counter(r.get("reason", "unspecified") for r in excluded)
    if reasons:
        doc.paragraph("Exclusion counts: " + "; ".join(f"{key.replace('_', ' ')}: {value}" for key, value in sorted(reasons.items())) + ".")
    doc.paragraph("The historical archive did not yield systematic timestamped coverage. Report copies were recovered selectively and may omit intervening updates. "
                  "The initial-only September 2024 snapshot does not establish the latest Thursday status. Unresolved non-RB transcription differences are retained; the RB-only scope is accepted only where RB lists agree. "
                  "A Friday-evening or game-day downgrade cannot be used for a Friday-noon prediction.")

    doc.heading("What this method measures")
    doc.paragraph("This pilot uses raw CFBD involvement PPA: unique Rush, Reception, Target and receiving-touchdown Completion associations with finite play PPA, including failed fourth downs and fumbles. "
                  "Some reconstructed means do not exactly match the provider's stored player aggregates. The diagnostics remain visible; this is not an exact replication of provider player PPA or validation of the earlier 2.8-point Kewan Lacy scenario.")
    doc.paragraph("PPA depends on teammates, opponents, role and game context. Backup samples, uncertainty about who actually replaces the missing workload, incomplete histories and the small selected sample limit interpretation. "
                  "No causal injury effect, expected future MAE gain, ATS edge or production promotion is established by this report.")

    doc.heading("Evidence and reproducibility")
    evidence_rows = []
    seen = set()
    for case in cases:
        key = (case.get("source_url"), case.get("report_id"))
        if key in seen:
            continue
        seen.add(key)
        evidence_rows.append([case["player_name"], case.get("report_published_at", "unknown"),
                              case.get("timestamp_kind", "unknown").replace("_", " "),
                              case.get("evidence_grade", "unknown").replace("_", " "), external(case.get("source_url", ""), "Source")])
    doc.table(["Case", "Evidence timestamp (UTC)", "Timestamp evidence", "Source grade", "Report"], evidence_rows)
    doc.paragraph("Article publication times establish when the reported copy was available; they are not invented SEC release timestamps. "
                  "Retrieval times, original update times and source hashes remain separate in the source ledgers.")
    ledger_paths = [
        (evaluation / "paired_predictions.csv", "Paired predictions"), (evaluation / "metrics.csv", "All scale metrics"),
        (evaluation / "manifest.json", "Evaluation manifest"), (locations["case_features.csv"], "Case features"),
        (locations["case_eligibility.csv"], "Eligibility ledger"), (locations["coverage.csv"], "Target and report coverage"),
        (locations["reports.csv"], "Report metadata"), (pilot / "source_data/player_statuses.csv", "All source player statuses"),
        (pilot / "source_data/timing_exclusions.csv", "Late-report exclusions"), (pilot / "source_data/SOURCE_COVERAGE.md", "Source audit"),
        (pilot / "player_data/provider_population_audit.csv", "Provider population audit"),
        (pilot / "player_data/provider_role_coverage_audit.csv", "Provider role coverage audit"),
        (pilot / "player_data/ppa_mismatch_diagnostics.csv", "PPA mismatch diagnostics"),
        (ROOT / "cfb_v3/experiments/INJURY_PILOT_PROTOCOL.md", "Protocol"),
    ]
    doc.table(["Artifact", "Provenance"], [[local_link(path, label), "Evaluation snapshot" if path.parent == evaluation else "Supporting source file (may be updated independently)"]
                                           for path, label in ledger_paths if path.exists()])
    for case in cases:
        directory = Path(case.get("history_dir", ""))
        if directory.is_dir():
            for filename, label in (("manifest.json", "History manifest"), ("player_game_history.csv", "Prior player-game history")):
                path = directory / filename
                if path.exists():
                    doc.table(["Player", "Player-data evidence"], [[case["player_name"], local_link(path, label)]])
    created = manifest.get("created_at", "unknown")
    generated = datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z")
    doc.paragraph(f"Evaluation created: {created}. Report generated: {generated}. Multiplier selection used 2024 only. Production changed: {manifest.get('production_changed', 'unknown')}.")
    page = '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">' + \
           '<title>SEC running-back injury pilot</title><style>' + CSS + '</style></head><body><main><div class="eyebrow">CFB model research · availability pilot</div>' + \
           "\n".join(doc.web) + '<footer>Local research artifact. All margins and deltas are measured in points; PPA is identified separately.</footer></main></body></html>'
    (evaluation / "report.html").write_text(page, encoding="utf-8")
    (evaluation / "report.md").write_text("\n".join(doc.markdown), encoding="utf-8")
    return {"html": str(evaluation / "report.html"), "markdown": str(evaluation / "report.md"), "affected_games": len(affected_ids), "report_covered_games": len(report_covered)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evaluation", required=True, type=Path)
    parser.add_argument("--pilot", type=Path, default=DEFAULT_PILOT, help="Supporting source, feature and player-data directory")
    args = parser.parse_args()
    print(json.dumps(create_report(args.evaluation, args.pilot), indent=2))


if __name__ == "__main__":
    main()
