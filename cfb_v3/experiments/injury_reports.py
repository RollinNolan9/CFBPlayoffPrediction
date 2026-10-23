"""Recover timestamped SEC availability evidence; never infer release times.

Run with Python and lxml. Inputs are public report copies, not model outcomes.
Outputs are a deliberately incomplete source pilot, not a league-wide archive.
Use --offline to rebuild CSVs from the downloaded normalized evidence.
"""
from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from urllib.request import Request, urlopen
from zoneinfo import ZoneInfo

from lxml import html

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "cfb_v3/output/experiments/injury_pilot/source_data"
SHEET = "https://docs.google.com/spreadsheets/d/1m9NvaYU1N4ViI4MLrXLoTp5SdYYAp2tlWgxS5t9triM/pubhtml"
SOURCES = [
    dict(id="rebelwalk_20241107", parser="rebelwalk", season=2024,
         game_date="2024-11-09", away="Georgia", home="Mississippi",
         url="https://therebelwalk.com/2024/11/thursday-availability-report-ole-miss-and-georgia-update-lists-for-saturdays-game/",
         report_type="Thursday", evidence_grade="reported_copy_full_transcription"),
    dict(id="sec_sheet_20240904", parser="sec_sheet", season=2024,
         url="https://web.archive.org/web/20240905205904id_/" + SHEET + "?gid=1425434111&single=true&widget=false&chrome=false&headers=false",
         original_source_url=SHEET + "?gid=1425434111&single=true&widget=false&chrome=false&headers=false",
         archive_capture_at="2024-09-05T20:59:04Z", evidence_grade="official_report_archived_copy"),
    dict(id="sec_sheet_20241004", parser="sec_sheet", season=2024,
         url="https://web.archive.org/web/20241005020434id_/" + SHEET + "?gid=1425434111&single=true&chrome=false&headers=false",
         original_source_url=SHEET + "?gid=1425434111&single=true&chrome=false&headers=false",
         archive_capture_at="2024-10-05T02:04:34Z", evidence_grade="official_report_archived_copy"),
    dict(id="rebelwalk_20250925", parser="rebelwalk", season=2025,
         game_date="2025-09-27", away="LSU", home="Mississippi",
         url="https://therebelwalk.com/2025/09/thursdays-availability-report-ole-miss-vs-lsu/",
         report_type="Thursday", evidence_grade="reported_copy_full_transcription"),
    dict(id="insidearkansas_20241024", parser="insidearkansas", season=2024,
         game_date="2024-10-26", away="Arkansas", home="Mississippi State",
         url="https://insidearkansas.com/arkansas-mississippi-state-midweek-injury-report-2/",
         report_type="Thursday", evidence_grade="reported_copy_full_transcription"),
]


def clean(s):
    return re.sub(r"\s+", " ", s.replace("\xa0", " ")).strip()


def iso(dt):
    return dt.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


def team_name(s):
    return "Mississippi" if s == "Ole Miss" else s


def utc(s):
    return datetime.fromisoformat(s.replace("Z", "+00:00"))


def fetch(source):
    request = Request(source["url"], headers={"User-Agent": "SEC historical availability research (public reports)"})
    with urlopen(request, timeout=45) as response:
        raw = response.read()
    if raw.startswith(b"\x1f\x8b"):
        raw = gzip.decompress(raw)
    # lxml's HTML charset guess can corrupt curly dashes in article lists.
    doc = html.fromstring(raw.decode("utf-8", errors="replace"))
    (OUT / "raw_html").mkdir(exist_ok=True)
    (OUT / "raw_html" / (source["id"] + ".html")).write_bytes(raw)
    meta = {x.get("property") or x.get("name"): x.get("content") for x in doc.xpath("//meta")}
    # Save only report content and date metadata, excluding navigation, images and logos.
    content = doc.xpath("//*[contains(concat(' ',normalize-space(@class),' '),' entry-content ')]")
    if not content:
        content = doc.xpath("//article") or [doc]
    blocks = [clean(x.text_content()) for x in content[0].xpath(".//h2|.//h3|.//p|.//li")]
    tables = [[[clean(x.text_content()) for x in tr.xpath("./td")] for tr in table.xpath(".//tr")]
              for table in doc.xpath("//table")]
    return dict(source=source, retrieved_at=iso(datetime.now(timezone.utc)),
                source_sha256=hashlib.sha256(raw).hexdigest(),
                date_published=meta.get("article:published_time", ""),
                date_modified=meta.get("article:modified_time", ""),
                blocks=blocks, tables=tables)


def base_report(e, team, game_date, away, home, published, report_type, timestamp_kind):
    s = e["source"]
    return dict(report_id=f"{s['id']}_{game_date}_{team_name(team).replace(' ', '_')}",
                season=s["season"], game_date=game_date, away=away, home=home,
                team=team_name(team), report_published_at=published,
                timestamp_kind=timestamp_kind, report_type=report_type,
                source_url=s["url"], original_source_url=s.get("original_source_url", s["url"]),
                archive_capture_at=s.get("archive_capture_at", ""),
                source_article_modified_at=e.get("date_modified", ""),
                team_report_complete=True, rb_report_complete=True, rb_scope_agreement=True,
                non_rb_source_disagreement=False, coverage_scope="full_team_report",
                evidence_grade=s["evidence_grade"], source_sha256=e["source_sha256"],
                retrieved_at=e["retrieved_at"])


def parse_rebelwalk(e):
    s = e["source"]
    if not e["date_published"]:
        raise ValueError(f"{s['id']}: no original article timestamp")
    published = iso(utc(e["date_published"]))
    reports, players, current = [], [], None
    teams = {"Ole Miss": "Mississippi", "Georgia": "Georgia", "LSU": "LSU"}
    for line in e["blocks"]:
        heading = next((name for name in teams if re.fullmatch(name + r"\s+Availability Report", line, re.I)), None)
        if heading:
            current = base_report(e, teams[heading], s["game_date"], s["away"], s["home"],
                                  published, s["report_type"], "exact_article_publication")
            reports.append(current)
            continue
        if line.lower().startswith("game info"):
            current = None
        if current:
            m = re.fullmatch(r"(?:(QB|RB|WR|TE|OL|OT|OG|C|DL|DT|DE|EDGE|LB|ILB|OLB|DB|CB|S|K|P|LS)\s+)?(.+?)\s*[-–—]\s*(Out|Doubtful|Questionable|Probable|Available)(?:\s*\(.*\))?", line, re.I)
            if m:
                players.append(dict(report_id=current["report_id"], team=current["team"],
                                    player_name=clean(m[2]), jersey="", position=(m[1] or "").upper(),
                                    status=m[3].title(), source_text=line))
    if {r["team"] for r in reports} != {s["away"], s["home"]}:
        raise ValueError(f"{s['id']}: could not find complete both-team headings")
    if not players:
        raise ValueError(f"{s['id']}: no player statuses parsed")
    return reports, players


def parse_sec_sheet(e):
    reports, players = [], []
    source = e["source"]
    current = None
    rows = [r for table in e["tables"] for r in table if any(r)]
    game_date = published = report_type = away = home = ""
    for i, row in enumerate(rows):
        text = clean(" ".join(row))
        if re.match(r"Saturday,", text, re.I):
            date_text = re.sub(r"(?<=\d)(?:st|nd|rd|th)\b", "", text, flags=re.I)
            game_date = datetime.strptime(date_text.title() + f" {source['season']}", "%A, %B %d %Y").date().isoformat()
            current = None
        m = re.search(r"(.+?): Updated as of (\d{1,2}/\d{1,2}) at (\d{1,2}:\d{2} [AP]M) CT", text, re.I)
        if m:
            report_type = m[1]
            d = datetime.strptime(f"{source['season']}/{m[2]} {m[3]}", "%Y/%m/%d %I:%M %p").replace(tzinfo=ZoneInfo("America/Chicago"))
            published = iso(d)
        if text == "AT":
            names = [x for x in rows[i + 1] if x]
            if len(names) != 2:
                raise ValueError(f"Unexpected matchup row {names}")
            away, home = map(team_name, names)
        nonempty = [x for x in row if x]
        if len(nonempty) == 1 and team_name(nonempty[0]) in {away, home} and away:
            current = base_report(e, nonempty[0], game_date, away, home, published, report_type, "exact_report_header")
            reports.append(current)
        elif current and len(nonempty) == 3 and nonempty[2] in {"Out", "Doubtful", "Questionable", "Probable", "Available", "Game Time Decision"}:
            players.append(dict(report_id=current["report_id"], team=current["team"], player_name=nonempty[0],
                                jersey="", position=nonempty[1], status=nonempty[2], source_text=" | ".join(nonempty)))
    if not reports or not all(r["report_published_at"] for r in reports):
        raise ValueError(f"{source['id']}: missing report headers")
    return reports, players


def parse_insidearkansas(e):
    s = e["source"]
    reports, players, current = [], [], None
    for line in e["blocks"]:
        team = {"Arkansas Razorbacks": "Arkansas", "Mississippi State Bulldogs": "Mississippi State"}.get(line)
        if team:
            current = base_report(e, team, s["game_date"], s["away"], s["home"],
                                  iso(utc(e["date_published"])), "Thursday", "exact_article_publication")
            reports.append(current)
        if current:
            match = re.fullmatch(r"(\w+) (.+?) \((OUT|QUESTIONABLE|PROBABLE|DOUBTFUL|AVAILABLE)\)", line)
            if match:
                players.append(dict(report_id=current["report_id"], team=current["team"],
                                    player_name=match[2], jersey="", position=match[1],
                                    status=match[3].title(), source_text=line))
    assert len(reports) == 2 and len(players) == 15, "Incomplete Arkansas/MS State report parse"
    return reports, players


def import_moss_local_copy():
    """Use the separately audited, saved article; retain its non-RB conflict."""
    folder = OUT / "secondary_2025"
    path = folder / "moss_thursday_si.html"
    metadata = folder / "retrieval_metadata.json"
    if not path.exists() or not metadata.exists():
        return
    raw = path.read_bytes()
    m = next(x for x in json.loads(metadata.read_text(encoding="utf-8-sig")) if x["id"] == "moss_thursday_si")
    if hashlib.sha256(raw).hexdigest() != m["sha256"]:
        raise ValueError("Moss article hash does not match collector metadata")
    document = raw.decode("utf-8-sig")
    published = re.search(r'"datePublished":"([^"]+)"', document)[1]
    modified = re.search(r'"dateModified":"([^"]+)"', document)[1]
    doc = html.fromstring(document)
    nodes = doc.xpath("//h2|//p")
    start = next(i for i, node in enumerate(nodes) if "Thursday Student-Athlete Availability Report" in node.text_content())
    blocks = []
    for node in nodes[start + 1:start + 5]:
        blocks.append(dict(tag=node.tag, lines=[clean(t) for t in node.itertext() if clean(t)]))
    evidence = dict(source=dict(id="si_20251016_moss", parser="si_moss", season=2025,
                    game_date="2025-10-18", away="Texas A&M", home="Arkansas", url=m["url"],
                    report_type="Thursday", evidence_grade="reported_copy_non_rb_conflict"),
                    retrieved_at=m["retrieved_at_utc"], source_sha256=m["sha256"],
                    date_published=published, date_modified=modified, blocks=blocks,
                    notes="SI and On3 disagree about non-RB Anderson/Armstrong and David Oke statuses. Both full report copies list the same RB statuses: A&M Moss Out and Arkansas AJ Green Out, with no other RBs listed. Full-team reconciliation is withheld. RB-only coverage is accepted with this discrepancy explicit. The SI exact article timestamp is the cutoff evidence; On3 is corroboration only, since its exact hour was not recovered. Position RB and name apostrophe verified in official A&M pregame notes.")
    (OUT / "raw_normalized/si_20251016_moss.json").write_text(json.dumps(evidence, ensure_ascii=False, indent=2), encoding="utf-8")


def parse_si_moss(e):
    s = e["source"]
    reports, players, current = [], [], None
    for block in e["blocks"]:
        if block["tag"] == "h2":
            team = " ".join(block["lines"])
            assert team in {s["away"], s["home"]}
            current = base_report(e, team, s["game_date"], s["away"], s["home"],
                                  iso(utc(e["date_published"])), "Thursday", "exact_article_publication")
            current.update(team_report_complete=False, rb_report_complete=True, coverage_scope="full_report_copy_rb_scope_only",
                           non_rb_source_disagreement=True, rb_scope_agreement=True,
                           corroborating_source_urls="https://www.on3.com/news/leveon-moss-injury-update-texas-am-at-arkansas-thursday-availability-report-revealed/|https://gigemgazette.com/texas-a-m-unexpectedly-losing-senior-defender-to-injury-after-only-one-game-back|https://12thman.com/news/2025/10/13/mike-elko-press-conference-quotes-arkansas",
                           quality_notes=e["notes"])
            reports.append(current)
        else:
            assert current and block["lines"][0] == "Out"
            for line in block["lines"][1:]:
                position, name = line.split(" ", 1)
                if name == "Le\ufffdVeon Moss":
                    name = "Le'Veon Moss"
                players.append(dict(report_id=current["report_id"], team=current["team"], player_name=name,
                                    jersey="", position=position, status="Out", source_text=line))
    assert len(reports) == 2 and len(players) == 10
    corroboration_path = OUT / "secondary_2025/moss_thursday_on3_report_copy.json"
    corroboration = json.loads(corroboration_path.read_text(encoding="utf-8")) if corroboration_path.exists() else {}
    normalize = lambda name: re.sub(r"[^a-z0-9]", "", name.lower())
    for report in reports:
        team = report["team"]
        own_rbs = {(normalize(p["player_name"]), p["status"]) for p in players if p["team"] == team and p["position"] == "RB"}
        other_rbs = {(normalize(p["player_name"]), p["status"]) for p in corroboration.get(team, []) if p["position"] == "RB"}
        agreement = team in corroboration and own_rbs == other_rbs
        report["rb_scope_agreement"] = agreement
        report["rb_report_complete"] = agreement
        report["corroboration_copy_sha256"] = hashlib.sha256(corroboration_path.read_bytes()).hexdigest() if corroboration_path.exists() else ""
    return reports, players


def write_csv(path, rows, minimum):
    fields = list(dict.fromkeys(minimum + [key for row in rows for key in row]))
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def resolve_positions(players):
    """Retain printed positions; resolve one documented source typo separately."""
    for player in players:
        player["resolved_position"] = player["position"]
        player["position_resolution_source"] = player["source_url"]
    targets = [p for p in players
               if p["report_id"] == "rebelwalk_20241107_2024-11-09_Georgia"
               and p["player_name"] == "Roderick Robinson II"]
    if not targets:
        return
    evidence = [p for p in players
                if p["report_id"] == "sec_sheet_20241004_2024-10-05_Georgia"
                and p["player_name"] == "Roderick Robinson II" and p["position"] == "RB"]
    if len(targets) != 1 or len(evidence) != 1:
        raise ValueError("Roderick Robinson position correction needs one retained official RB source")
    target, prior = targets[0], evidence[0]
    if target["position"] != "CB" or utc(prior["report_published_at"]) >= utc(target["report_published_at"]):
        raise ValueError("Unexpected source position or timing for Roderick Robinson correction")
    target["resolved_position"] = "RB"
    target["position_resolution_source"] = prior["source_url"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--only", help="One source ID; existing cached sources are retained")
    args = parser.parse_args()
    (OUT / "raw_normalized").mkdir(parents=True, exist_ok=True)
    for source in SOURCES:
        path = OUT / "raw_normalized" / (source["id"] + ".json")
        if not args.offline and (not args.only or args.only == source["id"]):
            evidence = fetch(source)
            {"rebelwalk": parse_rebelwalk, "sec_sheet": parse_sec_sheet, "insidearkansas": parse_insidearkansas}[source["parser"]](evidence)
            path.write_text(json.dumps(evidence, ensure_ascii=False, indent=2), encoding="utf-8")
    import_moss_local_copy()
    schedule_path = OUT.parent / "baseline/schedule.csv"
    schedule = list(csv.DictReader(schedule_path.open(encoding="utf-8-sig")))
    reports, players = [], []
    for path in sorted((OUT / "raw_normalized").glob("*.json")):
        e = json.loads(path.read_text(encoding="utf-8"))
        rs, ps = {"rebelwalk": parse_rebelwalk, "sec_sheet": parse_sec_sheet, "si_moss": parse_si_moss, "insidearkansas": parse_insidearkansas}[e["source"]["parser"]](e)
        reports.extend(rs)
        players.extend(ps)
    for r in reports:
        matches = [g for g in schedule if g["game_date_pacific"] == r["game_date"] and g["away"] == r["away"] and g["home"] == r["home"]]
        if len(matches) > 1:
            raise ValueError("Ambiguous schedule match")
        g = matches[0] if matches else {}
        r["game_id"] = g.get("game_id", "")
        r["prediction_cutoff_utc"] = g.get("prediction_cutoff_utc", "")
        r["eligible_by_cutoff"] = bool(g and utc(r["report_published_at"]) <= utc(g["prediction_cutoff_utc"]))
        modified = r["source_article_modified_at"]
        r["content_update_after_cutoff"] = bool(g and modified and utc(modified) > utc(g["prediction_cutoff_utc"]))
        r["eligible_by_cutoff"] &= not r["content_update_after_cutoff"]
        r["exclusion_reason"] = "" if r["eligible_by_cutoff"] else ("not_in_target_schedule" if not g else "content_modified_after_cutoff" if r["content_update_after_cutoff"] else "report_after_cutoff")
    lookup = {r["report_id"]: r for r in reports}
    assert len(lookup) == len(reports), "Duplicate report IDs"
    for p in players:
        r = lookup[p["report_id"]]
        for key in ("game_id", "season", "game_date", "away", "home", "report_published_at", "source_url", "report_type", "eligible_by_cutoff"):
            p[key] = r[key]
    resolve_positions(players)
    write_csv(OUT / "reports.csv", reports, ["report_id", "game_id", "team", "report_published_at", "timestamp_kind", "source_url", "team_report_complete", "evidence_grade"])
    player_fields = ["report_id", "game_id", "team", "player_name", "position", "resolved_position", "position_resolution_source", "status"]
    write_csv(OUT / "player_statuses.csv", players, player_fields)
    write_csv(OUT / "rb_statuses.csv", [p for p in players if p["resolved_position"] == "RB"], player_fields)
    coverage = []
    for g in schedule:
        rs = [r for r in reports if r["game_id"] == g["game_id"]]
        covered = {r["team"] for r in rs if r["eligible_by_cutoff"] and r["team_report_complete"]}
        rb_covered = {r["team"] for r in rs if r["eligible_by_cutoff"] and r["rb_report_complete"] and r["rb_scope_agreement"]}
        coverage.append(dict(**g, recovered_reports=len(rs), covered_teams="|".join(sorted(covered)), both_teams_covered={g["home"], g["away"]}.issubset(covered),
                             rb_covered_teams="|".join(sorted(rb_covered)), both_rb_reports_covered={g["home"], g["away"]}.issubset(rb_covered)))
    write_csv(OUT / "coverage.csv", coverage, ["game_id", "season", "home", "away"])
    print(json.dumps(dict(reports=len(reports), player_rows=len(players),
                          source_typed_rb_rows=sum(p["position"] == "RB" for p in players),
                          rb_rows=sum(p["resolved_position"] == "RB" for p in players),
                          position_corrections=sum(p["resolved_position"] != p["position"] for p in players),
                          target_games=len(schedule), both_teams_covered=sum(c["both_teams_covered"] for c in coverage),
                          both_rb_reports_covered=sum(c["both_rb_reports_covered"] for c in coverage))))


if __name__ == "__main__":
    main()
