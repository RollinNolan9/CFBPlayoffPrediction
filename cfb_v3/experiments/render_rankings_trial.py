"""Render saved-model power rankings as a standalone, offline HTML page."""

from __future__ import annotations

import argparse
import csv
import html
import json
import math
from datetime import datetime
from pathlib import Path


def escape(value: object) -> str:
    return html.escape(str(value), quote=True)


def integer(value: object) -> int:
    return int(float(str(value)))


def optional_number(value: object) -> float | None:
    try:
        number = float(str(value))
    except (ValueError, TypeError):
        return None
    return number if math.isfinite(number) else None


def canonical_team(value: object) -> str:
    name = str(value).strip()
    return {"Ole Miss": "Mississippi", "USC": "Southern California",
            "Miami (FL)": "Miami", "Miami (Fla.)": "Miami",
            "UConn": "Connecticut", "UMass": "Massachusetts",
            "NC State": "North Carolina State"}.get(name, name)


def render(directory: Path) -> Path:
    with (directory / "rankings.csv").open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        required = {"rank", "team", "conference", "wins", "losses", "rating", "source_games"}
        if not required.issubset(reader.fieldnames or []):
            raise ValueError("rankings.csv is missing required columns")
        rankings = list(reader)
    metadata = json.loads((directory / "qa.json").read_text(encoding="utf-8-sig"))
    season, week = integer(metadata["season"]), integer(metadata["week"])
    completed_week = integer(metadata.get("completed_week", max(0, week - 1)))
    ap_file = directory / "ap_poll.json"
    ap = json.loads(ap_file.read_text(encoding="utf-8-sig")) if ap_file.exists() else None
    supplement_file = directory / "epa_supplement.json"
    supplement_note = ""
    if supplement_file.exists():
        supplement = json.loads(supplement_file.read_text(encoding="utf-8-sig"))
        if supplement.get("epa_status") != "calculated_supplement":
            raise ValueError("epa_supplement.json must identify its calculated_supplement status")
        game_id = integer(supplement["game_id"])
        if game_id <= 0 or not supplement.get("away") or not supplement.get("home"):
            raise ValueError("Supplement must identify its game and both teams")
        matchup = f'{supplement["away"]} at {supplement["home"]}'
        source_url = str(supplement.get("source_url") or f"https://www.espn.com/college-football/playbyplay/_/gameId/{game_id}")
        source_name = str(supplement.get("source_name") or "ESPN play-by-play")
        if not source_url.startswith("https://"):
            raise ValueError("Supplement source must use an HTTPS URL")
        supplement_note = (
            '<p class="data-note"><strong>Game data:</strong> '
            f'{escape(matchup)} uses '
            f'<a href="{escape(source_url)}">{escape(source_name)}</a> '
            'with separately calculated EPA. This game\u2019s EPA is a calculated supplement, not official published '
            'cfbfastR EPA. The other games use the published cfbfastR feed.</p>'
        )
    elif metadata.get("data_note"):
        supplement_note = f'<p class="data-note"><strong>Game data:</strong> {escape(metadata["data_note"])}</p>'
    completion = metadata.get("non_epa_source_completion", {})
    if supplement_file.exists() and completion.get("note"):
        supplement_note += (
            '<p class="data-note"><strong>Additional source limitation:</strong> '
            f'{escape(completion["note"])}</p>'
        )
    ap_ranks = {}
    ap_notice = ""
    if ap:
        same_week = optional_number(ap.get("season")) == season and optional_number(ap.get("week")) == week
        current_poll = same_week and ap.get("status") in {"published", "available", "current", "released"}
        if current_poll and ap.get("poll_date") and len(ap.get("teams", [])) >= 25:
            for item in ap["teams"]:
                team, rank = canonical_team(item["team"]), integer(item["rank"])
                if rank < 1 or rank > 25 or team in ap_ranks:
                    raise ValueError("Invalid or duplicate AP poll entry")
                ap_ranks[team] = rank
            ap_notice = (
                f'<strong>AP comparison \u00b7 {escape(ap["poll_date"])}</strong> '
                'The AP column shows the current published poll. AP rankings are not an input to these model ratings. '
                '<a href="https://apnews.com/hub/ap-top-25-college-football-poll">View the official AP poll</a>.'
            )
        elif same_week and ap.get("status") == "pending_release":
            release = "The new AP poll has not been released."
            local_release = str(ap.get("expected_release_local", ""))
            if local_release.endswith(" America/Los_Angeles"):
                try:
                    moment = datetime.strptime(local_release.split(" America/")[0], "%Y-%m-%d %H:%M")
                    hour = moment.strftime("%I").lstrip("0")
                    minutes = f":{moment.minute:02d}" if moment.minute else ""
                    meridiem = "a.m." if moment.hour < 12 else "p.m."
                    release = f'AP\u2019s {moment.strftime("%B")} {moment.day} poll is scheduled for {hour}{minutes} {meridiem} Pacific.'
                except ValueError:
                    pass
            ap_notice = (
                f'<strong>AP comparison pending</strong> {escape(release)} '
                'These model rankings are independent of AP voting. '
                '<a href="https://apnews.com/hub/ap-top-25-college-football-poll">Official AP poll</a>.'
                f'<span class="checked">Status checked {escape(ap.get("checked_at", "at generation"))}.</span>'
            )
        else:
            ap_notice = (
                '<strong>AP comparison unavailable</strong> A verified AP poll for this snapshot was not supplied. '
                'Prior polls are not shown as a current comparison.'
            )
    rankings.sort(key=lambda row: integer(row["rank"]))
    if len(rankings) != integer(metadata["team_count"]):
        raise ValueError("CSV team count differs from qa.json")
    if [integer(row["rank"]) for row in rankings] != list(range(1, len(rankings) + 1)):
        raise ValueError("Expected unique consecutive ranks")
    if len({row["team"] for row in rankings}) != len(rankings):
        raise ValueError("Duplicate team names")

    table_rows = []
    for row in rankings:
        rank = integer(row["rank"])
        rating = float(row["rating"])
        if not math.isfinite(rating):
            raise ValueError(f"Nonfinite rating for {row['team']}")
        conference = row["conference"] if row["conference"] not in ("NA", "") else "FBS"
        record = f"{integer(row['wins'])}\u2013{integer(row['losses'])}"
        rating_label = f"{rating:+.1f}" if abs(rating) >= 0.05 else "0.0"
        previous_rank = optional_number(row.get("previous_rank"))
        rank_change = optional_number(row.get("rank_change"))
        rating_change = optional_number(row.get("rating_change"))
        movement = ""
        if previous_rank is not None:
            change = int(previous_rank) - rank if rank_change is None else int(rank_change)
            symbol = "\u25b2" if change > 0 else "\u25bc" if change < 0 else "\u2014"
            movement_label = f"{symbol}{abs(change)}" if change else symbol
            direction = "up" if change > 0 else "down" if change < 0 else "flat"
            movement = (f'<span class="movement {direction}" title="Previous rank: {int(previous_rank)}" '
                        f'aria-label="Previous rank {int(previous_rank)}">{movement_label}</span>')
        rating_delta = (f'<span class="rating-change">{rating_change:+.1f} vs. prior</span>'
                        if rating_change is not None else "")
        ap_cell = ""
        if ap_ranks:
            ap_rank = ap_ranks.get(canonical_team(row["team"]))
            ap_cell = f'<td class="ap-rank">{ap_rank if ap_rank is not None else "NR"}</td>'
        search_text = f"{row['team']} {conference}".casefold()
        table_rows.append(
            f'<tr data-rank="{rank}" data-search="{escape(search_text)}"'
            f'{" hidden" if rank > 25 else ""}>'
            f'<td class="rank">{rank:02d}{movement}</td>'
            f'<th scope="row"><span class="team">{escape(row["team"])}</span>'
            f'<span class="conference">{escape(conference)}</span></th>'
            f'<td class="record">{record}</td>'
            f'<td class="rating">{rating_label}<span class="unit"> pts</span>{rating_delta}</td>{ap_cell}</tr>'
        )

    count = len(rankings)
    cutoff = escape(metadata["as_of"])
    pairs = f"{integer(metadata['pair_count']):,}"
    parity = float(metadata["max_prediction_error"])
    replay = "exactly reproduced" if parity < 1e-8 else f"reproduced within {parity:.6g} points"
    if metadata.get("replay_scope") == "source_snapshot":
        source_week = integer(metadata["source_week"])
        replay_note = (
            f"The saved Week {source_week} forecasts were {replay} from their original frozen features. "
            f"The same fitted model is then applied to fresh Week {week} team inputs; "
            "the replay verifies the source forecasts, not equality between old and new team features."
        )
    else:
        replay_note = f"The saved weekly projections were {replay} before generating the rankings."
    if metadata.get("replay_note"):
        replay_note = str(metadata["replay_note"])
    source_stamp = (f' Saved model snapshot: {escape(metadata["source_model_as_of"])}.'
                    if metadata.get("source_model_as_of") else "")
    model_explanation = (
        "The saved v3 foundation ridge model powers these rankings. From Week 5 onward, its scheduled "
        "preseason roster component is switched off."
        if week >= 5 else
        "The saved v3 model combines a ridge margin model with faded preseason roster priors."
    )
    if metadata.get("model_family") == "uncapped_opponent_relative":
        model_explanation = (
            "The saved opponent-relative ridge model powers these rankings, using uncapped margins "
            "and no direct game-count predictors. Opponent-relative performance compares a team's "
            "past margins with opponents' results available before each game. The explicit preseason "
            "roster component is off; historical performance still informs other features."
        )
    has_movement = any(optional_number(row.get("previous_rank")) is not None for row in rankings)
    movement_note = (
        "Arrows show rank movement from the previous saved model snapshot; rating changes appear below the current rating."
        if has_movement else "Movement will appear when a previous model snapshot is available."
    )
    if has_movement and week == 5:
        movement_note += " This update includes new game results and the scheduled Week 5 switch to the foundation model."
    if metadata.get("movement_note"):
        movement_note = str(metadata["movement_note"])
    ap_section = f'<aside class="ap-notice">{ap_notice}</aside>' if ap_notice else ""
    public_metadata = {
        key: metadata[key]
        for key in ("season", "week", "as_of", "team_count", "pair_count", "max_prediction_error",
                    "neutral_label_bias", "max_matchup_residual", "replay_scope", "source_week",
                    "source_model_as_of", "snapshot_mode")
        if key in metadata
    }
    # Protect the script terminator even when values originate in data files.
    json_payload = json.dumps(public_metadata, ensure_ascii=True).replace("<", "\\u003c").replace(
        ">", "\\u003e").replace("&", "\\u0026")
    page = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="color-scheme" content="light">
<title>Good Eats, Bad Beats Model Top 25</title>
<style>
:root{--paper:#f6f3ed;--ink:#192521;--muted:#626962;--line:#d7d9ce;--accent:#aa472f;--soft:#e8ede5}
*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);font-family:Arial,Helvetica,sans-serif}
.masthead{background:var(--ink);color:var(--paper);padding:22px 24px}.masthead-inner{max-width:960px;margin:auto;display:flex;justify-content:space-between;align-items:center;gap:18px}
.brand{font:700 23px Georgia,'Times New Roman',serif;letter-spacing:-.5px}.edition{font-size:11px;text-transform:uppercase;letter-spacing:1.8px;white-space:nowrap}
main{max-width:960px;margin:auto;padding:46px 24px 32px}.eyebrow{color:var(--accent);font-size:11px;letter-spacing:1.8px;font-weight:800;text-transform:uppercase;display:flex;align-items:center;gap:12px}.eyebrow:before{content:'';width:30px;height:3px;background:var(--accent)}
h1{font:700 clamp(36px,6vw,62px)/1.05 Georgia,'Times New Roman',serif;letter-spacing:-2px;margin:15px 0 18px}.dek{font-size:17px;line-height:1.55;max-width:690px;margin:0 0 24px}
.snapshot{display:flex;gap:10px 30px;flex-wrap:wrap;border-top:2px solid var(--ink);border-bottom:1px solid var(--line);padding:16px 0;font-size:12px;line-height:1.55}.snapshot strong{display:block;font-size:11px;text-transform:uppercase;letter-spacing:1px;margin-bottom:3px}.snapshot span{color:var(--muted)}
.ap-notice{font-size:12px;line-height:1.65;border-left:3px solid var(--accent);padding:12px 15px;margin:20px 0 0;background:#eee9df}.ap-notice strong{display:block;color:var(--ink)}.ap-notice a{color:var(--accent)}.checked{display:block;color:var(--muted);font-size:10px;margin-top:4px}
.controls{display:flex;align-items:end;justify-content:space-between;gap:18px;margin:28px 0 13px}.views{display:flex;gap:5px;background:var(--soft);padding:4px;border-radius:3px}.views button{font:700 12px Arial,sans-serif;color:var(--ink);background:transparent;border:0;padding:11px 16px;border-radius:2px;cursor:pointer}.views button[aria-pressed="true"]{background:var(--ink);color:white}
.search-wrap{flex:0 1 310px}.search-wrap label{display:block;font-size:11px;font-weight:700;margin-bottom:6px}.search-wrap input{font:14px Arial,sans-serif;width:100%;padding:11px 12px;background:#fff;border:1px solid #a7afa4;border-radius:2px;color:var(--ink)}
button:focus-visible,input:focus-visible,summary:focus-visible{outline:3px solid var(--accent);outline-offset:3px}.result-count{font-size:12px;color:var(--muted);margin:0 0 10px;min-height:16px}
table{width:100%;border-collapse:collapse;table-layout:fixed}col:first-child{width:64px}col:nth-child(3){width:110px}col:nth-child(4){width:150px}thead{background:var(--ink);color:white}thead th{font-size:10px;letter-spacing:1.4px;text-transform:uppercase;padding:13px 10px;text-align:left}thead th:nth-child(n+3){text-align:right}thead th:last-child{padding-right:18px}
tbody tr{border-bottom:1px solid var(--line)}tbody tr:nth-child(even){background:rgba(232,237,229,.42)}tbody tr:hover{background:#e5eadf}tbody th,tbody td{padding:15px 10px;text-align:left}tbody th{font-weight:400}.rank{font:400 20px Georgia,'Times New Roman',serif;color:var(--muted);font-variant-numeric:tabular-nums}.team{display:block;font-weight:700;font-size:17px;line-height:1.2}.conference{display:block;color:var(--muted);font-size:11px;margin-top:5px}.record{text-align:right;font-size:14px;font-variant-numeric:tabular-nums}.rating{text-align:right;padding-right:18px;font-size:24px;font-weight:800;letter-spacing:-.5px;font-variant-numeric:tabular-nums}.unit{font-size:10px;font-weight:400;letter-spacing:0;color:var(--muted)}
.movement{display:block;font:700 10px Arial,sans-serif;margin-top:5px;letter-spacing:0}.movement.up{color:#36654b}.movement.down{color:var(--accent)}.rating-change{display:block;font-size:10px;font-weight:400;color:var(--muted);letter-spacing:0;margin-top:4px}.ap-rank{text-align:right;font-size:14px;font-variant-numeric:tabular-nums;padding-right:18px}col.ap-col{width:64px}.has-ap col:nth-child(4){width:128px}
[hidden]{display:none!important}.no-results{padding:35px;text-align:center;border-bottom:1px solid var(--line);font-size:14px;color:var(--muted)}
.reading-note{font-size:13px;line-height:1.6;margin:19px 0 28px;max-width:700px}.reading-note strong{color:var(--accent)}details{border-top:1px solid var(--line);padding:17px 0;font-size:12px;line-height:1.7;color:var(--muted)}summary{cursor:pointer;font-weight:700;color:var(--ink);font-size:13px}details p{max-width:740px;margin:10px 0}.stamp{font-size:11px;overflow-wrap:anywhere}footer{border-top:2px solid var(--ink);padding-top:17px;margin-top:10px;display:flex;justify-content:space-between;gap:20px;font-size:11px;color:var(--muted)}footer strong{color:var(--ink)}
.data-note{font-size:12px;line-height:1.65;color:var(--muted);max-width:740px;margin:-12px 0 25px}.data-note strong{color:var(--ink)}.data-note a{color:var(--ink)}
@media(max-width:560px){.masthead{padding:18px}.brand{font-size:20px}.edition{font-size:9px;letter-spacing:1px}main{padding:30px 16px 24px}h1{letter-spacing:-1.3px}.dek{font-size:15px}.snapshot{gap:13px 22px}.controls{flex-wrap:wrap;margin-top:23px}.views{width:100%}.views button{flex:1}.search-wrap{flex-basis:100%}.snapshot .freeze{width:100%}col:first-child{width:38px}col:nth-child(3){width:56px}col:nth-child(4){width:82px}thead th{padding:12px 6px;font-size:9px;letter-spacing:.6px}thead th:last-child{padding-right:8px}tbody th,tbody td{padding:13px 6px}.team{font-size:14px}.rank{font-size:17px}.record{font-size:12px}.rating{font-size:21px;padding-right:8px}.unit{display:none}.conference{font-size:10px}footer{font-size:10px}}
@media(max-width:560px){col.ap-col{width:34px}.has-ap col:nth-child(3){width:44px}.has-ap col:nth-child(4){width:74px}.ap-rank{font-size:12px;padding-right:8px}.rating-change{font-size:8px}.movement{font-size:9px}}
@media print{body{background:white}.controls,.result-count,details{display:none}.masthead{background:white;color:var(--ink);border-bottom:2px solid var(--ink)}main{padding-top:25px}tbody th,tbody td{padding-top:8px;padding-bottom:8px}tr{break-inside:avoid}h1{font-size:40px}.rating{font-size:18px}.team{font-size:14px}}
</style>
</head>
<body>
<header class="masthead"><div class="masthead-inner"><div class="brand">Good Eats, Bad Beats</div><div class="edition">College football &middot; __SEASON__</div></div></header>
<main>
<div class="eyebrow">The model's view &middot; Through Week __COMPLETED_WEEK__</div>
<h1>Model Top 25</h1>
<p class="dek">Every FBS team. The same neutral-field test. Who the model expects to play best against the field.</p>
<div class="snapshot"><div><strong>Snapshot</strong><span>Through Week __COMPLETED_WEEK__ &middot; Week __WEEK__ pregame</span></div><div><strong>The field</strong><span>__COUNT__ FBS teams</span></div><div class="freeze"><strong>Frozen inputs</strong><span>No completed Week __WEEK__ results included</span></div></div>
__AP_SECTION__
<div class="controls"><div class="views" role="group" aria-label="Ranking view"><button id="top-view" type="button" aria-pressed="true">Top 25</button><button id="all-view" type="button" aria-pressed="false">All __COUNT__ teams</button></div><div class="search-wrap"><label for="search">Find a team or conference</label><input id="search" type="search" placeholder="Search the full FBS field" autocomplete="off"></div></div>
<p class="result-count" id="result-count" role="status">Showing the top 25 of __COUNT__ teams</p>
<table class="__TABLE_CLASS__" aria-label="Neutral-field model power rankings"><colgroup><col><col><col><col>__AP_COL__</colgroup><thead><tr><th scope="col">Rank</th><th scope="col">Team</th><th scope="col">Record</th><th scope="col">Rating</th>__AP_HEADER__</tr></thead><tbody id="rankings">__ROWS__</tbody></table>
<p class="no-results" id="no-results" hidden>No teams match your search.</p>
<p class="reading-note"><strong>Read the rating:</strong> points above an average FBS team on a neutral field. A +15.0 rating means the model places that team about 15 points above the field average. These are predictive power rankings, not a ranking of resumes.</p>
__SUPPLEMENT_NOTE__
<details><summary>How the rankings work</summary><p>__MODEL_EXPLANATION__ It is reused without retraining. Every team faces the full FBS field on a neutral site; both team orientations are evaluated and averaged to remove the model's home-label offset. The resulting ratings are centered at zero across the field. AP votes are not an input.</p><p>This run uses __PAIRS__ ordered matchup evaluations. Records and team inputs are frozen before Week __WEEK__. __REPLAY_NOTE__ Ratings summarize the full matchup grid, so subtracting two ratings may differ from a direct matchup projection.</p><p>__MOVEMENT_NOTE__</p><p class="stamp">Snapshot cutoff: __CUTOFF__.__SOURCE_STAMP__</p></details>
<footer><strong>Good Eats, Bad Beats</strong><span>__SEASON__ &middot; Through Week __COMPLETED_WEEK__ &middot; Model power rankings</span></footer>
</main>
<script type="application/json" id="run-metadata">__METADATA__</script>
<script>
"use strict";
const metadata=JSON.parse(document.getElementById("run-metadata").textContent);
const rows=Array.from(document.querySelectorAll("#rankings tr"));
const topButton=document.getElementById("top-view");
const allButton=document.getElementById("all-view");
const search=document.getElementById("search");
let view="top";
function update(){
 const query=search.value.trim().toLocaleLowerCase();
 let visible=0;
 rows.forEach(row=>{const matches=row.dataset.search.toLocaleLowerCase().includes(query);const inView=!!query||view==="all"||Number(row.dataset.rank)<=25;row.hidden=!(matches&&inView);if(!row.hidden)visible++;});
 topButton.setAttribute("aria-pressed",String(view==="top"&&!query));
 allButton.setAttribute("aria-pressed",String(view==="all"||!!query));
 document.getElementById("result-count").textContent=query?`${visible} ${visible===1?"team":"teams"} matching your search across the full FBS field`:(view==="top"?`Showing the top 25 of ${metadata.team_count} teams`:`Showing all ${metadata.team_count} teams`);
 document.getElementById("no-results").hidden=visible>0;
}
topButton.addEventListener("click",()=>{view="top";search.value="";update();});
allButton.addEventListener("click",()=>{view="all";search.value="";update();});
search.addEventListener("input",update);
</script>
</body>
</html>
"""
    values = {
        "SEASON": str(season), "WEEK": str(week), "COUNT": str(count),
        "ROWS": "\n".join(table_rows), "PAIRS": pairs, "REPLAY_NOTE": escape(replay_note),
        "CUTOFF": cutoff, "METADATA": json_payload,
        "SOURCE_STAMP": source_stamp,
        "SUPPLEMENT_NOTE": supplement_note,
        "COMPLETED_WEEK": str(completed_week), "MODEL_EXPLANATION": escape(model_explanation),
        "MOVEMENT_NOTE": escape(movement_note), "AP_SECTION": ap_section,
        "TABLE_CLASS": "has-ap" if ap_ranks else "",
        "AP_COL": '<col class="ap-col">' if ap_ranks else "",
        "AP_HEADER": '<th scope="col">AP</th>' if ap_ranks else "",
    }
    for key, value in values.items():
        page = page.replace(f"__{key}__", value)
    output = directory / "index.html"
    output.write_text(page, encoding="utf-8")
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    args = parser.parse_args()
    print(render(args.output_directory.resolve()))
