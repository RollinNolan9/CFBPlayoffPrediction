"""Render the uncapped, no-direct-count, opponent-relative ranking candidate.

Usage: python render_relative_rankings.py OUTPUT_DIRECTORY
Reads rankings.csv and qa.json; writes only the standalone index.html.
"""

import argparse
import csv
import html
import json
import math
from datetime import datetime
from pathlib import Path


FIELDS = {"rank", "team", "conference", "wins", "losses", "rating", "published_rank",
          "published_rating", "no_counts_rank", "no_counts_rating",
          "rank_change_vs_published", "rank_change_vs_no_counts"}


def esc(value):
    return html.escape(str(value), quote=True)


def number(value, field, integer=False):
    result = float(value)
    if not math.isfinite(result) or integer and result != int(result):
        raise ValueError(f"Invalid {field}: {value!r}")
    return int(result) if integer else result


def points(value):
    return f"{value:+.1f}" if abs(value) >= .05 else "0.0"


def movement(change):
    label = f"Up {change} places" if change > 0 else f"Down {abs(change)} places" if change < 0 else "Unchanged rank"
    text = f"↑{change}" if change > 0 else f"↓{abs(change)}" if change < 0 else "—"
    tone = "up" if change > 0 else "down" if change < 0 else "flat"
    return f'<span class="movement {tone}" aria-label="{label}">{text}</span>'


def read_inputs(directory):
    qa = json.loads((directory / "qa.json").read_text(encoding="utf-8-sig"))
    for key in ("season", "week", "completed_week", "team_count"):
        qa[key] = number(qa[key], key, True)
    if qa["team_count"] < 25 or qa["completed_week"] >= qa["week"]:
        raise ValueError("Expected at least 25 teams and a pregame cutoff")
    if datetime.fromisoformat(str(qa["as_of"]).replace("Z", "+00:00")).tzinfo is None:
        raise ValueError("as_of must include its timezone")
    with (directory / "rankings.csv").open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        if not FIELDS <= set(reader.fieldnames or ()):
            raise ValueError(f"Missing ranking fields: {sorted(FIELDS-set(reader.fieldnames or ()))}")
        rows = []
        for source in reader:
            row = {key: source[key].strip() for key in FIELDS}
            for key in FIELDS - {"team", "conference"}:
                row[key] = number(row[key], key, integer=not key.endswith("rating"))
            if not row["team"] or min(row["wins"], row["losses"]) < 0:
                raise ValueError("Missing team or invalid record")
            for reference in ("published", "no_counts"):
                if row[f"rank_change_vs_{reference}"] != row[f"{reference}_rank"] - row["rank"]:
                    raise ValueError(f"Rank movement disagrees for {row['team']}: {reference}")
            if row["conference"] in ("", "NA", "N/A"):
                row["conference"] = "FBS"
            rows.append(row)
    if len(rows) != qa["team_count"] or len({row["team"] for row in rows}) != len(rows):
        raise ValueError("Team count mismatch or duplicate team names")
    for key in ("rank", "published_rank", "no_counts_rank"):
        if sorted(row[key] for row in rows) != list(range(1, len(rows)+1)):
            raise ValueError(f"Expected unique consecutive {key}")
    return sorted(rows, key=lambda row: row["rank"]), qa


def table_row(row):
    mobile = []
    reference_cells = []
    for reference, label in (("published", "Published"), ("no_counts", "No-count baseline")):
        arrow = movement(row[f"rank_change_vs_{reference}"])
        rank = row[f"{reference}_rank"]
        rating = points(row[f"{reference}_rating"])
        reference_cells.append(f'<td class="reference-cell">{arrow}<span class="reference-detail">from #{rank} · {rating} pts</span></td>')
        mobile.append(f'<span>{label}: {arrow} <span class="mobile-old">(#{rank}, {rating} pts)</span></span>')
    return (f'<tr data-rank="{row["rank"]}" data-search="{esc(row["team"]+" "+row["conference"])}"'
            f'{" hidden" if row["rank"]>25 else ""}>'
            f'<td class="rank">{row["rank"]:02d}</td>'
            f'<th scope="row"><span class="team">{esc(row["team"])}</span>'
            f'<span class="team-meta">{esc(row["conference"])} · {row["wins"]}–{row["losses"]}</span>'
            f'<span class="mobile-references">{"".join(mobile)}</span></th>'
            f'<td class="rating">{points(row["rating"])}<span class="unit">points</span></td>'
            f'{"".join(reference_cells)}</tr>')


PAGE = r'''<!doctype html>
<html lang="en">
<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><meta name="color-scheme" content="light">
<title>Week __WEEK__ Model Top 25 · Good Eats, Bad Beats</title>
<style>
:root{--paper:#f6f3ed;--ink:#192b24;--muted:#636e65;--line:#cfd5ca;--soft:#e8ede3;--green:#315d45;--rust:#9c472f}*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);font-family:Arial,Helvetica,sans-serif}
.masthead{background:var(--ink);color:var(--paper);padding:22px 26px}.masthead-inner{max-width:1100px;margin:auto;display:flex;align-items:center;justify-content:space-between;gap:20px}.brand{font:700 25px Georgia,'Times New Roman',serif;letter-spacing:-.6px}.edition{font-size:10px;text-transform:uppercase;letter-spacing:1.7px}main{max-width:1152px;margin:auto;padding:40px 26px 26px}.eyebrow{display:flex;align-items:center;gap:11px;font-size:10px;color:var(--rust);letter-spacing:1.6px;text-transform:uppercase;font-weight:700}.eyebrow:before{content:'';width:28px;height:3px;background:var(--rust)}h1{font:700 clamp(40px,6vw,63px)/1.06 Georgia,'Times New Roman',serif;letter-spacing:-2px;margin:14px 0 16px}.dek{font-size:16px;line-height:1.6;max-width:760px;margin:0}.snapshot{display:flex;flex-wrap:wrap;gap:15px 35px;border-top:2px solid var(--ink);border-bottom:1px solid var(--line);padding:15px 0;margin:24px 0 0}.snapshot strong{font-size:10px;letter-spacing:1px;text-transform:uppercase;display:block;margin-bottom:5px}.snapshot span{font-size:12px;color:var(--muted)}
.controls{display:flex;align-items:end;justify-content:space-between;gap:20px;margin:26px 0 12px}.views{display:flex;background:var(--soft);padding:4px;gap:4px}.views button{border:0;background:transparent;color:var(--ink);font:700 12px Arial,sans-serif;padding:11px 16px;cursor:pointer}.views button[aria-pressed="true"]{background:var(--ink);color:var(--paper)}.search-field{flex:0 1 330px}.search-field label{display:block;font-size:11px;font-weight:700;margin-bottom:6px}.search-field input{width:100%;background:#fff;border:1px solid #a5afa0;border-radius:0;padding:11px 12px;font:13px Arial,sans-serif;color:var(--ink)}button:focus-visible,input:focus-visible,summary:focus-visible{outline:3px solid var(--rust);outline-offset:3px}.result-count{font-size:11px;color:var(--muted);min-height:16px;margin:0 0 11px}
table{width:100%;table-layout:fixed;border-collapse:collapse}.rank-col{width:6%}.team-col{width:36%}.rating-col{width:16%}.reference-col{width:21%}thead{background:var(--ink);color:var(--paper)}thead th{font-size:10px;line-height:1.4;letter-spacing:.8px;text-transform:uppercase;text-align:left;padding:13px 12px}thead th:nth-child(n+3){text-align:right}thead .subheading{display:block;font-size:9px;font-weight:400;color:#ced8cd;letter-spacing:.15px;text-transform:none;margin-top:3px}tbody tr{border-bottom:1px solid var(--line)}tbody tr:nth-child(even){background:rgba(232,237,227,.45)}tbody tr:hover{background:#e3e9dc}tbody td,tbody th{padding:15px 12px;text-align:left}tbody th{font-weight:400}.rank{font:22px Georgia,'Times New Roman',serif;color:var(--muted);font-variant-numeric:tabular-nums;vertical-align:middle}.team{display:block;font-size:17px;font-weight:700;line-height:1.3;overflow-wrap:break-word}.team-meta{display:block;font-size:11px;line-height:1.45;color:var(--muted);margin-top:5px}.rating{text-align:right;font-size:27px;font-weight:700;letter-spacing:-.6px;font-variant-numeric:tabular-nums;white-space:nowrap}.unit{display:block;font-size:10px;font-weight:400;letter-spacing:0;color:var(--muted);margin-top:3px}.reference-cell{text-align:right;font-variant-numeric:tabular-nums}.movement{font-weight:700;font-size:17px}.up{color:var(--green)}.down{color:var(--rust)}.flat{color:var(--muted)}.reference-detail{display:block;font-size:10px;color:var(--muted);margin-top:6px}.mobile-references{display:none}[hidden]{display:none!important}.empty{padding:25px;text-align:center;color:var(--muted);font-size:13px;border-bottom:1px solid var(--line)}.reading-note{font-size:12px;line-height:1.7;color:var(--muted);max-width:900px;margin:18px 0}.reading-note strong{color:var(--ink)}details{border-top:1px solid var(--line);padding:17px 0;margin-top:24px;font-size:12px;line-height:1.75;color:var(--muted)}summary{font-weight:700;font-size:13px;color:var(--ink);cursor:pointer}details p{max-width:910px;margin:11px 0}.source-note{font-size:11px;color:var(--muted);line-height:1.7;max-width:910px;margin:6px 0 20px}.source-note strong{color:var(--ink)}footer{display:flex;justify-content:space-between;flex-wrap:wrap;gap:12px 20px;margin-top:18px;border-top:2px solid var(--ink);padding-top:16px;font-size:10px;line-height:1.6;color:var(--muted)}footer strong{color:var(--ink)}.stamp{overflow-wrap:anywhere}
@media(max-width:800px){.reference-col,.reference-header,.reference-cell{display:none}.rank-col{width:9%}.team-col{width:66%}.rating-col{width:25%}.mobile-references{display:block;font-size:10px;line-height:1.65;margin-top:7px;color:var(--muted)}.mobile-references>span{display:block}.mobile-references .movement{font-size:11px;margin-left:2px}.mobile-old{white-space:nowrap}.rating{vertical-align:top;padding-top:18px}.rank{vertical-align:top;padding-top:19px}.reference-explanation{max-width:650px}.team{font-size:16px}}
@media(max-width:540px){.masthead{padding:19px 17px}.masthead-inner{display:block}.brand{font-size:23px}.edition{font-size:9px;margin-top:7px;letter-spacing:1.2px}main{padding:29px 16px 22px}h1{font-size:45px;letter-spacing:-1.5px}.dek{font-size:14px}.snapshot{gap:14px 25px}.snapshot .model-snapshot{width:100%}.controls{flex-wrap:wrap;margin-top:23px;gap:16px}.views{width:100%}.views button{flex:1}.search-field{flex-basis:100%}.rank-col{width:10%}.team-col{width:65%}.rating-col{width:25%}thead th{font-size:9px;letter-spacing:.7px;padding:12px 6px}tbody td,tbody th{padding:14px 6px}.rank{font-size:18px;padding-top:17px}.team{font-size:14px}.team-meta{font-size:10px}.rating{font-size:23px;padding-top:15px}.unit{font-size:9px}.mobile-references{font-size:9px}.mobile-references .movement{font-size:10px}.reading-note{font-size:11px}.stamp{width:100%}}
@media print{body{background:white}.masthead{background:white;color:var(--ink);border-bottom:2px solid var(--ink)}main{padding-top:22px}.controls,.result-count,details{display:none}tr{break-inside:avoid}thead{background:#e8ede3;color:var(--ink)}tbody td,tbody th{padding-top:10px;padding-bottom:10px}.rating{font-size:23px}}
</style></head>
<body>
<header class="masthead"><div class="masthead-inner"><div class="brand">Good Eats, Bad Beats</div><div class="edition">College football · __SEASON__</div></div></header>
<main><div class="eyebrow">Week __WEEK__ · experimental candidate</div><h1>Model Top 25</h1>
<p class="dek">Uncapped margins, with the opponent-relative performance feature. The model is refitted without the two direct game-count predictors.</p>
<div class="snapshot"><div><strong>Results included</strong><span>Through Week __COMPLETED_WEEK__ · __SEASON__</span></div><div><strong>The field</strong><span>__COUNT__ FBS teams · actual records</span></div><div class="model-snapshot"><strong>The rating</strong><span>Neutral-field points above average FBS</span></div></div>
<div class="controls"><div class="views" role="group" aria-label="Rankings to display"><button id="top-view" type="button" aria-pressed="true">Top 25</button><button id="all-view" type="button" aria-pressed="false">All __COUNT__ teams</button></div><div class="search-field"><label for="search">Find a team or conference</label><input id="search" type="search" placeholder="Search all __COUNT__ teams" autocomplete="off"></div></div>
<p id="result-count" class="result-count" role="status" aria-live="polite">Showing the Top 25 of __COUNT__ teams</p>
<table aria-label="Experimental opponent-relative model rankings"><colgroup><col class="rank-col"><col class="team-col"><col class="rating-col"><col class="reference-col"><col class="reference-col"></colgroup><thead><tr><th scope="col">Rank</th><th scope="col">Team / record</th><th scope="col">Rating</th><th scope="col" class="reference-header">Vs. published<span class="subheading">original model</span></th><th scope="col" class="reference-header">Vs. no-count baseline<span class="subheading">relative feature absent</span></th></tr></thead><tbody id="rankings">__ROWS__</tbody></table>
<p id="no-results" class="empty" hidden>No teams match your search.</p>
<p class="reading-note"><strong>Read the rating:</strong> +15.0 means the model rates a team about 15 points above an average FBS team on a neutral field. These are predictive power ratings, not a ranking of resumes.</p>
<p class="reading-note reference-explanation"><strong>Read the changes:</strong> ↑ means a higher rank than the reference. “Published” compares with the original model. “No-count baseline” compares with the uncapped refit that already removed the direct count predictors; that comparison isolates adding the relative feature and refitting. These are model comparisons, not week-to-week movement.</p>
<details><summary>How this candidate works</summary><p>The opponent-relative feature compares a team's past margins with how its opponents had performed before those games. It uses strictly earlier same-season results, with a fixed small-sample adjustment. All margins remain uncapped.</p><p>This 19-predictor foundation ridge is refitted after removing <code>source_games_diff</code> and <code>games_played_diff</code> and adding <code>opponent_relative_margin_diff</code>. Real game counts still inform the existing sample and history weighting; records stay unchanged.</p><p>Every team is compared with the FBS field on a neutral site. Ratings are centered at zero across that field. Team-label orientations are balanced; a direct matchup forecast can differ slightly from subtracting two ratings. AP rankings are not used.</p><p>This is a local experimental candidate using the same frozen game data as its references. It does not replace the published model or public rankings.</p></details>
__DATA_NOTE__
<footer><span><strong>Good Eats, Bad Beats</strong><br>Week __WEEK__ · uncapped + opponent-relative</span><span class="stamp">Snapshot: <time datetime="__AS_OF__">__AS_OF__</time><br>Experimental candidate · published model unchanged</span></footer></main>
<script>
(() => {
 'use strict';
 const rows=Array.from(document.querySelectorAll('#rankings tr'));
 const top=document.getElementById('top-view'),all=document.getElementById('all-view'),search=document.getElementById('search');
 let view='top';
 function update(){
  const query=search.value.trim().toLowerCase();let shown=0;
  for(const row of rows){const show=query?row.dataset.search.toLowerCase().includes(query):view==='all'||Number(row.dataset.rank)<=25;row.hidden=!show;if(show)shown++;}
  top.setAttribute('aria-pressed',String(view==='top'&&!query));all.setAttribute('aria-pressed',String(view==='all'||Boolean(query)));
  document.getElementById('result-count').textContent=query?shown+' '+(shown===1?'team':'teams')+' matching across the full FBS field':view==='top'?'Showing the Top 25 of '+rows.length+' teams':'Showing all '+rows.length+' teams';
  document.getElementById('no-results').hidden=shown!==0;
 }
 top.addEventListener('click',()=>{view='top';search.value='';update();});all.addEventListener('click',()=>{view='all';search.value='';update();});search.addEventListener('input',update);update();
})();
</script></body></html>'''


def render(directory):
    rows, qa = read_inputs(directory)
    note = str(qa.get("data_note", "")).strip()
    values = {"__SEASON__": qa["season"], "__WEEK__": qa["week"],
              "__COMPLETED_WEEK__": qa["completed_week"], "__COUNT__": len(rows),
              "__AS_OF__": esc(qa["as_of"]), "__ROWS__": "".join(table_row(row) for row in rows),
              "__DATA_NOTE__": f'<p class="source-note"><strong>Game data:</strong> {esc(note)}</p>' if note else ""}
    page = PAGE
    for key, value in values.items():
        page = page.replace(key, str(value))
    output = directory / "index.html"
    output.write_text(page, encoding="utf-8")
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    args = parser.parse_args()
    print(render(args.output_directory.resolve(strict=True)))
