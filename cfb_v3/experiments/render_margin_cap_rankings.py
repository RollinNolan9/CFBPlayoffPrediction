"""Render comparison.csv and qa.json as a self-contained rankings comparison.

Usage: python render_margin_cap_rankings.py OUTPUT_DIRECTORY
This presentation helper does not fit models or modify its input data.
"""

import argparse
import csv
import html
import json
import math
from datetime import datetime
from pathlib import Path


VARIANTS = (("uncapped", "No margin cap", "Every point counts in the power fit."),
            ("cap_24", "24-point cap", "Power-fit margins stop at 24 points."),
            ("cap_42", "42-point cap", "Power-fit margins stop at 42 points."))
FIELDS = {"team", "conference", "wins", "losses"} | {
    f"{model}_{field}" for model in ("published", "uncapped", "cap_24", "cap_42")
    for field in ("rank", "rating")}


def esc(value):
    return html.escape(str(value), quote=True)


def numeric(value, field):
    try:
        result = float(value)
    except (TypeError, ValueError) as error:
        raise ValueError(f"Invalid {field}: {value!r}") from error
    if not math.isfinite(result):
        raise ValueError(f"Nonfinite {field}")
    return result


def integer(value, field):
    result = numeric(value, field)
    if result != int(result):
        raise ValueError(f"Noninteger {field}")
    return int(result)


def rating(value):
    return f"{value:+.1f}" if abs(value) >= .05 else "0.0"


def read_inputs(directory):
    metadata = json.loads((directory / "qa.json").read_text(encoding="utf-8-sig"))
    for field in ("season", "week", "team_count"):
        metadata[field] = integer(metadata[field], field)
    if metadata["week"] < 1 or metadata["team_count"] < 25:
        raise ValueError("Expected a positive target week and at least 25 teams")
    timestamp = str(metadata["as_of"]).strip()
    try:
        as_of = datetime.fromisoformat(timestamp.replace("Z", "+00:00"))
    except ValueError as error:
        raise ValueError("qa.json as_of must be an ISO timestamp") from error
    if as_of.tzinfo is None:
        raise ValueError("qa.json as_of must include its timezone")
    metadata["as_of"] = timestamp
    rows = []
    with (directory / "comparison.csv").open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        missing = FIELDS - set(reader.fieldnames or ())
        if missing:
            raise ValueError(f"comparison.csv is missing {sorted(missing)}")
        for source in reader:
            row = {key: source[key].strip() for key in FIELDS}
            for key in FIELDS - {"team", "conference"}:
                row[key] = numeric(row[key], key) if key.endswith("_rating") else integer(row[key], key)
            if not row["team"] or min(row["wins"], row["losses"]) < 0:
                raise ValueError("Missing team name or invalid record")
            if row["conference"] in ("", "NA", "N/A"):
                row["conference"] = "Independent / FBS"
            rows.append(row)
    if len(rows) != metadata["team_count"] or len({row["team"] for row in rows}) != len(rows):
        raise ValueError("Team count mismatch or duplicate teams")
    for model in ("published", "uncapped", "cap_24", "cap_42"):
        if sorted(row[f"{model}_rank"] for row in rows) != list(range(1, len(rows) + 1)):
            raise ValueError(f"{model} must have unique consecutive ranks")
    return rows, metadata


def top_card(rows, model, title, description):
    entries = []
    for row in sorted(rows, key=lambda x: x[f"{model}_rank"])[:25]:
        entries.append(
            f'<li><span class="position">{row[f"{model}_rank"]:02d}</span>'
            f'<div class="card-team"><strong>{esc(row["team"])}</strong>'
            f'<span>{esc(row["conference"])} <b aria-hidden="true">·</b> {row["wins"]}–{row["losses"]}'
            f' <b aria-hidden="true">·</b> <span class="published-ref" title="Published model rank with count predictors included">Pub. #{row["published_rank"]}</span></span></div>'
            f'<span class="card-rating">{rating(row[f"{model}_rating"])}<small>points</small></span></li>')
    return (f'<section class="ranking-panel" aria-labelledby="heading-{model}">'
            f'<header class="panel-heading"><p class="panel-kicker">Refitted model</p>'
            f'<h3 id="heading-{model}">{title}</h3><p>{description}</p></header>'
            f'<ol class="top-list" aria-label="{title} Top 25">{"".join(entries)}</ol></section>')


def comparison_row(row):
    top = any(row[f"{model}_rank"] <= 25 for model, _, _ in VARIANTS)
    attrs = " ".join(f'data-{model}="{row[f"{model}_rank"]}"' for model, _, _ in VARIANTS)
    search = esc(f'{row["team"]} {row["conference"]}'.lower())
    cells = "".join(f'<td class="variant-cell"><strong>#{row[f"{model}_rank"]}</strong>'
                    f'<span>{rating(row[f"{model}_rating"])} pts</span></td>' for model, _, _ in VARIANTS)
    return (f'<tr {attrs} data-search="{search}" data-top="{str(top).lower()}"'
            f'{"" if top else " hidden"}>'
            f'<th scope="row"><span class="table-team">{esc(row["team"])}</span>'
            f'<span class="table-meta">{esc(row["conference"])} <b aria-hidden="true">·</b> {row["wins"]}–{row["losses"]}</span></th>'
            f'{cells}<td class="published-cell"><strong>#{row["published_rank"]}</strong>'
            f'<span>{rating(row["published_rating"])} pts</span></td></tr>')


PAGE = r'''<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="color-scheme" content="light">
<title>Week __WEEK__ ranking comparison · Good Eats, Bad Beats</title>
<style>
:root{--paper:#f6f3ed;--ink:#192b24;--muted:#657067;--line:#cfd5ca;--green:#2f5743;--soft:#e9ede4;--rust:#9b472f}
*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);font-family:Arial,Helvetica,sans-serif}a{color:var(--green)}
.masthead{background:var(--ink);color:var(--paper);padding:22px 28px}.masthead-inner{max-width:1240px;margin:auto;display:flex;align-items:center;justify-content:space-between;gap:22px}.brand{font:700 25px Georgia,'Times New Roman',serif;letter-spacing:-.6px}.edition{font-size:11px;letter-spacing:1.6px;text-transform:uppercase}
main{max-width:1296px;margin:auto;padding:44px 28px 26px}.eyebrow{display:flex;align-items:center;gap:11px;color:var(--rust);font-size:11px;letter-spacing:1.7px;text-transform:uppercase;font-weight:700}.eyebrow:before{content:'';width:28px;height:3px;background:var(--rust)}
h1,h2,h3{font-family:Georgia,'Times New Roman',serif}h1{font-size:clamp(37px,5vw,60px);line-height:1.07;letter-spacing:-1.9px;margin:15px 0 17px;max-width:860px}.dek{font-size:17px;line-height:1.65;max-width:840px;margin:0}.dek strong{font-weight:700}.snapshot{display:flex;gap:18px 40px;flex-wrap:wrap;border-top:2px solid var(--ink);border-bottom:1px solid var(--line);padding:16px 0;margin:25px 0 27px}.snapshot strong{display:block;font-size:10px;text-transform:uppercase;letter-spacing:1.1px;margin-bottom:6px}.snapshot span{font-size:12px;color:var(--muted)}
.section-intro{display:flex;justify-content:space-between;align-items:baseline;gap:20px;margin:0 0 14px}h2{font-size:27px;letter-spacing:-.6px;margin:0}.section-intro p{font-size:12px;color:var(--muted);line-height:1.55;margin:0;max-width:510px}.three-up{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:20px;align-items:start}.ranking-panel{border:1px solid var(--line);border-top:4px solid var(--green)}.panel-heading{padding:20px 16px 16px;background:var(--soft);border-bottom:1px solid var(--line)}.panel-kicker{margin:0 0 7px!important;font-size:9px!important;letter-spacing:1.5px;text-transform:uppercase;font-weight:700;color:var(--green)!important}h3{font-size:27px;letter-spacing:-.65px;margin:0 0 8px}.panel-heading p{margin:0;font-size:11px;line-height:1.5;color:var(--muted)}
.top-list{list-style:none;margin:0;padding:0}.top-list li{display:grid;grid-template-columns:26px minmax(0,1fr) 58px;gap:9px;align-items:center;min-height:59px;padding:11px 13px;border-bottom:1px solid var(--line)}.top-list li:last-child{border-bottom:0}.top-list li:nth-child(even){background:rgba(233,237,228,.42)}.position{font:20px Georgia,'Times New Roman',serif;color:var(--muted);font-variant-numeric:tabular-nums}.card-team strong{display:block;font-size:14px;line-height:1.3;overflow-wrap:break-word}.card-team>span{display:block;font-size:9px;line-height:1.5;color:var(--muted);margin-top:4px}.card-team b,.table-meta b{padding:0 3px;font-weight:400}.published-ref{white-space:nowrap}.card-rating{text-align:right;font-size:20px;font-weight:700;letter-spacing:-.4px;font-variant-numeric:tabular-nums}.card-rating small{display:block;font-size:9px;font-weight:400;letter-spacing:0;color:var(--muted);margin-top:2px}
.reading-note{font-size:12px;line-height:1.65;color:var(--muted);margin:17px 0 38px;max-width:850px}.reading-note strong{color:var(--ink)}.comparison{border-top:2px solid var(--ink);padding-top:22px}.comparison .section-intro{margin-bottom:22px}.controls{display:flex;align-items:end;gap:16px;flex-wrap:wrap;margin-bottom:12px}.view-switch{display:flex;padding:4px;background:var(--soft);gap:4px}.view-switch button{border:0;padding:11px 14px;font:700 12px Arial,sans-serif;color:var(--ink);background:transparent;cursor:pointer}.view-switch button[aria-pressed="true"]{background:var(--ink);color:var(--paper)}.field{flex:1 1 220px;max-width:340px}.sort-field{max-width:200px;flex-basis:170px}.field label{display:block;font-size:11px;font-weight:700;margin-bottom:6px}input,select{width:100%;font:13px Arial,sans-serif;padding:11px 12px;background:#fff;border:1px solid #a4b09f;border-radius:0;color:var(--ink);height:40px}select{padding-right:25px}button:focus-visible,input:focus-visible,select:focus-visible,summary:focus-visible,.table-scroll:focus-visible,a:focus-visible{outline:3px solid var(--rust);outline-offset:3px}.result-count{font-size:11px;color:var(--muted);margin:0 0 11px;min-height:16px}.table-scroll{overflow-x:auto}table{border-collapse:collapse;table-layout:fixed;width:100%;min-width:660px}col.team-col{width:34%}col.variant-col{width:17%}col.published-col{width:15%}caption{text-align:left}thead{background:var(--ink);color:var(--paper)}thead th{font-size:10px;line-height:1.4;padding:14px 16px;text-align:right;letter-spacing:.7px;text-transform:uppercase}thead th:first-child{text-align:left}thead th[aria-sort="ascending"]:after{content:' ↓';font-size:13px;color:#d1dfc6}thead th:last-child{color:#d1d5cc}tbody tr{border-bottom:1px solid var(--line)}tbody tr:nth-child(even){background:rgba(233,237,228,.4)}tbody tr:hover{background:#e3e9dc}tbody th,tbody td{padding:15px 16px}tbody th{text-align:left;font-weight:400}.table-team{display:block;font-size:15px;font-weight:700;line-height:1.3}.table-meta{display:block;font-size:10px;color:var(--muted);line-height:1.4;margin-top:5px}.variant-cell,.published-cell{text-align:right;font-variant-numeric:tabular-nums}.variant-cell strong{display:block;font-size:19px}.variant-cell span,.published-cell span{display:block;font-size:11px;color:var(--muted);margin-top:5px}.published-cell strong{font-size:13px;font-weight:400;color:var(--muted)}.published-cell span{font-size:10px}.empty{font-size:13px;color:var(--muted);text-align:center;padding:30px;border-bottom:1px solid var(--line)}[hidden]{display:none!important}.sr-only{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0}
.table-scroll{position:relative}
.method{margin-top:27px;border-top:1px solid var(--line);padding-top:17px;color:var(--muted);font-size:12px;line-height:1.75}.method summary{font-size:13px;font-weight:700;cursor:pointer;color:var(--ink)}.method p{max-width:930px;margin:12px 0}.method strong{color:var(--ink)}.source-note{font-size:11px;line-height:1.7;color:var(--muted);max-width:930px;margin:18px 0}.source-note strong{color:var(--ink)}footer{display:flex;justify-content:space-between;flex-wrap:wrap;gap:12px 25px;border-top:2px solid var(--ink);padding-top:17px;margin-top:28px;font-size:10px;color:var(--muted);line-height:1.65}footer strong{color:var(--ink)}.timestamp{overflow-wrap:anywhere}
@media(max-width:1050px) and (min-width:851px){main{padding-left:18px;padding-right:18px}.three-up{gap:12px}.top-list li{padding:10px 9px;gap:6px;grid-template-columns:23px minmax(0,1fr) 48px}.card-team strong{font-size:13px}.card-team>span{font-size:8px}.card-rating{font-size:18px}.panel-heading{padding:17px 12px}h3{font-size:24px}}
@media(max-width:850px){.three-up{grid-template-columns:1fr;gap:24px}.ranking-panel{max-width:640px;width:100%;margin:auto}.top-list li{min-height:58px;grid-template-columns:32px minmax(0,1fr) 66px;gap:14px;padding:11px 18px}.card-team strong{font-size:16px}.card-team>span{font-size:10px}.card-rating{font-size:22px}.section-intro{display:block}.section-intro p{margin-top:9px;max-width:none}.panel-heading{padding:20px}.controls{gap:14px}.field{max-width:none}.sort-field{max-width:210px}}
@media(max-width:540px){.masthead{padding:19px 17px}.masthead-inner{display:block}.brand{font-size:23px}.edition{font-size:9px;margin-top:7px;letter-spacing:1.2px}main{padding:30px 16px 22px}h1{font-size:39px;letter-spacing:-1.4px}.dek{font-size:15px}.snapshot{gap:16px 28px}.snapshot .model-note{width:100%}.three-up{gap:22px}.top-list li{padding:11px 13px;gap:9px;grid-template-columns:27px minmax(0,1fr) 62px}.card-team strong{font-size:15px}.card-team>span{font-size:9px}.card-rating{font-size:21px}.view-switch{width:100%}.view-switch button{flex:1;padding-left:9px;padding-right:9px}.field{flex-basis:100%}.sort-field{max-width:none;flex-basis:100%}h2{font-size:25px}.reading-note{margin-bottom:29px}table{min-width:640px}thead th,tbody th,tbody td{padding-left:12px;padding-right:12px}.timestamp{width:100%}}
@media print{body{background:white}.masthead{background:white;color:var(--ink);border-bottom:2px solid var(--ink)}.controls,.result-count,.method{display:none}main{padding:22px 0}.three-up{grid-template-columns:repeat(3,minmax(0,1fr));gap:12px}.top-list li{padding:7px 6px;grid-template-columns:20px minmax(0,1fr) 43px;gap:5px;min-height:48px}.card-team strong{font-size:11px}.card-team>span{font-size:8px}.card-rating{font-size:16px}.ranking-panel{break-inside:avoid}.panel-heading{padding:12px 8px}h3{font-size:20px}.comparison{break-before:page}thead{background:#e9ede4;color:#192b24}tr{break-inside:avoid}.table-scroll{overflow:visible}}
</style>
</head>
<body>
<header class="masthead"><div class="masthead-inner"><div class="brand">Good Eats, Bad Beats</div><div class="edition">College football · __SEASON__</div></div></header>
<main>
<div class="eyebrow">Experimental rankings · Week __WEEK__</div>
<h1>Three versions of the Top 25</h1>
<p class="dek">The same field, with three treatments of blowout margins. <strong>All three models were refitted without the two game-count predictors.</strong> Compare the rankings below; the published model remains a separate reference.</p>
<div class="snapshot"><div><strong>Results included</strong><span>Through Week __COMPLETED_WEEK__ · __SEASON__</span></div><div><strong>The field</strong><span>__COUNT__ FBS teams · actual records</span></div><div class="model-note"><strong>Status</strong><span>Experimental comparison · no production replacement</span></div></div>
<div class="section-intro"><h2>The three Top 25s</h2><p>Ratings are neutral-field points relative to the field. <strong>Pub.</strong> is the published model's rank, with count predictors included.</p></div>
<div class="three-up">__CARDS__</div>
<p class="reading-note"><strong>Read across the versions.</strong> The cap changes how large past wins and losses enter the power calculation. Every team's record stays the same. These rankings alone do not show which version predicts future games better.</p>
<section class="comparison" aria-labelledby="comparison-heading">
<div class="section-intro"><h2 id="comparison-heading">Find a team. Compare the numbers.</h2><p>The default view includes any team appearing in at least one of the three experimental Top 25s. Search reaches the full FBS field.</p></div>
<div class="controls"><div class="view-switch" role="group" aria-label="Teams to display"><button id="top-view" type="button" aria-pressed="true">Top 25 union</button><button id="all-view" type="button" aria-pressed="false">All __COUNT__ teams</button></div><div class="field"><label for="team-search">Team or conference</label><input id="team-search" type="search" autocomplete="off" placeholder="Search all __COUNT__ teams"></div><div class="field sort-field"><label for="sort-model">Order by model rank</label><select id="sort-model"><option value="uncapped">No margin cap</option><option value="cap_24">24-point cap</option><option value="cap_42">42-point cap</option></select></div></div>
<p id="result-count" class="result-count" role="status" aria-live="polite">Showing __UNION_COUNT__ teams in the Top 25 union · ordered by No margin cap</p>
<div class="table-scroll" role="region" aria-label="Ranking comparison table; scroll horizontally on small screens" tabindex="0"><table><caption class="sr-only">Experimental model rankings and ratings, compared with the published model. All records are actual game results.</caption><colgroup><col class="team-col"><col class="variant-col"><col class="variant-col"><col class="variant-col"><col class="published-col"></colgroup><thead><tr><th scope="col">Team / record</th><th id="col-uncapped" scope="col" aria-sort="ascending">No cap</th><th id="col-cap_24" scope="col" aria-sort="none">24-point cap</th><th id="col-cap_42" scope="col" aria-sort="none">42-point cap</th><th scope="col">Published<span class="sr-only"> model with count predictors</span></th></tr></thead><tbody id="comparison-rows">__ROWS__</tbody></table></div>
<p id="no-results" class="empty" hidden>No teams match that search. Try a team name or conference.</p>
</section>
<details class="method"><summary>How this comparison was made</summary><p><strong>What changed:</strong> each of the three ridge models was refitted after removing the direct <code>source_games_diff</code> and <code>games_played_diff</code> predictors. This is a new fit, not a subtraction of terms from the old model. Real game counts remain available for the existing sample and history weighting.</p><p><strong>Margin caps:</strong> the 24- and 42-point versions clip observed home margins symmetrically before the home-field adjustment in the opponent-adjusted power fit. Power-dependent coaching inputs are rebuilt consistently. The uncapped version uses the full observed margins. Actual final scores, training targets, records and EPA remain uncapped.</p><p><strong>Reading the ratings:</strong> the three versions use the same game data through Week __COMPLETED_WEEK__ and the same neutral-field comparison across the FBS field. The published column shows the earlier model with the count predictors included. Differences from that reference reflect both removing those predictors and refitting; differences among the three experimental versions isolate the margin-cap choice within this setup.</p><p><strong>Research, not validation:</strong> a team's position here is a model estimate. This snapshot does not establish a forecasting improvement or a betting edge, and none of the three versions replaces the published model.</p>__VALIDATION_NOTE__</details>
__SOURCE_NOTE__
<footer><span><strong>Good Eats, Bad Beats</strong><br>Week __WEEK__ · experimental ranking comparison</span><span class="timestamp">Frozen snapshot: <time datetime="__AS_OF__">__AS_OF__</time><br>Published rank is a model reference, not the AP poll.</span></footer>
</main>
<script>
(() => {
  'use strict';
  const body = document.getElementById('comparison-rows');
  const rows = Array.from(body.rows);
  const search = document.getElementById('team-search');
  const sort = document.getElementById('sort-model');
  const top = document.getElementById('top-view');
  const all = document.getElementById('all-view');
  const count = document.getElementById('result-count');
  const empty = document.getElementById('no-results');
  const labels = {uncapped:'No margin cap',cap_24:'24-point cap',cap_42:'42-point cap'};
  let view = 'top';
  function update() {
    const query = search.value.trim().toLowerCase();
    const variant = sort.value;
    let shown = 0;
    rows.sort((a,b) => Number(a.getAttribute('data-'+variant))-Number(b.getAttribute('data-'+variant)));
    for (const row of rows) {
      const visible = query ? row.dataset.search.includes(query) : view === 'all' || row.dataset.top === 'true';
      row.hidden = !visible;
      if (visible) shown++;
      body.appendChild(row);
    }
    for (const name of Object.keys(labels)) document.getElementById('col-'+name).setAttribute('aria-sort', name === variant ? 'ascending' : 'none');
    top.setAttribute('aria-pressed', String(view === 'top'));
    all.setAttribute('aria-pressed', String(view === 'all'));
    const scope = query ? 'searching all '+rows.length+' teams' : view === 'all' ? 'full FBS field' : 'Top 25 union';
    count.textContent = 'Showing '+shown+' '+(shown === 1 ? 'team' : 'teams')+' · '+scope+' · ordered by '+labels[variant];
    empty.hidden = shown !== 0;
  }
  top.addEventListener('click', () => {view='top';search.value='';update();});
  all.addEventListener('click', () => {view='all';search.value='';update();});
  search.addEventListener('input',update);
  sort.addEventListener('change',update);
  update();
})();
</script>
</body>
</html>'''


def render(directory):
    rows, metadata = read_inputs(directory)
    source_note = str(metadata.get("data_note", "")).strip()
    validation_note = str(metadata.get("validation_note", "")).strip()
    replacements = {
        "__SEASON__": metadata["season"], "__WEEK__": metadata["week"],
        "__COMPLETED_WEEK__": metadata["week"] - 1, "__COUNT__": len(rows),
        "__AS_OF__": esc(metadata["as_of"]),
        "__CARDS__": "".join(top_card(rows, *variant) for variant in VARIANTS),
        "__ROWS__": "".join(comparison_row(row) for row in sorted(rows, key=lambda x: x["uncapped_rank"])),
        "__UNION_COUNT__": sum(any(row[f"{model}_rank"] <= 25 for model, _, _ in VARIANTS) for row in rows),
        "__SOURCE_NOTE__": f'<p class="source-note"><strong>Game data:</strong> {esc(source_note)}</p>' if source_note else "",
        "__VALIDATION_NOTE__": f'<p>{esc(validation_note)}</p>' if validation_note else "",
    }
    page = PAGE
    for token, value in replacements.items():
        page = page.replace(token, str(value))
    output = directory / "index.html"
    output.write_text(page, encoding="utf-8")
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_directory", type=Path)
    args = parser.parse_args()
    print(render(args.output_directory.resolve(strict=True)))


if __name__ == "__main__":
    main()
