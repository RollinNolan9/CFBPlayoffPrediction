# Six-Game OG Versus V3: Comparison Protocol

Locked September 9, 2026 before any new multi-season six-game fit or score.
This is a local protocol, not external preregistration. Existing 2022-2025 results
and the 2024 bracket are already known; none is an untouched research holdout.

## Stage 1: Data Gate

Run `Rscript run_cfb_og_audit.R`. Inventory the extracted original workspace,
v2/v3 rating ledgers, historical games, pregame Elo, six-game history availability,
and line provenance. Read databases in read-only mode. Do not fit models in the audit.

A season label, week number, filesystem modification time, or current download
time does not establish that a rating was published before a historical game.
Annual SP+ cannot pass the weekly prediction gate without separate dated evidence.
For each accepted SP+ snapshot require team, season, overall/offense/defense ratings,
availability time, data-through time, source URL and reviewed source evidence.
Both timestamps must precede the prediction cutoff. Reject missing values,
wrong-season rows, conflicting keys and unreviewed or future-dated evidence.

Use a preserved original publication or an authorized historical export. A mutable
page's old URL/date is insufficient if its contents were updated later. A ranking
position alone cannot replace the OG's three numeric SP+ inputs. Do not bypass
paywalls, backdate a new download, substitute prior-year SP+, or silently remove SP+.
If coverage remains unavailable, report that exact limitation before scoring.

## Models And Fit Timing

### September 9 Amendment: Corroborated Reconstruction

User approved corroborated author weekly tables with explicit reconstruction limits.
Add a separate exploratory lane; do not relax or relabel the strict data gate above.
Public weekly table dates, scheduled matchups, team records and dated numerical
release checks must agree. Preserve raw source hashes and record the checks. A
table with a conflicting record or a failed numerical check is not silently accepted.
The table's scheduled week determines its reconstructed cycle, NOT a claimed
publication timestamp. `available_at` stays unknown and strict verification stays
false unless independently established. Same-cycle Monday availability is an
explicit assumption; report a one-cycle-delayed sensitivity as well. No interpolation
or carry-forward across missing weekly cycles. Bowl tables are not reusable as
updated ratings throughout the playoff. Preseason/final tables are not gap fillers.

Reuse the three already fitted 2014-2021 forests only after checking their training
data and saved source hashes. Verify compact raw-play sufficient statistics against
the archived aggregator and complete feature table before scoring. Retain the
original week grouping, alias behavior, joins and home/away lookup. Log omissions.
Recovered Elo is corroborated against next-game pregame Elo on consecutive weeks;
unresolved Elo vintage remains a limitation, never a certified historical release.
Scores in this lane are reconstructed evidence, not a definitive point-in-time
comparison or a reproduction of the user's exact saved forest. Sparse CFP coverage
does not establish a whole-bracket verdict. No changes to production are authorized.

- Primary OG is `365ca12:model (1).Rmd`: six-game window, mtry 6, 500 trees,
  node size 5, team identities, EPA/WPA, SP+ overall/offense/defense, and Elo.
  Preserve original 2014-2021 training, exclusions, joins and latest-home/latest-away
  prediction lookup. The fixed 2014-2021 fit can be evaluated in 2022-2025 without
  adding any evaluated season's outcomes. No expanding-history retrain is silently
  introduced; that would be an explicitly named secondary challenger.
- Preserve original training-table construction in the faithful workflow lane and
  disclose its same-game training features and duplicate joins. Those can create
  train/predict mismatch; changing them creates a repaired OG, not the untouched OG.
  Evaluation inputs must never contain the game being predicted or later games.
  Training-source vintages also remain a reconstruction limitation where unverified.
- V3 keeps its current engine/configuration and season-by-season training through
  the prior season. Its 2024 playoff fit uses the already selected historical settings,
  not choices optimized on that playoff. Hash all engine, data and reference outputs.
- Primary OG seed 20260909; sensitivity seeds 20260910 and 20260911. Report all;
  do not select a seed, create a seed ensemble, or tune to recover the perfect bracket.
- No PFF experiment, new injury/coach feature, market blend, bowl exclusion, threshold
  search, production promotion, commit or push is part of this comparison.

This compares the original fixed-fit workflow with current v3, not one isolated
architectural change. Training-period advantages must be disclosed in the report.

## Matched Historical Evaluation

- Test seasons 2022-2025. Include completed FBS-vs-FBS regular-season games,
  conference championships and CFP. Exclude non-CFP bowls from evaluation only;
  keep each model's training/history bowl policy unchanged.
- The existing historical v3 reference uses Monday 00:00 UTC feature cutoffs.
  Use that same cutoff for the initial paired historical lane. This is a weekly
  pregame reconstruction, not a recreation of Friday publication inputs.
  A Friday-cutoff historical lane would require separately rebuilding both inputs.
- Report coverage before scores. Six prior same-season game rows are only a
  structural upper bound: original complete-case lookup, excluded teams and unseen
  team-side levels may reduce actual coverage further. Do not invent an OG preseason
  layer or report late-season-only accuracy as a full-season comparison.
- Join predictions one-to-one by game ID and compare exactly the same scored rows.
  Missing lines do not exclude margin/SU comparisons. ATS uses the identical finite
  closing spread for both models; record pushes and zero-edge no-selections separately.
- Closing-line ATS is a retrospective benchmark, not Friday bet execution. Claim
  realized-price returns only where provider, capture time, spread and both prices
  are preserved. Never assume every archived line was available at Friday 1 p.m.

## Metrics And Decision Rule

Primary forecast metric: paired margin MAE. Also report RMSE, signed margin bias,
straight-up accuracy, ATS W-L-P, coverage, and no-selection rates by model and seed.
Display each season, the article audience (historical power conferences plus
Notre Dame/UConn), spreads over 21, and CFP. Keep all-FBS and audience results distinct.

Report paired differences and 5,000 whole-season bootstrap draws with seed 20260909.
With four seasons, intervals are descriptive and fragile; also show leave-one-season-
out differences. Do not treat correlated games as thousands of independent trials.
Call a historical margin advantage supported only if its paired interval excludes
zero and it does not reverse when any one season is omitted. Apply the same reporting
standard to an ATS advantage. Mixed metrics or insufficient coverage mean inconclusive,
not permission to select the favorite metric. No automatic production promotion.

## CFP And Prospective Confirmation

Report frozen whole-bracket and round-updated CFP lanes separately. Frozen brackets
advance each model's own winners through explicit predecessor links, never actual
later-round opponents. Do not grade future-round lines as bets available before the
playoff. Keep the verified saved 2024 bracket distinct from new retrospective fits.

For future 2026 head-to-head cards, freeze predictions, source bytes/hashes, ratings,
provider lines/prices and timestamps at Friday 1 p.m. America/New_York. Do not include
games already kicked off. Grade immutable original picks, even when later revisions
would perform better. Begin the shared lane only when the OG has enough history;
retain v3-only early-season coverage as a separate operational benefit.

This audit does not schedule recurring jobs or manufacture historical snapshots.
If the SP+ gate fails, the next step is source recovery or explicit approval of a
clearly named modified OG. Do not produce a new ATS verdict from rejected inputs.
