# CFB Model V2 Implementation Guide

Last updated: 2026-08-30

This document records what changed during the model-rebuild session, why each change
was made, where it lives, and how to reproduce it. It distinguishes completed work
from generated local artifacts and planned work.

## 1. Preserve the Legacy Baseline

The original notebooks and `model_final.Rmd` remain available for comparison. The new
pipeline was built in `cfb_v2` and is launched through `run_cfb_v2.R`.

This kept the proven model available while allowing the data model, training rules,
and weekly workflow to change without silently altering the legacy path.

Key files:

- `model_final.Rmd`: documented legacy/refactored model entry point.
- `cfb_model_rebuild.R`: first rebuild implementation and reusable prediction helper.
- `run_cfb_v2.R`: production v2 command-line entry point.
- `cfb_v2/`: versioned feature, coach, model, storage, and workflow modules.

## 2. Remove School-Brand Memory

Raw home and away team names, conference labels, polls, market spreads, FPI, PFF, and
public power ratings are prohibited from the production football feature set.

The model receives numeric matchup differentials such as:

- opponent-adjusted power
- offensive and defensive EPA
- passing and rushing EPA
- success rate
- havoc generated and allowed
- regressed turnover rate
- prior-season and trailing form
- coach rating
- home-field points

The prohibition is enforced in `football_feature_names()` and again when fitting the
ensemble. This prevents Alabama, Clemson, or any other school name from acting as a
historical brand coefficient.

## 3. Restrict the Training Era and Weight Recent Games

The production training era now emphasizes post-COVID football:

- 2021 onward receives full eligibility.
- 2020 receives half weight.
- Pre-2020 games receive zero production weight.
- Recent seasons receive more weight through season decay.
- Non-CFP bowls receive zero weight.
- Conference championships and CFP games retain full weight.

The August 29 preseason power calculation uses completed 2023-2025 margins with
weights `1.00`, `0.35`, and `0.1225` for 2025, 2024, and 2023. It removes generic home
field before solving simultaneous opponent-adjusted ratings and shrinks extremes
toward the national mean.

Configuration lives in `cfb_v2/config.R`.

## 4. Build the Historical Foundation

`cfb_v2/historical_data.R` builds one chronological foundation from public sources:

1. Pull CFBD schedules and season-specific FBS membership.
2. Pull compact cfbfastR play-by-play.
3. Use ESPN metadata only to classify CFP, bowls, and conference championships.
4. Remove canceled or incomplete games.
5. Identify competitive scrimmage plays.
6. Remove kneels, spikes, clock-kill plays, and garbage time.
7. Aggregate team-game EPA, success, havoc, turnovers, and special teams.
8. Build strictly pregame team snapshots.
9. Join week-effective coach assignments and ratings.
10. Validate row counts, keys, dates, and leakage rules.

The reusable historical CSVs are committed under `cfb_v2/data` and `cfb_v2/inbox`.
The generated DuckDB database and public-source cache remain local and are ignored by
Git.

Run the foundation build with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R `
  --mode=build-foundation --seasons=2020:2025 --overwrite=true
```

## 5. Create Pregame Team Features

Each feature is calculated using information available before kickoff. Team snapshots
contain current-season form when available and historical priors when it is not.

Current-season form includes:

- last three eligible games
- last six eligible games
- season-to-date performance
- current EPA and success components

Historical form includes:

- prior-season efficiency
- trailing three-year efficiency
- opponent-adjusted power

The generic home-field value is 2.4 points and becomes exactly zero at a neutral site.
Team-specific home field remains a challenger.

Prior-season influence falls as current games accumulate and is capped at 10% from
Week 8 onward. Preseason-only columns use the configured Week 0-4 fade of
`100/60/30/10/0%`.

The production prediction adds a second guard around those columns: it blends 80%
foundation with 20% full-preseason challenger in Weeks 0/1, then fades the challenger
share to 12%/6%/2% in Weeks 2/3/4 and zero from Week 5. Postseason rows use an explicit
phase week of 99, so a source week reset can never reactivate preseason information.

## 6. Rebuild Coach Identity and Ratings

The coach system was changed from loose name joins to canonical identity and dated
assignment records.

Implemented controls include:

- one canonical `coach_id` per person
- team-season-week assignment intervals
- explicit interim flags
- dated midseason transitions
- overlap and duplicate-key failures
- strictly prior coach performance
- lower-level portability factors and caps
- capped postseason/title experience
- 65/35 and 70/30 recent-history candidate ratings

The initial production choice is 65% recent performance and 35% longer history. This
preserves evidence that winning coaches often continue to win without allowing old
Saban-era Alabama or pre-portal Clemson results to dominate current teams.

Key files:

- `cfb_v2/coaches.R`
- `cfb_v2/coach_migration.R`
- `cfb_v2/data/coach_identities.csv`
- `cfb_v2/data/coach_transitions.csv`
- `cfb_v2/inbox/coach_assignments.csv`
- `cfb_v2/inbox/coach_history.csv`

## 7. Select a Production Margin Model

The production core is an 80/20 blend of two weighted ridge regressions during the
preseason phase: the current-team foundation and the full-preseason challenger.
Numeric inputs are median-imputed, centered, and scaled using training-only values.
Each ridge lambda is selected through rolling season validation, and the blend is
calibrated only from matched out-of-fold predictions.

A residual random forest and team-specific home field are retained as challengers.
They are not promoted merely because they fit the training sample better. Margin MAE
is the lead comparison metric.

Rolling folds train only on seasons before the predicted season. Reports separate:

- straight-up accuracy
- margin MAE and RMSE
- ATS results when closing lines exist
- preseason and in-season phases
- P4 and selected article-card games
- CFP and neutral-site games
- large-spread buckets

Run the cached backtest with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R --mode=backtest
```

## 8. Separate Football Margin From the Market

Betting lines are not football predictors. The core first estimates expected scoring
margin from team and coach information. A separate ATS layer then compares that margin
with the selected DraftKings/FanDuel line.

The ATS workflow provides:

- expected margin and fair spread
- market edge
- cover probability
- confidence tier
- normal pass thresholds
- a forced-model-pick option for article games

A forced pick chooses the side with the higher modeled cover probability but records
`forced_model_pick`; it does not pretend the normal threshold passed.

An audit found that the ATS residual calibration could reverse the core model on large
spreads. `fit_ats_residual_model()` and `predict_ats_home_cover()` now reject a
non-monotonic effective slope and fall back to the margin model.

## 9. Add Snapshot and Article Workflows

DuckDB stores immutable source snapshots and run metadata. Article and live runs use
different run IDs, and a published Friday article slot cannot be overwritten.

The weekly workflow validates:

- schedule keys and venue status
- team feature coverage
- coach assignment coverage
- line timestamps and providers
- injury source conflicts
- prediction feature names
- article freeze time

The normal Week 1 article command is:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R `
  --mode=article --season=2026 --week=1 --as-of="2026-09-04 13:00:00"
```

The timestamp is interpreted in America/New_York.

## 10. Prepare Week 0 and Week 1

Week 0/1 has no current-season team evidence, so the pipeline contains preseason-phase
hooks for quarterback, roster, staff, returning production, and public-rating
challengers. Their influence fades completely after Week 4.

CFBD provides and the pipeline has frozen historical 2021-2025:

- returning PPA and returning usage
- Week 1 AP and Coaches poll points
- 247Sports team-talent composites

The full talent variant passed the margin-MAE screen but was too aggressive to lead:
it reduced Week 0/1 winner accuracy and extrapolated USC to -75.5 in the 2026 dry run.
The selected 80/20 production blend improved held-out Week 0/1 MAE from 15.30 to 14.97
without reducing the foundation's 84.4% winner accuracy. Raw preseason rank does not
enter either model; retained quality and replacement capacity are derived from
objective source values, while poll hype remains diagnostic.

## 11. Run the August 29, 2026 Dry Run

`cfb_v2/dry_run_aug29_2026.R` began as the isolated July audit and now generates the
production card for the eight August 29 games. It reads frozen CFBD returning
production and 247 talent priors, snapshots available DraftKings/FanDuel lines, fits
the foundation and preseason controls, and leads with the rolling-tested 80/20 blend.

The dry run established these corrections and diagnostics:

- TCU-North Carolina is neutral in Dublin.
- NC State-Virginia is at Scott Stadium in Charlottesville.
- The `San Jose State` schedule alias resolves to the existing historical team
  record instead of losing prior games because of an accent mismatch.
- Provider availability is recorded per snapshot; FanDuel absence never causes a
  DraftKings line to be mislabeled as consensus.
- North Dakota State and Sacramento State are flagged as 2026 FCS-to-FBS transitions.
- The standalone dry run merges the same sourced manual coach ledger as the reusable
  weekly path: Tim Polasek receives his portability-adjusted FCS history, while Alonzo
  Carter and Tavita Pritchard use documented neutral records rather than failed joins.

The FCS-to-FBS bridge now supplies conservative missing-history priors and adds
translation uncertainty. Transition rows remain provisional even when the forced card
returns a side.

The preseason refresh also fixed a source-scale bug: CFBD PPA shares may legitimately exceed
one when the denominator is near zero, so one outlier can no longer divide an entire
season's values by 100. Historical returning-only priors are rebuilt from the raw
cached snapshots with robust scale detection on every run.

Regenerate the dry run with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' `
  .\cfb_v2\dry_run_aug29_2026.R --refresh=false
```

Generate the preliminary Week 1 article slate with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' `
  .\cfb_v2\dry_run_aug29_2026.R --slate=week1 --refresh=true
```

That mode excludes games which begin before the Friday, September 4 at 1 p.m.
Eastern publication cutoff and writes beneath `week_1_preliminary`.

Generated reports, predictions, and charts are written beneath
`cfb_v2/output/2026/week_0_1_blend/<UTC timestamp>` and are intentionally ignored
by Git. The original July audit remains under `august_29_dry_run`.

## 12. Add Prediction Explanations

Every weekly prediction stores its four largest ridge feature contributions. A shared
`ridge_feature_contributions()` helper returns the full point-contribution matrix and
checks that the intercept plus contributions equals the predicted core margin.

The dry run also generates:

- a feature-importance PNG
- a full feature-contribution CSV
- a report section with signed top drivers for each game

The chart compares all eight games with a view excluding the two FCS-transition games.
This exposed the missing-history distortion rather than treating it as model
confidence.

## 13. Fix Repository and Credential Hygiene

Before the first v2 push, hard-coded CFBD keys were removed from four legacy files.
Notebooks now require `CFBD_API_KEY` from `.Renviron`.

The following remain ignored:

- `.Renviron`
- `.RData` and temporary R workspaces
- `cfb_v2/data/*.duckdb*`
- `cfb_v2/cache/`
- `cfb_v2/output/`

The reusable source, tests, historical CSV foundation, and coach ledgers were pushed
to `RollinNolan9/CFBPlayoffPrediction` in commit `a0ed1d7`.

## 14. Build the Static Prediction Dashboard

Every Week 0/1 dry run and article/live command now renders a self-contained Quarto
dashboard beside its prediction CSV. The dashboard is a presentation layer only: it
reads frozen output and never feeds market data, filters, or display choices back into
the football model.

The three views are:

- Slate: article-ready official, review, and forced-side boards plus the filterable
  full card.
- Model vs Market: fair-margin comparison, largest edges, exact feature importance,
  and snapshot provenance.
- Availability: injury scenarios, provisional/data flags, and coach alignment.

Key files:

- `cfb_v2/dashboard.R`: shared prediction normalization and Quarto render helper.
- `cfb_v2/dashboard/dashboard.qmd`: dashboard data views and charts.
- `cfb_v2/dashboard/dashboard.css`: compact desktop/mobile presentation rules.
- `cfb_v2/dashboard/dashboard-after-body.html`: local search, sort, filter, and row
  detail behavior.
- `cfb_v2/render_dashboard.R`: standalone rerender command for an existing CSV.

The renderer stages the Quarto source in a guarded temporary directory beneath the
dashboard source, avoiding the Windows short-path cleanup mismatch, then copies one
embedded HTML file to the run directory. No web server or JavaScript package install
is required. Quarto CLI must be on `PATH`.

Rerender the latest saved prediction CSV with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\render_dashboard.R
```

## 15. Verify a New Machine

From a fresh or updated clone:

```powershell
git pull origin main
```

Create the ignored project-root `.Renviron` with the CFBD key, then initialize local
storage:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R --mode=init
```

Run the test suite:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e `
  "testthat::test_file('cfb_v2/tests/test_v2.R', reporter='stop')"
```

Then rebuild the local DuckDB foundation or run the backtest from the committed CSV
foundation. Local caches and generated output will be recreated as needed.

## 16. Known Work Remaining

The authoritative pending list is [`../TODOS.md`](../TODOS.md). The highest-priority
items are:

1. Correct `turnover_rate_regressed` — complete. The stored foundation, rolling
   backtest, and August 29 production dry run were regenerated from the corrected
   turnover-only rate.
2. Backfill and rolling-test 2021-2026 preseason challengers — complete. Production
   is the phase-faded 80/20 foundation/full-preseason blend; the uncapped profile and
   Week 1 polls remain challengers. Prediction-season priors are frozen through 2026.
3. Build an FCS-to-FBS bridge and transition-specific uncertainty — done.
   `cfb_v2/bridge.R` calibrates against the historical movers and crossover games,
   flags transition games, inflates uncertainty, and blocks high-confidence picks
   when bridge variance dominates.
4. Complete prior history for the three unmapped 2026 head coaches — done via the
   sourced `coach_history_manual.csv` contract (Polasek modeled from his 2024 FCS
   title season; Pritchard and Carter documented neutral). Append Polasek's
   verified 2025 season when available.
5. Automate the final 2026 preseason data refresh — complete for returning production,
   247 talent, polls, coverage validation, and the August 29 report.

Do not treat PFF, portal rankings, weather, conference labels, or raw poll rank as
production features without separate challenger evidence and rolling validation.
