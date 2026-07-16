# CFB Model v2

This is the parallel production pipeline. It does not modify or source the original
`model_final.Rmd` or `cfb_model_rebuild.R`.

See [`IMPLEMENTATION_GUIDE.md`](IMPLEMENTATION_GUIDE.md) for the chronological rebuild
history, design decisions, verification commands, and current limitations.

## Design contract

- The football model predicts score margin without team names, conference labels,
  polls, betting lines, FPI, PFF, or public power ratings.
- The ATS layer is separate. It calibrates the pure model's out-of-sample margin edge
  against a market spread and cannot feed the CFP bracket model.
- Full production training begins in 2021. The 2020 season receives half weight.
  Earlier seasons and non-CFP bowls receive zero weight.
- Team-form and coach histories exclude FCS games and non-CFP bowls. The core uses
  2.4 home-field points (zero at neutral sites); team-specific home field is a
  challenger only.
- Preseason roster, quarterback, portal, and returning-production inputs are multiplied
  by `100/60/30/10/0%` in Weeks `0-1/2/3/4/5+`.
- Prior-season team performance is capped at 10% from Week 8 onward.
- Coach assignments are interval records keyed by team, season, start week, and end
  week. Overlap or a missing as-of coach stops the run.
- PFF, FEI, Massey, KFord, travel, and weather are challenger inputs. Challenger
  columns use the `challenger_` prefix and are excluded from the production model.
- Article and live runs have different immutable DuckDB run IDs. A Friday article
  snapshot is never updated in place.

## First-time setup

From `CFBPlayoffPrediction`:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R --mode=init
```

This creates `cfb_v2/data/cfb_v2.duckdb` and CSV contracts in `cfb_v2/inbox`.
The weekly run intentionally fails if a required source is empty. Silent fallback to
old coach, line, or feature data is not allowed.

Put the CFBD key in an ignored `.Renviron` file at the project root:

```text
CFBD_API_KEY=your_key_here
```

Confirm R can see it without printing the key:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e `
  "cat(cfbfastR::has_cfbd_key(), '\n')"
```

## Historical foundation command

Build the completed 2020-2025 game, team-form, and coach foundation with one command:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R `
  --mode=build-foundation --seasons=2020:2025 --overwrite=true
```

Public pulls are cached beneath `cfb_v2/cache`. Add `--refresh-schedule=true`,
`--refresh-pbp=true`, `--refresh-coaches=true`, or `--refresh-espn=true` only when
that source should be downloaded again. `--overwrite=true` replaces the three
generated inbox foundation files after their existing contents have been reviewed.

The historical source hierarchy is:

- CFBD regular/postseason schedules and season-specific FBS membership.
- cfbfastR compact play-by-play for competitive EPA and efficiency features.
- ESPN metadata for CFP, bowl, and conference-championship labels only.
- CFBD coach-season history plus dated public in-season transition tables.
- `cfb_v2/inbox/coach_assignment_overrides.csv` as the highest-priority correction.

Public coach changes become effective at the first scheduled game after the reported
date. A confirmed manual team-season override replaces the public assignment; public
assignments replace game-count inference. End-of-season hiring tables are excluded so
future permanent hires cannot leak into earlier games.

Audit outputs are written to:

- `cfb_v2/output/foundation/foundation_qa.csv`
- `cfb_v2/output/foundation/coach_assignment_qa.csv`
- `cfb_v2/data/historical_coach_seasons.csv`
- `cfb_v2/data/historical_coach_ratings.csv`
- `cfb_v2/data/coach_transitions.csv`
- `cfb_v2/data/cfb_v2.duckdb`

## Rolling backtest command

Run the cached foundation through season-by-season folds with one command:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R --mode=backtest
```

The command compares the ridge core with team-home-field and residual-forest
challengers, then writes the report, fold predictions, summary metrics, and selected
coach split beneath `cfb_v2/output/backtest`.

## Preseason challenger command

Freeze the 2021-2025 preseason sources (CFBD returning production, 247 team talent,
Week 1 AP/Coaches poll points) into one timestamped row per team-season:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R `
  --mode=build-preseason --seasons=2021:2025 --overwrite=true
```

Raw pulls are cached beneath `cfb_v2/cache/preseason`; add `--refresh-preseason=true`
to re-download. The frozen output is `cfb_v2/data/preseason_team_priors.csv` with
season-normalized values only (percentiles and vote shares, never raw ranks) and no
team or conference identity predictors. Any FBS team missing from returning
production stops the build.

When the priors file has rows, `--mode=backtest` additionally evaluates three
preseason challengers (returning production; plus talent; plus poll hype gap) whose
`challenger_ps_*` matchup features fade with the standard Week 0-4 preseason
weights and are excluded from the production model by the `challenger_` prefix. The
Week 0/1 diagnostics land in `cfb_v2/output/backtest/preseason_challenger_report.md`
covering margin MAE, straight-up accuracy, ATS accuracy, uncertainty coverage,
conference slices, and the learned points-per-standard-deviation effect of each
preseason feature.

The returning-production and talent variants are the only promotion candidates.
The poll hype variant is diagnostic by design: the production contract excludes
polls, so `preseason_poll_vote_share` and `hype_gap` exist to quantify how far
preseason AP/Coaches sentiment misleads relative to prior on-field results (the
2025 Clemson/LSU pattern), not to rate teams.

## Weekly command

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R `
  --mode=article --season=2026 --week=1 --as-of="2026-08-28 13:00:00"
```

The `--as-of` value is interpreted in `America/New_York`. The output includes expected
home margin, median fair spread, uncertainty, straight-up pick, ATS edge, cover
probability, confidence, injury scenarios, and top model drivers. CSV and Parquet
copies are written beneath `cfb_v2/output/<season>/week_<week>`.

To require a side in a game that would normally pass:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v2.R `
  --mode=article --season=2026 --week=1 --as-of="2026-08-28 13:00:00" `
  --force=401752001
```

The model chooses the side with the higher cover probability and stores
`forced_model_pick`; it does not pretend that the normal betting threshold passed.

Use `--mode=live` for a later refresh. Live results are separate records and do not
alter the article card.

## Required inbox data

`schedule.csv`
: One row per game. The spread convention is separate: negative means the home team
  is favored. Neutral-site games receive exactly zero generic home-field points.

`lines.csv`
: Timestamped provider observations. DraftKings and FanDuel are the production books.
  The article output retains each book, opening consensus, article-time consensus,
  best home line, and best away line. Closing lines belong in historical
  `training_games.csv` for grading only.

`coach_assignments.csv` and `coach_history.csv`
: Mandatory canonical IDs, week-effective assignments, prior performance above an
  independent expectation, opponent level, context strength, and capped postseason
  experience. Both 65/35 and 70/30 recent/history ratings are persisted; 65/35 is the
  initial production setting until rolling validation chooses otherwise.

`team_week_features.csv`
: Pregame team rows only. Current-season fields must contain games completed before
  kickoff. Week 0/1 rows use prior and preseason columns because current-season fields
  are empty.

`training_games.csv`
: One row per historical game with the same numeric matchup feature names used for
  prediction, final margin, and optional closing spread. Current-season final values
  cannot appear in a pregame feature column.

`membership.csv`, `rankings.csv`, and `cfp_probabilities.csv`
: Schedule inclusion only. They include P4 games, Notre Dame, UConn, ranked teams,
  non-P4 teams at 2% CFP probability, and each non-P4 conference leader. These fields
  are not predictors.

`injuries.csv`
: Approved sources are conference/school reports, game notes or depth charts, verified
  beat reporting, and explicit manual exceptions. Starting quarterbacks and kickers,
  starting offensive linemen, skill players at 25% opportunity share, and defenders at
  50% snap share or major havoc usage can move the projection. Conflicting reports stop
  automatic pick status and request review.

## Public-data adapters

`adapters.R` includes version-tolerant wrappers for:

- CFBD regular and postseason game endpoints
- season-specific `cfbfastR::cfbd_team_info()` FBS membership
- `cfbfastR::load_cfb_pbp()`
- `cfbfastR::cfbd_betting_lines()` when `CFBD_API_KEY` is configured
- competitive-play EPA aggregation with garbage-time and clock-kill removal

Official injury reports and DraftKings/FanDuel snapshots do not share a stable public
API. They enter through the strict timestamped CSV contracts so a scraper or manual
fallback can be changed without changing the model.

## Verification

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e `
  "testthat::test_file('cfb_v2/tests/test_v2.R', reporter='stop')"
```

The suite covers phase fading, modern-era weights, bowl/FCS rules, canceled games,
garbage-time filtering, coach snapshot/count anomalies, dated transition intervals,
stable coach identities, pure-model exclusions, large-spread extrapolation, neutral
sites, forced picks, eligibility, injuries, line snapshots, CFP bye uncertainty,
rolling splits, deterministic simulations, and DuckDB immutability.
