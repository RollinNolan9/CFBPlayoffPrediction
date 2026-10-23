# CFB Model 3.0.0

**Status: candidate, additional data defects open.** The September 8 review fixes
are implemented, but matched historical margin MAE is slightly worse than v2.
The September 11/12 research found further turnover/classification and upstream
duplicate issues. Isolated corrections do not establish an ATS edge and have not
changed production or published cards. See [validation results](VALIDATION.md) and
[the latest research](experiments/MATCHUP_RESULTS.md) before treating this as a predictive upgrade.

The subsequent [bounded play cleanup](experiments/PLAY_CLEANUP_RESULTS.md) adds a
shared experimental classifier, reconciliation queue and one-command comparison.
Its fixed tests do not establish an upgrade; the strict partial classifier loses
substantial usable rate coverage. It has not been installed in the production engine.
See the [offline cleanup guide](experiments/PLAY_CLEANUP_GUIDE.md).

The [Week 3 efficiency-retention test](experiments/WEEK3_EFFICIENCY_RESULTS.md)
compares 30% versus 50% prior-year efficiency while leaving roster weights fixed.
The challenger does not improve margin MAE and its one-cover original-line gain
reverses on corroborated quotes. It is not promoted; the existing 30% schedule
is not established as optimal. Reproduce offline with retained caches using
`Rscript run_cfb_week3_efficiency.R`.

V3 corrects the feature, coach, and validation problems identified in the September
8 review. The shared R engine remains in `cfb_v2/` to avoid duplicating the model.
New data, DuckDB records, and predictions live in `cfb_v3/`. Existing v2 datasets
and published snapshots are preserved. `run_cfb_v2.R` is a compatibility entry point
to the current engine; use the prior Git revision to execute the original 2.1.0 code.

## Run

**Required before the first weekly prediction run: complete the full-slate injury
research below, or ask Nolan to fill the specific reporting gaps.** This applies
to preliminary, live, opponent-relative, and article runs. Do not wait for Nolan
to name injured players. Refresh the review again before the final article run.

From the repository root in PowerShell:

```powershell
# Rebuild the historical features, preseason priors, and rolling backtest.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=rebuild --season=2026 --seasons=2020:2025 --overwrite=true

# Backtest an already rebuilt foundation without pulling data again.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=backtest

# Compare frozen v2 features and v3 using the same corrected validation.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=compare

# Generate a Week 3 live card and self-contained Quarto dashboard with fresh sources.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=live --season=2026 --week=3 --all-picks=true --refresh=true
```

Use `--refresh=true` to update schedule, current play-by-play, membership, coaches,
and lines when preparing the article. Keep the CFBD key in the ignored root
`.Renviron`. Sources are cached in the existing `cfb_v2/cache/` directory. On another
computer, copy those ignored caches for an identical rerun, or allow the APIs to
download them. R package dependencies are the same as v2; Quarto renders the dashboard.

The automatic weekly path includes upcoming FBS-vs-FBS games and omits Thursday
games. It constructs inputs from the rebuilt historical foundation and completed
prior-week play-by-play. `--all-picks=true` requests a model side on every lined game;
it does not override the model's preferred side or turn a selection into an official bet.

Run `--mode=article` on publication day for an immutable article snapshot. Live runs
are separate. Supply `--as-of="2026-09-11 13:00:00"` only for the actual intended data
cutoff; timestamps are interpreted in Eastern time. Historical/current source pulls
are not a substitute for archived point-in-time data when reconstructing old articles.
When `--as-of` is omitted, the cutoff is set after live sources finish downloading.
An explicit cutoff remains fixed. The dashboard uses the selected quotes' actual
capture times. For consensus lines, the capture time is the oldest contributing quote.
The dashboard omits cover probabilities, confidence tiers, and validation badges;
calibration and confidence metadata remain in the prediction exports.

Manual injury rows belong in `cfb_v3/inbox/injuries.csv`. The weekly command reads
them, rejects information dated after the cutoff, and uses the latest report per
player, team, and game for injury scenarios. A later active status clears an older
out status; conflicting statuses at the same latest timestamp require resolution.
Automated injury collection remains future work; the research requirement below
is an analyst/agent responsibility on every weekly run. Coach
interval corrections belong in `cfb_v3/inbox/coach_assignment_overrides.csv` and take
precedence over the current-coach source. Review those intervals after midseason changes.

### Required injury research before weekly runs

1. Build the intended slate and research **both teams in every matchup**, including
   all reported injuries and availability changes across positions. Check existing
   season-long absences, new injuries, returns, uncertain starters, and replacement
   depth. Research must include teams outside the article's featured games. User
   examples such as LJ Martin, JKS, and Jared Curtis are leads, not an exhaustive list.
2. Start with current conference availability reports, school releases, and coach
   press conferences. Fill gaps with credible team beat reporters and corroborated
   national reporting. Verify player identity, team, applicable game, report date,
   and exact status; resolve initials before matching player data. Distinguish an
   in-game injury designation from availability for the next game. An older report
   must not silently override a newer update.
3. Save a dated research log with each team's sources and coverage, plus each
   reported player's status, source URL, publication time when available, retrieval
   time, and replacement information. Distinguish "reviewed, no reported issue"
   from "not reviewed/report unavailable"; neither proves a healthy roster. Record
   source-time uncertainty instead of inventing precise timestamps. Use only
   information available by the run's cutoff. The Friday article injury cutoff is
   **noon America/Los_Angeles** unless Nolan specifies otherwise; earlier preliminary
   runs use their actual cutoff and must not borrow later Friday information.
4. If reporting is missing, stale, contradictory, or leaves a material role unclear,
   **ask Nolan for the specific missing information before the initial model run**.
   Batch the open cases into one concise question after doing the available research.
   Do not make him supply the entire injury list or assume an unmentioned player is
   available. If he requests a baseline while cases remain unresolved, label those
   cases and the incomplete coverage explicitly.
5. Preserve exact availability states. Out/season-ending absences can enter the
   confirmed-availability registry; questionable, doubtful, and concussion-protocol
   reports remain explicit open cases until resolved. Being in protocol does not
   itself establish whether a player will play. The current opponent-relative
   overlay accepts only out/active reports: retain other statuses in the research
   log and flag the game for review rather than converting them to out, active, or
   an invented participation probability. For material uncertainty, prepare
   available/unavailable scenarios when the replacement inputs support them.
6. After freezing the baseline, apply fresh, source-backed usage/EPA replacement
   estimates where the method is defensible. Research every reported injury even
   when no defensible point value exists. Offensive line, defensive, special-teams,
   thin-sample, and overlapping injuries may require qualitative review or a joint
   scenario; do not silently assign zero impact or invent values. Account for
   replacement performance already present in team history and avoid counting the
   same lost production twice. Keep all unresolved material cases out of featured
   pick recommendations until reviewed.
7. Before handing over the dashboard, state the research cutoff, teams covered,
   important absences, open cases, adjustments actually applied, and cases that are
   flagged without a numerical adjustment. Refresh news for the final article run
   and revisit changed statuses. Never describe a one-player adjustment as a
   comprehensive injury-adjusted slate.

### Injury scenarios for the opponent-relative cards

The separate `run_cfb_live_relative.R` and `cfb_v3/run_week6_relative_preliminary.R`
runners read confirmed availability from `cfb_v3/inbox/player_availability.csv`.
The latest eligible status persists through its stated season/week; a later active
report supersedes an older absence. Known absences without a fresh estimate retain
their numerical baseline, appear as injury reviews, and are excluded from the
Largest Point Gaps board. The registry does not provide comprehensive injury coverage.

For a reviewed player, freeze a baseline first, then apply a game-specific estimate:

```powershell
Rscript --vanilla cfb_v3/apply_live_injuries.R --baseline=FROZEN_OUTPUT_DIR --registry=cfb_v3/inbox/player_availability.csv --estimates=ESTIMATES.csv --output=NEW_OUTPUT_DIR
```

The directory input selects `unadjusted_predictions.csv`. Verified estimate rows
require `game_id`, `team`, `player_id`, signed `impact_points` (positive means the
absence costs that team points), `input_verified`, `estimate_game_id`,
`baseline_as_of`, `baseline_sha256`, `estimate_method`, and `estimate_source`.
`input_verified` confirms the underlying inputs, not predictive accuracy. The
timestamp and SHA-256 must match the exact frozen baseline. Optional
`estimate_note` records workload, replacement sample, and limitations.

The output includes a full replacement scenario dashboard, the untouched baseline,
half-strength sensitivity, comparison CSV, and source audit. It does not refit or
publish. Usage/EPA estimates must use compatible player/backup play definitions,
past-only samples, and reconciled source data. They are supplemental scenarios,
not learned injury coefficients or cover probabilities. Reassess the estimate as
backup performance enters team history; never carry an old point penalty forward
solely because the player remains out. Overlapping player effects require a joint
estimate rather than summing QB and receiver/RB production from the same plays.

## What Changed

1. Promoted FCS teams receive a prior from earlier transitions/crossover games before
   their current rates are blended. A missing prior never becomes a 0% success rate.
2. Historical and weekly predictions share the same differential/phase builder.
   The automatic weekly command fills the actual v3 feature schema.
3. Coach ratings use destination-team power and completed current-season residuals,
   with the same chronological cutoff as historical training. Unknown coach history
   gets a neutral value and a visible diagnostic; a missing coach assignment still fails.
4. Each outer test season chooses its ridge penalty and coach split using earlier
   seasons only. The first outer fold uses the declared defaults (ridge 8, coach 65/35).
   Fold training weights are normalized so the penalty has a consistent scale.
5. Historical uncertainty uses earlier out-of-fold errors. The initial fold uses its
   training residuals as a warmup; final live calibration uses all available past OOF errors.
6. ATS calibration is cross-fitted by season. Thresholds are selected before the final
   holdout season, then qualify only on that holdout. Pushes never count as wins/losses.
7. Turnover smoothing uses eligible prior-season games, with a fixed 2% prior when no
   past sample exists. Appending later-season games cannot revise earlier observations.
8. Preseason strength excludes FCS matchups and non-CFP bowls before calculating
   retained quality and transfer-source adjustments.
9. From Week 8 onward, historical games collectively receive at most 10% of the power
   fit's total observation weight. This is a data-weight budget, not a guarantee about
   the percentage of an individual predicted margin attributable to history.

The roster-feature fade remains 100/60/30/10% in Weeks 0-1/2/3/4, and zero from Week 5.
It is not a literal percentage split of the final predicted margin. Raw team names,
FPI, polls, PFF, and market lines remain excluded from the football model.

## Outputs And Checks

The [external ratings benchmark](experiments/EXTERNAL_RATINGS.md) freezes SP+,
Sagarin Predictor and FPI alongside an existing v3 card without refitting it.
Use `Rscript run_cfb_external_ratings.R snapshot --card PATH` before kickoff,
then `Rscript run_cfb_external_ratings.R score --snapshot DIRECTORY` after games.
This is a prospective, rating-derived comparison, not a historical backfill or
automatic promotion. Missing sources and actual spread-price coverage stay explicit.

The [historical SP+ comparison](experiments/EXTERNAL_BACKTEST_RESULTS.md) runs offline
with `Rscript run_cfb_external_backtest.R`. On 336 matched corroborated games,
v3/SP+/50-50 blend score 49.85%/52.04%/52.31% ATS; the modest improvements do not
establish a profitable edge. That SP+ cohort has no early-season or CFP coverage;
production remains unchanged.

The separate [Prediction Tracker challenger](experiments/PREDICTION_TRACKER_RESULTS.md)
now compares archived Sagarin/FPI forecasts and past-only augmented models on
3,006 identical lined 2022-2025 games. V3/equal Sagarin-FPI/football-plus-external
score 49.66%/51.27%/49.53% ATS, with margin MAE 12.779/12.223/12.295. No promotion:
margin estimation improves, but no betting edge is established. Publisher archives
lack per-game publication timestamps; 207 Sagarin values have independent pregame
checks. FPI's missing 2025 CFP coverage is reported separately from the broader
28-game Sagarin comparison. Run `Rscript run_cfb_prediction_tracker.R` offline with
the retained research caches. Production feature guards, weights and picks remain
unchanged; explicit external features and fitted RDS files live only in the experiment.

The [deeper market-edge investigation](experiments/TRACKER_EDGE_RESULTS.md) tests
direct cover probabilities, market-residual learning, fixed selectivity rules and
OOF-v3 incremental value. Its small positive 55%-confidence result fails line-stress
and source-agreement checks; no promotion. The audit also traces all historical
test handicaps to PBP quotes, not verified bookmaker closings. Run
`Rscript run_cfb_tracker_edge.R` with retained caches; production and article cards
remain untouched. Quote provenance now takes priority over additional blend tuning.

The isolated [controlled ATS experiments](experiments/ATS_RESULTS.md) did not
establish a betting advantage; neither challenger was promoted. The
[locked protocol](experiments/ATS_PROTOCOL.md) and one-command runner are retained:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_ats_experiments.R
```

This uses the existing v3 foundation and writes a separate timestamped experiment
directory. It does not rebuild inputs, update the live model, or overwrite cards.

The [original v1 comparison](experiments/V1_RESULTS.md) is also available via
`Rscript run_cfb_v1_comparison.R`. It requires the original `.RData` or its extracted
cache and compares the five-game random forest with v3 on matched pregame inputs.
V3 improves margin/SU accuracy; v1 has a higher aggregate ATS point estimate but no
established betting edge. Neither this comparison nor its seeds promote a model.

- `cfb_v3/output/backtest/`: chronological fold predictions and diagnostic reports.
- `cfb_v3/output/comparison/report.md`: v2/v3 comparison using matched games and the
  same validation, including season-block uncertainty intervals.
- `cfb_v3/output/2026/week_2/`: predictions, dashboard, exact matchup features, and
  serialized model/calibration for each run.
- `cfb_v3/output/2026/week_2_preliminary/`: detailed feature-importance chart and
  diagnostics from the retained matchup runner (`--slate=week2 --refresh=false`).
- `cfb_v3/data/cfb_v3.duckdb`: the v3 foundation and immutable weekly ledger.

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_v2.R
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_v3.R
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_predictor_safeguards.R
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_ats_experiments.R
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_v1_comparison.R
# After generating the v3 foundation and a Week 2 live card:
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_v3_artifacts.R
```

Correctness fixes do not establish a betting advantage. Evaluate matched rolling
results and the prospective frozen-card record before claiming improved ATS performance.
Historical preseason sources are API backfills, not verified original release archives.
