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

From the repository root in PowerShell:

```powershell
# Rebuild the historical features, preseason priors, and rolling backtest.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=rebuild --season=2026 --seasons=2020:2025 --overwrite=true

# Backtest an already rebuilt foundation without pulling data again.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=backtest

# Compare frozen v2 features and v3 using the same corrected validation.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=compare

# Generate a preliminary Week 2 card and self-contained Quarto dashboard.
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=live --season=2026 --week=2 --all-picks=true --refresh=false
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

Manual injury rows belong in `cfb_v3/inbox/injuries.csv`. The weekly command reads
them, rejects information dated after the cutoff from the prediction input, and
reports injury scenarios. Automated injury reporting remains future work. Coach
interval corrections belong in `cfb_v3/inbox/coach_assignment_overrides.csv` and take
precedence over the current-coach source. Review those intervals after midseason changes.

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
