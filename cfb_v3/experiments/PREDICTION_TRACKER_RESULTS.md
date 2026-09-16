# Prediction Tracker challenger results

Completed September 12, 2026 UTC (September 11 Pacific).
**Experimental only. Production v3 and the published Week 2 card are unchanged.**

## Bottom line

External ratings help estimate margins in this archived comparison. They do not
establish a profitable ATS model. Adding Sagarin Predictor and FPI to the football
model improves margin MAE by 0.417 points versus its matched football-only control,
but scores 49.53% ATS. Their simple equal average has lower aggregate MAE than the
augmented model and scores 51.27% ATS. Do not promote either on this evidence.

These are publisher-archived forecasts, not fully verified Friday-available inputs.
The timing limitations below apply to both the standalone and learned comparisons.

## Same games, same lines

2022-2025 evaluation: 3,007 shared FBS games, 3,006 with canonical closing spreads.
Regular season, conference championships and CFP only; no non-CFP bowls.
All rows below use the same 3,006 lined games. Lower MAE is better.

| Model | Margin MAE | ATS W-L-P | No pick | ATS |
|---|---:|---:|---:|---:|
| Frozen v3 | 12.779 | 1466-1486-54 | 0 | 49.66% |
| Sagarin Predictor | 12.315 | 1506-1440-54 | 6 | 51.12% |
| ESPN FPI | 12.309 | 1478-1468-54 | 6 | 50.17% |
| Equal Sagarin/FPI | 12.223 | 1513-1438-54 | 1 | 51.27% |
| Half v3, half external average | 12.331 | 1463-1489-54 | 0 | 49.56% |
| Matched football-only refit | 12.712 | 1455-1497-54 | 0 | 49.29% |
| Football plus Sagarin/FPI | 12.295 | 1462-1490-54 | 0 | 49.53% |
| External-only ridge calibration | 12.244 | 1497-1455-54 | 0 | 50.71% |
| Closing-market reference | 12.004 | Not an ATS strategy | 3006 | N/A |

ATS accuracy excludes pushes and no-picks. A no-pick means zero edge (within
numeric tolerance), not a tuned confidence filter. Actual prices are unavailable;
these are not realized betting returns. None of the ATS point estimates reaches
the illustrative 52.38% break-even rate at uniform -110 prices.

The equal external average improves MAE versus v3 in all four evaluation seasons.
Its ATS advantage does not hold in 2025. The augmented model's ATS advantage over
v3 changes sign when different seasons are omitted. Better margin forecasts do
not automatically identify the correct side of an already accurate market line.

## CFP and early-season coverage

The tracker has no FPI projections for the entire 2025 CFP. Therefore the primary
joint-source/learned comparison covers only 17 CFP games from 2022-2024, not all
28 playoff games. Its v3 and augmented results are 11-6 and 7-10 ATS respectively.

A separate same-game Sagarin comparison retains all 28 CFP games from 2022-2025:

| Model | CFP ATS | CFP straight up | Margin MAE |
|---|---:|---:|---:|
| Frozen v3 | 18-10 | 21/28 | 12.528 |
| Sagarin Predictor | 17-11 | 22/28 | 12.146 |

These use actual round matchups, not a bracket frozen before the first game.
The sample is too small to establish superiority. Missing FPI is never filled
with final-season ratings or a zero projection.

On the 184 shared Week 0/1 games (183 lined), v3 and the equal external average
have MAE 14.014 and 12.283, respectively, but ATS records of 84-99 and 88-95.
FPI is 97-86 ATS in that slice; this observed slice is not a validated deployment
rule. On spreads above 21 points, MAE improves from v3's 14.020 to 12.603 for the
equal external average. The experiment does not cap projected margins at 21.

## SP+ overlap

The existing qualified SP+ reconstruction overlaps 336 lined games here. On that
same smaller cohort, SP+ scores 166-153-11 with six no-picks (52.04% ATS), while
the fixed equal v3/SP+/Sagarin/FPI blend scores 162-163-11 (49.85%). Their MAEs
are 12.125 and 12.124. V3 is also 162-163-11, with MAE 12.428.

This cohort has no early-season or CFP games and inherits the SP+ reconstruction
limits. No SP+ coefficients were fitted on it. Do not compare its ATS percentages
directly with the 3,006-game table as if the populations were identical.

## What was fitted

- 2020-2021 warm-up; rolling test seasons 2022-2025, always fitting earlier years.
- Existing v3 football features, recency weights, half-weight 2020, training-only
  imputation/scaling, coach selection, and preseason fade/blend are preserved.
- The football-only refit and augmentation use identical archive-covered training
  rows and the same past-only coach choices. The latter adds exactly two explicit
  features: `challenger_tracker_sagarin` and `challenger_tracker_fpi`.
- Ridge penalties use the existing .5/2/8/32 grid. The first fold uses 8; later
  folds select using earlier validation MAE, never the test season's outcomes.
- The external-only ridge tests whether football adds value beyond two calibrated
  external forecasts. Fixed blends were declared before scoring, not optimized.
- Team names, market lines and outcomes are not predictors. Production's FPI
  guard remains intact. No production model or weight was replaced.

Tracker inputs are home-margin game forecasts, already venue-adjusted, not team
rating ranks. We do not add home-field advantage a second time. Weekly external
forecasts remain active all season in this isolated challenger; their internal
preseason/talent/brand assumptions cannot be stripped out. Production's explicit
roster-feature fade to zero from Week 5 is unchanged.

## Data audit and uncertainty

Source: [Prediction Tracker historical files](https://www.thepredictiontracker.com/ncaaarchive.html).
All six 2020-2025 source CSVs have retained URLs, retrieval times and SHA256 hashes.
Retrieval times are not original publication times. See also the
[source investigation](EXTERNAL_HISTORY_SOURCES.md) and [locked protocol](PREDICTION_TRACKER_PROTOCOL.md).

- Team/season/calendar identify games; final scores only verify an identity
  already resolved. Monday-night CFP rematches map to the preceding weekend.
- Neutral-site side reversals flip all signs. Four nonneutral 2020 reversals
  are quarantined. Duke-Clemson 2023 has a source score mismatch and is excluded.
- Identical forecasts with conflicting duplicate market quotes retain one
  forecast, set the conflicting quote missing, and log both source rows.
- Four original pregame Wayback pages corroborate 207 Sagarin game forecasts
  through numeric Predictor/HFA and pregame win/loss record checks. Four other
  checks differ by exactly HFA, consistent with different venue conventions;
  the archived forecasts are not edited using outcomes.
- Those checks authenticate only the corresponding Sagarin values, not FPI,
  every training row, or publication-day prices. Tracker opening/midweek/updated
  quote sensitivity is reported separately, not called executable closing data.
- Paired season-cluster bootstrap intervals use 5,000 draws. Only four seasons
  and repeated historical experimentation mean these are exploratory, not an
  untouched confirmatory test. No profitable-edge or actual-ROI claim is warranted.

## Reproduce and inspect

From the repository root, with R on PATH:

```powershell
Rscript .\run_cfb_prediction_tracker.R
```

Without R on PATH on this machine:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_prediction_tracker.R
```

The command takes no arguments, runs offline, and creates a new timestamped
directory under `cfb_v3/output/experiments/prediction_tracker/`. It does not fetch
current data or alter the weekly workflow. An identical rerun on another computer
requires the ignored v3 foundation/backtest caches and retained experiment inputs,
not just the Git checkout. Required research inputs are:

- `external_history_sources/discovery_20260911T235344Z/` under the experiment output root;
- `external_history_sources/tracker_warmup_20260912/` under that same root;
- the sealed `external_backtest/20260911T231911.953Z/` SP+ run.

Accepted run: [full generated report](../output/experiments/prediction_tracker/20260912T003231.716Z/REPORT.md).
Its directory includes per-game predictions, join/coverage audits, fold choices,
feature manifest, paired intervals, leave-one-season-out results, line sensitivity,
separate SP+/available-source/independent-Sagarin diagnostics, R session information,
source/code checksums and before/after protected-file hashes.

`football_plus_external_model.rds` and `football_matched_control_model.rds` are
trained-through-2025 artifacts for future experiments only. After sourcing
`cfb_v2/config.R`, `store.R`, `features.R`, `models.R`, and
`cfb_v3/experiments/prediction_tracker.R`, use
`predict_tracker_challenger(object, new_data)`. Supply pregame v3 football features
and explicit same-cutoff external home-margin projections. It rejects training
seasons, missing required columns and nonfinite external inputs; it returns a
numeric home margin. It does not fetch live ratings or create official picks.
`external_only_ridge_model.rds` uses ordinary `predict()` and does not carry the
season guard; historical scoring must use the saved rolling predictions instead.

Verification: 19 tracker test blocks plus 41 existing external/backtest/four-way/v3
blocks passed. The optional original-page integration check ran successfully here;
it skips when the retained archive is absent, while synthetic parser tests still run.
Two completed runs produced identical per-game predictions. Frozen v3 reproduction
has maximum difference 4.974e-14; all protected production hashes stayed unchanged.
All sealed output checksums, common-game denominators and past-only fold limits
were checked after the final run. Benign R startup locale/version warnings remain.

## Next decision

Keep v3 and the published card unchanged. Track the predeclared equal external
average prospectively alongside v3, with timestamped forecasts and the same actual
article lines/prices. Continue treating the augmented model as an experiment,
not an upgrade. Additional FPI CFP and early-season SP+ history would improve
coverage, but must be genuinely pregame. Do not tune a new blend or ATS filter on
these already-examined results and call it independent validation.
