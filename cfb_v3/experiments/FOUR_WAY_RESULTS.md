# Four-Way Team-Strength Results

Run September 9, 2026. No production model, data, backtest or article card changed.
No challenger promoted. This is a historical diagnostic, not proof of an ATS edge.

## Bottom Line

Full-feature v3 has the best overall margin MAE and straight-up accuracy of the
four controlled variants. Simplifying to internal power or adding just EPA does
not improve margin forecasting. Elo is competitive, but its margin difference
from v3 is uncertain. None establishes an ATS advantage worth acting on.

Power-only's leading 50.61% ATS result is not stable: its advantage over matched
v3 disappears when 2025 is omitted. The results do not support a rollback, a new
version, or choosing a model from whichever metric or season looks best.

## Controlled Comparison

Train on preceding seasons starting in 2020; predict 2022-2025 separately. All
four variants share the same 4,270 eligible FBS fitting rows across 2020-2025,
with earlier-year subsets used in each fold. Their common test set is 3,025 games.
Of those, 3,024 have closing lines: 2,970 non-push decisions and 54 pushes.
The game without a line remains in MAE/SU, but not ATS. No confidence filtering.

| Model | ATS W-L-P | ATS | SU | Margin MAE |
|---|---:|---:|---:|---:|
| Pregame Elo + home field | 1,479-1,491-54 | 49.80% | 70.78% | 12.956 |
| Internal power + home field | 1,503-1,467-54 | 50.61% | 70.94% | 13.019 |
| Internal power + offense/defense EPA + home field | 1,461-1,509-54 | 49.19% | 71.34% | 13.002 |
| Full-feature v3, matched fitting rows | 1,471-1,499-54 | 49.53% | 72.43% | 12.760 |

`v3_matched` is the current v3 ridge, coach-selection and preseason workflow refit
on the common FBS population, NOT the saved production model. This restriction
avoids giving the other models additional training games where Elo is absent.
The unchanged v3 reference retains its original fitting population and scores
1,476-1,494-54 ATS (49.70%), 71.97% SU, and 12.823 MAE on these same test games.

On identical 3,024 lined games, matched v3 MAE is 12.764 and unchanged v3 is
12.826, versus 12.038 for the closing-line implied margin. The market reference
makes no ATS selections against itself and is not a deployable Friday forecast.

## Stability And Uncertainty

Primary comparisons are candidate minus matched v3 MAE, so positive means the
candidate is worse. Whole-season bootstrap intervals below adjust for three
margin comparisons; they are descriptive, not independent confirmation.

| Candidate | MAE difference | Adjusted 98.333% interval | Historical margin screen |
|---|---:|---:|---|
| Elo | +0.195 | -0.094 to +0.404 | Did not pass |
| Internal power | +0.258 | +0.032 to +0.558 | Did not pass |
| Power + EPA | +0.241 | +0.052 to +0.465 | Did not pass |

All three simpler models have worse MAE point estimates under every
leave-one-season-out calculation. All three ATS-difference 95% intervals include
zero. Power-only's overall ATS advantage is +1.08 percentage points, with an
interval of -1.08 to +3.35 points. Excluding 2025 changes that advantage to -0.14
points. There are only four season clusters, and these outcomes have already
been examined in earlier experiments; the intervals do not correct that reuse.

## Where To Investigate Next

The predeclared diagnostic slices show where a targeted audit could be useful,
not where to select a winning backtest after the fact:

| Identical lined-game slice | Games | Matched v3 MAE | Closing-market MAE |
|---|---:|---:|---:|
| Week 0/1 | 191 | 14.595 | 11.976 |
| Absolute closing spread over 21 | 390 | 14.113 | 12.005 |

First inspect preseason source coverage/mapping and forecast shrinkage for big
favorites using saved game-level predictions. Separate data defects from model
limitations before proposing another locked experiment. Do not add features,
change thresholds, or promote power-only based on this run.

For CFP SU, Elo is 23-5, power-only and power-plus-EPA are 22-6, and both matched
and unchanged v3 are 21-7. These 28 actual-matchup, round-updated forecasts are
not frozen whole-bracket predictions and do not resolve the original 11-0 claim.
The sample does not establish a reliable playoff ranking of these models.

## Verification And Limits

- Locked [protocol](FOUR_WAY_PROTOCOL.md) before fitting these baselines. Explicit
  feature lists, existing ridge grid, earlier-validation-only penalty/coach
  choices, existing COVID/recency weights and preseason fade. No raw team-name,
  market, PFF or FPI predictors were added. Non-CFP bowls remain excluded.
- Elo is the explicit cached pregame field, not final annual Elo. Monday cutoffs
  exclude two games where a team played after the common feature cutoff:
  2020 UCLA-USC and 2025 Charlotte-UNC. Missing Elo/FCS exclusions and all
  timing decisions are recorded. Underlying power construction is unchanged.
- Original publication timestamps of cached pregame Elo and preseason inputs
  remain unverified. This test checks available metadata, not a fully archived
  historical information feed. Closing-line ATS is not Friday execution or ROI.
- Unchanged v3 reproduces its frozen predictions to a maximum difference of
  4.974e-14 points. Protected source, cached data, backtest and live-card hashes
  match before/after. Prior FBS-only replay parity was not asserted because the
  two timing exclusions change that study's fitting population.
- Eight four-way and six existing ATS-experiment test blocks pass, including
  future-label isolation, training-only imputation, join failures, timing and
  explicit UTC ledger formatting.
- A metadata-only UTC display correction followed the first run. Predictions,
  fold choices, summaries, paired intervals, omission results and cohort counts
  are identical between `20260909T214250Z` and `20260909T214828Z`. No numerical
  model or selection rule was changed after seeing results.

## Reproduce And Inspect

From the repository root, using the preserved v3 foundation and backtest caches:

```sh
Rscript run_cfb_four_way.R
```

Verified output: `cfb_v3/output/experiments/four_way/20260909T214828Z/`.
The runner refuses arguments and creates a new timestamped directory on each run.

See [generated report](../output/experiments/four_way/20260909T214828Z/REPORT.md),
[predictions](../output/experiments/four_way/20260909T214828Z/predictions.csv),
[metrics](../output/experiments/four_way/20260909T214828Z/metrics.csv), and
[verification](../output/experiments/four_way/20260909T214828Z/verification.json).
That directory also includes the protocol, source hashes, cohort ledger, feature
manifest, fitted validations, fold choices and paired/omission diagnostics.
Generated caches are local artifacts; this repository report preserves the result
when those artifacts are unavailable on another computer.
