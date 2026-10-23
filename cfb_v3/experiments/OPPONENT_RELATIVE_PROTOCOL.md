# Opponent-relative margin: fixed exploratory experiment

Specified September 27, 2026 before calculating candidate results.

For each completed eligible FBS-vs-FBS game, orient final margin to the team.
The opponent baseline is that opponent's mean signed margin in its earlier
eligible games in the same season. Relative performance = team margin PLUS
opponent prior mean margin, equivalently team margin minus the previous
opponents' average margin against that opponent. Freeze the opponent average
before the source game; never use its final-season average or revise old
baselines using later outcomes. No opponent history means no usable component.

For each target game, average the team's available relative performances from
strictly earlier chronological model weeks in the same season. Require source
kickoffs before the feature-week-start cutoff when available. Multiply the
average by n/(n+3), where n is the number of usable components. Three zero-mean
pseudogames are fixed before looking at results, not tuned. No usable data
produces a neutral zero feature. Home minus away yields one new predictor,
opponent_relative_margin_diff. Audit counts are retained but not added as new
predictors. Raw margins are uncapped and not venue-adjusted for this feature.
Use the existing FBS history eligibility rule, excluding non-CFP bowls.

Compare four foundation ridge fits, all uncapped:
1. uncapped: the published 20-predictor control.
2. opponent_relative: control plus the new predictor (21).
3. no_counts: remove source_games_diff and games_played_diff (18).
4. no_counts_opponent_relative: no_counts plus the new predictor (19).

Keep training targets, cohort, weights, existing power/efficiency/coaching
features and fixed prior-only hyperparameters identical. Refit on earlier
seasons only for test years 2022-2025, with the same saved folds as the cap
experiment. The control must reproduce saved forecasts to numerical precision.
Do not retune the ridge penalty, coach split, shrinkage or averaging window.

Primary cohort: regular-season Week 5+ FBS-vs-FBS games. Primary comparisons
are opponent_relative minus uncapped and no_counts_opponent_relative minus
no_counts, measured by paired MAE. Direct count removal and the combined model
versus the published control are secondary. Report RMSE, SU and ATS, all-FBS
and per-season results, 2022-2024 development and 2025 separately. Preserve
missing-line games for MAE/SU; ATS uses the same historical closing lines,
excluding pushes from accuracy and explicitly recording zero-edge passes.

Use paired whole-season bootstrap intervals (10,000 draws, seed 20260927):
95% descriptive and 97.5% Bonferroni intervals for the two primary MAE tests.
There are only four season clusters; intervals are fragile. All seasons were
previously researched, including 2025, so this is exploratory historical
evidence, not pristine confirmation. No production replacement or ranking
refresh is part of this test. The earlier cap backtest does not validate this
new predictor. Freeze source/code hashes and save feature audits and forecasts.
