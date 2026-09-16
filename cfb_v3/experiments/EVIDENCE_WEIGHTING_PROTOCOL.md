# Zero-Sample Preseason Retention Test

Locked September 9, 2026 before fitting/scoring this candidate, following the
user-approved hypothesis from the early-season evidence audit. Already-examined
2022-2025 outcomes make this exploratory research, not a pristine holdout.

## One Change

For an FBS team in regular Weeks 2-4 with a known zero `source_games` count,
retain preseason roster features at weight 1. Other teams keep the existing
calendar fade (1/1/0.6/0.3/0.1 for Weeks 0/1/2/3/4). Week 5 onward and postseason
use the unchanged foundation. Missing source counts are not silently called zero.
Source games mean usable pregame FBS efficiency games, not all games played.
The rule applies independently to the two sides; it does not reset the opponent.

Preserve the existing power ratings, reduced-weight FCS results, EPA construction,
coach history, bowl exclusions, training weights and all eight roster features.
Do not add FCS EPA, new predictors, injury assumptions or market inputs.

## Correct Team-Level Weighting

Use `w_home * (home_value - center) - w_away * (away_value - center)` for roster
features. The center is each raw preseason feature's median over team-season rows
in that outer fold's training seasons only. Each team-season is counted once.
This prevents unequal weights from rewarding a weak team merely because its
percentile is a positive number. When both weights are equal, the center cancels
and this is the original feature definition. Keep original values on unaffected
rows to avoid roundoff changes. Missing priors remain unknown and use the existing
training-fold imputation; no outcome-based row removal or fabricated scouting.

Apply this policy consistently to historical training and test features, refit
only the roster-aware ridge, and preserve the original foundation forecasts.
The refit can change early predictions for teams whose own weights did not change;
report that separately from the directly targeted cases. Week 0/1 features remain
unchanged, but the refitted roster coefficients can affect their forecasts.

## Fixed Controls

Use unchanged v3's full original training population, not the matched-FBS refit.
Reproduce its frozen rolling forecasts first. Predict each of 2022-2025 from
earlier years starting in 2020. Reuse v3's original prior-only outer-fold coach
share and selected ridge penalty; do not retune them for the challenger. Preserve
the two-covered-prior-season activation gate (roster features disabled in 2022).
Train-only centering and imputation are repeated inside each outer fold.

Score both models on the same 3,025 games / 3,024 closing lines as the four-way
study (`20260909T214828Z`). Line-free games stay in MAE/SU. Pushes and no-selections
are explicit. Do not modify grading thresholds, search alternative fade schedules,
add the rejected calibration layer or promote automatically.

## Reporting

Primary: paired margin MAE on Weeks 2-4 matchups where at least one FBS team
arrives with zero efficiency samples after only FCS games this season and the
roster feature layer is active. Verify prior-opponent classification from the
historical schedule, under the existing Monday cutoff. Also report the stricter
single-FCS-opener subgroup. No-games-yet and unexplained gaps are separate labels.

Also show all targeted zero-sample cases, all Weeks 2-4, Week 0/1, unchanged
Week 5+/CFP, large market spreads, per-season results, and overall MAE/RMSE/SU/ATS.
Distinguish targeted/active rows from 2022's disabled roster layer. Show coverage,
training centers, exact feature names, prediction shifts and games that flip ATS.

Use 5,000 whole-season bootstrap draws, seed 20260909, descriptive 95% paired
intervals, and leave-one-season-out deltas. Tiny slices and as few as three active
seasons limit inference; do not treat these intervals as correcting prior research.
No result establishes Friday execution performance or a reliable betting edge.

Fit the same candidate through 2025 and apply it to the frozen 2026 Week 2 card
as a shadow only. First reproduce that card's saved roster model. Keep foundation
and confidence policy unchanged; do not claim newly calibrated uncertainty.
No current injury/odds refresh, publishing, production overwrite, commit or push.
Write immutable timestamped results with source/model hashes and tests. Verify
production source, foundation, saved models and cards are unchanged before/after.
