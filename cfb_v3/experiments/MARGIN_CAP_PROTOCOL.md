# Historical power-margin cap experiment

Specified September 27, 2026 before fitting these candidates.

Compare uncapped results with symmetric caps of 24, 25, 35 and 42 points in
the opponent-adjusted power-rating fit. Clip the observed home margin before
the existing home-field subtraction. Apply the rule to all historical power
snapshots and recompute power-dependent coach expectations consistently.
Keep final scores, regression targets and grading margins uncapped.

Keep the two game-count predictors, genuine sample counts, FCS weights,
historical weighting, EPA construction, cohort, coach blend and ridge penalties
unchanged. No inference-time clipping of old fitted coefficients and no changes
to the production model, source data, saved dashboards or public website.

## Model and chronological evaluation

Test the foundation ridge used by the published Week 5 power rankings. No
random forest or preseason roster branch is used. Refit each candidate on
seasons preceding each test season, starting with 2020 history and testing
2022, 2023, 2024 and 2025 separately. Within-season power and coach inputs use
only strictly earlier model weeks; no current/future outcomes enter inputs.

Retain the production fitting population and normalized training weights.
Reuse the previously selected, prior-only control choices: coach share 0.65
for all folds, ridge penalty 8 for 2022 and 32 for 2023–2025. Refitting changes
the model coefficients; hyperparameters do not receive an additional search.
The uncapped control must reproduce the saved foundation rolling forecasts
within numerical tolerance before any capped result is interpreted.

Primary evaluation: regular-season FBS-vs-FBS games from Week 5 onward, matching
the phase now used for the published rankings. Secondary: all eligible FBS
games, early weeks, postseason/CFP, large spreads and individual seasons.
Preserve missing-line games for score error and straight-up results. ATS uses
the same saved historical closing lines for every candidate, with pushes and
zero-edge passes reported explicitly. These are not Friday execution results.

Primary metric: paired change in margin MAE versus uncapped. Also report RMSE,
straight-up accuracy, ATS W-L-P and percentage-point changes. Do not choose a
cap merely because it moves USC, JMU or South Florida to preferred positions.
Show per-season effects and leave-one-season-out changes. Whole-season
bootstrap intervals are descriptive; only four season clusters exist. For
the four cap comparisons, also show 98.75% Bonferroni-adjusted intervals.

Report 2022–2024 development results and 2025 separately. Select a development
candidate by primary MAE, then inspect its 2025 effect. This is a historical
research comparison: these seasons have appeared in previous experiments,
so 2025 is not a pristine, untouched confirmation set. No automatic promotion
or assertion that an apparent advantage establishes a reliable ATS edge.

Save frozen inputs, code hashes, feature checks, fold choices, per-game
predictions, metrics and a readable report in a new timestamped experiment
directory. Verify the original production inputs are unchanged afterward.
