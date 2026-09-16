# Four-Way Team-Strength Experiment

Locked September 9, 2026 before fitting these baselines or viewing their scores.
Local protocol, not external preregistration. Prior studies have already examined
2022-2025 outcomes; none of these seasons is a pristine research holdout.

## Questions And Fixed Variants

1. `elo_baseline`: cached explicit pregame Elo difference plus home-field points.
2. `internal_power`: existing pregame internal power difference plus home-field points.
3. `small_football`: internal power, offensive EPA difference, defensive EPA
   difference, and home-field points.
4. `v3_matched`: the existing v3 feature set and its established preseason blend
   and coach-selection workflow, fitted on the same eligible rows as 1-3.

EPA means the existing season-to-date pregame features, with their established
early-season prior-year blend. It does NOT introduce a new three/six-game window.
Home-field points are the existing 2.4 for campus games and zero for neutral games;
their coefficient is learned by the existing ridge model. Elo units are mapped to
margin points inside training, not assumed equal to scoreboard points.

All four use the existing weighted ridge learner without a residual forest.
Grid: 0.5, 2, 8, 32; default 8 in the first outer fold. Later seasons select their
penalty using earlier validation seasons only. Preserve v3's prior-only coach
choice and predefined preseason fade/blend; do not select new blends or thresholds.
Only v3 needs those additional choices. The simpler models have explicit feature
allowlists and no manual scouting, polls, PFF, SP+, market or raw team predictors.

## Population And Timing

Training starts in 2020, COVID season retains its existing half weight, and the
same recency/game weights apply. Train through the preceding season and evaluate
2022, 2023, 2024, 2025. Keep the existing foundation and feature construction.
Exclude non-CFP bowls from fitting and scoring using the established policy.

Elo is absent for most FCS opponents. To avoid comparing an imputed-Elo model
against models with observed team-strength inputs, use a common FBS-vs-FBS fitting
population with both explicit pregame Elo values present for ALL FOUR variants.
This is a controlled feature comparison, not a silent replacement of current v3.
Reproduce unchanged `v3_control` on its original fitting population as an additional
reference, and display it separately. If the common cohort matches the previous
FBS-only study, verify that `v3_matched` reproduces its saved predictions as well.
Keep FCS influence inside the existing power/transition-feature construction;
altering that construction is outside this experiment.

Validate source joins one-to-one by game ID, season, teams and margin. Explicit
pregame Elo comes from the cached historical schedule, never annual/final Elo.
Use the foundation's Monday feature cutoff. Exclude a target from the common
cohort if a team played another game since that cutoff: game-pregame Elo could
otherwise contain newer information than the other models' weekly features.
Missing/unresolved kickoff/cutoff metadata fails the common timing gate.
Preserve source-vintage limitations: a historical pregame field is not proof of
its original publication timestamp. The current preseason backfills likewise do
not have independently verified publication snapshots.

All compared forecasts use identical evaluation rows. Keep games without lines
for MAE/SU; exclude only their ATS calculation. Use identical closing spreads and
count pushes and zero-edge/no-line no-selections explicitly. This grades projections,
not a selectively published betting card. No Friday execution or ROI claim.

## Reporting And Interpretation

Primary metric is paired margin MAE versus `v3_matched`. Also show RMSE, bias,
SU, ATS W-L-P, and no-selection counts. Predeclared slices: every season, Week 0/1,
Weeks 2-4, regular Week 5+, CFP, neutral CFP, conference championships, article
audience, and closing spread magnitudes 0-7, over 7 through 21, and over 21.
CFP scores are round-updated predictions of actual matchups, NOT frozen brackets.
Closing-line implied margins are a reference, not another candidate or ATS picker.

For each of the three primary comparisons, resample entire seasons 5,000 times
with seed 20260909. Report descriptive 95% intervals and Bonferroni 98.333% intervals
for the three margin comparisons. ATS intervals are descriptive secondary results.
Also report leave-one-season-out deltas. Four seasons make all such intervals
fragile, and the intervals do not adjust for previous research on these years.

A candidate passes the historical margin screen only when its adjusted upper
interval is below zero and its MAE advantage remains when each season is omitted.
This is not a betting-edge or automatic-promotion rule. No threshold search,
outlier deletion, best-season selection, post-score feature edits or new experiments.
Freeze all production source, data, live cards and backtest outputs with hashes.
Write timestamped experiment artifacts without overwriting previous runs. No
production edit, foundation rebuild, model promotion, commit or push is authorized.
