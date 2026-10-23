# Model power rankings

The rankings compare every current FBS team against the same full FBS field on a
neutral site. Both orientations of each matchup are predicted; half their
difference removes the nominal home-label intercept. Each team's rating is its
mean margin across the field, including a zero self-matchup. The field average
is zero. This is a predictive power ranking, not a resume ranking.

AP votes and betting lines are not predictors. An AP poll can be displayed as an
external comparison only when its season and week match the ranking snapshot.

## Run from the repository root

The runner reads existing local caches. Refresh and verify the season's schedule,
play-by-play, membership and coaches before creating a new weekly snapshot. It
stops when a completed prior-week FBS matchup has no play-by-play or an earlier
scheduled game remains unfinished.

```text
Rscript cfb_v3/experiments/model_rankings_trial.R SAVED_MODEL.rds OUTPUT_DIRECTORY PREVIOUS_RANKINGS.csv TARGET_WEEK
python cfb_v3/experiments/render_rankings_trial.py OUTPUT_DIRECTORY
```

The optional fourth argument advances the team inputs to a later week while
reusing the saved fitted model. This avoids repeating a historical fit just to
update current-season features. The source model cutoff and the new data cutoff
are recorded separately. Without that argument, the script reconstructs and
checks the original frozen snapshot instead.

From Week 5, both the explicit preseason roster feature weight and the preseason
model-component share are zero. Historical performance still contributes to
other features. Movement from Week 4 therefore reflects new game results and
the scheduled switch to the foundation model.

## Outputs and checks

- `rankings.csv` and `top25.csv`: ratings, records and optional weekly movement.
- `neutral_matchups.csv`: every ordered neutral matchup.
- `team_snapshots.csv` and `included_games.csv`: inputs and game coverage.
- `source_prediction_parity.csv`: replay of the source model's saved projections.
- `qa.json`: both cutoffs, fitted-model provenance, input hashes, coverage and
  mathematical checks. Updated mode checks the original forecast replay; it does
  not claim that new features match the prior week's features.
- `index.html`: standalone dashboard with full-field search.

Optional `ap_poll.json` supplies a dated external poll or a pending-release notice.
Optional `epa_supplement.json` labels the user-authorized ESPN supplement for
Oklahoma State at West Virginia, game 401856881. That game's EPA is separately
calculated; it must not be described as published cfbfastR EPA. The published
values for all other games are retained. Recovery source files and calculation
provenance live under `cfb_v3/output/2026/week_5_recovery/`.

These checks verify computation and provenance. They do not establish predictive
accuracy for the ranking method. Ratings summarize all matchups, so a difference
between two ratings can differ slightly from their direct matchup prediction.
