# V1 Versus V3 Protocol

Locked locally before fitting on September 9, 2026. This is not an external
preregistration. The user identified `model.Rmd` at commit `365ca12` (January 23,
2025), with a five-game window, as the working original. Preserve that source and
all current production files; run a separate reconstruction, not the old notebook.

## Models

- V1 reconstruction: randomForest regression, 500 trees, default node size
  5, uniform observation weights. Keep home/away team-name dummy predictors,
  separate home/away rolling EPA (all/pass/rush) and WPA, SP overall/offense/defense,
  and Elo. No additional football variables, market predictors, coach/PFF/injury
  variables or preseason roster variables. Preserve the original mtry grid 2, 4,
  6 and 10-fold training CV (default RMSE selection), entirely inside each outer
  training set. Random training CV does not receive the outer test season.
- Five previous games within the same season, with complete five-game windows
  required, as in the original. Correct the current-game leakage by using only
  games before the target week's cutoff. Use all original EPA-eligible play types;
  do not add the v3 garbage-time/play-type cleaning as another modeling change.
- Use original archived EPA/WPA and SP data from the saved `.RData`, with explicit
  year/coverage checks. Use cached pregame Elo, never same-week postgame Elo.
- Use previous-season SP rather than current-season final SP because the saved
  SP table has no weekly release archive. This is a material, disclosed feature
  change, not an exact reproduction of the original leaky retrospective model.
- Use official v3 schedule/outcomes for 2020 onward to avoid legacy score extraction
  errors. Earlier training outcomes come from the legacy PBP. No 2026 outcomes.
- Preserve the original James Madison exclusion, complete-case filtering, and
  original unweighted training (including
  eligible legacy bowl rows). No bowl ablation yet. V3 already gives non-CFP bowls
  zero training weight and excludes them from its main form history.
- Annual rolling fits on all available original seasons from 2014 through the
  year before each 2022-2025 test season. This updates the original static training
  cutoff chronologically rather than freezing a 2020 fit indefinitely.
- Primary seed 20260909; fixed sensitivity seeds 20260910 and 20260911 refit the
  primary seed's training-selected mtry values without retuning. Report all
  seeds, never select the best. Do not average predictions into a new ensemble.
- V3 reference is the saved `ridge_core` rolling predictions. Do not retrain,
  promote, rewrite production outputs, or alter current settings.

## Comparison

Compare identical FBS-vs-FBS regular-season/CFP games, excluding non-CFP bowls.
Missing input rows and unseen team-side levels are explicitly unscored, not imputed
with new priors. Report coverage by season/week and omission reasons. V1 cannot
give meaningful Week 0/1 predictions under its original complete-window design.

Lead with matched margin MAE, then straight-up and closing-line ATS W-L-P.
Report each season, CFP, Weeks 0/1, Week 2, Week 5+, P4/independents, spreads above
21, and each declared seed. Missing lines do not block margin/SU grading; ATS uses
only matched finite lines, excludes pushes, and abstains on zero model edge.

Paired differences are v1 minus v3, with 5,000 whole-season bootstrap draws, seed
20260909. Four seasons give limited uncertainty estimates. Do not promote based
on a favorable slice, seed, or point estimate. Display the full-v3 record separately
from the matched subset so v1's late-season selection is not mistaken for broad
season coverage.

The 2022-2025 seasons are already familiar from model development, not a pristine
research holdout. Historical closing lines and preseason/backfilled ratings are
not verified Friday publication archives. This comparison tests whether a faithful
pregame reconstruction of the simple original beats v3 on the games it can cover;
it cannot validate the old notebook's headline accuracy or prove a betting edge.

## Source Correction Before Interpretation

The initial pilot `20260909T185651Z` mistakenly retained mtry 6 and three team
exclusions from the sibling six-game notebook. A direct source check identified
the mismatch before its scores were inspected. That pilot is invalid and excluded
from conclusions. The corrected protocol above restores the five-game notebook's
actual CV grid and James Madison-only exclusion. This is a source-fidelity fix,
not tuning based on test performance. All later runs preserve this correction.
