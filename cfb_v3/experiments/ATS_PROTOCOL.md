# Controlled ATS Experiments: Protocol 1

Written before fitting these two challengers. This is a locked local experiment
specification, not an external preregistration or an untouched-data claim.

## Question

Does removing FCS games from the margin learner improve FBS predictions? Does a
separate market-aware learner identify useful errors in the closing line beyond
the independent football model? Outcomes determine the answer, including failure.

## Frozen Controls

- Production 3.0.0 source, configuration, foundation, and published/live cards are
  read-only. No promotion or prediction overwrite is part of this experiment.
- Use the existing 2020-2025 foundation and 2021-2025 preseason priors. Keep 2020
  half-weighted, season decay 0.82, the existing coach candidates, roster-feature
  fade, late-season history cap, and CFP/non-CFP eligibility rules unchanged.
- Cached team ratings and coach histories are identical across experiments.
  Experiment A changes the margin learner's rows, not the underlying rating fit.
  FCS crossover/transition priors and existing power-feature construction remain.
- No network refresh, 2026 outcomes, team-name predictors, FPI, PFF, or poll features.
- Fingerprint source/input files before and after the run. Reproduce the saved v3
  rolling predictions before interpreting either challenger.

## Models

1. **V3 control:** existing football-only rolling model, unchanged.
2. **A: FBS-only:** same fitting and model-selection procedure, with only eligible
   FBS-vs-FBS rows in the margin fits and validation choices. Ridge grid 0.5/2/8/32;
   first test season uses ridge 8 and coach 65/35. Later choices use earlier folds.
3. **B: Market residual:** FBS-only ridge predicts `actual_margin + home_spread`,
   then adds the predicted correction to `-home_spread`. Use the same football
   features plus market-implied margin, its absolute magnitude, market total,
   and a missing-total indicator. Total medians/scaling are learned on each
   training fold only. Ridge grid 8/32/128/512/2048/8192; first fold uses 2048.
   Later penalties minimize margin MAE on earlier folds. Reuse A's chronological
   coach choices. Separate full-roster and foundation fits preserve the exact
   Week 5 switch. No calibrated-cover-probability or official-bet claim.
4. **Market reference:** unadjusted `-home_spread`; it provides a margin benchmark,
   not an ATS betting strategy when its predicted edge is exactly zero.

## Evaluation

- Outer test seasons 2022, 2023, 2024, and 2025, training only on earlier seasons.
  2022 is the default-parameter warmup fold. Preseason variables are enabled only
  when two covered earlier seasons exist, matching v3's original rule.
- Main comparison uses identical eligible FBS games with closing lines for all
  models. A may train on FBS rows without lines; B cannot. Report that coverage
  difference, including missing 2020 lines, instead of implying identical samples.
- Margin MAE remains the lead predictive metric. ATS uses the sign of the model
  edge. Zero edges abstain; pushes are not wins/losses. Keep missing lines visible.
- Report all-game results, every season, Week 0/1, Week 2, Weeks 5+, CFP,
  P4/independent games, market spreads over 21, and the fixed 3-point edge subset.
  These slices are diagnostic, not separate opportunities to declare a winner.
- Show exact binomial 97.5% ATS intervals for each of the two challengers (a
  two-comparison adjustment). These assume independent games; they do not correct
  for the project's earlier experiments or within-team/season dependence.
- Show paired MAE and ATS differences using 5,000 whole-season bootstrap draws,
  seed 20260909. Four seasons provide limited cluster-based uncertainty evidence.
- Illustrative flat-risk ROI assumes -110 for every selected game, with pushes
  returning the stake. It is not actual historical profit or evidence of the odds
  that were available to the author. No stake-sizing recommendation.

## Decision Rule

No automatic promotion. A historical result is only a candidate for prospective
testing if its all-game MAE beats v3, its ATS point estimate beats 52.38%, and its
2025 ATS estimate also beats 52.38%. B additionally must beat market-only MAE.
Report whether the adjusted ATS interval clears break-even; do not call a passing
point estimate proof. Do not revise grids, features, or thresholds after seeing
these results and present the revised run as this original experiment.

The 2025 results have already been examined during model development. They are
chronologically out of sample for these fits, but are not a pristine research
holdout. Historical API preseason values are backfills. Market fields are cached
cfbfastR spread/over-under data without verified Friday publication-time archives.
A locked future prediction ledger, followed by honest grading, is still required
before making a reader-facing claim that past success generalizes.

## Run

From the repository root:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_ats_experiments.R
```

Every run writes a separate timestamped directory under
`cfb_v3/output/experiments/controlled_ats/`, including this protocol, source/input
hashes, predictions, fold choices, metrics, paired uncertainty, and a report.
