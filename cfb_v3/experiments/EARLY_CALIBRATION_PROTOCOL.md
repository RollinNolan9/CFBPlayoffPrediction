# Early-Season Calibration Challenger

Locked September 9, 2026 after the early/spread audit, before running this
challenger. This is explicitly exploratory follow-up on already-examined years,
not a pristine holdout or external preregistration.

## Hypothesis

Current v3's Week 0/1 historical regression of actual margin on forecast margin
has slope about 0.76 overall and below 1 in each observed season. Large-market-
spread underprediction is not evidence for globally stretching predictions:
model-selected large favorites already slightly overpredict winning margins.
Test early-phase calibration, not global expansion or a market-driven correction.

## One Fixed Candidate

- Base: unchanged `v3_control` predictions from the four-way run
  `20260909T214828Z`, with its original v3 fitting population. Do not refit v3,
  change its inputs, or silently substitute the matched-FBS v3 refit.
- Calibration learns `actual_margin - expected_margin` from the base forecast,
  using the existing weighted ridge with ONE input, `expected_margin`.
  Add the learned correction to the base forecast.
- Fit independent groups for regular Week 0/1 and Weeks 2-4. All other weeks and
  postseason games receive an exactly zero correction, including CFP.
- Each test season uses ONLY base-model out-of-sample predictions/outcomes from
  earlier seasons, never fitted training predictions or current-season results.
  Use uncalibrated base predictions to train, not recursively corrected forecasts.
- Require 100 earlier games in a phase; otherwise use identity. Fixed lambda 8,
  existing training-only numeric recipe, and 0.82 annual decay normalized so the
  latest available calibration year has weight 1. No grid search, slope cap,
  threshold tuning, manual exceptions, or second calibration candidate.
- Calibration uses all available common FBS rows regardless of line availability.
  Do not use spread, total, team name, or outcomes to select individual games.
  Note that 2022 predates the roster-feature activation gate; retaining its
  historical errors is part of this fixed experiment, with that limitation stated.

## Reporting And Decision

Score on the same 3,025 common 2022-2025 games and same closing spreads. Primary
comparison: paired MAE over regular Weeks 0-4. Report overall, phase/year, large
market spreads, SU, ATS W-L-P, coverage/fallback counts and correction magnitude.
Also report the number of games actually corrected; unchanged fallback games
must not be portrayed as independently validating calibration.

Use the existing 5,000-draw whole-season bootstrap, seed 20260909, descriptive
95% intervals and leave-one-season-out differences. Four clusters, earlier use
of these outcomes and audit-driven hypothesis selection limit inference. No
statistical or ATS result automatically authorizes promotion. No ROI claim.

After scoring, optionally apply the SAME rule fitted through 2025 to the frozen
2026 Week 2 preliminary card, as a labeled shadow comparison only. Do not publish
new picks, refresh sources, overwrite the article card, or select from 2026 results.
Preserve protocol, code/input hashes, paired rows, fitted calibrators and choices.
Production data, code, saved models and predictions must hash identically after.
