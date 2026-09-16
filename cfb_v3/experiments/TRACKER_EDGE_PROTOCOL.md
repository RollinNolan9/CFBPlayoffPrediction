# Tracker market-edge experiment

Declared before scoring this experiment, September 12, 2026 UTC.
Exploratory: 2022-2025 outcomes were inspected in earlier experiments and cannot
become an untouched holdout again. No automatic promotion or live-card changes.

## Questions

1. Do external forecasts predict residual errors in the historical market line?
2. Does directly estimating home-cover probability work better than margin MSE?
3. Do fixed strong-edge/agreement rules help, without choosing thresholds after scoring?
4. Does frozen out-of-fold v3 add information beyond external/market inputs?
5. How sensitive are apparent gains to the line source and half-point execution?

## Locked design

- Reuse the sealed tracker run `20260912T003231.716Z`, its eligible training rows,
  weights, frozen v3 predictions and provenance. No new rating systems or backfill.
- Primary evaluation: same 3,006 lined FBS games, 2022-2025, no non-CFP bowls.
  Fit 2020-2021 initially; refit with past seasons only. Existing recency/2020 weights.
- Market-aware ridge: target actual home margin minus market home margin;
  features Sagarin-minus-market, FPI-minus-market, signed market home margin.
  Fixed ridge penalty 8, existing training-only standardization. No penalty search.
- Cover GLM: same three features; target home covers, excluding pushes in fitting.
  R stats::glm quasibinomial/logit handles fractional recency weights without an
  integer-binomial warning. No hyperparameter search or new package. Report
  conditional-on-no-push probabilities, Brier score, log loss and calibration.
  Do not convert this probability into invented margin or SU forecasts.
- OOF v3 addition: separate 2023-2025 cohort, using only previous 2022+ frozen OOF
  v3 forecasts in fitting. Compare external residual ridge with/without v3 edge on
  IDENTICAL fitting rows. Never use in-sample final-model v3 forecasts for warm-up.
- All selections are computed without test outcomes. Complete all declared fits
  and report all results, including zero-pick strategies. No follow-on tuning.

## Twelve fixed policies

Primary ten: v3 all; Sagarin all; external average all; v3 absolute edge >=3;
external average absolute edge >=3; unanimous v3/Sagarin/FPI direction with EACH
absolute edge >=2; residual ridge all; residual ridge absolute edge >=2;
cover GLM all; cover GLM preferred-side probability >=0.55.

Separate cohort two: matched external-residual ridge all; same plus OOF v3 all.

All means nonzero signal, not mandatory wagers. Preserve pass/push distinctions.
No threshold sweeps or selecting the best historical subgroup as a new policy.

## Evidence and grading

- Canonical historical line is the primary comparison. Its column is named
  closing_home_spread, but bookmaker identity and exact observation timestamp
  are not verified. Tracker "updated" is not assumed to be true closing either.
- Audit source differences without replacing the canonical line or selecting the
  most profitable source. Use the fixed subset where both agree within 0.5 as a
  sensitivity check, never as a post-hoc promotion shortcut.
- Closing/late-line-aware learners must NOT be scored at opening lines using
  future quote information. Diagnose raw-v3/external opening comparisons only as
  counterfactual; forecasts may have been published after those quotes vanished.
- Stress the same selected sides at 0.5 and 1 point WORSE lines. Do not change the
  picks or assume free line shopping/buying. Show changes in W-L-P and hypothetical
  flat-risk results at -110; prices are unavailable so these are not actual ROI.
- Report every season, leave-one-season-out stability, season-cluster bootstrap
  intervals and Bonferroni-adjusted binomial intervals across all 12 policies.
  Binomial intervals assume independent games; neither method cures timing issues
  or the reuse of these seasons across prior studies.
- Fixed diagnostic bins: absolute edge 0-1/1-3/3-7/7+, favorite/dog/pick'em,
  spread <=7/7-21/21+, phase, agreement, CFP and source-agreement coverage.
  Diagnostic slices are descriptive, not extra deployable policies.
- A positive aggregate alone does not qualify: examine 2025, season stability,
  uncertainty, half-point stress, coverage and provenance. No claim of a verified
  edge without prospective same-cutoff forecasts and executable prices.

## Deliverables

Isolated runner, per-game outputs, fold models/coefficients, probability calibration,
market audit, all policy metrics/stress/uncertainty, tests and readable results.
Protect production by before/after hashes; hash source code and seal outputs.
