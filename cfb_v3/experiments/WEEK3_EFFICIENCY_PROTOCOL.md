# Week 3 Efficiency Retention Test

Locked September 13, 2026, before fitting. One challenger, no grid search.

- Baseline: unchanged v3, 30% prior-season / 70% current efficiency in Week 3.
- Challenger: 50% prior-season / 50% current efficiency in regular-season Week 3.
- Change only the existing nine metric averages passed through
  `blend_current_with_history`: offense/defense EPA, special teams, pass/rush EPA,
  success rate, havoc allowed/generated, and regressed turnover rate.
- Preserve missing-value fallbacks and past-only FBS eligibility, including bridge
  priors. Do not import FCS EPA, remove duplicates, or replace turnover classification.
- Do not change roster weights (Week 3 remains 30%), power, recent-form columns,
  coaching, season weights, target margins, market quotes, or the feature schema.
- Reconstruct inputs from cached team-game data for both fitting and prediction.
  Reproduce the frozen baseline before accepting any result. Fit on earlier seasons
  only: 2020-2021 warmup and 2022-2025 outer tests. Keep the original past-selected
  fold coach shares and ridge penalties fixed to isolate the feature change.
- Primary endpoint: margin MAE on all eligible Week 3 FBS-vs-FBS games in the
  existing four-way cohort. Secondary: ATS and straight-up accuracy on those same
  games, per-season and leave-one-season-out results, plus the pre-existing
  provider-corroborated quote cohort. Retain unlined games for margin/SU only.
- Prespecified sample slices: minimum home/away usable FBS efficiency games of
  zero, one, two, or three-plus; large spreads (>21); P4/independent audience.
  These are diagnostics, not alternate deployment rules selected after outcomes.
- Report full-season and other-week effects of refitting separately. Unchanged
  feature policy outside Week 3 does not imply unchanged refitted predictions.
- Use the existing nonzero-edge grading rule. Pushes and no-selections are not
  wins/losses; paired ATS deltas require a decision from both candidates. Independently
  verify grades. Show both wins-to-losses and losses-to-wins.
- A useful historical signal must lower Week 3 MAE, survive omission of any one
  season, and not worsen ATS on either quote cohort. Report a descriptive 95%
  whole-season bootstrap interval (5,000 draws, seed 20260913); four reused seasons
  are not independent prospective validation or proof of a betting edge.
- No 2026 outcomes, automatic promotion, production writes, or changed article picks.
  Snapshot inputs, code, protocol, config, full forecasts and feature diagnostics;
  verify unchanged production hashes and repeat determinism.

Historical source-vintage and quote-timing limitations remain. Named-book
corroboration does not prove Friday availability or actual spread prices. This
experiment cannot establish realized ROI or that 50% is globally optimal.

Run offline from the repo root: `Rscript run_cfb_week3_efficiency.R`.
