# Early-Season And Large-Spread Investigation

September 9, 2026. Status: audit complete; ATS weakness remains unresolved.
No production model, frozen card, foundation, or feature configuration changed.
One exploratory calibration challenger tested and not promoted.

Subsequent user-approved zero-sample preseason retention testing is now complete:
see [the separate results](EVIDENCE_WEIGHTING_RESULTS.md). The candidate was rejected;
the next-work section below records the recommendation at this audit's conclusion.

## Most Important Finding For Friday

The frozen 49-game Week 2 article card contains 43 teams with no eligible
current-season FBS efficiency sample. That affects 36 matchups; seven have no
such sample on either side. Every one of these 43 teams played only FCS opponents
before the common feature cutoff. No missing eligible FBS play-by-play explained
these zeros. Twenty-two affected matchups currently carry the `standard` flag.

This is not complete absence of current-season information: FCS results still
enter opponent-adjusted power at reduced weight. But raw FCS EPA is excluded by
policy. When no current eligible EPA exists, `blend_current_with_history()`
substitutes the prior-year value before blending. Thus the displayed 60%
preseason feature weight does NOT mean every forecast has 40% new-season
efficiency evidence. Also, Week 2 uses the full roster-aware model with its
roster inputs faded to 60%, not a literal 60/40 split between two margins.

The [review queue](../output/experiments/early_spread_audit/20260909T221201Z/week_2_review_queue.csv)
contains all 49 games with the existing forecast, frozen line, evidence status
for each team, roster adjustment and selected feature contributions. It is a
diagnostic companion, not a replacement betting card. For example:

| Game | Home FBS efficiency games | Away FBS efficiency games | Existing home-margin forecast | Frozen home spread |
|---|---:|---:|---:|---:|
| Maryland at Connecticut | 0 | 0 | +1.50 | +12.5 |
| Old Dominion at Virginia Tech | 0 | 0 | +10.77 | -19.5 |
| South Florida at Army | 0 | 1 | -10.85 | -3.5 |
| Washington State at Kansas State | 0 | 1 | +7.79 | -19.5 |

Positive forecast means the home team wins by that many points. Positive spread
means the home team is the market underdog. These are September 9 cached lines,
not fresh Friday prices, and large discrepancies do not establish betting value.

## Source And Feature Audit

- Reconstructed all eight production preseason features for 2021-2026 from the
  existing returning-production, talent, portal, prior-player-PPA and poll caches,
  plus eligible prior-year efficiency. Zero discrepancies with frozen priors.
  No duplicate FBS talent or returning-production team keys were found. Two
  duplicate 2023 talent records are FCS teams outside the model's FBS universe.
- Missingness still matters even with correct joins: Air Force and Navy have
  missing 2025 talent values in the cache. New FBS entrants and COVID opt-outs
  also have expected missing returning/history inputs. Reconstruction parity
  does not imply every field is available or the underlying source is correct.
- Returning PPA ratios are not bounded roster-retention percentages. New Mexico
  State's 2025 passing-PPA ratio is -26.167; its direct contribution reaches
  +8.21 points against Tulsa in Week 2 of that backtest. A different matchup has
  the opposite sign. This is a representation risk, not grounds to replace raw
  values after seeing outcomes. In the current 49-game card, that feature's
  largest absolute direct contribution is only 0.359 points (mean 0.026), so it
  does not explain the current card's largest discrepancies.
- Portal rating coverage varies: rated incoming players are 307/816 in 2021
  versus 2,577/3,076 in 2026. Player-PPA matches are also incomplete. Missing
  scouting/production is not evidence of zero talent; historical measurement
  quality and changing portal volume remain modeling limitations.
- The 2022 outer fold intentionally disables roster features because only one
  earlier roster season exists. It is not a full preseason-layer evaluation.
  Matched v3 Week 0/1 MAE is 17.91 in 2022, 11.58 in 2023, 14.04 in 2024 and
  14.35 in 2025. Keep 2022 in declared aggregate results, but identify this regime
  difference rather than presenting all four seasons as identical coverage.
- Current-year FSU-SMU on September 7 is excluded by the existing Monday bucket.
  Neither team is on this 49-game slate. The general kickoff-versus-weekly-cutoff
  issue remains on the backlog; this audit does not silently change that policy.

Historical source release timestamps remain independently unverified. Matching
today's cached source reconstruction does not prove historical availability.

## Large Spreads: Not A Hard Cap

Unchanged v3 forecasts exceed 21 points in 376 of the 3,025 common historical
games, with a maximum absolute margin of 58.44. There is no current 21-point cap.

On the 390 games where the MARKET spread exceeds 21, unchanged v3 forecasts
25.27 points for the market favorite on average, versus an actual 28.52 and a
market 28.81. That looks compressed conditional on the market's selection.

But on the 376 games where the MODEL predicts a margin above 21, its own favored
team's margin is overstated by 1.25 points on average. These are different game
sets. Blindly stretching all model margins confuses missing market information
with universal underprediction and is not supported by this audit.

## Calibration Experiment: No Switch

The [locked follow-up protocol](EARLY_CALIBRATION_PROTOCOL.md) tests one candidate:
a one-feature residual ridge fitted separately for Week 0/1 and Weeks 2-4 using
only earlier-season out-of-sample v3 errors. Minimum 100 earlier phase games,
fixed lambda 8 and 0.82 annual decay. Current season outcomes and market lines
cannot enter the fit. Week 5 onward and all postseason margins stay identical.

| Metric | Unchanged v3 | Early calibration |
|---|---:|---:|
| Overall margin MAE | 12.82267 | 12.82098 |
| Overall ATS W-L-P | 1,476-1,494-54 | 1,476-1,494-54 |
| Overall straight-up wins | 2,177/3,025 | 2,173/3,025 |
| Week 0/1 MAE | 14.57214 | 14.53003 |
| Week 0/1 ATS W-L | 87-104 | 90-101 |
| Weeks 2-4 MAE | 13.27972 | 13.28446 |
| Weeks 2-4 ATS W-L-P | 291-319-12 | 288-322-12 |

Primary early-season paired MAE difference: -0.0063 points, descriptive 95%
whole-season interval -0.0370 to +0.0280. Only 552 games receive a fitted
correction; earlier phases without enough history stay unchanged. Week 0/1
calibration first activates in 2024, leaving little independent validation.
The overall improvement is negligible, ATS is unchanged, SU worsens, and the
four season clusters were already examined before choosing this hypothesis.

The optional 2026 shadow applies the same fixed rule trained through 2025. Its
Week 2 slope is 1.0522 and intercept -0.0274. It changes margins by a mean 0.72
points and flips only South Alabama/Tulane across a near-zero edge at the old
line. Those shadow numbers are NOT approved picks or an updated article card.

## Recommended Next Work

1. Use the evidence queue when reviewing the current card. Promote evidence
   status to future dashboard flags so `standard` cannot conceal zero current
   FBS efficiency samples. That is a transparency fix, not a claim of better ATS.
2. Design a separate, locked test of early-season weighting based on usable
   current FBS samples, not calendar week alone. Preserve the hard roster-feature
   shutoff after Week 4. Do not import unadjusted FCS EPA or extend preseason
   talent into playoff forecasts. This candidate has NOT been implemented or
   tested by this investigation.
3. Keep unstable passing-PPA ratios and portal measurement coverage as separate
   representation questions. Do not combine them with weighting changes and
   lose the ability to attribute results.
4. Refresh injury evidence and execution-time lines before Friday's freeze.
   This investigation used existing caches and did not perform that refresh.
   Keep requested article selections distinct from validated betting plays.

## Verification And Reproduction

- All 3,025 matched historical projections and their feature contributions
  reconcile; replay difference is zero. All 49 saved Week 2 margins reproduce
  exactly. Protected production source, data, models and predictions hash
  identically before/after each run and across the audit reruns.
- 88 test blocks passed across core v2, v3, saved-artifact, ATS, four-way and six
  new audit/calibration blocks. The existing cfbfastR source-failure test emitted
  offline package-model download warnings; there were no test failures. Fresh
  source connectivity was not verified.
- New tests cover line-matched metrics, phase exclusion, minimum history,
  training-only calibration, future-label and market-line invariance, exact
  inactive predictions, coefficient reconciliation and duplicate-key failures.

```sh
Rscript run_cfb_early_spread_audit.R
Rscript run_cfb_early_calibration.R
```

Both commands target the declared frozen reference files and create new local
timestamped outputs; they do not refresh the weekly production card.

Audit artifacts: `cfb_v3/output/experiments/early_spread_audit/20260909T221201Z/`.
Calibration artifacts: `cfb_v3/output/experiments/early_calibration/20260909T220529Z/`.
The former contains source/feature coverage, per-game decompositions, coefficient
diagnostics and the Week 2 evidence queue. The latter contains the locked
protocol, paired predictions, intervals, omission checks, fitted calibrators,
coverage choices and the unpromoted shadow card. Generated artifacts remain local;
this repository report preserves the conclusions when those caches are absent.
