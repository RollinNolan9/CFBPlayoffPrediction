# V3 Review And Validation

## Decision

Version 3.0.0 is a correctness-focused candidate, not a demonstrated betting upgrade.
Keep its prospective record separate from v2. The changes repair the audited bugs,
but the lead metric, margin MAE, is slightly worse in the matched rolling comparison.
Do not tune more settings on these same test seasons simply to reverse that result.

## Historical Comparison

Training foundation: 2020-2025, with 2020 half-weighted. Outer test seasons: 2022-2025.
The frozen v2 features and rebuilt v3 features use the same corrected chronological
penalty selection, coach-split selection, and uncertainty procedure. Therefore the
v2 figures below are not necessarily the figures in older v2 reports.

| Slice | Games | V2 MAE | V3 MAE | V2 ATS | V3 ATS |
| --- | ---: | ---: | ---: | ---: | ---: |
| FBS vs FBS | 3,026 | 12.75 | 12.82 | 49.31% | 49.68% |
| Week 0/1, FBS vs FBS | 192 | 14.52 | 14.57 | 45.55% | 45.55% |
| Week 2, FBS vs FBS | 193 | 12.25 | 12.46 | 52.13% | 53.72% |
| Week 5+, regular season FBS vs FBS | 2,145 | 12.48 | 12.54 | 50.02% | 50.40% |
| CFP | 28 | 12.39 | 12.53 | 57.14% | 64.29% |
| Market spreads over 21, FBS vs FBS | 390 | 13.97 | 14.02 | 47.14% | 46.88% |
| FBS transitions | 70 | 13.71 | 13.55 | 53.62% | 62.32% |

ATS excludes missing lines, pushes, and exactly zero model edges. The overall ATS
denominator is 2,971, not 3,026. CFP improvement is two additional covers out of 28.
For overall MAE, v3 minus v2 is +0.071 points; the season-block bootstrap interval
is +0.020 to +0.113. Only four test seasons are available. Smaller slices are diagnostic,
not independent evidence for promoting a model or increasing betting stakes.

The historical market MAE is 12.03 on its available FBS-vs-FBS rows. Neither version
establishes a market-beating model from this comparison. The ATS calibration cannot
produce an acceptable positive-slope fit, so it supplies no validated official plays.
The displayed 50% ATS probabilities in this run are conservative fallback values,
not evidence that every matchup has been independently calibrated to exactly 50%.

## Concrete Corrections

- Rebuilt 1,340,044 play rows into 5,115 games and 10,230 team-game observations.
  Duplicate game keys: zero. Missing FBS-vs-FBS coach assignments: zero.
- NDSU's Week 2 success-rate input is 40.47%, with one eligible FBS game and a
  transition prior. The old input was 17.45%, caused by blending against zero.
- The same cached matchup changes from Air Force favored by 10.99 to NDSU favored
  by 6.59. This illustrates a repaired input, not proof that NDSU will cover.
- Current-season residuals now contribute to 93 coach ledger rows. Destination
  context is supplied consistently. The existing portability formula's 0.70 cap
  still binds for this actual slate; stress tests verify that larger context jumps
  reduce portability. V3 does not claim that every coach's portability changed.
- The ordinary weekly command and dated matchup runner agree on all 49 expected
  margins within 0.0000000001 points.
- Preseason inputs fade to zero after Week 4. From Week 8, historical observations
  collectively receive at most 10% of the power fit's total weight.

## Limits And Next Work

Historical preseason API values are backfills, not verified original release-time
archives. The historical foundation still flags 20 schedule games without PBP and
missing coach assignments on FCS-involved rows; these are visible in foundation QA.
Current coach-source changes require explicit interval overrides after a midseason
departure. Injury inputs remain manual and should be refreshed before publication.

Feature cutoffs currently use Monday-start chronological weeks. Consequently the
September 7 SMU-FSU game is in the same feature bucket as Week 2 and is excluded
from this card's current form. A different cutoff policy must be implemented and
backtested consistently, not applied only to live predictions.

Compare subsequent frozen v2/v3 cards prospectively. Keep large-spread calibration,
coach portability sensitivity, and kickoff-level cutoffs as separate experiments.

## Reproduce

Verification: 54 existing test blocks, 12 new regression blocks, and two real-artifact
checks pass. The final live command writes CSV, Parquet, the model/feature audit files,
and a self-contained Quarto dashboard. Browser-tool visual inspection of the HTML
was blocked by its local-file URL policy; the feature-importance PNG was inspected.

The verified Week 2 run is `live_2026_w02_20260909T005125Z`, using the cached
DraftKings/FanDuel snapshot captured at 2026-09-09 00:51:23.099 UTC. It contains 49
lined games: 38 article picks and 11 large-spread reviews. Refresh inputs before
publication; this is not a newly fetched market snapshot or a frozen article.

See [the run guide](README.md). Run `run_cfb_v3.R --mode=compare` after rebuilding
and backtesting. Full matched predictions and season-block intervals are written to
`cfb_v3/output/comparison/`. The generated outputs are ignored by Git; this summary
and the rebuilt CSV foundation are retained in the repository.
