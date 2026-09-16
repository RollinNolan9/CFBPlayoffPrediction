# Week 3 Efficiency Retention Results

September 13, 2026. **Do not promote the 50% challenger.** Production remains
unchanged. This does not establish that the existing 30% setting is optimal or
that either model has an ATS edge.

## Test

The [locked protocol](WEEK3_EFFICIENCY_PROTOCOL.md) compared 30% versus 50%
prior-season efficiency in regular-season Week 3, with complementary 70% versus
50% current-season efficiency when both samples exist. Nine existing efficiency
metrics change; roster/talent/portal features still receive their original 30%
Week 3 multiplier. Power, coaching, recent form, training weights, raw PBP and
missing-value/transition fallbacks remain unchanged.

Reconstructed both fitting and test features from cached team-game observations.
Used 4,908 eligible fitting rows, 2020-2021 warmup and rolling 2022-2025 tests,
with original past-selected coach splits and ridge penalties held fixed. No
2026 results or market lines entered model fitting.

## Primary Week 3 Results

Each row within a quote cohort uses exactly the same games and handicaps.

| Quote cohort | Prior efficiency weight | Games | ATS W-L-P | ATS | Margin MAE | SU |
|---|---:|---:|---|---:|---:|---:|
| Original cached lines | 30% | 203 | 89-113-1 | 44.06% | 14.596 | 154/203 |
| Original cached lines | 50% | 203 | 90-112-1 | 44.55% | 14.617 | 153/203 |
| Corroborated quotes | 30% | 201 | 92-109-0 | 45.77% | 14.604 | 153/201 |
| Corroborated quotes | 50% | 201 | 91-110-0 | 45.27% | 14.635 | 152/201 |

The original-line gain is one additional cover, entirely in 2022. ATS is unchanged
in 2023, 2024 and 2025 on those original lines. On corroborated quotes the net
change is one lost cover, in 2024. The original cohort flips three selections
(one win to loss, two losses to wins); corroborated quotes flip five (three wins
to losses, two losses to wins).

MAE is slightly worse in three of four seasons, and still worse when any one
season is omitted. Overall Week 3 MAE changes by +0.020 points on original lines
(descriptive season-bootstrap 95% interval -0.009 to +0.053) and +0.031 on the
corroborated subset (-0.009 to +0.084). These differences are tiny, not proof of
a meaningful superiority for either weight. The predeclared signal screen fails.

## The Small-Sample Concern

The minimum of the two teams' usable current-season FBS efficiency samples is:

| Minimum sample | Games | 30% ATS | 50% ATS | 30% MAE | 50% MAE |
|---|---:|---|---|---:|---:|
| Zero | 4 | 3-1 | 3-1 | 25.640 | 25.281 |
| One | 183 | 79-103-1 | 80-102-1 | 14.391 | 14.399 |
| Two | 16 | 7-9 | 7-9 | 14.182 | 14.439 |
| Three or more | 0 | Not available | Not available | Not available | Not available |

Most matchups really do have very little current FBS evidence. Nevertheless,
this particular reweighting does not improve their margin estimates. The zero
sample slice is only four games, not grounds for a new subgroup rule. Counts
refer to net-efficiency observations; individual metrics can have fewer valid
cells. Missing current data still falls back to history, not zero.

For original-line spreads above 21, ATS slips from 21-33 to 20-34 and MAE rises
from 14.835 to 14.923. No large-spread rescue is demonstrated.

## Other Weeks And Limits

Only Week 3 input metrics change, but refitted coefficients move other-week
forecasts. Full-season original-line ATS is 1,476-1,494-54 versus 1,477-1,493-54;
MAE rises from 12.823 to 12.826. On corroborated quotes ATS is 1,448-1,447-60
versus 1,445-1,450-60. These are diagnostics, not an all-season deployment proposal.

These four historical seasons have already been examined in earlier research.
There is no fresh holdout, demonstrated betting profitability, or actual ROI.
Original quote provenance is weak; the 2,955-game corroborated cohort improves
book/source agreement but does not establish Friday execution times or prices.
The candidate retains existing PBP defects to isolate the requested weight change.

## Verification And Reproduction

- 92 focused test blocks pass, including 11 new experiment blocks. Existing
  source-failure tests emit expected offline package-download warnings.
- All 3,511 original rolling forecasts reproduce within 4.98e-14 points.
  Original reconstructed features match within the 1e-8 gate.
- Only the nine declared metric differential families change (255 fitting rows
  per family); roster, power, coaching and other feature values remain unchanged.
- Repeat reconstruction/fitting yields identical training inputs, forecasts and
  nine metric/diagnostic files. The final run only adds identity checks, an empty
  slice test and the explicit `reference_margin_sd` label; no predictive rule changed.
- 11,960 original/corroborated forecast grades independently match. All 88
  protected production files retain their hashes, including published cards.
- No production promotion, foundation rebuild, commit or push.

Run from the repository root with retained local caches:

```powershell
Rscript run_cfb_week3_efficiency.R
```

Accepted run: `cfb_v3/output/experiments/week3_efficiency/20260913T171211.076Z/`.
Earlier numeric repeat: `cfb_v3/output/experiments/week3_efficiency/20260913T170822.516Z/`.
Independent verification: `cfb_v3/output/experiments/week3_efficiency_verification/20260913T171531.638Z/`.
Outputs include per-game forecasts, sample/feature ledgers, per-season and
leave-one-season-out comparisons, serialized inputs, source/code hashes and a report.
