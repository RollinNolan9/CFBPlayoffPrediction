# Zero-Sample Preseason Retention Results

September 9, 2026. Decision: reject this challenger; retain the existing fade.
No production model, foundation, frozen article card or confidence policy changed.

## What Was Tested

The user-approved [locked protocol](EVIDENCE_WEIGHTING_PROTOCOL.md) keeps full
preseason roster weight for an FBS team with zero usable current-season FBS
efficiency games in Weeks 2-4. Other teams retain the original calendar weights.
The change is team-specific and applied consistently in training and prediction.
Raw FCS EPA remains excluded; existing power and prior-efficiency inputs stay.

Each team's roster values are centered on medians from the outer training
seasons before applying unequal weights. Equal weights recover the original
home-away difference; a weak team's positive percentile cannot accidentally
become a boost merely because its weight increased. Missing priors remain unknown.

Only the roster-aware ridge was refitted, with v3's existing past-only coach and
penalty choices. The foundation is unchanged. Week 5+ and all postseason
forecasts are identical. Refitting can affect early games whose own weights did
not change; those indirect effects are reported separately.

## Primary Result

There are 105 active target games in 2023-2025. Every target has a team following
one FCS opener with zero eligible FBS efficiency games. Of these games, 101 are
in Week 2 and four are in Week 3. Two are ATS pushes.

| Metric | Existing v3 | Full preseason retention |
|---|---:|---:|
| ATS W-L-P | 54-49-2 | 39-64-2 |
| ATS accuracy, excluding pushes | 52.43% | 37.86% |
| Straight-up winners | 78/105 | 75/105 |
| Margin MAE | 13.406 | 14.490 |

Twenty-five ATS selections changed: 20 existing wins became losses and only
five losses became wins. Margin error worsened by 1.085 points, with a descriptive
whole-season 95% interval of +0.728 to +1.505. The ATS difference is -14.56
percentage points. These intervals have only three season clusters and do not
correct for the repeated historical research that preceded this hypothesis.

The decline is not confined to one season:

| Season | Games | Existing ATS | Challenger ATS | Existing MAE | Challenger MAE |
|---|---:|---:|---:|---:|---:|
| 2023 | 27 | 13-12-2 | 7-18-2 | 11.135 | 12.639 |
| 2024 | 43 | 21-22-0 | 15-28-0 | 15.753 | 16.865 |
| 2025 | 35 | 20-15-0 | 17-18-0 | 12.273 | 13.001 |

MAE and ATS remain worse when any one active season is omitted. The 2022 fold
has 29 zero-sample opportunities but roster features are disabled by the existing
two-covered-prior-season gate. Those games stay in aggregate evaluation; they
are not counted as active evidence for or against this intervention.

## Aggregate And Indirect Effects

The comparison uses unchanged v3's full original fitting population: 4,908
eligible 2020-2025 rows, with only preceding seasons used in each fold. This is
NOT the FBS-only v3 refit. Score both variants on the same 3,025 FBS games and
3,024 closing lines as the four-way experiment.

| Slice | Existing ATS | Challenger ATS | Existing MAE | Challenger MAE |
|---|---:|---:|---:|---:|
| All 3,025 games | 1,476-1,494-54 | 1,468-1,502-54 | 12.823 | 12.846 |
| Weeks 2-4, 622 games | 291-319-12 | 277-333-12 | 13.280 | 13.502 |
| Week 0/1, 192 games | 87-104-0 | 93-98-0 | 14.572 | 14.218 |

The Week 0/1 improvement is an indirect coefficient-refit effect, not evidence
that the zero-sample retention rule succeeds on its intended population. It does
not justify selecting another post-hoc deployment window. One Week 0/1 game has
no line and remains in MAE/SU only. Week 5+ and CFP are unchanged.

## Why The Intuition Was Not Enough

Giving the roster prior more weight did bring the target group's average
market-favorite prediction closer to its actual average winning margin:
13.83 points became 14.99, against an actual 15.15. But it made individual
matchups less accurate. Matching an aggregate spread average is not the same as
correctly identifying which teams are mispriced.

For illustration, 2024 Oregon-Boise State moved from Oregon by 16.44 to Oregon
by 22.00 against a closing Oregon -21 line. Oregon won by three, so the change
flipped a winning Boise State ATS selection into a losing Oregon selection.
This example illustrates the measured decline; it was not used to select a rule.

The conclusion is narrow: this blanket 100% retention policy is not supported.
It does not establish that preseason data is worthless, raw FCS EPA should enter
the model, or zero-sample teams should be presented as having normal evidence.
Keep the evidence warnings and the existing fade; do not increase confidence
merely because a team lacks usable current-season efficiency data.

## Week 2 Shadow, Not A New Card

The same candidate fitted through 2025 was applied to the frozen 49-game 2026
Week 2 card. Mean absolute movement is 2.05 points, maximum 6.49. Three ATS sides
flip at the frozen lines: South Alabama-Tulane, Georgia Southern-Clemson and
Arkansas-Utah. Because the candidate failed, none of these changes is adopted.
This run did not refresh injuries, lines, or the production dashboard.

## Verification And Reproduction

- Existing historical v3 reproduced to 4.974e-14 points. Final roster-model replay
  and all 49 original live-card predictions reconcile exactly. Production hashes
  match before/after; Week 5+ and postseason forecasts are exactly unchanged.
- 95 test blocks passed, including seven new blocks covering independent weights,
  centering/symmetry, missing inputs, FCS exclusion, training-only centers,
  chronology, future-label isolation and fixed feature schemas. Existing package
  startup/source-failure tests emitted offline download warnings, not failures.
- The final run only adds metadata clarity versus the first run: uncertainty is
  labeled `reference_margin_sd`, and a feature manifest/runtime report is saved.
  Point predictions, scores, target definitions, fitted choices and shadow
  comparisons are unchanged. No parameter changed after outcomes were viewed.
- Historical source-vintage limitations persist. These are reconstructed pregame
  backtests against closing lines, not proven Friday execution results or ROI.

```sh
Rscript run_cfb_evidence_weighting.R
```

Verified run: `cfb_v3/output/experiments/evidence_weighting/20260909T223251Z/`.
The runner takes no arguments and creates a new timestamped experiment directory
using the declared frozen references. Inspect `metrics.csv`, `paired_comparisons.csv`,
`feature_weight_ledger.csv`, `opener_evidence.csv`, `training_centers.csv`,
`feature_manifest.csv`, `week_2_shadow.csv` and `verification.json` there.
Fitted candidate models, source hashes and runtime information are also preserved.
These generated artifacts remain local; this report preserves the conclusion
when the frozen caches are unavailable on another computer.
