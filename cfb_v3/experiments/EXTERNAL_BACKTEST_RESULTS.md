# Historical SP+ Comparison

## Bottom Line

SP+ improves margin accuracy modestly on the available historical sample. Its
ATS improvement, and the fixed blend's ATS improvement, rely heavily on 2025.
This supports continued comparison, **not switching production or claiming a
profitable edge**. V3, its saved models and the published Week 2 card are unchanged.

Run: `cfb_v3/output/experiments/external_backtest/20260911T231911.953Z/`
Rerun offline: `Rscript run_cfb_external_backtest.R`.

## Primary Results

**336 identical lined games from six weekly cycles across 2022-2025**, not four full
seasons. Both teams require previously corroborated SP+ rows, raw-source parity,
rechecked pre-cycle records, and dated corroboration before kickoff. Only 336 of
3,026 eligible v3 reference games qualify for this primary comparison.

| Forecast | ATS W-L-P | No ATS pick | ATS accuracy | SU accuracy | Margin MAE |
|---|---:|---:|---:|---:|---:|
| Unchanged v3 | 162-163-11 | 0 | 49.85% | 71.43% | 12.43 |
| SP+ rating-derived | 166-153-11 | 6 | 52.04% | 72.62% | 12.13 |
| 50% v3 / 50% SP+ | 170-155-11 | 0 | 52.31% | 72.02% | 12.14 |
| Closing-market margin reference | Not an ATS strategy | 336 | N/A | 71.43% | 12.00 |

Lower MAE is better. SP+ uses its numeric overall rating differential plus a fixed
2.4-point home adjustment, zero for neutral sites. This is not its publisher's
official matchup forecast. The SP-only blend is separate from the prospective
three-external-source consensus; no missing-source weights were reallocated.

All forecasts use the same games and closing lines. ATS percentages exclude pushes
and no-picks. The paired ATS comparison additionally uses only games on which both
models select a side. Numerical edges smaller than `1e-8` count as zero, preventing
floating-point subtraction from creating a selection at a mathematically fair line.

At hypothetical uniform -110 pricing, break-even ATS accuracy is 52.38%, slightly
above even this blend's point estimate. Actual spread prices are unavailable, so
these are not realized ROI figures. The market still leads aggregate margin MAE.

## Robustness

- SP+ reduces MAE by 0.303 points versus v3; the blend reduces it by 0.286. Both
  retain a lower pooled MAE when any one season is omitted.
- Paired ATS uncertainty includes no advantage: SP+ versus v3 is +2.51 percentage
  points on their 319 common decisions, with a descriptive season-block interval
  of -7.14 to +8.36 points. The blend is +2.46 points on 325 decisions, interval
  -1.00 to +4.31 points. Four seasons and sparse weekly coverage limit these intervals.
- On the **172 selected 2025 games, from only three weeks**, v3 is 84-82-6 ATS,
  SP+ is 97-66-6 with three no-picks, and the blend is 92-74-6. These are not
  full-season records. Excluding 2025, SP+ loses its paired ATS advantage and the
  blend's paired ATS advantage is exactly zero.
- For 28 spreads above 21 points, SP+ is 17-10 with one no-pick; v3 and the blend
  are both 14-14. This small diagnostic slice is not a new betting rule.

## Timing Sensitivity

The one-cycle-delayed lane covers 338 games, with v3 at 48.20% ATS, SP+ at 48.05%,
and the blend at 48.50%. **Those are different games**, so the primary-versus-delayed
aggregate difference cannot be attributed to source age.

On the 114 games shared by both timing lanes, v3's forecasts and 46-65-3 record
remain identical. The blend is 50-61-3 in both lanes. SP+ is 50-58-3 with three
no-picks using the current-cycle table, versus 52-58-3 with one no-pick using the
previous-cycle table. Current-cycle SP+ has lower MAE (12.55 versus 12.68).
Neither timing treatment establishes a betting edge on that common subset.

## What This Cannot Answer

There are **zero qualified CFP games and zero Week 0-4 games** in the primary sample.
This test cannot validate the original perfect bracket or choose this week's early-
season blend. Historical SP+ tables are corroborated reconstructions, not independently
timestamped originals; record checks do not prove the tables were never revised.
The dataset has already informed earlier investigations and is not a fresh holdout.

FPI and Sagarin were not backtested using annual final ratings. CFBD's documented
[FPI endpoint](https://api.collegefootballdata.com/api/ratings) has no week/as-of selector;
an annual response is not accepted as a historical pregame snapshot.
An [ESPN historical page](https://www.espn.com/college-football/playoffPicture/_/week/8/year/2024)
retains selected 2024 Week 8 and ranked-team FPI values despite a Final heading.
It remains a candidate for validation, not scored evidence. The saved probe is in
`cfb_v3/output/experiments/external_history_sources/probe_20260911T230923.411Z/`.
No accepted historical Sagarin weekly archive was retrieved in this run.

Next: validate the FPI weekly-page lead, expand independently corroborated SP+
coverage (especially early season and CFP), and continue the frozen prospective
benchmark. Do not optimize blend weights on this already-examined sample.

## Verification

Eight new backtest test blocks plus 33 existing external/four-way/v3 blocks passed.
The run rechecks raw workbook hashes, numerical anchors, records, game identities,
venue and closing-line joins. It records every excluded matchup, paired differences,
leave-one-season-out results, common-game timing metrics and output checksums.
All protected production-file hashes remained unchanged during the run.

The earlier `20260911T231631.129Z` diagnostic run is superseded for ATS/SU counts:
the final run removes floating-point phantom selections. Forecast margins and
MAE are unchanged. See [the protocol](EXTERNAL_BACKTEST_PROTOCOL.md) for the fixed rules.
