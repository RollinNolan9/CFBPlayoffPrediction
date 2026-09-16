# Deeper market-edge investigation

September 12, 2026 UTC. **Research only; production v3 and published picks unchanged.**

## Findings

The primary problem is not simply which blend weight to use. The systems predict
football margins substantially better than they predict the market's mistakes.
Large disagreements with the line do not reliably imply stronger betting edges.
A separate quote-provenance audit also shows that the field named
`closing_home_spread` must not be treated as verified bookmaker closing evidence.

Twelve policies were declared before this experiment's scoring. One produced a
small positive hypothetical return; it failed robustness checks. None establishes
a deployable edge. Prior use of these seasons makes all of this exploratory.

## Fixed policy results

Primary: identical 3,006 lined FBS games from 2022-2025. No non-CFP bowls.
Initial training uses 2020-2021, then only seasons before each test season.

| Policy | Picks | W-L-P | ATS |
|---|---:|---:|---:|
| V3 all nonzero edges | 3006 | 1466-1486-54 | 49.66% |
| V3 absolute edge >=3 | 1620 | 781-810-29 | 49.09% |
| Sagarin all nonzero edges | 3000 | 1506-1440-54 | 51.12% |
| Equal Sagarin/FPI all | 3005 | 1513-1438-54 | 51.27% |
| Equal Sagarin/FPI absolute edge >=3 | 1045 | 508-517-20 | 49.56% |
| V3/Sagarin/FPI unanimous, each edge >=2 | 555 | 265-281-9 | 48.53% |
| Market-residual ridge all | 3006 | 1474-1478-54 | 49.93% |
| Market-residual ridge absolute edge >=2 | 143 | 72-70-1 | 50.70% |
| Direct cover-probability GLM all | 3006 | 1474-1478-54 | 49.93% |
| Cover GLM preferred-side probability >=55% | 168 | 88-78-2 | 53.01% |

The two all-pick learned models have the same aggregate record, not necessarily
identical individual picks. Pushes are excluded from ATS percentages. Passes are
retained in per-game outputs; no-pick policies cannot disappear from the report.

### Why the 53.01% result is not ready

At hypothetical flat risk of one unit per selection and uniform -110 odds:

- Original quote: 88-78-2, +2.00 units, +1.19% hypothetical ROI.
- Half-point worse quote, same sides: 86-80-2, -1.82 units.
- One-point worse quote, same sides: 85-82-1, -4.73 units.
- 2025 alone: 8-8. Only 16 selections, insufficient to verify persistence.
- Where canonical and tracker updated quotes agree within 0.5: 63-67-1 (48.46%).
- 128 of 168 picks came from 2022. Later fits generally became less confident.
- Leave-one-season-out aggregate returns stay slightly positive, but the
  season-cluster interval includes losses, and the twelve-policy-adjusted ATS
  interval spans approximately 41.7%-64.1%. This is not a precise edge estimate.

These are not actual betting returns: historical prices and executable timestamps
are unavailable. Worse-line stress does not assume that points can be bought for
free or that historical prices were uniform. No threshold was retuned afterward.

## Why raw edge size fails as confidence

Across all 3,006 games, the correlation between a model's disagreement with the
market and the actual market error is only 0.013 for v3 and 0.035 for the equal
external average. These are descriptive correlations, not causal estimates or
new coefficients applied to historical predictions.

V3's 533 games with a claimed edge above seven points averaged a claimed 10.34
points, but the realized cushion in its chosen direction averaged only 0.76
points; decisive ATS was 49.72%. That does not mean every large edge is false.
It means the raw gap is not a calibrated estimate of an exploitable advantage.

Some bins have attractive point estimates, including the external average's
above-seven-point bin (56.10% on 123 decisions). Selecting that bin after seeing
it would be a new, outcome-informed strategy, not independent validation. All
predeclared edge bins remain in the diagnostic output; none becomes a live rule.

The market-residual ridge reduces MAE to 12.020, near the historical market
reference's 12.004, but is 49.93% ATS. Moving closer to the line improves average
margin error without reliably identifying which side will cover.

The direct cover model's Brier score is 0.2506 versus 0.2500 for a constant 50%
forecast; log loss is 0.6944 versus log(2) = 0.6931. Its probability estimates do
not improve overall non-push calibration here. A probability model does not
receive an invented margin or straight-up prediction.

## Does v3 add information?

A separate 2,276-game 2023-2025 comparison fits only earlier frozen out-of-fold
v3 predictions. It cannot use in-sample 2020-2021 forecasts as pretend OOF inputs.
The two models have exactly matched fitting and evaluation rows:

| Residual stack | ATS W-L-P | ATS | MAE |
|---|---:|---:|---:|
| External/market only | 1108-1126-42 | 49.60% | 12.009 |
| Same inputs plus OOF v3 edge | 1123-1111-42 | 50.27% | 12.022 |

V3 adds some aggregate ATS wins in this setup, but slightly worsens margin MAE
and does not establish profitability. Do not compare these percentages directly
with the larger 2022-2025 cohort as if they used identical games.

## Historical quote problem

All 3,006 canonical handicaps exactly match the retained compact play-by-play
quote, with one finite quote per game and matching source team orientation.
The trace therefore does not identify a sign flip introduced by this experiment's
join. It also does not prove that the upstream quote was a valid closing line.
`historical_data.R` derives this field from PBP `spread`, then carries it into the
canonical schedule. Bookmaker and observation timestamp are not established there.

Compared with the tracker updated quote:

- Only 1,051/3,006 are exactly identical.
- 2,097/3,006 agree within half a point; 909 differ by more.
- 56 differ by more than three points; the median absolute difference is 0.5.

Different books or times can legitimately disagree. The following spot checks
show why neither source should be substituted blindly:

| Game | Cached PBP/canonical home handicap | Tracker updated home handicap | Dated quote check |
|---|---:|---:|---|
| Colorado vs Colorado State, 2023 | Colorado -17 | Colorado -23.5 | FanDuel article: Colorado -23.5 |
| UNC vs South Carolina, 2023 | UNC +2.5 | UNC -2.5 | SI article: UNC -2.5 |
| Charlotte vs Memphis, 2023 | Charlotte +11 | Charlotte -2 | FOX article: Memphis -10.5 |

Sources: [FanDuel's September 16 slate](https://www.fanduel.com/research/college-football-betting-picks-for-saturday-9-16-23),
[SI's August 30 preview](https://www.si.com/betting/2023/08/30/college-football-week-1-odds-lsu-florida-state-north-carolina-south-carolina),
[FOX's November 11 quote](https://www.foxsports.com/articles/college-football/memphis-vs-charlotte-prediction-odds-picks-november-11).
These corroborate particular published quotes, not the exact closing quote or a
complete independent historical archive. They were not used to rewrite outcomes,
fit inputs or pick a more profitable reference line.

On the fixed 3,004-game four-quote overlap, the external average is 51.27% ATS at
canonical quotes versus 50.54% at tracker updated quotes. Neither establishes an
edge. The opening/midweek comparison is explicitly counterfactual: some forecasts
may not have existed while those quotes were available. Market-aware learners are
never scored at earlier opening quotes using later-line inputs.

## Implementation and verification

- Isolated [runner](../../run_cfb_tracker_edge.R) and [implementation](tracker_edge.R).
- [Pre-scoring protocol](TRACKER_EDGE_PROTOCOL.md) fixes models, thresholds and
  twelve-policy multiplicity accounting. Production feature guards remain intact.
- Market residual ridge reuses the existing training-only recipe and ridge math,
  with fixed penalty 8. [R's GLM](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html)
  fits a three-feature quasibinomial/logit with existing fractional recency weights.
- Saved fold models and standardized coefficients document the actual fitting
  rows and years. Later outcomes cannot affect earlier folds or the same year's
  held-out predictions. There is no end-of-season in-sample replay.
- Nine new test blocks plus 60 existing targeted blocks passed. Repeat runs
  produced identical forecasts/policies; output seals and production hashes pass.
  Benign existing R startup locale/package-version warnings remain.

Accepted run: [full generated report](../output/experiments/tracker_edge/20260912T005442.891Z/REPORT.md).
Alongside it are policies/predictions, all metrics, probability/edge calibration,
season uncertainty, leave-one-season-out, adverse-line stress, source discrepancies,
the PBP quote trace with source hashes, code hashes and fitted fold artifacts.

```powershell
# From the repository root, with the original sealed tracker caches present:
Rscript .\run_cfb_tracker_edge.R
```

The full PBP trace also uses `cfb_v2/cache/pbp/pbp_2022_compact.rds` through 2025.
Missing PBP caches are explicitly labeled unavailable rather than fabricated.
The command does not download new data, update the foundation or change live picks.

## Priority next

1. Establish a provider/timestamp/side-aware historical line ledger. Preserve the
   existing quote as legacy/unverified; independently corroborate discrepancies
   and regrade all models on the same qualifying quotes. Never choose whichever
   source makes a strategy look best.
2. Capture future book quotes and model forecasts at the same real cutoff, with
   spread prices, then separately retain the actual close. That supports actual
   execution and closing-line-value tests which this archive cannot provide.
3. Keep large-gap/agreement confidence rules experimental. Retain the cover model
   as a falsifiable candidate, but do not deploy it from its fragile 53.01% result.
   Further models need new information or a specific diagnosed defect, not more
   outcome-selected weights and filters on these already-examined seasons.
