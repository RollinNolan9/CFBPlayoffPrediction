# Matchup Research: September 11-12, 2026

## Bottom Line

**No new model is promoted.** There are real data defects worth fixing, but this
investigation did not establish a profitable betting edge. Production v3, cached
production datasets, fitted backtests and published picks remain unchanged.

The [sealed full report](../output/experiments/matchup_report/20260912T034256.592Z/REPORT.md)
contains the complete result tables, source inventories and independent grading checks.
That generated report is retained locally, not included by a Git clone alone.

The research tests seven fixed chronological models and 18 fixed selection
policies. It adds pass/rush matchup preferences, sack pressure, high/negative-EPA
play rates, recent form changes and schedule-strength controls to external/market
baselines. Team-name predictors are prohibited; the new matchup signals use only
completed current-season source games whose kickoff precedes the target week's source bucket. Week 0/1 matchup
signals are neutral and CFP games are correctly treated as postseason.

## Same-Game Results

2,955 FBS-vs-FBS games from 2022-2025, with 2020-2021 warm-up. Non-CFP bowls are
excluded. All rows below use the same named-book, provider-corroborated quotes.

| System | ATS W-L-P | ATS | Margin MAE |
| --- | --- | --- | --- |
| Frozen v3 | 1,448-1,447-60 | 50.02% | 12.761 |
| Equal Sagarin/FPI forecast | 1,488-1,406-60 | 51.42% | 12.211 |
| Learned external/market baseline | 1,456-1,439-60 | 50.29% | 11.991 |
| Linear football components | 1,452-1,443-60 | 50.16% | 12.031 |
| All matchup additions | 1,437-1,458-60 | 49.64% | 12.076 |
| Market margin alone | Not an ATS selection | N/A | 11.984 |

The external average has one zero-edge pass. ATS excludes pushes; MAE covers the
full evaluation cohort, not just selected bets. Every fixed primary policy loses
at hypothetical uniform -110 pricing, including the edge-threshold policies.
Those unit illustrations are not realized returns: actual spread prices are absent.

The all-matchup model also performs worse than the linear control after verified
duplicate removal. In 499 fixed-seed negative-control shuffles, about 90% of the
shuffled all-matchup fits have lower MAE than the actual one. This is a descriptive
method check, not a calibrated probability that the real model has no signal.

## What We Actually Found

**Turnover masking.** The shared feature builder prioritizes a composite flag that
is frequently zero on ordinary interceptions and lost fumbles. The flag-only
correction also changes havoc because havoc uses the same resolved flag.
The problem is present in the retained 2026 source bucket too, not only old seasons.

**The alternative flag is not enough.** In a fixed 888-team-game box-score sample,
exact agreement rises from 264 to 721, but 133 corrected counts still fall below
the box total and 34 exceed it. Some differences reflect intentional play filters;
others are verified source/classification defects. Michigan's own fumble recovery
against East Carolina in 2023 is labeled as an opponent recovery. LSU's Chris
Hilton Jr. lost fumble against Clemson in 2025 is labeled `Pass Reception` and
still gets excluded by the giveaway-type gate.
An official gamebook also identifies lost fumbles missing from both cached flags
and descriptions; that makes source completeness and associated EPA a separate
unresolved concern, not something the flag-only test has already repaired.

**Verified duplicate source rows.** Full identity-rich source files contain 5,177
exact duplicates across 2020-2025, mostly 2021. They reproduce the compact caches
exactly before removal. Numeric play IDs also collide across distinct plays, so
deduplicating by play ID or compact rows alone would delete legitimate football.

These issues warrant a bounded data cleanup, not another unmeasured weight change.
The [cleanup contract](TURNOVER_DATA_CONTRACT.md) records the defects, fixtures,
reconciliation requirements and versioned rebuild boundary.

## Did Correcting Them Improve V3?

The isolated flag-precedence correction retains the full original v3 training
population, phase blend, coach shares, penalties and season weights. Original
features and predictions are reproduced before corrected inputs are fitted.

| Full-v3 control, same 2,955 games | ATS | Margin MAE |
| --- | --- | --- |
| Original | 50.017% | 12.761 |
| Flag-precedence corrected | 50.052% | 12.753 |

That is not evidence of a useful betting advantage. It is also not a test of a
fully repaired play classifier: the remaining fumble defects were not silently
fixed inside this candidate. The separate duplicate-only original-football audit
also shows essentially unchanged margin error. Game-score-derived power ratings
do not directly change when duplicate play rows are removed.

## Playoffs And Source Limits

- Shared Sagarin/FPI coverage has only 17 CFP games because the tracker lacks all
  11 FPI forecasts for the 2025 playoff. Frozen v3 is 12-5 ATS and 14-3 SU on those
  17 games. The all-matchup model is 5-12 ATS and 14-3 SU.
- The broader full-v3-only sample retains 28 CFP games on original PBP quotes.
  Original and turnover-corrected v3 are both 18-10 ATS; SU changes from 21-7 to
  22-6. That is a different cohort and quote source, not an external comparison.
- These are actual weekly round matchups, not an entire bracket frozen before
  round one. Small playoff samples do not establish a special playoff policy.
- Historical bookmaker corroboration does not establish the time a quote was
  executable. A fixed 12-game public-source audit recovered 11 quotes, nine exact
  matches; two differ and one remains unresolved. No complete Friday-to-close,
  actual-price record has been recovered.
- All 2022-2025 seasons were examined in earlier research. Chronological folds
  prevent within-fit future leakage but do not make reused history untouched
  validation. Nominal confidence intervals cannot erase that limitation.
- No FPI, PFF, external-rating blend, new matchup feature or corrected candidate
  was added to production during this investigation.

## Verification And Reproduction

The fixed verification passed 147 test blocks, including saved Week 2 card/model
replay and read-only ledger checks. It reproduces 20,685 selected forecasts,
62,055 penalty-candidate forecasts and all 28 fold choices to floating-point
precision. Model coefficients/recipes match; production files match their recorded
research baseline. cfbfastR's installed package emits blocked-network model-load
warnings in the source-failure test; those are not failed tests or new data pulls.
An independent raw-cache preparation exactly reproduces the model inputs,
team-game counts, feature-source audit and quote ledgers. The verification command
now includes that raw-to-feature rebuild before checking forecasts.

Run from the repository root:

```powershell
Rscript run_cfb_matchup_research.R
Rscript run_cfb_matchup_foundation_audit.R
Rscript run_cfb_matchup_foundation_audit.R --turnover-flag
Rscript run_cfb_v3_turnover_audit.R
Rscript run_cfb_matchup_verify.R
Rscript run_cfb_matchup_report.R
```

The first command prepares and fits the seven fixed matchup variants. The next
three are separate data-defect controls, not production rebuilds. Verification
requires the recorded frozen production state; it intentionally stops if that
state has changed. The report command summarizes pinned, sealed audit artifacts.

Retained ignored source bundles, foundation files and experimental outputs are
required. A Git clone alone does not include those caches. None of these commands
automatically downloads replacements or changes the article card.

See [locked protocol](MATCHUP_PROTOCOL.md), [quote audit](MATCHUP_QUOTE_AUDIT.md)
and the sealed artifact inventories under `cfb_v3/output/experiments/`.

## Drive Evidence

Full-source drive outcomes recover evidence of the three omitted sack-fumbles,
but drive-based counts agree with only 709 of 888 sampled team-game box totals.
There are 155 undercounts and 24 overcounts. This is a useful cross-check, not a
replacement classifier. Drive ownership must use `pos_team`, not the kicking-unit
`offense_play` field. Conflicting metadata stays quarantined. No drive-based model
or EPA changes were made.

## Next Step

Fix and reconcile the bounded play-data contract in an isolated candidate first.
Keep the article's published record intact. Evaluate any deployment candidate
prospectively against frozen v3 and external benchmarks at the same publication
cutoff, using actual bookmaker prices. Do not tune another subgroup or reverse
the losing picks on this same historical sample and call the result confirmation.
