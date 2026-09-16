# Bounded Play Cleanup Results

Status: full offline audit, fixed comparison and repeat verification complete.
No candidate is promoted. The classifier remains an audit/partial
repair tool, not a certified replacement for v3's production dataset.

## Fixed Comparison

All profiles score the same 2,955 qualified-quote games in 2022-2025. ATS excludes
60 pushes per profile; MAE uses the whole cohort. All profiles lose at hypothetical
-110 pricing. Actual prices and original publication-time execution are unverified.

| Profile | ATS W-L-P | ATS | Margin MAE |
| --- | --- | --- | --- |
| Frozen v3 | 1448-1447-60 | 50.017% | 12.76070 |
| Exact duplicates removed | 1449-1446-60 | 50.052% | 12.76039 |
| Duplicates plus classifier | 1445-1450-60 | 49.914% | 12.77489 |
| Duplicates, turnover/havoc predictors removed | 1448-1447-60 | 50.017% | 12.75783 |

The ablation has the same aggregate ATS record, not identical individual picks.
Its small MAE difference is not a demonstrated advantage. The classified candidate
has worse aggregate MAE and ATS than frozen v3. There is no basis for deployment
or for adding another tuned threshold on this reused history.

On the shared 17-game CFP subset, every profile is 12-5 ATS and 14-3 SU.
This is not all historical CFP games: the fixed external-coverage cohort excludes
the 2025 playoff's missing FPI rows. These are weekly actual matchups, not a bracket
frozen before the first round. See the
[sealed comparison report](../output/experiments/play_cleanup_comparison/20260912T185102.186Z/REPORT.md).

## Data Quality And Coverage

The pipeline examined 1,340,044 full-source rows and removed 5,177 proven exact
duplicates. All 10,230 reconstructed original team-game rows match the frozen
baseline. Classifier changes leave all other numeric team-game metrics unchanged,
including EPA, success, play counts, special teams, scores and margins.

The strict 888-team-game audit confirms 676 event-level giveaways as a lower bound,
against 1,226 reported box-score turnovers. Counts match on 508 team-games, but only
90 have no remaining automated review blockers; 798 require review. This does not
establish that 798 games are wrong or that the other games are independently
verified. The new conservative lower-bound counts are not comparable to the old
flag-only count as a claim of improved or worsened source accuracy.

The candidate sacrifices substantial coverage: turnover rates are missing in
6,365/10,230 team-games, versus 40 originally. Havoc rates are missing in 5,579,
versus 40. These counts refer to reconstructed team-game rows, not all raw-source
teams; the generated report's rate-cell table covers the wider source population.
Existing past-only summaries skip missing cells and can fall back to prior-season
values. Training feature missingness is separately recorded. This creates a risk
of relying on fewer current games or more prior-season information, especially for
rapid risers and declining teams; missing rate cells are not a count of actual
prior-season fallbacks. The lost coverage is another reason not to promote this candidate.

**Recommendation:** keep the automation for auditing, preserve v3 as the benchmark,
and avoid manually reconstructing hundreds of games just to retain weak predictors.
A bounded next candidate could use validated full-game box-score turnover totals
as separately named features and omit reconstructed havoc. That is a different
feature definition, not a PBP/EPA repair, and has not been tested in this run.

## What Was Built

Two delegated agents owned the classifier and the reconciliation report in
separate files. The parent integrated the rate adapter and fixed-model comparison.
No production code, individual EPA value, cached source or published card was edited.

- A shared event classifier preserves original fields and appends outcome,
  eligibility, resolution status, reason and evidence.
- A bounded box-score audit separates confirmed-event lower bounds from full-game
  reported totals and from the model's filtered play subset.
- An exception queue retains play/drive identity, context and reasons for review.
- Four fixed profiles compare original v3, duplicate-only inputs, classified
  duplicate-free inputs, and a duplicate-only turnover/havoc ablation.
- Source, code, output and production hashes guard reproducibility.

## Independent Review Fixes

Review and fixtures caught the following before accepting any model comparison:

- Explicit no-play/nullification wording that could leave a false interception.
- Team aliases and explicit ownership contradictions that could create a false
  opposing-team interception.
- Legitimately missing administrative-play metadata creating unnecessary review rows.
- Unknown scrimmage eligibility disappearing from rate aggregation despite an
  unresolved classifier status.
- Resolution summaries counting known giveaway flags instead of actual statuses.
- Data-table dispatch and source-loader metadata compatibility at the experimental
  boundary. Raw-value and metadata checks remain strict.

The synthetic review fixtures demonstrate possible failure paths, not measured
historical prevalence. Retained real-source fixtures also cover Michigan's own
recovery, LSU-Clemson lost fumbles and Miami's omitted sack-fumbles. The last group
remains unresolved; no arbitrary turnover penalty was substituted into EPA.

## Verification

All 54 cleanup test blocks passed (23 classifier, 20 audit, 11 integration), along
with the existing 147-block verification suite: 201 unique test blocks in total.
The existing verification also reproduced raw-to-feature inputs and frozen research
forecasts. Package-built-under-newer-R and startup locale warnings are not test failures.

The [repeat comparison](../output/experiments/play_cleanup_comparison/20260912T185447.460Z/REPORT.md)
exactly reproduces predictions, metrics, feature changes/missingness, fold choices,
audit summaries, full forecasts and both candidate training-input objects.
An independent score-plus-home-spread calculation verifies all 11,820 prediction
grades. All 88 protected production files and 12 full/compact source files retain
their hashes. See the
[verification summary](../output/experiments/play_cleanup_verification/20260912T185759.176Z/verification_summary.csv).

Earlier failed or deliberately interrupted runs remain unsealed and are not
accepted evidence. Neither production nor published cards were changed. No commit
or push was performed.

## Run And Inspect

```powershell
Rscript run_cfb_play_cleanup.R
```

See the [run and review guide](PLAY_CLEANUP_GUIDE.md) and
[fixed protocol](PLAY_CLEANUP_PROTOCOL.md). The command requires retained ignored
caches, which are not supplied by a Git clone alone.
