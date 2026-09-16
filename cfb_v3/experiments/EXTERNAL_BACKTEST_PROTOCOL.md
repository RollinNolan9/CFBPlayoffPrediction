# Historical External Benchmark Protocol

Locked before examining this comparison's results. Production v3, publication
cards and the prospective external benchmark remain unchanged.

## Available Test

Compare frozen v3 rolling out-of-season forecasts with:

- SP+ overall rating differential plus 2.4 home points (zero at neutral sites).
- Fixed 50% v3 / 50% SP+ margin. This is a separate SP-only challenger, **not** the
  three-source consensus from the prospective benchmark. No weights are fitted.
- Negative closing home spread as a margin/SU reference, never an ATS strategy.

Historical FPI and Sagarin are not filled from current/annual-final values. The
available SP+ workbook reconstructions are the previously audited 2022-2025 sources.
This does not establish a complete SP+/FPI/Sagarin four-way historical comparison.

## Evidence And Timing

The primary lane is **same_cycle_corroborated**. A team row must have passed the
existing numerical-anchor and record checks, match its retained raw workbook, and
still match the historical schedule's record before that weekly cycle. Failed
numeric anchors reject the entire table. Previously rejected rows are not promoted.

The inferred Monday cycle must equal the target game's feature-week start, and
kickoff must follow all dated corroborating evidence. Because evidence dates have
no precise timezone/time, use the date plus 36 hours UTC as a conservative upper
bound on that calendar day's publication worldwide. A later article explicitly
quoting last week's value does **not** qualify that value for last week's games.

The sensitivity lane is **one_cycle_delayed**: use exactly the previous weekly
cycle, subject to the same evidence-date gate. Never carry a table across a longer
gap. Report timing sensitivity both on its full cohort and the intersection of
games shared with the primary lane; unequal cohorts are not a timing experiment.

These are **corroborated reconstructions, not independently timestamped originals**.
Dates establish corroborating evidence, not the exact historical workbook release.
Record matching cannot prove ratings were never revised retrospectively. Sparse
coverage and retrospective source validation limit any claim of superiority.

## Matched Scoring

Use the saved `ridge_core` rolling predictions from v3; do not refit or tune on
evaluated seasons. Match official game identities, season, final margin, venue and
closing line. Restrict 2022-2025 comparisons to FBS-vs-FBS games, excluding non-CFP
bowls. Missing matchup ratings, dates, or invalid records produce explicit exclusions.

Every scored game has all compared forecasts. Overall margin metrics may retain
unlined games; the common-lined slice is the fair comparison with market margins.
Winner ties/zero-margin forecasts are no-picks; pushes do not enter ATS percentages.
Evaluate every nonzero edge, with no outcome-tuned threshold or forced selection.
Treat absolute edges/margins below `1e-8` as numerical zero, not a betting filter;
decimal rating arithmetic can otherwise create spurious picks at exactly fair lines.

Report margin MAE, RMSE, signed bias, SU, ATS counts, per-season results, large spreads,
CFP, Week 0/1, Weeks 2-4, Week 5+, and article audience. Paired MAE/ATS differences
use identical games; season-block bootstrap and leave-one-season-out summaries are
descriptive uncertainty checks, not model-promotion tests. No probability calibration
or actual betting ROI is inferred without the necessary probability/price histories.
Historical closing spreads are retrospective comparisons, not Friday executable quotes.

No automatic promotion, new feature ingestion, or change to the article picks.
This dataset has informed previous experiments, so it is not an untouched final
holdout. Prospective results remain necessary even if a historical point estimate wins.

## Run

From the repository root, using the existing local audit and foundation caches:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_external_backtest.R
```

The runner is offline and writes a new timestamped experiment directory with raw
input fingerprints, row exclusions, source rechecks, forecasts, metrics, timing
sensitivity and a report. Missing caches stop the run; live API values never replace them.
