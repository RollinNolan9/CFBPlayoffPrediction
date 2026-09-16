# Running The Bounded Play Cleanup

This workflow creates research outputs. It does not update production v3, article
cards, cached source files, individual EPA values or the production ledger.

## Run

From the repository root, with the existing R dependencies and retained caches:

```powershell
Rscript run_cfb_play_cleanup.R
```

The command runs focused tests, validates full-source/compact-cache identity,
classifies 2020-2025 plays, reconstructs three team-game datasets, reconciles the
fixed 888-team-game box sample, and compares four predeclared model profiles.
It prints the prepared-data directory and final `REPORT.md` location.

To inspect data quality without fitting models:

```powershell
Rscript run_cfb_play_cleanup.R --audit-only
```

To compare a retained prepared run without repeating classification:

```powershell
Rscript run_cfb_play_cleanup.R --compare "PATH_FROM_THE_CLEANUP_AUDIT_OUTPUT"
```

The comparison requires the same production and cleanup-code hashes used for
preparation. After any code change, run the full command again. Failed or unsealed
runs are not valid inputs. The workflow is offline; it never replaces a missing
cache with a new API response. Ignored raw and generated files are not included
by a Git clone alone.

## Read The Audit

- `team_game_reconciliation.csv`: reported totals, confirmed-event lower bounds,
  unresolved observations, model exclusions and reasons a game still needs review.
- `exception_queue.csv`: event or team-level exceptions, source identity, event
  text, surrounding plays, classification evidence and box-count context.
- `classifications_YEAR.rds`: retained source identity and the classifier's five
  output fields, plus original full-file row indices.
- `duplicate_removal_ledger.csv`: proven complete-row duplicates, not repeated
  numeric play IDs. Original raw files are unchanged.
- `rate_coverage.csv`: offense/defense cells whose uncertainty prevents assigning
  a reliable rate. Missing is not zero.
- `source_inventory.csv`, `code_inventory.csv`, `checksums.csv`: provenance and
  integrity checks for this exact run.

A matching count does not prove the individual events are correct. Review the
blockers before calling a team-game reconciled. Some official turnovers occur
outside the model's competitive scrimmage finite-EPA subset; those exclusions are
intentional and must not be repaired by inventing a play or changing its EPA.

## Resolve An Exception

1. Check the game, team, drive and play sequence, not just the numeric play ID.
2. Compare event description, reliable structured recovery fields and drive context.
3. For contradictions, check an official school gamebook. CFBD and ESPN can share
   source information, so agreement between their feeds is not independent proof.
4. Record a reproducible source-backed regression fixture before changing a rule.
   Fix the shared classifier once, not one historical and one weekly regex.
5. Keep unsupported cases unresolved. Do not use box totals to distribute invented
   events across plays. A source correction may also require a separate EPA audit.
6. Rerun preparation and comparison. Published picks remain immutable regardless
   of whether the candidate's retrospective record improves.

## Interpret The Comparison

The profiles are frozen v3, duplicates removed, duplicates plus classification,
and duplicates with turnover/havoc predictors removed. Coach splits, penalties,
training population, phase weights and evaluation quotes are fixed. Classification
changes only turnover/havoc rate cells, not the original model denominator or EPA.

Unknown rate cells flow through existing past-only aggregation and training-only
imputation. This can reduce usable history or trigger a prior-season fallback;
coverage and feature missingness are part of the result, not details to suppress.

These reused historical seasons are not fresh validation, and historical quote
corroboration does not establish executable publication-time prices. A correctness
improvement is not automatically a betting improvement. Deployment is a separate
decision after reviewing the evidence and remaining data limitations.
