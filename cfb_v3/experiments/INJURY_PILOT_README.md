# Reproduce the injury feasibility pilot

From the repository root, with Python, pandas and numpy available:

```powershell
python -B cfb_v3/experiments/run_injury_pilot.py
```

This command uses the locally retained evidence, reconstructs the three sampled
player cases, evaluates the fixed multipliers, and creates a timestamped HTML and
Markdown report under `cfb_v3/output/experiments/injury_pilot/evaluations/`.
It makes no network requests and does not change the live model or injury inbox.
The output directory is ignored by Git: a fresh clone needs the retained evidence
bundle, not just these scripts. Missing files cause a clear error rather than a
silent fetch or a zero injury adjustment.

The experiment is defined in [INJURY_PILOT_PROTOCOL.md](INJURY_PILOT_PROTOCOL.md).
Friday noon uses `America/Los_Angeles` (PDT/PST), not a fixed UTC offset. These are
selected examples with incomplete report and opposing-player coverage, not a
complete SEC backtest. Historical ATS uses PBP reference lines whose Friday
availability is unverified.

## Components and evidence

- `injury_baseline.R`: audits saved chronological predictions, verifies phase
  blending, joins game IDs and derives the outcome-free Saturday schedule.
- `injury_reports.py --offline`: regenerates normalized reports from retained
  copies. Without `--offline`, retrieves the configured public report sources.
- `injury_player_history.R`: reconstructs strictly prior player involvements.
  Source it in R and call `injury_prior_player_history()` with a season, team,
  prediction cutoff and `mode = "raw_involvement_ppa"`. First inspect its request
  plan (`execute = FALSE`); execution requires an explicit request budget.
  Cached rebuilds can use `max_new_requests = 0`. CFBD credentials remain in
  the existing ignored environment configuration and must never be logged.
- `injury_case_features.py`: uses only the outcome-free schedule, dated reports
  and prior player histories. Exclusions and source hashes are retained.
- `injury_evaluate.py`: joins outcomes after feature construction; selects a
  multiplier on 2024 only and reports every fixed multiplier for 2025.
- `injury_report.py`: writes a static local report from a completed evaluation.

The raw involvement measure intentionally differs from CFBD's stored player PPA
aggregates on some plays. Strict provider-aggregate validation still rejects
those mismatches. This pilot cannot establish that the separate 2.8-point Lacy
scenario is accurate. Negative estimates are preserved, and questionable or
doubtful backups are retained under the declared Out-only rule.

Source coverage, excluded late reports and archived HTML hashes are documented in
the retained `source_data/SOURCE_COVERAGE.md`. Each evaluation copies its case
inputs and coverage ledger and fingerprints its inputs. Source-position mistakes,
unrecovered intervening updates, and missing player histories remain explicit.

## Focused checks

```powershell
python -B -m unittest discover -s cfb_v3/tests -p 'test_injury_*.py'
Rscript --vanilla cfb_v3/experiments/test_injury_player_history.R
```

The checks cover cutoff leakage, prior-game filtering, report conflicts and later
updates, prediction signs, zero-adjustment controls, holdout isolation, missing
PPA, role deduplication, cache identity and request budgets. No experiment result
automatically promotes an injury model into production.
