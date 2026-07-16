# TODOS

## CFB V2

### Correct Turnover Rate

**Status:** Code and tests landed. `build_team_game_efficiencies_v2` now builds the
rate from interception and lost-fumble plays only (excluding failed fourth downs,
defensive scores, and special-teams changes of possession that the upstream
cfbfastR flags also count) and persists the raw `turnover_lost_rate` numerator for
audit. The committed foundation CSVs still hold the old havoc-derived values until
the local rebuild below is run.

**Remaining:** Rebuild the historical foundation
(`--mode=build-foundation --seasons=2020:2025 --overwrite=true`), rerun
`--mode=backtest`, and regenerate the August 29 dry run on a machine with the CFBD
key and PBP cache.

**What:** Build `turnover_rate_regressed` from turnover-only scrimmage plays instead
of `havoc_allowed`, then rebuild the historical foundation and rerun every rolling
fold.

**Why:** The current field is a shrunken copy of havoc allowed. That duplicates a
feature, overstates its apparent importance, and especially distorts teams with
missing prior FBS history.

**Acceptance:** The raw turnover numerator uses only turnovers, the rate shrinks to a
league turnover baseline, the contribution reconciles to model margin, and the full
backtest plus August 29 dry run are regenerated.

**Effort:** M
**Priority:** P0
**Depends on:** Nothing

### Backfill Preseason Challengers

**Status:** Code and tests landed. `cfb_v2/preseason.R` freezes one timestamped row
per FBS team-season (`--mode=build-preseason`), and the backtest evaluates the core
against returning-production, +talent, and +poll-hype challengers whose
`challenger_ps_*` features fade with the standard preseason weights. Week 0/1
diagnostics write to `cfb_v2/output/backtest/preseason_challenger_report.md`,
including learned per-feature effects. Poll features are diagnostic only — they
measure how much preseason sentiment misleads (the Clemson/LSU pattern) and are
not promotion candidates; only the returning-production and talent variants can
be promoted.

**Remaining:** Run `--mode=build-preseason --seasons=2021:2025 --overwrite=true` with
the CFBD key, rerun `--mode=backtest`, and review the Week 0/1 report before any
challenger is promoted.

**What:** Pull and freeze 2021-2025 CFBD returning production, Week 1 AP/Coaches poll
points, and 247 team-talent composites. Build four rolling challengers: core,
returning production, returning production plus talent, and returning production plus
talent plus poll hype gap.

**Why:** Week 0/1 has no current-season evidence. Historical misses such as 2025
Clemson and LSU should teach the model how much preseason information deserves trust
instead of receiving a hand-set positive adjustment.

**Acceptance:** Exactly one timestamped row per team-season, no team or conference
identity predictors, no post-kickoff data, season-normalized numeric values rather
than raw ranks, and a rolling report covering Week 0/1 margin MAE, straight-up
accuracy, ATS accuracy, calibration, and conference diagnostic slices.

**Feature candidates:** `returning_ppa_pct`, `returning_passing_ppa_pct`,
`returning_usage_pct`, `talent_percentile`, `preseason_poll_vote_share`,
`retained_quality`, `replacement_capacity`, and `hype_gap`.

**Effort:** L
**Priority:** P0
**Depends on:** Correct Turnover Rate

### Build FCS-to-FBS Bridge

**What:** Create a conservative prior and added uncertainty for first-year FBS teams,
starting with North Dakota State and Sacramento State in 2026.

**Why:** Treating missing FBS history as league-average inputs produced structurally
invalid August 29 projections and extreme cover probabilities.

**Acceptance:** Calibrate against historical FCS-to-FBS movers and FBS/FCS crossover
games, preserve a visible transition flag, prohibit high-confidence picks when bridge
uncertainty dominates, and backtest promoted teams separately.

**Effort:** L
**Priority:** P1
**Depends on:** Correct Turnover Rate

### Complete New-Coach Coverage

**What:** Add prior head-coaching history for Tim Polasek, Alonzo Carter, and Tavita
Pritchard where supported, using the existing lower-level portability adjustment and
explicit source lineage.

**Why:** Their neutral coach values hide real information and weaken the mandatory
coach differential in three August 29 games.

**Acceptance:** Canonical coach identities map as of kickoff, lower-level success is
context-adjusted and capped, interim boundaries remain week-effective, and every
assignment passes the duplicate/overlap audit.

**Effort:** M
**Priority:** P1
**Depends on:** Existing coach identity and transition ledgers

### Automate 2026 Preseason Refresh

**What:** Add one command that checks CFBD for 2026 returning production, team talent,
and preseason polls; freezes available rows; validates team coverage; reruns the
selected preseason challenger; and produces the Week 1 report.

**Why:** The historical model can be built now. When 2026 data appears, the workflow
should require an append and rerun rather than new model development.

**Acceptance:** Missing sources fail visibly or enter a documented fallback, snapshots
retain source and capture time, manual overrides never silently replace public data,
and the command is safe to rerun without changing an existing article snapshot.

**Effort:** M
**Priority:** P1
**Depends on:** Backfill Preseason Challengers

### Automate Official Injury Sources

**What:** Add versioned conference, CFP, and school injury-source adapters after the
Snapshot Ledger foundation is stable.

**Why:** Reduce Friday manual maintenance and missed late injury updates while
preserving sourced in/out scenarios.

**Context:** Phase 1 intentionally ingests versioned manual injury artifacts so
fragmented HTML, PDF, and OCR parsers cannot destabilize source lineage, health gates,
publication, or exact replay. Begin with official conference and CFP hubs, retain the
manual review queue for unsupported schools, and never treat an unreadable report as
healthy coverage.

**Effort:** L
**Priority:** P2
**Depends on:** Snapshot Ledger capture, lineage, health gates, manual injury contract,
and exact replay

## Completed

- Parallel v2 foundation, DuckDB snapshot ledger, and one-command historical build.
- Post-COVID rolling model with recency weights, non-CFP bowl exclusion, and no raw
  team-name predictors.
- Canonical coach identities, dated assignments, transition checks, and 65/35 versus
  70/30 rating comparison.
- Separate ATS calibration, forced-pick behavior, injury scenarios, and immutable
  article/live runs.
- August 29, 2026 dry run, local feature-contribution diagnostics, San Jose State alias
  fix, and monotonic ATS guard.
- Full implementation history documented in
  [`cfb_v2/IMPLEMENTATION_GUIDE.md`](cfb_v2/IMPLEMENTATION_GUIDE.md).
