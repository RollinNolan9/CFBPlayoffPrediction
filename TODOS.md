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

**Status:** Complete through promotion. The 2021-2025 freeze ran, the rolling report
showed the talent variant beating the core on Week 0/1 margin MAE in all three
honest folds (13.01/15.28/14.08 vs 14.13/15.55/14.66), and its six objective
features were promoted into the production model under the `ps_` prefix with the
standard Week 0-4 fade. Poll features measured at roughly a tenth of the talent
signal (hype_gap +0.22 pts/SD) and stay diagnostic-only. The backtest now tracks a
`preseason_ablation_challenger` (production minus preseason features) and the poll
`preseason_hype_challenger`.

**Remaining:** Rerun `--mode=backtest` to confirm the promoted core, and freeze 2026
priors before the Week 1 article run (production now requires prediction-season
coverage through Week 4 — the Automate 2026 Preseason Refresh item).

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

**Status:** Code and tests landed. `cfb_v2/bridge.R` detects first-year FBS teams from
membership, calibrates a conservative prior and margin uncertainty against the six
historical movers (James Madison 2022 through Delaware/Missouri State 2025) plus the
FCS sides of crossover games, fills only the missing history fields, inflates
`margin_sd`, writes a visible `fbs_transition` flag into predictions and the DuckDB
snapshot, forces low confidence and `transition_review` status when bridge variance
dominates, and adds an `fbs_transition` slice to both backtest reports.

**Remaining:** Rerun `--mode=backtest` locally to review the transition slice, and
keep 2026 membership rows for North Dakota State and Sacramento State in
`membership.csv` so the Week 1 run flags them.

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

**Status:** Code and data landed. `cfb_v2/inbox/coach_history_manual.csv` is a new
sourced contract merged into the coach ledger at read time; manual rows may only add
seasons the public ledger lacks (collisions stop the run) and every row requires an
explicit source. Tim Polasek carries his documented 2024 NDSU FCS title season
(14-2, level-adjusted and portability-capped); Tavita Pritchard and Alonzo Carter
carry documented-neutral zero-game rows so their identities resolve without invented
value.

**Remaining:** Verify and append Polasek's 2025 NDSU season as a second row, and
replace Carter's neutral row if sourced junior-college records are worth modeling.

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
