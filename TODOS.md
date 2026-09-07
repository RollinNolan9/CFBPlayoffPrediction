# TODOS

## CFB V2

### Correct Turnover Rate

**Status:** Code, tests, and the cached foundation rebuild are complete.
`build_team_game_efficiencies_v2` now builds the rate from interception and
lost-fumble plays only (excluding failed fourth downs, defensive scores, and
special-teams changes of possession that the upstream cfbfastR flags also count).
Foundation run `foundation_2020_2025_20260720T171313Z` rebuilt 1,340,044 PBP rows,
5,115 games, and 10,230 team-game rows with the raw `turnover_lost_rate` numerator.
The correlation between `turnover_rate_regressed` and `havoc_allowed` fell from
0.967 in the stale foundation to 0.125 after the correction.

**Remaining:** None. The rolling backtest and August 29 production dry run were
regenerated on August 27 after the corrected foundation rebuild.

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

**Status:** Complete through guarded production use. The 2021-2026 freeze contains
802 team-seasons, including all 138 teams in the 2026 247 composite and 136 returning-
production rows (North Dakota State and Sacramento State are expected first-year-FBS
misses). The freeze now also measures portal depth and source-quality-adjusted prior-year
production from incoming skill-position transfers. Production uses the full roster-aware
model through Week 4 while those inputs fade once by 100/60/30/10%, then switches exactly
to foundation in Week 5 and throughout the postseason. A September 7 review removed the
old prediction-level cap because it was shrinking already-faded evidence a second time.
This correction is model version `2.1.0`.
On 384 held-out Week 0/1 games, the corrected model improved foundation MAE from 15.30
to 14.77 with 83.9% winner accuracy. On 328 held-out Week 2 games, it improved MAE from
15.03 to 14.61 and forced ATS from 48.0% to 50.2%. The replayed 2026 Week 1 snapshot was
25-12-1 ATS, 34-4 straight up, and 12.47 MAE through 38 completed games. Historical ATS
still does not clear the 52.38% -110 break-even rate, so the ATS layer does not promote
official plays without validation. Poll features remain diagnostic-only.

**Remaining:** Feed completed Week 0/1 games into the Week 2 snapshot, then monitor the
foundation, returning-only, and poll-hype profiles as diagnostics.

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
`retained_quality`, `replacement_capacity`, `portal_replacement_capacity`,
`portal_offense_replacement`, and `hype_gap`.

**Effort:** L
**Priority:** P0
**Depends on:** Correct Turnover Rate

### Accelerated History Challenger

**Status:** Rejected on September 7, 2026. Production remains model `2.1.0`.

The challenger moved the existing 10% prior-team-history cap from Week 8 to Week 5.
It rebuilt opponent-adjusted power plus explicit prior-season and trailing-three-year
features from the cached games before running the same rolling folds. Production
reconstruction matched the cached features within `6.93e-14`.

Across 654 held-out regular-season games in Weeks 5-7, MAE worsened from 12.292 to
12.334, with a paired-bootstrap 95% delta interval of `[-0.146, +0.231]`. Overall MAE
worsened from 13.158 to 13.166, recent-season MAE worsened from 12.945 to 12.956, and
only two of four season folds improved. The challenger improved every selected 2024
Florida State and 2025 Penn State collapse diagnostic, but that anecdotal benefit did
not generalize. Forced Weeks 5-7 ATS rose from 52.0% to 53.1%; because margin is the
lead metric and the fold results were unstable, that is insufficient for promotion.
The ATS change was seven net wins across 633 paired games (McNemar `p = 0.529`), also
providing no evidence of a repeatable betting improvement.

Reproduce with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' `
  .\cfb_v2\backtest_accelerated_history.R
```

The detailed report is generated at
`cfb_v2/output/experiments/accelerated_history_cap_week5/report.md`.

### Build FCS-to-FBS Bridge

**Status:** Code and tests landed. `cfb_v2/bridge.R` detects first-year FBS teams from
membership, calibrates feature priors and margin uncertainty against the six
historical movers (James Madison 2022 through Delaware/Missouri State 2025) plus the
FCS sides of crossover games, repairs missing or zero-placeholder transition inputs,
and applies the same fold-safe behavior in rolling backtests. Transition uncertainty
fades with games played, predictions retain a visible `fbs_transition` flag, and
bridge-dominated projections fall to low confidence and `transition_review` status.

**Remaining:** Keep 2026 membership rows for North Dakota State and Sacramento State
in `membership.csv` so the reusable weekly run flags them. The rebuilt backtest and
August 29 dry run both report the transition slice.

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

**Status:** Complete. The 2026 returning production, 247 team talent, transfer portal,
prior-year player PPA, and preseason poll sources are cached and frozen into
`preseason_team_priors.csv` without changing
the 2021-2025 snapshots. Current 247 coverage is 138/138; returning-production coverage
is 136/138, with only the two first-year FBS teams missing. The August 29 command
validates coverage, runs the roster-aware capped production blend, snapshots market lines, and writes
component margins, diagnostics, and the feature-contribution chart.

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
- Separate ATS calibration, requested article-pick behavior, injury scenarios, and immutable
  article/live runs.
- August 29, 2026 dry run, local feature-contribution diagnostics, San Jose State alias
  fix, and monotonic ATS guard.
- Full roster-aware production model through Week 4 with one feature-level phase fade,
  exact Week 5/postseason foundation switch, persisted component margins, and rolling tests.
- All-team transfer-production audit plus an ATS profitability gate: unvalidated cards
  pass by default, while explicitly requested games receive visible article sides at low confidence.
- Self-contained static Quarto dashboard with article, market-diagnostic,
  availability, search, sort, and filter views generated by the weekly command.
- Full implementation history documented in
  [`cfb_v2/IMPLEMENTATION_GUIDE.md`](cfb_v2/IMPLEMENTATION_GUIDE.md).
