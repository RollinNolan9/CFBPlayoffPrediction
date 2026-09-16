# TODOS

## CFB V3

- Complete: isolated Week 3 efficiency-history retention test, 30% versus 50%,
  with roster weights unchanged. On 203 identical 2022-2025 Week 3 games:
  89-113-1 versus 90-112-1 ATS; MAE 14.596 versus 14.617. On 201 games with
  corroborated quotes, ATS worsens from 92-109 to 91-110. No promotion; the
  original-line gain is confined to 2022. Most games have at least one team with
  only one usable FBS efficiency observation. See [results and limits](cfb_v3/experiments/WEEK3_EFFICIENCY_RESULTS.md).
  92 focused test blocks pass, repeated forecasts/inputs match, 11,960 grades
  independently verify, and all 88 protected production files remain unchanged.
  Rerun offline: `Rscript run_cfb_week3_efficiency.R`. Keep v3 unchanged while
  preparing Week 3; rejecting 50% does not establish that 30% is optimal.

- Complete: bounded play cleanup automation, four-way comparison and exact repeat
  verification. Results: v3 50.017%, duplicates 50.052%,
  classifier 49.914%, turnover/havoc ablation 50.017% ATS. No promotion. Strict
  classification loses substantial rate coverage; 798/888 sampled team-games retain
  review flags, not 798 proven incorrect games. Shared classifier, reconciliation,
  exception queue and one-command runner remain isolated from production. See
  [cleanup results](cfb_v3/experiments/PLAY_CLEANUP_RESULTS.md).
  All 201 unique test blocks pass, 11,820 grades independently match, and production
  hashes remain unchanged. Run `Rscript run_cfb_play_cleanup.R` with retained caches.
- Next cleanup decision: do not deploy the strict partial classifier or hand-repair
  hundreds of games solely to keep weak predictors. Consider a bounded, separately
  named full-game box-score turnover feature and omit reconstructed havoc in that
  candidate; validate coverage and point-in-time use before fitting. This alternative
  has not been tested. Keep v3 and published cards unchanged in the meantime.

- Complete: isolated matchup research on 2,955 identical, provider-corroborated
  2022-2025 games. Seven fixed variants, 18 policies, duplicate-only controls and
  499 negative-control shuffles find no validated betting edge. All fixed primary
  policies lose at hypothetical -110. Production and published picks unchanged.
  See [research findings](cfb_v3/experiments/MATCHUP_RESULTS.md).
- Priority: repair the play-data contract before another model version. Verified
  defects include 5,177 identity-rich upstream duplicates, composite turnover
  flags masking ordinary giveaways, and contradictory/missing fumble classifications.
  Flag precedence alone improves box-score agreement but leaves 167/888 sampled
  team-games unreconciled. Its isolated full-v3 replay changes ATS only from
  50.02% to 50.05% on the qualified-quote cohort. This is a correctness project,
  not evidence of a profitable model upgrade. See the
  [bounded cleanup contract](cfb_v3/experiments/TURNOVER_DATA_CONTRACT.md).
  Full-source drive outcomes can corroborate omitted fumbles, but drive counts
  agree with only 709/888 sampled box totals. Reconcile event identity and EPA
  vintage; do not replace the classifier with drive totals or box-score totals.

- Complete: deeper market-edge investigation with twelve predeclared policies,
  direct cover GLM, market-residual ridge, fixed edge/agreement filters and a matched
  OOF-v3 stack. The 55% cover filter is 88-78-2 ATS but loses its small hypothetical
  gain under half-point stress and falls to 63-67-1 on agreeing-source quotes. No
  promotion. 69 targeted test blocks pass; production/published cards unchanged.
  See [findings](cfb_v3/experiments/TRACKER_EDGE_RESULTS.md); rerun offline with
  `Rscript run_cfb_tracker_edge.R` and the retained tracker/foundation caches.
- Partially complete: audit historical betting-line provenance before further edge tuning.
  All 3,006 tested canonical lines inherit cached PBP quotes without verified book
  or observation time; 909 differ from tracker updated quotes by over 0.5. Dated
  examples show conflicts in both sources. A provider/family/side/score-aware ledger
  now qualifies named-book quotes for the separate matchup study, preserving old
  references and exclusions. A fixed 12-game public-source spot check recovered
  11 quotes, nine agreeing exactly. Observation times and spread prices remain
  unavailable; corroborating books is not proof of Friday-executable or true closing
  quotes. Continue same-cutoff prospective captures with actual prices.

- Complete: separate Prediction Tracker benchmark and past-only learned challengers
  on 3,006 matched lined 2022-2025 games. V3/equal Sagarin-FPI/football-plus-external:
  49.66%/51.27%/49.53% ATS; MAE 12.779/12.223/12.295. No promotion or published-card
  changes. 60 targeted test blocks pass; repeat-run forecasts and production hashes
  match. Includes 207 independent Sagarin checks, qualified SP+ overlap, source/line
  audits and saved experimental models. Publisher timing limits remain explicit.
  See [results](cfb_v3/experiments/PREDICTION_TRACKER_RESULTS.md); rerun offline with
  `Rscript run_cfb_prediction_tracker.R` and the retained source/foundation caches.
- Next: prospectively compare the fixed external average with frozen v3 at the
  same cutoff and article quote, retaining actual prices. No learned-model promotion
  or post-hoc ATS filtering from these results. Recover genuinely pregame 2025 CFP
  FPI coverage; the tracker lacks all 11 games. Available-Sagarin CFP comparison
  already covers all 28 games (v3 18-10 ATS versus Sagarin 17-11).

- Complete: offline historical SP+ benchmark on 336 identical lined 2022-2025
  games from six corroborated weekly cycles. V3/SP+/fixed 50-50 blend:
  49.85%/52.04%/52.31% ATS and 12.43/12.13/12.14 margin MAE. No promotion:
  ATS gains depend on 2025; no Week 0-4 or CFP coverage. 41 test blocks passed,
  source/record/timing gates checked and production hashes unchanged. See
  [results](cfb_v3/experiments/EXTERNAL_BACKTEST_RESULTS.md); reproduce with
  `Rscript run_cfb_external_backtest.R` using the existing local audit caches.
- External-history next: validate additional ESPN weekly Playoff Picture numeric
  FPI pages (ranked-team coverage and vintage must be checked), expand independently
  captured Sagarin coverage and corroborated SP+ early-season/CFP cycles. Do not
  use current/final ratings to fill old weeks or tune blends on these results.

- External benchmark: isolated capture/score workflow added for frozen v3 versus
  SP+, Sagarin Predictor, FPI, fixed external consensus and 50/50 v3-plus-consensus.
  See [workflow and locked rules](cfb_v3/experiments/EXTERNAL_RATINGS.md).
  Production picks and weights are unchanged. Capture before kickoff; score finals
  against the identical article quote. Do not backdate current ratings.
- Verified: first prospective Week 2 capture covers all 49 article games with SP+
  and FPI. Sagarin is blocked by HTTPS certificate validation; consensus stays
  blank. Live scoring correctly leaves pending games ungraded. 33 targeted test
  blocks passed; published card and saved model hashes are unchanged. See
  [initial benchmark status](cfb_v3/experiments/EXTERNAL_RATINGS_STATUS.md).
- Next: accumulate prospective external snapshots, resolve live Sagarin capture
  failures, and retain spread prices for actual ROI. Historical tracker matchup
  forecasts and learned coefficients have now been compared separately, with
  archive-timing limits; do not treat them as Friday-executable evidence. Reserve
  new prospective observations for evaluating any deployment change. No automatic
  promotion.

**Status:** Review fixes implemented and verified as version 3.0.0. Candidate only:
overall historical ATS improved slightly, but margin MAE worsened slightly.
See [the v3 guide](cfb_v3/README.md) and [validation report](cfb_v3/VALIDATION.md).

- Complete: rebuilt 2020-2025 foundation, matched v2/v3 backtests, 49-game Week 2
  card/dashboard, saved-model replay, and 68 passing test blocks.
- Complete: isolated FBS-only and market-residual experiments on 3,025 matched
  2022-2025 FBS games, with six additional experiment test blocks passing.
  ATS: v3 49.68%, FBS-only 49.51%, market-residual 50.69%. Neither passes the
  locked screening rules; no promotion and v3 files/cards unchanged. See
  [the experiment results](cfb_v3/experiments/ATS_RESULTS.md) and rerun with
  `Rscript run_cfb_ats_experiments.R` using the existing v3 foundation.
- Complete: pregame reconstruction of the original five-game `model.Rmd`, compared
  on 1,791 identical 2022-2025 FBS games. V1/v3: 50.68%/49.43% ATS,
  13.225/12.677 margin MAE, and 67.95%/71.41% straight-up accuracy. V1's ATS
  advantage is uncertain and does not hold in the primary seed's latest two
  seasons or CFP slice of that modified five-game reconstruction. This is not the
  user's original frozen bracket. See [v1 results](cfb_v3/experiments/V1_RESULTS.md).
- Complete: archived six-game `model (1).Rmd`, fixed-input 2024 CFP bracket replay.
  Replay: 11-0 in all three fixed seeds. V3 frozen: 10-1, missing the final.
  Original presentation page 18 also records the 11-0 bracket. Cached ratings lack
  original release timestamps; the follow-up audit confirms that all 134 cached
  2024 SP+ team rows match post-playoff ratings. The replay is not a valid pregame
  comparison and does not independently verify the saved original bracket.
  No rollback/promotion. See [frozen bracket results](cfb_v3/experiments/MODEL1_RESULTS.md).
  Rerun with `Rscript run_cfb_model1_replay.R`.
- Complete: read-only OG source audit and locked comparison protocol. Inventoried
  3,026 candidate games, recovered public SP+ workbooks, and extracted 6,976 numeric
  team-snapshot rows for review (including 2026 and rejected final ratings).
  See [the source audit](cfb_v3/experiments/OG_DATA_AUDIT.md) and
  [comparison protocol](cfb_v3/experiments/OG_COMPARISON_PROTOCOL.md).
- Complete: user-approved corroborated OG reconstruction, distinct from strict
  timestamp verification. Seven accepted weekly cycles / 938 team-snapshot rows;
  435 x 21 archived feature parity and three fixed 2014-2021 forests verified.
  On 208 matched games: primary OG 95-105-8 ATS / 12.958 MAE; v3 88-112-8 /
  12.893 MAE. Mixed seeds and timing sensitivity: inconclusive, no promotion.
  Six new test blocks plus 15 existing audit/replay blocks pass. See
  [verification results](cfb_v3/experiments/OG_VERIFICATION_RESULTS.md).
  Reproduce with `Rscript run_cfb_og_verification.R` and preserved local caches.
- Complete: locked four-way Elo / internal power / power-plus-EPA / full-v3
  comparison on identical FBS fitting rows and 3,025 test games (3,024 lined).
  Full matched v3 leads MAE (12.760) and SU (72.43%); power-only leads ATS
  (50.61%), but its advantage disappears without 2025. No established ATS edge,
  rollback or promotion. Unchanged v3 reproduces its frozen predictions;
  eight new and six existing experiment test blocks pass. See
  [four-way results](cfb_v3/experiments/FOUR_WAY_RESULTS.md).
  Reproduce with `Rscript run_cfb_four_way.R` using preserved v3 caches.
- Complete: early-season/large-spread source and contribution audit. Frozen
  preseason inputs reconstruct exactly; historical and 49-game live replay
  deltas are zero. No hard margin cap. One locked early-calibration challenger
  leaves ATS unchanged and changes MAE by only -0.002 overall; not promoted.
  88 test blocks pass. See [findings](cfb_v3/experiments/EARLY_SPREAD_FINDINGS.md).
  Reproduce with `Rscript run_cfb_early_spread_audit.R` and
  `Rscript run_cfb_early_calibration.R` using the frozen local references.
- Before Friday: review the saved Week 2 evidence queue. 43 teams played only
  FCS opponents before the cutoff, leaving 36/49 games without current FBS
  efficiency evidence on at least one side (22 still labeled `standard`).
  FCS results inform power at reduced weight; EPA falls back to prior history.
  Add explicit evidence-status flags to future dashboards without changing picks.
- Complete: user-approved zero-FBS-sample preseason retention challenger,
  applied per team in training and prediction with training-only centering.
  On 105 active FCS-opener follow-ups, ATS fell from 54-49-2 to 39-64-2 and
  MAE worsened from 13.406 to 14.490, with declines in every active season.
  Rejected; existing fade, Week 5+/CFP forecasts and live cards unchanged.
  95 test blocks pass. See [retention results](cfb_v3/experiments/EVIDENCE_WEIGHTING_RESULTS.md)
  and reproduce with `Rscript run_cfb_evidence_weighting.R` using frozen caches.
- Keep the evidence-status warnings despite rejecting automatic 100% roster
  retention. Investigate unstable passing-PPA ratios and varying portal coverage
  separately; do not combine unproven changes or select a different deployment
  window from this already-examined experiment's indirect Week 0/1 improvement.
- Next: corroborate additional recovered 2023/2025 cycles and the 2025 bowl preview;
  resolve the Florida 2025 Week 7 source conflict and remaining Elo disagreements.
  Recover late-2022 and late-2024/CFP components separately. No new CFP score is
  established: none of the 28 playoff games passed this run's snapshot gate.
  Do not carry old ratings forward for months, backdate downloads, or score the OG
  with final-season SP+. Independent publication timestamps remain unverified.
- Before using the generic production CFP bracket helper, add explicit predecessor
  links for byes: `predict_cfp_bracket()` does not propagate four first-round winners
  into four quarterfinal games. The isolated replay has a tested explicit graph;
  no production helper was changed during this comparison.
- Verified: all 206 non-CFP bowls in the v3 foundation have zero training weight.
  V1's bowl exclusion is a possible separate experiment, not a change already tested.
- Refresh the market and manual injury inputs before freezing the Friday article.
- Track prospective frozen v3 cards separately from historical replays and v2 cards.
- Keep automated injury-source adapters and archived preseason release dates on the backlog.
- Test kickoff-level cutoffs consistently in historical and live runs; Monday-start
  buckets currently exclude the September 7 SMU-FSU result from the Week 2 inputs.
- Keep large-spread calibration and coach portability sensitivity as separate
  experiments. Do not promote v3 on small-sample ATS improvements alone.

## CFB V2

### Correct Turnover Rate

**Status:** The earlier code change and cached rebuild were completed; additional
source-semantic defects were found in the September 11/12 audit and remain open.
`build_team_game_efficiencies_v2` now builds the rate from interception and
lost-fumble plays only (excluding failed fourth downs, defensive scores, and
special-teams changes of possession that the upstream cfbfastR flags also count).
Foundation run `foundation_2020_2025_20260720T171313Z` rebuilt 1,340,044 PBP rows,
5,115 games, and 10,230 team-game rows with the raw `turnover_lost_rate` numerator.
The correlation between `turnover_rate_regressed` and `havoc_allowed` fell from
0.967 in the stale foundation to 0.125 after the correction.

**Remaining:** Fix the newly verified composite-flag precedence and fumble
classification defects, with explicit box-score reconciliation and regression
fixtures. The earlier rebuild is real but does not establish correct giveaway
counts. The rolling backtest and August 29 dry run were regenerated on August 27;
the new flag-only candidate was tested separately and has NOT replaced them.
See [the data contract](cfb_v3/experiments/TURNOVER_DATA_CONTRACT.md).

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
