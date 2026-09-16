# Matchup-specific research protocol

Session started 2026-09-12 01:14:16 UTC; requested research window ends about
03:44:16 UTC. Models, thresholds and source rules below are declared before
scoring the new matchup features. All work remains separate from production v3.

## Evidence and historical quotes

- Reuse the sealed tracker experiment `20260912T003231.716Z` and 2020-2025
  foundation. Newly captured CFBD /lines responses are retained under
  `matchup_sources/20260912T011754.513Z`, including URLs/times/SHA256s.
- CFBD calls these historical closing lines, but individual observation times
  are absent. Named-book metadata improves provenance; it does not prove Friday
  availability or independent point-in-time authenticity.
- Check game ID, season, team orientation and scores. Flip only a documented
  neutral reversal. Verify formatted spread against its numerical side/sign.
  Conflicting duplicate game/provider records are quarantined, never selected
  by which quote improves results.
- Ignore consensus, teamrankings and numberfire as independent bookmaker support.
  Combine Caesars/William Hill variants into one family; normalize Draft Kings.
- A qualifying line has another bookmaker FAMILY within one point. Select by
  fixed priority: DraftKings, FanDuel, Bovada, Caesars Colorado, Caesars, William
  Hill NJ, Caesars PA, ESPN Bet, SugarHouse. Select an actual quote, not a synthetic
  median or best line. No per-game observation timestamp is invented.
- Show all provider records, qualification/exclusion reasons and original PBP
  comparisons. Score book-specific and stricter agreement sensitivities without
  silently changing the primary population or identifying them as actual ROI.
- Source examination found Colorado-CSU 2023's suspect DK -17 within CFBD itself,
  and UNC-South Carolina's sign conflict in consensus/teamrankings records.
  These examples informed source controls before model scoring, not hand edits.

## Pregame matchup features

Competitive plays only, preserving the approved garbage-time/kneel filters.
Both teams must be FBS at the time; exclude non-CFP bowls as source games.
Classification into recorded pass plays (including sacks) versus runs is exclusive.
Scrambles without dropback metadata cannot be called designed runs versus passes.
EPA-defined high-impact events are not mislabeled as 20-yard explosive plays.

Pre-scoring schema audit: compact numeric play IDs collide across distinct plays.
Preserve source row multiplicity, audit ID collisions and identical compact rows,
and never deduplicate solely on those IDs. The compact schema lacks sufficient
clock/down/precise-ID information to prove whether identical rows are duplicates.
This is a source limitation, not evidence of duplicated game joins.

Team measurements use completed CURRENT-SEASON games strictly before the target
week cutoff and kickoff. Week 0/1 matchup features are neutral even if a team had
a Week 0 game. There is no prior-school/talent/roster carryover. League-only priors
may use earlier seasons if the current league sample is empty; never future games.
Shrink efficiency/type rates with 50 relevant plays, overall style/high-impact rates
with 100 plays. Prior counts are fixed, not optimized. Missing evidence stays
neutral with explicit counts. Current-game plays may never affect its own features.

Linear components: pass/run EPA for and allowed, pass fraction, sack rates allowed
and generated, high EPA (>=2) and negative EPA (<=-2) rates for/allowed, source-game
count and source-opponent pregame power. Include current pregame power differential
as a control. These are schedule-strength controls, not a claim that each play
measurement is fully opponent-adjusted.

Four declared groups:

1. Style: pass preference versus opposing relative pass/run weakness, and relative
   offensive pass/run efficiency versus that opposing weakness.
2. Sacks: sack susceptibility versus opposing sack generation, weighted by recorded
   pass exposure. Compare with all linear components to isolate interaction value.
3. High-impact plays: high-EPA creation versus high-EPA concession; negative-EPA
   susceptibility versus opposing disruption. Same linear-component control.
4. Form shifts: last two completed games versus earlier current-season passing,
   rushing and sack performance, only with at least four source games. These are
   form changes, NOT historical quarterback/injury/starting-lineup indicators.

## Models and comparisons

Primary: same eligible lined 2022-2025 games, train on earlier 2020+ seasons.
Use the existing 0.82 season decay and half-weight 2020. Never fit current outcomes.

Seven ridge variants targeting actual margin minus qualified historical market:
external/market baseline; baseline plus linear components; then linear components
plus each of the four groups separately; and linear components plus all four.
External inputs are Sagarin-minus-market, FPI-minus-market and signed market margin.
All fits share training/evaluation rows and use training-only standardization.
Penalty grid 32/128/512; first outer fold uses 128, later choices use earlier outer
validation MAE only. No ATS-based penalty or feature selection.

Compare frozen v3 and the fixed equal external average on exactly the same games.
The historical market itself is a margin reference, never an ATS strategy.
Eighteen fixed policies: seven fits at nonzero edge and >=2 points (14), plus frozen
v3/equal external at nonzero edge and >=3 points (4). Report every policy.
No threshold sweeps, post-result subgroup promotion or additional architecture search.

## Verification and inference

- Perturb future/current-game outcomes and assert earlier/pregame features and
  corresponding held-out forecasts do not change. Assert exact feature schema,
  unique IDs, side-swap antisymmetry, finite rates and chronological source limits.
- Report MAE/RMSE, SU, ATS W-L-P/passes, seasons, phase, spread size, CFP, audience,
  G5 participation, source quality, and confidence intervals. CFP is small and
  missing 2025 tracker FPI; never imply a complete 28-game learned CFP comparison.
- Stress fixed picks at half/one point worse; show unchanged-side grading against
  alternative qualified provider quotes separately. No late-line learner is scored
  at an opening quote with later information. No actual-price or execution claim.
- Paired season-block intervals, leave-one-season-out and family-adjusted policy
  intervals. Permutation/placebo checks preserve grouping where feasible; no null
  test can rehabilitate reused history or unknown publication times.
- All 2022-2025 seasons have informed earlier work. These are new hypotheses on
  reused data, not an untouched confirmation set. Prospectively verified evidence
  is still needed before claiming a betting edge or promoting a live model.
- If a genuine implementation defect is found, document it and test the root-cause
  correction separately. Never modify the production model/card during research.

## Deliverables

Audited provider ledger, causal matchup feature builder, fixed rolling experiment,
all per-game results and diagnostics, falsification/tests, report, reproducible
commands and updated TODOs. Preserve and verify production hashes throughout.

## Root-cause sensitivity, declared after first scores

The upstream full 2021 source confirms exact duplicated rows including game play
number, down, distance, clock and EPA. Numeric-ID collisions alone do NOT prove
duplicates. A separate sensitivity removes only full-row duplicates verified in
identity-rich upstream files that reproduce the normalized cached rows exactly.
Any non-identical upstream season blocks that correction; do not mix source versions.

Rebuild only the new matchup measurements on those cleaned rows. Reuse the exact
primary cohort, external/v3 forecasts, line rules, feature definitions, penalties
and policies. Existing foundation power remains frozen, so this does not estimate
the benefit of rebuilding the whole production foundation. Keep the first results.
Do not treat this follow-up as independent confirmation or claim that removing
duplicates creates an edge. Joint betting-policy intervals, if compared across
both source profiles, account for 36 policy/profile combinations.

Implementation correction: the initial run `20260912T013933.970Z` incorrectly
neutralized CFP matchup features because CFBD's raw postseason week is 1.
The repaired builder uses phase 99 for CFP, consistent with production's existing
phase handling. That first run and its diagnostics are superseded, not evidence
for model selection. Repeat all fixed models, including training and earlier-fold
penalty choices. Add a regression test; never treat CFP week 1 as regular Week 1.

Negative-control implementation: 499 seeded joint permutations of the new `mx_`
feature vectors within season, feature-week cutoff and CFP blocks. Keep outcomes,
market, external ratings and existing power fixed. Repeat the same chronological
fits and prior-fold-only penalty rule. Verify the external baseline is unchanged.
Show every model's placebo distribution, not a newly selected strategy. Since
shuffling breaks conditional relationships with fixed team strength, the resulting
percentiles are descriptive negative controls, NOT calibrated conditional p-values.
Single-game blocks cannot be permuted. Exact season sign-flip diagnostics separately
state their symmetry/independence assumption and the four-season resolution limit.

Further data-defect isolation, declared before scoring: reconstruct the original
football team-game statistics and pregame snapshots from unchanged cached plays,
then from the full-source-verified duplicate removal. Require unchanged feature
and forecast parity first. Keep the original tracker-matched training population,
weights, per-fold coach splits, feature schema, ridge penalties and phase blend
fixed. Score both football refits on the same 2,955 qualified-quote evaluation
games at nonzero edge and the existing >=3-point rule. Do not rerun feature or
penalty selection. This is a data-quality sensitivity, not a replacement for v3
or an additional tuned strategy. Preserve power ratings derived from game scores,
coaches and manual/preseason inputs. If exact reconstruction fails, report the
discrepancy and withhold claims that any forecast change is duplicate-only.

2026-09-12 02:27 UTC field-contract discovery: finite zeroes in upstream
`turnover_indicator` mask ordinary interceptions/lost fumbles whose `turnover` is
one. The original aggregation prefers the former, so its earlier giveaway-only
filter still undercounts. Declare one further root-cause control before scoring:
prefer a finite `turnover` value, falling back to `turnover_indicator` only when
`turnover` is unavailable. Keep the existing scrimmage/play-type giveaway gate;
do not reinterpret return-only rows or change the garbage-time filter. Because
the shared resolved flag also feeds havoc, rebuild both those rates and the
past-only league turnover prior. Use original (not deduplicated) plays to isolate
this defect. Same fixed football training population, folds, features, penalties,
coach choices and phase blends as the duplicate-only audit. Score all games and
the existing >=3-point policy; no new tuning or automatic production changes.

Extend the same turnover-only correction to the full frozen v3 training population
as a deployment-impact check. Reproduce frozen v3 first, retain its original
past-selected coach/penalty choices, and then change only reconstructed rate
features. Report the qualified-quote cohort and broader original PBP-quote cohort
separately, including all available CFP games in the latter. No point-in-time
upgrade is implied by the broader sample. Classification omissions discovered
afterward (notably unflagged 2025 fumble plays) remain a separate unresolved audit
item, not an unannounced additional correction in this test.
# Synthetic Sensitivity Check

Added after the real matchup results as a test of the research method, not a new
betting strategy. Fixed seeds 947501-947516; injected effect sizes 0, 1, 2 and 4
points per warm-up-standard-deviation of `mx_style_preference`. Center/scale come
only from 2020-2021. For each seed, permute the observed market residuals within
season/week/CFP blocks, then add the fixed synthetic effect. Reuse the same draw
at every effect size and run the unchanged chronological fitting/penalty procedure.
Report paired margin-error changes for style versus linear/external controls and
their Monte Carlo ranges. These are simulated outcomes, NOT historical ATS results,
not a formal power calculation, and not evidence that such a real effect exists.
