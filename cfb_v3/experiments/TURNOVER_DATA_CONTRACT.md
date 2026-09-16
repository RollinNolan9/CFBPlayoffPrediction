# Bounded Play-Data Cleanup

Status: proposed correctness migration, not a promoted model. Keep v3 and all
published snapshots frozen until an explicitly approved, versioned rebuild.

September 12 follow-up: the isolated classifier, reconciliation queue and fixed
comparison are implemented. The strict partial repair loses substantial rate
coverage and does not improve aggregate ATS or MAE. Production remains unchanged.
See [bounded cleanup results](PLAY_CLEANUP_RESULTS.md) and
[one-command guide](PLAY_CLEANUP_GUIDE.md). This contract is not yet satisfied as
a complete source-data migration; remaining review flags are not proven errors.

## Verified Defects

1. The shared feature builder prefers finite `turnover_indicator` over `turnover`.
   In the retained classic cfbfastR files, the composite indicator is often zero
   for ordinary interceptions and lost fumbles. That suppresses giveaways and
   changes the custom havoc numerator. An existing play-type gate cannot restore
   events after the flag has suppressed them.
2. The field named `turnover` is not official ground truth either. It also marks
   missed field goals/other possession changes, and some source events are wrong.
   Michigan-East Carolina 2023 (`401520162`) labels McCarthy recovering his own
   fumble as `Fumble Recovery (Opponent)`, with `turnover=1`. Michigan's reported
   box score has zero turnovers.
3. LSU-Clemson 2025 (`401752671`) has a Chris Hilton Jr. lost fumble labeled
   `Pass Reception`. The existing giveaway-type gate excludes it even with the
   flag-precedence correction. The description reports a Clemson recovery.
4. In 2025, 181 eligible finite-EPA opponent-fumble-recovery rows have neither a
   pass nor rush flag and are excluded by the scrimmage gate. Some descriptions
   explicitly describe a rush or completed pass. Do not treat every recovery row
   as a scrimmage play: returns and multi-possession events require separate review.
5. Identity-rich upstream files contain 5,177 exact duplicate rows across 2020-2025.
   Numeric play IDs also collide across distinct plays. Compact-row equality or
   ID equality alone is NOT sufficient evidence for removal.
6. Duke's official 2022 Miami gamebook records three sack-fumbles that the cached
   source labels as ordinary sacks, with both turnover flags zero. This is missing
   source information, not just precedence. See
   [matched witnesses](matchup_gamebook_witnesses.csv) and
   [the official play-by-play](https://goduke.com/sports/football/stats/2022/miami/boxscore/20473).

## Minimal Implementation Boundary

- Create one shared, tested event-classification helper used by historical and
  weekly aggregation. Preserve the unmodified source fields and an exclusion or
  review reason. Do not distribute competing regex rules across multiple builders.
- Resolve giveaways from verified event semantics: interception or lost fumble,
  team responsible, and whether the event actually stood. Do not count a failed
  fourth down, missed kick, own recovery, or a reversed/no-play event as a giveaway.
- Use structured recovering-team/type information where reliable. When fields and
  text disagree, retain a review flag; do not silently infer recovery from a player
  surname, blindly trust one boolean, or substitute season-ending totals.
- Keep an all-play event reconciliation distinct from the model's competitive,
  scrimmage, finite-EPA subset. Record why an official event is outside the model
  subset. A rate over a filtered subset need not equal the full box-score total.
- Missing source plays are missing data, never zero turnovers. Do not feed an
  unresolved classification's assumed zero into a team-strength claim unnoticed.
- Remove duplicates only after an identity-rich source file exactly reproduces
  the normalized cache and the complete original row is duplicated. Retain the
  source digest and removal ledger. Never deduplicate numeric IDs or compact rows.
- Keep source family and EPA model vintage fixed. Upgrading cfbfastR or switching
  to its newer ESPN-derived dataset is a separate migration, not a hidden repair.
- Audit EPA on independently verified misclassified events before assuming a
  numerator correction fully repairs the foundation. Its downstream error impact
  has not been measured; do not overwrite EPA using an assumed turnover penalty.

## Acceptance Checks

1. Fixtures cover ordinary interceptions, lost and own-recovered fumbles, generic
   pass/rush labels, missing pass/rush flags, failed fourth downs, specials, sacks,
   kneels, replay reversals, duplicate rows, distinct colliding IDs, and missing PBP.
   Preserve the Michigan and LSU witnesses as explicit current-limit regressions
   until a replacement classifier correctly resolves them.
2. Reconcile the fixed 888-team-game audit sample before and after the change.
   Explain all remaining discrepancies; do not claim perfect reconciliation from
   a total-sum comparison where overcounts and undercounts can cancel.
3. Expand reconciliation to other weeks only after this contract passes. The
   current sample covers regular Weeks 1 and 8 and postseason Week 1 in 2022-2025,
   filtered to eligible FBS games. CFBD/ESPN may share an underlying provider.
   Relaxing the scrimmage/EPA gates in this sample exposes only 14 additional
   giveaway-type events and raises exact count agreement to 727/888, still leaving
   161 discrepancies. Those relaxed counts are audit-only, not model features.
4. Rebuild source rates, prior-only smoothing and downstream preseason/current
   features in an isolated candidate. Prove original-input replay parity first.
   Deduplication and turnover correction remain separately attributable controls.
5. Freeze features, coach shares, training population, penalties, season decay,
   phase blending and quoted lines for the impact comparison. Report common-game
   MAE, SU, ATS, playoffs, large spreads and season-by-season changes, including
   all failures. Reusing this history is not fresh validation.
6. After explicit deployment approval, version the data and model artifacts and
   rebuild once. Never overwrite an article snapshot or silently revise its picks.

## Evidence So Far

The issue is also present in the retained 2026 source bucket: 5 versus 75 included
giveaway-type events across 5,530 eligible competitive scrimmage plays under the
old versus flag-only rule. This uses source games kicked off before the existing
September 7 Monday bucket, not a reconstructed information-arrival timestamp.
It is a field audit, not a new live projection or verification of 75 official losses.

## Drive Evidence Follow-Up

The full-source raw drive outcome retains the three Miami sack-fumble losses that
the compact play flags and descriptions omit. Drive totals still match only
709/888 sampled box totals (155 below, 24 above), so this is corroborating evidence,
not an approved classifier. Use `pos_team` for ownership, quarantine contradictory
drive metadata, and do not infer that missing/ambiguous outcomes mean no turnover.
No drive-based relabeling or EPA repair has been deployed or evaluated as a model.

The structured CFBD `/plays/stats` source was checked on four known contradictory
games. No fumble-recovery associations were returned, and the LSU-Clemson 2025
response omitted both known LSU fumble events. Every response was below the
documented 2,000-record limit. Missing associations therefore cannot be treated as
zero events or used to resolve the recovery team automatically. See the
[current primary API documentation](https://github.com/CFBD/cfbd-python/blob/main/docs/PlaysApi.md).

The flag-only candidate raises exact box-score agreement from 264/888 to 721/888;
133 counts remain below reported totals and 34 exceed them. Missing PBP: zero in
this sample. The full-v3 controlled replay moves from 50.017% to 50.052% ATS and
12.761 to 12.753 margin MAE on 2,955 qualified-quote games. This does not establish
a betting advantage, nor does it mean the untested classification repair will.

See [research results](MATCHUP_RESULTS.md), the copied protocols and sealed audit
inventories. Production remains unchanged.
