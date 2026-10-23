# SEC running-back availability pilot

Protocol written September 24, 2026, before evaluating injury-adjusted historical
outcomes. This is a local analysis plan, not external preregistration. Historical
2024/2025 baseline results have already been examined in other project research.

## Question and scope

Does a prior-game usage/PPA replacement adjustment improve the frozen v3 margin
forecast when an established running back is newly reported out? The primary
information cutoff is **Friday noon America/Los_Angeles**, selected by the user.
An injury first confirmed on game day is not information available to that article.

Target population: Saturday regular-season SEC-vs-SEC games in 2024 and 2025.
Non-Saturday games, conference championships and postseason games are excluded
from this first pilot. Report coverage against the complete target population;
absence of a report is missing evidence, not a healthy roster. Older SEC archive
access and report timestamps must be verified rather than assumed from policy.

## Frozen controls

- Reuse saved chronological v3 predictions after auditing their model identity,
  features and game IDs. No 2026 outcomes, refitting on test-season outcomes,
  live-model changes, production injury-inbox edits or publication changes.
- Save source URLs, original report/publication times, retrieval times and hashes.
  Reports without a defensible original timestamp cannot enter the primary test.
  A date-only source may instead establish a conservative publication-time upper
  bound, explicitly marked as a bound rather than an exact report timestamp. It
  qualifies only if that entire possible interval precedes the cutoff. A Friday
  date alone cannot establish publication before Friday noon Pacific. Dated
  complete media transcriptions of official reports must be labeled reported
  copies and distinguished from original conference/team documents. Later-edited
  pages without an identifiable historical report version are not sufficient.
- Use the latest report known by Friday noon Pacific. Later active reports clear
  older out reports; conflicting unresolved evidence is excluded and counted.
- Keep game-day-only evidence separate as a descriptive secondary analysis. It
  cannot be reported as a Friday performance result.
- Reconstruct usage, efficiency and replacement workloads from completed games
  strictly before the target game's prediction cutoff. Full-season legacy injury
  totals and season-end player usage/PPA are forbidden predictors.

## Eligibility and adjustment

Primary events are running backs explicitly **Out**, with at least two completed
prior team games, at least eight relevant offensive involvements per prior team
game, and at least one involvement in the most recent completed team game. This
new-absence restriction reduces double-counting of persistent absences already
embedded in recent team results. Questionable/doubtful/probable are not silently
treated as out. This narrow test does not cover all injury effects.

For each qualifying player, estimate expected involvements from prior team games
and average PPA from the same eligible involvement cohort. Replacement backs must
have prior opportunities and must not be reported out at the cutoff. Weight their
PPA by reconstructed prior opportunities, holding the workload and efficiency
assumptions fixed. Loss = expected player involvements per team game multiplied
by (player PPA minus replacement PPA). Preserve the signed result; do not clip
unfavorable estimates after seeing outcomes. If no usable replacement sample or
player identity exists, exclude and report why rather than impute a zero loss.

Player-data adapter validation must establish which plays count and how player
identities join. If APIs cannot reconstruct compatible opportunity counts, record
the limitation before evaluating results; do not substitute receptions for
targets or a different EPA provider without disclosing a changed experiment.
The backtest is a weekly reconstruction of the Lacy scenario's method family,
not proof of a precise player-specific causal effect.

Source-adapter audit and amendment, completed before injury-adjusted outcome
evaluation: three initial samples reproduced all 13 player-game PPA averages
using Rush + Reception associations, but broader validation found Target
associations on incompletions. A receiving touchdown was associated with its RB
recipient as Completion rather than Reception. Include Rush, Reception, Target
and the receiving-touchdown Completion association, deduplicated by
game/play/athlete. Box-score counts failed because some touches have missing
PPA; never multiply box-score touches by rounded average PPA.

Even after adding targets, some official player-game aggregates differ from the
raw play values, particularly on failed fourth downs and a reception fumble.
Independent audits found no bad play/team/player joins. The API reads stored
player aggregates separately from raw play PPA. Exact replication therefore
remains unverified and is not a claim of this pilot.

For this first pilot, use the explicitly named **raw involvement PPA** measure:
unique Rush/Reception/Target/receiving-TD Completion associations with finite raw CFBD play PPA, including
failed fourth downs and fumbles. Counts, totals and means all use that identical
cohort for absent players and backups. Preserve API-aggregate comparison errors
as diagnostic fields. Keep strict aggregate-replication mode available, with its
failures intact. This amendment responds to source behavior before observing
injury-adjusted outcomes; it does not validate the precise 2.8-point Lacy estimate.

Report-copy disagreements outside running back are retained as source flags.
For this RB-only pilot, a complete RB status list corroborated across copies may
qualify even when a tight-end/defensive-player discrepancy prevents declaring the
whole team report reconciled. Any unresolved RB-status conflict is excluded.

Compare fixed multipliers 0 (baseline), 0.5 and 1.0. Select the lowest affected-game
MAE on 2024 only, breaking ties in favor of the smaller multiplier, then apply
that choice unchanged to 2025. Report every fixed multiplier in both seasons;
do not change the eligibility threshold or formulas in response to test results.

## Evaluation and limits

- Lead with source coverage, exclusions and the number of affected games/players.
- Compare paired margin MAE, straight-up accuracy, ATS W-L-P and ATS pick changes
  on identical games and identical market lines. Show affected-game and complete
  covered-slate results; unchanged games must not inflate the injury sample size.
- The existing historical market fields originate in PBP and are not verified
  Friday bookmaker snapshots. ATS against those fields is a reference-line
  diagnostic, not a reconstructed executable Friday betting record or profit.
- Report paired error changes per game, and descriptive uncertainty when sample
  size supports it. With few eligible absences, call the finding inconclusive;
  do not promote a model based on one favorable aggregate.
- PPA includes teammate, opponent, role and game-context effects. Small backup
  samples and available-report selection bias limit causal interpretation.
- No automatic production promotion. This pilot may end with an auditable
  coverage/data limitation rather than a defensible performance estimate.

## Output

Reproducible collectors and evaluation code live in `cfb_v3/experiments/`.
Raw evidence, normalized player histories, inclusion/exclusion ledgers, source
hashes and timestamped reports live under
`cfb_v3/output/experiments/injury_pilot/`.
