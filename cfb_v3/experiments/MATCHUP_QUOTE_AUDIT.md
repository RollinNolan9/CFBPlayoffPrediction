# Independent public quote spot checks

Checked September 12, 2026 UTC. These observations do not alter any model,
training line, policy, or qualification rule.

## Selection

Use corrected primary inputs `matchup_prepared/20260912T015157.387Z`.
Keep 2022-2025; split by season and absolute qualified spread (<=7, >7 to 21,
>21). In R, set seed 947222 and sample one row from each of the 12 sorted
`split()` groups. Sampling does not read outcomes or model performance.

The factual observations and direct source links are in
`matchup_public_quote_checks.csv`. All 12 selected games remain in that ledger,
including the unresolved James Madison example.

## What this establishes

Eleven games have a recovered public quote observation. Nine match the selected
spread, Auburn differs by half a point, and Utah State differs by 3.5 points in
the cited table. These are checks of displayed values, not an error-rate estimate
for closing quotes. Different bookmakers, observation times, retrospective pages,
and article updates prevent treating the differences as automatic feed errors.

Several pages contain different numbers in their odds table and prose. For
example, the [Utah preview](https://www.actionnetwork.com/ncaaf/utah-utes-vs-utah-state-aggies-prediction-pick-odds-college-football-saturday-sept-14)
shows Utah -18 in its table, -17 in a summary, and -20 in the introduction.
Its displayed update is before kickoff, but today's mutable page is not proof
of what a reader or bettor saw at that historical moment.

The [South Alabama/Louisiana preview](https://www.actionnetwork.com/ncaaf/south-alabama-jaguars-vs-louisiana-ragin-cajuns-prediction-pick-odds-ncaaf-saturday-november-16)
likewise gives Louisiana -6.5 in the bet365 table while the pick names South
Alabama +7.5 at DraftKings. The [Boise State preview](https://www.actionnetwork.com/ncaaf/new-mexico-lobos-vs-boise-state-broncos-prediction-pick-odds-college-football-saturday-october-11-qs)
has -14.5 in the odds table but different spread values in the written analysis.

Therefore **zero of these spot checks establishes a complete, independently
timestamped Friday-to-close execution record**. They corroborate the existence
of nearby quoted prices and help expose source ambiguity. No quote is selected
because it would turn a model loss into a win. No historical sportsbook price
from these articles is substituted for the experiment's hypothetical -110 pricing.

## Implication

Retain exact future book, side, line, price, source update time, retrieval time,
publication cutoff and closing observation separately. Agreement among two
bookmaker families is a useful source check, not a substitute for timestamps.
The retrospective odds pages and original publisher articles above remain
evidence with explicitly different strengths, not interchangeable snapshots.
