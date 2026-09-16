# Historical external ratings: source discovery

Checked September 11, 2026. Research only: no model, article card, package,
or backtest results changed. Downloads and inventories are retained under:

`cfb_v3/output/experiments/external_history_sources/discovery_20260911T235344Z/`

## Recommendation

Start the broad comparison with The Prediction Tracker's archived game
projections. Use SportsDataverse's weekly FPI and pregame Wayback Sagarin
pages for independent checks and rating-level experiments. Keep publisher
archives distinct from independently timestamped evidence. This discovery
substantially expands the sources available after the initial SP+ comparison;
it does not make the earlier results a full four-system backtest.

## 1. The Prediction Tracker: game projections

[Archive index](https://www.thepredictiontracker.com/ncaaarchive.html)
links public season CSVs. All four files below were downloaded and inspected.
Counts are raw rows, not yet eligible matched backtest games.

| Season | Game rows | Sagarin Predictor present | ESPN FPI present |
| --- | ---: | ---: | ---: |
| [2022](https://www.thepredictiontracker.com/ncaa2022.csv) | 776 | 776 | 769 |
| [2023](https://www.thepredictiontracker.com/ncaa2023.csv) | 792 | 792 | 792 |
| [2024](https://www.thepredictiontracker.com/ncaa2024.csv) | 799 | 799 | 799 |
| [2025](https://www.thepredictiontracker.com/ncaa2025.csv) | 808 | 808 | 762 |

Important fields: `Home`, `Road`, `week`, `linesagpred`, `linesag`,
`linesaggm`, `linesagr`, `lineespn`, `lineopen`, `linemidweek`, `line`,
`hscore`, `vscore`, `actual`. Use `linesagpred` for Predictor, not the other
Sagarin variants. These are tracker-recorded matchup projections, not raw
team power ratings or necessarily official publisher matchup forecasts.

[The tracker explains its sign convention](https://www.thepredictiontracker.com/predncaa.html):
positive means a projected home win. Its market `line` therefore has the
opposite sign to our conventional home handicap. Do not add home advantage
again to an archived game projection. `line` is labeled updated on the site;
do not silently call it a verified closing line or Friday DraftKings quote.

Observed limitations and checks:

- 3,175 raw rows, with 3,122 populated FPI values. The 2024 South Florida vs
  Memphis entry appears twice at tracker week 8 with the same displayed
  predictions and scores. Resolve this explicitly before joining or grading.
- These CSVs have no game IDs, kickoff dates, neutral-site flags, or per-row
  prediction publication timestamps. Recover identity and venue from our
  schedule; reject ambiguous matches and conflicting duplicates.
- Tracker week 1 includes conventional Week 0 games. Observed regular-season
  examples are one week ahead of our schedule labels. Do not blindly join week
  numbers, especially across postseason and repeated matchups.
- All downloaded `actual` values equal `hscore - vscore`. No duplicate
  Home/Road/week groups were found outside the one 2024 case.
- FPI is absent for the seven first-week 2022 rows and 46 rows in 2025.
  Missing projections must remain missing, not become zero-margin forecasts.
- Archived results contain non-CFP bowls. Exclude those from the primary
  comparison and report regular season and CFP separately, per model policy.
- A historical season file is publisher-maintained evidence, not independent
  proof that every value was publicly available by our Friday cutoff.
  Report this limit and preserve an independently corroborated subset.
- Use a common game/line cohort for every model and keep bookmaker source and
  observation time explicit. No actual ROI can be established without prices.

### Independent Sagarin spot checks

For each row below, an actual Wayback page was retrieved with the exact
requested capture timestamp (no fallback to a later capture). Its page header
predates the game, and its Predictor column plus Predictor-specific home
advantage exactly reproduces `linesagpred`. Kickoffs were checked against
`cfb_v3/data/historical_games.csv`. These are four checks, not full validation.

| Game | Pregame capture UTC | Calculation, home margin | Tracker |
| --- | --- | --- | ---: |
| Ohio State at Michigan State, 2022-10-08 | 2022-10-07 23:13:01 | 74.99 - 93.96 + 2.07 | -16.90 |
| Ohio State at Rutgers, 2023-11-04 | 2023-11-03 11:14:48 | 74.06 - 93.76 + 2.18 | -17.52 |
| Purdue at Ohio State, 2024-11-09 | 2024-11-05 22:55:12 | 89.91 - 61.58 + 3.33 | 31.66 |
| Ohio State at Purdue, 2025-11-08 | 2025-11-03 17:10:11 | 66.07 - 94.15 + 3.76 | -24.32 |

## 2. SportsDataverse / cfbfastR: weekly numeric FPI

[Loader documentation](https://cfbfastr.sportsdataverse.org/reference/load_cfb_fpi_weekly.html)
and [public CSV/RDS/Parquet release](https://github.com/sportsdataverse/sportsdataverse-data/releases/tag/cfb_fpi_weekly).
The 2022-2025 CSVs were downloaded directly; no API key, subscription, or
package upgrade was needed.

| Season | Raw team-week rows | Teams | Regular-season rows passing source flags |
| --- | ---: | ---: | ---: |
| 2022 | 2,096 | 131 | 1,834 |
| 2023 | 2,128 | 133 | 1,862 |
| 2024 | 2,278 | 134 | 2,010 |
| 2025 | 2,312 | 136 | 2,040 |

All 8,814 rows have numeric `fpi`; there are no duplicate season/type/week/team
keys. The 7,746 flag-passing regular-season rows are candidates, not approved
pregame joins. The archive is retrieved now and retains upstream metadata;
it is not an independently timestamped capture of every original release.

Required controls:

- Every inspected season's regular Week 1 slot is out of sequence. For example,
  2024 Week 1 has a December 15 timestamp and Alabama's 9-3 record. Reject these
  as preseason inputs. This archive alone does not solve Week 0/1 history.
- Use `fpi`, not ordinal rank. Reject out-of-sequence and noncontemporaneous
  flags. Both `last_updated` and `run_date_time_key` must be parsed and checked
  against the prediction cutoff, with records checked against completed games.
- Week labels can describe ratings AFTER that week's games. For example,
  Alabama's 2024 Week 2 row is September 8 with a 2-0 record. It cannot predict
  the September 7 game. Join by verified availability, not equal week numbers.
- Some 2022 `last_updated` values repeat while the as-of key and record advance.
  Neither timestamp alone is sufficient. Use a conservative availability bound
  and quarantine any record/date conflict.
- The postseason slot in these files is a season-final snapshot, not separate
  pre-round CFP releases. A frozen pre-playoff rating is a different experiment
  from rerunning before each round; do not substitute the final postseason row.
- Inspect direct files or existing compatible loaders. Do not upgrade cfbfastR
  just for this source: package changes may alter the production EPA/WPA stack.

## 3. Wayback: original Sagarin numeric tables

[Example original page captured November 5, 2024](https://web.archive.org/web/20241105225512/http://sagarin.com/sports/cfsend.htm).
The four sampled pages contain numeric Predictor, other rating variants,
records, through-results dates, and variant-specific home advantage.

The saved CDX index queried `sagarin.com/sports/cfsend.htm` from August 2022
through January 2026, collapsed to one capture per calendar day. It returned
34 capture days in the queried portion of 2022, 48 in 2023, 35 in 2024,
40 in 2025, and 3 in January 2026. These are capture days, not distinct weekly
ratings: offseason captures, stale pages, and missing weeks remain possible.

Use the exact archived response timestamp, page season, through-results date,
and team records. Only apply a page to games after verified availability.
Never accept Wayback's nearest later redirect silently. Numeric Predictor and
overall Sagarin are not interchangeable; older HTML is formatted differently.

## Lower-priority mirror

[football-statfinder](https://github.com/llampwall/football-statfinder)
has `data/SAGARIN_WEEKLY_HISTORICAL_CFB.csv`: 6,137 rows spanning portions of
2024/2025, without row-level capture dates. Its `pr` is not safely interpretable
as Predictor: the 2024 Week 10 Georgia value 92.81 matches the archived overall
rating, while Predictor is 90.90. Weeks 7-9 of 2024 contain only 25 teams.

Its separate `data/archive/sagarin_wayback/cfb_2025_w10.csv` does distinguish
rating variants, but needs source timestamps. A sampled weekly output was
fetched November 23 while labeled through November 15, illustrating why file
week labels are insufficient. Prefer original archived pages and the broader
Prediction Tracker game archive over treating this mirror as ready to score.

## Next implementation

1. Build a separate, provenance-preserving importer for archived projections.
2. Resolve aliases, weeks, repeated opponents, neutral venues, duplicate rows,
   and missing values against the canonical schedule; emit a join audit.
3. Expand independent pregame checks across seasons, early weeks and CFP.
4. Compare frozen v3, tracker Sagarin Predictor and tracker FPI on identical
   games/lines; include SP+ only where qualified history overlaps. Report
   source-specific coverage and uncertainty, with no automatic model promotion.

No new performance figures were calculated during this discovery pass.
