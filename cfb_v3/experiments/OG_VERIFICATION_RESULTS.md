# OG Versus V3: Corroborated Reconstruction

Run: `cfb_v3/output/experiments/og_verification/20260909T212008Z/`.
Protocol: [OG_COMPARISON_PROTOCOL.md](OG_COMPARISON_PROTOCOL.md), including the
user-approved reconstruction amendment. Production engine, data, live cards and
reference backtest hashes were unchanged. No promotion, commit or push.

## Bottom Line

**Inconclusive. This does not establish that v3 is worse than the OG.**
The primary OG has better ATS results on the accepted same-cycle subset, while
v3 has slightly better margin error and straight-up accuracy. The other fixed
seeds and delayed-publication check do not establish a consistent advantage.
These are reconstructed historical inputs, not independently timestamped forecasts.

### Primary Matched Games

Positive predicted margin means the home team is favored. ATS percentages exclude
pushes. Both models receive exactly the same closing lines and scored games.

| Model | Games | ATS W-L-P | ATS | SU | Margin MAE |
|---|---:|---:|---:|---:|---:|
| OG, seed 20260909 | 208 | 95-105-8 | 47.50% | 152/208 (73.08%) | 12.958 |
| V3 control | 208 | 88-112-8 | 44.00% | 155/208 (74.52%) | 12.893 |
| OG, seed 20260910 | 208 | 96-104-8 | 48.00% | 154/208 (74.04%) | 12.862 |
| OG, seed 20260911 | 208 | 93-107-8 | 46.50% | 148/208 (71.15%) | 12.971 |

Primary OG minus v3: MAE +0.064 points, descriptive 95% whole-season bootstrap
interval **[-0.550, +0.686]**. ATS +3.50 percentage points, interval
**[-11.11, +7.32] percentage points**. All seeds' intervals include zero.
The primary margin difference changes sign when 2025 is omitted. Four unevenly
covered seasons, including only two 2022 games, do not justify strong inference.
Closing-line ATS is not Friday execution, realized ROI, or a full-season result.

### Timing Sensitivity

Same-cycle Monday availability is an explicit assumption, not a recovered timestamp.
The delayed check uses exactly the previous weekly cycle, with no interpolation
over gaps. It covers 225 games: OG 106-115-4 ATS and 13.050 MAE; v3 108-113-4
and 12.830 MAE. Those are DIFFERENT games from the primary 208-game comparison.

On the **85 games common to both timing checks**, v3 remains identical at 28-54-3
ATS and 12.627 MAE. The primary OG is 37-45-3 / 12.378 with same-cycle ratings and
35-47-3 / 12.436 with delayed ratings. Fresher inputs help slightly on this small
overlap; do not attribute the entire 208-versus-225 result difference to timing.

## Recovery And Coverage

All recovered weekly tables were checked against the cached schedule/results for
team records. Only tables with passing dated numerical spot checks were scored;
matching one team's rating does NOT independently authenticate every other number.
Unanchored tables remain available for further review, not silently certified.

| Season | Numeric author-table coverage recovered | Numerically corroborated cycles | Accepted team rows | Primary scored games |
|---|---|---|---:|---:|
| 2022 | Weeks 2-8, plus separate preseason table | Week 7 | 130 | 2 |
| 2023 | Weeks 1-14 | Weeks 9-10 | 266 | 83 |
| 2024 | Weeks 1-9; final table rejected | Week 9 | 134 | 17 |
| 2025 | Weeks 0-15 and bowl preview | Weeks 9, 10, 14 | 408 | 106 |

The accepted reconstruction ledger contains **938 team-snapshot rows across seven
cycles**. The full recovered weekly inventory is broader. All 2023, 2024 and 2025
records in the accepted cycles agree with the cached results; one 2022 team row
does not and is excluded. Date-coerced records such as `11-1` are decoded explicitly
and flagged, never guessed without a record check.

**Source conflict:** 16 of 17 numerical checks pass, allowing 0.1 points only for
differences of rounded ratings. Florida's 2025 Week 7 workbook change is +4.9,
but the dated author recap says +4.3. That entire snapshot is excluded. Its other
successful check is not used to excuse the conflict.

Late-2022 and late-2024 component tables remain missing. Alternate 2022 author
links and UMGOBLUE's link resolve to the same incomplete workbook. The late tabs
contain game predictions without the three required team-rating components.
No full late-season backfill was manufactured from those picks, ranks or final SP+.

## Verification And Remaining Limits

- Exact archived model: `365ca12:model (1).Rmd`, six-game window, fixed 2014-2021
  training, mtry 6, 500 trees, three preselected seeds. No new model fit or tuning.
- The compact raw-play builder reproduces the archived complete prediction table:
  **435 rows x 21 columns**, tolerance 1e-12. It retains raw names, weighted play
  aggregation, original week grouping, joins and complete-case behavior.
- Matchup-row construction also matches the actual archived `predict_winner`
  function in both home/away orientations. Neutral margins retain the original
  `(forward - reverse)/2` workflow.
- Every saved forest's training outcome and predictor columns match the preserved
  training table. The original same-game training features and duplicated joins
  remain; fixing them would be a separate repaired-OG experiment.
- The original latest-home/latest-away lookup substantially reduces coverage:
  having six games does not guarantee a complete row in the required orientation.
  No early-season stand-in or raw-team-factor replacement is introduced.
- Cached weekly Elo matches next-game pregame Elo in 5,342/5,941 comparable rows.
  For the primary selected home/away source rows, 414/416 match; two disagreements
  remain. This corroborates most lookup values but does not prove their release
  vintages. `selected_elo_checks.csv` records the disagreements; no Elo value is
  replaced using a later game's outcome.
- SP+ `available_at` and `through_at` stay unknown. A reconstructed weekly cycle
  is not backdated publication evidence. Source bytes/hashes are preserved.
- No game at or after its prediction cutoff supplies raw stats. Unknown game
  dates are omitted and logged. Tests cover cutoff boundaries, future-data
  mutation, duplicate anchors, date/record parsing and original side lookup.
- Six new test blocks and the 15 existing OG audit/replay blocks pass, 136 total
  expectations. The complete run also checks source hashes and production hashes.

## CFP Status

**No valid new CFP score or whole-bracket comparison can be reported from this run.**
None of the 28 CFP games has a qualifying snapshot in the scored lane. The 2023
bowl tab lacks components, the late-2024 components remain missing, and the 2025
bowl components still need dated numerical corroboration. Later-round ratings
would also be needed for a separate round-updated comparison.

The user's saved 2024 bracket remains evidence of the original 11-0 picks. The
previous retrospective 11-0 replay is separate: its 2024 SP+ values match all
134 post-playoff final rows, so it cannot establish a fair advantage over v3.
That does not prove the user's actual December 2024 run used final ratings.

## Reproduce

From the repository root, with the preserved local source caches available:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' run_cfb_og_verification.R
```

First-time compact cache extraction, if needed:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' cfb_v3/experiments/extract_og_compact.R
```

Requires the original `.RData`, preserved `model1_replay/20260909T195945Z` fits,
and `og_audit/20260909T203217Z/public_sources` capture. These large local output
artifacts are not automatically supplied by a fresh Git clone. Both commands
refuse to overwrite their existing immutable artifacts. Verification does no
network fetching and makes no live model or database changes.

Run outputs include the full SP ledger, record/numeric checks, complete omission
ledger, forward/reverse inputs, selected Elo checks, three-seed predictions,
season/audience/large-spread metrics, paired intervals, leave-one-season-out
results and common-game timing sensitivity.

## Next Evidence

1. Corroborate more of the already recovered 2023/2025 cycles, including the 2025
   bowl preview. Investigate the Florida discrepancy before reusing Week 7.
2. Recover missing late-2022 and late-2024 three-component ratings. Preserve gaps
   when that evidence cannot be recovered; do not use annual final ratings.
3. Resolve the remaining selected Elo discrepancies and vintage limitations.
4. Keep v3 unchanged pending broader matched evidence. Any bowl-policy change or
   repaired OG should remain a separate controlled experiment.

## Source Evidence

- [Author's 2022 workbook](https://docs.google.com/spreadsheets/d/1llrN8luL0XWuP8Y-Pb1NXKU84JhXLeUPafy1RfITEDw/edit),
  corroborated by [Ohio State's October 11, 2022 rating report](https://scarletandgame.com/2022/10/11/advanced-stats-show-ohio-state-football-best-college-football-team//).
- [2023 dated Michigan, Georgia and Purdue values](https://www.maizenbrew.com/2023/10/29/23937317/michigan-wolverines-football-sp-espn-rankings-bill-connelly-ohio-state-georgia-oregon-texas).
- [Author's 2024 workbook](https://docs.google.com/spreadsheets/d/1CJImfkg0ouHIIIGOWRfbvwC0TNWh76n47xkz8nqrVBc/edit),
  corroborated by [October 23, 2024 numeric table](https://www.elevenwarriors.com/skull-sessions/2024/10/149994/skull-session-ohio-state-ranks-no-1-in-sp-and-college-football-power-index-will-howard-buckeyes-national-championship).
- [Author's 2025 workbook](https://docs.google.com/spreadsheets/d/1a6hboWNnPeUzx5oUEjwJwf9vw4DuAV7lTaW4Q92Zpls/edit).
  Dated author recap URLs and individual numerical checks are in
  [og_sp_anchors.csv](og_sp_anchors.csv); failed checks are preserved there too.
