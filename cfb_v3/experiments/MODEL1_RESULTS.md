# Archived Six-Game Model: Frozen 2024 CFP

## Source Audit Correction

**September 9 follow-up: the replay's cached 2024 SP+ values match Connelly's
post-playoff `FBS FINAL` table for all 134 teams, across overall, offense and defense.**
This is confirmed hindsight contamination, not just missing timestamps. The 11-0
replay below is an archival-workflow diagnostic and is **not a valid pregame
head-to-head result against v3**. See [the source audit](OG_DATA_AUDIT.md).

The saved presentation still supports the user's original perfect bracket. This
finding concerns the inputs used in our new replay, not proof that the user's
original December run used those same values. Its exact live inputs remain unrecovered.

## Replay Result

The archived `365ca12:model (1).Rmd` reconstructs the entire 2024-season playoff
bracket at **11-0 in all three predeclared random seeds**, using the contaminated
cached ratings described above. V3, with its inputs frozen before the playoff,
goes **10-1**, picking Notre Dame over Ohio State in the
championship. No model advances using actual earlier outcomes.

| Model | Frozen Bracket | Champion |
|---|---:|---|
| Archived model (1), seed 20260909 | 11-0 | Ohio State |
| Archived model (1), seed 20260910 | 11-0 | Ohio State |
| Archived model (1), seed 20260911 | 11-0 | Ohio State |
| V3, frozen before first round | 10-1 | Notre Dame |

The earlier five-game reconstruction substituted prior-season SP+ for same-season
SP+ and compared against v3 updated each round. Its seven-correct 2024 result is
not the user's original record. Its broad performance comparisons cannot establish
that v3 improved on the original frozen playoff bracket. See the corrected notice
in [V1_RESULTS.md](V1_RESULTS.md).

## Original Bracket Evidence

Page 18 of the original `CFB Playoff  Bracket Prediction.pdf` contains all 11 correct
winners, including Ohio State over Notre Dame. Page 19 also shows Ohio State as
champion. The local PDF and the copy at January 2025 commit `365ca12` have the same
Git blob: `10b2cd775ba80d0cdb72b6d68a094e60826804ad`.

The PDF's creation metadata is December 13, 2024, consistent with a pre-playoff
presentation. Metadata alone is not independently verified publication timing,
but this is saved bracket evidence supporting the user's account, not merely a
new model fit that happened to get 11-0. The fitted forest and exact rating release
snapshots used then have not been recovered.

## Matchup Predictions

Numbers below are the chosen winner's projected winning margin, not sportsbook
spread notation. The archived column uses the first predeclared seed; all three
seeds pick the same winners. The first ten winners agree between models.

| Round | Matchup | Archived Model (1) | V3 Frozen |
|---|---|---|---|
| First | Notre Dame / Indiana | Notre Dame by 10.72 | Notre Dame by 7.40 |
| First | Penn State / SMU | Penn State by 7.46 | Penn State by 8.91 |
| First | Texas / Clemson | Texas by 3.14 | Texas by 14.99 |
| First | Ohio State / Tennessee | Ohio State by 9.51 | Ohio State by 10.37 |
| Quarterfinal | Boise State / Penn State | Penn State by 3.15 | Penn State by 1.78 |
| Quarterfinal | Arizona State / Texas | Texas by 0.43 | Texas by 9.04 |
| Quarterfinal | Oregon / Ohio State | Ohio State by 2.35 | Ohio State by 0.92 |
| Quarterfinal | Georgia / Notre Dame | Notre Dame by 0.46 | Notre Dame by 6.92 |
| Semifinal | Penn State / Notre Dame | Notre Dame by 1.84 | Notre Dame by 2.51 |
| Semifinal | Texas / Ohio State | Ohio State by 8.14 | Ohio State by 0.14 |
| Final | Notre Dame / Ohio State | Ohio State by 2.98 | Notre Dame by 2.55 |

The archived model's Texas/Arizona State and Georgia/Notre Dame calls are under
half a point in the primary fit. A perfect winner record does not mean all 11 were
high-confidence picks, accurate margins, or winning ATS selections. This replay
does not evaluate ATS: future-round market lines were unavailable when the bracket
was frozen.

## Fidelity And Limits

- Original notebook table transformations, six-game window, 2014-2021 training,
  fixed mtry 6, 500 trees and node size 5 retained. The one-setting CV cannot choose
  another mtry; the final forest is refitted directly. Original RNG state is unknown.
- Original joins retained: 3,641 complete training rows represent 3,210 games.
  Duplicate rows are not silently removed. The 19 selected columns include the
  outcome; PFF, coach and injury columns are not direct predictors in this version.
- PFF/coach join files come from the same archived commit under different filenames
  from those referenced by the notebook. Their exact live versions cannot be certified.
- One frozen prediction table uses only PBP before `2024-12-21T01:00:00Z`, with
  zero 2024 CFP game IDs. Forty-six missing game dates are recovered from the cached
  schedule; four unresolved FCS games not involving CFP participants are logged and
  omitted. A missing date involving a CFP participant stops the run.
- Original latest-home/latest-away lookup is preserved. Neutral margin is
  `(home-orientation prediction - reversed-orientation prediction) / 2`.
- **Cached 2024 SP+ is confirmed to match final, post-playoff ratings.** Weekly Elo
  also lacks original availability timestamps. Freezing PBP alone did not make
  the reconstruction a point-in-time backtest, even though its picks match the saved bracket.
- V3 trains through 2023 using its previously selected 2024 settings: 65/35 coach
  split and ridge lambda 32. Its first-round replay matches the saved reference to
  numerical precision. Neither model sees new playoff PBP between rounds.
- Different training periods and architectures make this a workflow comparison,
  not an experiment attributing the result to one feature or to bowl exclusion.

## Verification And Reproduction

```powershell
Rscript run_cfb_model1_replay.R
Rscript cfb_v2/tests/test_model1_replay.R
```

Run from the repository root with the existing R dependencies. This requires the
archived Git commit, original `.RData` (or its extracted cache plus the original for
hash verification), v3 foundation/backtest files, and the saved controlled-experiment
2024 fold choices. Generated inputs/models/results are ignored by Git.

Verified rerun: `20260909T195945Z` under
`cfb_v3/output/experiments/model1_replay/`. It contains `REPORT.md`, full per-seed
brackets, conditional-matchup diagnostics, fitted models, input tables, date recovery,
source hashes and R session information. The earlier completed diagnostic
`20260909T195350Z` has identical picks but an empty v3 latest-source timestamp caused
by NA placeholder rows; the rerun explicitly removes those placeholders and asserts
the cutoff. The initial `20260909T195044Z` attempt stopped at missing PBP dates.

Seven passing test blocks cover archived source selection, cutoff/future-data invariance,
missing-date recovery, propagation through byes, grading without future matchup
substitution, neutral orientation signs, and completed-artifact audit consistency. Production engine/data/card hashes
remain unchanged. Neither model was promoted, replaced, committed or pushed.

Before using the separate generic production bracket helper, fix its lack of explicit
first-round-to-bye links. This replay uses its own tested bracket graph and is not
affected; the production helper remains untouched and the issue is recorded in TODOS.

## Decision

The original perfect bracket is supported by the saved presentation, not independently
validated by this contaminated replay. Neither this replay nor the earlier modified
five-game results establishes whether v3 improved on the original. Keep production
unchanged. Recover and validate actual pregame ratings before a scored six-game
comparison; keep bowl exclusion separate.
