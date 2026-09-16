# Five-Game V1 Reconstruction Versus V3

## Correction: The Original Frozen Bracket

This experiment is NOT the user's six-game `model (1).Rmd` or original 11-0 bracket.
The archived six-game replay, frozen before the 2024 playoff, returns 11-0 in all
three fixed seeds; v3 under the same freeze returns 10-1. The original presentation
also contains the 11-0 bracket. See [the corrected frozen comparison](MODEL1_RESULTS.md).
The results below remain valid for the modified five-game reconstruction only.
In particular, its prior-year SP+ substitution and round-updated v3 comparison
cannot establish that v3 improved on the user's original playoff picks.

## Verdict

For this five-game reconstruction, v3 improves margin and winner prediction, but the reconstructed
v1 has a higher aggregate ATS percentage on the matched sample. There is no evidence
here for an across-the-board rollback, and no established profitable ATS edge for
either model. No production model, input, or prediction card was changed.

The user identified the five-game `model.Rmd` at commit `365ca12` (January 23, 2025)
as v1. This is a pregame reconstruction of that model, not a reproduction of the
original notebook's leaked retrospective accuracy. Read the [protocol](V1_PROTOCOL.md)
for the source corrections and limitations.

## Matched Results

Both models predict exactly the same 1,791 FBS-vs-FBS regular-season/CFP games from
2022-2025. All matched games have lines; ATS excludes 31 pushes. Lower margin MAE
is better. The primary seed was declared before fitting.

| Metric | Original-Style V1 | V3 |
| --- | ---: | ---: |
| Margin MAE | 13.225 points | 12.677 points |
| Straight-up accuracy | 67.95% | 71.41% |
| ATS record | 892-868-31 | 870-890-31 |
| ATS accuracy | 50.68% | 49.43% |

V3 reduces MAE by 0.547 points and improves winner accuracy by 3.46 percentage
points. V1 wins 22 additional ATS decisions, a 1.25-point percentage advantage.

Whole-season bootstrap intervals put v1's extra margin error at +0.160 to +0.934
points, but its ATS advantage at -1.49 to +4.16 percentage points. Four test seasons
limit inference; the ATS difference does not clearly distinguish signal from noise.
Neither aggregate ATS rate reaches the mathematical 52.38% break-even rate at
uniform -110 odds. This is not a record of actual prices or bets.

The two fixed sensitivity seeds produce v1 ATS rates of 51.31% and 50.85%, with
MAE of 13.229 and 13.233. They preserve the primary seed's training-selected mtry
settings. No best-seed selection or new ensemble was used.

## Season Results

| Test Season | V1 ATS | V3 ATS | V1 MAE | V3 MAE |
| --- | ---: | ---: | ---: | ---: |
| 2022 | 51.13% | 48.87% | 12.792 | 12.598 |
| 2023 | 53.74% | 48.07% | 13.259 | 13.134 |
| 2024 | 47.81% | 48.27% | 13.259 | 12.574 |
| 2025 | 50.00% | 52.49% | 13.585 | 12.407 |

V3 has lower margin error in every test season. V1's aggregate ATS advantage comes
from 2022-2023, not the latest two seasons. These are historical comparisons, not
evidence from an untouched research holdout.

## Playoffs And Large Spreads

- On 27 matched CFP games, v3 is 17-10 ATS versus v1's 13-14. V3's margin MAE is
  12.60 versus 14.52, and winner accuracy is 74.07% versus 59.26%. The small sample
  does not prove a lasting playoff advantage. Oregon-James Madison in 2025 is
  omitted because the original notebook explicitly excludes James Madison.
- On the existing power-conference/Notre Dame/UConn slice (which includes historical
  Pac-12 teams), v3 is 50.70% ATS versus v1's 50.20%. The aggregate v1 advantage
  should not be assumed to hold for the main article audience.
- On 155 games with spreads above 21, v1 is 80-74-1 ATS (51.95%) versus v3's
  71-83-1 (46.10%). However, v1's MAE is worse: 17.40 versus 13.96 points.
- A descriptive check after fitting found that v1 selects the underdog in all 155
  large-spread games. Its mean absolute predicted margin is just 14.15 points,
  compared with a mean market spread of 27.39; v3 predicts 25.32 on average.
  V1's maximum absolute margin anywhere in the matched sample is 24.20, compared
  with v3's 46.69. This is strong evidence of compressed v1 predictions, not a
  reason to treat its large-spread ATS score as precise matchup discrimination.

## Coverage And Reconstruction

V1 covers 1,791 of v3's 3,026 eligible FBS test games (59.2%). Omissions:

- 1,137 games lack five prior same-season observations.
- 45 involve the original James Madison exclusion.
- 34 lack a required rating or efficiency value.
- 19 have a team-side categorical level not seen in the training set.

There are no matched Week 0/1 or Week 2 games. Do not compare v1's late-season ATS
number with v3's full-season 49.68%, or use this experiment to evaluate preseason
predictions. Coverage and every omitted game are exported.

The original random forest retains team-name dummies, rolling EPA/WPA, SP+, and
Elo. Its original 10-fold training CV over mtry 2, 4, 6 selected 6 in all four outer
fits, with 500 trees and node size 5. Important corrections/limits:

- Rolling inputs use only five games before the target week's cutoff, never the
  game being predicted. Both sides use consistently mapped team statistics.
- Pregame Elo replaces possibly postgame same-week Elo. Prior-season SP replaces
  current-season SP because the saved table lacks weekly release timestamps.
  This is a material difference from the original feature values.
- Official cached schedules and outcomes are used from 2020 onward. Earlier
  outcomes come from legacy PBP. The archive has no 2013 SP, so complete-case
  fitting actually starts in 2015. Each outer fit ends before its test season.
- The source `.RData` contains 2014-2025 data from a later saved workspace. It is
  not an original point-in-time archive. No 2026 outcomes are used.
- V1 retains its unweighted training and bowl handling. V3 retains its existing
  recency weights, feature construction, preseason fade, and bowl exclusions.
  This is a comparison of approaches, not an attribution study isolating each change.

## Non-CFP Bowls

V3 already gives all 206 non-CFP bowl rows in its 2020-2025 foundation zero training
weight, removes zero-weight rows before fitting, and excludes these games from its
main team-form history. Simply removing them again cannot change that fit. Their
effect on v1 has not been tested; no separate bowl ablation was run.

## Reproduction

From the repository root in PowerShell:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v1_comparison.R
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_v1_comparison.R
```

The first run extracts legacy input tables from the ignored root `.RData`; later
runs reuse `cfb_v3/output/experiments/v1_inputs/workspace_inputs.rds`. Another
computer needs that extracted cache or the original workspace, plus v3's existing
data/backtest files. Dependencies include caret, randomForest, dplyr, jsonlite,
and the existing v3 engine packages. Four local R workers run the outer folds.

Verified run: `20260909T190243Z`. Its [generated report](../output/experiments/v1_comparison/20260909T190243Z/report.md)
links by location to the accompanying game-level predictions, coverage, reconstructed
features, training CV scores, fold settings, protocol, source hashes, and verification.
Generated artifacts are ignored by Git; this summary and the runner/protocol/tests
are intended to travel with the repository. The earlier pilot `20260909T185651Z`
is explicitly marked invalid because it used two settings from the sibling notebook;
its scores were not used to select settings or draw these conclusions.

Five focused test blocks passed. They cover prior-game windows, future-data
invariance, training-only categorical levels, chronological fitting/tuning, and
the correct original team exclusion. Protected engine/data/card hashes were
unchanged, and the archived original source remains untouched.

Next decision: retain v3 as the current candidate rather than roll back wholesale.
Any further ATS/large-spread calibration study should be separately specified and
evaluated. Do not promote a new threshold or betting strategy from these slices.
