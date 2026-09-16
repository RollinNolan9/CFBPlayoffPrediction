# External Ratings Benchmark

## Purpose And Locked Rules

An isolated prospective benchmark, not a production model change. V3 forecasts,
published picks, training data, feature policy and fitted models remain untouched.
The primary question is margin accuracy on the **same games**, followed by ATS
at the **same frozen article quote**. A perfect bracket does not establish an ATS edge.

Pre-specified candidates:

1. Frozen v3 home-minus-away expected margin.
2. SP+ overall numeric rating differential.
3. Sagarin **Predictor** differential, not composite, Golden Mean or Recent.
4. FPI numeric rating differential, not its ordinal rank or strength of record.
5. Equal-weight mean of all three external margin forecasts.
6. 50% v3 / 50% external consensus, testing whether v3 adds complementary information.
7. Negative frozen home spread as a market margin/SU reference, never an ATS strategy.

External rating differentials receive a fixed **2.4 points at home, zero at neutral
sites**, with no fitted adjustment. These are explicitly **rating-derived baselines**,
not the publishers' official matchup forecasts. Those may use additional adjustments.
Do not claim to have beaten a publisher's full prediction system on this basis alone.
There is no clipping at 20 or 21 points. Positive expected margin favors home;
fair home betting spread is its negative; home edge = expected margin + quoted home spread.

No outcome-dependent weights, thresholds, variant selection, or automatic promotion.
All-model headline comparisons use the complete four-way cohort. Pairwise comparisons
report their own matched game count; available-per-model totals are diagnostic only.
The common-four-way-lined cohort also requires a valid quote so the market reference
has the identical games. Paired ATS differences require decisions from both models;
pushes and no-picks are excluded, and the market reference has no ATS decisions.
Consensus stays missing unless **all three** external inputs are present.

## Capture Before Games

From the repository root, after producing the article card:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_external_ratings.R snapshot --card .\cfb_v3\output\2026\week_2_publication\20260911T195549Z\predictions.csv
```

The runner reads season/week from that card, freezes a copy, downloads the sources,
and writes a new `cfb_v3/output/experiments/external_ratings/snapshot_*` directory.
Use the newly generated card path for subsequent weeks. Update `external_sources.json`
when the author's annual workbook changes; the runner refuses a different season.
An explicit `--sources PATH` may point to another reviewed source configuration.

SP+ comes from the author's public `FBS Week N` worksheet; FPI uses CFBD's current
`/ratings/fpi` endpoint and the configured `CFBD_API_KEY`. Sagarin uses his public
ratings page with season/header checks. TLS verification stays enabled. Source
failures are visible in `source_ledger.csv`, with no silent substitute or old-cache reuse.
All raw responses, source labels, retrieval times and SHA-256 hashes are retained.
API credentials are never persisted in artifacts.
The R packages used are the existing `httr`, `jsonlite`, `digest`, `readxl`, `xml2`
and `testthat` (tests only); there is no new service or database to operate.

`comparison.csv` has side-by-side margins. `predictions.csv` adds SU and ATS sides.
`coverage.csv` records missed teams, started games and invalid quote timestamps.
Only rows with `game_status=included` / `eligible=TRUE` are valid prospective forecasts.
Where the publication directory includes `cfbd_lines_raw.rds`, conference metadata
is copied from it using checked, unique game identities, without altering the card.
The current article's line and provider are retained, **not refreshed and not relabeled
as closing lines**. Spread juice is optional, in `home_spread_odds` and
`away_spread_odds`; these must be the American odds at that same quote. Moneyline
odds are not accepted as spread prices. Unknown prices remain unknown.

The common cutoff is the end of source capture. Already-started games are excluded.
V3 may have been generated earlier than external ratings; this is a frozen published
v3 versus latest-accessible external benchmark, not equal-age feature reconstruction.
Capture proves availability by now, **not the original publication date**. Provider
update/data-through times may be unknown; retain source labels and review unusual
disagreements. Never backdate current ratings to evaluate an earlier card.

## Score Without Rebuilding

After games finish, use the directory printed by the capture command:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_external_ratings.R score --snapshot .\cfb_v3\output\experiments\external_ratings\snapshot_TIMESTAMP
```

The score command verifies the sealed snapshot and fetches CFBD final scores. It
writes a separate `score_*` directory and never overwrites forecasts. Rerunning after
more games finish is safe. Pending games are not counted as losses or zero scores.
Combine weeks using repeated `--snapshot DIRECTORY` arguments. Duplicate game/model
entries fail: choose one predeclared capture per game, not whichever performed best.

Offline scoring accepts `--results PATH.csv`, with columns:
`game_id,season,home,away,kickoff,home_score,away_score,completed`.
Kickoff must be an explicit UTC ISO timestamp, e.g. `2026-09-12T16:00:00Z`.
Only completed games are graded; team/season identity must match the frozen card.
CFBD and supplied result files are saved with the score report for reproducibility.
Each score directory also contains normalized `results.csv` for offline replay.

## Diagnostics And Interpretation

- Margin MAE leads; RMSE, signed home bias, SU, ATS wins/losses/pushes also reported.
- Week 0/1, Weeks 2-4, Week 5+, CFP, neutral sites, spreads above 21, season slices.
- SEC-versus-other games have a signed SEC optimism measure, positive when the
  forecast overstated the SEC side. Unknown conference metadata is not guessed.
- Paired MAE changes compare each alternative with v3 and the combined model with
  external consensus. Season-block intervals require at least two seasons; one
  weekend cannot produce a meaningful cross-season robustness claim.
- ATS binomial intervals are descriptive, not multiple-testing-adjusted evidence.
- ROI uses one unit risked per actual-priced ATS selection. `roi_all_picks` stays
  missing if even one selected bet lacks a valid spread price. Priced-subset ROI
  is separately labeled and should not be compared on unequal price coverage.
- No invented normal-distribution win/cover probabilities or calibration claims.
  Comparable archived matchup probabilities are needed to test calibration.
- Non-CFP bowls are excluded when explicitly identified. Prospective regular-season
  article cards are the initial scope; CFP metadata must be supplied for CFP slices.

## Next Gates

Accumulate locked prospective cards, including the lines/prices actually available
at publication. Recover genuine dated historical Sagarin/FPI snapshots before any
multi-season four-way backtest. Existing reconstructed SP+ tables remain a separate,
qualified evidence lane; annual final ratings are not historical weekly substitutes.

Only after coverage is adequate: compare official published matchup forecasts,
add properly timestamped closing-line diagnostics, and test learned blend weights
using earlier seasons only with a reserved evaluation season. Benchmark inclusion
does not authorize external preseason priors in v3's Week 5+ production features.

Sources: [Connelly public workbook](https://docs.google.com/spreadsheets/d/1vwoVl-Dxy0es87Z9I1RTvFzr72Lb1fAkREfbLxbK-eg/edit),
[Sagarin](https://sagarin.com/sports/cfsend.htm?lv=true),
[CFBD ratings API](https://api.collegefootballdata.com/api/ratings).
