# Week 3 Preliminary Run

Completed September 13, 2026. Prediction cutoff: **22:04:16 UTC / 6:04 p.m. Eastern**.
This is a live preliminary run, not a frozen publication card or a model promotion.

## Outputs

- [Dashboard](output/2026/week_3_preliminary/20260913T220416Z/dashboard.html)
- [Predictions CSV](output/2026/week_3_preliminary/20260913T220416Z/predictions.csv)
- [Run report](output/2026/week_3_preliminary/20260913T220416Z/RUN_REPORT.md)
- [Verification](output/2026/week_3_preliminary/20260913T220416Z/verification.json)
- [Review queue](output/2026/week_3_preliminary/20260913T220416Z/review_queue.csv)
- [Included/excluded slate](output/2026/week_3_preliminary/20260913T220416Z/slate_coverage.csv)

Generated data and outputs are ignored by Git; copy the output/cache folders when
moving this exact run to another computer.

## Verified

- 56 unique Friday/Saturday FBS-vs-FBS games, all with DraftKings spreads via CFBD.
- All 100 eligible completed prior-week games have PBP after the recovery below.
- No upcoming game entered the completed-game inputs; quotes precede the run cutoff.
- Saved-model replay reproduces all margins exactly. The fitted model is identical
  to the retained Week 2 live model, including its unchanged ATS calibration status.
- V3 weights are unchanged; Week 3 roster features retain their 0.30 multiplier.
- AP Week 3's 25 teams are captured in `inbox/rankings.csv` with the `ap` tag,
  and copied to the run. Rankings are metadata, not football predictors.
- 45 protected engine/history/Week 2 publication files have unchanged SHA256 hashes.
- Quarto generated a self-contained HTML dashboard. Static HTML checks match all
  56 rows' market lines, model lines, ATS sides, SU sides, and Unvalidated labels
  to the CSV. Interactive/visual browser verification was blocked by browser policy.

## Recovery And Limits

The refreshed cfbfastR season release lacked UAB-UL Monroe, game `401862708`.
The standard runner correctly stopped. CFBD supplied 169 plays through the installed
`cfbfastR::cfbd_pbp_data(2026, week=2, team="UAB", epa_wpa=TRUE)` pipeline.
Identity and final score state (26-20) were checked. Five missing EPA values occur
on punts/end-period rows, not offensive efficiency scrimmage plays. Schedule
metadata and the cache's numeric play-ID type were aligned; EPA was not altered.
All pre-existing 2026 cache rows remain identical.

The original cache, raw recovery, compact supplement, source ledger, input snapshot,
and recovery scripts are retained under
`output/2026/week_3_preliminary/source_recovery_20260913T215951Z/`.
This is a documented cache supplement, **not an automatic fallback added to v3**.
A later `--refresh=true` replaces the season cache and could lose this supplement
until the upstream release includes the game. Check coverage before publication.

Sources were refreshed before the prediction cutoff so newly captured quotes
would not be rejected as later than the run's `as_of` time. The first dashboard
render failed because the sandbox blocked Quarto launching R; a separate permitted
render succeeded without refitting or changing predictions.

18 games have spreads above 21 points. 47 games involve a team with at most one
current-season FBS efficiency game; Northwestern has zero and retains its existing
prior-year fallback. No injury adjustments, FanDuel quotes, or spread juice are
included. ATS calibration remains unavailable and is displayed as Unvalidated.
Thursday games and FCS opponents remain outside the automated card.

## Cached Rerun

From the repository root, with these local caches present:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_v3.R --mode=live --season=2026 --week=3 --all-picks=true --refresh=false
```

This writes a new live snapshot; it does not overwrite the published Week 2 card.
The enriched preliminary dashboard linked above includes timestamps and AP/sample
metadata from the retained `finalize_run.R` script. Successful execution is not
evidence of an ATS advantage; no model challenger was promoted by this run.
