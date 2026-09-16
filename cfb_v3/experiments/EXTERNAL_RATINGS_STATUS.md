# Initial External Benchmark Capture

Captured September 11, 2026 at **22:59:06 UTC**, before kickoff of all 49 games on
the frozen Week 2 publication card. This is a prospective comparison, not a claim
about historical ATS performance. No production model or publication pick changed.

Capture: `cfb_v3/output/experiments/external_ratings/snapshot_20260911T225904.407Z/`

| Source | Team ratings captured | Article games covered |
|---|---:|---:|
| SP+ | 138 | 49 |
| FPI | 138 | 49 |
| Sagarin Predictor | 0 | 0 |

Sagarin's HTTPS certificate failed validation in both the HTTP client and browser.
No certificate bypass, stale-cache substitution, or reconstructed rating was used.
Consequently there are **zero complete four-way games** and both fixed consensus
columns are missing. V3-versus-SP+ and v3-versus-FPI pairwise evaluation can proceed
after these games finish. A later Sagarin capture cannot amend this frozen snapshot;
it would require a new, separately timed pregame capture, excluding started games.

The same original DraftKings quote is used for every model. These are not refreshed
closing lines. Spread juice is unavailable, so the eventual ATS scores will not
establish realized ROI. The initial live scoring dry run returned zero completed
games, correctly leaving performance metrics blank rather than inventing results.

## Illustrative Disagreements

All numbers below are **home-minus-away margins**, not betting-line notation.
External margins are rating differentials plus the protocol's fixed home adjustment,
not the publishers' official matchup projections. Disagreement alone is not an edge.

| Home vs. away | V3 | SP+ derived | FPI derived |
|---|---:|---:|---:|
| Army vs. South Florida | -10.9 | +7.7 | +2.8 |
| Florida Atlantic vs. Navy | +1.4 | -13.4 | -12.3 |
| Connecticut vs. Maryland | +1.5 | -13.0 | -8.0 |
| Georgia vs. Western Kentucky | +29.9 | +44.2 | +38.9 |

Do not change the article picks after seeing this comparison. Freeze the experiment,
score the same games, and use the resulting errors to decide what warrants a proper
follow-up test. See [the protocol and commands](EXTERNAL_RATINGS.md).

Verification: 13 new benchmark test blocks, eight existing four-way blocks and
12 existing v3 blocks passed. The copied publication card matches its original
SHA-256; the fitted v3 model and preliminary card still match their previously
recorded hashes. Network capture and live pending-game scoring both completed.
