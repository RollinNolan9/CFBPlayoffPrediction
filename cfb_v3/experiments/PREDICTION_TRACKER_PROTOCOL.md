# Prediction Tracker challenger protocol

Locked before scoring, September 12, 2026 UTC. All outputs are experimental.
Production v3, article cards, historical foundation and packages stay unchanged.

## Questions and fixed comparisons

1. Compare frozen v3 with tracker Sagarin Predictor, tracker FPI, their equal
   average, and a fixed 50/50 v3/external-average blend on the same games.
2. Fit a v3-style football control on the shared archive-covered training rows.
   Compare with the same football model plus the two external projections.
3. Fit a two-feature ridge using only the external projections, to test whether
   football inputs add value beyond calibrated external ratings.
4. On the smaller qualified SP+ overlap, show SP+ and an equal four-system
   blend (v3, SP+, Sagarin Predictor, FPI). No SP+ coefficient fitting on this
   sparse reconstruction. Do not compare different cohorts as if identical.

No search over blending weights, ATS thresholds, additional rating systems,
forests or new architecture. No automatic promotion or alteration of live picks.

## Time and population

- Retrieve publisher CSVs for 2020-2021 as training warm-up. Freeze all six
  raw files, URLs, retrieval times and SHA256s. Reuse retained 2022-2025 files.
- Outer evaluation seasons: 2022, 2023, 2024, 2025. A test season is never used
  for fitting or selecting its coefficients, penalty, coach split or imputers.
- Keep existing v3 time decay, half-weight 2020, coach selection, preseason
  fade/blend and feature construction. External weekly game projections remain
  active all season in this challenger; they are NOT frozen preseason ratings.
  Their internal priors cannot be separately removed and may retain brand bias.
- Hold the matched football control's past-only coach choices fixed when
  adding external inputs. Use an isolated ridge validator with an explicit
  feature allowlist; do not weaken the production ensemble's FPI prohibition.
- Use existing penalty grid .5, 2, 8, 32 and default 8. Select using only earlier
  outer-season MAE; first fold uses the default. No ATS-driven tuning.
- Primary population: both teams FBS at the time, regular season/conference
  championship/CFP only, both external values available. Exclude non-CFP bowls
  from new fits and primary evaluation. Preserve frozen v3 as-is for comparison.
- Freeze 2025 as the latest evaluation fold, but label all retrospective
  evidence exploratory: these seasons have already informed prior model work.

## Joins and evidence

- Explicit team aliases only. Teams and season identify candidate schedule rows.
  Disambiguate repeated matchups by an independently defined calendar-week map,
  never by score, market line or forecast error. Ambiguity is rejected.
- Determine the per-season tracker week offset from unique regular matchups
  using schedule identity alone. Emit offsets, deviations and all join reasons.
  Use Eastern game dates, assigning Monday games to the preceding football
  weekend. This resolves Monday-night championship rematches without scores.
- Reverse a matchup's predictions and market signs only when canonical sides
  are reversed at a neutral venue. A non-neutral reversal is quarantined.
- Scores only verify an already-resolved identity. Score disagreement rejects
  a row; it must never pick one candidate over another.
- Remove exact duplicates with an audit trail. Conflicting prediction/score
  duplicates fail. When predictions and scores agree but a market quote differs,
  retain one forecast, mark that quote missing, and record both source rows.
  This policy was refined during source inspection, before scoring: Rutgers vs
  Temple 2021 differs on updated line; USF vs Memphis 2024 on midweek line.
- Source `linesagpred` is a home-margin projection, not a numeric team rating.
  `lineespn` is the tracker's ESPN projection. Never add HFA again. A positive
  tracker market line means home favored, opposite our handicap convention.
- Historical publisher CSVs do not carry per-game publication timestamps. The
  main lane is publisher-archived, not independently time-verified or a Friday
  executable simulation. Report independently captured Sagarin checks separately;
  those do not authenticate FPI, all training rows, or the archive's market line.
- Missing ratings are not imputed as zero. Football feature imputation/scaling
  remains training-fold-only. No raw team names, outcomes or market lines enter
  the challenger feature matrix.

## Scoring and interpretation

- Main ATS line: identical canonical historical closing handicap for all models.
  Sensitivity: tracker opening, midweek and updated lines, separately labeled;
  these do not have verified bookmaker/timestamp history. Do not call updated
  lines closing lines. No actual ROI or achievable execution-price claim.
- Report MAE, RMSE, bias, SU and ATS with wins/losses/pushes/no-picks, separately
  by season, Week 0/1, Weeks 2-4, Week 5+, CFP, neutral CFP, audience, spread size,
  Alabama/Clemson, Indiana/SMU and G5 participation. No result clipping at 21.
- CFP scores use actual round matchups and pregame inputs when supported,
  not a reconstructed perfect bracket or one pre-tournament bracket simulation.
- MAE leads; ATS second. Show paired common-game deltas, season-cluster bootstrap
  intervals (5,000 draws), leave-one-season-out stability, and small-sample
  warnings. Statistical summaries cannot repair unknown publication timing.
- Standalone external models can have broader coverage; headline comparisons
  must use the common eligible v3 cohort, never silently different denominators.

## Deliverables

One offline rerunnable command after source capture, audited joined inputs,
fold choices, per-game predictions, diagnostic tables, a readable report,
fitted experimental model artifacts and tests. A separate prediction helper
must require the fitted feature schema and explicit external inputs; it must
not silently replace the production weekly workflow.
