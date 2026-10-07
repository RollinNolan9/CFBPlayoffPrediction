# Walk-Forward Rating Baseline

A minimal, dependency-free reference model for straight-up winners and the spread.
It needs no API key: it reads the committed `cfb_v2/data/historical_games.csv`
(2020-2025 scores, CFBD pregame Elo, and historical home spreads).

```bash
Rscript run_cfb_baseline.R   # base R only, ~70 seconds
```

## Model

- Margin rating: `home_margin = hfa * !neutral + r_home - r_away + fcs_offset`,
  fit by weighted ridge least squares (SRS-style). Current-season games weigh 1,
  prior-season games 0.25.
- Refit before every kickoff date on games with kickoff strictly earlier (current and
  prior season only). A `stopifnot` enforces that no training row is on or after the
  predicted date.
- Win probability: `pnorm(pred_margin / sigma)`, where sigma is the sd of earlier
  out-of-sample residuals only.
- ATS: back the home side when `pred_margin + home_spread > 0`. Pushes excluded,
  flat -110 pricing (break-even 52.38%).
- Hyperparameters (prior-season weight, ridge penalty, margin cap) are selected on
  2021 by margin MAE only, then frozen for the 2022-2025 test. ATS is never used to tune.

## Results, 2022-2025 (3,172 lined games)

| Model | SU accuracy | Brier | Log loss | Margin MAE |
|---|---|---|---|---|
| Baseline ratings | 70.65% | 0.1871 | 0.5499 | 12.70 |
| CFBD pregame Elo (walk-forward margin map) | 70.02% | 0.1911 | 0.5612 | 12.96 |
| Closing spread (market) | 72.67% | 0.1791 | 0.5296 | 12.03 |

| ATS | Bets | Record | Rate (95% CI) | ROI at -110 |
|---|---|---|---|---|
| Baseline, all games | 3,115 | 1535-1580-57 | 49.3% (47.5-51.0) | -5.9% |
| Baseline, edge >= 3 | 1,495 | 723-772-28 | 48.4% (45.8-50.9) | -7.7% |
| Baseline, edge >= 7 | 399 | 187-212-8 | 46.9% (42.0-51.8) | -10.5% |
| CFBD Elo, all games | 3,115 | 1570-1545-57 | 50.4% (48.7-52.2) | -3.8% |

The baseline beats CFBD Elo on every winner metric but stays behind the market, and
it has no ATS edge: larger disagreements with the line lose more often, the usual
sign that the market already prices what a scores-only rating knows. Treat this as
the floor any richer model must beat on the same games.

Spread caveat: per the v3 line-provenance audit, these historical spreads come from
cached play-by-play quotes without a verified book or observation time, so they are
not guaranteed closing lines.

Per-game predictions are written to `cfb_baseline/output/predictions_2022_2025.csv`.
