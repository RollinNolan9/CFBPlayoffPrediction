# Controlled ATS Results

## Verdict

Neither challenger passes the pre-specified screening rules. Leave v3 unchanged;
do not promote either experiment or describe these results as a demonstrated
betting advantage. These tests neither prove past success was luck nor rule it out.

Run date: September 9, 2026. The [protocol](ATS_PROTOCOL.md) was written before
fitting these two challengers locally; it was not externally preregistered.
Training starts in 2020; outer test seasons are 2022-2025. Each fit and penalty
choice uses earlier seasons only. No 2026 outcomes were used.

## Matched Results

All models are compared on the same 3,025 lined FBS-vs-FBS games. ATS percentages
exclude 54 pushes. Non-CFP bowls are excluded. Margin MAE is in points; lower is better.

| Model | ATS W-L-P | ATS | Margin MAE |
| --- | --- | ---: | ---: |
| Frozen v3 control | 1,476-1,495-54 | 49.68% | 12.825 |
| A: FBS-only fitting | 1,471-1,500-54 | 49.51% | 12.762 |
| B: Market residual challenger | 1,506-1,465-54 | 50.69% | 12.050 |
| Closing-line margin reference | No selections | N/A | 12.034 |

A changes only the margin learner/selection population, not the cached power
construction or FCS transition priors. B learns corrections to the market margin
using the football features, spread, spread size, total, and a missing-total flag.
Missing values are imputed within training folds. Neither uses raw team identity,
FPI, polls, or PFF. Existing preseason feature fade and coach selection remain intact.

At uniform -110 pricing, the mathematical break-even rate is 52.38% of non-push
decisions. Neither challenger reaches it overall or in 2025. This is a pricing
assumption for comparison, not actual historical odds or a profit record.

| Test Season | v3 ATS | FBS-Only ATS | Market Residual ATS |
| --- | ---: | ---: | ---: |
| 2022 | 47.03% | 46.07% | 50.48% |
| 2023 | 50.68% | 51.36% | 52.17% |
| 2024 | 48.87% | 50.87% | 50.47% |
| 2025 | 52.04% | 49.67% | 49.67% |

## What We Learned

- FBS-only fitting reduces margin MAE by 0.063 points but does not improve ATS.
- The market challenger reduces MAE by 0.775 points relative to v3, but is still
  0.016 points worse than the closing-line reference. Its mean absolute correction
  to the line is just 0.559 points. This is primarily market following, not evidence
  of reliable extra information beyond the line.
- The market challenger's exact 97.5% ATS interval is 48.62%-52.76%. These intervals
  adjust for two challenger comparisons but assume independent games and do not
  account for earlier model-development searches.
- Resampling whole seasons gives a 95% interval of -1.4 to +3.0 percentage points
  for B's ATS improvement over v3. Only four test seasons limit this estimate.
- On spreads above 21 points, v3 is 46.88% ATS, A is 46.61%, and B is 48.18%.
  These experiments do not solve the large-spread problem.
- The fixed 3-point-edge slice contains only five B selections (3-2). That is not
  enough evidence to turn it into a recommended strategy.
- CFP games favor v3 descriptively: 18-10 ATS versus A's 16-12 and B's 14-14.
  Twenty-eight games are too few to establish a durable playoff advantage.

## Reproduction And Limits

From the repository root in PowerShell, with the rebuilt v3 data already present:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\run_cfb_ats_experiments.R
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' .\cfb_v2\tests\test_ats_experiments.R
```

Each execution writes a new timestamped directory under
`cfb_v3/output/experiments/controlled_ats/`, including the protocol, configuration,
source/input hashes, fold choices, game-level predictions, metrics, uncertainty,
screening results, and verification. It never promotes a model or overwrites a card.

Verified run: `20260909T182438Z`. See its [full generated report](../output/experiments/controlled_ats/20260909T182438Z/report.md)
and [verification](../output/experiments/controlled_ats/20260909T182438Z/verification.json).
Generated output/data are ignored by Git; this summary, protocol, runner, and tests
are intended to travel with the repository.

All six experiment test blocks passed. Protected engine, data, and saved prediction
files remained unchanged. The control reproduced frozen v3 margins within
`4.98e-14` points. The first run (`20260909T040956Z`) had stale duplicate grading
columns on the market-reference CSV rows; the report used correctly recomputed
columns. The verified rerun synchronizes those fields. Predictions, metrics,
fold choices, and protocol are identical between runs; no tuning followed the results.

The historical embedded cfbfastR spreads are closing-line diagnostics, not verified
Friday-publication snapshots. Preseason API backfills lack verified original
release timestamps. The 2025 season was excluded from its own fitting and tuning
but was already examined during development, so it is not a pristine research
holdout. These limitations prevent treating the backtest as a prospective record.

Next: preserve these failed experiments, maintain a frozen future publication
ledger with model version, cutoff, available line/price, selection, and outcome,
and evaluate every published pick consistently. Any further model change needs a
new hypothesis and evaluation design, not thresholds chosen to rescue these results.
