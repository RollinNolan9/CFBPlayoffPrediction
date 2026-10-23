# Earlier historical-weight decay: controlled retrospective experiment

Frozen before results on October 12, 2026. Test the existing uncapped, opponent-relative 19-feature ranking/prediction family, with both direct game-count predictors excluded. No production or published ranking changes.

Only score-power and explicit prior-season/trailing-three-year inputs change. Coaching features, including their existing power-based expectations, remain frozen. This is a controlled feature experiment, not a rebuild of the entire coaching pipeline. A deployment that also rebuilds those coaching inputs needs its own validation.

## Prespecified schedules

Each number is a maximum historical share of total weighted game mass in the score-power fit, after original age decay, FCS weighting and non-CFP bowl exclusion. It is also a maximum scaling factor for each team's explicit prior/trailing inputs. Those quantities are not percentages of the final model or per-team guarantees. Apply by calendar phase week, not chronological model week.

| Variant | Week 0-1 | 2 | 3 | 4 | 5 | 6 | 7 | 8+ / postseason |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| baseline | Existing | Existing | Existing | Existing | Existing | Existing | Existing | 10% |
| early_week5 | Existing | Existing | Existing | Existing | 10% | 10% | 10% | 10% |
| smooth_moderate | Existing | 60% | 45% | 30% | 20% | 15% | 10% | 10% |
| smooth_fast | Existing | 50% | 30% | 20% | 10% | 10% | 10% | 10% |
| current_only_week5 | Existing | Existing | Existing | Existing | 0% | 0% | 0% | 0% |

The zero-history variant still retains historical coaching inputs. Existing per-game decay remains, with a global cap applied only when it binds; no double per-game historical scaling. If no current-season positive-weight games exist, defer the experimental power cap and log the case. Retain original direct-prior missingness.

## Validation

- Refitted chronological folds: train on earlier seasons only, test 2022, 2023, 2024 and 2025. Training-only imputation and standardization.
- Reuse frozen prior-only fold choices: ridge lambda 8 for 2022, 32 thereafter, coaching share .65. No challenger retuning. Current 2026 fits use lambda 32 and all 2020-2025 training rows.
- Baseline historical forecasts must exactly replay the saved no-counts opponent-relative experiment; current ratings must replay the October 11 snapshot within 1e-8. Rebuilt baseline power and direct-history features must match cached inputs.
- Preflight found five Labor Day Monday Week 1 games sharing a chronological power snapshot whose maximum calendar week is 2. Preserve those rows' baseline Week 1 side-power inputs explicitly, honoring the declared Weeks 0-1 policy. The FBS-transition bridge is reapplied as in the original training pipeline. These fixes were made before challenger metrics were calculated.
- Original targets, closing lines, cohort identities, weights, EPA, coaching and opponent-relative features remain unchanged. Audit earlier-week/kickoff feature cutoffs and source hashes.
- Primary development selection: lowest MAE on regular FBS Weeks 2-7 of 2022-2024. Separately examine 2025 confirmation. All these seasons have been researched before; none is a pristine new holdout.
- Report Weeks 0-1, 2-4, 5-7, 8+, all regular FBS, each season and current ranking sensitivity. Do not choose by Oklahoma State/Kentucky positions or AP agreement.
- ATS is secondary, uses identical frozen closing lines, excludes pushes from win percentage, distinguishes no-selection and missing lines, and uses common decisions for paired changes.
- 10,000 paired season-week cluster resamples, weighted by game counts, fixed seed 20260927: descriptive 95% and 98.75% Bonferroni intervals for four contrasts. Also report whole-season 95% intervals and leave-one-season-out sensitivity; weekly clustering does not remove cross-week team dependence.
- Evidence gates, not automatic deployment: development MAE improves at least 0.10 points, development adjusted interval upper bound below zero, 2025 primary MAE does not worsen, all-regular pooled MAE does not worsen, and primary MAE improves in at least 3 of 4 seasons.

Current-ranking comparisons refit each variant consistently, use the same October 11 completed games and fixed other inputs, and are diagnostics rather than forecasts of Week 8 results.
