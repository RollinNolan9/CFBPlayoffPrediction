# Combining opponent-relative margin and power-margin caps

Specified September 27, 2026 before fitting these combinations.

Run a 3 x 2 x 2 factorial comparison: power margin cap none/24/42; direct
source_games_diff and games_played_diff predictors retained/removed; the
opponent_relative_margin_diff predictor absent/present. These twelve models
separate the cap effect, the new feature effect, and their interaction.

Use the exact frozen historical feature rebuilds from margin_caps/20260927T211431Z.
Caps apply symmetrically to observed margins before home-field subtraction in
the opponent-adjusted power fit. Dependent coach expectations are rebuilt
consistently. Use the exact previously tested opponent-relative feature from
opponent_relative/20260927T215407Z: prior same-season FBS results, opponent
baselines frozen before each source game, mean of prior relative performances
shrunk by n/(n+3), neutral zero without usable history. This feature continues
to use RAW margins; do not add a second intervention by clipping it. Thus this
experiment limits blowout influence through power, not through every feature.
Actual targets, efficiency features, weights and genuine sample counts remain
unchanged. Only direct count predictors are removed in the no_counts factor.

Train on earlier seasons only for tests in 2022, 2023, 2024, 2025, using the same
prior-only fold choices: coach recent share 0.65, ridge penalty 8 in 2022 and 32
in 2023-2025. No hyperparameter, shrinkage, averaging-window or additional-cap
search. No random forest or preseason roster branch. Require the six previously
tested combinations to reproduce their frozen predictions within 1e-8.

Primary evaluation is the same 2,145 regular-season Week 5+ FBS-vs-FBS games.
Four primary paired MAE comparisons ask whether cap 24 or cap 42 improves on
uncapped WITH opponent-relative already present, separately with direct count
predictors retained and removed. Report 95% descriptive and 98.75% Bonferroni
season-bootstrap intervals for these four MAE comparisons. Primary candidate
selection within each count setting uses lowest 2022-2024 Week 5+ MAE among
uncapped/24/42 with the new feature, then describes 2025 separately. The tie
preference is uncapped, then 24, then 42. Previously researched 2025 is not a
pristine holdout.

Secondary comparisons show all six cap-only/no-cap controls, incremental
feature effects within each cap/count setting, combined-vs-published effects,
RMSE, SU, ATS, full-season/early-season and individual-year results. Calculate
the per-game MAE interaction as (cap+relative - uncapped+relative) minus
(cap-only - uncapped-only), holding count setting fixed. Negative interaction
means the new feature makes the cap's MAE effect more favorable; it does not
alone establish that the combination beats uncapped+relative.

Retain missing-line games for MAE/SU and original closing lines for ATS. ATS
accuracy excludes pushes and records exact-zero-edge passes. Use paired
whole-season bootstrap 10,000 draws/seed 20260927, acknowledge only four clusters,
and report leave-one-season-out primary MAE effects. Secondary slices and
interactions are exploratory. No production promotion, public-page change,
Friday-execution claim or ROI claim. Save all forecasts, models, input/code
hashes, variant specification and a readable report.
