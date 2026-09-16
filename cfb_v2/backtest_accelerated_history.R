args <- commandArgs(trailingOnly = TRUE)
if (length(args)) stop("This experiment takes no arguments.", call. = FALSE)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_arg)) {
  dirname(dirname(normalizePath(
    sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE
  )))
} else normalizePath(getwd(), winslash = "/", mustWork = TRUE)

for (file in c(
  "config.R", "store.R", "features.R", "coaches.R", "adapters.R",
  "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
  "bridge.R", "workflow.R"
)) source(file.path(project_dir, "cfb_v2", file))

options(warn = 1)
production_config <- cfb_v2_config(project_dir, 2026L)
training_path <- file.path(production_config$inbox_dir, "training_games.csv")
games_path <- file.path(production_config$data_dir, "historical_games.csv")
baseline_path <- file.path(
  production_config$output_dir, "backtest", "rolling_predictions.csv"
)
for (path in c(training_path, games_path, baseline_path)) {
  if (!file.exists(path)) stop("Missing required cache: ", path, call. = FALSE)
}

read_model_csv <- function(path) {
  utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA", "N/A", "null")
  )
}

rebuild_history_columns <- function(training, games, config) {
  assert_columns(
    training,
    c("home", "away", "season", "model_week", "phase_week",
      "home_games_played", "away_games_played", "home_prior_season",
      "away_prior_season", "home_trailing_3yr", "away_trailing_3yr"),
    "cached training games"
  )
  power <- power_rating_snapshots(games, config)
  assert_unique_keys(power, c("team", "season", "model_week"),
                     "reconstructed power ratings")
  power_key <- paste(power$team, power$season, power$model_week, sep = "\r")
  home_key <- paste(canonical_team(training$home), training$season,
                    training$model_week, sep = "\r")
  away_key <- paste(canonical_team(training$away), training$season,
                    training$model_week, sep = "\r")
  home_index <- match(home_key, power_key)
  away_index <- match(away_key, power_key)
  home_power <- power$power_rating[home_index]
  away_power <- power$power_rating[away_index]
  home_power[!is.finite(home_power)] <- 0
  away_power[!is.finite(away_power)] <- 0

  history_weight <- rowMeans(cbind(
    prior_season_feature_weight(
      training$phase_week, training$home_games_played, config
    ),
    prior_season_feature_weight(
      training$phase_week, training$away_games_played, config
    )
  ))
  training$prior_history_weight <- history_weight
  training$prior_season_diff <-
    (training$home_prior_season - training$away_prior_season) * history_weight
  training$trailing_3yr_diff <-
    (training$home_trailing_3yr - training$away_trailing_3yr) * history_weight
  training$home_power <- home_power
  training$away_power <- away_power
  training$home_power_rating <- home_power
  training$away_power_rating <- away_power
  training$power_rating_diff <- home_power - away_power
  training$pregame_expected_margin <-
    training$power_rating_diff + training$home_field_points
  training
}

max_column_delta <- function(expected, rebuilt, column) {
  expected_value <- as.numeric(expected[[column]])
  rebuilt_value <- as.numeric(rebuilt[[column]])
  if (any(xor(is.na(expected_value), is.na(rebuilt_value)))) {
    stop("Reconstruction changed missingness for ", column, call. = FALSE)
  }
  delta <- abs(expected_value - rebuilt_value)
  delta <- delta[is.finite(delta)]
  if (length(delta)) max(delta) else 0
}

training <- read_model_csv(training_path)
games <- read_model_csv(games_path)
production_rebuilt <- rebuild_history_columns(training, games, production_config)
reconstructed_columns <- c(
  "prior_history_weight", "prior_season_diff", "trailing_3yr_diff",
  "power_rating_diff"
)
reconstruction <- data.frame(
  feature = reconstructed_columns,
  max_absolute_delta = vapply(
    reconstructed_columns,
    function(column) max_column_delta(training, production_rebuilt, column),
    numeric(1)
  ),
  stringsAsFactors = FALSE
)
if (any(reconstruction$max_absolute_delta > 1e-8)) {
  stop(
    "Production reconstruction did not match the cached foundation: ",
    paste(
      reconstruction$feature[reconstruction$max_absolute_delta > 1e-8],
      collapse = ", "
    ),
    call. = FALSE
  )
}

challenger_config <- production_config
challenger_config$phase$prior_season_cap_after_week <- 5L
challenger_training <- rebuild_history_columns(training, games, challenger_config)

experiment_dir <- file.path(
  production_config$output_dir, "experiments", "accelerated_history_cap_week5"
)
experiment_inbox <- file.path(experiment_dir, "inbox")
experiment_output <- file.path(experiment_dir, "model_output")
dir.create(experiment_inbox, recursive = TRUE, showWarnings = FALSE)
dir.create(experiment_output, recursive = TRUE, showWarnings = FALSE)
challenger_training_path <- file.path(experiment_inbox, "training_games.csv")
utils::write.csv(challenger_training, challenger_training_path,
                 row.names = FALSE, na = "")

challenger_config$inbox_dir <- experiment_inbox
challenger_config$output_dir <- experiment_output
challenger_result <- run_v2_backtest(challenger_config)

baseline <- read_model_csv(baseline_path)
challenger <- read_model_csv(challenger_result$predictions)
baseline <- baseline[baseline$model == "ridge_core", , drop = FALSE]
challenger <- challenger[challenger$model == "ridge_core", , drop = FALSE]
assert_unique_keys(baseline, c("game_id", "season"), "production predictions")
assert_unique_keys(challenger, c("game_id", "season"), "challenger predictions")

comparison <- merge(
  baseline, challenger,
  by = c("game_id", "season"), suffixes = c("_production", "_challenger"),
  all = TRUE, sort = FALSE
)
if (nrow(comparison) != nrow(baseline) ||
    any(is.na(comparison$expected_margin_production)) ||
    any(is.na(comparison$expected_margin_challenger))) {
  stop("Production and challenger prediction folds did not align.", call. = FALSE)
}

comparison$production_absolute_error <- abs(
  comparison$actual_margin_production - comparison$expected_margin_production
)
comparison$challenger_absolute_error <- abs(
  comparison$actual_margin_challenger - comparison$expected_margin_challenger
)
comparison$paired_absolute_error_delta <-
  comparison$challenger_absolute_error - comparison$production_absolute_error

paired_interval <- function(delta, seed = 20260907L, replicates = 10000L) {
  delta <- delta[is.finite(delta)]
  if (!length(delta)) return(c(low = NA_real_, high = NA_real_))
  set.seed(seed)
  means <- replicate(
    replicates, mean(sample(delta, length(delta), replace = TRUE))
  )
  stats::quantile(means, c(.025, .975), names = FALSE, na.rm = TRUE) |>
    stats::setNames(c("low", "high"))
}

metric_row <- function(data, keep, slice) {
  x <- data[keep & !is.na(keep), , drop = FALSE]
  interval <- paired_interval(x$paired_absolute_error_delta)
  production_ats <- as.logical(x$ats_correct_production)
  challenger_ats <- as.logical(x$ats_correct_challenger)
  data.frame(
    slice = slice,
    games = nrow(x),
    production_mae = mean(x$production_absolute_error, na.rm = TRUE),
    challenger_mae = mean(x$challenger_absolute_error, na.rm = TRUE),
    mae_delta = mean(x$paired_absolute_error_delta, na.rm = TRUE),
    delta_ci_low = interval[["low"]],
    delta_ci_high = interval[["high"]],
    production_su = mean(as.logical(x$winner_correct_production), na.rm = TRUE),
    challenger_su = mean(as.logical(x$winner_correct_challenger), na.rm = TRUE),
    production_ats = mean(production_ats, na.rm = TRUE),
    challenger_ats = mean(challenger_ats, na.rm = TRUE),
    ats_graded = sum(!is.na(production_ats) & !is.na(challenger_ats)),
    stringsAsFactors = FALSE
  )
}

week <- as.integer(comparison$week_production)
regular <- comparison$postseason_type_production == "regular"
metrics <- do.call(rbind, list(
  metric_row(comparison, rep(TRUE, nrow(comparison)), "all_weighted_games"),
  metric_row(comparison, regular & week >= 5L & week <= 7L, "regular_weeks_5_7"),
  metric_row(comparison, regular & week == 5L, "regular_week_5"),
  metric_row(comparison, regular & week == 6L, "regular_week_6"),
  metric_row(comparison, regular & week == 7L, "regular_week_7"),
  metric_row(comparison, regular & week >= 8L, "regular_week_8_plus"),
  metric_row(comparison, !regular, "postseason"),
  metric_row(comparison, comparison$season >= 2023L, "seasons_2023_2025")
))

fold_rows <- lapply(sort(unique(comparison$season)), function(season) {
  keep <- comparison$season == season & regular & week >= 5L & week <= 7L
  metric_row(comparison, keep, paste0("season_", season, "_weeks_5_7"))
})
folds <- do.call(rbind, fold_rows)

ats_pair <- regular & week >= 5L & week <= 7L &
  !is.na(comparison$ats_correct_production) &
  !is.na(comparison$ats_correct_challenger)
ats_table <- table(
  production = as.logical(comparison$ats_correct_production[ats_pair]),
  challenger = as.logical(comparison$ats_correct_challenger[ats_pair])
)
ats_mcnemar_p <- if (all(dim(ats_table) == c(2L, 2L))) {
  stats::mcnemar.test(ats_table, correct = TRUE)$p.value
} else NA_real_
ats_net_wins <- sum(as.logical(comparison$ats_correct_challenger[ats_pair])) -
  sum(as.logical(comparison$ats_correct_production[ats_pair]))

diagnostic_keep <-
  (comparison$season == 2024L &
     (comparison$home_production == "Florida State" |
        comparison$away_production == "Florida State")) |
  (comparison$season == 2025L &
     (comparison$home_production == "Penn State" |
        comparison$away_production == "Penn State"))
diagnostics <- comparison[diagnostic_keep & regular & week >= 5L & week <= 7L, ]
diagnostic_team <- ifelse(diagnostics$season == 2024L, "Florida State", "Penn State")
team_sign <- ifelse(diagnostics$home_production == diagnostic_team, 1, -1)
diagnostics <- data.frame(
  season = diagnostics$season,
  week = diagnostics$week_production,
  team = diagnostic_team,
  opponent = ifelse(
    team_sign == 1, diagnostics$away_production, diagnostics$home_production
  ),
  production_margin = team_sign * diagnostics$expected_margin_production,
  challenger_margin = team_sign * diagnostics$expected_margin_challenger,
  actual_margin = team_sign * diagnostics$actual_margin_production,
  production_absolute_error = diagnostics$production_absolute_error,
  challenger_absolute_error = diagnostics$challenger_absolute_error,
  stringsAsFactors = FALSE
)

week57 <- metrics[metrics$slice == "regular_weeks_5_7", ]
overall <- metrics[metrics$slice == "all_weighted_games", ]
folds_improved <- sum(folds$mae_delta < 0)
promote <- is.finite(week57$mae_delta) && week57$mae_delta <= -0.10 &&
  week57$delta_ci_high < 0 && overall$mae_delta <= 0 &&
  folds_improved >= ceiling(nrow(folds) * .75)
verdict <- if (promote) {
  "PROMOTE: challenger cleared the predeclared margin and fold-consistency gates."
} else {
  "RETAIN 2.1.0: challenger did not clear the predeclared promotion gates."
}

format_metric_table <- function(data) {
  header <- c(
    "| Slice | Games | Production MAE | Challenger MAE | Delta | 95% paired CI | Production SU | Challenger SU | Production ATS | Challenger ATS |",
    "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
  )
  rows <- apply(data, 1, function(row) {
    sprintf(
      "| %s | %d | %.3f | %.3f | %+.3f | [%+.3f, %+.3f] | %.1f%% | %.1f%% | %.1f%% | %.1f%% |",
      row[["slice"]], as.integer(row[["games"]]),
      as.numeric(row[["production_mae"]]), as.numeric(row[["challenger_mae"]]),
      as.numeric(row[["mae_delta"]]), as.numeric(row[["delta_ci_low"]]),
      as.numeric(row[["delta_ci_high"]]), 100 * as.numeric(row[["production_su"]]),
      100 * as.numeric(row[["challenger_su"]]),
      100 * as.numeric(row[["production_ats"]]),
      100 * as.numeric(row[["challenger_ats"]])
    )
  })
  c(header, rows)
}

diagnostic_lines <- if (nrow(diagnostics)) {
  c(
    "| Season | Week | Team | Opponent | Production margin | Challenger margin | Actual margin | Production AE | Challenger AE |",
    "|---:|---:|---|---|---:|---:|---:|---:|---:|",
    apply(diagnostics, 1, function(row) sprintf(
      "| %d | %d | %s | %s | %+.1f | %+.1f | %+.1f | %.1f | %.1f |",
      as.integer(row[["season"]]), as.integer(row[["week"]]), row[["team"]],
      row[["opponent"]], as.numeric(row[["production_margin"]]),
      as.numeric(row[["challenger_margin"]]), as.numeric(row[["actual_margin"]]),
      as.numeric(row[["production_absolute_error"]]),
      as.numeric(row[["challenger_absolute_error"]])
    ))
  )
} else "No diagnostic games matched."

report <- c(
  "# Accelerated History Challenger Backtest",
  "",
  paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("Production model: `", production_config$version, "` (unchanged)."),
  "Challenger: cap prior-team history at 10% beginning in Week 5 instead of Week 8.",
  "Affected opponent-adjusted power snapshots and explicit prior/trailing features were rebuilt from cached games before rolling validation.",
  "Negative MAE delta favors the challenger. ATS is diagnostic only.",
  "",
  "## Verdict",
  "",
  paste0("**", verdict, "**"),
  "",
  "Promotion required at least a 0.10-point Weeks 5-7 MAE gain, a paired-bootstrap 95% interval below zero, no overall MAE regression, and improvement in at least 75% of season folds.",
  paste0("The challenger added ", ats_net_wins, " ATS wins across ",
         sum(ats_pair), " paired Weeks 5-7 games; McNemar p-value `",
         sprintf("%.3f", ats_mcnemar_p), "`, which does not establish an ATS improvement."),
  "",
  "## Reconstruction Check",
  "",
  "Rebuilding with production settings had to match every cached affected feature within `1e-8`.",
  "",
  paste0("- Maximum reconstruction delta: `",
         format(max(reconstruction$max_absolute_delta), scientific = TRUE), "`."),
  "",
  "## Primary Metrics",
  "",
  format_metric_table(metrics),
  "",
  "## Weeks 5-7 Season Folds",
  "",
  format_metric_table(folds),
  "",
  "## Collapse Diagnostics",
  "",
  diagnostic_lines
)

comparison_path <- file.path(experiment_dir, "paired_predictions.csv")
metrics_path <- file.path(experiment_dir, "metrics.csv")
folds_path <- file.path(experiment_dir, "season_folds.csv")
diagnostics_path <- file.path(experiment_dir, "collapse_diagnostics.csv")
report_path <- file.path(experiment_dir, "report.md")
utils::write.csv(comparison, comparison_path, row.names = FALSE, na = "")
utils::write.csv(metrics, metrics_path, row.names = FALSE, na = "")
utils::write.csv(folds, folds_path, row.names = FALSE, na = "")
utils::write.csv(diagnostics, diagnostics_path, row.names = FALSE, na = "")
writeLines(report, report_path, useBytes = TRUE)

cat(verdict, "\n")
print(metrics, row.names = FALSE)
cat("Season folds improved:", folds_improved, "of", nrow(folds), "\n")
cat("Report:", report_path, "\n")
