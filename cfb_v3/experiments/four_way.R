four_way_features <- function() {
  list(elo_baseline = c("challenger_pregame_elo_diff", "home_field_points"),
    internal_power = c("power_rating_diff", "home_field_points"),
    small_football = c("power_rating_diff", "offense_epa_diff", "defense_epa_diff", "home_field_points"))
}

four_way_cohort <- function(data, weights, games) {
  assert_unique_keys(data, "game_id", "four-way training data")
  assert_unique_keys(games, "game_id", "four-way Elo source")
  assert_columns(games, c("game_id", "season", "home", "away", "margin", "kickoff",
    "feature_week_start", "home_pregame_elo", "away_pregame_elo"), "four-way Elo source")
  stopifnot(length(weights) == nrow(data))
  index <- match(data$game_id, games$game_id)
  if (anyNA(index)) stop("Training rows lack an Elo source game.")
  source <- games[index, ]
  if (!all(data$season == source$season) ||
      !identical(canonical_team(data$home), canonical_team(source$home)) ||
      !identical(canonical_team(data$away), canonical_team(source$away)) ||
      !isTRUE(all.equal(data$margin, source$margin, tolerance = 1e-12, check.attributes = FALSE)))
    stop("Elo source game metadata does not match training data.")
  for (side in c("home", "away")) {
    column <- paste0(side, "_pregame_elo")
    if (column %in% names(data) &&
        !isTRUE(all.equal(data[[column]], source[[column]], check.attributes = FALSE)))
      stop("Cached pregame Elo disagrees between inputs.")
  }
  kickoff <- parse_utc_datetime(source$kickoff)
  cutoff <- as.POSIXct(as.Date(source$feature_week_start), tz = "UTC")
  all_kickoff <- parse_utc_datetime(games$kickoff)
  team_rows <- rbind(data.frame(row = seq_len(nrow(games)), team = canonical_team(games$home), season = games$season),
    data.frame(row = seq_len(nrow(games)), team = canonical_team(games$away), season = games$season))
  groups <- split(team_rows$row, paste(team_rows$season, team_rows$team, sep = "\r"))
  timing <- !is.na(kickoff) & !is.na(cutoff) & cutoff < kickoff
  for (i in which(timing)) for (side in c("home", "away")) {
    rows <- groups[[paste(data$season[i], canonical_team(data[[side]][i]), sep = "\r")]]
    dates <- all_kickoff[rows]
    same_season_other <- games$game_id[rows] != data$game_id[i]
    if (any(is.na(dates[same_season_other])) ||
        any(dates >= cutoff[i] & dates < kickoff[i], na.rm = TRUE)) timing[i] <- FALSE
  }
  elo <- is.finite(source$home_pregame_elo) & is.finite(source$away_pregame_elo)
  fbs <- raw_history_eligible(data)
  keep <- fbs & elo & timing & is.finite(weights) & weights > 0
  reason <- ifelse(!fbs, "not_eligible_fbs", ifelse(!elo, "missing_pregame_elo",
    ifelse(!timing, "cutoff_or_same_cycle_game", ifelse(!is.finite(weights) | weights <= 0,
    "zero_training_weight", "included"))))
  coverage <- data.frame(game_id = data$game_id, season = data$season, home = data$home,
    away = data$away, cutoff = format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    kickoff = format(kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    elo_present = elo, timing_passed = timing, reason = reason,
    line_present = is.finite(data$closing_home_spread),
    offense_epa_missing = !is.finite(data$offense_epa_diff),
    defense_epa_missing = !is.finite(data$defense_epa_diff))
  data$challenger_pregame_elo_diff <- source$home_pregame_elo-source$away_pregame_elo
  stopifnot(!"challenger_pregame_elo_diff" %in% football_feature_names(data))
  list(data = data[keep, , drop = FALSE], weights = weights[keep], coverage = coverage)
}

four_way_fit_small <- function(data, weights, features, name, config) {
  allowed <- four_way_features()[[name]]
  if (!identical(features, allowed)) stop("Unexpected baseline feature set.")
  validation <- rolling_validate_ensemble(data, features = features, weights = weights,
    config = config, fit_nonlinear = FALSE)
  predictions <- build_v2_backtest_predictions(data, validation, name)
  choices <- do.call(rbind, lapply(sort(unique(predictions$season)), function(season) {
    lambda <- unique(predictions$lambda[predictions$season == season])
    stopifnot(length(lambda) == 1L)
    data.frame(model = name, test_season = season, train_through_season = max(data$season[data$season < season]),
      training_rows = sum(data$season < season), coach_recent_share = NA_real_,
      foundation_lambda = lambda, preseason_lambda = NA_real_)
  }))
  list(predictions = predictions, choices = choices, validation = validation)
}

four_way_match <- function(predictions, ids, reference) {
  assert_unique_keys(reference, "game_id", "four-way scoring reference")
  stopifnot(!anyDuplicated(ids), all(ids %in% reference$game_id))
  expected <- reference[match(ids, reference$game_id), ]
  parts <- lapply(predictions, function(x) {
    assert_unique_keys(x, "game_id", "four-way candidate predictions")
    index <- match(ids, x$game_id)
    if (anyNA(index)) stop("A candidate is missing common evaluation rows.")
    x <- x[index, ]
    for (column in c("season", "home", "away", "actual_margin", "closing_home_spread"))
      if (!isTRUE(all.equal(x[[column]], expected[[column]], check.attributes = FALSE)))
        stop("Scoring metadata mismatch: ", column)
    if (any(!is.finite(x$expected_margin))) stop("Nonfinite common prediction.")
    x
  })
  experiment_grade(do.call(rbind, parts))
}

four_way_metrics <- function(predictions) {
  sides <- p4_or_independent_sides(predictions)
  spread <- abs(predictions$closing_home_spread)
  regular <- predictions$postseason_type == "regular"
  slices <- list(all_fbs = rep(TRUE, nrow(predictions)), lined_games = is.finite(spread),
    week_0_1 = regular & predictions$week <= 1L,
    weeks_2_4 = regular & predictions$week >= 2L & predictions$week <= 4L,
    week_5_plus = regular & predictions$week >= 5L,
    cfp = predictions$is_cfp, neutral_cfp = predictions$is_cfp & predictions$neutral_site,
    conference_championship = predictions$postseason_type == "conference_championship",
    article_audience = sides$home | sides$away,
    spread_0_7 = spread <= 7, spread_over_7_to_21 = spread > 7 & spread <= 21,
    spread_over_21 = spread > 21)
  for (season in sort(unique(predictions$season))) slices[[paste0("season_", season)]] <- predictions$season == season
  rows <- list()
  for (model in unique(predictions$model)) for (slice in names(slices)) {
    x <- predictions[which(predictions$model == model & slices[[slice]]), ]
    n <- nrow(x); decisions <- sum(x$win | x$loss)
    error <- x$expected_margin-x$actual_margin
    rows[[paste(model, slice)]] <- data.frame(model = model, slice = slice, games = n,
      lined_games = sum(is.finite(x$closing_home_spread)),
      wins = sum(x$win), losses = sum(x$loss), pushes = sum(x$push), no_selection = sum(!x$selected),
      ats_accuracy = if (decisions) sum(x$win)/decisions else NA_real_,
      su_wins = sum(x$su_correct), su_accuracy = if (n) mean(x$su_correct) else NA_real_,
      margin_mae = if (n) mean(abs(error)) else NA_real_,
      margin_rmse = if (n) sqrt(mean(error^2)) else NA_real_, margin_bias = if (n) mean(error) else NA_real_,
      mean_absolute_prediction = if (n) mean(abs(x$expected_margin)) else NA_real_)
  }
  do.call(rbind, rows)
}

four_way_intervals <- function(delta, seasons, draws = 5000L, seed = 20260909L) {
  keep <- is.finite(delta) & is.finite(seasons)
  groups <- split(delta[keep], seasons[keep])
  if (length(groups) < 2L) return(c(low = NA, high = NA, adjusted_low = NA, adjusted_high = NA))
  totals <- vapply(groups, sum, numeric(1)); counts <- lengths(groups)
  set.seed(seed)
  boot <- replicate(draws, {
    take <- sample(seq_along(groups), replace = TRUE)
    sum(totals[take])/sum(counts[take])
  })
  setNames(as.numeric(quantile(boot, c(.025, .975, .05/6, 1-.05/6))),
    c("low", "high", "adjusted_low", "adjusted_high"))
}

four_way_paired <- function(predictions) {
  reference <- predictions[predictions$model == "v3_matched", ]
  rows <- list(); omissions <- list()
  for (name in names(four_way_features())) {
    x <- predictions[predictions$model == name, ]
    control <- reference[match(x$game_id, reference$game_id), ]
    stopifnot(identical(x$game_id, control$game_id))
    delta <- x$mae_error-control$mae_error
    ats <- x$ats_correct-control$ats_correct
    ci <- four_way_intervals(delta, x$season); ats_ci <- four_way_intervals(ats, x$season)
    loso <- lapply(sort(unique(x$season)), function(season) {
      keep <- x$season != season
      data.frame(model = name, omitted_season = season, games = sum(keep),
        mae_change = mean(delta[keep]), ats_change = mean(ats[keep], na.rm = TRUE))
    })
    loso <- do.call(rbind, loso); omissions[[name]] <- loso
    rows[[name]] <- data.frame(model = name, reference = "v3_matched", games = nrow(x),
      mae_change = mean(delta), mae_low = ci["low"], mae_high = ci["high"],
      mae_adjusted_low = ci["adjusted_low"], mae_adjusted_high = ci["adjusted_high"],
      ats_change = mean(ats, na.rm = TRUE), ats_low = ats_ci["low"], ats_high = ats_ci["high"],
      margin_screen_passed = is.finite(ci["adjusted_high"]) && ci["adjusted_high"] < 0 && all(loso$mae_change < 0))
  }
  list(paired = do.call(rbind, rows), leave_one_season_out = do.call(rbind, omissions))
}

run_four_way_experiment <- function(config) {
  stopifnot(identical(config$version, "3.0.0"), identical(config$model$ridge_lambda_grid, c(.5, 2, 8, 32)),
    config$model$ridge_lambda_default == 8)
  protected <- experiment_file_hashes(config)
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  output <- file.path(config$output_dir, "experiments", "four_way", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (dir.exists(output)) stop("Refusing to overwrite an experiment run.")
  dir.create(output, recursive = TRUE)
  message("Four-way output: ", output)
  file.copy(file.path(root, "FOUR_WAY_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  code <- c(file.path(root, c("FOUR_WAY_PROTOCOL.md", "four_way.R", "controlled_ats.R")),
    file.path(config$project_dir, "run_cfb_four_way.R"))
  jsonlite::write_json(list(status = "started", version = config$version, code_md5 = as.list(tools::md5sum(code)),
    features = four_way_features(), bootstrap_draws = 5000L, bootstrap_seed = 20260909L,
    automatic_promotion = FALSE), file.path(output, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
  saveRDS(config, file.path(output, "config.rds"))
  write_foundation_csv(protected, file.path(output, "protected_before.csv"))

  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  training <- training[training$season <= 2025, ]
  stopifnot(all(training$feature_version == config$version))
  validation <- prepare_training_data(training, config)
  members <- combine_membership(read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  validation$data <- apply_fbs_bridge_backtest_features(validation$data,
    team_games[team_games$season <= 2025, ], members[members$season <= 2025, ], config)
  priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
  priors <- priors[priors$season <= 2025, ]
  validation$data <- attach_preseason_features(validation$data, priors, config,
    features = config$preseason$production_features, prefix = "ps_")
  covered <- sort(unique(priors$season))
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  common <- four_way_cohort(validation$data, validation$weights, games[games$season <= 2025, ])
  message("Common fitting population: ", nrow(common$data), " of ", nrow(validation$data), " rows")
  write_foundation_csv(common$coverage, file.path(output, "cohort_coverage.csv"))
  counts <- as.data.frame(xtabs(~ season + reason, common$coverage))
  write_foundation_csv(counts[counts$Freq > 0, ], file.path(output, "cohort_counts.csv"))
  saveRDS(common, file.path(output, "common_inputs.rds"))
  selected_features <- unique(unlist(four_way_features()))
  write_foundation_csv(common$data[c("game_id", "season", "week", "home", "away", "margin", selected_features)],
    file.path(output, "baseline_inputs.csv"))

  message("Reproducing unchanged v3 on its original training population")
  control <- experiment_football(validation$data, validation$weights, config, covered, "v3_control")
  frozen <- read_csv_if_present(file.path(config$output_dir, "backtest", "rolling_predictions.csv"), TRUE)
  frozen <- frozen[frozen$model == "ridge_core", ]
  assert_unique_keys(frozen, "game_id", "frozen v3")
  idx <- match(control$predictions$game_id, frozen$game_id)
  stopifnot(nrow(frozen) == nrow(control$predictions), !anyNA(idx))
  control_delta <- max(abs(control$predictions$expected_margin-frozen$expected_margin[idx]))
  if (control_delta > 1e-8) stop("Current v3 no longer reproduces its frozen reference.")
  message("V3 reference parity: ", format(control_delta))

  fits <- list()
  for (name in names(four_way_features())) {
    message("Fitting ", name)
    fits[[name]] <- four_way_fit_small(common$data, common$weights, four_way_features()[[name]], name, config)
    saveRDS(fits[[name]], file.path(output, paste0(name, "_validation.rds")))
  }
  message("Fitting current v3 features on the common training population")
  full <- experiment_football(common$data, common$weights, config, covered, "v3_matched")
  saveRDS(full, file.path(output, "v3_matched_validation.rds"))
  same_as_previous_fbs <- setequal(common$data$game_id,
    validation$data$game_id[raw_history_eligible(validation$data)])
  fbs_delta <- NA_real_
  previous_path <- file.path(config$output_dir, "experiments", "controlled_ats", "20260909T182438Z", "experiment_predictions.csv")
  if (same_as_previous_fbs && file.exists(previous_path)) {
    previous <- read_csv_if_present(previous_path, TRUE)
    previous <- previous[previous$model == "fbs_only", ]
    idx <- match(previous$game_id, full$predictions$game_id)
    stopifnot(!anyNA(idx), !anyDuplicated(previous$game_id))
    fbs_delta <- max(abs(previous$expected_margin-full$predictions$expected_margin[idx]))
    if (fbs_delta > 1e-8) stop("Matched v3 no longer reproduces the prior FBS-only fit.")
  }
  ids <- full$predictions$game_id
  predictions <- four_way_match(c(lapply(fits, `[[`, "predictions"),
    list(full$predictions, control$predictions)), ids, control$predictions)
  market <- predictions[predictions$model == "v3_control" & is.finite(predictions$closing_home_spread), ]
  market$model <- "closing_line_reference"; market$expected_margin <- -market$closing_home_spread
  market <- experiment_grade(market)
  predictions <- rbind(predictions, market)
  metrics <- four_way_metrics(predictions)
  paired <- four_way_paired(predictions)
  choices <- do.call(rbind, c(lapply(fits, `[[`, "choices"), list(full$choices, control$choices)))
  stopifnot(all(choices$train_through_season < choices$test_season))
  feature_manifest <- do.call(rbind, c(lapply(names(four_way_features()), function(name)
    data.frame(model = name, feature = four_way_features()[[name]])),
    list(data.frame(model = "v3_matched", feature = full$coach$features))))
  write_foundation_csv(feature_manifest, file.path(output, "feature_manifest.csv"))
  write_foundation_csv(predictions, file.path(output, "predictions.csv"))
  write_foundation_csv(metrics, file.path(output, "metrics.csv"))
  write_foundation_csv(paired$paired, file.path(output, "paired_comparisons.csv"))
  write_foundation_csv(paired$leave_one_season_out, file.path(output, "leave_one_season_out.csv"))
  write_foundation_csv(choices, file.path(output, "fold_choices.csv"))
  summary <- metrics[metrics$slice == "all_fbs", ]
  write_foundation_csv(summary, file.path(output, "summary.csv"))
  after <- experiment_file_hashes(config)
  write_foundation_csv(after, file.path(output, "protected_after.csv"))
  if (!identical(protected, after)) stop("Protected production files changed during experiment.")
  jsonlite::write_json(list(status = "complete", production_unchanged = TRUE,
    control_max_delta = control_delta, matched_fbs_max_delta = fbs_delta,
    common_fitting_rows = nrow(common$data), common_test_games = length(ids),
    common_lined_test_games = nrow(market), same_as_previous_fbs_population = same_as_previous_fbs,
    source_files_md5 = as.list(tools::md5sum(code))), file.path(output, "verification.json"), pretty = TRUE, auto_unbox = TRUE)
  display <- function(x) {
    x$ats_accuracy <- round(100*x$ats_accuracy, 2)
    x$su_accuracy <- round(100*x$su_accuracy, 2)
    x$margin_mae <- round(x$margin_mae, 3)
    x <- x[c("model", "slice", "games", "wins", "losses", "pushes", "ats_accuracy", "su_accuracy", "margin_mae")]
    names(x)[names(x) == "ats_accuracy"] <- "ats_pct"
    names(x)[names(x) == "su_accuracy"] <- "su_pct"
    markdown_table(x)
  }
  report <- file.path(output, "REPORT.md")
  writeLines(c("# Four-Way Team-Strength Results", "",
    "Controlled feature comparison: common FBS fitting population. Current v3 and closing lines are separate references.", "",
    display(summary), "", "## Paired Deltas Versus Matched V3", "", markdown_table(paired$paired), "",
    "Negative MAE deltas favor the challenger. The margin screen uses the three-comparison adjusted interval and leave-one-season-out stability.",
    "Four season clusters and repeated prior use of these outcomes limit inference. Passing the margin screen does not establish a betting edge or authorize promotion.", "",
    "## Seasons", "", display(metrics[grepl("^season_", metrics$slice), ]), "",
    "## Diagnostic Slices", "", display(metrics[!grepl("^season_", metrics$slice) & metrics$slice != "all_fbs", ]), "",
    "CFP scores predict actual round matchups; these are NOT frozen whole-bracket scores. Closing-line ATS is retrospective, not Friday execution.", "",
    "## Verification", "", paste("Current v3 reproduction max delta:", format(control_delta)),
    paste("Prior matched FBS-only reproduction max delta:", format(fbs_delta)),
    paste("Common fitting rows:", nrow(common$data), "; common evaluation games:", length(ids)),
    "Training starts in 2020; folds predict 2022-2025 using preceding years. Penalty and coach choices use earlier validation seasons only.",
    "EPA keeps the existing season-to-date and early-history-blend definitions. Internal power and bridge construction are unchanged.",
    "Elo uses explicit cached pregame fields. Original Elo and preseason release timestamps are not independently authenticated.",
    "Same-cycle prior-game and missing-date exclusions appear in cohort_coverage.csv. Missing EPA uses training-fold imputation, not outcome-based dropping.",
    "Production source, cached data, live cards and backtest hashes match before/after. No model is promoted.", "",
    "See feature_manifest.csv, fold_choices.csv, baseline_inputs.csv, common_inputs.rds, predictions.csv, paired_comparisons.csv and leave_one_season_out.csv."), report)
  list(report = report, metrics = summary, paired = paired$paired, output = output)
}
