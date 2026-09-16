early_calibration_phase <- function(data) {
  week <- as.integer(data$week)
  regular <- !is.na(data$postseason_type) & data$postseason_type == "regular"
  ifelse(regular & is.finite(week) & week <= 1, "week_0_1",
    ifelse(regular & is.finite(week) & week >= 2 & week <= 4, "weeks_2_4", "inactive"))
}

fit_early_calibration <- function(history, season, minimum_rows = 100L) {
  assert_unique_keys(history, "game_id", "calibration reference")
  stopifnot(minimum_rows == 100L)
  phase <- early_calibration_phase(history)
  models <- list(); choices <- list()
  for (group in c("week_0_1", "weeks_2_4")) {
    keep <- history$season < season & phase == group &
      is.finite(history$expected_margin) & is.finite(history$actual_margin)
    past <- history[which(keep), ]
    fit <- NULL
    if (nrow(past) >= minimum_rows) {
      past$residual <- past$actual_margin-past$expected_margin
      weights <- 0.82^(max(past$season)-past$season)
      fit <- fit_weighted_ridge(past, "residual", "expected_margin", weights, lambda = 8)
    }
    models[group] <- list(fit)
    slope <- if (is.null(fit)) 1 else 1+fit$coefficients[[2]]/fit$recipe$scales[[1]]
    intercept <- if (is.null(fit)) 0 else fit$coefficients[[1]] -
      fit$coefficients[[2]]*fit$recipe$means[[1]]/fit$recipe$scales[[1]]
    choices[[group]] <- data.frame(test_season = season, phase = group, prior_games = nrow(past),
      last_training_season = if (nrow(past)) max(past$season) else NA_integer_,
      applied = !is.null(fit), slope = slope, intercept = intercept)
  }
  list(test_season = season, models = models, choices = do.call(rbind, choices))
}

predict_early_calibration <- function(fit, data) {
  if (any(data$season != fit$test_season) || any(!is.finite(data$expected_margin)))
    stop("Calibration target season or predictions are invalid.")
  phase <- early_calibration_phase(data)
  correction <- rep(0, nrow(data)); applied <- rep(FALSE, nrow(data))
  for (group in names(fit$models)) {
    rows <- which(phase == group)
    if (!is.null(fit$models[[group]]) && length(rows)) {
      correction[rows] <- predict(fit$models[[group]], data[rows, ])
      applied[rows] <- TRUE
    }
  }
  data.frame(game_id = data$game_id, base_margin = data$expected_margin,
    expected_margin = data$expected_margin+correction, correction = correction,
    calibration_applied = applied, calibration_phase = phase)
}

run_early_calibration <- function(config) {
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  source <- file.path(config$output_dir, "experiments", "four_way", "20260909T214828Z", "predictions.csv")
  live_source <- file.path(config$output_dir, "2026", "week_2_preliminary", "20260909T032636Z", "predictions.csv")
  protected <- experiment_file_hashes(config)
  output <- file.path(config$output_dir, "experiments", "early_calibration",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (dir.exists(output)) stop("Refusing to overwrite a calibration experiment.")
  dir.create(output, recursive = TRUE)
  file.copy(file.path(root, "EARLY_CALIBRATION_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  provenance <- c(source, live_source, file.path(root, c("early_calibration.R", "EARLY_CALIBRATION_PROTOCOL.md")),
    file.path(config$project_dir, "run_cfb_early_calibration.R"))
  write_foundation_csv(data.frame(path = provenance, md5 = unname(tools::md5sum(provenance))),
    file.path(output, "input_code_hashes.csv"))
  message("Calibration output: ", output)
  reference <- read_csv_if_present(source, TRUE)
  reference <- reference[reference$model == "v3_control", ]
  assert_unique_keys(reference, "game_id", "frozen v3 reference")
  candidate <- reference; candidate$model <- "early_calibration"
  choices <- list(); fits <- list(); ledger <- list()
  for (season in sort(unique(reference$season))) {
    fit <- fit_early_calibration(reference, season)
    rows <- which(reference$season == season)
    prediction <- predict_early_calibration(fit, reference[rows, ])
    candidate$expected_margin[rows] <- prediction$expected_margin
    ledger[[as.character(season)]] <- prediction
    choices[[as.character(season)]] <- fit$choices
    fits[[as.character(season)]] <- fit
  }
  candidate <- experiment_grade(candidate)
  ledger <- do.call(rbind, ledger)
  inactive <- early_calibration_phase(reference) == "inactive"
  stopifnot(identical(candidate$expected_margin[inactive], reference$expected_margin[inactive]))
  metrics <- four_way_metrics(rbind(reference, candidate))
  parts <- list(all = rep(TRUE, nrow(reference)), early = !inactive,
    week_0_1 = early_calibration_phase(reference) == "week_0_1",
    weeks_2_4 = early_calibration_phase(reference) == "weeks_2_4")
  paired <- list(); omissions <- list()
  for (name in names(parts)) {
    keep <- parts[[name]]
    delta <- candidate$mae_error[keep]-reference$mae_error[keep]
    ats <- candidate$ats_correct[keep]-reference$ats_correct[keep]
    years <- reference$season[keep]
    ci <- four_way_intervals(delta, years); ai <- four_way_intervals(ats, years)
    paired[[name]] <- data.frame(slice = name, games = sum(keep), mae_change = mean(delta),
      mae_low = ci["low"], mae_high = ci["high"], ats_change = mean(ats, na.rm = TRUE),
      ats_low = ai["low"], ats_high = ai["high"])
    for (year in sort(unique(years))) omissions[[paste(name, year)]] <- data.frame(slice = name,
      omitted_season = year, mae_change = mean(delta[years != year]), ats_change = mean(ats[years != year], na.rm = TRUE))
  }
  live_fit <- fit_early_calibration(reference, 2026L)
  live <- read_csv_if_present(live_source, TRUE)
  live$postseason_type <- "regular"
  shadow <- predict_early_calibration(live_fit, live)
  shadow <- cbind(live[c("season", "week", "home", "away", "market_home_spread")], shadow)
  shadow$base_edge <- shadow$base_margin+shadow$market_home_spread
  shadow$shadow_edge <- shadow$expected_margin+shadow$market_home_spread
  shadow$ats_side_changed <- sign(shadow$base_edge) != sign(shadow$shadow_edge)
  choices <- do.call(rbind, c(choices, list(live_fit$choices)))
  results <- list(predictions = rbind(reference, candidate), calibration_ledger = ledger,
    metrics = metrics, paired_comparisons = do.call(rbind, paired),
    leave_one_season_out = do.call(rbind, omissions), choices = choices, week_2_shadow = shadow)
  for (name in names(results)) write_foundation_csv(results[[name]], file.path(output, paste0(name, ".csv")))
  saveRDS(c(fits, list(`2026` = live_fit)), file.path(output, "calibrators.rds"))
  after <- experiment_file_hashes(config)
  stopifnot(identical(protected, after))
  write_foundation_csv(protected, file.path(output, "protected_before.csv"))
  write_foundation_csv(after, file.path(output, "protected_after.csv"))
  jsonlite::write_json(list(status = "complete", production_unchanged = TRUE,
    inactive_predictions_identical = TRUE, automatic_promotion = FALSE,
    games = nrow(reference), corrected_games = sum(ledger$calibration_applied)),
    file.path(output, "verification.json"), pretty = TRUE, auto_unbox = TRUE)
  list(output = output, results = results)
}
