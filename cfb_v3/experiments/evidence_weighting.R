fit_evidence_centers <- function(priors, training_seasons, test_season, features) {
  stopifnot(length(training_seasons) > 0, all(training_seasons < test_season))
  assert_unique_keys(priors, c("team", "season"), "evidence preseason priors")
  rows <- priors$season %in% training_seasons
  vapply(priors[rows, features, drop = FALSE], function(x) {
    finite <- as.numeric(x)[is.finite(as.numeric(x))]
    if (length(finite)) stats::median(finite) else 0
  }, numeric(1))
}

attach_evidence_weighting <- function(data, priors, centers, config) {
  required <- c("game_id", "season", "week", "home", "away", "postseason_type",
    "home_level", "away_level", "home_source_games", "away_source_games")
  assert_columns(data, required, "evidence feature rows")
  assert_unique_keys(data, "game_id", "evidence feature rows")
  assert_unique_keys(priors, c("team", "season"), "evidence priors")
  features <- config$preseason$production_features
  stopifnot(identical(names(centers), features), all(is.finite(centers)))
  regular <- !is.na(data$postseason_type) & data$postseason_type == "regular"
  phase_week <- ifelse(regular, data$week, 99L)
  original <- preseason_feature_weight(phase_week, config)
  eligible_week <- regular & is.finite(data$week) & data$week >= 2 & data$week <= 4
  weights <- list(); hold <- list()
  for (side in c("home", "away")) {
    counts <- as.numeric(data[[paste0(side, "_source_games")]])
    eligible <- eligible_week & !is.na(data[[paste0(side, "_level")]]) &
      tolower(data[[paste0(side, "_level")]]) == "fbs"
    if (any(eligible & (!is.finite(counts) | counts < 0 | counts != floor(counts))))
      stop("FBS sample counts must be known nonnegative integers.")
    hold[[side]] <- eligible & is.finite(counts) & counts == 0
    weights[[side]] <- ifelse(hold[[side]], 1, original)
  }
  key <- paste(canonical_team(priors$team), priors$season, sep = "\r")
  if (anyDuplicated(key)) stop("Canonical preseason keys are duplicated.")
  home <- match(paste(canonical_team(data$home), data$season, sep = "\r"), key)
  away <- match(paste(canonical_team(data$away), data$season, sep = "\r"), key)
  covered <- data$season %in% priors$season
  for (f in features) {
    column <- paste0("ps_", f, "_diff")
    assert_columns(data, column, "original roster features")
    h <- as.numeric(priors[[f]][home]); a <- as.numeric(priors[[f]][away])
    changed <- covered & (hold$home | hold$away) & is.finite(data[[column]]) &
      is.finite(h) & is.finite(a)
    # Adjust around a training-only neutral team; do not impute a missing side here.
    data[[column]][changed] <- data[[column]][changed] +
      (weights$home[changed]-original[changed])*(h[changed]-centers[[f]]) -
      (weights$away[changed]-original[changed])*(a[changed]-centers[[f]])
  }
  ledger <- data.frame(game_id = data$game_id, season = data$season, week = data$week,
    home = data$home, away = data$away, calendar_weight = original,
    home_source_games = data$home_source_games, away_source_games = data$away_source_games,
    home_hold = hold$home, away_hold = hold$away,
    home_roster_weight = weights$home, away_roster_weight = weights$away,
    home_prior_present = !is.na(home), away_prior_present = !is.na(away))
  list(data = data, ledger = ledger)
}

fit_evidence_roster <- function(data, weights, priors, config, season, features, lambda) {
  train <- which(data$season < season); test <- which(data$season == season)
  centers <- fit_evidence_centers(priors, unique(data$season[train]), season,
    config$preseason$production_features)
  adjusted <- attach_evidence_weighting(data, priors, centers, config)
  weights <- weights[train]/max(weights[train])
  base <- fit_cfb_ensemble(data[train, ], features = features, weights = weights,
    lambda = lambda, config = config, fit_nonlinear = FALSE)
  candidate <- fit_cfb_ensemble(adjusted$data[train, ], features = features, weights = weights,
    lambda = lambda, config = config, fit_nonlinear = FALSE)
  list(base = base, candidate = candidate, centers = centers, rows = test,
    base_margin = predict(base, data[test, ])$expected_margin,
    candidate_margin = predict(candidate, adjusted$data[test, ])$expected_margin,
    ledger = adjusted$ledger[test, ])
}

evidence_opener_ledger <- function(data, games) {
  assert_unique_keys(games, "game_id", "opener schedule")
  index <- match(data$game_id, games$game_id)
  if (anyNA(index)) stop("Evaluation games lack schedule metadata.")
  source <- games[index, ]
  if (!identical(canonical_team(data$home), canonical_team(source$home)) ||
      !identical(canonical_team(data$away), canonical_team(source$away)) ||
      !isTRUE(all.equal(data$season, source$season, check.attributes = FALSE)))
    stop("Opener schedule metadata mismatch.")
  dates <- parse_utc_datetime(games$kickoff)
  rows <- list()
  for (i in seq_len(nrow(data))) for (side in c("home", "away")) {
    team <- data[[side]][i]
    cutoff <- as.POSIXct(as.Date(source$feature_week_start[i]), tz = "UTC")
    if (is.na(cutoff)) stop("Missing feature cutoff in opener ledger.")
    past <- games[which(games$season == data$season[i] & games$completed &
      as.numeric(dates) < as.numeric(cutoff) & (games$home == team | games$away == team)), ]
    opponent_level <- ifelse(past$home == team, past$away_level, past$home_level)
    fcs_only <- nrow(past) > 0 && all(!is.na(opponent_level) & opponent_level == "fcs")
    rows[[paste(i, side)]] <- data.frame(game_id = data$game_id[i], side = side, team = team,
      prior_games = nrow(past), prior_fbs_opponents = sum(opponent_level == "fbs", na.rm = TRUE),
      prior_fcs_opponents = sum(opponent_level == "fcs", na.rm = TRUE),
      unknown_opponent_levels = sum(is.na(opponent_level)), fcs_only = fcs_only,
      single_fcs_opener = fcs_only && nrow(past) == 1L)
  }
  do.call(rbind, rows)
}

evidence_shadow <- function(config, data, weights, priors, features) {
  root <- file.path(config$output_dir, "2026")
  stem <- file.path(root, "week_2", "live_live_2026_w02_20260909T005125Z")
  saved <- readRDS(paste0(stem, "_model.rds"))
  live <- read_csv_if_present(paste0(stem, "_features.csv"), TRUE)
  card <- read_csv_if_present(file.path(root, "week_2_preliminary", "20260909T032636Z", "predictions.csv"), TRUE)
  assert_unique_keys(card, "game_id", "shadow card")
  index <- match(live$game_id, card$game_id)
  stopifnot(!anyNA(index), nrow(card) == nrow(live), all(data$season < 2026))
  card <- card[index, ]
  stopifnot(max(abs(predict(saved$model, live)$expected_margin-card$expected_margin)) < 1e-8)
  baseline <- fit_cfb_ensemble(data, features = features, weights = weights,
    lambda = saved$model$preseason$ridge$lambda, config = config, fit_nonlinear = FALSE)
  parity <- max(abs(predict(baseline, live)$expected_margin-predict(saved$model$preseason, live)$expected_margin))
  if (parity > 1e-8) stop("Final roster refit does not reproduce the saved live model.")
  centers <- fit_evidence_centers(priors, unique(data$season), 2026L, config$preseason$production_features)
  training <- attach_evidence_weighting(data, priors, centers, config)$data
  adjusted <- attach_evidence_weighting(live, priors, centers, config)
  candidate <- fit_cfb_ensemble(training, features = features, weights = weights,
    lambda = saved$model$preseason$ridge$lambda, config = config, fit_nonlinear = FALSE)
  share <- preseason_blend_share_for_data(live, config)
  forecast <- (1-share)*predict(saved$model$foundation, live)$expected_margin +
    share*predict(candidate, adjusted$data)$expected_margin
  out <- cbind(card[c("game_id", "home", "away", "season", "week", "market_home_spread")],
    base_margin = card$expected_margin, candidate_margin = forecast,
    change = forecast-card$expected_margin,
    adjusted$ledger[setdiff(names(adjusted$ledger), c("game_id", "season", "week", "home", "away"))])
  out$base_edge <- out$base_margin+out$market_home_spread
  out$candidate_edge <- out$candidate_margin+out$market_home_spread
  out$ats_side_changed <- sign(out$base_edge) != sign(out$candidate_edge)
  list(table = out, model = candidate, centers = centers, replay_delta = parity)
}

run_evidence_weighting <- function(config) {
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  source <- file.path(config$output_dir, "experiments", "four_way", "20260909T214828Z")
  output <- file.path(config$output_dir, "experiments", "evidence_weighting",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (dir.exists(output)) stop("Refusing to overwrite this experiment.")
  protected <- experiment_file_hashes(config)
  dir.create(output, recursive = TRUE)
  message("Evidence-weighting output: ", output)
  file.copy(file.path(root, "EVIDENCE_WEIGHTING_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  code <- c(file.path(root, c("EVIDENCE_WEIGHTING_PROTOCOL.md", "evidence_weighting.R")),
    file.path(config$project_dir, "run_cfb_evidence_weighting.R"),
    file.path(source, c("predictions.csv", "fold_choices.csv")))
  write_foundation_csv(data.frame(path = code, md5 = unname(tools::md5sum(code))), file.path(output, "input_code_hashes.csv"))
  saveRDS(config, file.path(output, "config.rds"))
  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  training <- training[training$season <= 2025, ]
  stopifnot(all(training$feature_version == config$version))
  validation <- prepare_training_data(training, config)
  members <- combine_membership(read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  validation$data <- apply_fbs_bridge_backtest_features(validation$data, team_games[team_games$season <= 2025, ],
    members[members$season <= 2025, ], config)
  priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
  validation$data <- attach_preseason_features(validation$data, priors, config,
    features = config$preseason$production_features, prefix = "ps_")
  message("Reproducing unchanged v3 on its original fitting population")
  control <- experiment_football(validation$data, validation$weights, config, unique(priors$season), "v3_control")
  frozen <- read_csv_if_present(file.path(config$output_dir, "backtest", "rolling_predictions.csv"), TRUE)
  frozen <- frozen[frozen$model == "ridge_core", ]
  stopifnot(nrow(frozen) == nrow(control$predictions), !anyDuplicated(frozen$game_id))
  index <- match(control$predictions$game_id, frozen$game_id)
  control_delta <- max(abs(control$predictions$expected_margin-frozen$expected_margin[index]))
  stopifnot(!anyNA(index), control_delta < 1e-8)
  locked_choices <- read_csv_if_present(file.path(source, "fold_choices.csv"), TRUE)
  locked_choices <- locked_choices[locked_choices$model == "v3_control", ]
  stopifnot(isTRUE(all.equal(control$choices, locked_choices, check.attributes = FALSE)))
  ps <- paste0("ps_", config$preseason$production_features, "_diff")
  gate <- experiment_preseason_gate(unique(priors$season), ps)
  candidate <- control$predictions; candidate$model <- "zero_sample_roster_hold"
  ledgers <- list(); centers <- list(); folds <- list()
  for (season in control$choices$test_season) {
    message("Fitting zero-sample roster hold for ", season)
    choice <- control$choices[control$choices$test_season == season, ]
    data <- control$coach$data
    column <- if (choice$coach_recent_share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"
    data$coach_rating_diff <- data[[column]]
    features <- gate(season, control$coach$features)
    fit <- fit_evidence_roster(data, validation$weights, priors, config, season, features, choice$preseason_lambda)
    rows <- match(data$game_id[fit$rows], candidate$game_id)
    active <- preseason_blend_share_for_data(data[fit$rows, ], config) > 0
    stopifnot(!anyNA(rows), max(abs(fit$base_margin[active]-control$predictions$expected_margin[rows[active]])) < 1e-8)
    candidate$expected_margin[rows[active]] <- fit$candidate_margin[active]
    fit$ledger$roster_layer_active <- any(ps %in% features) & active
    ledgers[[as.character(season)]] <- fit$ledger
    centers[[as.character(season)]] <- data.frame(test_season = season, feature = names(fit$centers), center = unname(fit$centers))
    folds[[as.character(season)]] <- fit[c("candidate", "centers")]
  }
  reference <- read_csv_if_present(file.path(source, "predictions.csv"), TRUE)
  reference <- reference[reference$model == "v3_control", ]
  scored <- four_way_match(list(control$predictions, candidate), reference$game_id, reference)
  names(scored)[names(scored) == "margin_sd"] <- "reference_margin_sd"
  ref <- scored[scored$model == "v3_control", ]; cand <- scored[scored$model == "zero_sample_roster_hold", ]
  inactive <- ref$postseason_type != "regular" | ref$week >= 5
  stopifnot(identical(cand$expected_margin[inactive], ref$expected_margin[inactive]))
  ledger <- do.call(rbind, ledgers)
  ledger <- ledger[match(ref$game_id, ledger$game_id), ]
  ledger$base_margin <- ref$expected_margin; ledger$candidate_margin <- cand$expected_margin
  ledger$margin_change <- cand$expected_margin-ref$expected_margin
  ledger$ats_side_changed <- sign(cand$edge) != sign(ref$edge)
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  opener <- evidence_opener_ledger(ref, games)
  for (side in c("home", "away")) {
    info <- opener[opener$side == side, ]; info <- info[match(ref$game_id, info$game_id), ]
    ledger[[paste0(side, "_fcs_only")]] <- info$fcs_only
    ledger[[paste0(side, "_single_fcs_opener")]] <- info$single_fcs_opener
  }
  targeted <- ledger$roster_layer_active & (ledger$home_hold | ledger$away_hold)
  fcs <- ledger$roster_layer_active & ((ledger$home_hold & ledger$home_fcs_only) | (ledger$away_hold & ledger$away_fcs_only))
  single <- ledger$roster_layer_active & ((ledger$home_hold & ledger$home_single_fcs_opener) | (ledger$away_hold & ledger$away_single_fcs_opener))
  slices <- list(fcs_only_followup_active = fcs, single_fcs_opener_active = single,
    zero_sample_targeted_active = targeted, early_untargeted_active = ledger$roster_layer_active & !targeted)
  for (season in sort(unique(ref$season))) slices[[paste0("fcs_only_", season)]] <- fcs & ref$season == season
  metrics <- four_way_metrics(scored)
  for (name in names(slices)) {
    if (!any(slices[[name]])) next
    metric <- four_way_metrics(scored[rep(slices[[name]], 2), ])
    metric <- metric[metric$slice == "all_fbs", ]; metric$slice <- name
    metrics <- rbind(metrics, metric)
  }
  comparison_slices <- c(list(all = rep(TRUE, nrow(ref)), weeks_2_4 = ref$postseason_type == "regular" & ref$week >= 2 & ref$week <= 4), slices[1:4])
  paired <- list(); omissions <- list()
  for (name in names(comparison_slices)) {
    keep <- comparison_slices[[name]]
    delta <- cand$mae_error[keep]-ref$mae_error[keep]; ats <- cand$ats_correct[keep]-ref$ats_correct[keep]
    years <- ref$season[keep]
    ci <- four_way_intervals(delta, years); ai <- four_way_intervals(ats, years)
    paired[[name]] <- data.frame(slice = name, games = sum(keep), mae_change = mean(delta),
      mae_low = ci["low"], mae_high = ci["high"], ats_change = mean(ats, na.rm = TRUE), ats_low = ai["low"], ats_high = ai["high"])
    for (year in sort(unique(years))) omissions[[paste(name, year)]] <- data.frame(slice = name,
      omitted_season = year, mae_change = mean(delta[years != year]), ats_change = mean(ats[years != year], na.rm = TRUE))
  }
  message("Refitting through 2025 for a frozen Week 2 shadow comparison")
  shadow <- evidence_shadow(config, control$coach$data, validation$weights, priors, control$coach$features)
  results <- list(predictions = scored, metrics = metrics, paired_comparisons = do.call(rbind, paired),
    leave_one_season_out = do.call(rbind, omissions), feature_weight_ledger = ledger,
    opener_evidence = opener, training_centers = do.call(rbind, centers),
    fold_choices = control$choices, week_2_shadow = shadow$table)
  results$feature_manifest <- do.call(rbind, lapply(names(folds), function(year)
    data.frame(test_season = as.integer(year), feature = folds[[year]]$candidate$features)))
  for (name in names(results)) write_foundation_csv(results[[name]], file.path(output, paste0(name, ".csv")))
  writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
  saveRDS(list(folds = folds, shadow = shadow[c("model", "centers")]), file.path(output, "candidate_models.rds"))
  after <- experiment_file_hashes(config)
  stopifnot(identical(protected, after))
  write_foundation_csv(protected, file.path(output, "protected_before.csv"))
  write_foundation_csv(after, file.path(output, "protected_after.csv"))
  jsonlite::write_json(list(status = "complete", production_unchanged = TRUE, automatic_promotion = FALSE,
    control_replay_delta = control_delta, live_roster_replay_delta = shadow$replay_delta,
    inactive_predictions_identical = TRUE, fitting_rows = nrow(validation$data),
    evaluation_games = nrow(ref), targeted_active_games = sum(targeted), fcs_only_active_games = sum(fcs)),
    file.path(output, "verification.json"), pretty = TRUE, auto_unbox = TRUE)
  list(output = output, results = results)
}
