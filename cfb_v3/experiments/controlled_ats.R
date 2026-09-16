experiment_fbs_data <- function(data, weights) {
  keep <- raw_history_eligible(data) & is.finite(weights) & weights > 0
  list(data = data[keep, , drop = FALSE], weights = weights[keep])
}

experiment_preseason_gate <- function(covered_seasons, preseason_features) {
  function(season, features) {
    if (sum(covered_seasons < season) >= 2L) features else
      setdiff(features, preseason_features)
  }
}

experiment_football <- function(data, weights, config, covered_seasons, name) {
  ps <- paste0("ps_", config$preseason$production_features, "_diff")
  coach <- select_coach_split_validation(data, weights, config, fit_nonlinear = FALSE,
    fold_features = experiment_preseason_gate(covered_seasons, ps))
  foundation <- rolling_validate_ensemble(coach$data,
    features = setdiff(coach$features, ps), weights = weights, config = config,
    fit_nonlinear = FALSE)
  rolling <- blend_rolling_predictions(coach$data, foundation, coach$rolling, config)
  seasons <- sort(unique(rolling$predictions$test_season))
  choices <- do.call(rbind, lapply(seasons, function(season) {
    data.frame(model = name, test_season = season,
      train_through_season = max(data$season[data$season < season]),
      training_rows = sum(data$season < season),
      coach_recent_share = attr(coach$data, "fold_coach_shares")[[as.character(season)]],
      foundation_lambda = unique(foundation$predictions$lambda[
        foundation$predictions$test_season == season]),
      preseason_lambda = unique(coach$rolling$predictions$lambda[
        coach$rolling$predictions$test_season == season]))
  }))
  list(predictions = build_v2_backtest_predictions(coach$data, rolling, name),
       coach = coach, choices = choices)
}

experiment_market_inputs <- function(data) {
  assert_columns(data, c("closing_home_spread", "market_total"), "market inputs")
  data$market_margin <- -as.numeric(data$closing_home_spread)
  data$market_spread_size <- abs(data$market_margin)
  data$market_total_input <- as.numeric(data$market_total)
  data$market_total_missing <- as.numeric(!is.finite(data$market_total_input))
  if ("margin" %in% names(data)) data$cover_residual <- data$margin - data$market_margin
  data
}

experiment_choose_lambda <- function(predictions, season, default) {
  past <- predictions[predictions$test_season < season, , drop = FALSE]
  if (!nrow(past)) return(default)
  score <- aggregate(absolute_error ~ lambda, past, mean)
  score$lambda[which.min(score$absolute_error)]
}

experiment_market <- function(data, weights, coach, config, covered_seasons,
                               grid = c(8, 32, 128, 512, 2048, 8192), default = 2048) {
  stopifnot(default %in% grid)
  keep <- is.finite(data$closing_home_spread)
  data <- experiment_market_inputs(data[keep, , drop = FALSE])
  weights <- weights[keep]
  market_features <- c("market_margin", "market_spread_size", "market_total_input",
                       "market_total_missing")
  full_features <- c(coach$features, market_features)
  ps <- paste0("ps_", config$preseason$production_features, "_diff")
  base_features <- setdiff(full_features, ps)
  gate <- experiment_preseason_gate(covered_seasons, ps)
  splits <- rolling_season_splits(data)
  all_predictions <- list()
  k <- 1L
  for (variant in c("foundation", "preseason")) {
    for (lambda in grid) {
      for (split in splits) {
        fold <- data
        share <- attr(coach$data, "fold_coach_shares")[[as.character(split$test_season)]]
        column <- if (share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"
        fold$coach_rating_diff <- fold[[column]]
        features <- if (variant == "foundation") base_features else
          gate(split$test_season, full_features)
        w <- weights[split$train] / max(weights[split$train])
        fit <- fit_weighted_ridge(fold[split$train, ], "cover_residual", features, w, lambda)
        correction <- predict(fit, fold[split$test, ])
        expected <- fold$market_margin[split$test] + correction
        all_predictions[[k]] <- data.frame(variant = variant, lambda = lambda,
          test_season = split$test_season, row_id = split$test,
          actual = fold$margin[split$test], expected_margin = expected,
          absolute_error = abs(fold$margin[split$test] - expected),
          coach_recent_share = share)
        k <- k + 1L
      }
    }
  }
  grid_predictions <- do.call(rbind, all_predictions)
  choices <- list()
  selected <- lapply(seq_along(splits), function(i) {
    split <- splits[[i]]
    season <- split$test_season
    parts <- lapply(c("foundation", "preseason"), function(variant) {
      candidate <- grid_predictions[grid_predictions$variant == variant, ]
      lambda <- experiment_choose_lambda(candidate, season, default)
      candidate[candidate$test_season == season & candidate$lambda == lambda, ]
    })
    base <- parts[[1]]; full <- parts[[2]]
    stopifnot(identical(base$row_id, full$row_id))
    share <- preseason_blend_share_for_data(data[base$row_id, ], config)
    expected <- (1-share)*base$expected_margin + share*full$expected_margin
    choices[[i]] <<- data.frame(model = "market_residual", test_season = season,
      train_through_season = max(data$season[split$train]), training_rows = length(split$train),
      coach_recent_share = base$coach_recent_share[1],
      foundation_lambda = base$lambda[1], preseason_lambda = full$lambda[1])
    data.frame(row_id = base$row_id, test_season = season, lambda = NA_real_,
      actual = base$actual, expected_margin = expected, fair_margin = expected,
      margin_sd = NA_real_, error = base$actual-expected,
      absolute_error = abs(base$actual-expected))
  })
  rolling <- list(predictions = do.call(rbind, selected))
  list(predictions = build_v2_backtest_predictions(data, rolling, "market_residual"),
       choices = do.call(rbind, choices), grid_scores = aggregate(
         absolute_error ~ variant + lambda + test_season, grid_predictions, mean),
       feature_names = full_features)
}

experiment_grade <- function(data) {
  edge <- data$expected_margin + data$closing_home_spread
  result <- data$actual_margin + data$closing_home_spread
  data$edge <- edge
  data$selected <- is.finite(edge) & edge != 0 & is.finite(result)
  data$push <- data$selected & result == 0
  data$win <- data$selected & !data$push & sign(edge) == sign(result)
  data$loss <- data$selected & !data$push & !data$win
  data$mae_error <- abs(data$actual_margin - data$expected_margin)
  data$su_correct <- sign(data$actual_margin) == sign(data$expected_margin)
  data$ats_correct <- ifelse(data$win, 1, ifelse(data$loss, 0, NA_real_))
  data$absolute_error <- data$mae_error
  data$winner_correct <- data$su_correct
  data$model_edge <- edge
  data$fair_margin <- data$expected_margin
  data
}

experiment_slices <- function(data) {
  sides <- p4_or_independent_sides(data)
  slices <- list(all_fbs = rep(TRUE, nrow(data)),
    week_0_1 = data$game_phase == "preseason",
    week_2 = data$week == 2 & data$postseason_type == "regular",
    week_5_plus = data$week >= 5 & data$postseason_type == "regular",
    cfp = as.logical(data$is_cfp), p4_or_independent = sides$home | sides$away,
    spread_over_21 = abs(data$closing_home_spread) > 21,
    fixed_edge_3_plus = abs(data$edge) >= 3)
  for (season in sort(unique(data$season))) slices[[paste0("season_", season)]] <- data$season == season
  slices
}

experiment_metrics <- function(predictions) {
  slices <- experiment_slices(predictions)
  do.call(rbind, lapply(unique(predictions$model), function(model) {
    do.call(rbind, lapply(names(slices), function(slice) {
      keep <- predictions$model == model & slices[[slice]]
      keep[is.na(keep)] <- FALSE
      x <- predictions[keep, ]
      wins <- sum(x$win); losses <- sum(x$loss); pushes <- sum(x$push)
      decisions <- wins + losses
      interval <- if (decisions) stats::binom.test(wins, decisions, conf.level = .975)$conf.int else c(NA, NA)
      data.frame(model = model, slice = slice, games = nrow(x), wins = wins,
        losses = losses, pushes = pushes, abstentions = sum(!x$selected),
        ats_accuracy = if (decisions) wins / decisions else NA_real_,
        ats_ci_low = interval[1], ats_ci_high = interval[2],
        margin_mae = if (nrow(x)) mean(x$mae_error) else NA_real_,
        su_accuracy = if (nrow(x)) mean(x$su_correct) else NA_real_,
        illustrative_roi = if (sum(x$selected)) (wins/1.1-losses)/sum(x$selected) else NA_real_)
    }))
  }))
}

experiment_season_interval <- function(delta, seasons, replicates = 5000L, seed = 20260909L) {
  keep <- is.finite(delta) & is.finite(seasons)
  delta <- delta[keep]; seasons <- seasons[keep]
  if (length(unique(seasons)) < 2L) return(c(NA_real_, NA_real_))
  groups <- split(delta, seasons)
  totals <- vapply(groups, sum, numeric(1)); counts <- lengths(groups)
  set.seed(seed)
  boot <- replicate(replicates, {
    draw <- sample(seq_along(groups), replace = TRUE)
    sum(totals[draw])/sum(counts[draw])
  })
  as.numeric(stats::quantile(boot, c(.025, .975)))
}

experiment_paired <- function(predictions) {
  comparisons <- list(c("fbs_only", "v3_control"), c("market_residual", "v3_control"),
                       c("market_residual", "fbs_only"), c("market_residual", "market_reference"))
  do.call(rbind, lapply(comparisons, function(models) {
    x <- merge(predictions[predictions$model == models[1], ],
               predictions[predictions$model == models[2], ],
               by = c("game_id", "season"), suffixes = c("_candidate", "_reference"))
    stopifnot(nrow(x) == sum(predictions$model == models[1]))
    delta <- x$mae_error_candidate - x$mae_error_reference
    ats_delta <- x$ats_correct_candidate - x$ats_correct_reference
    mae_ci <- experiment_season_interval(delta, x$season)
    ats_ci <- experiment_season_interval(ats_delta, x$season)
    data.frame(candidate = models[1], reference = models[2], games = nrow(x),
      mae_change = mean(delta), mae_change_low = mae_ci[1], mae_change_high = mae_ci[2],
      ats_change = if (any(is.finite(ats_delta))) mean(ats_delta, na.rm = TRUE) else NA_real_,
      ats_change_low = ats_ci[1], ats_change_high = ats_ci[2])
  }))
}

experiment_file_hashes <- function(config) {
  paths <- c(list.files(file.path(config$project_dir, "cfb_v2"), "\\.R$", full.names = TRUE),
    list.files(config$data_dir, "\\.(csv|duckdb)$", full.names = TRUE),
    list.files(config$inbox_dir, "\\.csv$", full.names = TRUE),
    list.files(file.path(config$output_dir, "backtest"), full.names = TRUE),
    list.files(file.path(config$output_dir, "2026"), recursive = TRUE, full.names = TRUE),
    file.path(config$project_dir, c("run_cfb_v2.R", "run_cfb_v3.R")))
  paths <- sort(unique(paths[file.exists(paths) & !dir.exists(paths)]))
  data.frame(path = paths, md5 = unname(tools::md5sum(paths)))
}

experiment_report <- function(metrics, paired, coverage, choices, baseline_delta, path) {
  display <- function(x) {
    x <- x[c("model", "slice", "games", "wins", "losses", "pushes",
             "ats_accuracy", "margin_mae", "illustrative_roi")]
    x$ats_accuracy <- 100*x$ats_accuracy
    x$illustrative_roi <- 100*x$illustrative_roi
    names(x)[names(x) == "ats_accuracy"] <- "ats_pct"
    names(x)[names(x) == "illustrative_roi"] <- "roi_pct_at_minus110"
    markdown_table(x)
  }
  all <- metrics[metrics$slice == "all_fbs", ]
  screening <- do.call(rbind, lapply(c("fbs_only", "market_residual"), function(model) {
    x <- all[all$model == model, ]; last <- metrics[metrics$model == model & metrics$slice == "season_2025", ]
    baseline <- all$margin_mae[all$model == "v3_control"]
    market <- all$margin_mae[all$model == "market_reference"]
    data.frame(model = model, beats_v3_mae = x$margin_mae < baseline,
      ats_above_break_even = x$ats_accuracy > 110/210,
      last_season_above_break_even = last$ats_accuracy > 110/210,
      beats_market_mae = x$margin_mae < market,
      adjusted_ats_interval_above_break_even = x$ats_ci_low > 110/210,
      passes_point_estimate_screen = x$margin_mae < baseline & x$ats_accuracy > 110/210 &
        last$ats_accuracy > 110/210 & (model != "market_residual" | x$margin_mae < market))
  }))
  lines <- c("# Controlled ATS Experiments", "",
    "Two pre-specified challengers; production v3 remains unchanged. Read protocol.md",
    "for the locked design. These are historical closing-line diagnostics, not a",
    "fresh Friday article backtest or a record of bets actually made.", "",
    paste("Maximum absolute difference when reproducing saved v3 margins:", format(baseline_delta)), "",
    "## Matched FBS Games", "", display(all), "",
    "ROI assumes one unit risked at -110 on each selection, including pushes in risked",
    "volume. This is illustrative, not actual historical sportsbook pricing/profit.", "",
    "## Season Results", "", display(metrics[grepl("^season_", metrics$slice), ]), "",
    "## Locked Screening Rules", "", markdown_table(screening), "",
    "Passing this screen would justify prospective testing, not promotion or a claim",
    "of proof. Failure is also a valid result. No grids or thresholds were revised",
    "in response to these results.", "",
    "## ATS Intervals", "",
    markdown_table(all[c("model", "wins", "losses", "ats_accuracy", "ats_ci_low", "ats_ci_high")]), "",
    "Exact 97.5% binomial intervals adjust for the two new challenger comparisons,",
    "but assume independent games and do not erase earlier model-development searches.", "",
    "## Paired Differences", "", markdown_table(paired), "",
    "Negative MAE change favors the candidate; positive ATS change favors it.",
    "Intervals resample entire seasons (5,000 draws). Four seasons limit reliability.", "",
    "## Diagnostic Slices", "",
    display(metrics[!grepl("^season_", metrics$slice) & metrics$slice != "all_fbs", ]), "",
    "The 3-point edge subset differs by model. These are descriptive slices, not",
    "additional candidate strategies chosen after observing their records.", "",
    "## Input Coverage", "", markdown_table(coverage), "",
    "A trains on all eligible FBS games; B requires a line. Missing totals are",
    "imputed from each training fold, with an explicit missingness indicator.", "",
    "## Chronological Choices", "", markdown_table(choices), "",
    "The final test season has already been examined during development. It is",
    "excluded from its model fits and tuning, but is not an untouched research holdout.",
    "Preseason API backfills and unverified historical line timestamps remain limits.",
    "Only a locked future publication ledger can test generalization for readers.", "")
  writeLines(lines, path)
  screening
}

run_controlled_ats_experiments <- function(config) {
  stopifnot(identical(config$version, "3.0.0"))
  protected <- experiment_file_hashes(config)
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  started <- Sys.time()
  output <- file.path(config$output_dir, "experiments", "controlled_ats",
                       format(started, "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (dir.exists(output)) stop("Experiment output already exists; do not overwrite a run.")
  dir.create(output, recursive = TRUE)
  file.copy(file.path(root, "ATS_PROTOCOL.md"), file.path(output, "protocol.md"))
  write_foundation_csv(protected, file.path(output, "protected_hashes_before.csv"))
  code <- c(file.path(root, c("ATS_PROTOCOL.md", "controlled_ats.R")),
             file.path(config$project_dir, "run_cfb_ats_experiments.R"))
  jsonlite::write_json(list(started_at = format(started, tz = "UTC", usetz = TRUE),
    version = config$version, experiment_code_md5 = as.list(tools::md5sum(code)),
    market_grid = c(8, 32, 128, 512, 2048, 8192), market_default = 2048,
    bootstrap_seed = 20260909, bootstrap_draws = 5000, no_automatic_promotion = TRUE),
    file.path(output, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  saveRDS(config, file.path(output, "control_config.rds"))

  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  training <- training[training$season <= 2025, ]
  stopifnot(all(training$feature_version == config$version))
  validation <- prepare_training_data(training, config)
  members <- combine_membership(read_csv_if_present(
    file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  validation$data <- apply_fbs_bridge_backtest_features(validation$data,
    team_games[team_games$season <= 2025, ], members[members$season <= 2025, ], config)
  priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
  priors <- priors[priors$season <= 2025, ]
  validation$data <- attach_preseason_features(validation$data, priors, config,
    features = config$preseason$production_features, prefix = "ps_")
  covered <- sort(unique(priors$season))
  fbs <- experiment_fbs_data(validation$data, validation$weights)
  coverage <- do.call(rbind, lapply(split(fbs$data, fbs$data$season), function(x) {
    data.frame(season = x$season[1], eligible_fbs_games = nrow(x),
      with_line = sum(is.finite(x$closing_home_spread)),
      with_total = sum(is.finite(x$market_total)))
  }))
  write_foundation_csv(coverage, file.path(output, "coverage.csv"))

  message("Reproducing the frozen v3 control")
  control <- experiment_football(validation$data, validation$weights, config, covered, "v3_control")
  frozen <- read_csv_if_present(file.path(config$output_dir, "backtest", "rolling_predictions.csv"), TRUE)
  frozen <- frozen[frozen$model == "ridge_core", ]
  assert_unique_keys(frozen, c("game_id", "season"), "frozen v3 predictions")
  index <- match(control$predictions$game_id, frozen$game_id)
  stopifnot(nrow(frozen) == nrow(control$predictions), !anyNA(index))
  baseline_delta <- max(abs(control$predictions$expected_margin - frozen$expected_margin[index]))
  if (baseline_delta > 1e-8) stop("Control no longer reproduces saved v3 predictions.")

  message("Experiment A: FBS-only margin fitting and selection")
  a <- experiment_football(fbs$data, fbs$weights, config, covered, "fbs_only")
  message("Experiment B: market residuals with football and market features")
  b <- experiment_market(fbs$data, fbs$weights, a$coach, config, covered)
  ids <- b$predictions$game_id
  common <- lapply(list(control$predictions, a$predictions, b$predictions), function(x) {
    assert_unique_keys(x, c("game_id", "season"), "experiment predictions")
    out <- x[match(ids, x$game_id), ]
    stopifnot(!anyNA(out$game_id), all(out$actual_margin == b$predictions$actual_margin),
      all(out$closing_home_spread == b$predictions$closing_home_spread))
    out
  })
  reference <- common[[3]]
  reference$model <- "market_reference"
  reference$expected_margin <- -reference$closing_home_spread
  predictions <- experiment_grade(do.call(rbind, c(common, list(reference))))
  metrics <- experiment_metrics(predictions)
  paired <- experiment_paired(predictions)
  choices <- rbind(control$choices, a$choices, b$choices)
  write_foundation_csv(predictions, file.path(output, "experiment_predictions.csv"))
  write_foundation_csv(metrics, file.path(output, "metrics.csv"))
  write_foundation_csv(paired, file.path(output, "paired_comparisons.csv"))
  write_foundation_csv(choices, file.path(output, "fold_choices.csv"))
  write_foundation_csv(b$grid_scores, file.path(output, "market_grid_scores.csv"))
  write_foundation_csv(data.frame(feature = b$feature_names), file.path(output, "market_features.csv"))
  report <- file.path(output, "report.md")
  screening <- experiment_report(metrics, paired, coverage, choices, baseline_delta, report)
  write_foundation_csv(screening, file.path(output, "screening.csv"))
  after <- experiment_file_hashes(config)
  write_foundation_csv(after, file.path(output, "protected_hashes_after.csv"))
  if (!identical(protected, after)) stop("Protected source/input/output hashes changed during the run.")
  jsonlite::write_json(list(completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    protected_files_unchanged = TRUE, control_max_margin_delta = baseline_delta,
    common_test_games = length(ids), status = "complete"),
    file.path(output, "verification.json"), auto_unbox = TRUE, pretty = TRUE)
  list(report = report, metrics = metrics[metrics$slice == "all_fbs", ], screening = screening)
}
