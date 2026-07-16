fit_numeric_recipe <- function(data, features) {
  assert_columns(data, features, "training data")
  medians <- vapply(data[features], function(x) {
    value <- stats::median(as.numeric(x), na.rm = TRUE)
    ifelse(is.finite(value), value, 0)
  }, numeric(1))
  imputed <- as.data.frame(Map(function(x, m) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- m
    x
  }, data[features], medians), check.names = FALSE)
  means <- vapply(imputed, mean, numeric(1))
  scales <- vapply(imputed, stats::sd, numeric(1))
  scales[!is.finite(scales) | scales < 1e-8] <- 1
  list(features = features, medians = medians, means = means, scales = scales)
}

bake_numeric_recipe <- function(recipe, new_data) {
  missing <- setdiff(recipe$features, names(new_data))
  if (length(missing)) {
    stop("Prediction data is missing model features: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  x <- as.data.frame(Map(function(value, median, center, scale) {
    value <- as.numeric(value)
    value[!is.finite(value)] <- median
    (value - center) / scale
  }, new_data[recipe$features], recipe$medians, recipe$means, recipe$scales),
  check.names = FALSE)
  as.matrix(x)
}

fit_weighted_ridge <- function(data, target, features, weights = NULL, lambda = 8,
                               minimum_rows = NULL) {
  assert_columns(data, c(target, features), "training data")
  y <- as.numeric(data[[target]])
  if (is.null(weights)) weights <- rep(1, nrow(data))
  keep <- is.finite(y) & is.finite(weights) & weights > 0
  if (is.null(minimum_rows)) minimum_rows <- max(20, length(features) + 3)
  if (sum(keep) < minimum_rows) {
    stop("Not enough eligible rows to fit the ridge margin model.", call. = FALSE)
  }
  recipe <- fit_numeric_recipe(data[keep, , drop = FALSE], features)
  x <- bake_numeric_recipe(recipe, data[keep, , drop = FALSE])
  design <- cbind(`(Intercept)` = 1, x)
  w <- weights[keep]
  penalty <- diag(c(0, rep(lambda, ncol(x))))
  lhs <- crossprod(design * sqrt(w)) + penalty
  rhs <- crossprod(design * sqrt(w), y[keep] * sqrt(w))
  coefficients <- tryCatch(
    as.numeric(solve(lhs, rhs)),
    error = function(e) as.numeric(qr.solve(lhs, rhs, tol = 1e-9))
  )
  names(coefficients) <- colnames(design)
  structure(list(
    target = target, recipe = recipe, coefficients = coefficients,
    lambda = lambda, training_rows = sum(keep)
  ), class = "cfb_ridge")
}

predict.cfb_ridge <- function(object, newdata, ...) {
  x <- bake_numeric_recipe(object$recipe, newdata)
  as.numeric(cbind(1, x) %*% object$coefficients)
}

fit_residual_forest <- function(data, residual, features, weights, config) {
  if (!requireNamespace("randomForest", quietly = TRUE) ||
      nrow(data) < config$model$minimum_training_rows || length(features) < 2) {
    return(NULL)
  }
  recipe <- fit_numeric_recipe(data, features)
  x <- as.data.frame(bake_numeric_recipe(recipe, data), check.names = FALSE)
  names(x) <- features
  probability <- pmax(0, weights)
  probability <- probability / sum(probability)
  set.seed(config$seed)
  sampled <- sample(seq_len(nrow(data)), size = nrow(data), replace = TRUE,
                    prob = probability)
  forest <- randomForest::randomForest(
    x = x[sampled, , drop = FALSE], y = residual[sampled],
    ntree = config$model$residual_trees,
    mtry = max(1L, floor(sqrt(length(features)))), nodesize = 12
  )
  list(model = forest, recipe = recipe, features = features)
}

predict_residual_forest <- function(object, new_data) {
  if (is.null(object)) return(rep(0, nrow(new_data)))
  x <- as.data.frame(bake_numeric_recipe(object$recipe, new_data), check.names = FALSE)
  names(x) <- object$features
  as.numeric(stats::predict(object$model, x))
}

margin_bucket <- function(x) {
  cut(abs(x), breaks = c(-Inf, 3, 7, 14, 21, Inf),
      labels = c("0-3", "3.5-7", "7.5-14", "14.5-21", "21+"),
      right = TRUE)
}

fit_error_calibration <- function(predicted, actual, phase, config) {
  residual <- actual - predicted
  groups <- interaction(as.character(phase), margin_bucket(predicted), drop = TRUE)
  split_residual <- split(residual, groups)
  rows <- lapply(names(split_residual), function(group) {
    value <- split_residual[[group]]
    data.frame(
      group = group,
      median_adjustment = stats::median(value, na.rm = TRUE),
      margin_sd = max(config$model$uncertainty_floor, stats::sd(value, na.rm = TRUE)),
      n = sum(is.finite(value)), stringsAsFactors = FALSE
    )
  })
  calibration <- if (length(rows)) do.call(rbind, rows) else data.frame()
  global_sd <- stats::sd(residual, na.rm = TRUE)
  if (!is.finite(global_sd)) global_sd <- config$model$uncertainty_floor
  list(groups = calibration, global_median = stats::median(residual, na.rm = TRUE),
       global_sd = max(config$model$uncertainty_floor, global_sd))
}

apply_error_calibration <- function(calibration, predicted, phase) {
  group <- as.character(interaction(as.character(phase), margin_bucket(predicted),
                                    drop = TRUE))
  index <- match(group, calibration$groups$group)
  adjustment <- calibration$groups$median_adjustment[index]
  uncertainty <- calibration$groups$margin_sd[index]
  adjustment[!is.finite(adjustment)] <- calibration$global_median
  uncertainty[!is.finite(uncertainty)] <- calibration$global_sd
  list(fair_margin = predicted + adjustment, margin_sd = uncertainty)
}

fit_cfb_ensemble <- function(data, target = "margin", features = NULL, weights = NULL,
                             lambda = 8, config, fit_nonlinear = TRUE) {
  if (is.null(features)) features <- football_feature_names(data)
  if (!length(features)) stop("No eligible pure-football features were found.", call. = FALSE)
  prohibited <- features[
    tolower(features) %in% c("team", "team_id", "home", "away", "conference",
                             "home_conference", "away_conference") |
      grepl("spread|line|market|fpi|pff|(^|_)rank($|_)", features, ignore.case = TRUE)
  ]
  if (length(prohibited)) {
    stop("Pure football model contains prohibited features: ",
         paste(prohibited, collapse = ", "), call. = FALSE)
  }
  if (is.null(weights)) weights <- rep(1, nrow(data))
  ridge <- fit_weighted_ridge(data, target, features, weights, lambda)
  base <- predict(ridge, data)
  residual <- as.numeric(data[[target]]) - base
  forest <- if (fit_nonlinear) {
    fit_residual_forest(data, residual, features, weights, config)
  } else NULL
  expected <- base + predict_residual_forest(forest, data)
  phase <- if ("game_phase" %in% names(data)) data$game_phase else game_phase(data$week)
  calibration <- fit_error_calibration(expected, data[[target]], phase, config)
  structure(list(
    version = config$version, target = target, features = features,
    ridge = ridge, residual_forest = forest, calibration = calibration,
    trained_rows = nrow(data), trained_seasons = sort(unique(data$season)),
    training_residuals = as.numeric(data[[target]]) - expected
  ), class = "cfb_ensemble")
}

predict.cfb_ensemble <- function(object, newdata, ...) {
  base <- predict(object$ridge, newdata)
  nonlinear <- predict_residual_forest(object$residual_forest, newdata)
  expected <- base + nonlinear
  phase <- if ("game_phase" %in% names(newdata)) newdata$game_phase else game_phase(newdata$week)
  calibrated <- apply_error_calibration(object$calibration, expected, phase)
  data.frame(
    base_margin = base, nonlinear_adjustment = nonlinear,
    expected_margin = expected, fair_margin = calibrated$fair_margin,
    margin_sd = calibrated$margin_sd, stringsAsFactors = FALSE
  )
}

ridge_feature_contributions <- function(model, new_data) {
  x <- bake_numeric_recipe(model$ridge$recipe, new_data)
  beta <- model$ridge$coefficients[-1]
  sweep(x, 2, beta, `*`)
}

ridge_feature_drivers <- function(model, new_data, top_n = 4L) {
  contributions <- ridge_feature_contributions(model, new_data)
  apply(contributions, 1, function(row) {
    index <- head(order(abs(row), decreasing = TRUE), top_n)
    paste(sprintf("%s:%+.2f", colnames(contributions)[index], row[index]), collapse = "; ")
  })
}

select_ats_threshold <- function(validation, config) {
  assert_columns(validation, c("edge", "cover_probability", "covered"), "ATS validation")
  candidates <- config$ats$threshold_grid
  scored <- lapply(seq_len(nrow(candidates)), function(i) {
    threshold <- candidates[i, ]
    selected <- abs(validation$edge) >= threshold$min_edge &
      pmax(validation$cover_probability, 1 - validation$cover_probability) >=
      threshold$min_cover_probability
    picks <- sum(selected, na.rm = TRUE)
    accuracy <- if (picks) mean(validation$covered[selected], na.rm = TRUE) else NA_real_
    data.frame(candidates[i, ], picks = picks, accuracy = accuracy,
               score = ifelse(picks >= 20, accuracy - 0.002 / sqrt(picks), -Inf))
  })
  scored <- do.call(rbind, scored)
  scored[which.max(scored$score), , drop = FALSE]
}

fit_ats_residual_model <- function(validation, minimum_rows = 100L) {
  assert_columns(validation,
                 c("actual_margin", "expected_margin", "closing_home_spread", "margin_sd"),
                 "ATS residual training")
  keep <- is.finite(validation$actual_margin) & is.finite(validation$expected_margin) &
    is.finite(validation$closing_home_spread) & is.finite(validation$margin_sd) &
    validation$actual_margin + validation$closing_home_spread != 0
  data <- validation[keep, , drop = FALSE]
  if (nrow(data) < minimum_rows) return(NULL)
  data$home_cover <- as.integer(data$actual_margin + data$closing_home_spread > 0)
  data$edge_z <- (data$expected_margin + data$closing_home_spread) /
    pmax(5, data$margin_sd)
  data$edge_z_abs_spread <- data$edge_z * abs(data$closing_home_spread) / 14
  fit <- stats::glm(home_cover ~ 0 + edge_z + edge_z_abs_spread,
                    data = data, family = stats::binomial())
  coefficients <- stats::coef(fit)
  max_spread_scale <- max(abs(data$closing_home_spread), na.rm = TRUE) / 14
  effective_slope <- coefficients[["edge_z"]] +
    coefficients[["edge_z_abs_spread"]] * c(0, max_spread_scale)
  if (any(!is.finite(effective_slope)) || min(effective_slope) <= 0) return(NULL)
  structure(list(model = fit, training_rows = nrow(data)), class = "cfb_ats_residual")
}

predict_ats_home_cover <- function(object, expected_margin, home_spread, margin_sd) {
  fallback <- stats::pnorm((expected_margin + home_spread) / margin_sd)
  if (is.null(object)) return(fallback)
  edge_z <- (expected_margin + home_spread) / pmax(5, margin_sd)
  new_data <- data.frame(
    edge_z = edge_z,
    edge_z_abs_spread = edge_z * abs(home_spread) / 14
  )
  calibrated <- suppressWarnings(as.numeric(stats::predict(object$model, new_data,
                                                            type = "response")))
  coefficients <- stats::coef(object$model)
  effective_slope <- coefficients[["edge_z"]] +
    coefficients[["edge_z_abs_spread"]] * abs(home_spread) / 14
  invalid <- !is.finite(calibrated) | !is.finite(effective_slope) | effective_slope <= 0
  calibrated[invalid] <- fallback[invalid]
  pmax(.01, pmin(.99, calibrated))
}

make_game_picks <- function(prediction, schedule, market_home_spread = NA_real_,
                            force_pick = FALSE, ats_threshold = NULL, ats_model = NULL,
                            config) {
  if (is.null(ats_threshold)) ats_threshold <- config$ats$threshold_grid[2, ]
  expected <- prediction$expected_margin
  fair <- prediction$fair_margin
  sd <- pmax(config$model$uncertainty_floor, prediction$margin_sd)
  home_win <- stats::pnorm(expected / sd)
  market <- as.numeric(market_home_spread)
  edge <- expected + market
  home_cover <- predict_ats_home_cover(ats_model, expected, market, sd)
  has_market <- is.finite(market)
  preferred_home <- home_cover >= 0.5
  ats_pick <- ifelse(!has_market, NA_character_,
                     ifelse(preferred_home, schedule$home, schedule$away))
  qualifies <- has_market & abs(edge) >= ats_threshold$min_edge &
    pmax(home_cover, 1 - home_cover) >= ats_threshold$min_cover_probability
  status <- ifelse(!has_market, "no_line", ifelse(qualifies, "official_pick", "pass"))
  status[force_pick & has_market & !qualifies] <- "forced_model_pick"
  ats_pick[status == "pass"] <- NA_character_
  straight_up <- ifelse(home_win >= 0.5, schedule$home, schedule$away)
  confidence_probability <- pmax(home_win, 1 - home_win)
  confidence <- ifelse(confidence_probability >= 0.75, "high",
                       ifelse(confidence_probability >= 0.62, "medium", "low"))
  data.frame(
    expected_margin = expected, fair_spread = -fair, margin_sd = sd,
    home_win_probability = home_win, market_home_spread = market,
    ats_edge_home = edge, home_cover_probability = home_cover,
    straight_up_pick = straight_up, ats_pick = ats_pick, pick_status = status,
    confidence_tier = confidence, forced_pick = force_pick,
    stringsAsFactors = FALSE
  )
}

predict_week <- function(model, schedule, matchup_features, market_home_spread = NULL,
                         force_game_ids = character(), injuries = NULL,
                         ats_threshold = NULL, ats_model = NULL, config) {
  schedule <- standardize_schedule(schedule)
  assert_unique_keys(matchup_features, "game_id", "matchup_features")
  matchup_features <- matchup_features[match(schedule$game_id, matchup_features$game_id), ]
  if (any(is.na(matchup_features$game_id))) {
    stop("Every scheduled game must have exactly one matchup feature row.", call. = FALSE)
  }
  predictions <- predict(model, matchup_features)
  injury_details <- rep("most_likely", nrow(schedule))
  if (!is.null(injuries) && nrow(injuries)) {
    assert_columns(injuries, "game_id", "injuries")
    for (i in seq_len(nrow(schedule))) {
      game_injuries <- injuries[injuries$game_id == schedule$game_id[i], , drop = FALSE]
      if (!nrow(game_injuries)) next
      scenarios <- build_injury_scenarios(
        game_injuries, predictions$expected_margin[i], schedule$home[i], schedule$away[i], config
      )
      adjustment <- scenarios$margin_adjustment[scenarios$scenario == "most_likely"][1]
      if (!is.finite(adjustment)) adjustment <- 0
      predictions$expected_margin[i] <- predictions$expected_margin[i] + adjustment
      predictions$fair_margin[i] <- predictions$fair_margin[i] + adjustment
      scenario_spread <- stats::sd(scenarios$expected_margin, na.rm = TRUE)
      if (is.finite(scenario_spread)) {
        predictions$margin_sd[i] <- sqrt(predictions$margin_sd[i]^2 + scenario_spread^2)
      }
      injury_details[i] <- paste(
        sprintf("%s=%+.1f", scenarios$scenario, scenarios$expected_margin),
        collapse = "; "
      )
    }
  }
  if (is.null(market_home_spread)) market_home_spread <- rep(NA_real_, nrow(schedule))
  picks <- make_game_picks(
    predictions, schedule, market_home_spread,
    force_pick = schedule$game_id %in% force_game_ids,
    ats_threshold = ats_threshold, ats_model = ats_model, config = config
  )
  picks$game_id <- schedule$game_id
  picks$home <- schedule$home
  picks$away <- schedule$away
  picks$season <- schedule$season
  picks$week <- schedule$week
  picks$top_drivers <- ridge_feature_drivers(model, matchup_features)
  picks$injury_scenario <- injury_details

  if (!is.null(injuries) && nrow(injuries)) {
    conflict_games <- unique(injuries$game_id[
      "conflict_flag" %in% names(injuries) & as.logical(injuries$conflict_flag)
    ])
    picks$pick_status[picks$game_id %in% conflict_games &
                        picks$pick_status %in% c("official_pick", "forced_model_pick")] <-
      "injury_conflict_review"
  }
  picks
}

fit_cfp_bye_adjustment <- function(history, lambda = 40) {
  assert_columns(history,
                 c("margin_residual", "first_round_bye", "rest_days_diff",
                   "played_round_one", "conference_championship", "opponent_quality_diff",
                   "travel_timezone_diff"), "CFP bye history")
  features <- setdiff(names(history), "margin_residual")
  model <- fit_weighted_ridge(history, "margin_residual", features,
                              weights = rep(1, nrow(history)), lambda = lambda,
                              minimum_rows = length(features) + 2L)
  x <- bake_numeric_recipe(model$recipe, history)
  design <- cbind(1, x)
  residual <- history$margin_residual - predict(model, history)
  sigma2 <- sum(residual^2) / max(1, nrow(history) - ncol(design))
  penalty <- diag(c(0, rep(lambda, ncol(x))))
  covariance <- sigma2 * solve(crossprod(design) + penalty)
  bye_index <- match("first_round_bye", names(model$coefficients))
  list(
    model = model,
    bye_adjustment = model$coefficients[bye_index] /
      model$recipe$scales[["first_round_bye"]],
    standard_error = sqrt(covariance[bye_index, bye_index]) /
      model$recipe$scales[["first_round_bye"]],
    sample_size = nrow(history)
  )
}

rolling_season_splits <- function(data, minimum_train_seasons = 2L) {
  seasons <- sort(unique(as.integer(data$season)))
  test_seasons <- seasons[seq_along(seasons) > minimum_train_seasons]
  lapply(test_seasons, function(season) {
    list(train = which(data$season < season), test = which(data$season == season),
         test_season = season)
  })
}

rolling_validate_ensemble <- function(data, target = "margin", features = NULL,
                                      weights = NULL, config, fit_nonlinear = TRUE) {
  if (is.null(features)) features <- football_feature_names(data)
  if (is.null(weights)) weights <- rep(1, nrow(data))
  splits <- rolling_season_splits(data)
  if (!length(splits)) stop("Rolling validation requires at least three seasons.", call. = FALSE)
  results <- list()
  k <- 1L
  for (lambda in config$model$ridge_lambda_grid) {
    for (split in splits) {
      model <- fit_cfb_ensemble(
        data[split$train, , drop = FALSE], target, features,
        weights[split$train], lambda, config, fit_nonlinear
      )
      prediction <- predict(model, data[split$test, , drop = FALSE])
      actual <- data[[target]][split$test]
      results[[k]] <- data.frame(
        row_id = split$test, test_season = split$test_season, lambda = lambda,
        actual = actual, expected_margin = prediction$expected_margin,
        fair_margin = prediction$fair_margin,
        margin_sd = prediction$margin_sd,
        error = actual - prediction$expected_margin,
        absolute_error = abs(actual - prediction$expected_margin),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  predictions <- do.call(rbind, results)
  score <- aggregate(absolute_error ~ lambda, predictions, mean)
  best_lambda <- score$lambda[which.min(score$absolute_error)]
  list(best_lambda = best_lambda, scores = score,
       predictions = predictions[predictions$lambda == best_lambda, ])
}

spread_bin_metrics <- function(actual_margin, predicted_margin, market_home_spread = NULL) {
  bins <- margin_bucket(if (is.null(market_home_spread)) predicted_margin else market_home_spread)
  data <- data.frame(
    bin = bins, absolute_error = abs(actual_margin - predicted_margin),
    winner_correct = sign(actual_margin) == sign(predicted_margin)
  )
  aggregate(cbind(absolute_error, winner_correct) ~ bin, data, mean, na.rm = TRUE)
}

grade_prediction_card <- function(predictions, outcomes, closing_lines) {
  assert_columns(predictions, c("game_id", "home", "away", "expected_margin",
                                "straight_up_pick", "ats_pick", "pick_status"),
                 "predictions")
  assert_columns(outcomes, c("game_id", "home", "away", "home_score", "away_score"),
                 "outcomes")
  assert_columns(closing_lines, c("game_id", "closing_home_spread"), "closing_lines")
  data <- merge(predictions, outcomes, by = "game_id", suffixes = c("", "_outcome"))
  data <- merge(data, closing_lines, by = "game_id", all.x = TRUE)
  data$actual_margin <- data$home_score - data$away_score
  data$actual_winner <- ifelse(data$actual_margin >= 0, data$home_outcome, data$away_outcome)
  data$straight_up_correct <- data$straight_up_pick == data$actual_winner
  home_cover_margin <- data$actual_margin + data$closing_home_spread
  data$ats_result <- ifelse(
    !is.finite(home_cover_margin) | home_cover_margin == 0 | is.na(data$ats_pick), NA,
    ifelse(data$ats_pick == data$home_outcome, home_cover_margin > 0, home_cover_margin < 0)
  )
  data$margin_absolute_error <- abs(data$actual_margin - data$expected_margin)
  groups <- list(
    all_projections = rep(TRUE, nrow(data)),
    qualifying_picks = data$pick_status == "official_pick",
    forced_model_picks = data$pick_status == "forced_model_pick"
  )
  summary <- do.call(rbind, lapply(names(groups), function(group) {
    keep <- groups[[group]]
    data.frame(
      group = group, games = sum(keep),
      straight_up_accuracy = mean(data$straight_up_correct[keep], na.rm = TRUE),
      margin_mae = mean(data$margin_absolute_error[keep], na.rm = TRUE),
      ats_accuracy = mean(data$ats_result[keep], na.rm = TRUE),
      ats_graded = sum(!is.na(data$ats_result[keep])), stringsAsFactors = FALSE
    )
  }))
  list(games = data, summary = summary)
}

model_diagnostic_slices <- function(data) {
  assert_columns(data, c("home", "away", "actual_margin", "expected_margin"),
                 "diagnostic data")
  teams <- paste(data$home, data$away, sep = "|")
  slices <- list(
    historic_powers_in_decline = grepl("Alabama|Clemson", teams),
    rapid_risers = grepl("Indiana|SMU", teams),
    group_of_five = if ("g5_game" %in% names(data)) as.logical(data$g5_game) else FALSE,
    neutral_site = if ("neutral_site" %in% names(data)) as.logical(data$neutral_site) else FALSE,
    playoff = if ("is_cfp" %in% names(data)) as.logical(data$is_cfp) else FALSE
  )
  do.call(rbind, lapply(names(slices), function(slice) {
    keep <- slices[[slice]]
    data.frame(
      slice = slice, games = sum(keep, na.rm = TRUE),
      margin_mae = if (any(keep, na.rm = TRUE))
        mean(abs(data$actual_margin[keep] - data$expected_margin[keep]), na.rm = TRUE) else NA,
      winner_accuracy = if (any(keep, na.rm = TRUE))
        mean(sign(data$actual_margin[keep]) == sign(data$expected_margin[keep]), na.rm = TRUE) else NA,
      stringsAsFactors = FALSE
    )
  }))
}

simulate_matchup <- function(expected_margin, expected_total, margin_sd, total_sd,
                             correlation = 0.10, simulations = 20000L, seed = 20260712L) {
  set.seed(seed)
  z_margin <- stats::rnorm(simulations)
  z_total <- correlation * z_margin + sqrt(1 - correlation^2) * stats::rnorm(simulations)
  margin <- expected_margin + margin_sd * z_margin
  total <- expected_total + total_sd * z_total
  data.frame(home_score = pmax(0, (total + margin) / 2),
             away_score = pmax(0, (total - margin) / 2), margin = margin,
             total = total)
}

simulate_cfp_bracket <- function(bracket, matchup_predictor, simulations = 10000L,
                                 seed = 20260712L) {
  assert_columns(bracket, c("round", "game_id", "home", "away"), "bracket")
  set.seed(seed)
  teams <- sort(unique(c(bracket$home, bracket$away)))
  titles <- setNames(integer(length(teams)), teams)
  advances <- setNames(integer(length(teams)), teams)
  for (simulation in seq_len(simulations)) {
    current <- bracket
    rounds <- sort(unique(current$round))
    last_winners <- character()
    for (round in rounds) {
      games <- current[current$round == round, , drop = FALSE]
      if (round > min(rounds) && length(last_winners) == 2 * nrow(games)) {
        games$home <- last_winners[seq(1, length(last_winners), 2)]
        games$away <- last_winners[seq(2, length(last_winners), 2)]
      }
      winners <- character(nrow(games))
      for (i in seq_len(nrow(games))) {
        distribution <- matchup_predictor(games[i, , drop = FALSE])
        margin <- stats::rnorm(1, distribution$expected_margin, distribution$margin_sd)
        winners[i] <- if (margin >= 0) games$home[i] else games$away[i]
        advances[winners[i]] <- advances[winners[i]] + 1L
      }
      last_winners <- winners
    }
    if (length(last_winners) == 1) titles[last_winners] <- titles[last_winners] + 1L
  }
  data.frame(team = teams, advancement_events = as.integer(advances),
             championship_probability = as.integer(titles) / simulations,
             stringsAsFactors = FALSE)
}

predict_cfp_bracket <- function(bracket, matchup_predictor) {
  assert_columns(bracket, c("round", "game_id", "home", "away"), "bracket")
  rounds <- sort(unique(bracket$round))
  current_winners <- character()
  results <- list()
  k <- 1L
  for (round in rounds) {
    games <- bracket[bracket$round == round, , drop = FALSE]
    if (round > min(rounds) && length(current_winners) == 2 * nrow(games)) {
      games$home <- current_winners[seq(1, length(current_winners), 2)]
      games$away <- current_winners[seq(2, length(current_winners), 2)]
    }
    winners <- character(nrow(games))
    for (i in seq_len(nrow(games))) {
      prediction <- matchup_predictor(games[i, , drop = FALSE])
      winners[i] <- if (prediction$expected_margin >= 0) games$home[i] else games$away[i]
      results[[k]] <- data.frame(
        round = round, game_id = games$game_id[i], home = games$home[i],
        away = games$away[i], expected_margin = prediction$expected_margin,
        winner = winners[i], stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
    current_winners <- winners
  }
  do.call(rbind, results)
}
