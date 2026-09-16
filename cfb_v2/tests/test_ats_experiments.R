library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R")) source(file.path("cfb_v2", file))
source(file.path("cfb_v3", "experiments", "controlled_ats.R"))
config <- cfb_v2_config(getwd(), 2026)

test_that("experiment A filters the learner without mutating the source", {
  data <- standardize_schedule(data.frame(game_id = as.character(1:4),
    season = 2025, week = 1, home = "A", away = "B", home_level = "fbs",
    away_level = c("fbs", "fcs", "fbs", "fbs"),
    postseason_type = c("regular", "regular", "bowl", "cfp"),
    is_cfp = c(FALSE, FALSE, FALSE, TRUE)))
  before <- data
  result <- experiment_fbs_data(data, c(1, .25, 1, .8))
  expect_equal(result$data$game_id, c("1", "4"))
  expect_equal(result$weights, c(1, .8))
  expect_identical(data, before)
})

test_that("ATS signs, pushes, abstentions and illustrative ROI are explicit", {
  data <- data.frame(expected_margin = c(10, 10, -10, 1, 0, 8),
    actual_margin = c(7, 10, -14, 10, 4, 0), closing_home_spread = c(-7, -7, 7, -7, 0, NA))
  data$absolute_error <- data$model_edge <- data$fair_margin <- 999
  data$winner_correct <- FALSE
  x <- experiment_grade(data)
  expect_equal(x$absolute_error, x$mae_error)
  expect_equal(x$winner_correct, x$su_correct)
  expect_equal(x$model_edge, x$edge)
  expect_equal(x$fair_margin, x$expected_margin)
  expect_equal(x$win, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(x$loss, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(x$push, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(x$ats_correct, c(NA, 1, 1, 0, NA, NA))
  x$model <- "test"; x$season <- 2025; x$week <- 1; x$game_phase <- "preseason"
  x$postseason_type <- "regular"; x$is_cfp <- FALSE
  x$home <- "A"; x$away <- "B"; x$home_conference <- "ACC"; x$away_conference <- "SEC"
  metrics <- experiment_metrics(x)
  main <- metrics[metrics$slice == "all_fbs", ]
  expect_equal(main$illustrative_roi, (2/1.1-1)/4)
  expect_equal(main$abstentions, 2L)
  expect_equal(main$ats_accuracy, 2/3)
})

test_that("penalty selection ignores current and later test outcomes", {
  scores <- expand.grid(test_season = 2022:2025, lambda = c(8, 2048))
  scores$absolute_error <- ifelse(scores$lambda == 2048, 1, 5)
  expect_equal(experiment_choose_lambda(scores, 2022, 2048), 2048)
  before <- experiment_choose_lambda(scores, 2023, 2048)
  scores$absolute_error[scores$test_season >= 2023 & scores$lambda == 2048] <- 1000
  expect_equal(experiment_choose_lambda(scores, 2023, 2048), before)
})

experiment_fixture <- function() {
  set.seed(195)
  n <- 240L
  week <- rep(c(1L, 2L, 5L, 8L), length.out = n)
  data <- data.frame(game_id = as.character(seq_len(n)), season = rep(2020:2025, each = 40),
    week = week, model_week = week, game_phase = game_phase(week),
    home = "A", away = "B", home_coach_id = "coach_a", away_coach_id = "coach_b",
    home_level = "fbs", away_level = "fbs", home_conference = "ACC", away_conference = "SEC",
    postseason_type = "regular", is_cfp = FALSE, neutral_site = FALSE,
    power_rating_diff = rnorm(n), form_diff = rnorm(n), home_field_points = 2.4,
    coach_rating_65_35_diff = rnorm(n), coach_rating_70_30_diff = rnorm(n),
    closing_home_spread = rnorm(n, sd = 10), market_total = 50 + rnorm(n, sd = 5))
  data$coach_rating_diff <- data$coach_rating_65_35_diff
  for (feature in config$preseason$production_features) {
    data[[paste0("ps_", feature, "_diff")]] <- rnorm(n)*preseason_feature_weight(week, config)
  }
  data$margin <- -data$closing_home_spread + 2*data$form_diff + rnorm(n, sd = 8)
  data$market_total[c(3, 84, 180)] <- NA_real_
  data$closing_home_spread[1] <- NA_real_
  data
}

test_that("market recipes impute from training data and exclude outcomes", {
  data <- experiment_market_inputs(experiment_fixture())
  train <- data[data$season <= 2021 & is.finite(data$market_margin), ]
  features <- c("form_diff", "market_margin", "market_spread_size", "market_total_input",
                 "market_total_missing")
  fit <- fit_weighted_ridge(train, "cover_residual", features, lambda = 2048)
  expect_equal(unname(fit$recipe$medians["market_total_input"]), median(train$market_total, na.rm = TRUE))
  test <- data[data$season == 2025, ]
  test$market_total <- NA_real_
  test <- experiment_market_inputs(test)
  expect_true(all(is.finite(predict(fit, test))))
  expect_equal(test$market_total_missing, rep(1, nrow(test)))
  expect_false(any(c("margin", "cover_residual", "home", "away") %in% fit$recipe$features))
})

test_that("changing the last season cannot change either challenger's predictions", {
  data <- experiment_fixture()
  cfg <- config; cfg$model$ridge_lambda_grid <- c(8, 32)
  original <- config
  a <- experiment_football(data, rep(1, nrow(data)), cfg, 2021:2025, "fbs_only")
  b <- experiment_market(data, rep(1, nrow(data)), a$coach, cfg, 2021:2025,
                         grid = c(32, 2048))
  data$margin[data$season == 2025] <- -25*data$margin[data$season == 2025]
  for (column in grep("^ps_", names(data), value = TRUE)) {
    data[[column]][data$season == 2025 & data$week >= 5] <- 999
  }
  altered_a <- experiment_football(data, rep(1, nrow(data)), cfg, 2021:2025, "fbs_only")
  altered_b <- experiment_market(data, rep(1, nrow(data)), altered_a$coach, cfg,
                                 2021:2025, grid = c(32, 2048))
  expect_equal(a$predictions$expected_margin, altered_a$predictions$expected_margin)
  expect_equal(b$predictions$expected_margin, altered_b$predictions$expected_margin)
  expect_equal(a$choices, altered_a$choices)
  expect_equal(b$choices, altered_b$choices)
  expect_true(all(b$choices$train_through_season < b$choices$test_season))
  expect_equal(b$choices$training_rows[1], 79L)
  expect_false(any(c("home", "away", "margin", "cover_residual") %in% b$feature_names))
  expect_identical(config, original)
})

test_that("the bootstrap resamples whole seasons and preserves constant deltas", {
  expect_equal(experiment_season_interval(rep(-.5, 40), rep(2022:2025, each = 10),
                                         replicates = 200), c(-.5, -.5))
  expect_true(all(is.na(experiment_season_interval(rep(1, 10), rep(2025, 10)))))
})
