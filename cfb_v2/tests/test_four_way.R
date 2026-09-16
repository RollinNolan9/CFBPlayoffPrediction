library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R", "models.R",
  "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R")) source(file.path("cfb_v2", file))
source("cfb_v3/experiments/controlled_ats.R")
source("cfb_v3/experiments/four_way.R")

four_way_fixture <- function() {
  data.frame(game_id = as.character(1:4), season = 2024L, week = 9L,
    home = c("A", "C", "E", "G"), away = c("B", "D", "F", "H"),
    margin = c(7, -3, 14, 10), kickoff = "2024-10-26T18:00:00Z",
    feature_week_start = "2024-10-21", home_pregame_elo = c(1600, 1500, NA, 1700),
    away_pregame_elo = c(1400, 1550, 1600, 1300), home_level = "fbs",
    away_level = c("fbs", "fbs", "fbs", "fcs"), postseason_type = "regular", is_cfp = FALSE,
    closing_home_spread = c(-7, NA, 10, -21), offense_epa_diff = c(.1, NA, .2, .3),
    defense_epa_diff = c(.1, -.1, NA, .4), power_rating_diff = c(5, 0, -6, 20),
    home_field_points = 2.4)
}

test_that("baseline features are explicit and Elo cannot enter current v3", {
  features <- four_way_features()
  expect_length(features, 3L)
  expect_equal(lengths(features), c(elo_baseline = 2L, internal_power = 2L, small_football = 4L))
  data <- four_way_fixture(); data$challenger_pregame_elo_diff <- 1
  expect_false("challenger_pregame_elo_diff" %in% football_feature_names(data))
  expect_false(any(unlist(features) %in% c("home", "away", "closing_home_spread", "fpi", "pff")))
})

test_that("common cohort enforces Elo without dropping missing lines or EPA", {
  data <- four_way_fixture()
  cohort <- four_way_cohort(data, rep(1, nrow(data)), data)
  expect_equal(cohort$data$game_id, c("1", "2"))
  expect_equal(cohort$data$challenger_pregame_elo_diff, c(200, -50))
  expect_true(is.na(cohort$data$closing_home_spread[2]))
  expect_true(is.na(cohort$data$offense_epa_diff[2]))
  expect_equal(cohort$coverage$reason, c("included", "included", "missing_pregame_elo", "not_eligible_fbs"))
  expect_error(four_way_cohort(data, rep(1, 4), rbind(data, data[1, ])))
  source <- data; source$home[1] <- "WRONG"
  expect_error(four_way_cohort(data, rep(1, 4), source), "metadata")
  source <- data; source$home_pregame_elo[1] <- 999
  expect_error(four_way_cohort(data, rep(1, 4), source), "Elo disagrees")
})

test_that("Elo timing gate rejects newer same-cycle information and unknown times", {
  data <- four_way_fixture()
  source <- data
  extra <- data[1, ]; extra$game_id <- "extra"; extra$kickoff <- "2024-10-24T18:00:00Z"
  source <- rbind(source, extra)
  cohort <- four_way_cohort(data, rep(1, 4), source)
  expect_equal(cohort$data$game_id, "2")
  expect_equal(cohort$coverage$reason[1], "cutoff_or_same_cycle_game")
  extra$kickoff <- "2024-10-20T18:00:00Z"
  expect_equal(four_way_cohort(data, rep(1, 4), rbind(data, extra))$data$game_id, c("1", "2"))
  source <- data; source$kickoff[1] <- NA
  expect_false("1" %in% four_way_cohort(data, rep(1, 4), source)$data$game_id)
  source <- data; source$kickoff[1] <- "2024-10-21T00:00:00Z"
  expect_false("1" %in% four_way_cohort(data, rep(1, 4), source)$data$game_id)
})

test_that("timing ledger displays explicit UTC regardless of local timezone", {
  old_tz <- Sys.getenv("TZ", unset = NA_character_)
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz))
  Sys.setenv(TZ = "America/Los_Angeles")
  data <- four_way_fixture()
  ledger <- four_way_cohort(data, rep(1, 4), data)$coverage
  expect_equal(ledger$cutoff, rep("2024-10-21T00:00:00Z", 4))
  expect_equal(ledger$kickoff, rep("2024-10-26T18:00:00Z", 4))
})

test_that("matched scoring fails closed on wrong rows or spreads", {
  x <- four_way_fixture(); x$actual_margin <- x$margin; x$expected_margin <- c(8, -5, -12, 23)
  x$model <- "a"
  y <- x[4:1, ]; y$model <- "b"
  matched <- four_way_match(list(x, y), x$game_id, x)
  expect_equal(matched$game_id, rep(x$game_id, 2))
  expect_equal(matched$push, rep(c(TRUE, FALSE, FALSE, FALSE), 2))
  expect_false(matched$selected[2])
  expect_true(matched$win[3] == FALSE)
  expect_error(four_way_match(list(x[-1, ]), x$game_id, x), "missing common")
  y$closing_home_spread[1] <- 99
  expect_error(four_way_match(list(y), x$game_id, x), "metadata mismatch")
})

test_that("future labels cannot alter earlier forecasts or outer penalty choices", {
  set.seed(7)
  data <- data.frame(season = rep(2020:2023, each = 30), game_phase = "in_season",
    power_rating_diff = rnorm(120), home_field_points = rep(c(0, 2.4), 60))
  data$margin <- 5*data$power_rating_diff + data$home_field_points + rnorm(120)
  config <- cfb_v2_config(getwd(), 2026L)
  run <- function(x) rolling_validate_ensemble(x, features = four_way_features()$internal_power,
    weights = rep(1, nrow(x)), config = config, fit_nonlinear = FALSE)$predictions
  first <- run(data)
  changed <- data; changed$margin[changed$season == 2023] <- 999
  second <- run(changed)
  expect_equal(first$expected_margin, second$expected_margin)
  expect_equal(first$lambda, second$lambda)
  changed$power_rating_diff[changed$season == 2023] <- -999
  third <- run(changed)
  expect_equal(first$expected_margin[first$test_season == 2022], third$expected_margin[third$test_season == 2022])
  expect_equal(first$lambda, third$lambda)
  expect_true(all(first$lambda[first$test_season == 2022] == 8))
})

test_that("ridge imputation is learned only from training rows", {
  train <- data.frame(x = c(NA, seq_len(29)), margin = seq_len(30))
  fit <- fit_weighted_ridge(train, "margin", "x", lambda = 8)
  expect_equal(unname(fit$recipe$medians), 15)
  expect_equal(predict(fit, data.frame(x = NA_real_)), predict(fit, data.frame(x = 15)))
  expect_equal(nrow(bake_numeric_recipe(fit$recipe, data.frame(x = c(NA, 10000)))), 2L)
})

test_that("whole-season intervals are reproducible and adjustment is wider", {
  delta <- c(-3, -2, 1, 2, -1, 0, 4, 5); seasons <- rep(2022:2025, each = 2)
  first <- four_way_intervals(delta, seasons, draws = 500)
  expect_equal(first, four_way_intervals(delta, seasons, draws = 500))
  expect_lte(first["adjusted_low"], first["low"])
  expect_gte(first["adjusted_high"], first["high"])
  expect_true(all(is.na(four_way_intervals(1:3, rep(2022, 3)))))
})
