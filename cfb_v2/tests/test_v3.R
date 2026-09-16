library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R", "live_data.R", "dashboard.R")) source(file.path("cfb_v2", file))
config <- cfb_v2_config(getwd(), 2026)

test_that("snapshot identifiers use actual UTC regardless of input timezone", {
  eastern <- as.POSIXct("2026-09-08 20:51:24", tz = "America/New_York")
  expect_equal(v2_run_id("live", 2026, 2, eastern), "live_2026_w02_20260909T005124Z")
  expect_equal(v2_foundation_run_id(2020:2025, eastern),
               "foundation_2020_2025_20260909T005124Z")
})

test_that("dashboard discovery ignores newer audit and backtest tables", {
  directory <- tempfile(); dir.create(directory)
  card <- file.path(directory, "live_2026_2.csv")
  audit <- file.path(directory, "live_2026_2_features.csv")
  backtest <- file.path(directory, "rolling_predictions.csv")
  write_foundation_csv(data.frame(game_id = "g"), card)
  write_foundation_csv(data.frame(game_id = "g"), audit)
  write_foundation_csv(data.frame(game_id = "g"), backtest)
  Sys.setFileTime(audit, Sys.time() + 60)
  Sys.setFileTime(backtest, Sys.time() + 120)
  expect_equal(find_latest_prediction_csv(directory), card)
  stopifnot(startsWith(normalizePath(directory, winslash = "/"),
                       paste0(normalizePath(tempdir(), winslash = "/"), "/")))
  unlink(directory, recursive = TRUE)
})

test_that("foundation CSVs preserve accented source names across locales", {
  path <- tempfile(fileext = ".csv")
  name <- paste0("San Jos", intToUtf8(0xe9), " State")
  timestamp <- as.POSIXct("2026-09-08 20:51:24", tz = "America/New_York")
  write_foundation_csv(data.frame(team = name, as_of = timestamp), path)
  result <- read_csv_if_present(path, required = TRUE)
  expect_equal(canonical_team(result$team), "San Jose State")
  expect_equal(as.numeric(parse_utc_datetime(result$as_of)), as.numeric(timestamp))
  unlink(path)
})

test_that("undated games cannot enter a pregame power fit", {
  games <- standardize_schedule(data.frame(
    game_id = c("old", "undated", "target"), season = c(2025L, 2025L, 2026L),
    week = c(1L, 2L, 1L), model_week = c(1L, NA_integer_, 1L),
    home = "A", away = "B", margin = c(7, 1000, 3), completed = TRUE
  ))
  with_missing <- power_rating_snapshots(games, config)
  without_missing <- power_rating_snapshots(games[-2, ], config)
  expect_equal(with_missing, without_missing)
})

test_that("a promoted team's first game blends with a real transition prior", {
  games <- standardize_schedule(data.frame(
    game_id = c("played", "target"), season = 2026L, week = c(1L, 2L),
    model_week = c(2L, 3L), home = "New", away = "Old", neutral_site = FALSE,
    kickoff = as.POSIXct(c("2026-09-05", "2026-09-12"), tz = "UTC"),
    margin = c(7, NA), completed = c(TRUE, FALSE)
  ))
  tg <- data.frame(
    game_id = games$game_id, team = "New", season = 2026L, week = games$week,
    model_week = games$model_week, kickoff = games$kickoff,
    offense_epa = c(.1, NA), defense_epa = c(.1, NA),
    net_efficiency = c(.2, NA), success_rate = c(.44, NA)
  )
  power <- data.frame(team = "New", season = 2026L, model_week = c(2L, 3L),
                       power_rating = 0)
  priors <- data.frame(team = "New", season = 2026L, net_efficiency = -.1,
                       success_rate = .4, offense_epa = -.05, defense_epa = -.05)
  snapshots <- build_team_pregame_snapshots(tg, games, power, config, priors)$snapshots
  target <- snapshots[snapshots$game_id == "target", ]
  expect_equal(target$success_rate, .4*.44 + .6*.4)
  expect_equal(target$source_games, 1)
  expect_equal(target$prior_season, -.1)
  expect_equal(blend_current_with_history(.44, NA_real_, 2, config), .44)
})

test_that("historical and weekly builders produce identical model inputs", {
  schedule <- standardize_schedule(data.frame(
    game_id = "g", season = 2026L, week = 2L, model_week = 3L,
    home = "A", away = "B", neutral_site = FALSE
  ))
  snapshots <- data.frame(game_id = "g", team = c("A", "B"), season = 2026L,
                          week = 2L, model_week = 3L, home_field_rating = 2.4)
  for (metric in matchup_metric_names()) snapshots[[metric]] <- c(.3, .1)
  snapshots$games_played <- snapshots$source_games <- c(1L, 0L)
  historical <- build_historical_matchup_table(schedule, snapshots, config)
  weekly <- snapshots[setdiff(names(snapshots), c("game_id", "model_week"))]
  live <- make_matchup_features(schedule, weekly, weekly, config = config)
  features <- football_feature_names(historical)
  expect_true(all(features %in% names(live)))
  expect_equal(historical[features], live[features], ignore_attr = TRUE)
})

test_that("later plays cannot change an earlier turnover observation", {
  games <- data.frame(
    game_id = c("early", "late"), season = 2026L, week = 1:2, model_week = 1:2,
    kickoff = as.POSIXct(c("2026-08-29", "2026-09-05"), tz = "UTC"),
    home = "A", away = "B", home_score = 7, away_score = 0,
    neutral_site = FALSE, source_season_type = "regular", postseason_type = "regular"
  )
  plays <- data.frame(
    game_id = rep(games$game_id, each = 2), season = 2026L, year = 2026L,
    week = rep(1:2, each = 2), home = "A", away = "B", pos_team = "A",
    def_pos_team = "B", play_type = c("Rush", "Rush", "Interception Return", "Rush"),
    play_text = c("run", "run", "intercepted", "run"),
    EPA = .1, success = 0, rush = c(1, 1, 0, 1), pass = c(0, 0, 1, 0),
    sack = 0, turnover_indicator = c(0, 0, 1, 0), turnover = c(0, 0, 1, 0),
    stuffed_run = 0, garbage_time = FALSE, period = 1,
    pos_team_score = 0, def_pos_team_score = 0
  )
  before <- build_team_game_efficiencies_v2(plays[1:2, ], games[1, ])
  after <- build_team_game_efficiencies_v2(plays, games)
  index <- match(paste(before$game_id, before$team), paste(after$game_id, after$team))
  expect_equal(before$turnover_rate_regressed, after$turnover_rate_regressed[index])
})

test_that("preseason strength excludes FCS opponents and non-CFP bowls", {
  games <- standardize_schedule(data.frame(
    game_id = c("regular", "fcs", "bowl"), season = 2025L, week = 1:3,
    home = "A", away = "B", home_level = "fbs", away_level = c("fbs", "fcs", "fbs"),
    postseason_type = c("regular", "regular", "bowl"), is_cfp = FALSE
  ))
  tg <- data.frame(game_id = games$game_id, team = "A", season = 2025L,
                    net_efficiency = c(.1, 9, 20))
  expect_equal(eligible_team_strength(tg, games)$strength, .1)
})

test_that("the late-season power cap bounds total historical weight", {
  past <- standardize_schedule(data.frame(
    game_id = as.character(1:35), season = c(rep(2025L, 30), rep(2026L, 5)),
    week = 1L, home = "A", away = "B", margin = 7, completed = TRUE
  ))
  weight <- power_history_weights(past, 2026L, 8L, config)
  expect_lte(sum(weight[past$season < 2026]) / sum(weight), .1 + 1e-12)
  expect_equal(weight[past$season == 2026], rep(1, 5))
  expect_equal(sum(power_history_weights(past[1:30, ], 2026L, 8L, config)), 0)
})

test_that("future outcomes cannot choose earlier penalties, coaches, or uncertainty", {
  set.seed(39)
  d <- data.frame(season = rep(2020:2025, each = 40), week = 2L,
                  game_phase = "early_season", form_diff = rnorm(240),
                  coach_rating_65_35_diff = rnorm(240),
                  coach_rating_70_30_diff = rnorm(240))
  d$coach_rating_diff <- d$coach_rating_65_35_diff
  d$margin <- 4*d$form_diff + d$coach_rating_diff + rnorm(240, sd = 5)
  cfg <- config; cfg$model$ridge_lambda_grid <- c(2, 8)
  original <- select_coach_split_validation(d, rep(1, 240), cfg, fit_nonlinear = FALSE)
  d$margin[d$season == 2025] <- -100*d$margin[d$season == 2025]
  changed <- select_coach_split_validation(d, rep(1, 240), cfg, fit_nonlinear = FALSE)
  columns <- c("row_id", "test_season", "lambda", "expected_margin", "margin_sd",
               "coach_recent_share")
  expect_equal(original$rolling$predictions[columns], changed$rolling$predictions[columns])
  expect_equal(attr(original$data, "fold_coach_shares"),
               attr(changed$data, "fold_coach_shares"))
})

test_that("ATS thresholds are locked before the holdout and pushes never count", {
  set.seed(48)
  n <- 3200
  spread <- rep(c(-3, -7, -14, 3), length.out = n)
  edge <- rnorm(n, sd = 8)
  cover <- rbinom(n, 1, plogis(edge/6))
  data <- data.frame(season = rep(2022:2025, each = n/4),
                     actual_margin = -spread + ifelse(cover == 1, 7, -7),
                     expected_margin = -spread + edge, closing_home_spread = spread,
                     margin_sd = 14)
  first <- fit_validated_ats_layer(data, config)
  expect_true(!is.null(first$threshold))
  data$actual_margin[data$season == 2025] <-
    -2*data$closing_home_spread[data$season == 2025] - data$actual_margin[data$season == 2025]
  changed <- fit_validated_ats_layer(data, config)
  expect_equal(first$threshold[c("min_edge", "min_cover_probability")],
               changed$threshold[c("min_edge", "min_cover_probability")])
  expect_false(changed$threshold$validated)
  push <- data[1, ]; push$actual_margin <- -push$closing_home_spread
  expect_true(is.na(ats_threshold_rows(push, .8)$covered))
  expect_true(is.na(ats_threshold_rows(push, .2)$covered))
})

test_that("live coach ratings use destination power and the chronological cutoff", {
  history <- data.frame(coach_id = c("coach_a", "coach_b", "coach_a"),
                         season = c(2025L, 2025L, 2026L), week = c(99L, 99L, 2L),
                         games = c(10, 10, 1), wins = c(8, 5, 1),
                         above_expectation = c(5, 0, 10), level = "fbs",
                         context_strength = .1, target_context_strength = .1,
                         playoff_appearances = 0, titles = 0)
  assignments <- data.frame(team = c("A", "B"), season = 2026L, start_week = 0,
                             end_week = 99, coach_id = c("coach_a", "coach_b"),
                             coach_name = c("A", "B"))
  schedule <- standardize_schedule(data.frame(game_id = "g", season = 2026L,
                                               week = 2L, model_week = 3L,
                                               home = "A", away = "B"))
  team <- data.frame(team = c("A", "B"), season = 2026L, week = 2L,
                      power_rating = c(30, -30))
  actual <- attach_coach_ratings(schedule, team, assignments, history, .65, config)
  expected <- build_coach_ratings(history, 2026L, 3L, .65, config,
    target_context = coach_target_context(assignments$coach_id, team$power_rating, config))
  expect_equal(actual$coach_rating,
               expected$rating[match(actual$team, assignments$team)])
  expect_gt(expected$current_season_value[expected$coach_id == "coach_a"], 0)
  expect_lt(expected$portability[expected$coach_id == "coach_a"], .7)
})
