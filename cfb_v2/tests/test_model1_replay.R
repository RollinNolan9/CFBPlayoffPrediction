library(testthat)
suppressPackageStartupMessages(library(dplyr))
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R", "live_data.R")) source(file.path("cfb_v2", file))
source("cfb_v3/experiments/model1_replay.R")

test_that("the archived six-game source is pinned instead of the local notebook", {
  src <- model1_source(getwd())
  expect_true(any(grepl("load_cfb_pbp(2014:2021)", src$lines, fixed = TRUE)))
  expect_true(any(grepl("mtry=c(6)", src$lines, fixed = TRUE)))
  expect_equal(model1_assignment(quote(train_dat <- x)), "train_dat")
  expect_equal(model1_assignment(quote(train_dat$x <- x)), "train_dat")
  expect_equal(model1_assignment(quote(system("bad"))), "")
})

test_that("frozen inputs reject same-time and later games and missing timestamps", {
  p <- data.frame(game_id = c("before", "at", "after"), year = 2024L,
    start_date = c("2024-12-20T12:00:00Z", "2024-12-21T01:00:00Z", "2025-01-01T00:00:00Z"), EPA = 1:3)
  cutoff <- parse_utc_datetime("2024-12-21T01:00:00Z")
  before <- model1_freeze_pbp(p, cutoff, c("at", "after"))
  expect_equal(before$game_id, "before")
  p$EPA[2:3] <- 9999
  expect_equal(model1_freeze_pbp(p, cutoff, c("at", "after")), before)
  p$start_date[1] <- NA
  expect_error(model1_freeze_pbp(p, cutoff, c("at", "after")), "Missing")
})

test_that("date recovery cannot silently omit a CFP participant", {
  p <- data.frame(game_id = c("known", "unknown"), year = 2024L,
    home = c("Notre Dame", "Montana State"), away = c("Indiana", "South Dakota"), start_date = NA_character_)
  games <- data.frame(game_id = "known", kickoff = "2024-12-21T01:00:00Z")
  resolved <- model1_resolve_dates(p, games, c("Notre Dame", "Indiana"))
  expect_equal(resolved$pbp$game_id, "known")
  expect_equal(nrow(resolved$audit), 2L)
  p$home[2] <- "Notre Dame"
  expect_error(model1_resolve_dates(p, games, c("Notre Dame", "Indiana")), "Unresolved")
})

test_that("first-round upsets propagate through byes and semifinals", {
  bracket <- model1_bracket()
  picks <- model1_advance_bracket(bracket, function(game) list(expected_margin = -7))
  expect_equal(picks$away[5:8], c("SMU", "Clemson", "Tennessee", "Indiana"))
  expect_equal(picks$home[9:11], c("SMU", "Clemson", "Indiana"))
  expect_equal(picks$away[9:11], c("Indiana", "Tennessee", "Tennessee"))
  expect_equal(tail(picks$winner, 1), "Tennessee")
  reversed <- bracket[c(5, 1:4, 6:11), ]
  expect_error(model1_advance_bracket(reversed, function(game) list(expected_margin = 1)))
})

test_that("grading a full bracket does not supply actual later opponents", {
  bracket <- model1_bracket()
  picks <- model1_advance_bracket(bracket, function(game) list(expected_margin = -7))
  actual <- picks[c("game_id", "home", "away")]
  actual$margin <- -3
  actual$away[1] <- "Replacement"
  graded <- model1_grade_bracket(picks, actual)
  expect_equal(sum(graded$correct), 10L)
  expect_equal(graded$away[8], "Indiana")
  expect_false(graded$actual_matchup[1])
})

predict.model1_fixture <- function(object, newdata, ...) ifelse(newdata$home == "A", 10, -4)
test_that("legacy neutral averaging reverses the swapped margin sign", {
  src <- model1_source(getwd())
  empty <- data.frame(year = integer())
  inputs <- list(sp = empty, elo = empty, team_names = data.frame(raw = c("A", "B"), canonical = c("A", "B")))
  model <- structure(list(), class = "model1_fixture")
  data <- data.frame(home = c("A", "B"), away = c("B", "A"), year = 2024L, week = 15L)
  for (name in c("epa_per_play_last_n", "epa_per_pass_last_n", "epa_per_rush_last_n",
    "wpa_per_play_last_n", "sp_rating", "offense_rating", "defense_rating", "elo_rating")) {
    data[[paste0("home_team_", name)]] <- 1
    data[[paste0("away_team_", name)]] <- 1
  }
  game <- data.frame(home = "A", away = "B", neutral_site = TRUE)
  expect_equal(model1_predict_matchup(model, data, game, src, inputs)$expected_margin, 7)
  game$home <- "B"; game$away <- "A"
  expect_equal(model1_predict_matchup(model, data, game, src, inputs)$expected_margin, -7)
  game$neutral_site <- FALSE
  expect_equal(model1_predict_matchup(model, data, game, src, inputs)$expected_margin, -4)
})

test_that("completed replay artifacts agree with their cutoff and grading audit", {
  directories <- list.dirs("cfb_v3/output/experiments/model1_replay", recursive = FALSE)
  complete <- directories[file.exists(file.path(directories, "verification.json"))]
  skip_if(!length(complete), "Run the replay to generate integration artifacts.")
  directory <- tail(sort(complete), 1)
  audit <- jsonlite::fromJSON(file.path(directory, "verification.json"))
  expect_true(audit$production_unchanged)
  expect_lt(audit$v3_first_round_max_delta, 1e-8)
  cutoff <- parse_utc_datetime(audit$pbp_cutoff_exclusive)
  expect_length(audit$v3_history_last_kickoff, 1L)
  expect_lt(parse_utc_datetime(audit$v3_history_last_kickoff), cutoff)
  expect_lt(parse_utc_datetime(audit$legacy_pbp_last_kickoff), cutoff)
  expect_equal(audit$seeds, c(20260909L, 20260910L, 20260911L))
  summary <- read.csv(file.path(directory, "summary.csv"))
  picks <- read.csv(file.path(directory, "frozen_bracket_picks.csv"))
  expect_true(all(is.finite(picks$expected_margin)))
  expect_false(anyDuplicated(paste(picks$model, picks$game_id)) > 0)
  for (i in seq_len(nrow(summary))) {
    rows <- picks[picks$model == summary$model[i], ]
    expect_equal(nrow(rows), 11L)
    expect_equal(sum(rows$winner == rows$actual_winner), summary$frozen_bracket_correct[i])
    expect_equal(tail(rows$winner, 1), summary$champion[i])
  }
})
