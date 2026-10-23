library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R", "live_data.R")) source(file.path("cfb_v2", file))

live_input_probe <- function(week) {
  probe <- new.env(parent = globalenv())
  probe$clock <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  probe$Sys.time <- function() probe$clock
  config <- cfb_v2_config(tempfile("live-reliability-"), 2026)
  config$omit_thursday <- FALSE
  schedule <- data.frame(
    game_id = c("upcoming", "postseason"), season = 2026L, week = week,
    home_team = "A", away_team = "B", start_date = "2026-09-05T19:00:00Z",
    season_type = c("regular", "postseason"), home_classification = "fbs",
    away_classification = "fbs", completed = FALSE, neutral_site = FALSE,
    venue = "Test stadium"
  )
  membership <- data.frame(team = c("A", "B"), season = 2026L,
                           classification = "fbs", conference = c("SEC", "ACC"))
  history <- standardize_schedule(data.frame(
    game_id = "old", season = 2025L, week = 1L, model_week = 1L,
    home = "A", away = "B", kickoff = as.POSIXct("2025-09-06", tz = "UTC"),
    margin = 7, completed = TRUE, home_power = 3, away_power = -3
  ))
  team_games <- data.frame(
    game_id = "old", team = c("A", "B"), season = 2025L, week = 1L,
    model_week = 1L, kickoff = history$kickoff,
    offense_epa = c(.2, -.1), defense_epa = c(.1, -.2),
    net_efficiency = c(.3, -.3), margin = c(7, -7)
  )
  probe$load_cfbd_schedule_season <- function(...) schedule
  probe$load_cfbd_fbs_teams_season <- function(...) membership
  probe$load_compact_pbp_season <- function(...) stop("Preseason must not load PBP")
  probe$pull_current_fbs_coaches <- function(...) data.frame()
  probe$current_fbs_coach_assignments <- function(...) data.frame()
  probe$extend_current_coach_history <- function(history, ...) history
  probe$read_csv_if_present <- function(path, ...) switch(basename(path),
    historical_games.csv = history,
    historical_team_games.csv = team_games,
    historical_fbs_membership.csv = transform(membership, season = 2025L))
  probe$pull_cfbd_lines <- function(...) {
    probe$clock <- probe$clock + 60
    data.frame(game_id = "upcoming", provider = "DraftKings",
               captured_at = probe$clock, home_spread = -3)
  }
  probe$prepare_live_inputs <- prepare_live_inputs
  environment(probe$prepare_live_inputs) <- probe
  list(env = probe, config = config)
}

test_that("automatic Week 0/1 inputs use normalized regular-season schedules", {
  for (week in 0:1) {
    probe <- live_input_probe(week)
    result <- probe$env$prepare_live_inputs(v2_input_templates(), probe$config,
                                            2026, week, refresh = TRUE)
    expect_equal(result$schedule$game_id, "upcoming")
    expect_equal(result$schedule$postseason_type, "regular")
    expect_false(result$schedule$is_cfp)
    expect_false(result$schedule$conference_championship)
    expect_equal(result$schedule$model_week, 1L)
    expect_equal(nrow(result$team_week_features), 2L)
    expect_true(all(result$team_week_features$source_games == 0))
  }
})

test_that("default live cutoff includes newly fetched quotes without backdating", {
  probe <- live_input_probe(1)
  started <- probe$env$clock
  result <- probe$env$prepare_live_inputs(v2_input_templates(), probe$config,
                                          2026, 1, refresh = TRUE)
  expect_gt(as.numeric(result$as_of), as.numeric(started))
  selected <- select_article_lines(result$lines, "upcoming", result$as_of)
  expect_equal(selected$market_home_spread, -3)
  expect_equal(selected$market_captured_at, result$lines$captured_at)
})

test_that("explicit cutoffs continue to exclude later source observations", {
  probe <- live_input_probe(1)
  cutoff <- probe$env$clock
  result <- probe$env$prepare_live_inputs(v2_input_templates(), probe$config,
                                          2026, 1, as_of = cutoff, refresh = TRUE)
  expect_equal(result$as_of, cutoff)
  selected <- select_article_lines(result$lines, "upcoming", cutoff)
  expect_true(is.na(selected$market_home_spread))
  expect_true(is.na(selected$market_captured_at))
})

test_that("consensus provenance uses actual contributing quote times", {
  times <- as.POSIXct(c("2026-09-13 12:00:00", "2026-09-13 12:05:00",
                       "2026-09-13 11:00:00"), tz = "UTC")
  lines <- data.frame(game_id = "g", provider = c("DraftKings", "FanDuel", "DraftKings"),
                      captured_at = times, home_spread = c(-3, -4, -2))
  selected <- select_article_lines(lines, c("g", "missing"), times[2] + 86400)
  expect_equal(selected$market_home_spread[1], -3.5)
  expect_equal(selected$market_captured_at[1], times[1])
  expect_true(is.na(selected$market_captured_at[2]))
  expect_true(is.na(select_article_lines(NULL, "g", times[1])$market_captured_at))
})

test_that("schema migration preserves old rows and stores new provenance", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE prediction_snapshots (run_id VARCHAR, game_id VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO prediction_snapshots VALUES ('old', 'old-game')")
  v2_init_schema(con)
  timestamp <- as.POSIXct("2026-09-17 17:00:00", tz = "UTC")
  v2_append_snapshot(con, "prediction_snapshots", data.frame(
    game_id = "new-game", market_captured_at = timestamp, ats_calibrated = FALSE),
    "new", "game_id")
  stored <- DBI::dbGetQuery(con, "SELECT * FROM prediction_snapshots ORDER BY run_id")
  expect_equal(nrow(stored), 2L)
  expect_equal(stored$market_captured_at[stored$run_id == "new"], timestamp)
  expect_false(stored$ats_calibrated[stored$run_id == "new"])
  expect_true(is.na(stored$ats_calibrated[stored$run_id == "old"]))
})
