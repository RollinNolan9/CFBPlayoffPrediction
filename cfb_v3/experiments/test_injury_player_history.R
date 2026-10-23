source("cfb_v3/experiments/injury_player_history.R")

testthat::test_that("raw measure includes targets and completions without double counting", {
  plays <- data.frame(gameId = 1, id = as.character(11:14), offense = "Team",
    ppa = c(1, -2, 3, NA_real_))
  stats <- data.frame(gameId = 1, playId = as.character(c(11, 12, 13, 13, 14)),
    team = "Team", athleteId = "99", athleteName = "RB",
    statType = c("Rush", "Target", "Completion", "Touchdown", "Reception"))
  ppa <- data.frame(season = 2024, week = 1, team = "Team", position = "RB",
    seasonType = "regular", id = "99", name = "RB", averagePPA.all = 0.2)
  raw <- injury_exact_rb_game(plays, stats, ppa, 2024, 1, "Team", 1,
    mode = "raw_involvement_ppa")
  testthat::expect_equal(raw$raw_involvements, 4)
  testthat::expect_equal(raw$ppa_involvements, 3)
  testthat::expect_equal(raw$missing_ppa_involvements, 1)
  testthat::expect_equal(raw$total_ppa, 2)
  testthat::expect_equal(raw$average_ppa, 2 / 3)
  testthat::expect_false(raw$api_aggregate_reconciled)
  testthat::expect_true(all(attr(raw, "role_audit")$role_accounted))
  testthat::expect_error(injury_exact_rb_game(plays, stats, ppa, 2024, 1, "Team", 1),
    "does not match")
  stats$statType[1] <- "Unrecognized offensive role"
  testthat::expect_error(injury_exact_rb_game(plays, stats, ppa, 2024, 1, "Team", 1,
    mode = "raw_involvement_ppa"), "Unaccounted")
})

testthat::test_that("missing PPA and unmatched play IDs never become zero production", {
  p <- data.frame(gameId = 1, id = "11", offense = "Team", ppa = NA_real_)
  s <- data.frame(gameId = 1, playId = "11", team = "Team", athleteId = "99",
    athleteName = "RB", statType = "Rush")
  a <- data.frame(season = 2024, week = 1, team = "Team", position = "RB",
    id = "99", name = "RB", averagePPA.all = 0)
  testthat::expect_error(injury_exact_rb_game(p, s, a, 2024, 1, "Team", 1,
    mode = "raw_involvement_ppa"), "unavailable, not zero")
  s$playId <- "12"
  testthat::expect_error(injury_exact_rb_game(p, s, a, 2024, 1, "Team", 1,
    mode = "raw_involvement_ppa"), "no matching offensive play")
})

testthat::test_that("cached request identity is checked before use", {
  path <- tempfile("injury_cache_")
  dir.create(path)
  jsonlite::write_json(list(), file.path(path, "example.json"))
  jsonlite::write_json(list(endpoint = "/plays", query = list(year = 2024),
    sha256 = digest::digest(file = file.path(path, "example.json"), algo = "sha256")),
    file.path(path, "example_metadata.json"), auto_unbox = TRUE)
  testthat::expect_error(injury_player_fetch(path, "example", "/plays", list(year = 2025)),
    "request does not match")
  testthat::expect_error(injury_player_fetch(path, "example", "/plays/stats", list(year = 2024)),
    "request does not match")
})

testthat::test_that("planning excludes future and unfinished games before request budgeting", {
  project <- tempfile("injury_prior_")
  dir.create(project)
  schedule <- data.frame(game_id = as.character(1:5),
    kickoff = c("2024-09-01T12:00:00Z", "2024-09-08T12:00:00Z",
      "2024-09-14T12:00:00Z", "2024-09-13T12:00:00Z", "2024-09-02T12:00:00Z"),
    season = 2024, week = 1:5, home = "Test Team", away = "Opponent",
    completed = c(TRUE, TRUE, TRUE, TRUE, FALSE), source_season_type = "regular")
  path <- file.path(project, "schedule.csv")
  write.csv(schedule, path, row.names = FALSE)
  plan <- injury_prior_player_history(project, 2024, "Test Team", "2024-09-13T19:00:00Z",
    max_new_requests = 0, schedule_path = path, mode = "raw_involvement_ppa")
  testthat::expect_equal(plan$prior_games$game_id, c("1", "2"))
  testthat::expect_equal(plan$new_requests_needed, 5)
  testthat::expect_error(injury_prior_player_history(project, 2024, "Test Team",
    "2024-09-13T19:00:00Z", max_new_requests = 0, execute = TRUE,
    schedule_path = path, mode = "raw_involvement_ppa"), "No network requests made")
})
