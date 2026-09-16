library(testthat)
root <- normalizePath(file.path("..", ".."), winslash = "/")
if (!file.exists(file.path(root, "run_cfb_og_verification.R"))) root <- normalizePath(".", winslash = "/")
source(file.path(root, "cfb_v3", "experiments", "og_reconstruction.R"))

test_that("spreadsheet dates distinguish date serials and missing cells", {
  x <- og_public_dates(c("44814.0", "9/10", "2022-09-10", "", NA, "Date"), 2022)
  expect_equal(as.character(x[1:3]), rep("2022-09-10", 3))
  expect_true(all(is.na(x[4:6])))
  expect_equal(as.character(og_public_dates(c("31-Oct", "1-Nov"), 2023)), c("2023-10-31", "2023-11-01"))
})

test_that("record coercion is explicit and unknown records stay unknown", {
  x <- og_public_records(c("12-0", "0-6", "45962.0", "", NA))
  expect_equal(x$wins[1:3], c(12L, 0L, 11L))
  expect_equal(x$losses[1:3], c(0L, 6L, 1L))
  expect_equal(x$decoded_date, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_true(all(is.na(x$wins[4:5])))
})

test_that("conflicting dated anchors fail while rounded deltas allow one tenth", {
  sp <- data.frame(season = 2025, snapshot_label = c("week 1", "week 2"),
    team = "A", rating = c(10, 15.3))
  anchors <- data.frame(season = 2025, snapshot_label = "week 2", team = "A",
    field = "rating", expected = c(5.2, 4.9), previous_label = "week 1")
  expect_equal(og_verify_anchors(sp, anchors)$passed, c(TRUE, FALSE))
  expect_error(og_verify_anchors(rbind(sp, sp[1, ]), anchors))
})

test_that("cutoffs exclude same-time, future and unresolved plays", {
  cutoff <- as.POSIXct("2024-10-21", tz = "UTC")
  compact <- list(games = data.frame(game_id = 1:4, year = 2024,
    kickoff = as.POSIXct(c(as.numeric(cutoff)-1, as.numeric(cutoff), as.numeric(cutoff)+1, NA), origin = "1970-01-01", tz = "UTC")),
    stats = data.frame(game_id = 1:4, value = 1:4))
  frozen <- og_freeze_compact(compact, 2024, cutoff)
  expect_equal(frozen$games$game_id, 1L)
  compact$stats$value[2:4] <- 9999
  expect_equal(og_freeze_compact(compact, 2024, cutoff), frozen)
})

test_that("matchup lookup preserves side history rather than latest team game", {
  data <- data.frame(home = c("A", "B", "C"), away = c("C", "A", "B"),
    year = 2024, week = 6:8, home_team_elo_rating = c(100, 200, 300),
    away_team_elo_rating = c(101, 201, 301), source_game_id = c("a", "b", "c"))
  row <- og_matchup_row("A", "B", data)
  expect_equal(row$home_team_elo_rating, 100)
  expect_equal(row$away_team_elo_rating, 301)
  expect_equal(unname(attr(row, "source_games")), c("a", "c"))
  expect_null(og_matchup_row("missing", "B", data))
})

test_that("literal ratings require close agreement, not the delta rounding allowance", {
  sp <- data.frame(season = 2024, snapshot_label = "week 9", team = "A", rating = 10)
  anchors <- data.frame(season = 2024, snapshot_label = "week 9", team = "A",
    field = "rating", expected = 10.1, previous_label = NA_character_)
  expect_false(og_verify_anchors(sp, anchors)$passed)
})
