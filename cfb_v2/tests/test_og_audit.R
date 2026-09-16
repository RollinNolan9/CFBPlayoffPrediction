library(testthat)
Sys.unsetenv("LC_ALL")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
  "models.R", "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R",
  "workflow.R")) source(file.path("cfb_v2", file))
source("cfb_v3/experiments/audit_og_inputs.R")

sp_fixture <- function() data.frame(team = "Notre Dame", season = 2024L,
  rating = 20, offense_rating = 35, defense_rating = 15,
  available_at = "2024-10-13T20:00:00Z", through_at = "2024-10-13T06:00:00Z",
  source_url = "https://example.org/preserved-source", evidence_reviewed = TRUE)
cutoff <- parse_utc_datetime("2024-10-14T00:00:00Z")

test_that("only reviewed complete pre-cutoff rating evidence passes", {
  x <- sp_fixture()
  expect_identical(og_sp_rejection(x, cutoff, 2024L), "")
  for (name in c("available_at", "through_at", "source_url", "evidence_reviewed",
    "rating", "offense_rating", "defense_rating", "team", "season")) {
    missing <- x; missing[[name]] <- NA
    expect_true(nzchar(og_sp_rejection(missing, cutoff, 2024L)), info = name)
  }
  expect_equal(og_sp_rejection(x, cutoff, 2025L), "wrong_or_missing_season")
  x$evidence_reviewed <- FALSE
  expect_equal(og_sp_rejection(x, cutoff, 2024L), "unreviewed_source")
})

test_that("same-time, later, inconsistent and ambiguous timestamps fail", {
  for (name in c("available_at", "through_at")) {
    for (time in c("2024-10-14T00:00:00Z", "2024-10-15T00:00:00Z")) {
      x <- sp_fixture(); x[[name]] <- time
      expect_equal(og_sp_rejection(x, cutoff, 2024L), "not_before_cutoff")
    }
  }
  x <- sp_fixture(); x$through_at <- "2024-10-13T21:00:00Z"
  expect_equal(og_sp_rejection(x, cutoff, 2024L), "data_after_publication")
  for (time in c("2024-10-13", "2024-10-13T20:00:00-05:00", "2024-10-13T20:00:00Zjunk")) {
    x <- sp_fixture(); x$available_at <- time
    expect_true(nzchar(og_sp_rejection(x, cutoff, 2024L)))
  }
  expect_equal(og_sp_rejection(sp_fixture(), as.POSIXct(NA), 2024L), "missing_prediction_cutoff")
  x <- rbind(sp_fixture(), sp_fixture())
  expect_error(og_sp_rejection(x, cutoff, 2024L), "duplicate")
})

test_that("public SP extraction uses numeric FBS components and preserves missing data", {
  x <- data.frame(Team = c("Miami-FL", "Hawaii", ""), check.names = FALSE,
    `SP+` = c(20, -10, NA), `Off. SP+` = c(40, NA, NA), `Def. SP+` = c(20, 30, NA))
  y <- og_extract_public_sp(x)
  expect_equal(y$team, c("Miami", "Hawai'i"))
  expect_equal(nrow(y), 2L)
  expect_true(is.na(y$offense_rating[2]))
  expect_error(og_extract_public_sp(x[c("Team", "SP+")]), "missing")
  x$Team[2] <- "Miami"
  expect_error(og_extract_public_sp(x), "duplicate")
})

test_that("history ceiling excludes same-cutoff and future games without crossing seasons", {
  dates <- c(sprintf("2024-09-%02dT12:00:00Z", seq(1, 29, 7)),
    "2024-10-06T12:00:00Z", "2024-10-14T00:00:00Z", "2024-10-20T12:00:00Z")
  dates <- data.frame(game_id = as.character(seq_along(dates)), start_date = dates)
  stats <- data.frame(game_id = rep(dates$game_id, each = 2L), year = 2024L,
    pos_team = rep(c("A", "B"), nrow(dates)), epa_play = 1, epa_pass = 1,
    epa_rush = 1, wpa_play = 1)
  games <- data.frame(game_id = "target", season = 2024L, week = 8L,
    kickoff = "2024-10-19T12:00:00Z", home = "A", away = "B")
  result <- og_history_ceiling(games, stats, dates)
  expect_equal(result$home_prior_games, 6L)
  expect_true(result$six_game_history_ceiling)
  stats$epa_play[stats$game_id %in% c("7", "8")] <- NA_real_
  expect_equal(og_history_ceiling(games, stats, dates), result)
  stats$year[stats$game_id == "1"] <- 2023L
  expect_false(og_history_ceiling(games, stats, dates)$six_game_history_ceiling)
  games$kickoff <- NA_character_
  expect_false(og_history_ceiling(games, stats, dates)$six_game_history_ceiling)
})

test_that("Elo alignment is one-to-one and distinguishes pregame from same-week values", {
  games <- data.frame(game_id = "g", season = 2024L, week = 8L,
    home = "A", away = "B", postseason_type = "regular",
    home_pregame_elo = 1500, away_pregame_elo = 1400)
  elo <- data.frame(team = c("A", "B"), year = 2024L, week = 8L, elo = c(1520, 1400))
  result <- og_elo_alignment(games, elo)
  expect_equal(result$equal, c(FALSE, TRUE))
  elo$year <- 2023L
  expect_false(any(og_elo_alignment(games, elo)$comparable))
  expect_error(og_elo_alignment(games, rbind(elo, elo)), "duplicate")
})

test_that("completed audit includes championship games, not bowls, without fitting or writes to production", {
  directories <- list.dirs("cfb_v3/output/experiments/og_audit", recursive = FALSE)
  complete <- directories[file.exists(file.path(directories, "verification.json"))]
  skip_if(!length(complete), "Run the input audit to generate integration artifacts.")
  directory <- tail(sort(complete), 1)
  verified <- jsonlite::fromJSON(file.path(directory, "verification.json"))
  expect_true(verified$production_unchanged)
  expect_equal(verified$models_fitted, 0L)
  expect_false(verified$ready)
  x <- read.csv(file.path(directory, "game_coverage.csv"))
  expect_equal(nrow(x), 3026L)
  expect_equal(sum(x$is_cfp), 28L)
  expect_equal(sum(x$postseason_type == "conference_championship"), 38L)
  expect_false(any(grepl("bowl", x$postseason_type)))
  expect_equal(sum(x$closing_line_available), 3025L)
  expect_equal(sum(x$six_game_history_ceiling), 1611L)
  expect_false(anyDuplicated(x$game_id) > 0)
  expect_false(any(x$ready_for_scored_comparison))
})

public_capture <- function() {
  directories <- list.dirs("cfb_v3/output/experiments/og_audit", recursive = FALSE)
  directories <- file.path(directories, "public_sources")
  complete <- directories[file.exists(file.path(directories, "inspection", "2024_final_sp_parity.csv"))]
  skip_if(!length(complete), "Run public source recovery and inspection first.")
  tail(sort(complete), 1)
}

test_that("recovered tables remain unapproved and confirm final-2024 cache contamination", {
  directory <- file.path(public_capture(), "inspection")
  rows <- read.csv(file.path(directory, "recovered_sp_candidates.csv"))
  coverage <- read.csv(file.path(directory, "table_coverage.csv"))
  expect_equal(nrow(rows), sum(coverage$teams))
  expect_true(all(c(2022L, 2023L, 2024L, 2025L) %in% rows$season))
  expect_false(anyDuplicated(paste(rows$season, rows$snapshot_label, rows$team)) > 0)
  expect_true(all(is.finite(as.matrix(rows[c("rating", "offense_rating", "defense_rating")]))))
  expect_false(any(rows$evidence_reviewed))
  expect_true(all(is.na(rows$available_at) & is.na(rows$through_at)))
  expect_equal(sum(rows$gate_status == "rejected_final_snapshot"), 134L)
  parity <- read.csv(file.path(directory, "2024_final_sp_parity.csv"))
  expect_equal(nrow(parity), 134L)
  for (name in c("rating", "offense_rating", "defense_rating"))
    expect_equal(parity[[paste0(name, "_public_final")]], parity[[paste0(name, "_cached")]])
})

test_that("downloaded source bytes still match their recorded hashes", {
  directory <- public_capture()
  inventory <- jsonlite::fromJSON(file.path(directory, "inventory.json"), simplifyVector = FALSE)
  for (name in names(inventory)) {
    item <- inventory[[name]]
    extension <- if (name == "connelly_2023") ".html" else if (name == "cfbtxt_2026") ".csv" else ".xlsx"
    path <- file.path(directory, paste0(name, extension))
    expect_identical(digest::digest(file = path, algo = "sha256"), item$sha256)
    for (tab in item$tab_sources) {
      path <- file.path(directory, paste0(name, "_", sub(".*gid=", "", tab$url), ".html"))
      expect_identical(digest::digest(file = path, algo = "sha256"), tab$sha256)
    }
  }
})
