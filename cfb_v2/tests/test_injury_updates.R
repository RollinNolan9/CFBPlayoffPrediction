library(testthat)

project_dir <- if (file.exists(file.path(getwd(), "cfb_v2", "config.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
}
for (file in c("config.R", "store.R", "features.R")) {
  source(file.path(project_dir, "cfb_v2", file))
}
config <- cfb_v2_config(project_dir, 2026)
cutoff <- as.POSIXct("2026-09-17 18:00:00", tz = "UTC")

injury_reports <- function() {
  data.frame(
    game_id = "g1", team = "Notre Dame", player_id = "qb1", player_name = "A Quarterback",
    position = "QB", starter = TRUE, usage_share = 1, status = "out",
    impact_points = 6, confidence = 1, source_type = "school_official",
    source_timestamp = c("2026-09-16 12:00:00", "2026-09-17 12:00:00"),
    conflict_flag = FALSE, stringsAsFactors = FALSE
  )
}

test_that("repeated injury reports apply a single latest adjustment", {
  latest <- select_latest_injury_reports(injury_reports(), cutoff)
  expect_equal(nrow(latest), 1L)
  expect_equal(latest$source_timestamp, "2026-09-17 12:00:00")
  scenarios <- build_injury_scenarios(latest, 7, "Notre Dame", "Miami", config)
  expect_equal(scenarios$expected_margin[scenarios$scenario == "most_likely"], 1)
})

test_that("active updates clear previous unavailable adjustments", {
  reports <- injury_reports()
  reports$status[2] <- "active"
  latest <- select_latest_injury_reports(reports, cutoff)
  expect_equal(latest$status, "active")
  scenarios <- build_injury_scenarios(latest, 7, "Notre Dame", "Miami", config)
  expect_equal(scenarios$expected_margin[scenarios$scenario == "most_likely"], 7)
})

test_that("updates after the cutoff cannot replace a pre-cutoff report", {
  reports <- injury_reports()
  reports$status[2] <- "active"
  reports$source_timestamp[2] <- "2026-09-17 18:00:01"
  expect_equal(select_latest_injury_reports(reports, cutoff)$status, "out")
  reports$source_timestamp[2] <- "2026-09-17 18:00:00"
  expect_equal(select_latest_injury_reports(reports, cutoff)$status, "active")
  expect_equal(nrow(select_latest_injury_reports(reports, "2026-09-15")), 0L)
})

test_that("injury updates remain separate for games teams and players", {
  reports <- injury_reports()[rep(1L, 4L), ]
  reports$game_id[2] <- "g2"
  reports$team[3] <- "Miami"
  reports$player_id[4] <- "qb2"
  expect_equal(nrow(select_latest_injury_reports(reports, cutoff)), 4L)
})

test_that("names are a fallback and can connect an update to a unique ID", {
  reports <- injury_reports()
  reports$player_id <- c("qb1", NA_character_)
  reports$player_name[2] <- " a quarterback "
  reports$status[2] <- "active"
  expect_equal(select_latest_injury_reports(reports, cutoff)$status, "active")
  reports$player_id <- NULL
  expect_equal(nrow(select_latest_injury_reports(reports, cutoff)), 1L)
  reports$player_name[2] <- "Another Quarterback"
  expect_equal(nrow(select_latest_injury_reports(reports, cutoff)), 2L)
})

test_that("anonymous and ambiguous name-only reports require identity resolution", {
  reports <- injury_reports()
  reports$player_id[2] <- ""
  reports$player_name[2] <- NA_character_
  expect_error(select_latest_injury_reports(reports, cutoff), "player_id or player_name")
  reports <- injury_reports()[c(1L, 1L, 2L), ]
  reports$player_id <- c("qb1", "qb2", "")
  expect_error(select_latest_injury_reports(reports, cutoff), "Ambiguous injury player_name")
})

test_that("ties reject conflicting statuses and retain latest conflict flags", {
  reports <- injury_reports()
  reports$source_timestamp[] <- "2026-09-17 12:00:00"
  reports$status[2] <- "active"
  expect_error(select_latest_injury_reports(reports, cutoff), "Conflicting latest injury statuses")
  reports$status[2] <- "out"
  reports$conflict_flag <- c(TRUE, FALSE)
  latest <- select_latest_injury_reports(reports, cutoff)
  expect_equal(nrow(latest), 1L)
  expect_true(latest$conflict_flag)
  reports$source_timestamp[1] <- "2026-09-16 12:00:00"
  expect_false(select_latest_injury_reports(reports, cutoff)$conflict_flag)
})

test_that("empty injury inputs and invalid timestamps are handled explicitly", {
  expect_null(select_latest_injury_reports(NULL, cutoff))
  empty <- injury_reports()[FALSE, ]
  expect_equal(select_latest_injury_reports(empty, cutoff), empty)
  reports <- injury_reports()
  reports$source_timestamp[2] <- NA_character_
  expect_equal(select_latest_injury_reports(reports, cutoff)$source_timestamp,
               "2026-09-16 12:00:00")
  expect_error(select_latest_injury_reports(reports, ""), "one valid timestamp")
})

test_that("injury ledger accepts distinct name-only players at the same timestamp", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(v2_disconnect(con), add = TRUE)
  v2_init_schema(con)
  reports <- injury_reports()
  reports$player_id <- NULL
  reports$player_name <- c("First Player", "Second Player")
  reports$source_timestamp[] <- "2026-09-17 12:00:00"
  latest <- select_latest_injury_reports(reports, cutoff)
  latest$source_timestamp <- parse_utc_datetime(latest$source_timestamp)
  latest$captured_at <- cutoff
  keys <- c("game_id", "team", "player_id", "player_name", "source_timestamp")
  expect_equal(v2_append_snapshot(con, "injury_snapshots", latest, "name_only", keys), 2L)
  stored <- DBI::dbReadTable(con, "injury_snapshots")
  expect_setequal(stored$player_name, c("First Player", "Second Player"))
  expect_true(all(is.na(stored$player_id)))

  reports <- injury_reports()
  reports$player_name <- NULL
  latest <- select_latest_injury_reports(reports, cutoff)
  latest$source_timestamp <- parse_utc_datetime(latest$source_timestamp)
  latest$captured_at <- cutoff
  expect_equal(v2_append_snapshot(con, "injury_snapshots", latest, "id_only", keys), 1L)
})
