library(testthat)

project_dir <- if (file.exists(file.path(getwd(), "cfb_v2", "dashboard.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
}
source(file.path(project_dir, "cfb_v2", "dashboard.R"))

test_that("dashboard requires explicit ATS calibration evidence", {
  raw <- data.frame(
    game_id = "g", home = "Home", away = "Away", expected_margin = 6,
    market_home_spread = -3, home_cover_probability = .5,
    stringsAsFactors = FALSE
  )
  expect_false(prepare_dashboard_predictions(raw)$ats_calibrated)
  raw$ats_calibrated <- TRUE
  expect_true(prepare_dashboard_predictions(raw)$ats_calibrated)
  raw$ats_calibrated <- NA
  expect_false(isTRUE(prepare_dashboard_predictions(raw)$ats_calibrated))
})

test_that("dashboard quote time never falls back to the model run time", {
  raw <- data.frame(
    game_id = c("known", "blank", "missing"), home = "Home", away = "Away",
    expected_margin = 6, as_of = "2026-09-17 12:00:00 UTC",
    market_captured_at = c("2026-09-13 09:00:00 UTC", "", NA),
    stringsAsFactors = FALSE
  )
  expect_equal(prepare_dashboard_predictions(raw)$snapshot_time,
               c("2026-09-13 09:00:00 UTC", "", ""))
  raw$market_captured_at <- NULL
  expect_equal(prepare_dashboard_predictions(raw)$snapshot_time, rep("", 3))
})

test_that("grading discovers v3 raw cards and preserves saved passes", {
  fixture <- tempfile("grading-fixture-")
  dir.create(file.path(fixture, "cfb_v2", "output"), recursive = TRUE)
  dir.create(file.path(fixture, "cfb_v3", "output"), recursive = TRUE)
  # This test owns this fixture, and it must remain within R's temporary directory.
  on.exit({
    stopifnot(startsWith(normalizePath(fixture, winslash = "/"),
                        paste0(normalizePath(tempdir(), winslash = "/"), "/")))
    unlink(fixture, recursive = TRUE)
  }, add = TRUE)
  files <- c("grade_snapshot.R", "dashboard.R", "config.R")
  expect_true(all(file.copy(file.path(project_dir, "cfb_v2", files),
                            file.path(fixture, "cfb_v2"))))

  raw <- data.frame(
    game_id = c("home_win", "away_loss", "pass", "push"), season = 2026,
    home = "Home", away = "Away", expected_margin = c(7, -2, 4, 3),
    market_home_spread = -3, straight_up_pick = c("Home", "Away", "Home", "Home"),
    ats_pick = c("Home", "Away", NA, "Home"),
    pick_status = c("official_pick", "article_pick", "pass", "official_pick"),
    stringsAsFactors = FALSE
  )
  card_path <- file.path(fixture, "cfb_v3", "output", "live_2026_3.csv")
  utils::write.csv(raw, card_path, row.names = FALSE, na = "")
  legacy_path <- file.path(fixture, "cfb_v2", "output", "live_2026_2.csv")
  utils::write.csv(raw[1, ], legacy_path, row.names = FALSE, na = "")
  Sys.setFileTime(legacy_path, Sys.time() + 60)

  scores <- data.frame(
    game_id = raw$game_id, completed = TRUE, home_points = c(20, 20, 20, 17),
    away_points = 14, stringsAsFactors = FALSE
  )
  script_path <- file.path(fixture, "cfb_v2", "grade_snapshot.R")
  execution <- new.env(parent = globalenv())
  execution$commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) character() else paste0("--file=", script_path)
  }
  execution$source <- function(file) base::source(file, local = execution)
  execution$requireNamespace <- function(package, ...) {
    if (package == "cfbfastR") TRUE else base::requireNamespace(package, ...)
  }
  # Stub only the external score service; execute the actual grading script.
  execution$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (package == "cfbfastR" && name == "has_cfbd_key") return(function() TRUE)
    if (package == "cfbfastR" && name == "cfbd_game_info") {
      return(function(season, season_type) {
        if (season_type == "regular") scores else scores[FALSE, ]
      })
    }
    getExportedValue(package, name)
  }
  expect_output(base::sys.source(script_path, envir = execution), "Completed: 4 of 4")
  expect_equal(execution$prediction_path,
               normalizePath(card_path, winslash = "/", mustWork = TRUE))
  expect_equal(execution$graded$ats_pick, raw$ats_pick)
  expect_equal(execution$graded$ats_result, c("W", "L", NA, "P"))
  expect_equal(execution$graded$ats_pick_line,
               c("Home -3.0", "Away +3.0", "Pass", "Home -3.0"))
  expect_equal(execution$graded$model_line,
               c("Home -7.0", "Away -2.0", "Home -4.0", "Home -3.0"))
  expect_true(file.exists(execution$report_path))
  expect_match(paste(readLines(execution$report_path), collapse = "\n"),
               "Home -7.0", fixed = TRUE)

  # An explicit snapshot can still carry publication-specific presentation text.
  raw$model_line <- "Saved model label"
  raw$ats_pick_line <- c("Saved home label", "Saved away label", "Pass", "Saved push label")
  utils::write.csv(raw, card_path, row.names = FALSE, na = "")
  execution$commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) paste0("--predictions=", card_path) else paste0("--file=", script_path)
  }
  expect_output(base::sys.source(script_path, envir = execution), "Completed: 4 of 4")
  expect_equal(execution$graded$model_line, raw$model_line)
  expect_equal(execution$graded$ats_pick_line, raw$ats_pick_line)
  expect_equal(execution$graded$ats_result, c("W", "L", NA, "P"))
})
