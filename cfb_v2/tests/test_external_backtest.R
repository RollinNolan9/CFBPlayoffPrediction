library(testthat)
for (file in c("store.R", "features.R", "workflow.R")) source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "audit_og_inputs.R", "og_reconstruction.R", "external_ratings.R", "external_backtest.R"))
  source(file.path("cfb_v3/experiments", file))

historical_sp_fixture <- function() {
  reference <- data.frame(game_id = as.character(1:3), model = "ridge_core", season = 2023L, week = 9:11,
    home = c("A", "C", "A"), away = c("B", "D", "B"), home_level = "fbs", away_level = "fbs",
    home_conference = "SEC", away_conference = "ACC", postseason_type = "regular", is_cfp = FALSE,
    neutral_site = c(FALSE, TRUE, FALSE), actual_margin = c(7, -3, 14), expected_margin = c(8, -4, 11),
    closing_home_spread = c(-7, 3, NA), cycle = c("2023-10-23", "2023-10-30", "2023-11-06"),
    kickoff = parse_utc_datetime(c("2023-10-28T18:00:00Z", "2023-11-04T18:00:00Z", "2023-11-11T18:00:00Z")))
  rows <- data.frame(season = 2023L, cycle = rep(c("2023-10-23", "2023-10-30"), each = 4),
    team = rep(LETTERS[1:4], 2), rating = c(5, 0, 0, 5, 7, 0, 0, 6), qualified = TRUE,
    corroborated_by = rep(c("2023-10-25T12:00:00Z", "2023-11-01T12:00:00Z"), each = 4))
  games <- reference
  games$margin <- games$actual_margin; games$completed <- TRUE; games$feature_week_start <- games$cycle
  list(reference = reference, rows = rows, games = games)
}

test_that("historical rating and fixed blend retain signs, venue and unchanged v3", {
  f <- historical_sp_fixture(); x <- historical_sp_forecasts(f$reference, f$rows)
  p <- subset(x$predictions, lane == "same_cycle_corroborated")
  expect_equal(subset(p, model == "sp_rating")$expected_margin, c(7.4, -6))
  expect_equal(subset(p, model == "v3_sp_50_50")$expected_margin, c(7.7, -5))
  expect_equal(subset(p, model == "v3_control")$expected_margin, c(8, -4))
  expect_equal(length(unique(p$game_id)), 2L)
  expect_false(any(subset(p, model == "market_reference")$selected))
})

test_that("timing uses exact cycles without carrying across gaps", {
  f <- historical_sp_fixture(); x <- historical_sp_forecasts(f$reference, f$rows)
  expect_equal(unique(subset(x$predictions, lane == "one_cycle_delayed")$game_id), c("2", "3"))
  expect_equal(Reduce(intersect, split(x$predictions$game_id, x$predictions$lane)), "2")
  f$rows$cycle <- as.character(as.Date(f$rows$cycle)-21L)
  expect_equal(nrow(historical_sp_forecasts(f$reference, f$rows)$predictions), 0L)
})

test_that("postgame corroboration, failed records and duplicates are rejected", {
  f <- historical_sp_fixture(); f$rows$corroborated_by[1] <- "2023-10-29T00:00:00Z"
  x <- historical_sp_forecasts(f$reference, f$rows)
  expect_equal(subset(x$coverage, game_id == "1" & lane == "same_cycle_corroborated")$reason,
    "corroboration_not_before_kickoff")
  f$rows$qualified[5] <- FALSE
  x <- historical_sp_forecasts(f$reference, f$rows)
  expect_equal(subset(x$coverage, game_id == "3" & lane == "one_cycle_delayed")$reason,
    "row_failed_record_or_prior_evidence_checks")
  expect_error(historical_sp_forecasts(f$reference, rbind(f$rows, f$rows[1, ])), "duplicate|Duplicate")
})

test_that("reference joins reject mismatched metadata and non-CFP bowls", {
  f <- historical_sp_fixture()
  expect_equal(nrow(historical_sp_reference(f$reference, f$games)), 3L)
  g <- f$games; g$neutral_site[1] <- TRUE
  expect_error(historical_sp_reference(f$reference, g), "mismatch")
  g <- f$games; g$margin[1] <- 100
  expect_error(historical_sp_reference(f$reference, g), "margins")
  g <- f$games; g$feature_week_start[1] <- "2023-10-30"
  expect_error(historical_sp_reference(f$reference, g), "boundary")
  f$reference$postseason_type[1] <- "bowl"
  expect_equal(nrow(historical_sp_reference(f$reference, f$games)), 2L)
})

test_that("pushes, missing market lines and SU no-picks have honest denominators", {
  f <- historical_sp_fixture(); x <- historical_sp_forecasts(f$reference, f$rows)$predictions
  m <- historical_sp_metrics(x)
  v <- subset(m, lane == "same_cycle_corroborated" & model == "v3_control" & slice == "all_fbs")
  expect_equal(v$pushes, 2L); expect_true(is.na(v$ats_accuracy))
  market <- subset(m, lane == "one_cycle_delayed" & model == "market_reference" & slice == "all_fbs")
  expect_equal(market$games, 2L); expect_equal(market$margin_games, 1L); expect_equal(market$su_decisions, 1L)
  expect_false(is.na(market$margin_mae))
  expect_true(all(subset(m, slice == "cfp")$games == 0L))
  expect_true(all(is.na(subset(m, slice == "cfp")$ats_accuracy)))
})

test_that("floating-point zero edges are abstentions, not phantom ATS picks", {
  x <- data.frame(expected_margin = c(-6+1e-15, 1e-15, 7.00001),
    closing_home_spread = c(6, 3, -7), actual_margin = c(-3, 7, 10))
  y <- historical_sp_grade(x)
  expect_equal(y$selected, c(FALSE, TRUE, TRUE))
  expect_true(is.na(y$ats_correct[1])); expect_true(is.na(y$su_correct[2]))
  expect_identical(y$expected_margin, x$expected_margin)
})

test_that("future results cannot change forecasts; paired market has no ATS decisions", {
  f <- historical_sp_fixture(); x <- historical_sp_forecasts(f$reference, f$rows)$predictions
  f$reference$actual_margin <- 100
  y <- historical_sp_forecasts(f$reference, f$rows)$predictions
  expect_identical(x$expected_margin, y$expected_margin)
  paired <- historical_sp_paired(x)$paired
  expect_true(all(subset(paired, candidate == "market_reference")$ats_common_decisions == 0L))
  expect_true(all(is.na(paired$mae_low)))
})

test_that("raw values and pre-cycle records are rechecked and previous rejection is preserved", {
  d <- tempfile(); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  jsonlite::write_json(list(), file.path(d, "inventory.json"))
  raw <- data.frame(Team = c("A", "B"), Date = "2023-10-28", Record = "0-0", check.names = FALSE)
  raw[["SP+"]] <- c(5, 0); raw[["Off. SP+"]] <- c(25, 20); raw[["Def. SP+"]] <- 20
  original <- og_public_table
  assign("og_public_table", function(...) raw, envir = globalenv())
  withr::defer(assign("og_public_table", original, envir = globalenv()))
  candidates <- og_extract_public_sp(raw); candidates$season <- 2023L; candidates$snapshot_label <- "FBS week 9"
  ledger <- candidates; ledger$cycle <- "2023-10-23"; ledger$record_passed <- TRUE
  ledger$reconstruction_status <- c("corroborated_reconstruction", "record_mismatch")
  anchors <- data.frame(season = 2023L, snapshot_label = "FBS week 9", team = "A", field = "rating",
    expected = 5, previous_label = "", release_date = "2023-10-24")
  games <- historical_sp_fixture()$games
  a <- historical_sp_recheck(ledger, candidates, anchors, games, d)
  expect_equal(a$rows$qualified, c(TRUE, FALSE))
  expect_equal(a$rows$corroborated_by, rep("2023-10-25T12:00:00.000Z", 2))
  games$kickoff[1] <- parse_utc_datetime("2023-10-21T18:00:00Z")
  expect_false(any(historical_sp_recheck(ledger, candidates, anchors, games, d)$rows$qualified))
  candidates$rating[1] <- 999
  expect_error(historical_sp_recheck(ledger, candidates, anchors, games, d), "Raw anchor")
})
