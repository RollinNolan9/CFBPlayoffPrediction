library(testthat)
for (file in c("config.R", "models.R", "dashboard.R")) source(file.path("cfb_v2", file))
source("cfb_v3/publication.R")

fixture <- function() {
  card <- data.frame(game_id = "1", home = "Home", away = "Away", expected_margin = 3,
    margin_sd = 14, straight_up_pick = "Home", ats_pick = "Home", market_home_spread = -2.5,
    forced_pick = TRUE, pick_status = "article_pick", reported_confidence = "low",
    market_captured_at = "2026-09-09T00:00:00Z")
  lines <- data.frame(game_id = "1", home_spread = -3.5, total = 50,
    opening_home_spread = -2, captured_at = "2026-09-11T00:00:00Z")
  list(card = card, lines = lines, bundle = list(ats_model = NULL, ats_threshold = NULL,
    config = cfb_v2_config(getwd(), 2026)))
}

test_that("repricing flips the side without changing the frozen forecast", {
  x <- fixture()
  p <- reprice_publication_card(x$card, x$lines, x$bundle)
  expect_identical(p$expected_margin, x$card$expected_margin)
  expect_identical(p$margin_sd, x$card$margin_sd)
  expect_equal(p$straight_up_pick, "Home")
  expect_equal(p$ats_pick_line, "Away +3.5")
  expect_equal(p$pick_edge, .5)
  expect_equal(p$home_cover_probability, .5)
  expect_false(p$ats_calibrated)
  expect_true(is.na(p$fanduel_home_spread))
  expect_equal(p$previous_market_captured_at, "2026-09-09T00:00:00Z")
})

test_that("publication refuses missing and duplicate quotes", {
  x <- fixture()
  expect_error(reprice_publication_card(x$card, x$lines[FALSE, ], x$bundle), "missing")
  expect_error(reprice_publication_card(x$card, rbind(x$lines, x$lines), x$bundle))
  x$lines$home_spread <- NA_real_
  expect_error(reprice_publication_card(x$card, x$lines, x$bundle), "finite spread")
})

test_that("repricing cannot remove a manual-review restriction", {
  x <- fixture()
  x$card$pick_status <- "injury_conflict_review"
  x$card$reported_confidence <- "provisional"
  p <- reprice_publication_card(x$card, x$lines, x$bundle)
  expect_equal(p$pick_status, "injury_conflict_review")
  expect_equal(p$reported_confidence, "provisional")
})

test_that("presentation flags evidence without changing model status", {
  x <- fixture()$card
  x$evidence_note <- "No current FBS efficiency sample"
  x$availability_note <- "Starter questionable; not quantified"
  x$neutral_site <- TRUE
  p <- prepare_dashboard_predictions(x)
  expect_true(p$provisional_flag)
  expect_true(p$injury_flag)
  expect_equal(p$game_label, "Away vs Home")
  expect_equal(p$pick_status, x$pick_status)
  expect_equal(p$reported_confidence, "low")
  expect_equal(p$expected_margin, x$expected_margin)
  x$evidence_note <- NA_character_
  x$availability_note <- NA_character_
  expect_false(prepare_dashboard_predictions(x)$provisional_flag)
  expect_false(prepare_dashboard_predictions(x)$injury_flag)
})
