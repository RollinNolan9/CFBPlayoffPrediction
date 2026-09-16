library(testthat)
for (file in c("store.R", "features.R")) source(file.path("cfb_v2", file))
for (file in c("audit_og_inputs.R", "controlled_ats.R", "external_ratings.R"))
  source(file.path("cfb_v3/experiments", file))

external_fixture <- function() {
  card <- data.frame(game_id = as.character(1:4), season = 2026L, week = 2L,
    home = c("A", "C", "E", "G"), away = c("B", "D", "F", "H"),
    kickoff = "2026-09-12T16:00:00Z", neutral_site = c(FALSE, TRUE, FALSE, FALSE),
    expected_margin = c(8, -4, 0, 40), market_home_spread = c(-7, 3, 0, -31),
    market_captured_at = "2026-09-11T19:00:00Z", market_provider = "DraftKings",
    home_spread_odds = c(-110, -110, -110, NA), away_spread_odds = c(-117, 150, -110, NA),
    home_conference = c("SEC", "Big Ten", "SEC", "Big Ten"), away_conference = "ACC",
    is_cfp = FALSE, postseason_type = "regular")
  ratings <- do.call(rbind, lapply(c("sp", "sagarin_predictor", "fpi"), function(model)
    data.frame(model = model, season = 2026L, team = LETTERS[1:8], source_team = LETTERS[1:8],
      rating = c(5, 0, 0, 6, 0, 0, 45, 0), captured_at = "2026-09-11T20:00:00Z",
      source_label = "Week 2", source_url = "https://example.org", home_advantage = 2.4)))
  results <- card[c("game_id", "season", "home", "away", "kickoff")]
  results$home_score <- c(27, 10, 14, 50); results$away_score <- c(20, 14, 14, 10)
  results$completed <- TRUE
  list(card = card, ratings = ratings, cutoff = "2026-09-11T21:00:00Z", results = results)
}

test_that("numeric ratings, venue adjustment and fixed blend have correct signs", {
  f <- external_fixture(); b <- external_build(f$card, f$ratings, f$cutoff)
  expect_equal(b$comparison$sp_home_margin, c(7.4, -6, 2.4, 47.4))
  expect_equal(b$comparison$v3_plus_consensus_home_margin, c(7.7, -5, 1.2, 43.7))
  expect_true(all(b$coverage$common_ratings))
  p <- b$predictions[b$predictions$model == "sp", ]
  expect_equal(p$fair_home_spread, -p$expected_margin)
  expect_equal(p$ats_pick, c("A", "D", "E", "G"))
  expect_gt(p$expected_margin[4], 21)
  expect_identical(f$card, external_fixture()$card)
})

test_that("missing sources never silently change the consensus or multiply games", {
  f <- external_fixture(); r <- subset(f$ratings, model != "fpi" | team != "A")
  b <- external_build(f$card, r, f$cutoff)
  expect_equal(b$coverage$fpi[1], "missing_home")
  expect_true(is.na(b$comparison$external_consensus_home_margin[1]))
  expect_false(b$coverage$common_ratings[1])
  expect_equal(nrow(b$predictions), 28L)
  expect_error(external_build(f$card, rbind(r, r[1, ]), f$cutoff), "duplicate|Duplicate")
  expect_error(external_build(rbind(f$card, f$card[1, ]), r, f$cutoff), "duplicate|Duplicate")
  empty <- external_build(f$card, external_empty_ratings(), f$cutoff)
  expect_true(all(is.na(empty$comparison$external_consensus_home_margin)))
})

test_that("timestamp and kickoff gates are strict and exclude non-CFP bowls", {
  f <- external_fixture(); f$card$kickoff[1] <- f$cutoff
  f$card$market_captured_at[2] <- "2026-09-11T22:00:00Z"
  f$card$kickoff[3] <- "2026-09-12T16:00:00-04:00"
  f$card$postseason_type[4] <- "bowl"
  b <- external_build(f$card, f$ratings, f$cutoff)
  expect_equal(b$coverage$game_status, c("already_started_at_capture", "included", "unknown_kickoff", "excluded_non_cfp_bowl"))
  expect_true(is.na(b$comparison$line_home_spread[2]))
  f$ratings$captured_at[1] <- "2026-09-11T22:00:00Z"
  expect_error(external_build(f$card, f$ratings, f$cutoff), "after cutoff")
  expect_true(is.na(external_time("2026-09-11")))
})

test_that("grading handles pushes, no-picks, prices and market reference", {
  f <- external_fixture(); b <- external_build(f$card, f$ratings, f$cutoff)
  g <- external_grade(b$predictions, f$results)
  v <- subset(g, model == "v3")
  expect_equal(v$push, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(v$win, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(v$profit, c(0, 1.5, NA, NA))
  expect_false(any(subset(g, model == "market_reference")$selected))
  m <- subset(external_metrics(g), model == "v3" & slice == "all" & cohort == "common_four_way")
  expect_equal(m$margin_mae, 1/4)
  expect_equal(m$su_decisions, 3L)
  expect_equal(m$ats_accuracy, 1)
  expect_equal(m$roi_priced_subset, .75)
  expect_true(is.na(m$roi_all_picks))
  expect_true(all(is.na(external_paired(g)$mae_change_low)))
})

test_that("pending games, changed identities and duplicate results cannot be scored", {
  f <- external_fixture(); p <- external_build(f$card, f$ratings, f$cutoff)$predictions
  r <- f$results; r$completed <- FALSE
  expect_false(any(external_grade(p, r)$scored))
  r <- f$results; r$home[1] <- "OTHER"
  expect_error(external_grade(p, r), "metadata mismatch")
  r <- f$results; r$kickoff[1] <- f$cutoff
  expect_error(external_grade(p, r), "kickoff")
  expect_error(external_grade(p, rbind(f$results, f$results[1, ])), "duplicate|Duplicate")
  expect_error(external_grade(rbind(p, p[1, ]), f$results), "duplicate|Duplicate")
  expect_true(all(is.na(external_grade(p, f$results[0, ])$actual_margin)))
})

test_that("changing outcomes cannot change frozen forecasts and paired cohorts are matched", {
  f <- external_fixture(); p <- external_build(f$card, f$ratings, f$cutoff)$predictions
  a <- external_grade(p, f$results); f$results$home_score <- 100
  b <- external_grade(p, f$results)
  expect_identical(a$expected_margin, b$expected_margin)
  a$scored[a$model == "fpi" & a$game_id == "1"] <- FALSE
  expect_equal(subset(external_paired(a), candidate == "fpi")$common_games, 3L)
  expect_equal(subset(external_paired(a), candidate == "sp")$common_games, 4L)
})

test_that("Sagarin parser selects Predictor rather than composite and rejects wrong season", {
  p <- tempfile(fileext = ".html"); on.exit(unlink(p))
  writeLines(c("<html><pre>2026 College Football through games of September 7 Monday - Week 1",
    "RATING | PREDICTOR | GOLDEN_MEAN | RECENT",
    " 1 Miami-Florida A = 92.28 1 0 67.27( 61) 0 0 | 0 0 | 93.92 7 | 94.73 7 | 89.29 5",
    " 2 Army West Point A = 71.47 1 0 42.56(213) 0 0 | 0 0 | 66.22 71 | 64.98 78 | 89.23 6",
    "</pre></html>"), p)
  x <- external_parse_sagarin(p, 2026)
  expect_equal(x$team, c("Miami", "Army")); expect_equal(x$rating, c(93.92, 66.22))
  expect_error(external_parse_sagarin(p, 2025), "season")
})

test_that("FPI parser uses numeric power rather than rank and checks season", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p))
  jsonlite::write_json(data.frame(year = 2026, team = "USC", fpi = 18.5, rank = 1), p)
  x <- external_parse_fpi(p, 2026)
  expect_equal(x$rating, 18.5); expect_equal(x$team, "Southern California")
  expect_error(external_parse_fpi(p, 2025), "season")
  expect_error(external_parse_sp("unused", "FBS FINAL"), "weekly")
})

test_that("source errors are explicit and do not corrupt other captures", {
  d <- tempfile(); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  original <- external_fetch
  assign("external_fetch", function(...) stop("TLS validation failed"), envir = globalenv())
  withr::defer(assign("external_fetch", original, envir = globalenv()))
  s <- list(season = 2026, sp_url = "sp", sagarin_url = "sagarin", fpi_url = "fpi", rating_to_margin_home_advantage = 2.4)
  x <- external_capture(d, s, 2026, 2)
  expect_equal(nrow(x$ratings), 0L)
  expect_equal(nrow(x$sources), 3L)
  expect_true(all(grepl("TLS validation failed", x$sources$status)))
  expect_error(external_capture(d, s, 2027, 2), "season")
})

test_that("snapshot checksums detect edited or missing artifacts", {
  d <- tempfile(); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  p <- file.path(d, "predictions.csv"); writeLines("original", p)
  external_seal(d); expect_true(external_verify(d))
  writeLines("changed", p); expect_error(external_verify(d), "changed")
  unlink(p); expect_error(external_verify(d), "changed")
})

test_that("market cohort and paired ATS exclude different coverage and abstentions", {
  f <- external_fixture(); f$card$market_home_spread[4] <- NA
  b <- external_build(f$card, f$ratings, f$cutoff)
  g <- external_grade(b$predictions, f$results)
  m <- subset(external_metrics(g), cohort == "common_four_way_lined" & slice == "all")
  expect_true(all(m$games == 3L))
  pairs <- external_paired(g)
  expect_equal(subset(pairs, candidate == "sp")$ats_common_decisions, 1L)
  expect_equal(subset(pairs, candidate == "market_reference")$ats_common_decisions, 0L)
  expect_true(is.na(subset(pairs, candidate == "market_reference")$ats_accuracy_change))
})

test_that("conference enrichment checks keys and identities without changing forecasts", {
  f <- external_fixture(); card <- f$card
  card$home_conference <- NULL; card$away_conference <- NULL
  raw <- data.frame(id = card$game_id, season = card$season, homeTeam = card$home,
    awayTeam = card$away, homeConference = f$card$home_conference, awayConference = f$card$away_conference)
  enriched <- external_card_metadata(card, rbind(raw, raw))
  expect_equal(enriched$home_conference, f$card$home_conference)
  expect_equal(enriched$expected_margin, card$expected_margin)
  raw$homeTeam[1] <- "WRONG"
  expect_error(external_card_metadata(card, raw), "mismatch")
})

test_that("offline score writes new artifacts and does not alter the frozen snapshot", {
  f <- external_fixture(); project <- tempfile(); dir.create(project)
  on.exit(unlink(project, recursive = TRUE))
  snapshot <- file.path(project, "snapshot"); dir.create(snapshot)
  p <- external_build(f$card, f$ratings, f$cutoff)$predictions
  write.csv(p, file.path(snapshot, "predictions.csv"), row.names = FALSE)
  jsonlite::write_json(list(status = "complete", protocol = "external-ratings-v1"),
    file.path(snapshot, "manifest.json"), auto_unbox = TRUE)
  external_seal(snapshot)
  result_file <- file.path(project, "results.csv"); write.csv(f$results, result_file, row.names = FALSE)
  output <- external_score(project, snapshot, result_file)
  expect_true(external_verify(snapshot)); expect_true(external_verify(output))
  expect_equal(sum(read.csv(file.path(output, "graded_predictions.csv"))$scored), 28L)
  expect_error(external_score(project, c(snapshot, snapshot), result_file), "duplicate|Duplicate")
})
