library(testthat)
root <- if (file.exists("cfb_v2/config.R")) normalizePath(getwd(), winslash = "/") else
  normalizePath("../..", winslash = "/")
for (module in c("config", "store", "features", "dashboard"))
  source(file.path(root, "cfb_v2", paste0(module, ".R")))
source(file.path(root, "cfb_v3/injury_overlay.R"))
config <- cfb_v2_config(root, 2026)
baseline_time <- "2026-10-07T19:00:00Z"
cutoff <- "2026-10-07T20:00:00Z"
test_card <- function() {
  x <- data.frame(game_id = c("one", "two"), season = 2026, week = 6,
    home = c("Pittsburgh", "Florida"), away = c("Georgia", "Alabama"),
    kickoff = "2026-10-10T19:00:00Z", as_of = baseline_time,
    expected_margin = c(5, 8), foundation_expected_margin = c(5, 8),
    market_home_spread = c(-3, -7), fair_spread = c(-5, -8), ats_edge_home = c(2, 1),
    margin_sd = NA_real_, home_win_probability = NA_real_, confidence_tier = NA_character_,
    injuries_adjusted = FALSE, market_captured_at = baseline_time, top_drivers = "Frozen model feature",
    stringsAsFactors = FALSE)
  x <- prepare_dashboard_predictions(x)
  x$confidence_tier <- x$reported_confidence <- NA_character_
  x
}
test_report <- function(team = "Pittsburgh", impact = 4) {
  data.frame(game_id = "one", team = team, player_id = "qb", player_name = "Quarterback",
    position = "QB", starter = TRUE, status = "out", source_type = "school_official",
    source_url = "https://example.test/report", source_timestamp = "2026-10-07T19:10:00Z",
    impact_points = impact, estimate_game_id = "one", baseline_as_of = baseline_time,
    input_verified = TRUE, estimate_method = "same-cohort EPA scenario", stringsAsFactors = FALSE)
}
test_registry <- function() {
  r <- test_report()
  r$season <- 2026
  r$through_week <- 99
  r
}

test_that("signed home and away estimates change margin, ATS and displayed values only", {
  card <- test_card()
  home <- apply_injury_overlay(card, test_report(), cutoff, config)$predictions
  expect_equal(home$expected_margin, c(1, 8))
  expect_equal(home$injury_adjustment, c(-4, 0))
  expect_equal(home$fair_spread[1], -1)
  expect_equal(home$ats_edge_home[1], -2)
  expect_equal(home$ats_pick[1], "Georgia")
  expect_equal(home$model_line[1], "Pittsburgh -1.0")
  expect_equal(home$foundation_expected_margin, card$expected_margin)
  expect_equal(home$foundation_model_line[1], "Pittsburgh -5.0")
  expect_equal(home$pick_status[1], "injury_conflict_review")
  expect_equal(home$status_label[1], "Injury review")
  expect_true(home$injury_flag[1])
  expect_identical(home[2, names(card)], card[2, ])
  expect_identical(home$market_home_spread, card$market_home_spread)
  expect_identical(home$top_drivers, card$top_drivers)
  expect_true(all(is.na(home$home_cover_probability)))
  expect_true(all(is.na(home$confidence_tier)))
  away <- apply_injury_overlay(card, test_report("Georgia"), cutoff, config)$predictions
  expect_equal(away$expected_margin[1], 9)
  negative <- apply_injury_overlay(card, test_report(impact = -2), cutoff, config)$predictions
  expect_equal(negative$expected_margin[1], 7)
  flips <- apply_injury_overlay(card, test_report(impact = 7), cutoff, config)$predictions
  expect_equal(flips$straight_up_pick[1], "Georgia")
})

test_that("missing or unverified impact is explicit review, never a zero-valued healthy estimate", {
  r <- test_report(impact = NA_real_)
  result <- apply_injury_overlay(test_card(), r, cutoff, config)
  expect_equal(result$predictions$expected_margin, c(5, 8))
  expect_true(is.na(result$predictions$injury_adjustment[1]))
  expect_false(result$predictions$injuries_adjusted[1])
  expect_equal(result$predictions$injury_estimate_status[1], "unestimated_confirmed_absence")
  expect_match(result$predictions$availability_note[1], "no usable fresh point estimate")
  expect_equal(result$audit$estimate_status, "unestimated_confirmed_absence")
  r <- test_report(); r$input_verified <- FALSE
  expect_equal(apply_injury_overlay(test_card(), r, cutoff, config)$predictions$expected_margin[1], 5)
  r <- test_report(impact = 0)
  zero <- apply_injury_overlay(test_card(), r, cutoff, config)$predictions
  expect_true(zero$injuries_adjusted[1])
  expect_equal(zero$injury_estimate_status[1], "replacement_scenario")
})

test_that("report updates deduplicate and active clears an old Out", {
  old <- test_report()
  newer <- old; newer$source_timestamp <- "2026-10-07T19:20:00Z"
  repeated <- apply_injury_overlay(test_card(), rbind(old, newer), cutoff, config)
  expect_equal(repeated$predictions$expected_margin[1], 1)
  expect_equal(nrow(repeated$reports), 1)
  newer$status <- "active"; newer$impact_points <- NA
  active <- apply_injury_overlay(test_card(), rbind(old, newer), cutoff, config)
  expect_equal(active$predictions$expected_margin[1], 5)
  expect_equal(active$predictions$injury_estimate_status[1], "active_no_adjustment")
  expect_false(active$predictions$injury_flag[1])
  newer$source_timestamp <- old$source_timestamp
  expect_error(apply_injury_overlay(test_card(), rbind(old, newer), cutoff, config), "Conflicting")
})

test_that("wrong estimate identity, stale baseline, future evidence and overlapping losses are rejected", {
  r <- test_report(); r$estimate_game_id <- "two"
  expect_error(apply_injury_overlay(test_card(), r, cutoff, config), "Estimate game")
  r <- test_report(); r$baseline_as_of <- "2026-10-06T19:00:00Z"
  expect_error(apply_injury_overlay(test_card(), r, cutoff, config), "baseline_as_of")
  r <- test_report(); r$source_timestamp <- "2026-10-07T21:00:00Z"
  expect_error(apply_injury_overlay(test_card(), r, cutoff, config), "Future injury")
  r <- test_report("Florida")
  expect_error(apply_injury_overlay(test_card(), r, cutoff, config), "team does not match")
  r <- test_report(); r$game_id <- "missing"
  expect_error(apply_injury_overlay(test_card(), r, cutoff, config), "not in this card")
  second <- test_report(); second$player_id <- "receiver"; second$player_name <- "Receiver"
  expect_error(apply_injury_overlay(test_card(), rbind(test_report(), second), cutoff, config), "joint estimate")
  overlaid <- apply_injury_overlay(test_card(), test_report(), cutoff, config)$predictions
  expect_error(apply_injury_overlay(overlaid, test_report(), cutoff, config), "second injury overlay")
  unknown <- apply_injury_overlay(test_card(), test_report(impact = NA), cutoff, config)$predictions
  expect_error(apply_injury_overlay(unknown, test_report(), cutoff, config), "second injury overlay")
})

test_that("season registry maps confirmed absence to each new game but never carries a point estimate", {
  r <- test_registry(); r$status <- "out for season"
  resolved <- resolve_season_availability(r, test_card(), cutoff)
  expect_equal(resolved$game_id, "one")
  expect_equal(resolved$status, "out")
  expect_false(any(c("impact_points", "baseline_as_of", "estimate_game_id", "input_verified") %in% names(resolved)))
  next_card <- test_card(); next_card$game_id[1] <- "next"; next_card$week <- 7
  next_report <- resolve_season_availability(r, next_card, cutoff)
  expect_equal(next_report$game_id, "next")
  result <- apply_injury_overlay(next_card, next_report, cutoff, config)$predictions
  expect_equal(result$expected_margin[1], 5)
  expect_equal(result$injury_estimate_status[1], "unestimated_confirmed_absence")
  r$season <- 2025
  expect_equal(nrow(resolve_season_availability(r, test_card(), cutoff)), 0)
})

test_that("registry clearance and expiration resolve before older indefinite statuses", {
  old <- test_registry(); old$source_timestamp <- "2026-10-01T10:00:00Z"
  newer <- old; newer$source_timestamp <- "2026-10-06T10:00:00Z"; newer$status <- "active"
  resolved <- resolve_season_availability(rbind(old, newer), test_card(), cutoff)
  expect_equal(resolved$status, "active")
  newer$source_timestamp <- "2026-10-08T10:00:00Z"
  expect_equal(resolve_season_availability(rbind(old, newer), test_card(), cutoff)$status, "out")
  newer$source_timestamp <- "2026-10-06T10:00:00Z"; newer$status <- "out"; newer$through_week <- 5
  expect_equal(nrow(resolve_season_availability(rbind(old, newer), test_card(), cutoff)), 0)
  old$through_week <- "unrecognized"
  expect_error(resolve_season_availability(old, test_card(), cutoff), "through_week")
})

test_that("the saved live card becomes review-only for the real registry and otherwise stays frozen", {
  path <- file.path(root, "cfb_v3/output/2026/week_6_relative_preliminary/20261007T185200Z/predictions.csv")
  registry_path <- file.path(root, "cfb_v3/inbox/player_availability.csv")
  skip_if_not(file.exists(path) && file.exists(registry_path))
  card <- read.csv(path, stringsAsFactors = FALSE)
  registry <- read.csv(registry_path, stringsAsFactors = FALSE)
  reports <- resolve_season_availability(registry, card, cutoff)
  result <- apply_injury_overlay(card, reports, cutoff, config)
  expect_equal(nrow(result$predictions), 57)
  expect_identical(result$predictions$expected_margin, card$expected_margin)
  expect_equal(sum(result$predictions$injury_estimate_status == "unestimated_confirmed_absence"), 1)
  pitt <- which(card$home == "Pittsburgh" | card$away == "Pittsburgh")
  expect_equal(result$predictions$pick_status[pitt], "injury_conflict_review")
  expect_true(is.na(result$predictions$injury_adjustment[pitt]))
  for (name in names(card)) {
    before <- card[[name]][-pitt]; after <- result$predictions[[name]][-pitt]
    expect_true(all((is.na(before) & is.na(after)) | (!is.na(before) & !is.na(after) & before == after)), info = name)
  }
})
