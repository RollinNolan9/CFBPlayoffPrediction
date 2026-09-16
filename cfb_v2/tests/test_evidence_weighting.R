library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R", "models.R",
  "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R")) source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "evidence_weighting.R")) source(file.path("cfb_v3", "experiments", file))

evidence_fixture <- function() {
  config <- cfb_v2_config(getwd(), 2026L)
  config$preseason$production_features <- "talent_percentile"
  priors <- data.frame(team = rep(c("A", "B"), 5), season = rep(2020:2024, each = 2),
    talent_percentile = rep(c(.1, .9), 5))
  data <- data.frame(game_id = as.character(1:7), season = 2024L, week = c(2, 3, 4, 5, 1, 1, 0),
    home = "A", away = "B", home_level = "fbs", away_level = "fbs",
    postseason_type = c(rep("regular", 4), "cfp", "regular", "regular"),
    home_source_games = c(0, 1, 0, 0, 0, 0, 0), away_source_games = c(1, 0, 0, 0, 0, 0, 0),
    margin = 0, power_rating_diff = -5, home_field_points = 2.4)
  data <- attach_preseason_features(data, priors, config, features = "talent_percentile", prefix = "ps_")
  list(data = data, priors = priors, config = config, centers = c(talent_percentile = .5))
}

test_that("zero sample retention is per team and stops after week four", {
  f <- evidence_fixture()
  out <- attach_evidence_weighting(f$data, f$priors, f$centers, f$config)
  expect_equal(out$ledger$home_roster_weight, c(1, .3, 1, 0, 0, 1, 1))
  expect_equal(out$ledger$away_roster_weight, c(.6, 1, 1, 0, 0, 1, 1))
  expect_equal(out$data$ps_talent_percentile_diff, c(-.64, -.52, -.8, 0, 0, -.8, -.8))
  unchanged <- setdiff(names(f$data), "ps_talent_percentile_diff")
  expect_identical(out$data[unchanged], f$data[unchanged])
  expect_identical(out$data$ps_talent_percentile_diff[4:7], f$data$ps_talent_percentile_diff[4:7])
})

test_that("centering does not reward a weak team and swapping teams negates features", {
  f <- evidence_fixture()
  out <- attach_evidence_weighting(f$data, f$priors, f$centers, f$config)
  expect_lt(out$data$ps_talent_percentile_diff[1], f$data$ps_talent_percentile_diff[1])
  swapped <- f$data
  swapped$home <- f$data$away; swapped$away <- f$data$home
  swapped$home_source_games <- f$data$away_source_games
  swapped$away_source_games <- f$data$home_source_games
  swapped$ps_talent_percentile_diff <- -f$data$ps_talent_percentile_diff
  reverse <- attach_evidence_weighting(swapped, f$priors, f$centers, f$config)
  expect_equal(reverse$data$ps_talent_percentile_diff, -out$data$ps_talent_percentile_diff)
})

test_that("missing scouting remains missing and unknown sample counts are not zero", {
  f <- evidence_fixture(); f$priors$talent_percentile[f$priors$team == "B"] <- NA_real_
  f$data <- attach_preseason_features(f$data, f$priors, f$config, features = "talent_percentile", prefix = "ps_")
  expect_true(all(is.na(attach_evidence_weighting(f$data, f$priors, f$centers, f$config)$data$ps_talent_percentile_diff)))
  f <- evidence_fixture(); f$data$home_source_games[1] <- NA_real_
  expect_error(attach_evidence_weighting(f$data, f$priors, f$centers, f$config), "counts")
  f$data$home_source_games[1] <- -.1
  expect_error(attach_evidence_weighting(f$data, f$priors, f$centers, f$config), "counts")
  f$data$home_source_games[1] <- .5
  expect_error(attach_evidence_weighting(f$data, f$priors, f$centers, f$config), "counts")
})

test_that("FCS opponents do not receive the FBS retention rule", {
  f <- evidence_fixture(); f$data$home_level[1] <- "fcs"
  out <- attach_evidence_weighting(f$data, f$priors, f$centers, f$config)
  expect_false(out$ledger$home_hold[1])
  expect_equal(out$ledger$home_roster_weight[1], .6)
  expect_identical(out$data$ps_talent_percentile_diff[1], f$data$ps_talent_percentile_diff[1])
})

test_that("training centers cannot use held-out seasons or duplicate team years", {
  f <- evidence_fixture()
  before <- fit_evidence_centers(f$priors, 2020:2022, 2023, "talent_percentile")
  f$priors$talent_percentile[f$priors$season >= 2023] <- 999
  expect_equal(fit_evidence_centers(f$priors, 2020:2022, 2023, "talent_percentile"), before)
  expect_error(fit_evidence_centers(f$priors, 2020:2023, 2023, "talent_percentile"))
  expect_error(fit_evidence_centers(rbind(f$priors, f$priors[1, ]), 2020:2022, 2023, "talent_percentile"), "duplicate")
})

test_that("FCS opener labels use completed games before the feature cutoff", {
  target <- data.frame(game_id = "target", season = 2024L, home = "A", away = "B")
  games <- data.frame(game_id = c("target", "a_opener", "b_opener", "future", "same_cycle"),
    season = 2024L, home = c("A", "A", "B", "A", "A"), away = c("B", "X", "Y", "B", "B"),
    home_level = "fbs", away_level = c("fbs", "fcs", "fbs", "fbs", "fbs"), completed = TRUE,
    kickoff = c("2024-09-07T18:00:00Z", "2024-08-31T18:00:00Z", "2024-08-31T18:00:00Z",
      "2024-09-14T18:00:00Z", "2024-09-02T00:00:00Z"), feature_week_start = "2024-09-02")
  out <- evidence_opener_ledger(target, games)
  expect_equal(out$fcs_only, c(TRUE, FALSE))
  expect_equal(out$single_fcs_opener, c(TRUE, FALSE))
  expect_equal(out$prior_games, c(1L, 1L))
  expect_error(evidence_opener_ledger(target, rbind(games, games[1, ])), "duplicate")
  games$home[1] <- "WRONG"
  expect_error(evidence_opener_ledger(target, games), "metadata")
})

test_that("roster refit uses past labels and the same declared features", {
  f <- evidence_fixture(); set.seed(55)
  data <- f$data[rep(1:7, length.out = 160), ]
  data$game_id <- as.character(seq_len(nrow(data)))
  data$season <- rep(2020:2023, each = 40)
  data$power_rating_diff <- rnorm(nrow(data), 0, 8)
  data$margin <- 1.5*data$power_rating_diff+rnorm(nrow(data))
  data <- attach_preseason_features(data, f$priors, f$config, features = "talent_percentile", prefix = "ps_")
  features <- c("power_rating_diff", "home_field_points", "ps_talent_percentile_diff")
  before <- fit_evidence_roster(data, rep(1, nrow(data)), f$priors, f$config, 2023L, features, 8)
  data$margin[data$season == 2023] <- 999
  after <- fit_evidence_roster(data, rep(1, nrow(data)), f$priors, f$config, 2023L, features, 8)
  expect_equal(before$candidate_margin, after$candidate_margin)
  expect_identical(before$candidate$features, features)
  expect_equal(before$candidate$trained_seasons, 2020:2022)
  expect_equal(before$candidate$trained_rows, before$base$trained_rows)
  expect_equal(before$base_margin, after$base_margin)
})
