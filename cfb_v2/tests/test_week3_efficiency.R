library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R", "models.R",
  "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R"))
  source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "matchup_foundation_audit.R", "week3_efficiency.R"))
  source(file.path("cfb_v3/experiments", file))

week3_fixture <- function() {
  games <- standardize_schedule(data.frame(game_id = as.character(1:9),
    season = c(2023L, 2023L, 2024L, rep(2024L, 6)), week = c(1L, 15L, 0L, 1:5, 3L),
    model_week = c(1L, 15L, 0:5, 16L), home = "A", away = c("B", "B", "FCS", rep("B", 6)),
    home_level = "fbs", away_level = c("fbs", "fbs", "fcs", rep("fbs", 6)),
    postseason_type = c("regular", "bowl", rep("regular", 6), "cfp"),
    is_cfp = c(rep(FALSE, 8), TRUE), completed = TRUE, margin = 7,
    kickoff = as.POSIXct(c("2023-09-01", "2023-12-20", "2024-08-25", "2024-09-01",
      "2024-09-08", "2024-09-15", "2024-09-22", "2024-09-29", "2024-12-20"), tz = "UTC")))
  tg <- data.frame(game_id = games$game_id, team = "A", season = games$season,
    week = games$week, model_week = games$model_week, kickoff = games$kickoff,
    net_efficiency = c(.8, 99, 99, .1, .3, .2, .2, .2, .2))
  for (field in week3_efficiency_metrics()) tg[[field]] <- tg$net_efficiency
  power <- data.frame(team = "A", season = games$season, model_week = games$model_week, power_rating = 0)
  list(games = games, tg = tg, power = power, config = cfb_v2_config(getwd(), 2026))
}

test_that("only locked weights are accepted and production closures remain untouched", {
  config <- cfb_v2_config(getwd(), 2026); before <- config
  original_env <- environment(build_team_pregame_snapshots)
  custom <- week3_efficiency_builder(.5)
  blend <- environment(custom)$blend_current_with_history
  expect_equal(blend(.2, .8, 3L, config), .5)
  expect_equal(blend_current_with_history(.2, .8, 3L, config), .38)
  expect_identical(config, before)
  expect_identical(environment(build_team_pregame_snapshots), original_env)
  expect_error(week3_efficiency_builder(.4), "locked")
  expect_error(week3_efficiency_builder(NA_real_), "locked")
})

test_that("existing missing-current and missing-prior fallbacks are retained", {
  blend <- environment(week3_efficiency_builder(.5))$blend_current_with_history
  config <- cfb_v2_config(getwd(), 2026)
  expect_equal(blend(NA_real_, .8, 3, config), .8)
  expect_equal(blend(.2, NA_real_, 3, config), .2)
  expect_true(is.na(blend(NA_real_, NA_real_, 3, config)))
  for (week in c(0:2, 4:8, 99))
    expect_identical(blend(.2, .8, week, config), blend_current_with_history(.2, .8, week, config))
})

test_that("snapshot replay changes Week 3 efficiency but excludes bowls and FCS", {
  f <- week3_fixture()
  old <- build_team_pregame_snapshots(f$tg, f$games, f$power, f$config)$snapshots
  replay <- week3_efficiency_builder(.3)(f$tg, f$games, f$power, f$config)$snapshots
  new <- week3_efficiency_builder(.5)(f$tg, f$games, f$power, f$config)$snapshots
  expect_identical(replay, old)
  target <- old$game_id == "6"
  expect_equal(old$offense_epa[target], .7*.2 + .3*.8)
  expect_equal(new$offense_epa[target], .5*.2 + .5*.8)
  expect_equal(new$source_games[target], 2L)
  expect_identical(new[!target, ], old[!target, ])
  other <- setdiff(names(old), week3_efficiency_metrics())
  expect_identical(new[other], old[other])
})

test_that("same-week, future and target outcomes cannot alter Week 3 features", {
  f <- week3_fixture(); build <- week3_efficiency_builder(.5)
  a <- build(f$tg, f$games, f$power, f$config)$snapshots
  late <- f$tg$season == 2024 & f$tg$model_week >= 3
  for (field in c(week3_efficiency_metrics(), "net_efficiency")) f$tg[[field]][late] <- 999
  f$games$margin[f$games$season == 2024 & f$games$model_week >= 3] <- 999
  b <- build(f$tg, f$games, f$power, f$config)$snapshots
  expect_identical(a[a$game_id == "6", ], b[b$game_id == "6", ])
})

test_that("individual missing metric cells do not masquerade as extra observations", {
  f <- week3_fixture(); f$tg$pass_epa[f$tg$game_id == "5"] <- NA_real_
  x <- week3_efficiency_builder(.5)(f$tg, f$games, f$power, f$config)$snapshots
  expect_equal(x$pass_epa[x$game_id == "6"], .5*.1 + .5*.8)
  expect_equal(x$source_games[x$game_id == "6"], 2)
})

test_that("a missing prior uses the existing transition prior", {
  f <- week3_fixture(); f$tg <- f$tg[f$tg$season == 2024, ]
  prior <- data.frame(team = "A", season = 2024, net_efficiency = -.1)
  for (field in week3_efficiency_metrics()) prior[[field]] <- -.1
  x <- week3_efficiency_builder(.5)(f$tg, f$games, f$power, f$config, prior)$snapshots
  expect_equal(x$offense_epa[x$game_id == "6"], .5*.2 + .5*(-.1))
})

test_that("feature-scope guard rejects roster, power, schema and other-week changes", {
  base <- data.frame(game_id = c("1", "2"), week = c(3L, 4L), postseason_type = "regular",
    offense_epa_diff = .1, ps_talent_percentile_diff = .3, power_rating_diff = 2)
  candidate <- base; candidate$offense_epa_diff[1] <- .5
  expect_silent(week3_assert_feature_scope(base, candidate))
  candidate$offense_epa_diff[2] <- .5
  expect_error(week3_assert_feature_scope(base, candidate), "outside")
  candidate <- base; candidate$ps_talent_percentile_diff[1] <- .5
  expect_error(week3_assert_feature_scope(base, candidate), "Non-efficiency")
  candidate <- base; candidate$power_rating_diff[1] <- 3
  expect_error(week3_assert_feature_scope(base, candidate), "Non-efficiency")
  expect_error(week3_assert_feature_scope(base, base[2:1, ]), "identity")
})

test_that("sample groups use the smaller eligible sample and reject unknown counts", {
  expect_equal(week3_sample_group(c(2, 1, 2, 3), c(0, 2, 3, 3)),
    c("zero", "one", "two", "three_plus"))
  expect_error(week3_sample_group(NA, 2), "sample")
  expect_error(week3_sample_group(1.5, 2), "sample")
})

test_that("fixed refits ignore held-out labels and future feature distributions", {
  set.seed(1309)
  d <- data.frame(game_id = as.character(1:120), season = rep(2020:2022, each = 40),
    week = 3L, postseason_type = "regular", offense_epa_diff = rnorm(120),
    coach_rating_diff = 0, coach_rating_65_35_diff = 0, coach_rating_70_30_diff = 0)
  d$margin <- 3*d$offense_epa_diff + rnorm(120)
  input <- list(data = d, weights = rep(1, 120), covered_seasons = 2021:2022)
  choice <- data.frame(test_season = 2022, train_through_season = 2021,
    training_rows = 80, coach_recent_share = .65, foundation_lambda = 8, preseason_lambda = 8)
  features <- c("offense_epa_diff", "coach_rating_diff")
  config <- cfb_v2_config(getwd(), 2026)
  a <- foundation_fixed_forecasts(input, choice, features, config)
  input$data$margin[input$data$season == 2022] <- 999
  b <- foundation_fixed_forecasts(input, choice, features, config)
  expect_identical(a, b)
  extra <- input$data[input$data$season == 2022, ]; extra$season <- 2023
  extra$game_id <- paste0("future", extra$game_id); extra$offense_epa_diff <- 999
  input$data <- rbind(input$data, extra); input$weights <- rep(1, nrow(input$data))
  expect_identical(a, foundation_fixed_forecasts(input, choice, features, config))
  expect_error(foundation_fixed_forecasts(input, choice, c(features, "home"), config), "schema")
})

test_that("ATS grading keeps pushes, exact-edge passes and missing lines separate", {
  ref <- data.frame(game_id = as.character(1:5), season = 2024, home = "A", away = "B",
    actual_margin = c(1, 3, 7, 7, 7), closing_home_spread = c(-1.5, -3, -7, NA, -7), margin_sd = 14)
  d <- data.frame(game_id = ref$game_id, season = 2024, home = "A", away = "B",
    margin = ref$actual_margin, home_source_games = 1, away_source_games = 2)
  p <- data.frame(game_id = ref$game_id, expected_margin = c(2, 4, 7, 8, 9))
  out <- week3_score_pair(ref, list(v3_30 = p), d)
  expect_equal(out$loss, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(out$push, c(FALSE, TRUE, FALSE, FALSE, TRUE))
  expect_equal(out$selected, c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_false("margin_sd" %in% names(out))
  expect_equal(out$reference_margin_sd, rep(14, 5))
  expect_error(week3_score_pair(ref, list(v3_30 = p[-1, ]), d), "Missing")
  expect_error(week3_score_pair(rbind(ref, ref[1, ]), list(v3_30 = p), d), "duplicate")
})

test_that("empty sample slices retain zero games rather than fabricated accuracy", {
  x <- data.frame(model = rep(c("v3_30", "efficiency_50"), each = 2),
    game_id = rep(c("1", "2"), 2), season = rep(c(2023, 2024), 2), week = 3L,
    postseason_type = "regular", home = "A", away = "B", home_conference = "SEC",
    away_conference = "Big Ten", neutral_site = FALSE, is_cfp = FALSE,
    actual_margin = 10, expected_margin = 12, closing_home_spread = -7,
    home_source_games = 1, away_source_games = 1, sample_group = "one")
  summary <- week3_summaries(experiment_grade(x))
  empty <- summary$metrics[summary$metrics$slice == "week_3_min_sample_three_plus", ]
  expect_equal(empty$games, c(0, 0))
  expect_true(all(is.na(empty$ats_accuracy)))
  expect_equal(summary$paired$paired_ats_decisions[summary$paired$slice == "week_3"], 2)
})
