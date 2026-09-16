library(testthat)
for (file in c("config.R", "store.R", "features.R", "models.R", "workflow.R")) source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "external_ratings.R", "external_backtest.R", "prediction_tracker.R"))
  source(file.path("cfb_v3/experiments", file))

tracker_fixture <- function() {
  games <- data.frame(game_id = as.character(1:4), season = 2024L, week = c(1L, 2L, 3L, 1L),
    kickoff = c("2024-08-31T18:00:00Z", "2024-09-07T18:00:00Z", "2024-09-14T18:00:00Z", "2025-01-21T00:30:00Z"),
    home = c("A", "C", "A", "A"), away = c("B", "D", "C", "B"),
    home_score = c(21, 20, 14, 10), away_score = c(14, 27, 10, 24),
    neutral_site = c(FALSE, TRUE, FALSE, TRUE), postseason_type = c(rep("regular", 3), "cfp"),
    home_level = "fbs", away_level = "fbs", is_cfp = c(FALSE, FALSE, FALSE, TRUE),
    home_conference = "ACC", away_conference = "SEC", closing_home_spread = c(-7, 3, -3, 5),
    margin = c(7, -7, 4, -14), stringsAsFactors = FALSE)
  raw <- data.frame(Home = c("A", "D", "A", "A"), Road = c("B", "C", "C", "B"),
    week = c(1, 2, 3, 21), linesagpred = c(10, 4, 3, -8), lineespn = c(8, 3, 5, -6),
    lineopen = c(7, -3, 4, -7), linemidweek = c(7, -3, 4, -7), line = c(7, -3, 4, -7),
    hscore = c(21, 27, 14, 10), vscore = c(14, 20, 10, 24), actual = c(7, 7, 4, -14),
    season = 2024L, source_row = 1:4, exact_duplicate = FALSE,
    tracker_home = c("A", "D", "A", "A"), tracker_away = c("B", "C", "C", "B"))
  list(raw = raw, games = games)
}

test_that("explicit aliases distinguish similarly named teams", {
  expect_equal(tracker_team(c("Ohio St.", "Miami (Fla.)", "Miami (Ohio)", "Southern Miss.", "USC", "Texas-San Antonio")),
    c("Ohio State", "Miami", "Miami (OH)", "Southern Miss", "Southern California", "UTSA"))
  expect_equal(tracker_team("Unknown Academy"), "Unknown Academy")
})

test_that("numeric parser preserves zero and rejects malformed ratings", {
  expect_equal(tracker_number(c("", ".", "0", "-4.5")), c(NA, NA, 0, -4.5))
  expect_error(tracker_number("NaN!"), "nonnumeric")
  expect_error(tracker_number("Inf"), "nonnumeric")
})

test_that("neutral reversals flip all margin signs without adding HFA", {
  f <- tracker_fixture(); x <- tracker_join(f$raw, f$games)
  expect_equal(nrow(x$data), 4L)
  expect_equal(x$data$challenger_tracker_sagarin, c(10, -4, 3, -8))
  expect_equal(x$data$tracker_opening_margin, c(7, 3, 4, -7))
  expect_equal(x$audit$orientation, c(1, -1, 1, 1))
})

test_that("Monday championship rematches resolve by calendar without scores", {
  f <- tracker_fixture(); x <- tracker_join(f$raw, f$games)
  expect_equal(x$audit$game_id[4], "4")
  f$raw$hscore[4] <- 99
  y <- tracker_join(f$raw, f$games)
  expect_equal(y$audit$game_id[4], "4")
  expect_equal(y$audit$reason[4], "score_mismatch_after_identity_join")
  expect_false("4" %in% y$data$game_id)
  f$raw$week[4] <- 9
  expect_equal(tracker_join(f$raw, f$games)$audit$reason[4], "ambiguous_repeated_matchup")
})

test_that("nonneutral reversals and wrong scores are quarantined", {
  f <- tracker_fixture(); f$games$neutral_site[2] <- FALSE
  x <- tracker_join(f$raw, f$games)
  expect_equal(x$audit$reason[2], "nonneutral_home_away_reversal")
  expect_equal(nrow(x$data), 3L)
})

test_that("duplicate forecasts merge but conflicting quotes remain unknown", {
  f <- tracker_fixture(); d <- f$raw[3, ]; d$source_row <- 5; d$linemidweek <- 8
  f$raw <- rbind(f$raw, d)
  x <- tracker_join(f$raw, f$games)
  expect_equal(nrow(x$data), 4L)
  expect_true(is.na(x$data$tracker_midweek_margin[x$data$game_id == "3"]))
  expect_equal(x$data$challenger_tracker_sagarin[x$data$game_id == "3"], 3)
  expect_equal(x$audit$reason[5], "identical_forecast_duplicate_merged")
  f$raw$linesagpred[5] <- 10
  expect_error(tracker_join(f$raw, f$games), "Conflicting tracker predictions")
})

test_that("exact duplicates and unknown teams cannot multiply games", {
  f <- tracker_fixture(); d <- f$raw[1, ]; d$source_row <- 5; d$exact_duplicate <- TRUE
  f$raw <- rbind(f$raw, d)
  expect_equal(nrow(tracker_join(f$raw, f$games)$data), 4L)
  f$raw$tracker_home[2] <- "Unknown Academy"
  expect_equal(tracker_join(f$raw, f$games)$audit$reason[2], "unmatched_teams")
})

test_that("shared forecasts retain frozen values and reject missing learned rows", {
  f <- tracker_fixture(); joined <- tracker_join(f$raw, f$games)$data
  reference <- f$games; reference$actual_margin <- reference$margin; reference$expected_margin <- c(8, -3, 4, -7)
  x <- tracker_forecasts(reference, joined, list())$predictions
  expect_equal(subset(x, model == "v3_frozen")$expected_margin, reference$expected_margin)
  expect_equal(subset(x, model == "external_equal")$expected_margin, c(9, -3.5, 4, -7))
  expect_equal(subset(x, model == "v3_external_50_50")$expected_margin, c(8.5, -3.25, 4, -7))
  expect_false(any(subset(x, model == "market_reference")$selected))
  expect_error(tracker_forecasts(reference, joined, list(bad = list(predictions = reference[1:2, ]))), "Missing learned")
  joined$challenger_tracker_fpi[1] <- NA
  expect_equal(nrow(subset(tracker_forecasts(reference, joined, list())$predictions, model == "v3_frozen")), 3L)
})

test_that("grading reports pushes, zero edges, SU ties and missing lines honestly", {
  x <- data.frame(expected_margin = c(10, 7+1e-15, 0, -4), actual_margin = c(7, 10, -2, 10),
    closing_home_spread = c(-7, -7, 3, NA))
  p <- historical_sp_grade(x)
  expect_equal(p$push, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(p$selected, c(TRUE, FALSE, TRUE, FALSE))
  expect_true(is.na(p$su_correct[3]))
})

test_that("external feature names do not sneak into production selection", {
  x <- data.frame(power_rating_diff = 1, home_field_points = 2, home = "A", away = "B")
  for (name in tracker_features()) x[[name]] <- 99
  expect_equal(football_feature_names(x), c("power_rating_diff", "home_field_points"))
  expect_error(fit_cfb_ensemble(x, features = tracker_features()), "prohibited")
})

test_that("ridge rolling predictions cannot use future-season outcomes", {
  set.seed(31)
  d <- data.frame(season = rep(2020:2024, each = 35), week = rep(1:7, 25),
    challenger_tracker_sagarin = rnorm(175)*14, challenger_tracker_fpi = rnorm(175)*14)
  d$margin <- .7*d$challenger_tracker_sagarin+.3*d$challenger_tracker_fpi+rnorm(175)*10
  config <- cfb_v2_config(getwd(), 2026L)
  x <- tracker_validate(d, features = tracker_features(), weights = rep(1, nrow(d)), config = config)
  d$margin[d$season == 2024] <- 1000
  y <- tracker_validate(d, features = tracker_features(), weights = rep(1, nrow(d)), config = config)
  expect_equal(x$predictions$expected_margin, y$predictions$expected_margin)
  expect_equal(x$predictions$lambda, y$predictions$lambda)
})

test_that("final prediction helper rejects training seasons and absent inputs", {
  object <- list(trained_through = 2025, external_features = tracker_features())
  d <- data.frame(season = 2025, week = 1, coach_rating_65_35_diff = 0, coach_rating_70_30_diff = 0)
  expect_error(predict_tracker_challenger(object, d), "training seasons")
  d$season <- 2026
  expect_error(predict_tracker_challenger(object, d), "missing|required|Missing")
  for (name in tracker_features()) d[[name]] <- NA_real_
  expect_error(predict_tracker_challenger(object, d), "finite external")
})

test_that("external validator matches the existing ridge math without weakening guards", {
  set.seed(61)
  d <- data.frame(season = rep(2020:2024, each = 40), week = rep(1:10, 20),
    home_field_points = rep(c(0, 2.4), 100), power_rating_diff = rnorm(200)*20)
  d$margin <- d$power_rating_diff+d$home_field_points+rnorm(200)*10
  config <- cfb_v2_config(getwd(), 2026L); features <- football_feature_names(d)
  a <- rolling_validate_ensemble(d, features = features, weights = rep(1,200), config = config, fit_nonlinear = FALSE)
  b <- tracker_validate(d, features = features, weights = rep(1,200), config = config)
  expect_equal(a$predictions$expected_margin, b$predictions$expected_margin, tolerance = 1e-10)
  expect_equal(a$predictions$lambda, b$predictions$lambda)
  expect_true(any(abs(b$predictions$expected_margin) > 21))
  d$market_margin <- 1
  expect_error(tracker_validate(d, "market_margin", rep(1,200), config), "Unexpected")
})

test_that("metric denominators and late preseason fade remain intact", {
  f <- tracker_fixture(); joined <- tracker_join(f$raw, f$games)$data
  reference <- f$games; reference$actual_margin <- reference$margin; reference$expected_margin <- c(8,-3,4,-7)
  p <- tracker_forecasts(reference, joined, list())$predictions
  m <- tracker_metrics(p)
  x <- subset(m, model == "v3_frozen" & slice == "common_lined")
  expect_equal(x$games, 4L)
  expect_equal(x$wins+x$losses+x$pushes+x$no_selection, 4L)
  expect_true(all(is.na(subset(m, model == "market_reference")$ats_accuracy)))
  config <- cfb_v2_config(getwd(), 2026L)
  d <- data.frame(week = c(1,4,5,1), postseason_type = c(rep("regular",3),"cfp"))
  w <- preseason_blend_share_for_data(d, config)
  expect_equal(w[c(3,4)], c(0,0))
})

test_that("Sagarin parser separates Predictor HFA, transition teams and numeric ratings", {
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path))
  writeLines(c("<html><body><pre>", "COLLEGE FOOTBALL 2022 through results of 2022 OCTOBER 2",
    "RATING | PREDICTOR | GOLDEN_MEAN",
    "HOME ADVANTAGE=[ 2.42] [ 2.07] [ 2.93]",
    "1 Ohio State A = 92.50 5 0 | 1 2 3 | 93.96 1 | 90.00 1",
    "35 James Madison a = 72.00 4 0 | 1 2 3 | 73.51 35 | 72.00 35",
    "110 LouisianaMonroe(ULM) A = 52.00 2 3 | 1 2 3 | 52.57 110 | 51.00 110",
    "</pre></body></html>"), path)
  p <- tracker_sagarin_page(path, "20221007231301")
  expect_equal(p$home_advantage, 2.07)
  expect_equal(p$ratings$team, c("Ohio State", "James Madison", "UL Monroe"))
  expect_equal(p$ratings$predictor, c(93.96, 73.51, 52.57))
  expect_equal(p$ratings$wins, c(5, 4, 2))
  expect_equal(p$ratings$losses, c(0, 0, 3))
  expect_error(tracker_sagarin_page(path, "20221001231301"), "chronology")
})

test_that("retained original Sagarin tables pass the parser integration check", {
  root <- "cfb_v3/output/experiments/external_history_sources/discovery_20260911T235344Z"
  skip_if_not(all(file.exists(file.path(root, c("sagarin_20221007231301.html", "sagarin_20251103171011.html")))),
    "Optional retained historical source cache is unavailable")
  p <- tracker_sagarin_page(file.path(root,"sagarin_20221007231301.html"),"20221007231301")
  expect_equal(p$home_advantage, 2.07)
  expect_equal(subset(p$ratings, team == "Ohio State")$predictor, 93.96)
  expect_equal(subset(p$ratings, team == "James Madison")$predictor, 73.51)
  expect_equal(subset(p$ratings, team == "UL Monroe")$predictor, 52.57)
  p <- tracker_sagarin_page(file.path(root,"sagarin_20251103171011.html"),"20251103171011")
  expect_equal(p$home_advantage, 3.76)
  expect_error(tracker_sagarin_page(file.path(root,"sagarin_20251103171011.html"),"20251031171011"), "chronology")
})

test_that("football augmentation is past-only and its saved model obeys the phase contract", {
  set.seed(111)
  d <- data.frame(game_id = as.character(1:200), season = rep(2020:2024, each = 40),
    week = rep(1:10, 20), model_week = rep(1:10,20), home = "A", away = "B",
    home_coach_id = "coach_a", away_coach_id = "coach_b", postseason_type = "regular",
    home_field_points = 2.4, power_rating_diff = rnorm(200)*14,
    coach_rating_65_35_diff = rnorm(200), coach_rating_70_30_diff = rnorm(200),
    challenger_tracker_sagarin = rnorm(200)*12, challenger_tracker_fpi = rnorm(200)*12)
  d$game_phase <- game_phase(d$week)
  d$margin <- .5*d$power_rating_diff+.4*d$challenger_tracker_sagarin+rnorm(200)*10
  config <- cfb_v2_config(getwd(), 2026L)
  a <- tracker_fit_football(d, rep(1,200), config, 2020:2024, TRUE)
  d$margin[d$season == 2024] <- -999
  b <- tracker_fit_football(d, rep(1,200), config, 2020:2024, TRUE)
  expect_equal(a$predictions$expected_margin, b$predictions$expected_margin, tolerance = 1e-10)
  expect_equal(a$choices, b$choices)
  new <- d[1:10, ]; new$season <- 2026
  expected <- predict_tracker_challenger(a$final, new)
  expect_true(all(is.finite(expected)))
  explicit <- new
  explicit$coach_rating_diff <- explicit[[if(a$final$coach_share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
  expect_equal(expected[5:10], predict(a$final$foundation, explicit[5:10, ]))
  expect_error(predict_tracker_challenger(a$final, new[setdiff(names(new),"power_rating_diff")]), "missing")
})

test_that("available-source comparisons do not hide games missing the other rating", {
  f <- tracker_fixture(); joined <- tracker_join(f$raw, f$games)$data
  reference <- f$games; reference$actual_margin <- reference$margin; reference$expected_margin <- c(8,-3,4,-7)
  joined$challenger_tracker_fpi[4] <- NA
  p <- tracker_pairwise_available(reference, joined)
  expect_equal(sum(p$model == "sagarin_predictor"), 4L)
  expect_equal(sum(p$model == "espn_fpi"), 3L)
  expect_equal(sum(p$lane == "available_sagarin_predictor" & p$is_cfp), 2L)
})

test_that("line sensitivity changes only the grading quote, not model forecasts", {
  f <- tracker_fixture(); joined <- tracker_join(f$raw, f$games)$data
  reference <- f$games; reference$actual_margin <- reference$margin; reference$expected_margin <- c(8,-3,4,-7)
  p <- tracker_forecasts(reference, joined, list())$predictions
  m <- tracker_line_sensitivity(p)
  expect_equal(length(unique(m$lane)), 3L)
  expect_true(all(subset(m,model=="market_reference")$wins == 0))
  base <- subset(tracker_metrics(p), model == "sagarin_predictor" & slice == "all_fbs")$margin_mae
  expect_equal(subset(m, model == "sagarin_predictor" & slice == "all_fbs")$margin_mae, rep(base,3))
})
