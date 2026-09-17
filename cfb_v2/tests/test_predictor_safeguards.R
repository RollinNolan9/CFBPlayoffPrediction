library(testthat)

project_dir <- if (file.exists(file.path(getwd(), "cfb_v2", "config.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
}
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R")) {
  source(file.path(project_dir, "cfb_v2", file))
}
config <- cfb_v2_config(project_dir, 2026)

safeguard_training_rows <- function(n = 240, seed = 12) {
  set.seed(seed)
  training <- data.frame(
    game_id = paste0("t", seq_len(n)),
    season = rep(2021:2025, length.out = n),
    week = rep(1:12, length.out = n),
    game_phase = "in_season",
    home = "Home", away = "Away",
    home_level = "fbs", away_level = "fbs",
    postseason_type = "regular", is_cfp = FALSE,
    conference_championship = FALSE, neutral_site = FALSE
  )
  training$offense_rating_diff <- rnorm(n)
  training$defense_rating_diff <- rnorm(n)
  training$special_teams_rating_diff <- rnorm(n, 0, .2)
  training$recent_3_diff <- rnorm(n)
  training$recent_6_diff <- rnorm(n)
  training$season_to_date_diff <- rnorm(n)
  training$prior_season_diff <- rnorm(n)
  training$trailing_3yr_diff <- rnorm(n)
  training$preseason_prior_diff <- 0
  training$qb_continuity_diff <- 0
  training$roster_continuity_diff <- 0
  training$staff_continuity_diff <- 0
  training$games_played_diff <- rnorm(n, 0, 1)
  training$source_games_diff <- rnorm(n, 0, 1)
  training$coach_rating_65_35_diff <- rnorm(n, 0, .3)
  training$coach_rating_70_30_diff <- training$coach_rating_65_35_diff + rnorm(n, 0, .05)
  training$coach_rating_diff <- training$coach_rating_65_35_diff
  training$home_field_points <- 2.4
  training$margin <- 6 * training$offense_rating_diff +
    4 * training$defense_rating_diff + training$home_field_points + rnorm(n, 0, 8)
  training$total_points <- 52 + rnorm(n, 0, 8)
  training$closing_home_spread <- -training$margin + rnorm(n, 0, 4)
  training
}

write_supplied_week_inbox <- function(cfg, kickoff = "2026-09-05 23:30:00",
                                      team_features = NULL) {
  write_input <- function(name, data) {
    utils::write.csv(data, file.path(cfg$inbox_dir, paste0(name, ".csv")),
                     row.names = FALSE, na = "")
  }
  write_input("schedule", data.frame(
    game_id = "g1", season = 2026, week = 1, kickoff = kickoff,
    home = "Notre Dame", away = "Miami", neutral_site = FALSE,
    venue = "Notre Dame Stadium", home_level = "fbs", away_level = "fbs",
    postseason_type = "regular", conference_championship = FALSE, is_cfp = FALSE
  ))
  write_input("lines", data.frame(
    game_id = "g1", provider = c("DraftKings", "FanDuel"),
    captured_at = c("2026-09-04 16:45:00", "2026-09-04 16:50:00"),
    home_spread = c(-2.5, -3), total = c(51, 51.5),
    home_price = -110L, away_price = -110L, source_url = "test"
  ))
  write_input("membership", data.frame(
    team = c("Notre Dame", "Miami"), season = 2026, subdivision = "fbs",
    power_conference = c(FALSE, TRUE), conference = c("Independent", "ACC")
  ))
  write_input("coach_assignments", data.frame(
    team = c("Notre Dame", "Miami"), season = 2026, start_week = 0,
    end_week = 20, coach_id = c("nd", "mia"), coach_name = c("ND", "Miami"),
    interim = FALSE, source = "test"
  ))
  coach_history <- expand.grid(coach_id = c("nd", "mia"), season = 2021:2025,
                               stringsAsFactors = FALSE)
  coach_history$week <- 99L
  coach_history$games <- 13L
  coach_history$wins <- ifelse(coach_history$coach_id == "nd", 10, 8)
  coach_history$above_expectation <- ifelse(coach_history$coach_id == "nd", 2, .5)
  coach_history$level <- "fbs"
  coach_history$context_strength <- .9
  coach_history$target_context_strength <- .9
  coach_history$playoff_appearances <- 0L
  coach_history$titles <- 0L
  write_input("coach_history", coach_history)
  if (is.null(team_features)) {
    team_features <- data.frame(
      team = c("Notre Dame", "Miami"), season = 2026, week = 1,
      offense_rating = c(.25, .15), defense_rating = c(.20, .12),
      special_teams_rating = c(.02, 0), recent_3 = NA_real_, recent_6 = NA_real_,
      season_to_date = NA_real_, prior_season = c(4, 2), trailing_3yr = c(3, 2),
      preseason_prior = c(4, 2.5), qb_continuity = c(1, .5),
      roster_continuity = c(.7, .6), staff_continuity = c(1, 1),
      coach_rating = 0, home_field_rating = c(3, 2.4), source_games = 0L,
      games_played = 0L
    )
  }
  write_input("team_week_features", team_features)
  write_input("training_games", safeguard_training_rows())
  priors <- data.frame(
    team = c("Notre Dame", "Miami"), season = 2026,
    returning_ppa_pct = c(.7, .5), returning_passing_ppa_pct = c(.8, .4),
    returning_usage_pct = c(.65, .55), talent_percentile = c(.9, .8),
    preseason_poll_vote_share = c(.8, .6), retained_quality = c(.63, .4),
    replacement_capacity = c(.27, .4),
    portal_replacement_capacity = c(.2, .3),
    portal_offense_replacement = c(.35, .1), hype_gap = c(.1, -.05),
    source = "test", captured_at = "2026-07-01 12:00:00"
  )
  utils::write.csv(priors, file.path(cfg$data_dir, "preseason_team_priors.csv"),
                   row.names = FALSE, na = "")
  invisible(cfg)
}

new_week_config <- function() {
  temp <- tempfile("cfb_safeguard_")
  cfg <- cfb_v2_config(temp, 2026)
  cfg$model$minimum_training_rows <- 10000L
  cfg$model$ridge_lambda_grid <- c(2, 8)
  initialize_v2_project(cfg)
  cfg
}

# A. Predictor admission -------------------------------------------------------

test_that("unknown numeric _diff columns are not auto-admitted as predictors", {
  data <- data.frame(
    power_rating_diff = 1, home_field_points = 2.4,
    postgame_epa_diff = 9, final_margin_diff = 14, closing_epa_diff = 3,
    efficiency_diff = 2, mystery_leak_diff = 8,
    margin = 7
  )
  selected <- football_feature_names(data)
  expect_true("power_rating_diff" %in% selected)
  expect_true("home_field_points" %in% selected)
  expect_false("postgame_epa_diff" %in% selected)
  expect_false("final_margin_diff" %in% selected)
  expect_false("closing_epa_diff" %in% selected)
  expect_false("efficiency_diff" %in% selected)
  expect_false("mystery_leak_diff" %in% selected)
})

test_that("an explicitly requested prohibited predictor produces a clear error", {
  data <- safeguard_training_rows(n = 80)
  data$postgame_epa_diff <- rnorm(nrow(data))
  cfg <- config
  cfg$model$minimum_training_rows <- 10000L
  expect_error(
    fit_cfb_ensemble(data, features = c("offense_rating_diff", "postgame_epa_diff"),
                     config = cfg, fit_nonlinear = FALSE),
    "prohibited|postgame_epa_diff"
  )
  expect_error(
    fit_cfb_ensemble(data, features = "closing_epa_diff",
                     config = cfg, fit_nonlinear = FALSE),
    "prohibited|closing_epa_diff"
  )
  expect_error(
    fit_cfb_ensemble(data, features = c("coach_rating_diff", "coach_rating_65_35_diff"),
                     config = cfg, fit_nonlinear = FALSE),
    "prohibited|coach_rating_65_35_diff"
  )
})

test_that("games_played_diff and source_games_diff remain eligible production predictors", {
  data <- data.frame(
    power_rating_diff = 1, games_played_diff = 2, source_games_diff = -1,
    home_field_points = 2.4, margin = 7
  )
  selected <- football_feature_names(data)
  expect_true("games_played_diff" %in% selected)
  expect_true("source_games_diff" %in% selected)
  expect_true("power_rating_diff" %in% selected)
})

# B. Legitimate targets and metadata ------------------------------------------

test_that("historical outcomes and market benchmarks are not treated as predictors", {
  data <- safeguard_training_rows()
  data$postgame_epa_diff <- rnorm(nrow(data))
  data$home_score <- 28
  data$away_score <- 21
  selected <- football_feature_names(data)
  expect_false("closing_home_spread" %in% selected)
  expect_false("margin" %in% selected)
  expect_false("total_points" %in% selected)
  expect_false("home_score" %in% selected)
  expect_false("away_score" %in% selected)
  expect_false("postgame_epa_diff" %in% selected)
  expect_true("offense_rating_diff" %in% selected)
  expect_true("games_played_diff" %in% selected)

  cfg <- config
  cfg$model$minimum_training_rows <- 10000L
  model <- fit_cfb_ensemble(
    data, features = c("offense_rating_diff", "home_field_points"),
    config = cfg, fit_nonlinear = FALSE
  )
  expect_equal(model$features, c("offense_rating_diff", "home_field_points"))
})

test_that("future-looking name checks are not applied to whole training tables", {
  data <- safeguard_training_rows()
  cfg <- config
  cfg$model$minimum_training_rows <- 10000L
  expect_silent(
    fit_cfb_ensemble(
      data, features = football_feature_names(data),
      config = cfg, fit_nonlinear = FALSE
    )
  )
})

# C. Coach fallback ------------------------------------------------------------

test_that("production selection uses coach_rating_diff and not diagnostic variants", {
  data <- safeguard_training_rows()
  selected <- football_feature_names(data)
  expect_true("coach_rating_diff" %in% selected)
  expect_false("coach_rating_65_35_diff" %in% selected)
  expect_false("coach_rating_70_30_diff" %in% selected)

  cfg <- config
  cfg$model$ridge_lambda_grid <- c(2, 8)
  both_variants <- select_coach_split_validation(
    data, rep(1, nrow(data)), cfg, fit_nonlinear = FALSE
  )
  expect_true("coach_rating_diff" %in% both_variants$features)
  expect_false(any(c("coach_rating_65_35_diff", "coach_rating_70_30_diff") %in%
                     both_variants$features))
  expect_true(all(c("coach_rating_65_35_diff", "coach_rating_70_30_diff") %in%
                    names(both_variants$data)))

  fallback_data <- data
  fallback_data$coach_rating_70_30_diff <- NULL
  fallback <- select_coach_split_validation(
    fallback_data, rep(1, nrow(fallback_data)), cfg, fit_nonlinear = FALSE
  )
  expect_true("coach_rating_diff" %in% fallback$features)
  expect_false("coach_rating_65_35_diff" %in% fallback$features)
  expect_true("coach_rating_65_35_diff" %in% names(fallback$data))
})

# D. Prediction-time boundaries on the supplied CSV route ----------------------

test_that("valid pregame user-supplied team_week_features pass through run_v2_week", {
  cfg <- new_week_config()
  write_supplied_week_inbox(cfg, kickoff = "2026-09-05 23:30:00")
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  result <- run_v2_week(cfg, 2026, 1, "live", as_of, strict = TRUE)
  expect_equal(nrow(result$predictions), 1)
  expect_true(is.finite(result$predictions$expected_margin))
  expect_false(any(c("coach_rating_65_35_diff", "coach_rating_70_30_diff") %in%
                     result$predictions$top_drivers))
})

test_that("already-started supplied games are rejected before prediction", {
  cfg <- new_week_config()
  write_supplied_week_inbox(cfg, kickoff = "2026-09-04 16:00:00")
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  expect_error(
    run_v2_week(cfg, 2026, 1, "live", as_of, strict = TRUE),
    "kickoff|already-started|as_of|prediction_as_of"
  )
})

test_that("feature information after the prediction cutoff is rejected", {
  cfg <- new_week_config()
  features <- data.frame(
    team = c("Notre Dame", "Miami"), season = 2026, week = 1,
    offense_rating = c(.25, .15), defense_rating = c(.20, .12),
    special_teams_rating = c(.02, 0), recent_3 = NA_real_, recent_6 = NA_real_,
    season_to_date = NA_real_, prior_season = c(4, 2), trailing_3yr = c(3, 2),
    preseason_prior = c(4, 2.5), qb_continuity = c(1, .5),
    roster_continuity = c(.7, .6), staff_continuity = c(1, 1),
    coach_rating = 0, home_field_rating = c(3, 2.4), source_games = 0L,
    games_played = 0L,
    information_available_at = "2026-09-04 18:00:00"
  )
  write_supplied_week_inbox(cfg, kickoff = "2026-09-05 23:30:00",
                            team_features = features)
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  expect_error(
    run_v2_week(cfg, 2026, 1, "live", as_of, strict = TRUE),
    "information_available_at must be"
  )
})

test_that("missing kickoff on supplied schedule is an explicit failure", {
  cfg <- new_week_config()
  write_supplied_week_inbox(cfg, kickoff = NA_character_)
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  expect_error(
    run_v2_week(cfg, 2026, 1, "live", as_of, strict = TRUE),
    "kickoff"
  )
})

# Existing-database compatibility ------------------------------------------------

pre_patch_team_week_features_sql <- paste(
  "CREATE TABLE team_week_features (",
  "team VARCHAR, season INTEGER, week INTEGER, as_of TIMESTAMP,",
  "offense_rating DOUBLE, defense_rating DOUBLE, special_teams_rating DOUBLE,",
  "recent_3 DOUBLE, recent_6 DOUBLE, season_to_date DOUBLE,",
  "prior_season DOUBLE, trailing_3yr DOUBLE, preseason_prior DOUBLE,",
  "qb_continuity DOUBLE, roster_continuity DOUBLE, staff_continuity DOUBLE,",
  "coach_rating DOUBLE, home_field_rating DOUBLE, source_games INTEGER,",
  "run_id VARCHAR, games_played INTEGER",
  ")"
)

legacy_team_week_row <- function(run_id, team = "Notre Dame") {
  data.frame(
    team = team, season = 2026L, week = 1L,
    as_of = as.POSIXct("2026-09-04 17:00:00", tz = "UTC"),
    offense_rating = 0.25, defense_rating = 0.20, special_teams_rating = 0.02,
    recent_3 = NA_real_, recent_6 = NA_real_, season_to_date = NA_real_,
    prior_season = 4, trailing_3yr = 3, preseason_prior = 4,
    qb_continuity = 1, roster_continuity = 0.7, staff_continuity = 1,
    coach_rating = 0, home_field_rating = 3, source_games = 0L,
    run_id = run_id, games_played = 0L, stringsAsFactors = FALSE
  )
}

test_that("pre-patch DuckDB databases keep existing rows after optional column migration", {
  cfg <- cfb_v2_config(tempfile("cfb_compat_"), 2026)
  ensure_v2_directories(cfg)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = cfg$database)
  DBI::dbExecute(con, pre_patch_team_week_features_sql)
  DBI::dbAppendTable(con, "team_week_features", legacy_team_week_row("legacy_run"))
  expect_false("information_available_at" %in% DBI::dbListFields(con, "team_week_features"))
  DBI::dbDisconnect(con, shutdown = TRUE)

  con <- v2_connect(cfg)
  on.exit(v2_disconnect(con), add = TRUE)
  v2_init_schema(con)
  expect_true("information_available_at" %in% DBI::dbListFields(con, "team_week_features"))
  preserved <- DBI::dbReadTable(con, "team_week_features")
  expect_equal(preserved$team, "Notre Dame")
  expect_equal(preserved$offense_rating, 0.25)
  expect_equal(preserved$run_id, "legacy_run")
  expect_true(all(is.na(preserved$information_available_at)))

  without_field <- legacy_team_week_row("write_without")
  without_field$team <- "Miami"
  without_field$offense_rating <- 0.15
  v2_append_snapshot(con, "team_week_features", without_field, "write_without")

  with_field <- legacy_team_week_row("write_with")
  with_field$team <- "Ohio State"
  with_field$information_available_at <- as.POSIXct("2026-09-04 16:00:00", tz = "UTC")
  v2_append_snapshot(con, "team_week_features", with_field, "write_with")

  v2_init_schema(con)
  v2_init_schema(con)
  rows <- DBI::dbReadTable(con, "team_week_features")
  expect_equal(sort(rows$team), c("Miami", "Notre Dame", "Ohio State"))
  legacy <- rows[rows$run_id == "legacy_run", ]
  expect_equal(legacy$offense_rating, 0.25)
  expect_true(is.na(legacy$information_available_at))
  expect_true(is.na(rows$information_available_at[rows$run_id == "write_without"]))
  written <- rows$information_available_at[rows$run_id == "write_with"]
  expect_equal(as.POSIXct(written, tz = "UTC"),
               as.POSIXct("2026-09-04 16:00:00", tz = "UTC"))
})

# Production predictor contract --------------------------------------------------

# Committed training_games.csv currently stores EPA/power/havoc diffs, not
# offense_rating_diff/defense_rating_diff. The synthetic live fixture uses the
# rating columns instead. Both are subsets of the same allowlist; preseason
# ps_* columns are attached at runtime from config$preseason$production_features
# (8 names) and are not stored in training_games.csv. The earlier "20 + 8"
# count was the committed matchup diffs excluding games_played_diff,
# source_games_diff, and home_field_points; this test records the exact lists.
committed_foundation_predictors <- c(
  "power_rating_diff", "offense_epa_diff", "defense_epa_diff",
  "special_teams_rating_diff", "pass_epa_diff", "rush_epa_diff",
  "success_rate_diff", "havoc_allowed_diff", "havoc_generated_diff",
  "turnover_rate_regressed_diff", "recent_3_diff", "recent_6_diff",
  "season_to_date_diff", "prior_season_diff", "trailing_3yr_diff",
  "preseason_prior_diff", "qb_continuity_diff", "roster_continuity_diff",
  "staff_continuity_diff", "games_played_diff", "source_games_diff",
  "coach_rating_diff", "home_field_points"
)

synthetic_foundation_predictors <- c(
  "offense_rating_diff", "defense_rating_diff", "special_teams_rating_diff",
  "recent_3_diff", "recent_6_diff", "season_to_date_diff", "prior_season_diff",
  "trailing_3yr_diff", "preseason_prior_diff", "qb_continuity_diff",
  "roster_continuity_diff", "staff_continuity_diff", "games_played_diff",
  "source_games_diff", "coach_rating_diff", "home_field_points"
)

test_that("committed training table selects the production foundation subset, not the synthetic fixture", {
  training <- utils::read.csv(
    file.path(project_dir, "cfb_v2", "inbox", "training_games.csv"),
    stringsAsFactors = FALSE, check.names = FALSE, nrows = 20
  )
  selected <- football_feature_names(training, config)
  expect_equal(sort(selected), sort(committed_foundation_predictors))
  expect_false("offense_rating_diff" %in% names(training))
  expect_false("defense_rating_diff" %in% names(training))
  expect_false(any(grepl("^ps_", names(training))))
  expect_false("efficiency_diff" %in% selected)
  expect_true(all(selected %in% approved_production_predictors(config)))
  expect_equal(
    sort(paste0("ps_", intended_preseason_production_features(config), "_diff")),
    sort(paste0("ps_", config$preseason$production_features, "_diff"))
  )
  expect_equal(length(setdiff(synthetic_foundation_predictors, selected)), 2L)
  expect_equal(
    sort(setdiff(synthetic_foundation_predictors, selected)),
    c("defense_rating_diff", "offense_rating_diff")
  )
})

test_that("production selection cannot admit research-only names; explicit features remain a test hatch", {
  data <- safeguard_training_rows()
  data$efficiency_diff <- rnorm(nrow(data))
  data$form_diff <- rnorm(nrow(data))
  selected <- football_feature_names(data, config)
  expect_equal(sort(selected), sort(synthetic_foundation_predictors))
  expect_false("efficiency_diff" %in% selected)
  expect_false("form_diff" %in% selected)

  cfg <- config
  cfg$model$ridge_lambda_grid <- c(2, 8)
  cfg$model$minimum_training_rows <- 10000L
  coach <- select_coach_split_validation(
    data, rep(1, nrow(data)), cfg, fit_nonlinear = FALSE
  )
  expect_false(any(c("efficiency_diff", "form_diff") %in% coach$features))
  expect_true(all(coach$features %in% approved_production_predictors(cfg)))
  expect_false(any(c("coach_rating_65_35_diff", "coach_rating_70_30_diff") %in%
                     coach$features))

  research <- fit_cfb_ensemble(
    data, features = "efficiency_diff", config = cfg, fit_nonlinear = FALSE
  )
  expect_equal(research$features, "efficiency_diff")
})

test_that("preseason cache tests do not require a cfbfastR package", {
  local_config <- config
  local_config$project_dir <- tempfile("cfb_preseason_no_cfbfastr_")
  withr::local_envvar(CFBD_API_KEY = "test-key")
  expect_error(
    pull_preseason_source(
      local_config, "empty_test", 2026, FALSE, function(year) data.frame()
    ),
    "empty response was not cached"
  )
})

# Timing edge cases --------------------------------------------------------------

cutoff_schedule <- function(kickoff) {
  data.frame(game_id = "g1", kickoff = kickoff, stringsAsFactors = FALSE)
}

test_that("as_of equal to kickoff is rejected and information_available_at equal to as_of is permitted", {
  kickoff <- as.POSIXct("2026-09-05 23:30:00", tz = "UTC")
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  expect_error(
    assert_prediction_cutoff(cutoff_schedule(kickoff), kickoff),
    "at or after kickoff"
  )
  features <- data.frame(information_available_at = as_of)
  expect_silent(assert_prediction_cutoff(cutoff_schedule(kickoff), as_of, features))

  cfg <- new_week_config()
  write_supplied_week_inbox(cfg, kickoff = "2026-09-04 17:00:00")
  expect_error(
    run_v2_week(cfg, 2026, 1, "live", as.POSIXct("2026-09-04 17:00:00", tz = "UTC"),
                strict = TRUE),
    "at or after kickoff"
  )

  cfg2 <- new_week_config()
  equal_features <- data.frame(
    team = c("Notre Dame", "Miami"), season = 2026, week = 1,
    offense_rating = c(.25, .15), defense_rating = c(.20, .12),
    special_teams_rating = c(.02, 0), recent_3 = NA_real_, recent_6 = NA_real_,
    season_to_date = NA_real_, prior_season = c(4, 2), trailing_3yr = c(3, 2),
    preseason_prior = c(4, 2.5), qb_continuity = c(1, .5),
    roster_continuity = c(.7, .6), staff_continuity = c(1, 1),
    coach_rating = 0, home_field_rating = c(3, 2.4), source_games = 0L,
    games_played = 0L,
    information_available_at = "2026-09-04 17:00:00"
  )
  write_supplied_week_inbox(cfg2, kickoff = "2026-09-05 23:30:00",
                            team_features = equal_features)
  result <- run_v2_week(cfg2, 2026, 1, "live",
                        as.POSIXct("2026-09-04 17:00:00", tz = "UTC"),
                        strict = TRUE)
  expect_equal(nrow(result$predictions), 1)
})

test_that("invalid cutoffs and malformed optional provenance fail without labeling unknown as verified", {
  kickoff <- as.POSIXct("2026-09-05 23:30:00", tz = "UTC")
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  schedule <- cutoff_schedule(kickoff)
  expect_error(assert_prediction_cutoff(schedule, NA), "as_of")
  expect_error(assert_prediction_cutoff(schedule, NULL), "as_of")
  expect_error(
    assert_prediction_cutoff(data.frame(game_id = "g1"), as_of),
    "missing kickoff"
  )
  expect_error(
    assert_prediction_cutoff(
      schedule, as_of, data.frame(information_available_at = "not-a-timestamp")
    ),
    "finite timestamp when supplied"
  )
  expect_error(
    assert_prediction_cutoff(
      schedule, as_of,
      data.frame(information_available_at = c("2026-09-04 16:00:00", NA))
    ),
    "finite timestamp when supplied"
  )
  unknown <- data.frame(team = "Notre Dame", offense_rating = 0.25)
  expect_silent(assert_prediction_cutoff(schedule, as_of, unknown))
  expect_false("provenance_verified" %in% names(unknown))
  expect_false("information_available_at" %in% names(unknown))
})
