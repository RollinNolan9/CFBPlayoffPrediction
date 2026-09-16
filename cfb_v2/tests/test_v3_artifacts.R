library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R", "dashboard.R")) source(file.path("cfb_v2", file))
config <- cfb_v2_config(getwd(), 2026)

test_that("rebuilt foundation has unique keys and the declared feature version", {
  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  snapshots <- read_csv_if_present(file.path(config$data_dir, "historical_team_snapshots.csv"), TRUE)
  membership <- read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE)
  expect_silent(assert_unique_keys(training, "game_id", "training"))
  expect_silent(assert_unique_keys(snapshots, c("game_id", "team"), "snapshots"))
  expect_silent(assert_unique_keys(membership, c("season", "team"), "membership"))
  expect_true(all(training$feature_version == config$version))
  expect_false(any(grepl("<U\\+", membership$team)))
  expect_true(all(snapshots$source_games <= snapshots$games_played))
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  power <- power_rating_for_week(games, 2025L, 9L, 8L, config)
  expect_lte(max(power$historical_weight_share), .1 + 1e-12)
})

test_that("the saved Week 2 model reproduces its card and ledger", {
  path <- find_latest_prediction_csv(file.path(config$output_dir, "2026", "week_2"))
  card <- read_csv_if_present(path, TRUE)
  features <- read_csv_if_present(sub("\\.csv$", "_features.csv", path), TRUE)
  artifact <- readRDS(sub("\\.csv$", "_model.rds", path))
  features <- features[match(card$game_id, features$game_id), ]
  expect_true(all(card$model_version == config$version))
  expect_true(all(artifact$model$features %in% names(features)))
  expect_false(any(grepl("^(home|away)$|fpi|pff|market|spread", artifact$model$features)))
  clean <- card$injury_scenario == "most_likely"
  replay <- predict(artifact$model, features)$expected_margin
  expect_equal(replay[clean], card$expected_margin[clean], tolerance = 1e-10)
  expect_true(all(is.finite(card$expected_margin)))
  expect_true(all(is.finite(card$market_home_spread)))
  expect_true(all(nzchar(card$ats_pick[is.finite(card$market_home_spread)])))
  expect_true(all(is.finite(features$success_rate_diff)))
  con <- v2_connect(config, read_only = TRUE)
  on.exit(v2_disconnect(con), add = TRUE)
  run <- sub("^(live|article)_", "", tools::file_path_sans_ext(basename(path)))
  stored <- DBI::dbGetQuery(con,
    "SELECT game_id, expected_margin FROM prediction_snapshots WHERE run_id = ?",
    params = list(run))
  expect_equal(nrow(stored), nrow(card))
  expect_equal(stored$expected_margin[match(card$game_id, stored$game_id)], card$expected_margin)
  rates <- DBI::dbGetQuery(con,
    "SELECT team, success_rate, source_games FROM team_week_features WHERE run_id = ?",
    params = list(run))
  ndsu <- rates[rates$team == "North Dakota State", ]
  expect_equal(ndsu$source_games, 1L)
  expect_gt(ndsu$success_rate, .3)
  coaches <- DBI::dbGetQuery(con,
    "SELECT portability, current_season_value FROM coach_ratings WHERE run_id = ?",
    params = list(run))
  expect_true(any(abs(coaches$current_season_value) > 0))
  expect_true(all(coaches$portability >= .2 & coaches$portability <= config$coach$portability_cap))
})
