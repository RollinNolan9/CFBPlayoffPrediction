library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R")) source(file.path("cfb_v2", file))
source("cfb_v3/experiments/controlled_ats.R")
source("cfb_v3/experiments/v1_comparison.R")

v1_fixture <- function() {
  games <- expand.grid(week = 1:12, pair = 1:2, year = 2019:2025)
  games$game_id <- as.character(seq_len(nrow(games)))
  games$home <- ifelse(games$pair == 1, "A", "C")
  games$away <- ifelse(games$pair == 1, "B", "D")
  games$start_date <- paste0(games$year, "-09-01T12:00:00Z")
  games$kickoff <- parse_utc_datetime(games$start_date) + 7*86400*(games$week-1)
  games$start_date <- format(games$kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  games$margin <- 2*games$pair-games$week
  games$home_team_pregame_elo <- 1500+games$week
  games$away_team_pregame_elo <- 1400+games$week
  official <- games[games$year >= 2020, ]
  official$season <- official$year
  official$home_pregame_elo <- official$home_team_pregame_elo
  official$away_pregame_elo <- official$away_team_pregame_elo
  stats <- do.call(rbind, lapply(c("home", "away"), function(side) {
    data.frame(game_id = games$game_id, year = games$year, week = games$week,
      pos_team = games[[side]], epa_play = games$week, epa_pass = games$week*2,
      epa_rush = games$week/2, wpa_play = games$week/100)
  }))
  sp <- expand.grid(team = c("A", "B", "C", "D"), year = 2018:2025, stringsAsFactors = FALSE)
  sp$rating <- sp$year-2000; sp$offense_rating <- 40; sp$defense_rating <- 20
  list(inputs = list(games = games, stats = stats, sp = sp), official = official)
}

test_that("v1 requires five strictly prior same-season games", {
  fixture <- v1_fixture()
  features <- v1_build_features(fixture$inputs, fixture$official)
  expect_false(any(features$eligible[features$week <= 5]))
  sixth <- features[features$week == 6, ]
  expect_true(all(sixth$eligible))
  expect_equal(sixth$home_epa_play, rep(3, nrow(sixth)))
  expect_true(all(features$sp_source_season < features$season))
  expect_true(all(features$home_latest_source[features$eligible] < as.numeric(features$cutoff[features$eligible])))
})

test_that("current-game and later data cannot change earlier v1 features", {
  fixture <- v1_fixture()
  before <- v1_build_features(fixture$inputs, fixture$official)
  future <- fixture$inputs$stats$year > 2022 |
    (fixture$inputs$stats$year == 2022 & fixture$inputs$stats$week >= 8)
  fixture$inputs$stats$epa_play[future] <- 9999
  fixture$inputs$stats$wpa_play[future] <- 9999
  fixture$inputs$sp$rating[fixture$inputs$sp$year >= 2022] <- 9999
  after <- v1_build_features(fixture$inputs, fixture$official)
  rows <- before$season == 2022 & before$week <= 8
  expect_equal(before[rows, v1_numeric_features()], after[rows, v1_numeric_features()])
})

test_that("v1 recipes retain identities without accepting unseen levels or outcomes", {
  fixture <- v1_fixture()
  features <- v1_build_features(fixture$inputs, fixture$official)
  train <- features[features$eligible & features$season <= 2021, ]
  test <- features[features$eligible & features$season == 2022, ]
  test$home[1] <- "New team"
  design <- v1_design(train, test)
  expect_false(design$known[1])
  expect_equal(sum(design$known), nrow(test)-1L)
  expect_true(any(grepl("^home", colnames(design$train))))
  expect_identical(colnames(design$train), colnames(design$test))
  expect_false(any(c("margin", "closing_home_spread", "season", "week") %in% colnames(design$train)))
})

test_that("rolling v1 fits never use their own test-season outcomes", {
  fixture <- v1_fixture()
  features <- v1_build_features(fixture$inputs, fixture$official)
  original <- features
  before <- suppressMessages(v1_rolling(features, 20260909L, ntree = 12L))
  expect_identical(features, original)
  features$margin[features$season == 2025] <- 10000
  after <- suppressMessages(v1_rolling(features, 20260909L, ntree = 12L))
  expect_equal(before$predictions, after$predictions)
  expect_identical(before$choices, after$choices)
  expect_true(all(before$choices$train_through_season < before$choices$test_season))
  expect_true(all(before$choices$mtry %in% c(2, 4, 6)))
  expect_equal(nrow(before$cv_scores), 12L)
  sensitivity <- suppressMessages(v1_rolling(original, 20260910L, ntree = 12L,
    fixed_choices = before$choices))
  expect_equal(sensitivity$choices$mtry, before$choices$mtry)
  expect_null(sensitivity$cv_scores)
})

test_that("only the selected notebook's James Madison exclusion is retained", {
  fixture <- v1_fixture()
  fixture$official$home[1:3] <- c("James Madison", "Jacksonville State", "Sam Houston")
  features <- v1_build_features(fixture$inputs, fixture$official)
  index <- match(fixture$official$game_id[1:3], features$game_id)
  expect_equal(features$omission_reason[index[1]], "original_team_exclusion")
  expect_false(any(features$omission_reason[index[2:3]] == "original_team_exclusion"))
})
