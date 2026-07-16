library(testthat)

project_dir <- if (file.exists(file.path(getwd(), "cfb_v2", "config.R"))) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = TRUE)
}
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "workflow.R")) {
  source(file.path(project_dir, "cfb_v2", file))
}
config <- cfb_v2_config(project_dir, 2026)

test_that("preseason inputs fade completely after week four", {
  expect_equal(preseason_feature_weight(0:6, config), c(1, 1, .6, .3, .1, 0, 0))
  expect_equal(game_phase(c(0, 1, 2, 4, 5)),
               c("preseason", "preseason", "early_season", "early_season", "in_season"))
})

test_that("prior-season influence is capped late in the season", {
  weight <- prior_season_feature_weight(c(1, 8, 12), c(0, 7, 11), config)
  expect_gt(weight[1], .10)
  expect_lte(weight[2], .10)
  expect_lte(weight[3], .10)
})

test_that("preseason-only matchup columns are zero after week four", {
  schedule <- data.frame(
    game_id = c("w1", "w5"), season = 2026, week = c(1, 5),
    home = "A", away = "B", neutral_site = FALSE
  )
  features <- do.call(rbind, lapply(c(1, 5), function(week) {
    data.frame(
      team = c("A", "B"), season = 2026, week = week,
      preseason_prior = c(10, 0), qb_continuity = c(1, 0),
      roster_continuity = c(1, 0), staff_continuity = c(1, 0),
      prior_season = c(8, 0), trailing_3yr = c(6, 0),
      games_played = c(week - 1, week - 1), home_field_rating = 3,
      source_games = week - 1
    )
  }))
  matchup <- make_matchup_features(schedule, features, features, .65, config)
  expect_equal(matchup$preseason_prior_diff, c(10, 0))
  expect_equal(matchup$qb_continuity_diff, c(1, 0))
  expect_lt(matchup$prior_season_diff[2], 8)
})

test_that("modern-era weights exclude old and non-CFP bowl games", {
  games <- data.frame(
    game_id = as.character(1:5), season = c(2019, 2020, 2021, 2022, 2022), week = 1,
    home = paste0("H", 1:5), away = paste0("A", 1:5), neutral_site = FALSE,
    home_level = c("fbs", "fbs", "fbs", "fbs", "fbs"),
    away_level = c("fbs", "fbs", "fbs", "fcs", "fbs"),
    postseason_type = c("regular", "regular", "bowl", "regular", "bowl"),
    conference_championship = FALSE, is_cfp = c(FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  weight <- training_game_weights(games, 2023, config)
  expect_equal(weight[1], 0)
  expect_equal(weight[2], config$training$covid_weight * config$training$season_decay^2)
  expect_equal(weight[3], 0)
  expect_lt(weight[4], weight[5])
  expect_equal(weight[5], 1)
  expect_false(ats_training_eligible(games)[4])
})

test_that("garbage time and clock-kill plays are removed", {
  plays <- data.frame(
    play_type = c("Rush", "Rush", "QB Kneel", "Pass"),
    play_text = c("normal", "normal", "kneels", "normal"),
    garbage_time = c(FALSE, TRUE, FALSE, FALSE),
    id = 1:4
  )
  expect_equal(filter_competitive_plays(plays)$id, c(1, 4))
})

test_that("coach intervals are week-aware and reject overlap", {
  assignments <- data.frame(
    team = c("Auburn", "Auburn"), season = 2026,
    start_week = c(0, 8), end_week = c(7, 20),
    coach_id = c("old", "new"), coach_name = c("Old", "New"),
    interim = c(FALSE, TRUE)
  )
  games <- data.frame(
    game_id = c("a", "b"), season = 2026, week = c(2, 9),
    home = "Auburn", away = c("Georgia", "Alabama"), neutral_site = FALSE
  )
  opponents <- data.frame(
    team = c("Georgia", "Alabama"), season = 2026, start_week = 0, end_week = 20,
    coach_id = c("uga", "bama"), coach_name = c("UGA", "Bama"), interim = FALSE
  )
  mapped <- map_coaches_as_of(games, rbind(assignments, opponents))
  expect_equal(mapped$home_coach_id, c("old", "new"))
  bad <- assignments
  bad$start_week[2] <- 7
  expect_error(validate_coach_assignments(bad), "overlap")
})

test_that("coach ratings use only history known before the requested week", {
  history <- data.frame(
    coach_id = c("c", "c", "c"), season = c(2023, 2025, 2026), week = c(99, 99, 3),
    games = c(13, 13, 3), wins = c(12, 11, 3),
    above_expectation = c(5, 6, 20), level = "fbs", context_strength = .8,
    target_context_strength = .9, playoff_appearances = c(1, 1, 0), titles = 0
  )
  before <- build_coach_ratings(history, 2026, 2, .65, config)
  after <- build_coach_ratings(history, 2026, 4, .65, config)
  expect_equal(before$current_season_value, 0)
  expect_gt(after$current_season_value, before$current_season_value)
  expect_lte(after$portability, config$coach$portability_cap)
})

test_that("coach identities normalize punctuated initials consistently", {
  expect_equal(coach_id_from_name("D.J. Durkin"), coach_id_from_name("DJ Durkin"))
  expect_equal(coach_id_from_name("J. C. Price"), coach_id_from_name("JC Price"))
  expect_equal(canonical_team("UMass"), "Massachusetts")
  expect_equal(canonical_team("San Jose State"), "San José State")
})

test_that("coach count mismatches preserve a transition and assign remainder to successor", {
  coaches <- data.frame(
    source_name = c("Old Coach", "New Coach"), team = "Auburn", season = 2026,
    games = c(3L, 1L), wins = c(1, 1), srs = c(0, 0),
    canonical_name = c("Old Coach", "New Coach"),
    coach_id = c("old", "new"), data_source = "test"
  )
  games <- data.frame(
    game_id = paste0("g", 1:5), season = 2026, week = 1:5,
    coach_lookup_week = 1:5,
    kickoff = as.POSIXct(paste0("2026-09-", sprintf("%02d", 1:5)), tz = "UTC"),
    home = "Auburn", away = paste0("Opponent ", 1:5),
    home_level = "fbs", away_level = "fbs"
  )
  inferred <- infer_coach_assignments(coaches, games)
  auburn <- inferred$assignments[inferred$assignments$team == "Auburn", ]
  expect_equal(auburn$coach_id, c("old", "new"))
  expect_equal(auburn$start_week, c(0L, 4L))
  expect_equal(auburn$end_week, c(3L, 99L))
  expect_true(all(auburn$needs_review))
})

test_that("a coach snapshot without game counts maps the only coach without a false error", {
  coaches <- data.frame(
    source_name = "Snapshot Coach", team = "Auburn", season = 2025,
    games = 0L, wins = 0, srs = 7, hire_date = as.Date("2024-01-01"),
    counts_available = FALSE, canonical_name = "Snapshot Coach",
    coach_id = "snapshot", data_source = "cfbd_coaches"
  )
  games <- data.frame(
    game_id = paste0("g", 1:3), season = 2025, week = 1:3,
    coach_lookup_week = 1:3,
    kickoff = as.POSIXct(paste0("2025-09-", c("01", "08", "15")), tz = "UTC"),
    home = "Auburn", away = paste0("Opponent ", 1:3),
    home_level = "fbs", away_level = "fcs"
  )
  inferred <- infer_coach_assignments(coaches, games)
  auburn <- inferred$assignments[inferred$assignments$team == "Auburn", ]
  expect_equal(nrow(auburn), 1L)
  expect_equal(auburn$start_week, 0L)
  expect_equal(auburn$end_week, 99L)
  expect_false(auburn$needs_review)
  expect_true(is.na(auburn$source_games))
  expect_equal(auburn$inference_reason,
               "single_cfbd_snapshot_without_game_counts")
  expect_false(any(inferred$qa$team == "Auburn" &
                     inferred$qa$issue == "coach_schedule_game_count_mismatch"))
})

test_that("a single known coach count mismatch is informational, not ambiguous", {
  coaches <- data.frame(
    source_name = "Only Coach", team = "New Mexico State", season = 2020,
    games = 2L, wins = 1, srs = -10, counts_available = TRUE,
    canonical_name = "Only Coach", coach_id = "only",
    data_source = "cfbd_coaches"
  )
  games <- data.frame(
    game_id = "g1", season = 2020, week = 3L, coach_lookup_week = 3L,
    kickoff = as.POSIXct("2021-02-21", tz = "UTC"),
    home = "New Mexico State", away = "Tarleton State",
    home_level = "fbs", away_level = "fcs"
  )
  inferred <- infer_coach_assignments(coaches, games)
  assignment <- inferred$assignments[inferred$assignments$team ==
                                       "New Mexico State", ]
  issue <- inferred$qa[inferred$qa$team == "New Mexico State", ]
  expect_false(assignment$needs_review)
  expect_equal(assignment$inference_reason,
               "single_source_coach_count_mismatch")
  expect_equal(issue$severity, "info")
})

test_that("historical foundation excludes canceled games but keeps scored games without PBP", {
  games <- data.frame(
    game_id = c("canceled", "scored"), completed = c(FALSE, TRUE),
    home_score = c(NA, 20), away_score = c(NA, 17),
    pbp_available = c(FALSE, FALSE)
  )
  kept <- filter_completed_historical_games(games)
  expect_equal(kept$game_id, "scored")
})

test_that("public transitions start next game and resolve stable coach identities", {
  games <- data.frame(
    game_id = paste0("g", 1:5), season = 2025, week = 1:5,
    coach_lookup_week = 1:5,
    kickoff = as.POSIXct(paste0("2025-09-", c("01", "08", "15", "22", "29")),
                         tz = "UTC"),
    home = "Connecticut", away = paste0("Opponent ", 1:5),
    home_level = "fbs", away_level = "fcs"
  )
  transitions <- data.frame(
    team = "Connecticut", season = 2025, outgoing_coach = "Jim L. Mora",
    effective_date = as.Date("2025-09-16"), replacement_coach = "New Coach",
    interim = TRUE, reason = "test", source_url = "test",
    stringsAsFactors = FALSE
  )
  coach_seasons <- data.frame(
    coach_id = "coach_jim_mora", canonical_name = "Jim Mora",
    source_name = "Jim Mora", team = "Connecticut", season = 2025,
    stringsAsFactors = FALSE
  )
  assignments <- build_public_transition_overrides(
    transitions, games, coach_seasons
  )
  expect_equal(assignments$start_week, c(0L, 4L))
  expect_equal(assignments$end_week, c(3L, 99L))
  expect_equal(assignments$source_games, c(3L, 2L))
  expect_equal(assignments$coach_id[1], "coach_jim_mora")
  expect_equal(assignments$coach_name[1], "Jim Mora")
  expect_true(assignments$interim[2])
})

test_that("public coach names remove table annotations", {
  expect_equal(clean_public_coach_name("Pete Golding[192]"), "Pete Golding")
  expect_equal(clean_public_coach_name("D. J. Durkin (interim)"), "D. J. Durkin")
})

test_that("destination context reduces unproven coach portability", {
  history <- data.frame(
    coach_id = "c", team = "Lower Context", season = 2025, week = 99,
    games = 13, wins = 11, above_expectation = 5, level = "fbs",
    context_strength = .30, target_context_strength = .30,
    playoff_appearances = 0, titles = 0
  )
  same <- build_coach_ratings(
    history, 2026, 1, .65, config, target_context = c(c = .30)
  )
  jump <- build_coach_ratings(
    history, 2026, 1, .65, config, target_context = c(c = .90)
  )
  expect_lt(jump$portability, same$portability)
  expect_lt(jump$rating, same$rating)
})

test_that("chronological model weeks do not reset in the postseason", {
  games <- data.frame(
    season = 2025, week = c(14L, 1L, 1L),
    kickoff = as.POSIXct(c("2025-12-06", "2025-12-20", "2026-01-02"), tz = "UTC")
  )
  timed <- add_chronological_model_week(games)
  expect_true(all(diff(timed$model_week) > 0))
})

test_that("pure model excludes brand, market, poll, FPI, and PFF fields", {
  data <- data.frame(
    season = 2025, week = 1, home = "Alabama", away = "Indiana",
    efficiency_diff = 1, special_teams_rating_diff = 2,
    market_home_spread = -3, ap_rank_diff = 2, fpi_diff = 4, pff_diff = 5,
    margin = 7
  )
  expect_equal(sort(football_feature_names(data)),
               sort(c("efficiency_diff", "special_teams_rating_diff")))
})

test_that("pure model admits differentials but rejects separate side strength columns", {
  data <- data.frame(
    home_power = 10, away_power = 5, power_rating_diff = 5,
    pregame_power_diff = 5,
    home_field_points = 2.4, model_week = 12,
    home_pregame_elo = 1600, away_pregame_elo = 1500, margin = 7
  )
  expect_equal(
    sort(football_feature_names(data)),
    sort(c("power_rating_diff", "home_field_points"))
  )
})

test_that("ridge base model extrapolates beyond historical twenty-point predictions", {
  synthetic <- data.frame(
    season = rep(2021:2025, each = 20), week = rep(1:20, 5),
    game_phase = "in_season", efficiency_diff = rep(seq(-5, 5, length.out = 20), 5)
  )
  synthetic$margin <- 3 * synthetic$efficiency_diff
  model <- fit_cfb_ensemble(synthetic, features = "efficiency_diff", lambda = .01,
                            config = config, fit_nonlinear = FALSE)
  prediction <- predict(model, data.frame(week = 5, game_phase = "in_season",
                                          efficiency_diff = 10))
  expect_gt(prediction$expected_margin, 25)
  contributions <- ridge_feature_contributions(
    model, data.frame(week = 5, game_phase = "in_season", efficiency_diff = 10)
  )
  expect_equal(as.numeric(model$ridge$coefficients[1] + rowSums(contributions)),
               prediction$base_margin)
})

test_that("core home field is constant and neutral sites remain zero", {
  expect_equal(resolve_home_field(c(TRUE, FALSE)), c(0, 2.4))
})

test_that("team-specific home field remains available as a challenger", {
  games <- data.frame(
    season = rep(2025, 5), home = "Notre Dame", away = paste0("A", 1:5),
    neutral_site = FALSE, margin = rep(10, 5),
    home_power = rep(8, 5), away_power = rep(5, 5),
    home_level = "fbs", away_level = "fbs", postseason_type = "regular",
    is_cfp = FALSE,
    kickoff = as.POSIXct(paste0("2025-09-", sprintf("%02d", 1:5)), tz = "UTC")
  )
  estimate <- team_home_field_as_of(
    "Notre Dame", 2026, 1, as.POSIXct("2026-09-01", tz = "UTC"), games
  )
  expect_gt(estimate, 2.4)
  expect_lt(estimate, 7)
})

test_that("raw histories exclude FCS and non-CFP bowls", {
  games <- data.frame(
    home_level = "fbs", away_level = c("fbs", "fcs", "fbs", "fbs", NA),
    postseason_type = c("regular", "regular", "bowl", "cfp", NA),
    is_cfp = c(FALSE, FALSE, FALSE, TRUE, NA)
  )
  expect_equal(raw_history_eligible(games), c(TRUE, FALSE, FALSE, TRUE, FALSE))
})

test_that("forced model picks select the higher-probability side without hiding the pass", {
  prediction <- data.frame(expected_margin = 3, fair_margin = 3, margin_sd = 12)
  schedule <- data.frame(home = "Notre Dame", away = "Miami")
  normal <- make_game_picks(prediction, schedule, -2.5, FALSE, config = config)
  forced <- make_game_picks(prediction, schedule, -2.5, TRUE, config = config)
  expect_equal(normal$pick_status, "pass")
  expect_true(is.na(normal$ats_pick))
  expect_equal(forced$pick_status, "forced_model_pick")
  expect_equal(forced$ats_pick, "Notre Dame")
})

test_that("eligibility includes P4, Notre Dame, UConn, ranked, and contenders", {
  schedule <- data.frame(
    game_id = letters[1:5], season = 2026, week = 3,
    home = c("P4", "Notre Dame", "Connecticut", "Ranked G5", "CFP G5"),
    away = paste0("Other", 1:5), neutral_site = FALSE
  )
  membership <- data.frame(
    team = c("P4", schedule$away, schedule$home[-1]), season = 2026,
    power_conference = c(TRUE, rep(FALSE, 9))
  )
  rankings <- data.frame(team = "Ranked G5", season = 2026, week = 3, rank = 24)
  cfp <- data.frame(team = "CFP G5", season = 2026, cfp_probability = .03,
                    conference_leader = FALSE)
  expect_true(all(weekly_game_eligibility(schedule, membership, rankings, cfp, config)))
})

test_that("injury scenarios include starting quarterback in and out projections", {
  injuries <- data.frame(
    team = "Notre Dame", status = "questionable", impact_points = 5,
    confidence = 1, availability_probability = .6,
    position = "QB", starter = TRUE, usage_share = 1,
    source_type = "school_official", conflict_flag = FALSE
  )
  scenarios <- build_injury_scenarios(injuries, 7, "Notre Dame", "Miami", config)
  expect_true(all(c("most_likely", "all_key_available", "all_key_unavailable",
                    "starting_qb_unavailable") %in% scenarios$scenario))
  expect_equal(scenarios$expected_margin[scenarios$scenario == "all_key_available"], 7)
  expect_equal(scenarios$expected_margin[scenarios$scenario == "all_key_unavailable"], 2)
})

test_that("article lines preserve opening, both books, consensus, and best side", {
  lines <- data.frame(
    game_id = "g", provider = rep(c("DraftKings", "FanDuel"), each = 2),
    captured_at = as.POSIXct(c("2026-09-01 12:00:00", "2026-09-04 16:00:00",
                               "2026-09-01 12:00:00", "2026-09-04 16:30:00"), tz = "UTC"),
    home_spread = c(-6.5, -7, -6, -7.5), total = 50
  )
  selected <- select_article_lines(lines, "g", as.POSIXct("2026-09-04 17:00:00", tz = "UTC"))
  expect_equal(selected$opening_home_spread, -6.25)
  expect_equal(selected$market_home_spread, -7.25)
  expect_equal(selected$best_home_spread, -7)
  expect_equal(selected$best_away_spread, 7.5)
})

test_that("CFP bye adjustment is shrunk and reports sample uncertainty", {
  set.seed(1)
  history <- data.frame(
    margin_residual = rnorm(16), first_round_bye = rep(c(0, 1), 8),
    rest_days_diff = rnorm(16), played_round_one = rep(c(1, 0), 8),
    conference_championship = sample(0:1, 16, TRUE),
    opponent_quality_diff = rnorm(16), travel_timezone_diff = sample(-3:3, 16, TRUE)
  )
  estimate <- fit_cfp_bye_adjustment(history)
  expect_equal(estimate$sample_size, 16)
  expect_true(is.finite(estimate$bye_adjustment))
  expect_true(is.finite(estimate$standard_error))
})

test_that("DuckDB snapshots are immutable", {
  temp <- tempfile("cfb_v2_test_")
  cfg <- cfb_v2_config(temp, 2026)
  ensure_v2_directories(cfg)
  con <- v2_connect(cfg)
  on.exit(v2_disconnect(con), add = TRUE)
  v2_init_schema(con)
  expect_true(all(c("expected_total", "total_sd") %in%
                    DBI::dbListFields(con, "prediction_snapshots")))
  expect_true("availability_probability" %in%
                DBI::dbListFields(con, "injury_snapshots"))
  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  run_id <- v2_run_id("article", 2026, 1, as_of)
  v2_register_run(con, run_id, "article", 2026, 1, as_of, cfg)
  row <- data.frame(
    game_id = "1", provider = "DraftKings", captured_at = as_of,
    snapshot_type = "article", home_spread = -7, total = 49,
    home_price = -110L, away_price = -110L, source_url = "test"
  )
  expect_silent(v2_append_snapshot(con, "line_snapshots", row, run_id,
                                   c("game_id", "provider", "captured_at")))
  expect_error(v2_append_snapshot(con, "line_snapshots", row, run_id,
                                  c("game_id", "provider", "captured_at")), "Immutable")
})

test_that("rolling splits never train on the test season or future seasons", {
  data <- data.frame(season = rep(2021:2025, each = 2))
  splits <- rolling_season_splits(data)
  expect_true(all(vapply(splits, function(x) {
    all(data$season[x$train] < x$test_season) & all(data$season[x$test] == x$test_season)
  }, logical(1))))
})

test_that("backtest summaries grade ATS pushes as missing", {
  predictions <- data.frame(
    model = "ridge_core", season = 2025, game_phase = "in_season",
    home = c("A", "C"), away = c("B", "D"),
    home_level = "fbs", away_level = "fbs",
    home_conference = "ACC", away_conference = "SEC",
    is_cfp = FALSE, neutral_site = FALSE, new_coach_game = FALSE,
    actual_margin = c(7, 3), expected_margin = c(6, 4),
    absolute_error = 1, winner_correct = TRUE,
    closing_home_spread = c(-3, -3), model_edge = c(3, 1),
    ats_correct = c(TRUE, NA), market_absolute_error = c(4, 0),
    margin_sd = 10
  )
  summary <- summarize_v2_backtest(predictions)
  overall <- summary[summary$slice == "all_weighted_games", ]
  expect_equal(overall$games, 2)
  expect_equal(overall$ats_graded, 1)
  expect_equal(overall$ats_accuracy, 1)
  expect_true("market_bin_0_3" %in% summary$slice)
  expect_equal(summary$games[summary$slice == "market_bin_0_3"], 2)
})

test_that("ATS calibration cannot reverse the margin edge on large spreads", {
  set.seed(42)
  edge_z <- rep(seq(-2, 2, length.out = 120), 2)
  spread <- c(rep(-3, 120), rep(-28, 120))
  home_cover <- stats::rbinom(
    length(edge_z), 1,
    stats::plogis(c(2 * edge_z[1:120], -2 * edge_z[121:240]))
  )
  validation <- data.frame(
    actual_margin = -spread + ifelse(home_cover == 1, 1, -1),
    expected_margin = edge_z * 10 - spread,
    closing_home_spread = spread,
    margin_sd = 10
  )
  expect_null(fit_ats_residual_model(validation))
})

test_that("matchup simulations are deterministic with a fixed seed", {
  a <- simulate_matchup(3, 51, 12, 10, simulations = 100, seed = 42)
  b <- simulate_matchup(3, 51, 12, 10, simulations = 100, seed = 42)
  expect_equal(a, b)
})

test_that("one-command article workflow writes immutable CSV, Parquet, and DuckDB output", {
  temp <- tempfile("cfb_v2_integration_")
  cfg <- cfb_v2_config(temp, 2026)
  cfg$model$minimum_training_rows <- 10000L
  cfg$model$ridge_lambda_grid <- c(2, 8)
  initialize_v2_project(cfg)

  write_input <- function(name, data) {
    utils::write.csv(data, file.path(cfg$inbox_dir, paste0(name, ".csv")),
                     row.names = FALSE, na = "")
  }
  kickoff <- "2026-09-05 23:30:00"
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
  write_input("team_week_features", team_features)

  set.seed(12)
  n <- 240
  training <- data.frame(
    game_id = paste0("t", seq_len(n)), season = rep(2021:2025, length.out = n),
    week = rep(1:12, length.out = n), game_phase = "in_season",
    home_level = "fbs", away_level = "fbs", postseason_type = "regular",
    is_cfp = FALSE, conference_championship = FALSE, neutral_site = FALSE
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
  training$coach_rating_65_35_diff <- rnorm(n, 0, .3)
  training$coach_rating_70_30_diff <- training$coach_rating_65_35_diff + rnorm(n, 0, .05)
  training$coach_rating_diff <- training$coach_rating_65_35_diff
  training$home_field_points <- 2.4
  training$margin <- 6 * training$offense_rating_diff +
    4 * training$defense_rating_diff + training$home_field_points + rnorm(n, 0, 8)
  training$total_points <- 52 + rnorm(n, 0, 8)
  training$closing_home_spread <- -training$margin + rnorm(n, 0, 4)
  write_input("training_games", training)

  as_of <- as.POSIXct("2026-09-04 17:00:00", tz = "UTC")
  result <- run_v2_week(cfg, 2026, 1, "article", as_of, strict = TRUE)
  expect_true(file.exists(result$csv))
  expect_true(file.exists(result$parquet))
  expect_equal(nrow(result$predictions), 1)
  expect_true(result$predictions$published)
  expect_equal(result$predictions$line_source, "DraftKings+FanDuel")

  con <- v2_connect(cfg, read_only = TRUE)
  on.exit(v2_disconnect(con), add = TRUE)
  stored <- DBI::dbGetQuery(con, "SELECT * FROM prediction_snapshots")
  expect_equal(nrow(stored), 1)
  expect_equal(stored$run_id, result$run_id)
  v2_disconnect(con)
  con <- NULL
  expect_error(
    run_v2_week(cfg, 2026, 1, "article", as_of + 60, strict = TRUE),
    "published article snapshot already exists"
  )
})
