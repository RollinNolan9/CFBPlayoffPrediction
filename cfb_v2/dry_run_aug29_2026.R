script_path <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_path)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_path[1]),
                                winslash = "/", mustWork = TRUE)))
} else normalizePath(getwd(), winslash = "/", mustWork = TRUE)

for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R", "dashboard.R")) {
  source(file.path(project_dir, "cfb_v2", file))
}

cli_args <- commandArgs(trailingOnly = TRUE)
refresh_sources <- FALSE
slate <- "aug29"
for (arg in cli_args) {
  if (arg == "--refresh=true") refresh_sources <- TRUE
  else if (arg == "--refresh=false") refresh_sources <- FALSE
  else if (grepl("^--slate=", arg)) slate <- sub("^--slate=", "", arg)
  else stop("Unknown argument: ", arg, call. = FALSE)
}
if (!slate %in% c("aug29", "week1")) {
  stop("--slate must be aug29 or week1.", call. = FALSE)
}

config <- cfb_v2_config(project_dir, 2026L)
run_started_at <- as.POSIXct(Sys.time(), tz = "UTC")
captured_at <- run_started_at

if (slate == "aug29") {
  schedule <- data.frame(
    game_id = c("401856766", "401864494", "401858202", "401864577",
                "401866408", "401864570", "401858201", "401862693"),
    season = 2026L, week = 1L,
    kickoff = as.POSIXct(c(
      "2026-08-29 16:00:00", "2026-08-29 19:00:00",
      "2026-08-29 19:30:00", "2026-08-29 21:30:00",
      "2026-08-29 22:30:00", "2026-08-29 23:00:00",
      "2026-08-29 23:00:00", "2026-08-30 02:00:00"
    ), tz = "UTC"),
    home = c("TCU", "USC", "Virginia", "North Dakota State",
             "Eastern Michigan", "Florida State", "Stanford", "UNLV"),
    away = c("North Carolina", "San Jose State", "NC State", "Jacksonville State",
             "Sacramento State", "New Mexico State", "Hawai'i", "Memphis"),
    neutral_site = c(TRUE, rep(FALSE, 7)),
    venue = c("Aviva Stadium", "Los Angeles Memorial Coliseum", "Scott Stadium",
              "Fargodome", "Rynearson Stadium", "Doak Campbell Stadium",
              "Stanford Stadium", "Allegiant Stadium"),
    home_level = "fbs", away_level = "fbs", postseason_type = "regular",
    conference_championship = FALSE, is_cfp = FALSE,
    stringsAsFactors = FALSE
  )
  slate_title <- "2026 Week 0/1 Production Preseason Blend"
  slate_description <- "the eight August 29 games"
  output_bucket <- "week_0_1_blend"
  chart_title <- "Feature importance for the August 29 decisions"
} else {
  raw_schedule <- load_cfbd_schedule_season(
    2026L, config, refresh = refresh_sources, allow_missing_key = FALSE
  )
  schedule <- standardize_cfbd_schedule(raw_schedule)
  kickoff_date <- as.Date(schedule$kickoff, tz = "UTC")
  article_cutoff <- as.POSIXct(
    "2026-09-04 13:00:00", tz = config$article_timezone
  )
  keep <- !is.na(kickoff_date) & kickoff_date >= as.Date("2026-09-03") &
    kickoff_date <= as.Date("2026-09-07") &
    as.numeric(schedule$kickoff) > as.numeric(article_cutoff) &
    !is.na(schedule$completed) &
    !schedule$completed & schedule$home_level == "fbs" &
    schedule$away_level == "fbs"
  schedule <- schedule[keep, , drop = FALSE]
  raw_game_id <- as.character(column_value(raw_schedule, c("game_id", "id")))
  raw_venue <- as.character(column_value(raw_schedule, "venue"))
  schedule$venue <- raw_venue[match(schedule$game_id, raw_game_id)]
  schedule <- schedule[order(schedule$kickoff, schedule$game_id), , drop = FALSE]
  if (!nrow(schedule)) stop("CFBD returned no upcoming Week 1 FBS games.", call. = FALSE)
  slate_title <- "2026 Week 1 Preliminary Preseason Blend"
  slate_description <- "the post-Friday-publication FBS-vs-FBS slate"
  output_bucket <- "week_1_preliminary"
  chart_title <- "Feature importance for the preliminary Week 1 decisions"
}
schedule <- standardize_schedule(schedule)
schedule$source_season_type <- "regular"
schedule$completed <- FALSE
schedule$home_score <- NA_real_
schedule$away_score <- NA_real_
schedule$margin <- NA_real_
schedule$model_week <- 1L
schedule$coach_lookup_week <- 1L

line_cache <- file.path(config$project_dir, "cfb_v2", "cache", "lines",
                        "cfbd_week_1_2026.rds")
if (!refresh_sources && file.exists(line_cache)) {
  line_rows <- readRDS(line_cache)
  market_snapshot_source <- "cached_cfbd"
  captured_at <- max(as.POSIXct(line_rows$captured_at, tz = "UTC"), na.rm = TRUE)
} else {
  line_rows <- pull_cfbd_lines(2026L, 1L, captured_at)
  dir.create(dirname(line_cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(line_rows, line_cache)
  market_snapshot_source <- "live_cfbd"
}
market <- select_article_lines(line_rows, schedule$game_id, captured_at)
market <- market[match(schedule$game_id, market$game_id), , drop = FALSE]
if (any(!is.finite(market$market_home_spread))) {
  stop("DraftKings/FanDuel coverage is missing for game IDs: ",
       paste(schedule$game_id[!is.finite(market$market_home_spread)], collapse = ", "),
       call. = FALSE)
}

if (slate == "aug29") {
  coach_map <- data.frame(
    game_id = schedule$game_id,
    home_coach_id = c(
      "coach_sonny_dykes", "coach_lincoln_riley", "coach_tony_elliott",
      "coach_tim_polasek", "coach_chris_creighton", "coach_mike_norvell",
      "coach_tavita_pritchard", "coach_dan_mullen"
    ),
    home_coach = c(
      "Sonny Dykes", "Lincoln Riley", "Tony Elliott", "Tim Polasek",
      "Chris Creighton", "Mike Norvell", "Tavita Pritchard", "Dan Mullen"
    ),
    away_coach_id = c(
      "coach_bill_belichick", "coach_ken_niumatalolo", "coach_dave_doeren",
      "coach_charles_kelly", "coach_alonzo_carter", "coach_tony_sanchez",
      "coach_timmy_chang", "coach_charles_huff"
    ),
    away_coach = c(
      "Bill Belichick", "Ken Niumatalolo", "Dave Doeren", "Charles Kelly",
      "Alonzo Carter", "Tony Sanchez", "Timmy Chang", "Charles Huff"
    ),
    stringsAsFactors = FALSE
  )
  coach_mapping_source <- "audited manual August 29 mapping"
} else {
  coach_cache <- file.path(
    config$project_dir, "cfb_v2", "cache", "coaches",
    "current_fbs_coaches_2026.rds"
  )
  current_coaches <- pull_current_fbs_coaches(
    2026L, coach_cache, refresh = refresh_sources, captured_at = captured_at
  )
  team_coaches <- match_current_fbs_coaches(
    unique(c(schedule$home, schedule$away)), current_coaches
  )
  home_coaches <- team_coaches[match(schedule$home, team_coaches$team), ]
  away_coaches <- team_coaches[match(schedule$away, team_coaches$team), ]
  coach_map <- data.frame(
    game_id = schedule$game_id,
    home_coach_id = home_coaches$coach_id,
    home_coach = home_coaches$coach_name,
    away_coach_id = away_coaches$coach_id,
    away_coach = away_coaches$coach_name,
    stringsAsFactors = FALSE
  )
  coach_mapping_source <- unique(current_coaches$source_url)[1]
}

bind_compatible <- function(x, y) {
  columns <- union(names(x), names(y))
  for (column in setdiff(columns, names(x))) x[[column]] <- NA
  for (column in setdiff(columns, names(y))) y[[column]] <- NA
  rbind(x[columns], y[columns])
}

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = config$database, read_only = TRUE)
foundation_games <- DBI::dbReadTable(con, "foundation_games")
team_games <- DBI::dbReadTable(con, "foundation_team_games")
DBI::dbDisconnect(con, shutdown = TRUE)
for (column in intersect(c("home", "away"), names(foundation_games))) {
  foundation_games[[column]] <- canonical_team(foundation_games[[column]])
}
for (column in intersect(c("team", "opponent", "home", "away"), names(team_games))) {
  team_games[[column]] <- canonical_team(team_games[[column]])
}

preseason_power_ratings <- function(games, target_season, config) {
  past <- games[
    games$season >= target_season - 3L & games$season < target_season &
      is.finite(games$margin) & as.logical(games$completed), , drop = FALSE
  ]
  age <- target_season - past$season
  weights <- ifelse(age == 1L, 1, 0.35^(age - 1L))
  non_cfp_bowl <- past$postseason_type == "bowl" & !as.logical(past$is_cfp)
  weights[non_cfp_bowl] <- 0
  fcs <- is.na(past$home_level) | is.na(past$away_level) |
    past$home_level != "fbs" | past$away_level != "fbs"
  weights[fcs] <- weights[fcs] * config$training$fcs_rating_weight
  ratings <- opponent_adjusted_rating(
    past, value_col = "margin", ridge = 10, home_field = 2.4, weights = weights
  )
  names(ratings)[names(ratings) == "rating"] <- "power_rating"
  ratings$season <- target_season
  ratings$model_week <- 1L
  ratings$games_available <- 0L
  ratings
}

target_teams <- unique(c(schedule$home, schedule$away))
production_features <- preseason_feature_profile(config, "full")
fallback_features <- preseason_feature_profile(config, "returning_only")
prior_strength <- stats::aggregate(
  net_efficiency ~ team + season, team_games, mean, na.rm = TRUE
)
names(prior_strength)[names(prior_strength) == "net_efficiency"] <- "strength"
historical_membership <- utils::read.csv(
  file.path(config$data_dir, "historical_fbs_membership.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
preseason_priors <- read_csv_if_present(
  file.path(config$data_dir, "preseason_team_priors.csv"), required = TRUE
)
covered_priors_seasons <- sort(unique(as.integer(preseason_priors$season)))
if (!all(2021:2026 %in% covered_priors_seasons)) {
  stop("Full preseason priors must cover 2021-2026. Run ",
       "--mode=build-preseason --seasons=2026 before this card.", call. = FALSE)
}
target_priors <- preseason_priors[
  as.integer(preseason_priors$season) == 2026L, , drop = FALSE
]
if (!all(target_teams %in% target_priors$team)) {
  stop("2026 preseason priors are missing slate teams: ",
       paste(setdiff(target_teams, target_priors$team), collapse = ", "), call. = FALSE)
}

membership_flags <- membership_fbs_flags(historical_membership)
current_fbs <- sort(unique(target_priors$team))
membership_flags <- rbind(
  membership_flags,
  data.frame(team = current_fbs, season = 2026L, fbs = TRUE,
             stringsAsFactors = FALSE)
)
membership_flags <- membership_flags[
  !duplicated(paste(membership_flags$team, membership_flags$season)), , drop = FALSE
]
transitions <- fbs_transition_teams(membership_flags)
bridge <- calibrate_fbs_bridge(team_games, membership_flags, config)

history_rows <- team_games[
  team_games$team %in% target_teams & team_games$season >= 2023L, , drop = FALSE
]
history_games <- foundation_games[
  foundation_games$game_id %in% history_rows$game_id, , drop = FALSE
]

target_rows <- do.call(rbind, lapply(seq_len(nrow(schedule)), function(i) {
  game <- schedule[i, ]
  data.frame(
    game_id = game$game_id,
    team = c(game$home, game$away),
    offense_epa = NA_real_, epa_allowed = NA_real_, success_rate = NA_real_,
    pass_epa = NA_real_, rush_epa = NA_real_, havoc_allowed = NA_real_,
    havoc_generated = NA_real_, scrimmage_plays = NA_real_, defense_epa = NA_real_,
    turnover_rate_regressed = NA_real_, special_teams_epa_raw = NA_real_,
    special_teams_plays = NA_real_, special_teams_rating = NA_real_,
    season = 2026L, week = 1L, model_week = 1L, kickoff = game$kickoff,
    home = game$home, away = game$away, home_score = NA_real_, away_score = NA_real_,
    neutral_site = game$neutral_site, source_season_type = "regular",
    postseason_type = "regular", opponent = c(game$away, game$home),
    is_home = c(TRUE, FALSE), margin = NA_real_, net_efficiency = NA_real_,
    stringsAsFactors = FALSE
  )
}))

snapshot_games <- bind_compatible(history_games, schedule)
snapshot_team_games <- bind_compatible(history_rows, target_rows)
power <- preseason_power_ratings(foundation_games, 2026L, config)
snapshot_result <- build_team_pregame_snapshots(
  snapshot_team_games, snapshot_games, power, config
)
target_snapshots <- snapshot_result$snapshots[
  snapshot_result$snapshots$season == 2026L, , drop = FALSE
]
matchup <- build_historical_matchup_table(schedule, target_snapshots, config)

coach_history <- utils::read.csv(
  file.path(config$inbox_dir, "coach_history.csv"), stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = c("", "NA", "N/A", "null")
)
coach_history_manual <- read_csv_if_present(
  file.path(config$inbox_dir, "coach_history_manual.csv")
)
if (!is.null(coach_history_manual) && nrow(coach_history_manual)) {
  coach_history <- merge_manual_coach_history(
    coach_history, coach_history_manual
  )
}
ratings_65 <- build_coach_ratings(coach_history, 2026L, 1L, 0.65, config)
ratings_70 <- build_coach_ratings(coach_history, 2026L, 1L, 0.70, config)
coach_value <- function(ids, ratings) {
  value <- ratings$rating[match(ids, ratings$coach_id)]
  value[!is.finite(value)] <- 0
  value
}
matchup$coach_rating_65_35_diff <-
  coach_value(coach_map$home_coach_id, ratings_65) -
  coach_value(coach_map$away_coach_id, ratings_65)
matchup$coach_rating_70_30_diff <-
  coach_value(coach_map$home_coach_id, ratings_70) -
  coach_value(coach_map$away_coach_id, ratings_70)
matchup <- attach_preseason_features(
  matchup, preseason_priors, config, features = production_features,
  prefix = "ps_", require_coverage = TRUE
)
matchup <- apply_fbs_bridge_matchup_features(matchup, transitions, bridge)

training <- utils::read.csv(
  file.path(config$inbox_dir, "training_games.csv"), stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = c("", "NA", "N/A", "null")
)
validation <- prepare_training_data(training, config)
validation$data <- apply_fbs_bridge_backtest_features(
  validation$data, team_games, membership_flags, config
)
validation$data <- attach_preseason_features(
  validation$data, preseason_priors, config,
  features = production_features, prefix = "ps_"
)
production_columns <- paste0("ps_", production_features, "_diff")
fallback_columns <- paste0("ps_", fallback_features, "_diff")
preseason_gate_for <- function(columns) {
  function(test_season, features) {
    if (sum(covered_priors_seasons < test_season) >= 2L) features else
      setdiff(features, columns)
  }
}
coach_selection <- select_coach_split_validation(
  validation$data, validation$weights, config, fit_nonlinear = FALSE,
  fold_features = preseason_gate_for(production_columns)
)
validation$data <- coach_selection$data
preseason_rolling <- coach_selection$rolling
features <- coach_selection$features
matchup$coach_rating_diff <- if (coach_selection$recent_share == 0.70) {
  matchup$coach_rating_70_30_diff
} else matchup$coach_rating_65_35_diff
preseason_model <- fit_cfb_ensemble(
  validation$data, features = features, weights = validation$weights,
  lambda = preseason_rolling$best_lambda, config = config, fit_nonlinear = FALSE
)
preseason_model$calibration <- fit_error_calibration(
  preseason_rolling$predictions$expected_margin,
  preseason_rolling$predictions$actual,
  validation$data$game_phase[preseason_rolling$predictions$row_id], config
)

returning_features <- setdiff(
  features, setdiff(production_columns, fallback_columns)
)
returning_rolling <- rolling_validate_ensemble(
  validation$data, features = returning_features, weights = validation$weights,
  config = config, fit_nonlinear = FALSE,
  fold_features = preseason_gate_for(fallback_columns)
)
returning_model <- fit_cfb_ensemble(
  validation$data, features = returning_features, weights = validation$weights,
  lambda = returning_rolling$best_lambda, config = config, fit_nonlinear = FALSE
)
returning_model$calibration <- fit_error_calibration(
  returning_rolling$predictions$expected_margin,
  returning_rolling$predictions$actual,
  validation$data$game_phase[returning_rolling$predictions$row_id], config
)

foundation_features <- setdiff(features, production_columns)
foundation_rolling <- rolling_validate_ensemble(
  validation$data, features = foundation_features, weights = validation$weights,
  config = config, fit_nonlinear = FALSE
)
foundation_model <- fit_cfb_ensemble(
  validation$data, features = foundation_features, weights = validation$weights,
  lambda = foundation_rolling$best_lambda, config = config, fit_nonlinear = FALSE
)
foundation_model$calibration <- fit_error_calibration(
  foundation_rolling$predictions$expected_margin,
  foundation_rolling$predictions$actual,
  validation$data$game_phase[foundation_rolling$predictions$row_id], config
)

rolling <- blend_rolling_predictions(
  validation$data, foundation_rolling, preseason_rolling, config
)
model <- make_preseason_blend(
  foundation_model, preseason_model, rolling$calibration, config
)

week01_metrics <- function(result, data) {
  rows <- result$predictions$row_id
  keep <- data$game_phase[rows] == "preseason"
  actual <- result$predictions$actual[keep]
  expected <- result$predictions$expected_margin[keep]
  c(
    games = sum(is.finite(actual) & is.finite(expected)),
    mae = mean(abs(actual - expected), na.rm = TRUE),
    winner_accuracy = mean((actual > 0) == (expected > 0), na.rm = TRUE)
  )
}
blend_metrics <- week01_metrics(rolling, validation$data)
full_metrics <- week01_metrics(preseason_rolling, validation$data)
returning_metrics <- week01_metrics(returning_rolling, validation$data)
foundation_metrics <- week01_metrics(foundation_rolling, validation$data)
selected_profile <- "foundation_preseason_roster_aware_blend"

ats_rows <- rolling$predictions$row_id
ats_eligible <- ats_training_eligible(validation$data[ats_rows, , drop = FALSE])
ats_validation <- data.frame(
  season = validation$data$season[ats_rows][ats_eligible],
  actual_margin = rolling$predictions$actual[ats_eligible],
  expected_margin = rolling$predictions$expected_margin[ats_eligible],
  closing_home_spread = validation$data$closing_home_spread[ats_rows][ats_eligible],
  margin_sd = rolling$predictions$margin_sd[ats_eligible]
)
ats_model <- fit_ats_residual_model(ats_validation)
ats_threshold <- NULL
if (!is.null(ats_model)) {
  cover_probability <- predict_ats_home_cover(
    ats_model, ats_validation$expected_margin, ats_validation$closing_home_spread,
    ats_validation$margin_sd
  )
  threshold_validation <- data.frame(
    edge = ats_validation$expected_margin + ats_validation$closing_home_spread,
    cover_probability = cover_probability,
    covered = ifelse(
      cover_probability >= 0.5,
      ats_validation$actual_margin + ats_validation$closing_home_spread > 0,
      ats_validation$actual_margin + ats_validation$closing_home_spread < 0
    )
  )
  finite <- is.finite(threshold_validation$edge) &
    is.finite(threshold_validation$cover_probability)
  ats_threshold <- select_ats_threshold(threshold_validation[finite, ], config)
}

predictions <- predict_week(
  model, schedule, matchup, market$market_home_spread,
  force_game_ids = schedule$game_id, ats_threshold = ats_threshold,
  ats_model = ats_model, config = config
)
predictions <- apply_fbs_bridge_predictions(
  predictions, matchup, transitions, bridge, config,
  ats_threshold = ats_threshold, ats_model = ats_model
)
foundation_prediction <- predict(foundation_model, matchup)
returning_prediction <- predict(returning_model, matchup)
predictions$foundation_expected_margin <- foundation_prediction$expected_margin
predictions$returning_expected_margin <- returning_prediction$expected_margin
predictions$returning_production_adjustment <-
  predictions$returning_expected_margin - predictions$foundation_expected_margin
predictions$talent_adjustment <-
  predictions$preseason_expected_margin - predictions$returning_expected_margin
predictions$full_preseason_adjustment <-
  predictions$preseason_expected_margin - predictions$foundation_expected_margin
predictions$total_preseason_adjustment <-
  predictions$expected_margin - predictions$foundation_expected_margin

feature_eligible <- raw_history_eligible(
  foundation_games[match(team_games$game_id, foundation_games$game_id), , drop = FALSE]
)
coverage <- do.call(rbind, lapply(target_teams, function(team) {
  keep <- team_games$team == team & feature_eligible
  data.frame(
    team = team,
    prior_fbs_games = sum(keep & team_games$season == 2025L),
    trailing_fbs_games = sum(keep & team_games$season >= 2023L & team_games$season <= 2025L),
    stringsAsFactors = FALSE
  )
}))

home_coverage <- coverage[match(schedule$home, coverage$team), ]
away_coverage <- coverage[match(schedule$away, coverage$team), ]
selected_ratings <- if (coach_selection$recent_share == 0.70) ratings_70 else ratings_65
known_coaches <- unique(selected_ratings$coach_id)
prior_key <- paste(target_priors$team, target_priors$season)
returning_available <- function(teams) {
  value <- target_priors$returning_ppa_pct[
    match(paste(canonical_team(teams), 2026L), prior_key)
  ]
  is.finite(value)
}
talent_available <- function(teams) {
  value <- target_priors$talent_percentile[
    match(paste(canonical_team(teams), 2026L), prior_key)
  ]
  is.finite(value)
}

predictions$venue <- schedule$venue
predictions$neutral_site <- schedule$neutral_site
predictions$market_provider <- market$line_source
predictions$market_captured_at <- captured_at
predictions$opening_home_spread <- market$opening_home_spread
predictions$market_total <- market$market_total
predictions$draftkings_home_spread <- market$draftkings_home_spread
predictions$fanduel_home_spread <- market$fanduel_home_spread
predictions$home_coach <- coach_map$home_coach
predictions$away_coach <- coach_map$away_coach
predictions$home_coach_rating <- coach_value(coach_map$home_coach_id, selected_ratings)
predictions$away_coach_rating <- coach_value(coach_map$away_coach_id, selected_ratings)
predictions$home_coach_history <- coach_map$home_coach_id %in% known_coaches
predictions$away_coach_history <- coach_map$away_coach_id %in% known_coaches
predictions$home_returning_data <- returning_available(schedule$home)
predictions$away_returning_data <- returning_available(schedule$away)
predictions$home_talent_data <- talent_available(schedule$home)
predictions$away_talent_data <- talent_available(schedule$away)
predictions$home_prior_fbs_games <- home_coverage$prior_fbs_games
predictions$away_prior_fbs_games <- away_coverage$prior_fbs_games
predictions$home_trailing_fbs_games <- home_coverage$trailing_fbs_games
predictions$away_trailing_fbs_games <- away_coverage$trailing_fbs_games
predictions$transition_game <- nzchar(predictions$fbs_transition)
predictions$data_flag <- ifelse(
  predictions$transition_game, "fcs_to_fbs_transition",
  ifelse(!predictions$home_talent_data | !predictions$away_talent_data,
         "missing_team_talent",
         ifelse(!predictions$home_returning_data | !predictions$away_returning_data,
                "missing_returning_production",
                ifelse(!predictions$home_coach_history | !predictions$away_coach_history,
                       "new_coach_no_model_history",
                       ifelse(predictions$preseason_challenger_share >
                                config$preseason$challenger_share + 1e-8,
                              "roster_rebuild_review", "standard"))))
)
predictions$preseason_profile <- selected_profile
predictions$preseason_challenger_profile <- "objective_roster_rebuild_profile"
predictions$talent_available <-
  predictions$home_talent_data & predictions$away_talent_data
predictions$reported_confidence <- ifelse(
  predictions$data_flag == "standard", predictions$confidence_tier, "provisional"
)
predictions$pick_edge <- ifelse(
  predictions$ats_pick == predictions$home,
  predictions$ats_edge_home, -predictions$ats_edge_home
)
predictions$pick_cover_probability <- ifelse(
  predictions$ats_pick == predictions$home,
  predictions$home_cover_probability, 1 - predictions$home_cover_probability
)

feature_contributions <- ridge_feature_contributions(model, matchup)
feature_label_map <- c(
  power_rating_diff = "Opponent-adjusted power",
  success_rate_diff = "Success rate",
  turnover_rate_regressed_diff = "Regressed turnover rate",
  havoc_allowed_diff = "Havoc allowed",
  preseason_prior_diff = "Prior/trailing efficiency blend",
  trailing_3yr_diff = "Three-year efficiency",
  prior_season_diff = "Prior-season efficiency",
  havoc_generated_diff = "Havoc generated",
  offense_epa_diff = "Offensive EPA",
  pass_epa_diff = "Passing EPA",
  coach_rating_diff = "Coach rating",
  defense_epa_diff = "Defensive EPA",
  home_field_points = "Home field",
  rush_epa_diff = "Rushing EPA",
  source_games_diff = "Eligible source games",
  games_played_diff = "Current-season games",
  special_teams_rating_diff = "Special teams",
  recent_3_diff = "Last three games",
  recent_6_diff = "Last six games",
  season_to_date_diff = "Season to date",
  ps_returning_ppa_pct_diff = "Returning production PPA",
  ps_returning_passing_ppa_pct_diff = "Returning passing PPA",
  ps_returning_usage_pct_diff = "Returning usage",
  ps_retained_quality_diff = "Retained production quality",
  ps_talent_percentile_diff = "247 team talent percentile",
  ps_replacement_capacity_diff = "Talent replacement capacity",
  ps_portal_replacement_capacity_diff = "Portal replacement capacity",
  ps_portal_offense_replacement_diff = "Incoming transfer production"
)
feature_labels <- unname(feature_label_map[colnames(feature_contributions)])
feature_labels[is.na(feature_labels)] <- colnames(feature_contributions)[is.na(feature_labels)]
non_transition <- !predictions$transition_game
importance <- data.frame(
  feature = colnames(feature_contributions),
  label = feature_labels,
  standardized_coefficient = as.numeric(
    ridge_standardized_coefficients(model, matchup)[colnames(feature_contributions)]
  ),
  mean_abs_points_all_games = colMeans(abs(feature_contributions)),
  mean_abs_points_non_transition = colMeans(
    abs(feature_contributions[non_transition, , drop = FALSE])
  ),
  stringsAsFactors = FALSE
)
game_labels <- paste(schedule$away, "at", schedule$home)
importance <- cbind(
  importance,
  as.data.frame(t(feature_contributions), check.names = FALSE,
                stringsAsFactors = FALSE)
)
names(importance)[seq_along(game_labels) + 5L] <- game_labels

line_text <- function(home, away, home_spread) {
  ifelse(home_spread <= 0,
         paste0(home, " ", sprintf("%.1f", home_spread)),
         paste0(away, " ", sprintf("%.1f", -home_spread)))
}
pick_text <- function(pick, home, home_spread) {
  spread <- ifelse(pick == home, home_spread, -home_spread)
  paste0(pick, " ", ifelse(spread > 0, "+", ""), sprintf("%.1f", spread))
}
model_line_text <- function(home, away, margin) {
  favorite <- ifelse(margin >= 0, home, away)
  paste0(favorite, " -", sprintf("%.1f", abs(margin)))
}

predictions$market_line <- line_text(
  predictions$home, predictions$away, predictions$market_home_spread
)
predictions$model_line <- model_line_text(
  predictions$home, predictions$away, predictions$expected_margin
)
predictions$foundation_model_line <- model_line_text(
  predictions$home, predictions$away, predictions$foundation_expected_margin
)
predictions$returning_model_line <- model_line_text(
  predictions$home, predictions$away, predictions$returning_expected_margin
)
predictions$full_preseason_model_line <- model_line_text(
  predictions$home, predictions$away, predictions$preseason_expected_margin
)
predictions$ats_pick_line <- pick_text(
  predictions$ats_pick, predictions$home, predictions$market_home_spread
)

stopifnot(
  nrow(predictions) == nrow(schedule),
  !anyDuplicated(predictions$game_id),
  all(is.finite(predictions$expected_margin)),
  all(nzchar(predictions$ats_pick)),
  !any(tolower(features) %in% c("home", "away")),
  all(production_columns %in% features),
  all(fallback_columns %in% returning_features),
  !any(grepl("talent|replacement_capacity", returning_features,
             ignore.case = TRUE)),
  all(predictions$talent_available),
  all(predictions$preseason_profile == selected_profile),
  all(predictions$preseason_challenger_share >=
        config$preseason$challenger_share),
  all(predictions$preseason_challenger_share <=
        config$preseason$rebuild_challenger_share_ceiling),
  all(target_snapshots$games_played == 0),
  max(abs(ridge_intercept_contribution(model, matchup) +
            rowSums(feature_contributions) -
            predictions$expected_margin)) < 1e-8
)

run_slug <- format(run_started_at, "%Y%m%dT%H%M%SZ", tz = "UTC")
output_dir <- file.path(config$output_dir, "2026", output_bucket, run_slug)
if (dir.exists(output_dir) && length(list.files(output_dir))) {
  stop("Preseason blend snapshot already exists: ", output_dir,
       ". Use --refresh=true for a new source snapshot.", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(output_dir, "predictions.csv")
report_path <- file.path(output_dir, "report.md")
importance_path <- file.path(output_dir, "feature_contributions.csv")
chart_path <- file.path(output_dir, "feature_importance.png")
prior_path <- file.path(output_dir, "preseason_priors_2026.csv")
line_path <- file.path(output_dir, "market_snapshot.csv")
require_v2_package("readr")
readr::write_csv(predictions, csv_path, na = "")
readr::write_csv(target_priors, prior_path, na = "")
readr::write_csv(
  line_rows[line_rows$game_id %in% schedule$game_id, , drop = FALSE], line_path,
  na = ""
)
readr::write_csv(
  importance[order(-importance$mean_abs_points_all_games), ], importance_path,
  na = ""
)

top <- head(order(importance$mean_abs_points_all_games, decreasing = TRUE), 12L)
top <- top[order(importance$mean_abs_points_all_games[top])]
transition_teams <- sort(unique(c(
  predictions$home[predictions$transition_game],
  predictions$away[predictions$transition_game]
)))
if (length(transition_teams)) {
  plot_values <- rbind(
    all_games = importance$mean_abs_points_all_games[top],
    `Excluding FCS transitions` = importance$mean_abs_points_non_transition[top]
  )
  rownames(plot_values)[1] <- paste("All", nrow(predictions), "games")
  plot_colors <- c("#C46A3A", "#176B5B")
} else {
  plot_values <- matrix(
    importance$mean_abs_points_all_games[top], nrow = 1L,
    dimnames = list(paste("All", nrow(predictions), "games"), NULL)
  )
  plot_colors <- "#176B5B"
}
grDevices::png(chart_path, width = 1800, height = 1200, res = 180,
               bg = "white")
graphics::par(mar = c(7, 14, 5, 2), family = "sans")
positions <- graphics::barplot(
  plot_values, beside = TRUE, horiz = TRUE,
  names.arg = importance$label[top], las = 1,
  col = plot_colors, border = NA,
  xlim = c(0, max(plot_values) * 1.18), xlab = "Mean absolute points",
  main = chart_title
)
graphics::abline(v = 0, col = "#777777")
graphics::text(plot_values + 0.10, positions,
               labels = sprintf("%.2f", plot_values), pos = 4, cex = 0.72)
if (nrow(plot_values) > 1L) {
  graphics::legend("bottomright", legend = rownames(plot_values),
                   fill = plot_colors, border = NA, bty = "n")
}
graphics::mtext(
  paste0(
    "Exact ridge contribution to predicted margin.",
    if (length(transition_teams)) {
      paste0(" FCS transitions: ", paste(transition_teams, collapse = ", "), ".")
    } else " No FCS transitions are on this slate."
  ),
  side = 1, line = 4.5, cex = 0.78, col = "#444444"
)
grDevices::dev.off()
stopifnot(file.exists(chart_path), file.info(chart_path)$size > 0)

table_rows <- vapply(seq_len(nrow(predictions)), function(i) {
  x <- predictions[i, ]
  sprintf(
    "| %s at %s | %s | %s | %s | %s | %s | %.0f%% | %+.1f | %s | %s | %.1f | %.1f%% | %.1f | %s |",
    x$away, x$home, x$market_line, x$foundation_model_line,
    x$returning_model_line, x$full_preseason_model_line, x$model_line,
    100 * x$preseason_challenger_share,
    x$total_preseason_adjustment,
    x$straight_up_pick,
    x$ats_pick_line, x$pick_edge, 100 * x$pick_cover_probability,
    x$margin_sd, x$data_flag
  )
}, character(1))

missing_returning <- target_priors$team[
  target_priors$team %in% target_teams & !is.finite(target_priors$returning_ppa_pct)
]
missing_talent <- target_priors$team[
  target_priors$team %in% target_teams & !is.finite(target_priors$talent_percentile)
]
missing_coaches <- unique(c(
  coach_map$home_coach[!predictions$home_coach_history],
  coach_map$away_coach[!predictions$away_coach_history]
))
coach_split <- paste0(round(100 * coach_selection$recent_share), "/",
                      round(100 * (1 - coach_selection$recent_share)))
market_books <- paste(sort(unique(stats::na.omit(market$line_source))), collapse = "+")
season_data_note <- if (slate == "aug29") {
  "- Week 0 contains no 2026 game statistics. Priors use 2025 and trailing 2023-2025 FBS history; non-CFP bowls are excluded."
} else {
  paste0(
    "- This preliminary Week 1 run excludes the August 29 games from football features. ",
    "cfbfastR has not published standardized 2026 EPA yet; direct CFBD PPA is not mixed ",
    "onto the historical EPA scale. Priors use 2025 and trailing 2023-2025 FBS history."
  )
}
venue_note <- if (slate == "aug29") {
  "- TCU-North Carolina is neutral. NC State-Virginia is at Scott Stadium and receives the standard Virginia home field value."
} else {
  "- Neutral-site games receive zero generic home-field points; all other games use the venue listed in the CFBD schedule snapshot."
}
transition_note <- if (length(transition_teams)) {
  paste0(
    "- FCS-to-FBS transition teams on this slate: `",
    paste(transition_teams, collapse = ", "),
    "`. The calibrated bridge adds uncertainty and forces bridge-dominated games to transition review."
  )
} else {
  "- No FCS-to-FBS transition team appears on this slate."
}

report <- c(
  paste0("# ", slate_title),
  "",
  paste0("Market snapshot: ", market_books, " via CFBD, ",
         format(captured_at, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
         " (`", market_snapshot_source, "`)."),
  paste0("Selected profile: `", selected_profile,
         "`. The production margin starts at 80% foundation and 20% full preseason ",
         "challenger. Objective incoming-transfer production can raise the challenger ",
         "share to a 60% cap for major rebuilds; component estimates remain visible."),
  paste0("A side was requested for all ", nrow(predictions),
         " games; `article_pick` identifies a requested model selection that does not qualify as a validated best bet."),
  "",
  "| Game | Market | Foundation | Returning only | Full preseason challenger | Production blend | PS share | Blend delta | SU pick | ATS pick | Edge | Pick cover | Margin SD | Data flag |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---|---:|---:|---:|---:|---|",
  table_rows,
  "",
  "## Run settings",
  "",
  paste0("- Foundation ridge lambda `", foundation_rolling$best_lambda,
         "`; full-preseason ridge lambda `", preseason_rolling$best_lambda,
         "`; coach split `", coach_split, "`; training seasons 2020-2025."),
  "- The challenger adds CFBD returning PPA, returning passing PPA, returning usage, retained production quality, 247 talent percentile, portal depth, and source-quality-adjusted prior-year production from incoming transfers. Its Week 0/1 share is normally 20% and capped at 60% for major rebuilds; all shares phase-fade to zero in Week 5.",
  "- Raw talent rank does not enter the model. Talent is season-normalized, and its incremental effect is shown against the returning-only challenger.",
  "- No PFF, preseason QB tier, polls, FPI, injuries, weather, or market spread enters the margin model. Transfer ratings and prior-year transfer production are preseason challenger inputs only; the market is used by the separate ATS comparison layer.",
  "- Every requested game receives an article ATS side. Historical ATS calibration does not clear the -110 break-even rate, so none is promoted to a validated best bet; unvalidated selections retain 50% calibrated cover probability instead of unsupported confidence.",
  "- Market spreads above 21 points retain the model side and margin but are labeled `large_spread_review`; the cached requested-side ATS rate in that bucket is only 46.4%.",
  season_data_note,
  venue_note,
  paste0("- Current head-coach mappings were resolved from `", coach_mapping_source,
         "`; ratings still use only history available before the game."),
  "",
  "## Historical check",
  "",
  paste0("- Production-blend Week 0/1 rolling MAE: `",
         sprintf("%.2f", blend_metrics[["mae"]]), "` across `",
         blend_metrics[["games"]], "` held-out games; full challenger: `",
         sprintf("%.2f", full_metrics[["mae"]]), "`; returning-only: `",
         sprintf("%.2f", returning_metrics[["mae"]]),
         "`; foundation-only: `", sprintf("%.2f", foundation_metrics[["mae"]]), "`."),
  paste0("- Production-blend straight-up accuracy: `",
         sprintf("%.1f%%", 100 * blend_metrics[["winner_accuracy"]]),
         "`; full challenger: `",
         sprintf("%.1f%%", 100 * full_metrics[["winner_accuracy"]]),
         "`; returning-only: `",
         sprintf("%.1f%%", 100 * returning_metrics[["winner_accuracy"]]),
         "`; foundation-only: `",
         sprintf("%.1f%%", 100 * foundation_metrics[["winner_accuracy"]]), "`."),
  "",
  "## Data warnings",
  "",
  paste0("- Missing 2026 returning-production rows among this slate: `",
         if (length(missing_returning)) paste(missing_returning, collapse = ", ") else
           "none", "`. Missing values receive the fold-trained neutral recipe value."),
  paste0("- Missing 2026 talent rows among this slate: `",
         if (length(missing_talent)) paste(missing_talent, collapse = ", ") else
           "none", "`."),
  transition_note,
  paste0("- Coaches without modeled prior head-coach history: `",
         if (length(missing_coaches)) paste(missing_coaches, collapse = ", ") else
           "none", "`. Their contribution is neutral rather than inferred from school brand."),
  paste0("- This is a preliminary market snapshot captured ",
         format(captured_at, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
         "; it is not the Friday article snapshot or closing market."),
  "- The injury input is not populated for this preliminary run. Questionable high-usage players will require in/out scenarios before publication.",
  "",
  "## Feature importance",
  "",
  paste0("The chart ranks variables by their mean absolute point contribution across ",
         slate_description, ". The comparison removes any FCS-transition games, whose missing FBS histories distort several inputs."),
  "",
  "![Preliminary slate feature importance](feature_importance.png)",
  "",
  "## Feature drivers",
  "",
  paste0("- **", predictions$away, " at ", predictions$home, ":** ",
         predictions$top_drivers)
)
writeLines(report, report_path, useBytes = TRUE)
dashboard_path <- render_cfb_dashboard(
  csv_path, project_dir, output_dir, "dashboard.html"
)

cat("Dry-run report:", report_path, "\n")
cat("Predictions:", csv_path, "\n")
cat("Feature chart:", chart_path, "\n")
cat("Feature contributions:", importance_path, "\n")
cat("2026 preseason priors:", prior_path, "\n")
cat("Market snapshot:", line_path, "\n")
cat("Dashboard:", dashboard_path, "\n")
print(predictions[c("away", "home", "market_line", "foundation_model_line",
                    "returning_model_line", "full_preseason_model_line",
                    "model_line", "total_preseason_adjustment",
                    "straight_up_pick",
                    "ats_pick_line", "pick_edge", "pick_cover_probability",
                    "pick_status", "reported_confidence", "data_flag")])
