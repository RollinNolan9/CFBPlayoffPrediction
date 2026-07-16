script_path <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_path)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_path[1]),
                                winslash = "/", mustWork = TRUE)))
} else normalizePath(getwd(), winslash = "/", mustWork = TRUE)

for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "workflow.R")) {
  source(file.path(project_dir, "cfb_v2", file))
}

config <- cfb_v2_config(project_dir, 2026L)
captured_at <- as.POSIXct("2026-07-15 22:57:35", tz = "UTC")

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
schedule <- standardize_schedule(schedule)
schedule$source_season_type <- "regular"
schedule$completed <- FALSE
schedule$home_score <- NA_real_
schedule$away_score <- NA_real_
schedule$margin <- NA_real_
schedule$model_week <- 1L
schedule$coach_lookup_week <- 1L

market <- data.frame(
  game_id = schedule$game_id,
  provider = "DraftKings",
  captured_at = captured_at,
  home_spread = c(-6.5, -35.5, -3, -10, -8.5, -29.5, -3, -3),
  opening_home_spread = c(NA, -35.5, NA, -10, -8.5, -28.5, -3, -3),
  total = c(49.5, NA, 53.5, NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

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
matchup$coach_rating_diff <- matchup$coach_rating_65_35_diff

training <- utils::read.csv(
  file.path(config$inbox_dir, "training_games.csv"), stringsAsFactors = FALSE,
  check.names = FALSE, na.strings = c("", "NA", "N/A", "null")
)
validation <- prepare_training_data(training, config)
validation$data$coach_rating_diff <- validation$data$coach_rating_65_35_diff
features <- football_feature_names(validation$data)
features <- c(setdiff(features, c("coach_rating_65_35_diff",
                                  "coach_rating_70_30_diff", "coach_rating_diff")),
              "coach_rating_diff")
rolling <- rolling_validate_ensemble(
  validation$data, features = features, weights = validation$weights,
  config = config, fit_nonlinear = FALSE
)
model <- fit_cfb_ensemble(
  validation$data, features = features, weights = validation$weights,
  lambda = rolling$best_lambda, config = config, fit_nonlinear = FALSE
)
model$calibration <- fit_error_calibration(
  rolling$predictions$expected_margin, rolling$predictions$actual,
  validation$data$game_phase[rolling$predictions$row_id], config
)

ats_rows <- rolling$predictions$row_id
ats_eligible <- ats_training_eligible(validation$data[ats_rows, , drop = FALSE])
ats_validation <- data.frame(
  actual_margin = rolling$predictions$actual[ats_eligible],
  expected_margin = rolling$predictions$expected_margin[ats_eligible],
  closing_home_spread = validation$data$closing_home_spread[ats_rows][ats_eligible],
  margin_sd = rolling$predictions$margin_sd[ats_eligible]
)
ats_model <- fit_ats_residual_model(ats_validation)
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

predictions <- predict_week(
  model, schedule, matchup, market$home_spread,
  force_game_ids = schedule$game_id, ats_threshold = ats_threshold,
  ats_model = ats_model, config = config
)

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
known_coaches <- unique(ratings_65$coach_id)
transition_teams <- c("North Dakota State", "Sacramento State")

predictions$venue <- schedule$venue
predictions$neutral_site <- schedule$neutral_site
predictions$market_provider <- market$provider
predictions$market_captured_at <- market$captured_at
predictions$opening_home_spread <- market$opening_home_spread
predictions$market_total <- market$total
predictions$home_coach <- coach_map$home_coach
predictions$away_coach <- coach_map$away_coach
predictions$home_coach_rating <- coach_value(coach_map$home_coach_id, ratings_65)
predictions$away_coach_rating <- coach_value(coach_map$away_coach_id, ratings_65)
predictions$home_coach_history <- coach_map$home_coach_id %in% known_coaches
predictions$away_coach_history <- coach_map$away_coach_id %in% known_coaches
predictions$home_prior_fbs_games <- home_coverage$prior_fbs_games
predictions$away_prior_fbs_games <- away_coverage$prior_fbs_games
predictions$home_trailing_fbs_games <- home_coverage$trailing_fbs_games
predictions$away_trailing_fbs_games <- away_coverage$trailing_fbs_games
predictions$transition_game <- schedule$home %in% transition_teams |
  schedule$away %in% transition_teams
predictions$data_flag <- ifelse(
  predictions$transition_game, "fcs_to_fbs_transition",
  ifelse(!predictions$home_coach_history | !predictions$away_coach_history,
         "new_coach_no_model_history", "standard")
)
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
  season_to_date_diff = "Season to date"
)
feature_labels <- unname(feature_label_map[colnames(feature_contributions)])
feature_labels[is.na(feature_labels)] <- colnames(feature_contributions)[is.na(feature_labels)]
non_transition <- !predictions$transition_game
importance <- data.frame(
  feature = colnames(feature_contributions),
  label = feature_labels,
  standardized_coefficient = as.numeric(
    model$ridge$coefficients[colnames(feature_contributions)]
  ),
  mean_abs_points_all_games = colMeans(abs(feature_contributions)),
  mean_abs_points_non_transition = colMeans(
    abs(feature_contributions[non_transition, , drop = FALSE])
  ),
  stringsAsFactors = FALSE
)
game_labels <- c("UNC at TCU", "SJSU at USC", "NC State at Virginia",
                 "Jax State at NDSU", "Sac State at EMU", "NMSU at FSU",
                 "Hawaii at Stanford", "Memphis at UNLV")
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
predictions$ats_pick_line <- pick_text(
  predictions$ats_pick, predictions$home, predictions$market_home_spread
)

stopifnot(
  nrow(predictions) == 8L,
  !anyDuplicated(predictions$game_id),
  all(is.finite(predictions$expected_margin)),
  all(nzchar(predictions$ats_pick)),
  !any(tolower(features) %in% c("home", "away")),
  all(target_snapshots$games_played == 0),
  max(abs(model$ridge$coefficients[1] + rowSums(feature_contributions) -
            predictions$expected_margin)) < 1e-8
)

output_dir <- file.path(config$output_dir, "2026", "august_29_dry_run")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(output_dir, "predictions_20260715.csv")
report_path <- file.path(output_dir, "report_20260715.md")
importance_path <- file.path(output_dir, "feature_contributions_20260715.csv")
chart_path <- file.path(output_dir, "feature_importance_20260715.png")
utils::write.csv(predictions, csv_path, row.names = FALSE, na = "")
utils::write.csv(
  importance[order(-importance$mean_abs_points_all_games), ],
  importance_path, row.names = FALSE, na = ""
)

top <- head(order(importance$mean_abs_points_all_games, decreasing = TRUE), 12L)
top <- top[order(importance$mean_abs_points_all_games[top])]
plot_values <- rbind(
  `All eight games` = importance$mean_abs_points_all_games[top],
  `Excluding FCS transitions` = importance$mean_abs_points_non_transition[top]
)
grDevices::png(chart_path, width = 1800, height = 1200, res = 180,
               bg = "white")
graphics::par(mar = c(7, 14, 5, 2), family = "sans")
positions <- graphics::barplot(
  plot_values, beside = TRUE, horiz = TRUE,
  names.arg = importance$label[top], las = 1,
  col = c("#C46A3A", "#176B5B"), border = NA,
  xlim = c(0, max(plot_values) * 1.18), xlab = "Mean absolute points",
  main = "Feature importance for the August 29 decisions"
)
graphics::abline(v = 0, col = "#777777")
graphics::text(plot_values + 0.10, positions,
               labels = sprintf("%.2f", plot_values), pos = 4, cex = 0.72)
graphics::legend("bottomright", legend = rownames(plot_values),
                 fill = c("#C46A3A", "#176B5B"), border = NA, bty = "n")
graphics::mtext(
  "Exact ridge contribution to predicted margin. FCS transitions are NDSU and Sacramento State.",
  side = 1, line = 4.5, cex = 0.78, col = "#444444"
)
grDevices::dev.off()
stopifnot(file.exists(chart_path), file.info(chart_path)$size > 0)

table_rows <- vapply(seq_len(nrow(predictions)), function(i) {
  x <- predictions[i, ]
  sprintf(
    "| %s at %s | %s | %s | %s | %s | %.1f | %.1f%% | %.1f | %s |",
    x$away, x$home, x$market_line, x$model_line, x$straight_up_pick,
    x$ats_pick_line, x$pick_edge, 100 * x$pick_cover_probability,
    x$margin_sd, x$data_flag
  )
}, character(1))

report <- c(
  "# 2026 August 29 Dry Run",
  "",
  paste0("Market snapshot: DraftKings via CFBD, ",
         format(captured_at, "%Y-%m-%d %H:%M:%S UTC"), "."),
  "A side was requested for all eight games; `forced_model_pick` identifies a game the normal card would have passed.",
  "",
  "| Game | DraftKings | Model line | SU pick | ATS pick | Edge | Pick cover | Margin SD | Data flag |",
  "|---|---:|---:|---|---:|---:|---:|---:|---|",
  table_rows,
  "",
  "## Run settings",
  "",
  paste0("- Ridge core, lambda `", rolling$best_lambda,
         "`, coach split `65/35`, training seasons 2020-2025."),
  "- Current-team foundation only: no PFF, preseason QB, portal, returning production, injuries, weather, or market spread inside the margin model.",
  "- Week 0 contains no 2026 game statistics. Priors use 2025 and trailing 2023-2025 FBS history; non-CFP bowls are excluded.",
  "- TCU-North Carolina is neutral. NC State-Virginia is at Scott Stadium and receives the standard Virginia home field value.",
  "",
  "## Data warnings",
  "",
  "- North Dakota State and Sacramento State are 2026 FCS-to-FBS transitions. Their prior FCS schedules are not treated as ordinary FBS efficiency history, so both projections are provisional.",
  "- The transition-game edges and cover probabilities are diagnostics only; their uncertainty does not include the missing FCS-to-FBS translation error.",
  "- Tim Polasek, Alonzo Carter, and Tavita Pritchard do not yet have modeled head-coach history in the cached FBS coach ledger; their coach contribution is neutral. Charles Huff maps to his Marshall/Southern Miss history.",
  "- These are July look-ahead lines, not the Friday article snapshot or closing market.",
  "",
  "## Feature importance",
  "",
  "The chart ranks variables by their mean absolute point contribution across this slate. The comparison removes the two FCS-transition games, whose missing FBS histories distort several inputs.",
  "",
  "![August 29 feature importance](feature_importance_20260715.png)",
  "",
  "## Feature drivers",
  "",
  paste0("- **", predictions$away, " at ", predictions$home, ":** ",
         predictions$top_drivers)
)
writeLines(report, report_path)

cat("Dry-run report:", report_path, "\n")
cat("Predictions:", csv_path, "\n")
cat("Feature chart:", chart_path, "\n")
cat("Feature contributions:", importance_path, "\n")
print(predictions[c("away", "home", "market_line", "model_line", "straight_up_pick",
                    "ats_pick_line", "pick_edge", "pick_cover_probability",
                    "pick_status", "reported_confidence", "data_flag")])
