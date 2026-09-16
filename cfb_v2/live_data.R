cached_power_rows <- function(games) {
  rows <- rbind(
    data.frame(team = games$home, season = games$season, model_week = games$model_week,
               power_rating = games$home_power),
    data.frame(team = games$away, season = games$season, model_week = games$model_week,
               power_rating = games$away_power)
  )
  rows <- rows[is.finite(rows$power_rating), ]
  rows[!duplicated(paste(rows$team, rows$season, rows$model_week)), ]
}

prepare_live_inputs <- function(inputs, config, season, week, as_of, refresh = FALSE) {
  raw <- load_cfbd_schedule_season(season, config, refresh, allow_missing_key = FALSE)
  membership <- load_cfbd_fbs_teams_season(season, config, refresh)
  pbp <- if (week > 1L) load_compact_pbp_season(season, config, refresh) else NULL
  universe <- if (!is.null(pbp)) {
    derive_games_from_pbp(pbp, schedule = raw, fbs_membership = membership)
  } else add_chronological_model_week(standardize_cfbd_schedule(raw))
  schedule <- universe[universe$season == season & universe$week == week &
                         !is.na(universe$kickoff) & universe$kickoff > as_of &
                         !universe$completed & universe$home_level == "fbs" &
                         universe$away_level == "fbs", , drop = FALSE]
  if (!nrow(schedule)) stop("No upcoming FBS-vs-FBS games match this week/as-of.")
  if (isTRUE(config$omit_thursday)) {
    schedule <- schedule[format(schedule$kickoff, "%u", tz = config$article_timezone) != "4", ]
  }
  if (!nrow(schedule)) stop("No games remain after the Thursday exclusion.")
  schedule <- schedule[order(schedule$kickoff, schedule$game_id), ]
  if (week <= 1L) schedule$model_week <- 1L
  if (length(unique(schedule$model_week)) != 1L) {
    stop("This slate crosses chronological feature weeks; split its schedule.")
  }
  model_week <- unique(schedule$model_week)
  schedule$coach_lookup_week <- ifelse(schedule$postseason_type == "regular", week, 99L)

  history_games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  history_games$kickoff <- parse_utc_datetime(history_games$kickoff)
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  team_games$kickoff <- parse_utc_datetime(team_games$kickoff)
  history_games <- history_games[history_games$season < season, ]
  team_games <- team_games[team_games$season < season, ]
  games <- history_games
  completed <- data.frame()
  if (!is.null(pbp)) {
    completed <- filter_completed_historical_games(universe)
    completed <- completed[completed$model_week < model_week & completed$kickoff < as_of, ]
    missing <- raw_history_eligible(completed) & !completed$pbp_available
    if (any(missing)) stop("Completed prior-week FBS games are missing play-by-play.")
    efficiency <- build_team_game_efficiencies_v2(
      pbp[pbp$game_id %in% completed$game_id, ], completed,
      turnover_prior_as_of(team_games, history_games, season)
    )
    games <- bind_rows_fill(history_games, completed)
    team_games <- bind_rows_fill(team_games, efficiency)
  }
  flags <- combine_membership(membership, read_csv_if_present(
    file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  priors <- fbs_bridge_prior_rows(team_games, flags, config, season)
  power <- bind_rows_fill(cached_power_rows(history_games),
    power_rating_for_week(games, season, model_week, week, config))
  target_rows <- data.frame(
    game_id = rep(schedule$game_id, 2), team = c(schedule$home, schedule$away),
    season = season, week = week, model_week = model_week,
    kickoff = rep(schedule$kickoff, 2), offense_epa = NA_real_,
    defense_epa = NA_real_, net_efficiency = NA_real_
  )
  targets <- unique(target_rows$team)
  team_history <- team_games[team_games$team %in% targets & team_games$season >= season-3L, ]
  snapshots <- build_team_pregame_snapshots(
    bind_rows_fill(team_history, target_rows), bind_rows_fill(games, schedule),
    power, config, priors
  )$snapshots
  snapshots <- snapshots[snapshots$game_id %in% schedule$game_id, ]
  inputs$team_week_features <- snapshots[setdiff(names(snapshots), "game_id")]

  coach_cache <- file.path(config$project_dir, "cfb_v2", "cache", "coaches",
                           paste0("current_fbs_coaches_", season, ".rds"))
  coaches <- pull_current_fbs_coaches(season, coach_cache, refresh, captured_at = as_of)
  assignments <- current_fbs_coach_assignments(
    membership$team, coaches, season, inputs$coach_assignment_overrides)
  inputs$coach_assignments <- assignments
  inputs$coach_history <- extend_current_coach_history(
    inputs$coach_history, completed, games, assignments, season, config)

  schedule$venue <- as.character(raw$venue[match(schedule$game_id, as.character(raw$game_id))])
  columns <- c(names(v2_input_templates()$schedule), "model_week", "coach_lookup_week")
  inputs$schedule <- schedule[columns]
  inputs$membership <- data.frame(team = membership$team, season = season,
    subdivision = "fbs", conference = membership$conference,
    power_conference = membership$conference %in% c("ACC", "SEC", "Big Ten", "Big 12"))
  line_cache <- file.path(config$project_dir, "cfb_v2", "cache", "lines",
                          paste0("cfbd_week_", week, "_", season, ".rds"))
  supplied_lines <- inputs$lines
  if (refresh || !file.exists(line_cache)) {
    lines <- pull_cfbd_lines(season, week, Sys.time())
    dir.create(dirname(line_cache), recursive = TRUE, showWarnings = FALSE)
    saveRDS(lines, line_cache)
  } else lines <- readRDS(line_cache)
  inputs$lines <- bind_rows_fill(lines, supplied_lines)
  if (nrow(inputs$lines)) {
    key <- paste(inputs$lines$game_id, inputs$lines$provider, inputs$lines$captured_at)
    inputs$lines <- inputs$lines[!duplicated(key, fromLast = TRUE), ]
  }
  inputs$automated_slate <- TRUE
  inputs
}
