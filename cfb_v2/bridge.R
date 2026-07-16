membership_fbs_flags <- function(membership) {
  if (is.null(membership) || !nrow(membership)) {
    return(data.frame(team = character(), season = integer(), fbs = logical(),
                      stringsAsFactors = FALSE))
  }
  assert_columns(membership, c("team", "season"), "membership")
  column <- first_existing_column(membership, c("classification", "subdivision"),
                                  label = "membership classification")
  data.frame(
    team = canonical_team(membership$team),
    season = as.integer(membership$season),
    fbs = tolower(as.character(membership[[column]])) == "fbs",
    stringsAsFactors = FALSE
  )
}

combine_membership <- function(...) {
  frames <- Filter(function(x) !is.null(x) && nrow(x), list(...))
  if (!length(frames)) {
    return(data.frame(team = character(), season = integer(), fbs = logical(),
                      stringsAsFactors = FALSE))
  }
  flags <- do.call(rbind, lapply(frames, membership_fbs_flags))
  flags[!duplicated(paste(flags$team, flags$season)), , drop = FALSE]
}

fbs_transition_teams <- function(membership_flags) {
  flags <- membership_flags[membership_flags$fbs, , drop = FALSE]
  rows <- lapply(sort(unique(flags$season)), function(s) {
    prior <- flags$team[flags$season == s - 1L]
    if (!length(prior)) return(NULL)
    new_teams <- setdiff(flags$team[flags$season == s], prior)
    if (!length(new_teams)) return(NULL)
    data.frame(team = new_teams, season = s, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(team = character(), season = integer(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

calibrate_fbs_bridge <- function(team_games, membership_flags, config) {
  assert_columns(team_games, c("team", "season", "net_efficiency", "margin"),
                 "historical team games")
  transitions <- fbs_transition_teams(membership_flags)
  team <- canonical_team(team_games$team)
  season <- as.integer(team_games$season)
  key <- paste(team, season)
  mover_rows <- key %in% paste(transitions$team, transitions$season)
  movers <- split(team_games[mover_rows, , drop = FALSE], key[mover_rows])
  if (length(movers) < config$bridge$minimum_movers) {
    stop("FCS-to-FBS bridge calibration needs at least ",
         config$bridge$minimum_movers,
         " historical first-year FBS team-seasons with games.", call. = FALSE)
  }
  mover_summary <- do.call(rbind, lapply(names(movers), function(name) {
    x <- movers[[name]]
    data.frame(
      mover = name, games = nrow(x),
      net_efficiency = mean(as.numeric(x$net_efficiency), na.rm = TRUE),
      margin = mean(as.numeric(x$margin), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  # FCS sides of crossover games enter as one pseudo-mover, anchoring the prior
  # for a generic promoted program rather than only the teams that already moved.
  flags_key <- paste(membership_flags$team, membership_flags$season)
  fbs_lookup <- membership_flags$fbs[match(key, flags_key)]
  crossover <- !mover_rows & !is.na(fbs_lookup) & !fbs_lookup
  crossover_net <- mean(as.numeric(team_games$net_efficiency)[crossover], na.rm = TRUE)
  prior_pool <- c(mover_summary$net_efficiency,
                  if (is.finite(crossover_net)) crossover_net)
  list(
    movers = mover_summary,
    crossover_net_efficiency = crossover_net,
    prior_net_efficiency = mean(prior_pool),
    margin_sd = max(config$bridge$sd_floor,
                    stats::sd(mover_summary$margin, na.rm = TRUE))
  )
}

apply_fbs_bridge_features <- function(team_features, transitions, bridge) {
  team_features$team <- canonical_team(team_features$team)
  hit <- paste(team_features$team, as.integer(team_features$season)) %in%
    paste(transitions$team, transitions$season)
  for (column in c("prior_season", "trailing_3yr", "preseason_prior")) {
    if (!column %in% names(team_features)) next
    value <- as.numeric(team_features[[column]])
    replace <- hit & !is.finite(value)
    team_features[[column]][replace] <- bridge$prior_net_efficiency
  }
  team_features$fbs_transition <- hit
  team_features
}

fbs_transition_game_flags <- function(schedule, transitions) {
  key <- paste(transitions$team, transitions$season)
  season <- as.integer(schedule$season)
  home_hit <- paste(canonical_team(schedule$home), season) %in% key
  away_hit <- paste(canonical_team(schedule$away), season) %in% key
  ifelse(home_hit & away_hit, "both",
         ifelse(home_hit, "home", ifelse(away_hit, "away", "")))
}

apply_fbs_bridge_predictions <- function(predictions, schedule, transitions, bridge,
                                         config, ats_threshold = NULL,
                                         ats_model = NULL) {
  flags <- fbs_transition_game_flags(schedule, transitions)
  predictions$fbs_transition <- flags[match(predictions$game_id, schedule$game_id)]
  involved <- nzchar(predictions$fbs_transition)
  if (!any(involved)) return(predictions)
  original_status <- predictions$pick_status
  inflated <- sqrt(predictions$margin_sd[involved]^2 + bridge$margin_sd^2)
  refreshed <- make_game_picks(
    data.frame(
      expected_margin = predictions$expected_margin[involved],
      fair_margin = -predictions$fair_spread[involved],
      margin_sd = inflated
    ),
    schedule[match(predictions$game_id[involved], schedule$game_id), , drop = FALSE],
    market_home_spread = predictions$market_home_spread[involved],
    force_pick = predictions$forced_pick[involved],
    ats_threshold = ats_threshold, ats_model = ats_model, config = config
  )
  for (column in c("margin_sd", "home_win_probability", "ats_edge_home",
                   "home_cover_probability", "straight_up_pick", "ats_pick",
                   "pick_status", "confidence_tier")) {
    predictions[[column]][involved] <- refreshed[[column]]
  }
  conflict <- involved & original_status == "injury_conflict_review"
  predictions$pick_status[conflict] <- "injury_conflict_review"
  dominated <- involved &
    bridge$margin_sd^2 >= config$bridge$dominance_share * predictions$margin_sd^2
  predictions$confidence_tier[dominated] <- "low"
  review <- dominated &
    predictions$pick_status %in% c("official_pick", "forced_model_pick")
  predictions$pick_status[review] <- "transition_review"
  predictions
}
