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

fbs_bridge_feature_sources <- function() {
  c(
    prior_season = "net_efficiency", trailing_3yr = "net_efficiency",
    preseason_prior = "net_efficiency", power_rating = "margin",
    offense_rating = "offense_epa", offense_epa = "offense_epa",
    defense_rating = "defense_epa", defense_epa = "defense_epa",
    special_teams_rating = "special_teams_rating", pass_epa = "pass_epa",
    rush_epa = "rush_epa", success_rate = "success_rate",
    havoc_allowed = "havoc_allowed", havoc_generated = "havoc_generated",
    turnover_rate_regressed = "turnover_rate_regressed"
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
  flags_key <- paste(membership_flags$team, membership_flags$season)
  fbs_lookup <- membership_flags$fbs[match(key, flags_key)]
  covered_season <- season %in% membership_flags$season
  crossover <- !mover_rows & covered_season & (is.na(fbs_lookup) | !fbs_lookup)
  if (length(movers) < config$bridge$minimum_movers && !any(crossover)) {
    stop("FCS-to-FBS bridge calibration needs historical first-year FBS movers ",
         "or crossover games.", call. = FALSE)
  }
  mover_rows_list <- lapply(names(movers), function(name) {
    x <- movers[[name]]
    data.frame(
      mover = name, games = nrow(x),
      net_efficiency = mean(as.numeric(x$net_efficiency), na.rm = TRUE),
      margin = mean(as.numeric(x$margin), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  mover_summary <- if (length(mover_rows_list)) do.call(rbind, mover_rows_list) else
    data.frame(mover = character(), games = integer(), net_efficiency = numeric(),
               margin = numeric(), stringsAsFactors = FALSE)
  # FCS sides of crossover games enter as one pseudo-mover, anchoring the prior
  # for a generic promoted program rather than only the teams that already moved.
  metric_prior <- function(column) {
    if (!column %in% names(team_games)) return(NA_real_)
    mover_values <- vapply(movers, function(x) {
      mean(as.numeric(x[[column]]), na.rm = TRUE)
    }, numeric(1))
    crossover_value <- mean(as.numeric(team_games[[column]])[crossover], na.rm = TRUE)
    pool <- c(mover_values, crossover_value)
    pool <- pool[is.finite(pool)]
    if (length(pool)) mean(pool) else NA_real_
  }
  sources <- unique(unname(fbs_bridge_feature_sources()))
  feature_priors <- stats::setNames(vapply(sources, metric_prior, numeric(1)), sources)
  crossover_net <- mean(as.numeric(team_games$net_efficiency)[crossover], na.rm = TRUE)
  spread <- if (nrow(mover_summary) >= 2L) {
    stats::sd(mover_summary$margin, na.rm = TRUE)
  } else {
    stats::sd(as.numeric(team_games$margin)[crossover], na.rm = TRUE)
  }
  if (!is.finite(spread)) spread <- config$bridge$sd_floor
  list(
    movers = mover_summary,
    crossover_net_efficiency = crossover_net,
    prior_net_efficiency = unname(feature_priors[["net_efficiency"]]),
    feature_priors = feature_priors,
    margin_sd = max(config$bridge$sd_floor, spread)
  )
}

apply_fbs_bridge_features <- function(team_features, transitions, bridge) {
  team_features$team <- canonical_team(team_features$team)
  hit <- paste(team_features$team, as.integer(team_features$season)) %in%
    paste(transitions$team, transitions$season)
  games <- if ("source_games" %in% names(team_features)) {
    as.numeric(team_features$source_games)
  } else if ("games_played" %in% names(team_features)) {
    as.numeric(team_features$games_played)
  } else rep(0, nrow(team_features))
  games[!is.finite(games)] <- 0
  history <- c("prior_season", "trailing_3yr", "preseason_prior")
  sources <- fbs_bridge_feature_sources()
  for (column in names(sources)) {
    if (!column %in% names(team_features)) next
    prior <- unname(bridge$feature_priors[[sources[[column]]]])
    if (!is.finite(prior)) next
    value <- as.numeric(team_features[[column]])
    placeholder <- !is.finite(value) | abs(value) < sqrt(.Machine$double.eps)
    replace <- if (column %in% history) hit & !is.finite(value) else
      hit & games <= 0 & placeholder
    team_features[[column]][replace] <- prior
  }
  team_features$fbs_transition <- hit
  team_features
}

apply_fbs_bridge_matchup_features <- function(matchups, transitions, bridge) {
  flags <- fbs_transition_game_flags(matchups, transitions)
  affected <- nzchar(flags)
  sources <- fbs_bridge_feature_sources()
  history <- c("prior_season", "trailing_3yr", "preseason_prior")
  for (side in c("home", "away")) {
    hit <- flags %in% c(side, "both")
    games_column <- paste0(side, "_source_games")
    fallback_column <- paste0(side, "_games_played")
    games <- if (games_column %in% names(matchups)) {
      as.numeric(matchups[[games_column]])
    } else if (fallback_column %in% names(matchups)) {
      as.numeric(matchups[[fallback_column]])
    } else rep(0, nrow(matchups))
    games[!is.finite(games)] <- 0
    for (feature in names(sources)) {
      column <- paste0(side, "_", feature)
      if (!column %in% names(matchups)) next
      prior <- unname(bridge$feature_priors[[sources[[feature]]]])
      if (!is.finite(prior)) next
      value <- as.numeric(matchups[[column]])
      placeholder <- !is.finite(value) | abs(value) < sqrt(.Machine$double.eps)
      replace <- if (feature %in% history) hit & !is.finite(value) else
        hit & games <= 0 & placeholder
      matchups[[column]][replace] <- prior
    }
  }
  for (feature in names(sources)) {
    home <- paste0("home_", feature)
    away <- paste0("away_", feature)
    difference <- paste0(feature, "_diff")
    if (!all(c(home, away, difference) %in% names(matchups))) next
    weight <- if (feature %in% c("prior_season", "trailing_3yr")) {
      matchups$prior_history_weight
    } else if (feature == "preseason_prior") {
      matchups$preseason_weight
    } else 1
    matchups[[difference]][affected] <-
      ((matchups[[home]] - matchups[[away]]) * weight)[affected]
  }
  existing <- if ("fbs_transition_game" %in% names(matchups)) {
    as.logical(matchups$fbs_transition_game)
  } else rep(FALSE, nrow(matchups))
  existing[is.na(existing)] <- FALSE
  matchups$fbs_transition_game <- existing | affected
  matchups
}

apply_fbs_bridge_backtest_features <- function(data, team_games, membership_flags, config) {
  transitions <- fbs_transition_teams(membership_flags)
  data$fbs_transition_game <- FALSE
  data$fbs_bridge_sd <- 0
  for (season in intersect(sort(unique(transitions$season)), sort(unique(data$season)))) {
    season_transitions <- transitions[transitions$season == season, , drop = FALSE]
    flags <- fbs_transition_game_flags(data, season_transitions)
    rows <- as.integer(data$season) == season & nzchar(flags)
    if (!any(rows)) next
    bridge <- calibrate_fbs_bridge(
      team_games[as.integer(team_games$season) < season, , drop = FALSE],
      membership_flags[as.integer(membership_flags$season) < season, , drop = FALSE],
      config
    )
    data <- apply_fbs_bridge_matchup_features(data, season_transitions, bridge)
    data$fbs_transition_game[rows] <- TRUE
    games <- fbs_transition_games_played(data, flags)
    fade <- prior_season_feature_weight(data$week[rows], games[rows], config)
    data$fbs_bridge_sd[rows] <- bridge$margin_sd * fade
  }
  data
}

fbs_transition_game_flags <- function(schedule, transitions) {
  key <- paste(transitions$team, transitions$season)
  season <- as.integer(schedule$season)
  home_hit <- paste(canonical_team(schedule$home), season) %in% key
  away_hit <- paste(canonical_team(schedule$away), season) %in% key
  ifelse(home_hit & away_hit, "both",
         ifelse(home_hit, "home", ifelse(away_hit, "away", "")))
}

fbs_transition_games_played <- function(schedule, flags) {
  fallback <- pmax(0, as.numeric(schedule$week) - 1)
  side_games <- function(side) {
    column <- paste0(side, "_games_played")
    games <- if (column %in% names(schedule)) as.numeric(schedule[[column]]) else fallback
    games[!is.finite(games)] <- fallback[!is.finite(games)]
    games
  }
  home <- side_games("home")
  away <- side_games("away")
  games <- fallback
  games[flags == "home"] <- home[flags == "home"]
  games[flags == "away"] <- away[flags == "away"]
  games[flags == "both"] <- pmin(home[flags == "both"], away[flags == "both"])
  games
}

apply_fbs_bridge_predictions <- function(predictions, schedule, transitions, bridge,
                                         config, ats_threshold = NULL,
                                         ats_model = NULL) {
  flags <- fbs_transition_game_flags(schedule, transitions)
  predictions$fbs_transition <- flags[match(predictions$game_id, schedule$game_id)]
  involved <- nzchar(predictions$fbs_transition)
  if (!any(involved)) return(predictions)
  original_status <- predictions$pick_status
  schedule_rows <- match(predictions$game_id[involved], schedule$game_id)
  weeks <- as.numeric(schedule$week[schedule_rows])
  games <- fbs_transition_games_played(schedule, flags)[schedule_rows]
  fade <- prior_season_feature_weight(weeks, games, config)
  added_sd <- rep(0, nrow(predictions))
  added_sd[involved] <- bridge$margin_sd * fade
  inflated <- sqrt(predictions$margin_sd[involved]^2 + added_sd[involved]^2)
  refreshed <- make_game_picks(
    data.frame(
      expected_margin = predictions$expected_margin[involved],
      fair_margin = -predictions$fair_spread[involved],
      margin_sd = inflated
    ),
    schedule[schedule_rows, , drop = FALSE],
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
    added_sd^2 >= config$bridge$dominance_share * predictions$margin_sd^2
  predictions$confidence_tier[dominated] <- "low"
  review <- dominated &
    predictions$pick_status %in% c("official_pick", "forced_model_pick")
  predictions$pick_status[review] <- "transition_review"
  predictions
}
