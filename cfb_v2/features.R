canonical_team <- function(x) {
  x <- trimws(as.character(x))
  aliases <- c(
    "UConn" = "Connecticut", "UCONN" = "Connecticut",
    "UMass" = "Massachusetts", "UMASS" = "Massachusetts",
    "USC" = "Southern California", "Miami (FL)" = "Miami",
    "Miami (Ohio)" = "Miami (OH)", "Ole Miss" = "Mississippi",
    "NC State" = "North Carolina State",
    "San Jose State" = "San José State"
  )
  hit <- match(x, names(aliases))
  x[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  x
}

game_phase <- function(week) {
  week <- as.integer(week)
  ifelse(week <= 1L, "preseason",
         ifelse(week <= 4L, "early_season", "in_season"))
}

preseason_feature_weight <- function(week, config) {
  keys <- as.character(as.integer(week))
  configured <- config$phase$preseason_by_week[keys]
  configured[is.na(configured)] <- 0
  as.numeric(configured)
}

prior_season_feature_weight <- function(week, games_played, config) {
  week <- as.integer(week)
  games_played <- pmax(0, as.numeric(games_played))
  base <- 1 / (1 + games_played / 2.5)
  late <- week >= config$phase$prior_season_cap_after_week
  base[late] <- pmin(base[late], config$phase$prior_season_max_weight)
  pmax(0, pmin(1, base))
}

standardize_schedule <- function(games) {
  required <- c("game_id", "season", "week", "home", "away")
  assert_columns(games, required, "schedule")
  games$game_id <- as.character(games$game_id)
  games$season <- as.integer(games$season)
  games$week <- as.integer(games$week)
  games$home <- canonical_team(games$home)
  games$away <- canonical_team(games$away)
  if (!"neutral_site" %in% names(games)) games$neutral_site <- FALSE
  games$neutral_site[is.na(games$neutral_site)] <- FALSE
  if (!"postseason_type" %in% names(games)) games$postseason_type <- "regular"
  if (!"conference_championship" %in% names(games)) games$conference_championship <- FALSE
  if (!"is_cfp" %in% names(games)) games$is_cfp <- FALSE
  if (!"home_level" %in% names(games)) games$home_level <- "fbs"
  if (!"away_level" %in% names(games)) games$away_level <- "fbs"
  assert_unique_keys(games, "game_id", "schedule")
  games
}

training_game_weights <- function(games, prediction_season, config) {
  games <- standardize_schedule(games)
  season <- games$season
  weights <- config$training$season_decay ^ pmax(0, prediction_season - 1L - season)
  weights[season < config$training$covid_season] <- 0
  weights[season == config$training$covid_season] <-
    weights[season == config$training$covid_season] * config$training$covid_weight

  non_cfp_bowl <- tolower(games$postseason_type) %in% c("bowl", "non_cfp_bowl") &
    !as.logical(games$is_cfp)
  weights[non_cfp_bowl] <- config$training$non_cfp_bowl_weight
  fcs_game <- tolower(games$home_level) != "fbs" | tolower(games$away_level) != "fbs"
  weights[fcs_game] <- weights[fcs_game] * config$training$fcs_rating_weight
  weights[as.logical(games$conference_championship)] <-
    weights[as.logical(games$conference_championship)] *
    config$training$conference_championship_weight
  weights[as.logical(games$is_cfp)] <- weights[as.logical(games$is_cfp)] *
    config$training$cfp_weight
  pmax(0, weights)
}

ats_training_eligible <- function(games) {
  games <- standardize_schedule(games)
  raw_history_eligible(games)
}

raw_history_eligible <- function(games) {
  assert_columns(games, c("home_level", "away_level", "postseason_type", "is_cfp"),
                 "raw history games")
  fbs_only <- !is.na(games$home_level) & !is.na(games$away_level) &
    tolower(as.character(games$home_level)) == "fbs" &
    tolower(as.character(games$away_level)) == "fbs"
  postseason_type <- tolower(ifelse(is.na(games$postseason_type), "",
                                    as.character(games$postseason_type)))
  is_cfp <- !is.na(games$is_cfp) & as.logical(games$is_cfp)
  eligible <- fbs_only & !(postseason_type %in% c("bowl", "non_cfp_bowl") & !is_cfp)
  eligible[is.na(eligible)] <- FALSE
  eligible
}

filter_competitive_plays <- function(plays) {
  if (!nrow(plays)) return(plays)
  description <- if ("play_text" %in% names(plays)) plays$play_text else
    if ("play_text_cfbstats" %in% names(plays)) plays$play_text_cfbstats else ""
  play_type <- if ("play_type" %in% names(plays)) plays$play_type else ""
  clock_kill <- grepl(
    "kneel|spike|clock stop|end of game|end of half",
    paste(play_type, description), ignore.case = TRUE
  )

  if ("garbage_time" %in% names(plays)) {
    garbage <- !is.na(plays$garbage_time) & as.logical(plays$garbage_time)
  } else {
    assert_columns(plays, c("period", "home_score", "away_score"), "plays")
    differential <- abs(as.numeric(plays$home_score) - as.numeric(plays$away_score))
    period <- as.integer(plays$period)
    garbage <- (period == 2L & differential > 38) |
      (period == 3L & differential > 28) |
      (period >= 4L & differential > 22)
  }
  plays[!clock_kill & !garbage, , drop = FALSE]
}

opponent_adjusted_rating <- function(games, value_col = "margin", ridge = 8,
                                     home_field = 2.4, weights = NULL) {
  assert_columns(games, c("home", "away", value_col), "games")
  if (!nrow(games)) return(data.frame(team = character(), rating = numeric()))
  home <- canonical_team(games$home)
  away <- canonical_team(games$away)
  valid_team <- !is.na(home) & !is.na(away) & nzchar(home) & nzchar(away)
  if (is.null(weights)) weights <- rep(1, nrow(games))
  games <- games[valid_team, , drop = FALSE]
  weights <- weights[valid_team]
  home <- home[valid_team]
  away <- away[valid_team]
  if (!nrow(games)) return(data.frame(team = character(), rating = numeric()))
  teams <- sort(unique(c(home, away)))
  if (length(teams) < 2L) {
    return(data.frame(team = teams, rating = rep(0, length(teams)),
                      stringsAsFactors = FALSE))
  }
  n <- nrow(games)
  x <- matrix(0, nrow = n, ncol = length(teams), dimnames = list(NULL, teams))
  x[cbind(seq_len(n), match(home, teams))] <- 1
  x[cbind(seq_len(n), match(away, teams))] <- -1
  y <- as.numeric(games[[value_col]]) - ifelse(
    "neutral_site" %in% names(games) & as.logical(games$neutral_site), 0, home_field
  )
  keep <- is.finite(y) & is.finite(weights) & weights > 0
  if (!any(keep)) {
    return(data.frame(team = teams, rating = rep(0, length(teams)),
                      stringsAsFactors = FALSE))
  }
  x <- x[keep, , drop = FALSE]
  y <- y[keep]
  w <- weights[keep]
  penalty <- diag(ridge, ncol(x))
  centering <- matrix(1, nrow = 1, ncol = ncol(x))
  x_aug <- rbind(x * sqrt(w), centering * sqrt(1000))
  y_aug <- c(y * sqrt(w), 0)
  beta <- solve(crossprod(x_aug) + penalty, crossprod(x_aug, y_aug))
  data.frame(team = teams, rating = as.numeric(beta), stringsAsFactors = FALSE)
}

regress_unstable_rate <- function(observed_rate, opportunities, league_rate,
                                  prior_opportunities = 100) {
  opportunities <- pmax(0, as.numeric(opportunities))
  weight <- opportunities / (opportunities + prior_opportunities)
  weight * as.numeric(observed_rate) + (1 - weight) * as.numeric(league_rate)
}

shrink_special_teams_epa <- function(observed_epa, plays, league_epa = 0,
                                     prior_plays = 120) {
  regress_unstable_rate(observed_epa, plays, league_epa, prior_plays)
}

recency_summary <- function(team_games, as_of_date = NULL) {
  assert_columns(team_games, c("team", "season", "week", "game_date", "value"),
                 "team_games")
  if (!is.null(as_of_date)) team_games <- team_games[team_games$game_date < as_of_date, ]
  if (!nrow(team_games)) return(team_games[FALSE, c("team", "season", "week")])
  team_games <- team_games[order(team_games$team, team_games$game_date,
                                 team_games$week), , drop = FALSE]
  split_games <- split(team_games, team_games$team)
  rows <- lapply(split_games, function(x) {
    current <- max(x$season)
    current_values <- x$value[x$season == current]
    prior_values <- x$value[x$season == current - 1L]
    trailing_values <- x$value[x$season >= current - 3L & x$season < current]
    data.frame(
      team = x$team[1], season = current,
      week = if (length(current_values)) max(x$week[x$season == current]) + 1L else 0L,
      recent_3 = if (length(current_values)) mean(tail(current_values, 3), na.rm = TRUE) else NA,
      recent_6 = if (length(current_values)) mean(tail(current_values, 6), na.rm = TRUE) else NA,
      season_to_date = if (length(current_values)) mean(current_values, na.rm = TRUE) else NA,
      prior_season = if (length(prior_values)) mean(prior_values, na.rm = TRUE) else NA,
      trailing_3yr = if (length(trailing_values)) mean(trailing_values, na.rm = TRUE) else NA,
      games_played = length(current_values), stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

blend_team_form <- function(features, config) {
  assert_columns(features, c("week", "games_played", "recent_3", "recent_6",
                             "season_to_date", "prior_season", "trailing_3yr",
                             "preseason_prior"), "features")
  preseason_w <- preseason_feature_weight(features$week, config)
  prior_w <- prior_season_feature_weight(features$week, features$games_played, config)

  current <- rowMeans(cbind(features$recent_3, features$recent_6,
                            features$season_to_date), na.rm = TRUE)
  current[!is.finite(current)] <- NA_real_
  history <- ifelse(is.finite(features$prior_season), features$prior_season,
                    features$trailing_3yr)
  preseason <- ifelse(is.finite(features$preseason_prior), features$preseason_prior,
                      history)
  current <- ifelse(is.finite(current), current, preseason)
  history <- ifelse(is.finite(history), history, 0)
  preseason <- ifelse(is.finite(preseason), preseason, history)

  current_history <- (1 - prior_w) * current + prior_w * history
  (1 - preseason_w) * current_history + preseason_w * preseason
}

resolve_home_field <- function(neutral_site, default_hfa = 2.4) {
  ifelse(as.logical(neutral_site), 0, default_hfa)
}

make_matchup_features <- function(schedule, home_features, away_features,
                                  coach_recent_share = 0.65, config) {
  schedule <- standardize_schedule(schedule)
  home_features$team <- canonical_team(home_features$team)
  away_features$team <- canonical_team(away_features$team)
  home <- merge(schedule, home_features, by.x = c("home", "season", "week"),
                by.y = c("team", "season", "week"), all.x = TRUE, sort = FALSE)
  names(home)[names(home) %in% setdiff(names(home_features), c("team", "season", "week"))] <-
    paste0("home_", names(home)[names(home) %in%
      setdiff(names(home_features), c("team", "season", "week"))])
  joined <- merge(home, away_features, by.x = c("away", "season", "week"),
                  by.y = c("team", "season", "week"), all.x = TRUE, sort = FALSE)
  away_cols <- intersect(names(joined), setdiff(names(away_features), c("team", "season", "week")))
  names(joined)[match(away_cols, names(joined))] <- paste0("away_", away_cols)

  joined <- joined[match(schedule$game_id, joined$game_id), , drop = FALSE]
  numeric_base <- intersect(
    c("offense_rating", "defense_rating", "special_teams_rating", "recent_3",
      "recent_6", "season_to_date", "prior_season", "trailing_3yr",
      "preseason_prior", "qb_continuity", "roster_continuity", "staff_continuity",
      "coach_rating", "source_games", "games_played",
      "blended_team_form"),
    gsub("^(home_|away_)", "", names(joined))
  )
  for (feature in numeric_base) {
    h <- paste0("home_", feature)
    a <- paste0("away_", feature)
    if (all(c(h, a) %in% names(joined))) joined[[paste0(feature, "_diff")]] <- joined[[h]] - joined[[a]]
  }
  joined$game_phase <- game_phase(joined$week)
  joined$preseason_weight <- preseason_feature_weight(joined$week, config)
  home_games <- if ("home_games_played" %in% names(joined)) joined$home_games_played else 0
  away_games <- if ("away_games_played" %in% names(joined)) joined$away_games_played else 0
  joined$prior_history_weight <- rowMeans(cbind(
    prior_season_feature_weight(joined$week, home_games, config),
    prior_season_feature_weight(joined$week, away_games, config)
  ))
  for (feature in intersect(
    c("preseason_prior_diff", "qb_continuity_diff", "roster_continuity_diff",
      "staff_continuity_diff"), names(joined)
  )) {
    joined[[feature]] <- joined[[feature]] * joined$preseason_weight
  }
  for (feature in intersect(c("prior_season_diff", "trailing_3yr_diff"), names(joined))) {
    joined[[feature]] <- joined[[feature]] * joined$prior_history_weight
  }
  team_hfa <- if ("home_home_field_rating" %in% names(joined)) {
    joined$home_home_field_rating
  } else rep(2.4, nrow(joined))
  team_hfa[!is.finite(team_hfa)] <- 2.4
  joined$challenger_team_home_field_points <- ifelse(
    as.logical(joined$neutral_site), 0, team_hfa
  )
  joined$home_field_points <- resolve_home_field(joined$neutral_site)
  joined$coach_recent_share <- coach_recent_share
  joined
}

football_feature_names <- function(data) {
  prohibited <- c(
    "home", "away", "conference", "home_conference", "away_conference",
    "market_home_spread", "opening_spread", "closing_spread", "total",
    "ap_rank_diff", "cfp_rank_diff", "fpi", "pff", "public_rating",
    "pregame_power_diff", "home_field_rating_diff"
  )
  candidates <- names(data)[vapply(data, is.numeric, logical(1))]
  candidates <- setdiff(candidates, c(prohibited, "home_score", "away_score",
                                      "margin", "total_points", "season", "week",
                                      "model_week", "phase_week"))
  candidates <- candidates[
    grepl("_diff$", candidates) | candidates == "home_field_points"
  ]
  candidates[!grepl(
    "spread|line|price|rank|fpi|pff|market|^challenger_|pregame_elo",
    candidates, ignore.case = TRUE
  )]
}

build_context_challengers <- function(schedule, team_context, venue_context) {
  schedule <- standardize_schedule(schedule)
  assert_columns(team_context, c("team", "timezone", "last_game_date"), "team_context")
  assert_columns(venue_context, c("venue", "timezone", "latitude", "longitude"),
                 "venue_context")
  home_context <- merge(schedule[c("game_id", "home", "kickoff", "venue")], team_context,
                        by.x = "home", by.y = "team", all.x = TRUE)
  away_context <- merge(schedule[c("game_id", "away")], team_context,
                        by.x = "away", by.y = "team", all.x = TRUE)
  names(home_context)[names(home_context) == "timezone"] <- "home_timezone"
  names(home_context)[names(home_context) == "last_game_date"] <- "home_last_game_date"
  names(away_context)[names(away_context) == "timezone"] <- "away_timezone"
  names(away_context)[names(away_context) == "last_game_date"] <- "away_last_game_date"
  out <- merge(home_context, away_context, by = "game_id", all.x = TRUE)
  out <- merge(out, venue_context, by = "venue", all.x = TRUE)
  timezone_number <- function(x) suppressWarnings(as.numeric(gsub("UTC", "", x)))
  venue_tz <- timezone_number(out$timezone)
  out$challenger_home_timezone_change <- venue_tz - timezone_number(out$home_timezone)
  out$challenger_away_timezone_change <- venue_tz - timezone_number(out$away_timezone)
  out$challenger_away_eastward_3plus <- out$challenger_away_timezone_change >= 3
  out$challenger_home_rest_days <- as.numeric(as.Date(out$kickoff) - as.Date(out$home_last_game_date))
  out$challenger_away_rest_days <- as.numeric(as.Date(out$kickoff) - as.Date(out$away_last_game_date))
  out
}

build_weather_challengers <- function(weather) {
  assert_columns(weather, c("game_id", "temperature", "wind_speed", "precipitation"),
                 "weather")
  data.frame(
    game_id = weather$game_id,
    challenger_temperature = as.numeric(weather$temperature),
    challenger_wind_speed = as.numeric(weather$wind_speed),
    challenger_precipitation = as.numeric(weather$precipitation),
    stringsAsFactors = FALSE
  )
}

weekly_game_eligibility <- function(schedule, membership, rankings = NULL,
                                    cfp_probabilities = NULL, config) {
  schedule <- standardize_schedule(schedule)
  assert_columns(membership, c("team", "season", "power_conference"), "membership")
  membership$team <- canonical_team(membership$team)
  key <- paste(membership$team, membership$season)
  power <- setNames(as.logical(membership$power_conference), key)
  home_power <- power[paste(schedule$home, schedule$season)]
  away_power <- power[paste(schedule$away, schedule$season)]
  include <- (!is.na(home_power) & home_power) | (!is.na(away_power) & away_power)
  include <- include | schedule$home %in% canonical_team(config$eligibility$independent_always) |
    schedule$away %in% canonical_team(config$eligibility$independent_always)

  if (!is.null(rankings) && nrow(rankings)) {
    assert_columns(rankings, c("team", "season", "week", "rank"), "rankings")
    usable_rankings <- rankings$season == schedule$season[1] &
      rankings$week <= max(schedule$week) & is.finite(rankings$rank)
    if ("ranking_type" %in% names(rankings)) {
      type <- tolower(rankings$ranking_type)
      has_cfp <- any(usable_rankings & type %in% c("cfp", "playoff"))
      chosen_type <- if (has_cfp) c("cfp", "playoff") else c("ap", "ap25")
      usable_rankings <- usable_rankings & type %in% chosen_type
    }
    ranked <- unique(canonical_team(rankings$team[usable_rankings]))
    include <- include | schedule$home %in% ranked | schedule$away %in% ranked
  }
  if (!is.null(cfp_probabilities) && nrow(cfp_probabilities)) {
    assert_columns(cfp_probabilities,
                   c("team", "season", "cfp_probability", "conference_leader"),
                   "cfp_probabilities")
    contenders <- canonical_team(cfp_probabilities$team[
      cfp_probabilities$season == schedule$season[1] &
        (cfp_probabilities$cfp_probability >= config$eligibility$cfp_probability_min |
           as.logical(cfp_probabilities$conference_leader))
    ])
    include <- include | schedule$home %in% contenders | schedule$away %in% contenders
  }
  as.logical(include)
}

validate_pregame_features <- function(data, kickoff_col = "kickoff") {
  future_columns <- grep("final|postgame|closing", names(data), value = TRUE, ignore.case = TRUE)
  if (length(future_columns)) {
    stop("Pregame feature table contains prohibited future-looking columns: ",
         paste(future_columns, collapse = ", "), call. = FALSE)
  }
  if (kickoff_col %in% names(data) && "as_of" %in% names(data)) {
    invalid <- !is.na(data[[kickoff_col]]) & !is.na(data$as_of) & data$as_of >= data[[kickoff_col]]
    if (any(invalid)) stop("Feature timestamps must be strictly before kickoff.", call. = FALSE)
  }
  invisible(TRUE)
}

build_injury_scenarios <- function(injuries, base_margin, home_team, away_team, config) {
  if (is.null(injuries) || !nrow(injuries)) {
    return(data.frame(scenario = "most_likely", margin_adjustment = 0,
                      expected_margin = base_margin, probability = 1,
                      conflict_flag = FALSE, stringsAsFactors = FALSE))
  }
  assert_columns(injuries, c("team", "status", "impact_points", "confidence"), "injuries")
  if ("source_type" %in% names(injuries)) {
    untrusted <- !tolower(injuries$source_type) %in% config$injuries$trusted_source_types
    if (any(untrusted, na.rm = TRUE)) {
      stop("Injury adjustment contains a source outside the approved hierarchy.", call. = FALSE)
    }
  }
  injuries$team <- canonical_team(injuries$team)
  position <- toupper(if ("position" %in% names(injuries)) injuries$position else "")
  starter <- if ("starter" %in% names(injuries)) as.logical(injuries$starter) else FALSE
  usage <- if ("usage_share" %in% names(injuries)) as.numeric(injuries$usage_share) else 0
  high_havoc <- if ("high_havoc" %in% names(injuries)) as.logical(injuries$high_havoc) else FALSE
  relevant <- (starter & position %in% c("QB", "K", "OL", "OT", "OG", "C")) |
    (position %in% c("RB", "WR", "TE") & usage >= config$injuries$skill_opportunity_min) |
    (position %in% c("DL", "DE", "DT", "LB", "CB", "S", "DB") &
       (usage >= config$injuries$defender_snap_share_min | high_havoc))
  injuries <- injuries[relevant | tolower(injuries$status) %in% c("out", "inactive", "suspended"), ]
  if (!nrow(injuries)) {
    return(data.frame(scenario = "most_likely", margin_adjustment = 0,
                      expected_margin = base_margin, probability = 1,
                      conflict_flag = FALSE, stringsAsFactors = FALSE))
  }
  position <- toupper(if ("position" %in% names(injuries)) injuries$position else "")
  starter <- if ("starter" %in% names(injuries)) as.logical(injuries$starter) else FALSE
  injuries$sign <- ifelse(injuries$team == canonical_team(home_team), -1,
                          ifelse(injuries$team == canonical_team(away_team), 1, 0))
  questionable <- tolower(injuries$status) %in% c("questionable", "game-time decision", "doubtful")
  definite_out <- tolower(injuries$status) %in% c("out", "inactive", "suspended")
  reliability <- pmax(0, pmin(1, injuries$confidence))
  effective_impact <- injuries$impact_points * reliability
  status_availability <- ifelse(
    tolower(injuries$status) == "doubtful", .25,
    ifelse(tolower(injuries$status) %in% c("questionable", "game-time decision"), .50, 0)
  )
  availability <- if ("availability_probability" %in% names(injuries)) {
    value <- as.numeric(injuries$availability_probability)
    value[!is.finite(value)] <- status_availability[!is.finite(value)]
    pmax(0, pmin(1, value))
  } else status_availability
  base_adjustment <- sum(injuries$sign[definite_out] * effective_impact[definite_out],
                         na.rm = TRUE)
  q_impact <- injuries$sign[questionable] * effective_impact[questionable]
  q_out_probability <- 1 - availability[questionable]
  likely_adjustment <- base_adjustment + sum(q_impact * q_out_probability, na.rm = TRUE)
  scenarios <- data.frame(
    scenario = c("most_likely", "all_key_available", "all_key_unavailable"),
    margin_adjustment = c(likely_adjustment, base_adjustment,
                          base_adjustment + sum(q_impact, na.rm = TRUE)),
    probability = c(NA_real_, NA_real_, NA_real_), stringsAsFactors = FALSE
  )
  qb_q <- questionable & position == "QB" & starter
  if (any(qb_q, na.rm = TRUE)) {
    qb_adjustment <- base_adjustment + sum(
      injuries$sign[qb_q] * effective_impact[qb_q], na.rm = TRUE
    )
    scenarios <- rbind(scenarios, data.frame(
      scenario = "starting_qb_unavailable", margin_adjustment = qb_adjustment,
      probability = NA_real_, stringsAsFactors = FALSE
    ))
  }
  scenarios <- head(scenarios, config$injuries$max_scenarios)
  scenarios$expected_margin <- base_margin + scenarios$margin_adjustment
  scenarios$conflict_flag <- any(
    if ("conflict_flag" %in% names(injuries)) as.logical(injuries$conflict_flag) else FALSE,
    na.rm = TRUE
  )
  scenarios
}
