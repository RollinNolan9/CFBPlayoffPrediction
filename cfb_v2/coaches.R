validate_coach_assignments <- function(assignments) {
  assert_columns(assignments,
                 c("team", "season", "start_week", "end_week", "coach_id", "coach_name"),
                 "coach_assignments")
  assignments$team <- canonical_team(assignments$team)
  assignments$season <- as.integer(assignments$season)
  assignments$start_week <- as.integer(assignments$start_week)
  assignments$end_week <- as.integer(assignments$end_week)
  if (any(!nzchar(assignments$coach_id) | is.na(assignments$coach_id))) {
    stop("Every coach assignment requires a stable coach_id.", call. = FALSE)
  }
  if (any(assignments$start_week > assignments$end_week, na.rm = TRUE)) {
    stop("Coach assignment start_week cannot be after end_week.", call. = FALSE)
  }
  groups <- split(assignments, paste(assignments$team, assignments$season, sep = "\r"))
  overlap <- vapply(groups, function(x) {
    if (nrow(x) < 2) return(FALSE)
    x <- x[order(x$start_week, x$end_week), ]
    any(x$start_week[-1] <= x$end_week[-nrow(x)])
  }, logical(1))
  if (any(overlap)) {
    stop("Coach assignment intervals overlap for: ",
         paste(gsub("\r", " ", names(overlap)[overlap]), collapse = ", "), call. = FALSE)
  }
  assignments
}

map_coaches_as_of <- function(games, assignments) {
  games <- standardize_schedule(games)
  assignments <- validate_coach_assignments(assignments)
  lookup_week <- if ("coach_lookup_week" %in% names(games)) {
    as.integer(games$coach_lookup_week)
  } else games$week
  lookup_one <- function(team, season, week) {
    hit <- assignments$team == team & assignments$season == season &
      assignments$start_week <= week & assignments$end_week >= week
    if (sum(hit) != 1L) return(c(coach_id = NA_character_, coach_name = NA_character_,
                                interim = NA_character_))
    row <- assignments[which(hit), ][1, ]
    c(coach_id = row$coach_id, coach_name = row$coach_name,
      interim = as.character(if ("interim" %in% names(row)) row$interim else FALSE))
  }
  home <- mapply(lookup_one, games$home, games$season, lookup_week)
  away <- mapply(lookup_one, games$away, games$season, lookup_week)
  games$home_coach_id <- home["coach_id", ]
  games$home_coach_name <- home["coach_name", ]
  games$home_coach_interim <- as.logical(home["interim", ])
  games$away_coach_id <- away["coach_id", ]
  games$away_coach_name <- away["coach_name", ]
  games$away_coach_interim <- as.logical(away["interim", ])
  games
}

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

coach_level_multiplier <- function(level, config) {
  key <- tolower(ifelse(is.na(level), "unknown", as.character(level)))
  values <- unname(config$coach$lower_level_strength[key])
  values[is.na(values)] <- config$coach$lower_level_strength[["unknown"]]
  values
}

coach_context_portability <- function(from_strength, to_strength, config) {
  jump <- pmax(0, as.numeric(to_strength) - as.numeric(from_strength))
  context_factor <- exp(-0.85 * jump)
  pmin(config$coach$portability_cap, pmax(0.20, context_factor))
}

build_coach_ratings <- function(history, target_season, as_of_week,
                                recent_share = 0.65, config,
                                target_context = NULL) {
  assert_columns(
    history,
    c("coach_id", "season", "games", "wins", "above_expectation", "level",
      "context_strength", "playoff_appearances", "titles"),
    "coach_history"
  )
  if (!"week" %in% names(history)) history$week <- 99L
  history$week <- as.integer(history$week)
  if (!recent_share %in% config$coach$recent_share_candidates) {
    stop("recent_share must be one of the configured rolling-validation candidates.",
         call. = FALSE)
  }
  usable <- history$season < target_season |
    (history$season == target_season & history$week < as_of_week)
  h <- history[usable, , drop = FALSE]
  if (!nrow(h)) return(data.frame())
  h$level_weight <- coach_level_multiplier(h$level, config)
  h$age <- pmax(0, target_season - h$season)
  h$recency_weight <- 0.72 ^ h$age
  h$sample_weight <- h$level_weight * h$recency_weight * pmax(1, h$games)
  coaches <- split(h, h$coach_id)

  rows <- lapply(coaches, function(x) {
    prior <- x[x$season < target_season, , drop = FALSE]
    recent <- prior[prior$season >= target_season - 3L, , drop = FALSE]
    recent_value <- weighted_mean_safe(recent$above_expectation, recent$sample_weight)
    season_weight <- prior$level_weight * prior$recency_weight
    weighted_games <- sum(prior$games * season_weight, na.rm = TRUE)
    weighted_wins <- sum(prior$wins * season_weight, na.rm = TRUE)
    historical_win_rate <- (weighted_wins + 3) / (weighted_games + 6)
    historical_value <- (historical_win_rate - 0.5) * 20
    if (!is.finite(recent_value)) recent_value <- historical_value
    if (!is.finite(historical_value)) historical_value <- 0

    experience_by_season <- lapply(split(prior, prior$season), function(y) {
      safe_max <- function(value) if (any(is.finite(value))) max(value, na.rm = TRUE) else 0
      c(playoff = safe_max(y$playoff_appearances), titles = safe_max(y$titles))
    })
    experience_matrix <- if (length(experience_by_season)) {
      do.call(rbind, experience_by_season)
    } else matrix(c(0, 0), nrow = 1, dimnames = list(NULL, c("playoff", "titles")))
    experience_raw <- log1p(sum(experience_matrix[, "playoff"], na.rm = TRUE)) +
      1.5 * log1p(sum(experience_matrix[, "titles"], na.rm = TRUE))
    experience_value <- pmin(config$coach$title_experience_cap,
                             0.02 * experience_raw)
    current <- x[x$season == target_season, , drop = FALSE]
    current_value <- 0
    if (nrow(current)) {
      current_games <- sum(current$games, na.rm = TRUE)
      current_raw <- weighted_mean_safe(current$above_expectation,
                                        pmax(1, current$games))
      shrink <- current_games / (current_games + config$coach$current_season_prior_games)
      current_value <- ifelse(is.finite(current_raw), current_raw * shrink, 0)
    }

    last <- x[order(x$season, if ("week" %in% names(x)) x$week else 0,
                    decreasing = TRUE), , drop = FALSE][1, ]
    from_strength <- as.numeric(last$context_strength)
    to_strength <- from_strength
    if (!is.null(target_context) && x$coach_id[1] %in% names(target_context)) {
      candidate <- as.numeric(target_context[[x$coach_id[1]]])
      if (is.finite(candidate)) to_strength <- candidate
    } else if ("target_context_strength" %in% names(last)) {
      candidate <- as.numeric(last$target_context_strength)
      if (is.finite(candidate)) to_strength <- candidate
    }
    portability <- coach_context_portability(from_strength, to_strength, config)
    core <- recent_share * recent_value + (1 - recent_share) * historical_value
    total <- portability * core + experience_value + current_value
    data.frame(
      coach_id = x$coach_id[1], season = as.integer(target_season),
      as_of_week = as.integer(as_of_week), recent_above_expectation = recent_value,
      historical_win_value = historical_value, experience_value = experience_value,
      portability = portability, current_season_value = current_value,
      rating = total, games_available = sum(x$games, na.rm = TRUE),
      recent_share = recent_share, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

compare_coach_weight_splits <- function(history, target_season, as_of_week, config,
                                        target_context = NULL) {
  ratings <- lapply(config$coach$recent_share_candidates, function(share) {
    out <- build_coach_ratings(
      history, target_season, as_of_week, share, config,
      target_context = target_context
    )
    if (nrow(out)) names(out)[names(out) == "rating"] <- paste0(
      "rating_", as.integer(share * 100), "_", as.integer((1 - share) * 100)
    )
    out
  })
  if (!length(ratings) || !nrow(ratings[[1]])) return(data.frame())
  keep <- c("coach_id", "season", "as_of_week")
  merged <- ratings[[1]]
  for (i in seq_along(ratings)[-1]) {
    rating_column <- grep("^rating_", names(ratings[[i]]), value = TRUE)
    merged <- merge(merged, ratings[[i]][c(keep, rating_column)], by = keep, all = TRUE)
  }
  merged
}
