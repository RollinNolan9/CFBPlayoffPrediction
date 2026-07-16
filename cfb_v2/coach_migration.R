normalize_person_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("[[:space:]]+", " ", trimws(x))
  vapply(strsplit(x, " ", fixed = TRUE), function(tokens) {
    if (!length(tokens)) return("")
    output <- character()
    i <- 1L
    while (i <= length(tokens)) {
      if (nchar(tokens[i]) == 1L) {
        j <- i
        while (j <= length(tokens) && nchar(tokens[j]) == 1L) j <- j + 1L
        output <- c(output, paste(tokens[i:(j - 1L)], collapse = ""))
        i <- j
      } else {
        output <- c(output, tokens[i])
        i <- i + 1L
      }
    }
    paste(output, collapse = " ")
  }, character(1))
}

coach_id_from_name <- function(x) {
  key <- normalize_person_name(x)
  paste0("coach_", gsub(" ", "_", key, fixed = TRUE))
}

read_coach_aliases <- function(path = NULL) {
  if (is.null(path) || !file.exists(path)) {
    return(data.frame(source_name = character(), canonical_name = character(),
                      coach_id = character(), notes = character(),
                      stringsAsFactors = FALSE))
  }
  aliases <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  assert_columns(aliases, c("source_name", "canonical_name"), "coach_aliases")
  if (!"coach_id" %in% names(aliases)) aliases$coach_id <- ""
  if (!"notes" %in% names(aliases)) aliases$notes <- ""
  aliases
}

coach_source_column <- function(data, candidates, required = TRUE, default = NA) {
  hit <- intersect(candidates, names(data))
  if (length(hit)) return(data[[hit[1]]])
  if (required) {
    stop("Coach source is missing one of: ", paste(candidates, collapse = ", "),
         call. = FALSE)
  }
  rep(default, nrow(data))
}

standardize_coach_seasons <- function(raw, aliases_path = NULL,
                                      data_source = "legacy_csv") {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = TRUE)
  source_name <- coach_source_column(
    raw, c("FullName", "full_name", "fullname"), required = FALSE, default = ""
  )
  missing_name <- is.na(source_name) | !nzchar(trimws(source_name))
  if (any(missing_name)) {
    first <- coach_source_column(raw, c("first_name", "first"), required = FALSE,
                                 default = "")
    last <- coach_source_column(raw, c("last_name", "last"), required = FALSE,
                                default = "")
    source_name[missing_name] <- trimws(paste(first[missing_name], last[missing_name]))
  }
  standardized <- data.frame(
    source_name = source_name,
    team = coach_source_column(raw, c("school", "team")),
    season = coach_source_column(raw, c("year", "season")),
    games = coach_source_column(raw, "games"),
    wins = coach_source_column(raw, "wins"),
    srs = coach_source_column(raw, c("srs", "simple_rating_system"),
                              required = FALSE),
    hire_date = coach_source_column(raw, "hire_date", required = FALSE),
    data_source = data_source, stringsAsFactors = FALSE
  )
  raw <- standardized
  raw$source_name <- trimws(raw$source_name)
  raw$team <- canonical_team(raw$team)
  raw$season <- as.integer(raw$season)
  raw$games <- as.integer(raw$games)
  raw$wins <- as.numeric(raw$wins)
  raw$srs <- as.numeric(raw$srs)
  raw$hire_date <- as.Date(substr(as.character(raw$hire_date), 1, 10))
  raw <- raw[nzchar(raw$source_name) & nzchar(raw$team) & is.finite(raw$season), ]
  season_has_counts <- tapply(raw$games, raw$season, function(x) {
    any(is.finite(x) & x > 0)
  })
  raw$counts_available <- as.logical(season_has_counts[as.character(raw$season)])

  aliases <- read_coach_aliases(aliases_path)
  alias_key <- normalize_person_name(aliases$source_name)
  source_key <- normalize_person_name(raw$source_name)
  alias_match <- match(source_key, alias_key)
  raw$canonical_name <- raw$source_name
  has_alias <- !is.na(alias_match)
  raw$canonical_name[has_alias] <- aliases$canonical_name[alias_match[has_alias]]
  raw$coach_id <- coach_id_from_name(raw$canonical_name)
  explicit_id <- has_alias & nzchar(aliases$coach_id[alias_match])
  raw$coach_id[explicit_id] <- aliases$coach_id[alias_match[explicit_id]]

  exact_key <- paste(raw$coach_id, raw$team, raw$season, sep = "\r")
  if (anyDuplicated(exact_key)) {
    raw <- do.call(rbind, lapply(split(raw, exact_key), function(x) {
      x <- x[order(x$games, x$wins, decreasing = TRUE), , drop = FALSE]
      x[1, , drop = FALSE]
    }))
    rownames(raw) <- NULL
  }
  raw
}

read_legacy_coach_seasons <- function(path, aliases_path = NULL) {
  if (!file.exists(path)) stop("Legacy coach source does not exist: ", path, call. = FALSE)
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
  standardize_coach_seasons(raw, aliases_path, data_source = "legacy_coach_csv")
}

build_coach_identity_table <- function(coach_seasons) {
  assert_columns(coach_seasons, c("coach_id", "canonical_name", "source_name"),
                 "coach_seasons")
  groups <- split(coach_seasons, coach_seasons$coach_id)
  rows <- lapply(groups, function(x) {
    simultaneous <- any(vapply(split(x$team, x$season), function(team) {
      length(unique(team)) > 1L
    }, logical(1)))
    data.frame(
      coach_id = x$coach_id[1], canonical_name = x$canonical_name[1],
      normalization_key = normalize_person_name(x$canonical_name[1]),
      source_names = paste(sort(unique(x$source_name)), collapse = "|"),
      first_season = min(x$season), last_season = max(x$season),
      identity_collision = simultaneous,
      needs_review = simultaneous,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$canonical_name), ]
}

team_season_game_rows <- function(games) {
  assert_columns(games, c("game_id", "season", "week", "kickoff", "home", "away"),
                 "historical games")
  assignment_week <- if ("coach_lookup_week" %in% names(games)) {
    games$coach_lookup_week
  } else games$week
  home_level <- if ("home_level" %in% names(games)) games$home_level else "unknown"
  away_level <- if ("away_level" %in% names(games)) games$away_level else "unknown"
  home <- data.frame(
    game_id = games$game_id, season = games$season, week = assignment_week,
    kickoff = games$kickoff, team = canonical_team(games$home),
    opponent = canonical_team(games$away), team_level = home_level,
    opponent_level = away_level, site = "home", stringsAsFactors = FALSE
  )
  away <- data.frame(
    game_id = games$game_id, season = games$season, week = assignment_week,
    kickoff = games$kickoff, team = canonical_team(games$away),
    opponent = canonical_team(games$home), team_level = away_level,
    opponent_level = home_level, site = "away", stringsAsFactors = FALSE
  )
  out <- rbind(home, away)
  out[order(out$team, out$season, out$kickoff, out$game_id), ]
}

coach_order_for_team_season <- function(rows, all_coach_seasons) {
  if (nrow(rows) <= 1L) return(rows)
  team <- rows$team[1]
  season <- rows$season[1]
  previous <- all_coach_seasons$coach_id[
    all_coach_seasons$team == team & all_coach_seasons$season == season - 1L
  ]
  following <- all_coach_seasons$coach_id[
    all_coach_seasons$team == team & all_coach_seasons$season == season + 1L
  ]
  score <- rep(0, nrow(rows))
  score[rows$coach_id %in% previous] <- score[rows$coach_id %in% previous] - 100
  score[rows$coach_id %in% following] <- score[rows$coach_id %in% following] + 100
  score <- score - rows$games / pmax(1, max(rows$games))
  rows[order(score, -rows$games, rows$canonical_name), , drop = FALSE]
}

infer_coach_assignments <- function(coach_seasons, games, overrides = NULL) {
  team_games <- team_season_game_rows(games)
  coach_groups <- split(coach_seasons,
                        paste(coach_seasons$team, coach_seasons$season, sep = "\r"))
  schedule_groups <- split(team_games, paste(team_games$team, team_games$season, sep = "\r"))
  all_keys <- sort(unique(c(names(coach_groups), names(schedule_groups))))
  assignments <- list()
  qa <- list()
  a <- 1L
  q <- 1L

  for (key in all_keys) {
    coaches <- coach_groups[[key]]
    scheduled <- schedule_groups[[key]]
    if (is.null(coaches) || !nrow(coaches)) {
      parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
      source_severity <- if (!is.null(scheduled) && nrow(scheduled) &&
                               all(!is.na(scheduled$team_level) &
                                     scheduled$team_level == "fcs")) {
        "info"
      } else "error"
      qa[[q]] <- data.frame(
        team = parts[1], season = as.integer(parts[2]), issue = "missing_coach_source",
        detail = paste0("schedule_games=", if (is.null(scheduled)) 0 else nrow(scheduled)),
        severity = source_severity, stringsAsFactors = FALSE
      )
      q <- q + 1L
      next
    }
    if (is.null(scheduled)) scheduled <- team_games[FALSE, ]
    coaches <- coach_order_for_team_season(coaches, coach_seasons)
    scheduled <- scheduled[order(scheduled$kickoff, scheduled$game_id), , drop = FALSE]
    source_total <- sum(coaches$games, na.rm = TRUE)
    schedule_total <- nrow(scheduled)
    count_available <- if ("counts_available" %in% names(coaches)) {
      all(as.logical(coaches$counts_available))
    } else TRUE
    snapshot_single <- nrow(coaches) == 1L && !count_available
    complete_count <- source_total == schedule_total || snapshot_single
    mapping_ambiguous <- nrow(coaches) > 1L && !complete_count
    count_position <- 1L
    previous_end <- -1L

    for (i in seq_len(nrow(coaches))) {
      n_source <- if (snapshot_single) schedule_total else
        max(0L, as.integer(coaches$games[i]), na.rm = TRUE)
      if (nrow(coaches) == 1L) {
        game_index <- seq_len(schedule_total)
      } else if (i == nrow(coaches)) {
        game_index <- if (count_position <= schedule_total) {
          seq.int(count_position, schedule_total)
        } else integer()
      } else {
        end_position <- min(schedule_total, count_position + n_source - 1L)
        game_index <- if (count_position <= end_position) {
          seq.int(count_position, end_position)
        } else integer()
      }
      assigned_games <- if (length(game_index) && schedule_total) {
        scheduled[game_index, , drop = FALSE]
      } else scheduled[FALSE, ]
      inferred_start <- if (i == 1L) 0L else if (nrow(assigned_games)) {
        min(assigned_games$week, na.rm = TRUE)
      } else previous_end + 1L
      inferred_end <- if (i == nrow(coaches)) 99L else if (nrow(assigned_games)) {
        max(assigned_games$week, na.rm = TRUE)
      } else inferred_start
      if (inferred_start <= previous_end) inferred_start <- previous_end + 1L
      if (inferred_end < inferred_start) inferred_end <- inferred_start
      continuity_next <- any(
        coach_seasons$coach_id == coaches$coach_id[i] &
          coach_seasons$team == coaches$team[i] &
          coach_seasons$season == coaches$season[i] + 1L
      )
      interim <- nrow(coaches) > 1L && !continuity_next &&
        n_source <= max(4L, floor(schedule_total / 2))
      confidence <- if (snapshot_single) "medium" else
        if (nrow(coaches) == 1L && complete_count) "high" else
        if (nrow(coaches) == 1L) "medium" else
        if (complete_count) "medium" else "low"
      reason <- if (snapshot_single) {
        "single_cfbd_snapshot_without_game_counts"
      } else if (nrow(coaches) == 1L) {
        if (complete_count) "single_source_coach" else
          "single_source_coach_count_mismatch"
      } else if (complete_count) {
        "ordered_by_continuity_then_source_games"
      } else "count_mismatch_remainder_assigned_to_last_coach"
      assignments[[a]] <- data.frame(
        team = coaches$team[i], season = coaches$season[i],
        start_week = inferred_start, end_week = inferred_end,
        coach_id = coaches$coach_id[i], coach_name = coaches$canonical_name[i],
        interim = interim,
        source = if ("data_source" %in% names(coaches)) {
          coaches$data_source[i]
        } else "coach_season_source",
        assignment_confidence = confidence,
        source_games = if (snapshot_single) NA_integer_ else n_source,
        schedule_games = schedule_total, needs_review = mapping_ambiguous,
        inference_reason = reason, effective_date = NA_character_,
        source_url = NA_character_, stringsAsFactors = FALSE
      )
      a <- a + 1L
      previous_end <- inferred_end
      count_position <- count_position + n_source
    }
    if (!complete_count) {
      qa[[q]] <- data.frame(
        team = coaches$team[1], season = coaches$season[1],
        issue = "coach_schedule_game_count_mismatch",
        detail = paste0("coach_games=", source_total, ";schedule_games=", schedule_total),
        severity = if (nrow(coaches) == 1L) "info" else
          if (coaches$season[1] >= max(coach_seasons$season)) "error" else "warning",
        stringsAsFactors = FALSE
      )
      q <- q + 1L
    }
  }
  out <- if (length(assignments)) do.call(rbind, assignments) else data.frame()

  replace_keys <- character()
  if (!is.null(overrides) && nrow(overrides)) {
    assert_columns(overrides,
                   c("team", "season", "start_week", "end_week", "coach_id", "coach_name"),
                   "coach_assignment_overrides")
    overrides$team <- canonical_team(overrides$team)
    replace_keys <- unique(paste(overrides$team, overrides$season, sep = "\r"))
    out <- out[!paste(out$team, out$season, sep = "\r") %in% replace_keys, ]
    defaults <- list(interim = FALSE, source = "manual_override",
                     assignment_confidence = "confirmed", source_games = NA_integer_,
                     schedule_games = NA_integer_, needs_review = FALSE,
                     inference_reason = "manual_override",
                     effective_date = NA_character_, source_url = NA_character_)
    for (column in setdiff(names(defaults), names(overrides))) {
      overrides[[column]] <- defaults[[column]]
    }
    out <- rbind(out, overrides[names(out)])
  }
  out <- out[order(out$season, out$team, out$start_week), , drop = FALSE]
  rownames(out) <- NULL
  validated <- validate_coach_assignments(out)
  qa_out <- if (length(qa)) do.call(rbind, qa) else data.frame()
  if (length(replace_keys) && nrow(qa_out)) {
    qa_key <- paste(qa_out$team, qa_out$season, sep = "\r")
    qa_out <- qa_out[!qa_key %in% replace_keys, , drop = FALSE]
  }
  list(assignments = validated,
       qa = qa_out)
}

public_coach_identity <- function(name, team, season, coach_seasons = NULL) {
  fallback <- list(
    coach_id = coach_id_from_name(name),
    coach_name = trimws(as.character(name))
  )
  if (is.null(coach_seasons) || !nrow(coach_seasons)) return(fallback)
  assert_columns(
    coach_seasons,
    c("coach_id", "canonical_name", "source_name", "team", "season"),
    "coach seasons used to resolve public transitions"
  )
  key <- normalize_person_name(name)
  source_keys <- normalize_person_name(coach_seasons$source_name)
  canonical_keys <- normalize_person_name(coach_seasons$canonical_name)
  exact <- source_keys == key | canonical_keys == key
  exact_ids <- unique(coach_seasons$coach_id[exact])
  if (length(exact_ids) == 1L) {
    row <- coach_seasons[which(exact & coach_seasons$coach_id == exact_ids[1])[1], ]
    return(list(coach_id = row$coach_id, coach_name = row$canonical_name))
  }

  name_parts <- strsplit(key, " ", fixed = TRUE)[[1]]
  same_team_season <- coach_seasons$team == canonical_team(team) &
    coach_seasons$season == as.integer(season)
  compatible <- vapply(canonical_keys, function(candidate) {
    parts <- strsplit(candidate, " ", fixed = TRUE)[[1]]
    length(name_parts) >= 2L && length(parts) >= 2L &&
      name_parts[1] == parts[1] &&
      name_parts[length(name_parts)] == parts[length(parts)]
  }, logical(1))
  compatible_ids <- unique(coach_seasons$coach_id[same_team_season & compatible])
  if (length(compatible_ids) == 1L) {
    hit <- same_team_season & compatible &
      coach_seasons$coach_id == compatible_ids[1]
    row <- coach_seasons[which(hit)[1], ]
    return(list(coach_id = row$coach_id, coach_name = row$canonical_name))
  }
  fallback
}

build_public_transition_overrides <- function(transitions, games,
                                              coach_seasons = NULL) {
  if (is.null(transitions) || !nrow(transitions)) return(data.frame())
  assert_columns(
    transitions,
    c("team", "season", "outgoing_coach", "effective_date",
      "replacement_coach", "interim", "source_url"),
    "public coach transitions"
  )
  team_games <- team_season_game_rows(games)
  transition_groups <- split(
    transitions,
    paste(canonical_team(transitions$team), transitions$season, sep = "\r")
  )
  schedule_groups <- split(
    team_games, paste(team_games$team, team_games$season, sep = "\r")
  )
  rows <- list()
  k <- 1L
  for (key in names(transition_groups)) {
    changes <- transition_groups[[key]]
    scheduled <- schedule_groups[[key]]
    if (is.null(scheduled) || !nrow(scheduled)) next
    scheduled <- scheduled[order(scheduled$kickoff, scheduled$game_id), , drop = FALSE]
    changes$effective_date <- as.Date(changes$effective_date)
    changes <- changes[order(changes$effective_date), , drop = FALSE]
    valid_changes <- list()
    v <- 1L
    for (i in seq_len(nrow(changes))) {
      game_date <- as.Date(scheduled$kickoff, tz = "UTC")
      after <- which(game_date > changes$effective_date[i])
      if (!length(after)) next
      changes$start_week[i] <- scheduled$week[min(after)]
      valid_changes[[v]] <- changes[i, , drop = FALSE]
      v <- v + 1L
    }
    if (!length(valid_changes)) next
    changes <- do.call(rbind, valid_changes)
    changes <- changes[!duplicated(changes$start_week, fromLast = TRUE), , drop = FALSE]
    coach_names <- c(changes$outgoing_coach[1], changes$replacement_coach)
    starts <- c(0L, as.integer(changes$start_week))
    interims <- c(FALSE, as.logical(changes$interim))
    dates <- c(NA_character_, as.character(changes$effective_date))
    urls <- c(changes$source_url[1], changes$source_url)
    ends <- c(starts[-1] - 1L, 99L)
    for (i in seq_along(coach_names)) {
      assigned <- scheduled$week >= starts[i] & scheduled$week <= ends[i]
      identity <- public_coach_identity(
        coach_names[i], scheduled$team[1], scheduled$season[1], coach_seasons
      )
      rows[[k]] <- data.frame(
        team = scheduled$team[1], season = scheduled$season[1],
        start_week = starts[i], end_week = ends[i],
        coach_id = identity$coach_id,
        coach_name = identity$coach_name, interim = interims[i],
        source = "wikipedia_season_coaching_changes",
        assignment_confidence = "confirmed_public_transition",
        source_games = sum(assigned), schedule_games = nrow(scheduled),
        needs_review = FALSE,
        inference_reason = "public_transition_effective_next_game",
        effective_date = dates[i], source_url = urls[i],
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out <- out[order(out$season, out$team, out$start_week), , drop = FALSE]
  rownames(out) <- NULL
  validate_coach_assignments(out)
}

legacy_coach_prior_history <- function(coach_seasons, modern_start = 2020L) {
  x <- coach_seasons[order(coach_seasons$team, coach_seasons$season), , drop = FALSE]
  groups <- split(x, x$team)
  x$independent_expectation <- NA_real_
  for (group in groups) {
    index <- match(paste(group$coach_id, group$team, group$season, sep = "\r"),
                   paste(x$coach_id, x$team, x$season, sep = "\r"))
    for (i in seq_len(nrow(group))) {
      prior <- group$srs[group$season < group$season[i] &
                           group$season >= group$season[i] - 3L]
      x$independent_expectation[index[i]] <- if (any(is.finite(prior))) {
        mean(prior, na.rm = TRUE)
      } else 0
    }
  }
  x$above_expectation <- x$srs - x$independent_expectation
  x$above_expectation[!is.finite(x$above_expectation)] <- 0
  scale_context <- stats::sd(x$srs, na.rm = TRUE)
  if (!is.finite(scale_context) || scale_context == 0) scale_context <- 10
  data.frame(
    coach_id = x$coach_id, team = x$team, season = x$season, week = 99L,
    games = x$games, wins = x$wins,
    above_expectation = x$above_expectation,
    level = "fbs",
    context_strength = stats::pnorm(x$srs / scale_context),
    target_context_strength = stats::pnorm(x$srs / scale_context),
    playoff_appearances = 0L, titles = 0L,
    source = ifelse(x$season < modern_start, "legacy_srs_prior", "legacy_srs_fallback"),
    stringsAsFactors = FALSE
  )
}

coach_assignment_qa_summary <- function(assignments, qa, identities) {
  data.frame(
    metric = c("identity_rows", "identity_collisions", "assignment_rows",
               "assignment_reviews", "qa_errors", "qa_warnings"),
    value = c(
      nrow(identities), sum(identities$identity_collision, na.rm = TRUE),
      nrow(assignments), sum(assignments$needs_review, na.rm = TRUE),
      if (nrow(qa)) sum(qa$severity == "error") else 0,
      if (nrow(qa)) sum(qa$severity == "warning") else 0
    ), stringsAsFactors = FALSE
  )
}

build_modern_coach_history <- function(training_games, assignments) {
  required <- c(
    "game_id", "season", "week", "home", "away", "home_level", "away_level",
    "margin", "pregame_expected_margin", "home_power", "away_power",
    "postseason_type", "is_cfp", "is_national_championship"
  )
  assert_columns(training_games, required, "training_games")
  training_games <- training_games[raw_history_eligible(training_games), , drop = FALSE]
  mapped <- map_coaches_as_of(training_games, assignments)
  history_week <- if ("model_week" %in% names(training_games)) {
    training_games$model_week
  } else training_games$week
  scale_power <- 10
  home_won <- training_games$margin > 0
  away_won <- training_games$margin < 0
  home <- data.frame(
    coach_id = mapped$home_coach_id, team = training_games$home,
    season = training_games$season,
    week = history_week, games = 1L, wins = as.numeric(home_won),
    above_expectation = training_games$margin - training_games$pregame_expected_margin,
    level = training_games$home_level,
    context_strength = stats::pnorm(training_games$home_power / scale_power),
    target_context_strength = stats::pnorm(training_games$home_power / scale_power),
    playoff_appearances = as.integer(training_games$is_cfp),
    titles = as.integer(training_games$is_national_championship & home_won),
    source = "pregame_game_residual", stringsAsFactors = FALSE
  )
  away <- data.frame(
    coach_id = mapped$away_coach_id, team = training_games$away,
    season = training_games$season,
    week = history_week, games = 1L, wins = as.numeric(away_won),
    above_expectation = -(training_games$margin - training_games$pregame_expected_margin),
    level = training_games$away_level,
    context_strength = stats::pnorm(training_games$away_power / scale_power),
    target_context_strength = stats::pnorm(training_games$away_power / scale_power),
    playoff_appearances = as.integer(training_games$is_cfp),
    titles = as.integer(training_games$is_national_championship & away_won),
    source = "pregame_game_residual", stringsAsFactors = FALSE
  )
  game_rows <- rbind(home, away)
  missing <- game_rows[is.na(game_rows$coach_id) | !nzchar(game_rows$coach_id), ]
  usable <- game_rows[!is.na(game_rows$coach_id) & nzchar(game_rows$coach_id), ]
  if (!nrow(usable)) {
    return(list(history = data.frame(), missing = missing))
  }
  groups <- split(usable, paste(usable$coach_id, usable$team, usable$season,
                                usable$week, usable$level, sep = "\r"))
  rows <- lapply(groups, function(x) {
    data.frame(
      coach_id = x$coach_id[1], team = x$team[1],
      season = x$season[1], week = x$week[1],
      games = nrow(x), wins = sum(x$wins, na.rm = TRUE),
      above_expectation = mean(x$above_expectation, na.rm = TRUE),
      level = x$level[1], context_strength = mean(x$context_strength, na.rm = TRUE),
      target_context_strength = mean(x$target_context_strength, na.rm = TRUE),
      playoff_appearances = max(x$playoff_appearances, na.rm = TRUE),
      titles = max(x$titles, na.rm = TRUE),
      source = "pregame_game_residual", stringsAsFactors = FALSE
    )
  })
  history <- do.call(rbind, rows)
  rownames(history) <- NULL
  list(history = history[order(history$season, history$week, history$coach_id), ],
       missing = missing)
}

attach_coach_features_to_history <- function(training_games, assignments,
                                              coach_history, config) {
  mapped <- map_coaches_as_of(training_games, assignments)
  rating_week <- if ("model_week" %in% names(training_games)) {
    training_games$model_week
  } else training_games$week
  combinations <- unique(data.frame(
    season = training_games$season, rating_week = rating_week
  ))
  combinations <- combinations[order(combinations$season, combinations$rating_week), ]
  ratings <- list()
  k <- 1L
  for (i in seq_len(nrow(combinations))) {
    row_index <- training_games$season == combinations$season[i] &
      rating_week == combinations$rating_week[i]
    target_coach <- c(mapped$home_coach_id[row_index], mapped$away_coach_id[row_index])
    power_scale <- if (!is.null(config$coach$context_power_scale)) {
      config$coach$context_power_scale
    } else 10
    target_strength <- stats::pnorm(c(
      training_games$home_power[row_index], training_games$away_power[row_index]
    ) / power_scale)
    valid_context <- !is.na(target_coach) & nzchar(target_coach) &
      is.finite(target_strength)
    target_context <- if (any(valid_context)) {
      tapply(target_strength[valid_context], target_coach[valid_context], mean)
    } else NULL
    comparison <- compare_coach_weight_splits(
      coach_history, combinations$season[i], combinations$rating_week[i], config,
      target_context = target_context
    )
    if (nrow(comparison)) {
      ratings[[k]] <- comparison[c("coach_id", "season", "as_of_week",
                                    "rating_65_35", "rating_70_30")]
      k <- k + 1L
    }
  }
  rating_table <- if (length(ratings)) do.call(rbind, ratings) else data.frame()
  if (!nrow(rating_table)) {
    training_games$coach_rating_65_35_diff <- 0
    training_games$coach_rating_70_30_diff <- 0
    training_games$coach_rating_diff <- 0
    training_games$coach_mapping_missing <- TRUE
    return(list(training = training_games, ratings = rating_table))
  }
  key <- paste(rating_table$coach_id, rating_table$season, rating_table$as_of_week,
               sep = "\r")
  home_key <- paste(mapped$home_coach_id, training_games$season, rating_week,
                    sep = "\r")
  away_key <- paste(mapped$away_coach_id, training_games$season, rating_week,
                    sep = "\r")
  home_index <- match(home_key, key)
  away_index <- match(away_key, key)
  for (column in c("rating_65_35", "rating_70_30")) {
    home_value <- rating_table[[column]][home_index]
    away_value <- rating_table[[column]][away_index]
    home_value[!is.finite(home_value)] <- 0
    away_value[!is.finite(away_value)] <- 0
    training_games[[paste0("coach_", column, "_diff")]] <- home_value - away_value
  }
  training_games$coach_rating_diff <- training_games$coach_rating_65_35_diff
  training_games$home_coach_id <- mapped$home_coach_id
  training_games$away_coach_id <- mapped$away_coach_id
  training_games$home_coach_interim <- as.numeric(mapped$home_coach_interim)
  training_games$away_coach_interim <- as.numeric(mapped$away_coach_interim)
  training_games$coach_mapping_missing <- is.na(mapped$home_coach_id) |
    is.na(mapped$away_coach_id)
  list(training = training_games, ratings = rating_table)
}
