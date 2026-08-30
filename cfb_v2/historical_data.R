first_non_missing <- function(x, default = NA) {
  hit <- which(!is.na(x) & if (is.character(x)) nzchar(x) else TRUE)
  if (length(hit)) x[hit[1]] else default
}

safe_max_numeric <- function(x) {
  x <- as.numeric(x)
  if (any(is.finite(x))) max(x, na.rm = TRUE) else NA_real_
}

compact_pbp_columns <- function() {
  c(
    "year", "season", "week", "game_id", "id_play", "pos_team", "def_pos_team",
    "pos_team_score", "def_pos_team_score", "pos_score_diff", "period",
    "play_type", "play_text", "EPA", "success", "rush", "pass", "sack",
    "turnover_indicator", "turnover", "stuffed_run", "kick_play", "punt_play",
    "fg_inds", "home_EPA", "away_EPA", "home", "away", "neutral_site",
    "conference_game", "season_type", "start_date", "completed",
    "home_team_division", "away_team_division", "home_team_conference",
    "away_team_conference", "home_team_pregame_elo", "away_team_pregame_elo",
    "spread", "over_under", "garbage_time"
  )
}

compact_cfb_pbp <- function(raw) {
  columns <- intersect(compact_pbp_columns(), names(raw))
  out <- as.data.frame(raw[, columns, drop = FALSE], stringsAsFactors = FALSE)
  for (column in setdiff(compact_pbp_columns(), names(out))) out[[column]] <- NA
  out <- out[compact_pbp_columns()]
  out$game_id <- as.character(out$game_id)
  out$year <- as.integer(ifelse(is.finite(out$year), out$year, out$season))
  out$season <- as.integer(ifelse(is.finite(out$season), out$season, out$year))
  out$week <- as.integer(out$week)
  out$home <- canonical_team(out$home)
  out$away <- canonical_team(out$away)
  out$pos_team <- canonical_team(out$pos_team)
  out$def_pos_team <- canonical_team(out$def_pos_team)
  out
}

load_compact_pbp_season <- function(season, config, refresh = FALSE) {
  require_v2_package("cfbfastR")
  cache_dir <- file.path(config$project_dir, "cfb_v2", "cache", "pbp")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("pbp_", season, "_compact.rds"))
  if (!refresh && file.exists(path)) return(readRDS(path))
  raw <- cfbfastR::load_cfb_pbp(as.integer(season))
  compact <- compact_cfb_pbp(raw)
  saveRDS(compact, path, compress = "xz")
  compact
}

load_cfbd_schedule_season <- function(season, config, refresh = FALSE,
                                      allow_missing_key = TRUE) {
  cache_dir <- file.path(config$project_dir, "cfb_v2", "cache", "schedules")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("cfbd_schedule_", season, ".rds"))
  if (!refresh && file.exists(path)) return(readRDS(path))
  require_v2_package("cfbfastR")
  if (!cfbfastR::has_cfbd_key()) {
    message(
      "CFBD_API_KEY is not configured; using the audited ESPN schedule fallback for ",
      season, "."
    )
    if (allow_missing_key) return(NULL)
    stop(
      "CFBD_API_KEY is required for the authoritative schedule build. ",
      "Set it in the environment and rerun.", call. = FALSE
    )
  }
  regular <- as.data.frame(
    cfbfastR::cfbd_game_info(as.integer(season), season_type = "regular"),
    stringsAsFactors = FALSE
  )
  postseason <- as.data.frame(
    cfbfastR::cfbd_game_info(as.integer(season), season_type = "postseason"),
    stringsAsFactors = FALSE
  )
  columns <- union(names(regular), names(postseason))
  for (column in setdiff(columns, names(regular))) {
    regular[[column]] <- rep(NA, nrow(regular))
  }
  for (column in setdiff(columns, names(postseason))) {
    postseason[[column]] <- rep(NA, nrow(postseason))
  }
  schedule <- rbind(regular[columns], postseason[columns])
  schedule <- schedule[!duplicated(as.character(schedule$game_id)), , drop = FALSE]
  if (!nrow(schedule)) {
    stop("CFBD returned no schedule rows for ", season, ".", call. = FALSE)
  }
  saveRDS(schedule, path, compress = "xz")
  schedule
}

load_cfbd_fbs_teams_season <- function(season, config, refresh = FALSE) {
  require_v2_package("cfbfastR")
  cache_dir <- file.path(config$project_dir, "cfb_v2", "cache", "teams")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("cfbd_fbs_teams_", season, ".rds"))
  if (!refresh && file.exists(path)) return(readRDS(path))
  if (!cfbfastR::has_cfbd_key()) return(NULL)
  raw <- as.data.frame(
    cfbfastR::cfbd_team_info(only_fbs = TRUE, year = as.integer(season)),
    stringsAsFactors = FALSE
  )
  if (!nrow(raw)) {
    stop("CFBD returned no FBS membership rows for ", season, ".", call. = FALSE)
  }
  out <- data.frame(
    team = canonical_team(column_value(raw, c("school", "team"), "")),
    season = as.integer(season),
    conference = as.character(column_value(raw, "conference")),
    division = as.character(column_value(raw, "division")),
    classification = "fbs", source = "cfbd_teams_fbs",
    stringsAsFactors = FALSE
  )
  out <- out[nzchar(out$team), , drop = FALSE]
  out <- out[!duplicated(out$team), , drop = FALSE]
  assert_unique_keys(out, c("team", "season"), "CFBD FBS membership")
  saveRDS(out, path, compress = "xz")
  out
}

load_cfbd_coach_seasons <- function(min_year, max_year, config,
                                    aliases_path = NULL, refresh = FALSE) {
  require_v2_package("cfbfastR")
  cache_dir <- file.path(config$project_dir, "cfb_v2", "cache", "coaches")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(
    cache_dir, paste0("cfbd_coaches_", min_year, "_", max_year, ".rds")
  )
  required_cache_columns <- c(
    "coach_id", "canonical_name", "team", "season", "games", "wins",
    "srs", "hire_date", "counts_available", "data_source"
  )
  if (!refresh && file.exists(path)) {
    cached <- readRDS(path)
    if (all(required_cache_columns %in% names(cached))) return(cached)
  }
  if (!cfbfastR::has_cfbd_key()) return(NULL)
  raw <- as.data.frame(
    cfbfastR::cfbd_coaches(min_year = as.integer(min_year),
                           max_year = as.integer(max_year)),
    stringsAsFactors = FALSE
  )
  if (!nrow(raw)) {
    stop("CFBD returned no coach rows for ", min_year, "-", max_year, ".",
         call. = FALSE)
  }
  coaches <- standardize_coach_seasons(
    raw, aliases_path = aliases_path, data_source = "cfbd_coaches"
  )
  saveRDS(coaches, path, compress = "xz")
  coaches
}

clean_public_table_names <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

clean_public_coach_name <- function(x) {
  x <- gsub("\\[[0-9]+\\]", "", as.character(x))
  x <- gsub("\\s*\\([^)]*\\).*", "", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

empty_public_transitions <- function() {
  data.frame(
    team = character(), season = integer(), outgoing_coach = character(),
    effective_date = as.Date(character()), replacement_coach = character(),
    interim = logical(), reason = character(), source_url = character(),
    retrieved_at = as.POSIXct(character()), stringsAsFactors = FALSE
  )
}

pull_public_coach_transitions <- function(season, config, refresh = FALSE) {
  require_v2_package("rvest")
  cache_dir <- file.path(config$project_dir, "cfb_v2", "cache", "coaches")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("public_transitions_", season, ".rds"))
  if (!refresh && file.exists(path)) {
    cached <- readRDS(path)
    if (nrow(cached)) cached$team <- canonical_team(cached$team)
    return(cached)
  }
  url <- paste0(
    "https://en.wikipedia.org/wiki/", season,
    "_NCAA_Division_I_FBS_football_season"
  )
  document <- rvest::read_html(url)
  tables <- rvest::html_table(document, fill = TRUE)
  selected <- NULL
  for (table in tables) {
    names(table) <- clean_public_table_names(names(table))
    team_column <- intersect(c("school", "team"), names(table))
    required <- c("outgoing_coach", "date", "replacement")
    if (length(team_column) && all(required %in% names(table)) &&
        !"previous_position" %in% names(table)) {
      selected <- as.data.frame(table, stringsAsFactors = FALSE)
      selected$school <- selected[[team_column[1]]]
      break
    }
  }
  if (is.null(selected) || !nrow(selected)) {
    warning("No public in-season coach transition table found for ", season,
            call. = FALSE)
    out <- empty_public_transitions()
    saveRDS(out, path, compress = "xz")
    return(out)
  }
  date_text <- trimws(as.character(selected$date))
  effective_date <- as.Date(date_text, format = "%B %d, %Y")
  missing_year <- is.na(effective_date)
  effective_date[missing_year] <- as.Date(
    paste(date_text[missing_year], season), format = "%B %d %Y"
  )
  replacement_raw <- as.character(selected$replacement)
  reason <- if ("reason" %in% names(selected)) as.character(selected$reason) else ""
  out <- data.frame(
    team = canonical_team(trimws(as.character(selected$school))),
    season = as.integer(season),
    outgoing_coach = clean_public_coach_name(selected$outgoing_coach),
    effective_date = effective_date,
    replacement_coach = clean_public_coach_name(replacement_raw),
    interim = grepl("interim", replacement_raw, ignore.case = TRUE),
    reason = reason, source_url = url,
    retrieved_at = as.POSIXct(Sys.time(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  usable <- nzchar(out$team) & nzchar(out$outgoing_coach) &
    nzchar(out$replacement_coach) & !is.na(out$effective_date)
  out <- out[usable, , drop = FALSE]
  out <- out[!duplicated(paste(out$team, out$season, out$effective_date)), , drop = FALSE]
  saveRDS(out, path, compress = "xz")
  out
}

bind_rows_fill <- function(...) {
  frames <- Filter(function(x) !is.null(x) && nrow(x), list(...))
  if (!length(frames)) return(data.frame())
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[columns]
  })
  do.call(rbind, frames)
}

column_value <- function(data, candidates, default = NA) {
  hit <- intersect(candidates, names(data))
  if (length(hit)) data[[hit[1]]] else rep(default, nrow(data))
}

missing_value <- function(x) {
  is.na(x) | (is.character(x) & !nzchar(trimws(x)))
}

fill_from_lookup <- function(target, lookup, columns) {
  index <- match(target$game_id, lookup$game_id)
  for (column in intersect(columns, intersect(names(target), names(lookup)))) {
    incoming <- lookup[[column]][index]
    replace <- missing_value(target[[column]]) & !missing_value(incoming)
    target[[column]][replace] <- incoming[replace]
  }
  target
}

standardize_cfbd_schedule <- function(schedule) {
  schedule <- as.data.frame(schedule, stringsAsFactors = FALSE)
  game_id <- as.character(column_value(schedule, c("game_id", "id")))
  season <- as.integer(column_value(schedule, c("season", "year")))
  week <- as.integer(column_value(schedule, "week"))
  home <- canonical_team(column_value(schedule, c("home_team", "home"), ""))
  away <- canonical_team(column_value(schedule, c("away_team", "away"), ""))
  kickoff_raw <- column_value(schedule, c("start_date", "kickoff"))
  kickoff <- parse_utc_datetime(kickoff_raw)
  source_type <- tolower(as.character(
    column_value(schedule, c("season_type", "season_type_name"), "regular")
  ))
  source_type <- ifelse(grepl("post", source_type), "postseason", "regular")
  out <- data.frame(
    game_id = game_id, season = season, week = week, kickoff = kickoff,
    home = home, away = away,
    neutral_site = as.logical(column_value(schedule, "neutral_site", FALSE)),
    home_level = normalize_division(column_value(
      schedule, c("home_division", "home_classification", "home_team_division")
    )),
    away_level = normalize_division(column_value(
      schedule, c("away_division", "away_classification", "away_team_division")
    )),
    home_conference = as.character(column_value(
      schedule, c("home_conference", "home_team_conference")
    )),
    away_conference = as.character(column_value(
      schedule, c("away_conference", "away_team_conference")
    )),
    home_pregame_elo = as.numeric(column_value(
      schedule, c("home_pregame_elo", "home_elo")
    )),
    away_pregame_elo = as.numeric(column_value(
      schedule, c("away_pregame_elo", "away_elo")
    )),
    home_score = as.numeric(column_value(
      schedule, c("home_points", "home_score")
    )),
    away_score = as.numeric(column_value(
      schedule, c("away_points", "away_score")
    )),
    closing_home_spread = as.numeric(column_value(
      schedule, c("spread", "closing_home_spread")
    )),
    market_total = as.numeric(column_value(
      schedule, c("over_under", "market_total")
    )),
    source_season_type = source_type,
    completed = as.logical(column_value(schedule, "completed", TRUE)),
    schedule_notes = as.character(column_value(schedule, "notes")),
    schedule_source = "cfbd_games", stringsAsFactors = FALSE
  )
  out <- out[nzchar(out$game_id) & nzchar(out$home) & nzchar(out$away), , drop = FALSE]
  out <- out[!duplicated(out$game_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

espn_scoreboard_url <- function(season, season_type, week) {
  type_id <- if (tolower(season_type) == "postseason") 3L else 2L
  paste0(
    "https://site.api.espn.com/apis/site/v2/sports/football/college-football/",
    "scoreboard?dates=", season, "&seasontype=", type_id,
    "&week=", week, "&limit=1000&groups=80"
  )
}

nested_list_value <- function(x, path, default = NULL) {
  value <- x
  for (name in path) {
    if (is.null(value) || !is.list(value) || is.null(value[[name]])) return(default)
    value <- value[[name]]
  }
  if (is.null(value) || !length(value)) default else value
}

scalar_character <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x)) return(default)
  if (is.list(x)) {
    for (name in c("value", "displayValue", "display_value")) {
      if (!is.null(x[[name]]) && length(x[[name]])) return(as.character(x[[name]][1]))
    }
    return(default)
  }
  as.character(x[1])
}

espn_competitor_value <- function(competition, home_away) {
  competitors <- competition$competitors
  if (is.null(competitors) || !length(competitors)) return(NULL)
  side <- vapply(competitors, function(x) {
    tolower(scalar_character(x$homeAway, ""))
  }, character(1))
  hit <- which(side == home_away)
  if (!length(hit)) return(NULL)
  competitors[[hit[1]]]
}

pull_espn_event_metadata <- function(season, pbp, config, refresh = FALSE) {
  require_v2_package("httr")
  require_v2_package("jsonlite")
  cache_dir <- file.path(config$project_dir, "cfb_v2", "cache", "espn")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0("events_", season, ".rds"))
  required_cache_columns <- c(
    "game_id", "espn_home", "espn_away", "espn_home_score",
    "espn_away_score", "espn_completed"
  )
  if (!refresh && file.exists(path)) {
    cached <- readRDS(path)
    if (all(required_cache_columns %in% names(cached))) return(cached)
    message("Refreshing legacy ESPN metadata cache for ", season, ".")
  }

  observed_weeks <- as.integer(pbp$week[is.finite(pbp$week)])
  regular_weeks <- 0:max(20L, observed_weeks, na.rm = TRUE)
  combinations <- rbind(
    data.frame(season_type = "regular", week = regular_weeks),
    data.frame(season_type = "postseason", week = 1:5)
  )
  rows <- list()
  k <- 1L
  for (i in seq_len(nrow(combinations))) {
    url <- espn_scoreboard_url(season, combinations$season_type[i], combinations$week[i])
    response <- httr::GET(url, httr::timeout(30))
    if (httr::http_error(response)) {
      warning("ESPN metadata request failed: ", url, call. = FALSE)
      next
    }
    payload <- jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    events <- payload$events
    if (!length(events)) next
    for (event in events) {
      competition <- event$competitions[[1]]
      notes <- competition$notes
      headline <- if (length(notes)) notes[[1]]$headline else NA_character_
      home <- espn_competitor_value(competition, "home")
      away <- espn_competitor_value(competition, "away")
      home_name <- scalar_character(nested_list_value(home, c("team", "location")))
      away_name <- scalar_character(nested_list_value(away, c("team", "location")))
      if (is.na(home_name)) {
        home_name <- scalar_character(nested_list_value(home, c("team", "displayName")))
      }
      if (is.na(away_name)) {
        away_name <- scalar_character(nested_list_value(away, c("team", "displayName")))
      }
      event_type <- scalar_character(nested_list_value(event, c("season", "slug")), "")
      event_type <- if (grepl("post", event_type, ignore.case = TRUE)) {
        "postseason"
      } else combinations$season_type[i]
      rows[[k]] <- data.frame(
        game_id = as.character(event$id), season = as.integer(season),
        week = as.integer(combinations$week[i]),
        espn_season_type = event_type,
        event_name = as.character(event$name),
        event_short_name = as.character(event$shortName),
        event_headline = as.character(headline),
        event_date = as.character(event$date),
        espn_home = canonical_team(home_name),
        espn_away = canonical_team(away_name),
        espn_home_score = as.numeric(scalar_character(if (is.null(home)) NULL else home$score)),
        espn_away_score = as.numeric(scalar_character(if (is.null(away)) NULL else away$score)),
        espn_neutral_site = as.logical(nested_list_value(
          competition, c("neutralSite"), FALSE
        )),
        espn_completed = as.logical(nested_list_value(
          competition, c("status", "type", "completed"), FALSE
        )),
        source_url = url, stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
    Sys.sleep(0.05)
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (nrow(out)) {
    quality <- as.integer(out$espn_completed) * 4L +
      as.integer(is.finite(out$espn_home_score) & is.finite(out$espn_away_score)) * 2L +
      as.integer(!is.na(out$event_headline) & nzchar(out$event_headline))
    out <- out[order(out$game_id, -quality), , drop = FALSE]
    out <- out[!duplicated(out$game_id), , drop = FALSE]
  }
  saveRDS(out, path, compress = "xz")
  out
}

normalize_division <- function(x) {
  value <- tolower(as.character(x))
  ifelse(grepl("fbs", value), "fbs",
         ifelse(grepl("fcs", value), "fcs", "unknown"))
}

derive_pbp_game_rows <- function(pbp) {
  pbp$home_score_state <- ifelse(
    pbp$pos_team == pbp$home, pbp$pos_team_score,
    ifelse(pbp$def_pos_team == pbp$home, pbp$def_pos_team_score, NA)
  )
  pbp$away_score_state <- ifelse(
    pbp$pos_team == pbp$away, pbp$pos_team_score,
    ifelse(pbp$def_pos_team == pbp$away, pbp$def_pos_team_score, NA)
  )
  groups <- split(pbp, pbp$game_id)
  rows <- lapply(groups, function(x) {
    data.frame(
      game_id = x$game_id[1], season = as.integer(first_non_missing(x$season)),
      week = as.integer(first_non_missing(x$week)),
      kickoff = as.POSIXct(first_non_missing(x$start_date), tz = "UTC"),
      home = canonical_team(first_non_missing(x$home)),
      away = canonical_team(first_non_missing(x$away)),
      neutral_site = as.logical(first_non_missing(x$neutral_site, FALSE)),
      home_level = normalize_division(first_non_missing(x$home_team_division)),
      away_level = normalize_division(first_non_missing(x$away_team_division)),
      home_conference = as.character(first_non_missing(x$home_team_conference)),
      away_conference = as.character(first_non_missing(x$away_team_conference)),
      home_pregame_elo = as.numeric(first_non_missing(x$home_team_pregame_elo)),
      away_pregame_elo = as.numeric(first_non_missing(x$away_team_pregame_elo)),
      home_score = safe_max_numeric(x$home_score_state),
      away_score = safe_max_numeric(x$away_score_state),
      closing_home_spread = as.numeric(first_non_missing(x$spread)),
      market_total = as.numeric(first_non_missing(x$over_under)),
      source_season_type = tolower(as.character(first_non_missing(
        x$season_type, "regular"
      ))),
      completed = as.logical(first_non_missing(x$completed, TRUE)),
      schedule_notes = NA_character_,
      schedule_source = "cfbfastR_pbp_derived", stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

team_metadata_from_games <- function(games) {
  home <- data.frame(
    team = games$home, level = games$home_level,
    conference = games$home_conference, stringsAsFactors = FALSE
  )
  away <- data.frame(
    team = games$away, level = games$away_level,
    conference = games$away_conference, stringsAsFactors = FALSE
  )
  all <- rbind(home, away)
  groups <- split(all, all$team)
  rows <- lapply(groups, function(x) data.frame(
    team = x$team[1],
    level = first_non_missing(x$level, "unknown"),
    conference = first_non_missing(x$conference), stringsAsFactors = FALSE
  ))
  do.call(rbind, rows)
}

espn_supplemental_games <- function(event_metadata, known_games) {
  if (is.null(event_metadata) || !nrow(event_metadata)) return(known_games[FALSE, ])
  x <- event_metadata[
    !event_metadata$game_id %in% known_games$game_id &
      as.logical(event_metadata$espn_completed) &
      !is.na(event_metadata$espn_home) & !is.na(event_metadata$espn_away) &
      nzchar(event_metadata$espn_home) & nzchar(event_metadata$espn_away) &
      is.finite(event_metadata$espn_home_score) &
      is.finite(event_metadata$espn_away_score), , drop = FALSE
  ]
  if (!nrow(x)) return(known_games[FALSE, ])
  metadata <- team_metadata_from_games(known_games)
  home_index <- match(x$espn_home, metadata$team)
  away_index <- match(x$espn_away, metadata$team)
  out <- data.frame(
    game_id = x$game_id, season = x$season, week = x$week,
    kickoff = as.POSIXct(x$event_date, tz = "UTC"),
    home = x$espn_home, away = x$espn_away,
    neutral_site = x$espn_neutral_site,
    home_level = metadata$level[home_index],
    away_level = metadata$level[away_index],
    home_conference = metadata$conference[home_index],
    away_conference = metadata$conference[away_index],
    home_pregame_elo = NA_real_, away_pregame_elo = NA_real_,
    home_score = x$espn_home_score, away_score = x$espn_away_score,
    closing_home_spread = NA_real_, market_total = NA_real_,
    source_season_type = x$espn_season_type,
    completed = x$espn_completed,
    schedule_notes = x$event_headline,
    schedule_source = "espn_fbs_fallback", stringsAsFactors = FALSE
  )
  out$home_level[is.na(out$home_level)] <- "unknown"
  out$away_level[is.na(out$away_level)] <- "unknown"
  out
}

add_chronological_model_week <- function(games) {
  game_date <- as.Date(games$kickoff, tz = "UTC")
  weekday <- as.POSIXlt(game_date, tz = "UTC")$wday
  games$feature_week_start <- game_date - ((weekday + 6L) %% 7L)
  games$model_week <- NA_integer_
  for (season in unique(games$season)) {
    hit <- games$season == season
    starts <- sort(unique(games$feature_week_start[hit]))
    games$model_week[hit] <- match(games$feature_week_start[hit], starts)
  }
  games
}

derive_games_from_pbp <- function(pbp, event_metadata = NULL, schedule = NULL,
                                  fbs_membership = NULL) {
  pbp <- compact_cfb_pbp(pbp)
  pbp_games <- derive_pbp_game_rows(pbp)
  games <- if (is.null(schedule)) pbp_games else standardize_cfbd_schedule(schedule)
  games <- fill_from_lookup(
    games, pbp_games,
    c("season", "week", "kickoff", "home", "away", "neutral_site",
      "home_level", "away_level", "home_conference", "away_conference",
      "home_pregame_elo", "away_pregame_elo", "home_score", "away_score",
      "closing_home_spread", "market_total", "source_season_type", "completed")
  )
  if (!is.null(schedule)) {
    missing_pbp_games <- pbp_games[!pbp_games$game_id %in% games$game_id, , drop = FALSE]
    if (nrow(missing_pbp_games)) {
      missing_pbp_games$schedule_source <- "cfbfastR_pbp_schedule_gap"
      games <- rbind(games, missing_pbp_games)
    }
  }
  if (!is.null(event_metadata) && nrow(event_metadata)) {
    games <- fill_from_lookup(games, event_metadata, c("season", "week"))
  }
  supplemental <- espn_supplemental_games(event_metadata, games)
  if (nrow(supplemental)) games <- rbind(games, supplemental)
  if (!is.null(fbs_membership) && nrow(fbs_membership)) {
    fbs_teams <- unique(canonical_team(fbs_membership$team))
    home_is_fbs <- games$home %in% fbs_teams
    away_is_fbs <- games$away %in% fbs_teams
    games$home_level[home_is_fbs] <- "fbs"
    games$away_level[away_is_fbs] <- "fbs"
    unknown_home <- !home_is_fbs &
      (is.na(games$home_level) | games$home_level == "unknown")
    unknown_away <- !away_is_fbs &
      (is.na(games$away_level) | games$away_level == "unknown")
    games$home_level[unknown_home] <- "fcs"
    games$away_level[unknown_away] <- "fcs"
    if ("conference" %in% names(fbs_membership)) {
      conference <- fbs_membership$conference[match(games$home, fbs_membership$team)]
      replace <- home_is_fbs & (is.na(games$home_conference) |
                                  !nzchar(games$home_conference))
      games$home_conference[replace] <- conference[replace]
      conference <- fbs_membership$conference[match(games$away, fbs_membership$team)]
      replace <- away_is_fbs & (is.na(games$away_conference) |
                                  !nzchar(games$away_conference))
      games$away_conference[replace] <- conference[replace]
    }
    games <- games[home_is_fbs | away_is_fbs, , drop = FALSE]
  } else {
    explicit_fbs <- games$home_level == "fbs" | games$away_level == "fbs"
    espn_fbs_game <- !is.null(event_metadata) &
      games$game_id %in% event_metadata$game_id
    games <- games[(explicit_fbs | espn_fbs_game) & !is.na(explicit_fbs), , drop = FALSE]
  }
  games$pbp_available <- games$game_id %in% pbp_games$game_id
  if (!is.null(event_metadata) && nrow(event_metadata)) {
    event_columns <- setdiff(
      names(event_metadata),
      c("season", "week", "espn_home", "espn_away", "espn_home_score",
        "espn_away_score", "espn_neutral_site", "espn_completed")
    )
    games <- merge(games, event_metadata[event_columns], by = "game_id",
                   all.x = TRUE, sort = FALSE)
  } else {
    games$event_name <- NA_character_
    games$event_short_name <- NA_character_
    games$event_headline <- NA_character_
    games$espn_season_type <- NA_character_
    games$event_date <- NA_character_
  }
  espn_postseason <- !is.na(games$espn_season_type) &
    tolower(games$espn_season_type) == "postseason"
  games$source_season_type[espn_postseason] <- "postseason"
  games$source_season_type <- ifelse(
    !is.na(games$source_season_type) &
      grepl("post", games$source_season_type, ignore.case = TRUE),
    "postseason", "regular"
  )
  missing_kickoff <- is.na(games$kickoff) & !is.na(games$event_date)
  games$kickoff[missing_kickoff] <- as.POSIXct(games$event_date[missing_kickoff], tz = "UTC")
  label <- paste(games$event_name, games$event_headline, games$schedule_notes)
  games$is_cfp <- grepl(
    "college football playoff|national championship|cfp semifinal",
    label, ignore.case = TRUE
  )
  games$is_national_championship <- grepl("national championship", label,
                                           ignore.case = TRUE)
  games$conference_championship <- grepl("championship", label, ignore.case = TRUE) &
    !games$is_cfp & games$source_season_type != "postseason"
  games$postseason_type <- ifelse(
    games$is_cfp, "cfp",
    ifelse(games$conference_championship, "conference_championship",
           ifelse(games$source_season_type == "postseason", "bowl", "regular"))
  )
  games$coach_lookup_week <- ifelse(
    games$postseason_type == "regular", games$week, 99L
  )
  games$margin <- games$home_score - games$away_score
  games$total_points <- games$home_score + games$away_score
  games <- add_chronological_model_week(games)
  games <- games[order(games$season, games$kickoff, games$game_id), , drop = FALSE]
  assert_unique_keys(games, "game_id", "historical games")
  games
}

filter_completed_historical_games <- function(games) {
  assert_columns(
    games, c("completed", "home_score", "away_score"), "historical games"
  )
  keep <- !is.na(games$completed) & as.logical(games$completed) &
    is.finite(as.numeric(games$home_score)) &
    is.finite(as.numeric(games$away_score))
  games[keep, , drop = FALSE]
}

historical_competitive_plays <- function(pbp) {
  pbp <- compact_cfb_pbp(pbp)
  pbp$home_score <- ifelse(
    pbp$pos_team == pbp$home, pbp$pos_team_score,
    ifelse(pbp$def_pos_team == pbp$home, pbp$def_pos_team_score, NA)
  )
  pbp$away_score <- ifelse(
    pbp$pos_team == pbp$away, pbp$pos_team_score,
    ifelse(pbp$def_pos_team == pbp$away, pbp$def_pos_team_score, NA)
  )
  if (all(is.na(pbp$garbage_time))) pbp$garbage_time <- NULL
  filter_competitive_plays(pbp)
}

aggregate_mean_metric <- function(data, group_columns, value, output_name) {
  keep <- is.finite(data[[value]])
  if (!any(keep)) return(data.frame())
  formula <- stats::as.formula(paste(value, "~", paste(group_columns, collapse = "+")))
  out <- stats::aggregate(formula, data[keep, , drop = FALSE], mean, na.rm = TRUE)
  names(out)[names(out) == value] <- output_name
  out
}

build_team_game_efficiencies_v2 <- function(pbp, games) {
  plays <- historical_competitive_plays(pbp)
  plays$epa <- as.numeric(plays$EPA)
  plays$is_scrimmage <- as.logical(plays$rush == 1 | plays$pass == 1 | plays$sack == 1 |
    grepl("pass|rush|run|sack", plays$play_type, ignore.case = TRUE))
  plays$is_pass <- as.logical(plays$pass == 1 | plays$sack == 1 |
                               grepl("pass|sack", plays$play_type, ignore.case = TRUE))
  plays$is_rush <- as.logical(plays$rush == 1 |
                               grepl("rush|run", plays$play_type, ignore.case = TRUE))
  plays$success_value <- as.numeric(plays$success)
  turnover <- ifelse(is.finite(plays$turnover_indicator), plays$turnover_indicator,
                     ifelse(is.finite(plays$turnover), plays$turnover, 0))
  plays$havoc <- as.numeric(plays$sack == 1 | turnover == 1 | plays$stuffed_run == 1)
  # The upstream turnover flags also count failed fourth downs, defensive scores,
  # and special-teams changes of possession, so a giveaway additionally requires an
  # interception or lost-fumble play type.
  giveaway_description <- paste(plays$play_type, plays$play_text)
  giveaway_play <- grepl("intercept", giveaway_description, ignore.case = TRUE) |
    grepl("fumble recovery \\(opponent\\)|fumble return touchdown|sack touchdown",
          plays$play_type, ignore.case = TRUE)
  plays$turnover_lost <- as.numeric(turnover == 1 & giveaway_play)
  scrimmage <- plays[plays$is_scrimmage & is.finite(plays$epa) &
                       nzchar(plays$pos_team) & nzchar(plays$def_pos_team), , drop = FALSE]
  if (!nrow(scrimmage)) stop("No competitive scrimmage plays were available.", call. = FALSE)

  group_offense <- c("game_id", "pos_team")
  offense_epa <- aggregate_mean_metric(scrimmage, group_offense, "epa", "offense_epa")
  names(offense_epa)[names(offense_epa) == "pos_team"] <- "team"
  success <- aggregate_mean_metric(scrimmage, group_offense, "success_value", "success_rate")
  names(success)[names(success) == "pos_team"] <- "team"
  havoc_allowed <- aggregate_mean_metric(scrimmage, group_offense, "havoc", "havoc_allowed")
  names(havoc_allowed)[names(havoc_allowed) == "pos_team"] <- "team"
  turnovers_lost <- aggregate_mean_metric(scrimmage, group_offense, "turnover_lost",
                                          "turnover_lost_rate")
  names(turnovers_lost)[names(turnovers_lost) == "pos_team"] <- "team"
  pass_epa <- aggregate_mean_metric(scrimmage[scrimmage$is_pass, ], group_offense,
                                    "epa", "pass_epa")
  if (nrow(pass_epa)) names(pass_epa)[names(pass_epa) == "pos_team"] <- "team"
  rush_epa <- aggregate_mean_metric(scrimmage[scrimmage$is_rush, ], group_offense,
                                    "epa", "rush_epa")
  if (nrow(rush_epa)) names(rush_epa)[names(rush_epa) == "pos_team"] <- "team"
  play_count <- stats::aggregate(
    list(scrimmage_plays = scrimmage$epa),
    scrimmage[c("game_id", "pos_team")], length
  )
  names(play_count)[names(play_count) == "pos_team"] <- "team"

  group_defense <- c("game_id", "def_pos_team")
  defense <- aggregate_mean_metric(scrimmage, group_defense, "epa", "epa_allowed")
  names(defense)[names(defense) == "def_pos_team"] <- "team"
  havoc_generated <- aggregate_mean_metric(scrimmage, group_defense, "havoc", "havoc_generated")
  names(havoc_generated)[names(havoc_generated) == "def_pos_team"] <- "team"

  output <- Reduce(function(x, y) merge(x, y, by = c("game_id", "team"), all = TRUE),
                   Filter(function(x) nrow(x),
                          list(offense_epa, defense, success, pass_epa, rush_epa,
                               havoc_allowed, havoc_generated, turnovers_lost,
                               play_count)))
  output$defense_epa <- -output$epa_allowed
  turnover_prior <- mean(output$turnover_lost_rate, na.rm = TRUE)
  if (!is.finite(turnover_prior)) turnover_prior <- 0
  output$turnover_rate_regressed <- regress_unstable_rate(
    output$turnover_lost_rate, output$scrimmage_plays,
    turnover_prior, prior_opportunities = 80
  )

  special <- plays[grepl("punt|kickoff|field goal|extra point", plays$play_type,
                         ignore.case = TRUE), , drop = FALSE]
  if (nrow(special)) {
    home_special <- data.frame(game_id = special$game_id, team = special$home,
                               special_epa = as.numeric(special$home_EPA))
    away_special <- data.frame(game_id = special$game_id, team = special$away,
                               special_epa = as.numeric(special$away_EPA))
    special_long <- rbind(home_special, away_special)
    special_long <- special_long[is.finite(special_long$special_epa), , drop = FALSE]
  } else {
    special_long <- data.frame()
  }
  if (nrow(special_long)) {
    special_mean <- aggregate_mean_metric(special_long, c("game_id", "team"),
                                          "special_epa", "special_teams_epa_raw")
    special_count <- stats::aggregate(
      list(special_teams_plays = special_long$special_epa),
      special_long[c("game_id", "team")], function(x) sum(is.finite(x))
    )
    special_mean <- merge(special_mean, special_count, by = c("game_id", "team"), all = TRUE)
    special_mean$special_teams_rating <- shrink_special_teams_epa(
      special_mean$special_teams_epa_raw, special_mean$special_teams_plays,
      league_epa = 0, prior_plays = 120
    )
    output <- merge(output, special_mean, by = c("game_id", "team"), all = TRUE)
  } else {
    output$special_teams_rating <- 0
    output$special_teams_plays <- 0
  }

  expected <- rbind(
    data.frame(game_id = games$game_id, team = games$home, stringsAsFactors = FALSE),
    data.frame(game_id = games$game_id, team = games$away, stringsAsFactors = FALSE)
  )
  output <- merge(expected, output, by = c("game_id", "team"), all.x = TRUE,
                  sort = FALSE)
  game_keep <- games[c("game_id", "season", "week", "model_week", "kickoff",
                        "home", "away", "home_score", "away_score", "neutral_site",
                        "source_season_type", "postseason_type")]
  output <- merge(output, game_keep, by = "game_id", all.x = TRUE)
  output$opponent <- ifelse(output$team == output$home, output$away, output$home)
  output$is_home <- output$team == output$home
  output$margin <- ifelse(output$is_home,
                          output$home_score - output$away_score,
                          output$away_score - output$home_score)
  output$net_efficiency <- output$offense_epa + output$defense_epa
  output[order(output$team, output$season, output$kickoff, output$game_id), ]
}

power_rating_snapshots <- function(games, config) {
  games <- games[is.finite(games$margin) & as.logical(games$completed), , drop = FALSE]
  assert_columns(games, c("season", "week", "model_week"), "historical games")
  combinations <- unique(games[c("season", "model_week")])
  combinations <- combinations[
    order(combinations$season, combinations$model_week), , drop = FALSE
  ]
  rows <- list()
  k <- 1L
  for (i in seq_len(nrow(combinations))) {
    season <- combinations$season[i]
    model_week <- combinations$model_week[i]
    past <- games[
      games$season <= season & games$season >= season - 3L &
        (games$season < season | games$model_week < model_week), , drop = FALSE
    ]
    if (!nrow(past)) next
    current_games <- past[past$season == season, , drop = FALSE]
    games_played <- if (nrow(current_games)) {
      stats::median(table(c(current_games$home, current_games$away)))
    } else 0
    target_games <- games[
      games$season == season & games$model_week == model_week, , drop = FALSE
    ]
    phase_week <- if (any(target_games$postseason_type != "regular", na.rm = TRUE)) {
      99L
    } else if (any(is.finite(target_games$week))) {
      max(target_games$week, na.rm = TRUE)
    } else 99L
    history_weight <- prior_season_feature_weight(phase_week, games_played, config)
    age <- season - past$season
    weights <- ifelse(age == 0, 1,
                      ifelse(age == 1, history_weight,
                             history_weight * 0.35^(age - 1L)))
    non_cfp_bowl <- past$postseason_type == "bowl" & !past$is_cfp
    weights[non_cfp_bowl] <- 0
    fcs <- is.na(past$home_level) | is.na(past$away_level) |
      past$home_level != "fbs" | past$away_level != "fbs"
    weights[fcs] <- weights[fcs] * config$training$fcs_rating_weight
    ratings <- opponent_adjusted_rating(
      past, value_col = "margin", ridge = 10,
      home_field = 2.4, weights = weights
    )
    if (!nrow(ratings)) next
    ratings$season <- season
    ratings$model_week <- model_week
    ratings$games_available <- vapply(ratings$team, function(team) {
      sum(current_games$home == team | current_games$away == team)
    }, integer(1))
    rows[[k]] <- ratings
    k <- k + 1L
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  names(out)[names(out) == "rating"] <- "power_rating"
  rownames(out) <- NULL
  out
}

mean_or_na <- function(x) {
  if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
}

tail_mean_or_na <- function(x, n) {
  x <- x[is.finite(x)]
  if (length(x)) mean(tail(x, n)) else NA_real_
}

blend_current_with_history <- function(current, history, week, config) {
  preseason_weight <- preseason_feature_weight(week, config)
  if (!is.finite(history)) history <- 0
  if (!is.finite(current)) current <- history
  (1 - preseason_weight) * current + preseason_weight * history
}

team_home_field_as_of <- function(team, season, week, kickoff, games,
                                  default_hfa = 2.4, prior_games = 30) {
  power_diff <- games$home_power - games$away_power
  usable <- raw_history_eligible(games) & games$home == team &
    !as.logical(games$neutral_site) &
    games$season >= season - 4L & games$kickoff < kickoff &
    is.finite(games$margin) & is.finite(power_diff)
  usable[is.na(usable)] <- FALSE
  residuals <- games$margin[usable] - power_diff[usable]
  n <- sum(is.finite(residuals))
  if (!n) return(default_hfa)
  raw <- mean(residuals, na.rm = TRUE)
  shrink <- n / (n + prior_games)
  default_hfa + shrink * (raw - default_hfa)
}

build_team_pregame_snapshots <- function(team_games, games, power_ratings, config) {
  assert_columns(team_games,
                 c("game_id", "team", "season", "week", "model_week", "kickoff",
                   "offense_epa", "defense_epa", "net_efficiency"), "team_games")
  power_key <- paste(power_ratings$team, power_ratings$season,
                     power_ratings$model_week,
                     sep = "\r")
  games$home_power <- power_ratings$power_rating[
    match(paste(games$home, games$season, games$model_week, sep = "\r"), power_key)
  ]
  games$away_power <- power_ratings$power_rating[
    match(paste(games$away, games$season, games$model_week, sep = "\r"), power_key)
  ]
  games$home_power[!is.finite(games$home_power)] <- 0
  games$away_power[!is.finite(games$away_power)] <- 0
  game_index <- match(team_games$game_id, games$game_id)
  if (any(is.na(game_index))) {
    stop("Team-game rows are missing matching historical games.", call. = FALSE)
  }
  team_games$feature_eligible <- raw_history_eligible(games[game_index, , drop = FALSE])

  metric_names <- c(
    "offense_epa", "defense_epa", "special_teams_rating", "pass_epa", "rush_epa",
    "success_rate", "havoc_allowed", "havoc_generated", "turnover_rate_regressed"
  )
  for (metric in setdiff(metric_names, names(team_games))) team_games[[metric]] <- NA_real_
  groups <- split(team_games, team_games$team)
  rows <- vector("list", nrow(team_games))
  k <- 1L
  for (team in names(groups)) {
    x <- groups[[team]]
    x <- x[order(x$season, x$model_week, x$kickoff, x$game_id), , drop = FALSE]
    for (i in seq_len(nrow(x))) {
      current_games <- x$season == x$season[i] & x$model_week < x$model_week[i]
      current <- current_games & x$feature_eligible
      prior <- x$season == x$season[i] - 1L & x$feature_eligible
      trailing <- x$season < x$season[i] & x$season >= x$season[i] - 3L &
        x$feature_eligible
      current_values <- x$net_efficiency[current]
      prior_value <- mean_or_na(x$net_efficiency[prior])
      trailing_value <- mean_or_na(x$net_efficiency[trailing])
      preseason_prior <- if (is.finite(prior_value) && is.finite(trailing_value)) {
        0.65 * prior_value + 0.35 * trailing_value
      } else if (is.finite(prior_value)) prior_value else trailing_value
      game <- games[match(x$game_id[i], games$game_id), , drop = FALSE]
      phase_week <- if (game$postseason_type != "regular") 99L else x$week[i]
      row <- data.frame(
        game_id = x$game_id[i], team = team, season = x$season[i],
        week = x$week[i], model_week = x$model_week[i],
        games_played = sum(current_games),
        source_games = sum(current & is.finite(x$net_efficiency)),
        recent_3 = tail_mean_or_na(current_values, 3),
        recent_6 = tail_mean_or_na(current_values, 6),
        season_to_date = mean_or_na(current_values),
        prior_season = prior_value, trailing_3yr = trailing_value,
        preseason_prior = preseason_prior,
        qb_continuity = NA_real_, roster_continuity = NA_real_,
        staff_continuity = NA_real_, stringsAsFactors = FALSE
      )
      for (metric in metric_names) {
        current_metric <- mean_or_na(x[[metric]][current])
        prior_metric <- mean_or_na(x[[metric]][prior])
        row[[metric]] <- blend_current_with_history(
          current_metric, prior_metric, phase_week, config
        )
      }
      rating_match <- match(
        paste(team, x$season[i], x$model_week[i], sep = "\r"), power_key
      )
      row$power_rating <- if (!is.na(rating_match)) power_ratings$power_rating[rating_match] else 0
      row$home_field_rating <- team_home_field_as_of(
        team, x$season[i], x$week[i], x$kickoff[i], games
      )
      rows[[k]] <- row
      k <- k + 1L
    }
  }
  rows <- rows[seq_len(k - 1L)]
  snapshots <- do.call(rbind, rows)
  rownames(snapshots) <- NULL
  list(snapshots = snapshots, games = games)
}

prefix_snapshot_columns <- function(data, prefix) {
  keys <- c("game_id", "team", "season", "week", "model_week")
  names(data)[!names(data) %in% keys] <- paste0(prefix, names(data)[!names(data) %in% keys])
  data
}

build_historical_matchup_table <- function(games, snapshots, config) {
  home <- snapshots[
    snapshots$team == games$home[match(snapshots$game_id, games$game_id)], , drop = FALSE
  ]
  away <- snapshots[
    snapshots$team == games$away[match(snapshots$game_id, games$game_id)], , drop = FALSE
  ]
  home <- prefix_snapshot_columns(home, "home_")
  away <- prefix_snapshot_columns(away, "away_")
  home$team <- NULL
  away$team <- NULL
  home$season <- NULL
  away$season <- NULL
  home$week <- NULL
  away$week <- NULL
  home$model_week <- NULL
  away$model_week <- NULL
  table <- merge(games, home, by = "game_id", all.x = TRUE, sort = FALSE)
  table <- merge(table, away, by = "game_id", all.x = TRUE, sort = FALSE)
  table <- table[match(games$game_id, table$game_id), , drop = FALSE]

  base_metrics <- c(
    "power_rating", "offense_epa", "defense_epa", "special_teams_rating",
    "pass_epa", "rush_epa", "success_rate", "havoc_allowed", "havoc_generated",
    "turnover_rate_regressed", "recent_3", "recent_6", "season_to_date",
    "prior_season", "trailing_3yr", "preseason_prior", "qb_continuity",
    "roster_continuity", "staff_continuity", "games_played", "source_games"
  )
  for (metric in base_metrics) {
    home_name <- paste0("home_", metric)
    away_name <- paste0("away_", metric)
    if (all(c(home_name, away_name) %in% names(table))) {
      table[[paste0(metric, "_diff")]] <- table[[home_name]] - table[[away_name]]
    }
  }
  table$phase_week <- ifelse(table$postseason_type == "regular", table$week, 99L)
  table$game_phase <- ifelse(
    table$postseason_type == "regular", game_phase(table$week), "postseason"
  )
  table$preseason_weight <- preseason_feature_weight(table$phase_week, config)
  table$prior_history_weight <- rowMeans(cbind(
    prior_season_feature_weight(table$phase_week, table$home_games_played, config),
    prior_season_feature_weight(table$phase_week, table$away_games_played, config)
  ))
  for (feature in intersect(
    c("preseason_prior_diff", "qb_continuity_diff", "roster_continuity_diff",
      "staff_continuity_diff"), names(table)
  )) table[[feature]] <- table[[feature]] * table$preseason_weight
  for (feature in intersect(c("prior_season_diff", "trailing_3yr_diff"), names(table))) {
    table[[feature]] <- table[[feature]] * table$prior_history_weight
  }
  table$challenger_team_home_field_points <- ifelse(
    as.logical(table$neutral_site), 0,
    ifelse(is.finite(table$home_home_field_rating), table$home_home_field_rating, 2.4)
  )
  table$home_field_points <- resolve_home_field(table$neutral_site)
  table$pregame_expected_margin <- table$power_rating_diff + table$home_field_points
  table$line_source <- "cfbfastR_pbp_embedded"
  table
}

foundation_qa <- function(pbp, games, team_games, training, coach_result,
                          coach_features) {
  pbp_rows <- if (length(pbp) == 1L && is.numeric(pbp)) pbp else nrow(pbp)
  postseason <- games$source_season_type == "postseason"
  missing_event_label <- postseason &
    (is.na(games$event_headline) | !nzchar(games$event_headline)) &
    (is.na(games$schedule_notes) | !nzchar(games$schedule_notes))
  numeric_features <- football_feature_names(training)
  missing_feature_rate <- if (length(numeric_features)) {
    mean(!is.finite(as.matrix(training[numeric_features])))
  } else 1
  coach_missing <- as.logical(coach_features$training$coach_mapping_missing)
  fbs_fbs <- games$home_level == "fbs" & games$away_level == "fbs"
  missing_fbs_coach <- coach_missing & !is.na(fbs_fbs) & fbs_fbs
  duplicate_games <- sum(duplicated(games$game_id))
  missing_postseason_labels <- sum(missing_event_label, na.rm = TRUE)
  assignment_reviews <- sum(coach_result$assignments$needs_review, na.rm = TRUE)
  fallback_games <- sum(games$schedule_source == "espn_fbs_fallback", na.rm = TRUE)
  pbp_gap_games <- sum(games$schedule_source == "cfbfastR_pbp_schedule_gap", na.rm = TRUE)
  games_without_pbp <- sum(!as.logical(games$pbp_available), na.rm = TRUE)
  metrics <- data.frame(
    metric = c(
      "pbp_rows", "games", "team_game_rows", "training_rows",
      "duplicate_game_ids", "postseason_games_without_event_label",
      "training_rows_missing_fbs_fbs_coach_mapping",
      "training_rows_missing_any_coach_mapping", "coach_assignment_review_rows",
      "schedule_rows_from_espn_fallback", "schedule_rows_recovered_from_pbp",
      "schedule_games_without_pbp",
      "conference_championship_games", "cfp_games",
      "numeric_feature_missing_rate"
    ),
    value = c(
      pbp_rows, nrow(games), nrow(team_games), nrow(training),
      duplicate_games, missing_postseason_labels,
      sum(missing_fbs_coach, na.rm = TRUE), sum(coach_missing, na.rm = TRUE),
      assignment_reviews, fallback_games, pbp_gap_games, games_without_pbp,
      sum(games$conference_championship, na.rm = TRUE),
      sum(games$is_cfp, na.rm = TRUE),
      missing_feature_rate
    ),
    severity = c(
      "info", "info", "info", "info",
      ifelse(duplicate_games > 0, "error", "pass"),
      ifelse(missing_postseason_labels > 0, "error", "pass"),
      ifelse(any(missing_fbs_coach), "error", "pass"),
      ifelse(any(coach_missing), "warning", "pass"),
      ifelse(assignment_reviews > 0, "warning", "pass"),
      ifelse(fallback_games > 0, "warning", "pass"),
      ifelse(pbp_gap_games > 0, "warning", "pass"),
      ifelse(games_without_pbp > 0, "warning", "pass"),
      "info", "info",
      ifelse(missing_feature_rate > 0.20, "warning", "info")
    ), stringsAsFactors = FALSE
  )
  metrics
}

build_historical_foundation <- function(config, seasons = 2020:2025,
                                        coach_source = file.path(config$project_dir,
                                                                 "coach_ratings.csv"),
                                        refresh_pbp = FALSE,
                                        refresh_schedule = FALSE,
                                        refresh_coaches = FALSE,
                                        refresh_espn = FALSE) {
  seasons <- sort(unique(as.integer(seasons)))
  pbp_rows_total <- 0L
  games_by_season <- list()
  team_games_by_season <- list()
  membership_by_season <- list()
  transitions_by_season <- list()
  for (season in seasons) {
    message("Loading compact PBP for ", season)
    pbp <- load_compact_pbp_season(season, config, refresh = refresh_pbp)
    schedule <- load_cfbd_schedule_season(
      season, config, refresh = refresh_schedule, allow_missing_key = TRUE
    )
    membership <- load_cfbd_fbs_teams_season(
      season, config, refresh = refresh_schedule
    )
    events <- pull_espn_event_metadata(season, pbp, config, refresh = refresh_espn)
    games <- derive_games_from_pbp(
      pbp, events, schedule = schedule, fbs_membership = membership
    )
    games <- filter_completed_historical_games(games)
    team_games <- build_team_game_efficiencies_v2(pbp, games)
    pbp_rows_total <- pbp_rows_total + nrow(pbp)
    games_by_season[[as.character(season)]] <- games
    team_games_by_season[[as.character(season)]] <- team_games
    if (!is.null(membership)) membership_by_season[[as.character(season)]] <- membership
    transitions_by_season[[as.character(season)]] <- pull_public_coach_transitions(
      season, config, refresh = refresh_coaches
    )
  }
  games <- do.call(rbind, games_by_season)
  team_games <- do.call(rbind, team_games_by_season)
  membership <- if (length(membership_by_season)) {
    do.call(rbind, membership_by_season)
  } else data.frame()
  transitions <- do.call(bind_rows_fill, transitions_by_season)
  rownames(games) <- NULL
  rownames(team_games) <- NULL
  games <- games[order(games$season, games$kickoff, games$game_id), ]

  message("Building opponent-adjusted pregame power snapshots")
  power <- power_rating_snapshots(games, config)
  snapshot_result <- build_team_pregame_snapshots(team_games, games, power, config)
  games <- snapshot_result$games
  training <- build_historical_matchup_table(games, snapshot_result$snapshots, config)

  aliases_path <- file.path(config$inbox_dir, "coach_aliases.csv")
  overrides_path <- file.path(config$inbox_dir, "coach_assignment_overrides.csv")
  aliases <- if (file.exists(aliases_path)) aliases_path else NULL
  coach_seasons <- load_cfbd_coach_seasons(
    min_year = min(2014L, min(seasons)), max_year = max(seasons),
    config = config, aliases_path = aliases, refresh = refresh_coaches
  )
  if (is.null(coach_seasons)) {
    message("CFBD coach refresh unavailable; using ", basename(coach_source), ".")
    coach_seasons <- read_legacy_coach_seasons(coach_source, aliases)
  }
  identities <- build_coach_identity_table(coach_seasons)
  public_overrides <- build_public_transition_overrides(
    transitions, games, coach_seasons = coach_seasons
  )
  manual_overrides <- read_csv_if_present(overrides_path, required = FALSE)
  if (!is.null(manual_overrides) && nrow(manual_overrides)) {
    manual_overrides$team <- canonical_team(manual_overrides$team)
    manual_keys <- unique(paste(manual_overrides$team, manual_overrides$season,
                                sep = "\r"))
    if (nrow(public_overrides)) {
      public_overrides <- public_overrides[
        !paste(public_overrides$team, public_overrides$season, sep = "\r") %in%
          manual_keys, , drop = FALSE
      ]
    }
    manual_defaults <- list(
      source = "manual_override", assignment_confidence = "confirmed",
      source_games = NA_integer_, schedule_games = NA_integer_, needs_review = FALSE,
      inference_reason = "manual_override", effective_date = NA_character_,
      source_url = NA_character_
    )
    for (column in names(manual_defaults)) {
      if (!column %in% names(manual_overrides)) {
        manual_overrides[[column]] <- manual_defaults[[column]]
      }
    }
  } else manual_overrides <- NULL
  overrides <- bind_rows_fill(public_overrides, manual_overrides)
  if (!nrow(overrides)) overrides <- NULL
  assignment_source <- coach_seasons[coach_seasons$season %in% seasons, ]
  coach_result <- infer_coach_assignments(assignment_source, games, overrides)

  message("Building leakage-safe coach game history")
  modern <- build_modern_coach_history(training, coach_result$assignments)
  prior <- legacy_coach_prior_history(coach_seasons, modern_start = min(seasons))
  prior <- prior[prior$season < min(seasons), , drop = FALSE]
  coach_history <- rbind(prior, modern$history)
  coach_history <- coach_history[order(coach_history$season, coach_history$week,
                                       coach_history$coach_id), , drop = FALSE]
  coach_features <- attach_coach_features_to_history(
    training, coach_result$assignments, coach_history, config
  )
  training <- coach_features$training
  training$feature_cutoff_rule <- "strictly_prior_week"
  training$model_era_eligible <- training$season >= config$training$covid_season
  training$ats_eligible <- ats_training_eligible(training)

  qa <- foundation_qa(
    pbp_rows_total, games, team_games, training, coach_result, coach_features
  )
  qa <- rbind(
    qa,
    data.frame(
      metric = c("public_coach_transition_rows", "public_transition_assignment_rows"),
      value = c(nrow(transitions), nrow(public_overrides)),
      severity = c("info", "info"), stringsAsFactors = FALSE
    )
  )
  list(
    seasons = seasons, games = games, team_games = team_games,
    fbs_membership = membership, coach_transitions = transitions,
    team_snapshots = snapshot_result$snapshots, training_games = training,
    coach_seasons = coach_seasons, coach_identities = identities,
    coach_assignments = coach_result$assignments,
    coach_assignment_qa = coach_result$qa,
    coach_history = coach_history, coach_ratings = coach_features$ratings,
    qa = qa
  )
}

write_foundation_csv <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  invisible(path)
}

write_historical_foundation <- function(result, config, overwrite = FALSE) {
  targets <- list(
    training_games = file.path(config$inbox_dir, "training_games.csv"),
    coach_assignments = file.path(config$inbox_dir, "coach_assignments.csv"),
    coach_history = file.path(config$inbox_dir, "coach_history.csv"),
    historical_games = file.path(config$data_dir, "historical_games.csv"),
    historical_team_games = file.path(config$data_dir, "historical_team_games.csv"),
    historical_fbs_membership = file.path(config$data_dir,
                                          "historical_fbs_membership.csv"),
    historical_coach_seasons = file.path(config$data_dir,
                                         "historical_coach_seasons.csv"),
    historical_coach_ratings = file.path(config$data_dir,
                                         "historical_coach_ratings.csv"),
    coach_identities = file.path(config$data_dir, "coach_identities.csv"),
    coach_transitions = file.path(config$data_dir, "coach_transitions.csv"),
    coach_assignment_qa = file.path(config$output_dir, "foundation",
                                    "coach_assignment_qa.csv"),
    foundation_qa = file.path(config$output_dir, "foundation", "foundation_qa.csv")
  )
  protected <- targets[c("training_games", "coach_assignments", "coach_history")]
  populated <- names(protected)[vapply(protected, function(path) {
    if (!file.exists(path)) return(FALSE)
    data <- tryCatch(utils::read.csv(path, nrows = 1), error = function(e) data.frame())
    nrow(data) > 0
  }, logical(1))]
  if (length(populated) && !overwrite) {
    stop("Foundation outputs already contain rows: ", paste(populated, collapse = ", "),
         ". Rerun with overwrite=TRUE only after reviewing them.", call. = FALSE)
  }
  write_foundation_csv(result$training_games, targets$training_games)
  write_foundation_csv(result$coach_assignments, targets$coach_assignments)
  write_foundation_csv(result$coach_history, targets$coach_history)
  write_foundation_csv(result$games, targets$historical_games)
  write_foundation_csv(result$team_games, targets$historical_team_games)
  write_foundation_csv(result$fbs_membership, targets$historical_fbs_membership)
  write_foundation_csv(result$coach_seasons, targets$historical_coach_seasons)
  write_foundation_csv(result$coach_ratings, targets$historical_coach_ratings)
  write_foundation_csv(result$coach_identities, targets$coach_identities)
  write_foundation_csv(result$coach_transitions, targets$coach_transitions)
  write_foundation_csv(result$coach_assignment_qa, targets$coach_assignment_qa)
  write_foundation_csv(result$qa, targets$foundation_qa)
  persisted <- v2_persist_foundation(result, config)
  targets$database <- persisted$database
  targets$foundation_run_id <- persisted$run_id
  targets
}
