first_existing_column <- function(data, candidates, required = TRUE, label = NULL) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) return(hit[1])
  if (required) {
    stop("Could not find ", ifelse(is.null(label), paste(candidates, collapse = "/"), label),
         " in source columns.", call. = FALSE)
  }
  NA_character_
}

read_csv_if_present <- function(path, required = FALSE) {
  if (!file.exists(path)) {
    if (required) stop("Required input is missing: ", path, call. = FALSE)
    return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA", "N/A", "null"))
}

normalize_public_schedule <- function(raw) {
  game <- first_existing_column(raw, c("game_id", "id", "id_game"), label = "game id")
  home <- first_existing_column(raw, c("home", "home_team", "homeTeam"), label = "home team")
  away <- first_existing_column(raw, c("away", "away_team", "awayTeam"), label = "away team")
  season <- first_existing_column(raw, c("season", "year"), label = "season")
  week <- first_existing_column(raw, c("week"), label = "week")
  kickoff <- first_existing_column(raw, c("kickoff", "start_date", "startDate"), FALSE)
  neutral <- first_existing_column(raw, c("neutral_site", "neutralSite"), FALSE)
  season_type <- first_existing_column(raw, c("season_type", "seasonType"), FALSE)
  home_points <- first_existing_column(raw, c("home_points", "home_score", "homePoints"), FALSE)
  away_points <- first_existing_column(raw, c("away_points", "away_score", "awayPoints"), FALSE)

  out <- data.frame(
    game_id = as.character(raw[[game]]), season = as.integer(raw[[season]]),
    week = as.integer(raw[[week]]),
    kickoff = if (!is.na(kickoff)) as.POSIXct(raw[[kickoff]], tz = "UTC") else as.POSIXct(NA),
    home = canonical_team(raw[[home]]), away = canonical_team(raw[[away]]),
    neutral_site = if (!is.na(neutral)) as.logical(raw[[neutral]]) else FALSE,
    venue = if ("venue" %in% names(raw)) as.character(raw$venue) else NA_character_,
    home_level = if ("home_level" %in% names(raw)) raw$home_level else "fbs",
    away_level = if ("away_level" %in% names(raw)) raw$away_level else "fbs",
    postseason_type = if (!is.na(season_type)) tolower(raw[[season_type]]) else "regular",
    conference_championship = if ("conference_championship" %in% names(raw))
      as.logical(raw$conference_championship) else FALSE,
    is_cfp = if ("is_cfp" %in% names(raw)) as.logical(raw$is_cfp) else FALSE,
    home_score = if (!is.na(home_points)) as.numeric(raw[[home_points]]) else NA_real_,
    away_score = if (!is.na(away_points)) as.numeric(raw[[away_points]]) else NA_real_,
    stringsAsFactors = FALSE
  )
  out$postseason_type[
    out$postseason_type %in% c("postseason") & !out$is_cfp & !out$conference_championship
  ] <- "bowl"
  standardize_schedule(out)
}

pull_public_schedule <- function(seasons, cache_path = NULL, refresh = FALSE) {
  if (!refresh && !is.null(cache_path) && file.exists(cache_path)) return(readRDS(cache_path))
  require_v2_package("cfbfastR")
  raw <- cfbfastR::load_cfb_schedules(seasons = seasons)
  out <- normalize_public_schedule(raw)
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path)
  }
  out
}

pull_public_pbp <- function(seasons, cache_path = NULL, refresh = FALSE) {
  if (!refresh && !is.null(cache_path) && file.exists(cache_path)) return(readRDS(cache_path))
  require_v2_package("cfbfastR")
  raw <- cfbfastR::load_cfb_pbp(seasons = seasons)
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(raw, cache_path)
  }
  raw
}

normalize_betting_lines <- function(raw, captured_at = Sys.time()) {
  game <- first_existing_column(raw, c("game_id", "id", "id_game"), label = "line game id")
  provider <- first_existing_column(raw, c("provider", "provider_name", "line_provider"),
                                    FALSE)
  spread <- first_existing_column(raw, c("home_spread", "spread", "spread_line"),
                                  label = "home spread")
  total <- first_existing_column(raw, c("total", "over_under", "overUnder"), FALSE)
  data.frame(
    game_id = as.character(raw[[game]]),
    provider = if (!is.na(provider)) as.character(raw[[provider]]) else "unknown",
    captured_at = as.POSIXct(captured_at, tz = "UTC"), snapshot_type = "source",
    home_spread = as.numeric(raw[[spread]]),
    total = if (!is.na(total)) as.numeric(raw[[total]]) else NA_real_,
    home_price = if ("home_price" %in% names(raw)) as.integer(raw$home_price) else NA_integer_,
    away_price = if ("away_price" %in% names(raw)) as.integer(raw$away_price) else NA_integer_,
    source_url = if ("source_url" %in% names(raw)) raw$source_url else NA_character_,
    stringsAsFactors = FALSE
  )
}

pull_cfbd_lines <- function(season, week, captured_at = Sys.time()) {
  if (!nzchar(Sys.getenv("CFBD_API_KEY"))) {
    stop("CFBD_API_KEY is not set. Supply inbox/lines.csv for DraftKings/FanDuel data.",
         call. = FALSE)
  }
  require_v2_package("cfbfastR")
  raw <- cfbfastR::cfbd_betting_lines(year = season, week = week)
  normalize_betting_lines(raw, captured_at)
}

select_article_lines <- function(lines, game_ids, as_of) {
  if (is.null(lines) || !nrow(lines)) {
    return(data.frame(game_id = game_ids, market_home_spread = NA_real_,
                      market_total = NA_real_, opening_home_spread = NA_real_,
                      draftkings_home_spread = NA_real_, fanduel_home_spread = NA_real_,
                      best_home_spread = NA_real_, best_away_spread = NA_real_,
                      line_source = NA_character_))
  }
  assert_columns(lines, c("game_id", "provider", "captured_at", "home_spread"), "lines")
  lines$captured_at <- as.POSIXct(lines$captured_at, tz = "UTC")
  lines <- lines[lines$captured_at <= as.POSIXct(as_of, tz = "UTC") &
                   lines$game_id %in% game_ids, , drop = FALSE]
  main <- tolower(lines$provider) %in% c("draftkings", "draft kings", "fanduel", "fan duel")
  lines <- lines[main, , drop = FALSE]
  rows <- lapply(game_ids, function(game_id) {
    x <- lines[lines$game_id == game_id, , drop = FALSE]
    if (!nrow(x)) return(data.frame(
      game_id = game_id, market_home_spread = NA_real_, market_total = NA_real_,
      opening_home_spread = NA_real_, draftkings_home_spread = NA_real_,
      fanduel_home_spread = NA_real_, best_home_spread = NA_real_,
      best_away_spread = NA_real_, line_source = NA_character_, stringsAsFactors = FALSE
    ))
    by_provider <- split(x, tolower(x$provider))
    latest_by_provider <- do.call(rbind, lapply(by_provider, function(y) {
      y[which.max(y$captured_at), , drop = FALSE]
    }))
    opening_by_provider <- do.call(rbind, lapply(by_provider, function(y) {
      y[which.min(y$captured_at), , drop = FALSE]
    }))
    provider_value <- function(pattern) {
      hit <- grepl(pattern, latest_by_provider$provider, ignore.case = TRUE)
      if (any(hit)) latest_by_provider$home_spread[which(hit)[1]] else NA_real_
    }
    data.frame(
      game_id = game_id,
      market_home_spread = stats::median(latest_by_provider$home_spread, na.rm = TRUE),
      market_total = if ("total" %in% names(latest_by_provider))
        stats::median(latest_by_provider$total, na.rm = TRUE) else NA_real_,
      opening_home_spread = stats::median(opening_by_provider$home_spread, na.rm = TRUE),
      draftkings_home_spread = provider_value("draft"),
      fanduel_home_spread = provider_value("fan"),
      best_home_spread = max(latest_by_provider$home_spread, na.rm = TRUE),
      best_away_spread = max(-latest_by_provider$home_spread, na.rm = TRUE),
      line_source = paste(sort(unique(latest_by_provider$provider)), collapse = "+"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

build_team_game_efficiency <- function(plays) {
  plays <- filter_competitive_plays(plays)
  game_col <- first_existing_column(plays, c("game_id", "id_game"), label = "play game id")
  offense_col <- first_existing_column(plays, c("posteam", "pos_team", "offense"),
                                       label = "possession team")
  defense_col <- first_existing_column(plays, c("defteam", "def_pos_team", "defense"),
                                       label = "defense team")
  epa_col <- first_existing_column(plays, c("EPA", "epa"), label = "EPA")
  play_type_col <- first_existing_column(plays, c("play_type", "playType"), FALSE)
  base <- data.frame(
    game_id = as.character(plays[[game_col]]),
    offense = canonical_team(plays[[offense_col]]),
    defense = canonical_team(plays[[defense_col]]),
    epa = as.numeric(plays[[epa_col]]),
    play_type = if (!is.na(play_type_col)) as.character(plays[[play_type_col]]) else "",
    stringsAsFactors = FALSE
  )
  base <- base[is.finite(base$epa) & nzchar(base$offense) & nzchar(base$defense), ]
  scrimmage <- !grepl("punt|kickoff|field goal|extra point", base$play_type,
                      ignore.case = TRUE)
  offense <- aggregate(epa ~ game_id + offense, base[scrimmage, ], mean)
  names(offense) <- c("game_id", "team", "offense_epa")
  defense <- aggregate(epa ~ game_id + defense, base[scrimmage, ], mean)
  names(defense) <- c("game_id", "team", "defense_epa_allowed")
  special <- aggregate(epa ~ game_id + offense,
                       base[!scrimmage, ], mean)
  names(special) <- c("game_id", "team", "special_teams_epa")
  out <- merge(offense, defense, by = c("game_id", "team"), all = TRUE)
  out <- merge(out, special, by = c("game_id", "team"), all = TRUE)
  out$net_epa <- out$offense_epa - out$defense_epa_allowed
  out
}
