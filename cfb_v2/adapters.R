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
    kickoff = if (!is.na(kickoff)) parse_utc_datetime(raw[[kickoff]]) else as.POSIXct(NA),
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

normalize_current_coach_table <- function(raw, season, captured_at = Sys.time(),
                                          source_url = NA_character_) {
  team <- first_existing_column(raw, c("Team", "team"), label = "current coach team")
  coach <- first_existing_column(
    raw, c("Head coach", "head_coach", "coach"), label = "current head coach"
  )
  first_season <- first_existing_column(
    raw, c("First season", "first_season"), required = FALSE
  )
  first_value <- if (!is.na(first_season)) as.character(raw[[first_season]]) else NA_character_
  first_value <- suppressWarnings(as.integer(sub(".*?([0-9]{4}).*", "\\1", first_value)))
  out <- data.frame(
    source_team = trimws(gsub("\\[[^]]+\\]", "", as.character(raw[[team]]))),
    coach_name = trimws(gsub("\\[[^]]+\\]", "", as.character(raw[[coach]]))),
    first_season = first_value,
    season = as.integer(season),
    captured_at = as.POSIXct(captured_at, tz = "UTC"),
    source_url = as.character(source_url),
    stringsAsFactors = FALSE
  )
  out <- out[nzchar(out$source_team) & nzchar(out$coach_name), , drop = FALSE]
  out <- out[!duplicated(out$source_team), , drop = FALSE]
  rownames(out) <- NULL
  out
}

pull_current_fbs_coaches <- function(season, cache_path = NULL, refresh = FALSE,
                                     captured_at = Sys.time()) {
  if (!refresh && !is.null(cache_path) && file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    required <- c("source_team", "coach_name", "season", "captured_at", "source_url")
    if (all(required %in% names(cached))) return(cached)
  }
  require_v2_package("rvest")
  source_url <- paste0(
    "https://en.wikipedia.org/wiki/",
    "List_of_current_NCAA_Division_I_FBS_football_coaches"
  )
  tables <- rvest::html_table(rvest::read_html(source_url), fill = TRUE)
  if (!length(tables)) stop("Current FBS coach source returned no tables.", call. = FALSE)
  raw <- as.data.frame(tables[[which.max(vapply(tables, nrow, integer(1)))]],
                       stringsAsFactors = FALSE)
  out <- normalize_current_coach_table(raw, season, captured_at, source_url)
  if (nrow(out) < 130L) {
    stop("Current FBS coach source returned only ", nrow(out), " teams.", call. = FALSE)
  }
  if (!is.null(cache_path)) {
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cache_path, compress = "xz")
  }
  out
}

current_coach_team_key <- function(x) {
  x <- as.character(x)
  vapply(x, function(value) {
    value <- tryCatch(
      iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""),
      error = function(...) value
    )
    bytes <- as.integer(charToRaw(value))
    keep <- (bytes >= 48L & bytes <= 57L) |
      (bytes >= 65L & bytes <= 90L) | (bytes >= 97L & bytes <= 122L)
    tolower(rawToChar(as.raw(bytes[keep])))
  }, character(1), USE.NAMES = FALSE)
}

match_current_fbs_coaches <- function(teams, current_coaches) {
  assert_columns(
    current_coaches,
    c("source_team", "coach_name", "first_season", "season", "source_url"),
    "current FBS coaches"
  )
  teams <- unique(canonical_team(teams))
  aliases <- c(
    "App State" = "Appalachian State",
    "Connecticut" = "UConn",
    "Hawai'i" = "Hawaii",
    "Massachusetts" = "UMass",
    "Miami" = "Miami Hurricanes",
    "Miami (OH)" = "Miami RedHawks",
    "Mississippi" = "Ole Miss",
    "North Carolina State" = "NC State",
    "Southern California" = "USC",
    "Texas" = "Texas Longhorns",
    "UL Monroe" = "Louisiana-Monroe"
  )
  source_key <- current_coach_team_key(current_coaches$source_team)
  rows <- lapply(teams, function(team) {
    lookup <- if (team %in% names(aliases)) unname(aliases[team]) else team
    key <- current_coach_team_key(lookup)
    candidates <- which(startsWith(source_key, key))
    if (!length(candidates)) {
      stop("No current head coach matched team: ", team, call. = FALSE)
    }
    candidates <- candidates[order(nchar(source_key[candidates]),
                                   current_coaches$source_team[candidates])]
    hit <- candidates[1]
    data.frame(
      team = team,
      coach_id = coach_id_from_name(current_coaches$coach_name[hit]),
      coach_name = current_coaches$coach_name[hit],
      first_season = current_coaches$first_season[hit],
      source_team = current_coaches$source_team[hit],
      source_url = current_coaches$source_url[hit],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  assert_unique_keys(out, "team", "matched current FBS coaches")
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
  opening <- first_existing_column(
    raw, c("opening_home_spread", "spread_open", "spreadOpen"), FALSE
  )
  total <- first_existing_column(raw, c("total", "over_under", "overUnder"), FALSE)
  data.frame(
    game_id = as.character(raw[[game]]),
    provider = if (!is.na(provider)) as.character(raw[[provider]]) else "unknown",
    captured_at = as.POSIXct(captured_at, tz = "UTC"), snapshot_type = "source",
    home_spread = as.numeric(raw[[spread]]),
    opening_home_spread = if (!is.na(opening)) as.numeric(raw[[opening]]) else NA_real_,
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
    source_opening <- if ("opening_home_spread" %in% names(latest_by_provider)) {
      as.numeric(latest_by_provider$opening_home_spread)
    } else numeric()
    source_opening <- source_opening[is.finite(source_opening)]
    opening_value <- if (length(source_opening)) stats::median(source_opening) else
      stats::median(opening_by_provider$home_spread, na.rm = TRUE)
    provider_value <- function(pattern) {
      hit <- grepl(pattern, latest_by_provider$provider, ignore.case = TRUE)
      if (any(hit)) latest_by_provider$home_spread[which(hit)[1]] else NA_real_
    }
    data.frame(
      game_id = game_id,
      market_home_spread = stats::median(latest_by_provider$home_spread, na.rm = TRUE),
      market_total = if ("total" %in% names(latest_by_provider))
        stats::median(latest_by_provider$total, na.rm = TRUE) else NA_real_,
      opening_home_spread = opening_value,
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
