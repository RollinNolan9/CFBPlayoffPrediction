# Prospective line capture: append timestamped spreads, totals, prices, and books.
# Rscript cfb_v3/capture_lines.R --label=friday [--season=2026 --week=9 --season-type=regular]
#   [--sources=auto|cfbd|odds|cfbd,odds] [--store=cfb_v3/data/line_captures]
# Writes only under the git-ignored store. Never writes or logs API keys.

lc_store_columns <- c(
  "capture_id", "captured_at", "label", "source", "book", "book_title",
  "season", "season_type", "week", "game_id", "odds_event_id", "match_method",
  "kickoff", "kickoff_tbd", "neutral_site", "home", "away", "source_home", "source_away",
  "home_spread", "home_spread_price", "away_spread_price", "spread_open",
  "total", "over_price", "under_price", "total_open",
  "home_moneyline", "away_moneyline", "spread_updated_at", "total_updated_at",
  "minutes_to_kickoff", "pregame"
)
lc_log_columns <- c(
  "capture_id", "captured_at", "label", "source", "season", "season_type", "week",
  "status", "rows", "games", "matched_games", "unmatched_events", "requests_remaining",
  "requests_used", "raw_file", "raw_md5", "message"
)

lc_utc <- function(x) ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

lc_parse_time <- function(x) {
  x <- vapply(x, function(v) if (is.null(v) || !length(v)) NA_character_ else as.character(v),
              character(1), USE.NAMES = FALSE)
  as.POSIXct(sub("(\\.\\d+)?(Z|\\+00:00)$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

lc_value <- function(x, field, missing = NA) {
  v <- x[[field]]
  if (is.null(v) || !length(v)) missing else v
}

lc_num <- function(x, field) suppressWarnings(as.numeric(lc_value(x, field)))

lc_book_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

lc_team_key <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("&", " and ", tolower(x), fixed = TRUE)
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9 ]", " ", x)))
}

# CFBD /games: the game universe used for IDs, kickoff, and neutral-site flags.
lc_parse_cfbd_games <- function(games) {
  if (!length(games)) {
    return(data.frame(game_id = character(), home = character(), away = character(),
                      kickoff = as.POSIXct(character(), tz = "UTC"), kickoff_tbd = logical(),
                      neutral_site = logical(), stringsAsFactors = FALSE))
  }
  data.frame(
    game_id = vapply(games, function(g) as.character(lc_value(g, "id")), character(1)),
    home = vapply(games, function(g) as.character(lc_value(g, "homeTeam", lc_value(g, "home_team"))), character(1)),
    away = vapply(games, function(g) as.character(lc_value(g, "awayTeam", lc_value(g, "away_team"))), character(1)),
    kickoff = lc_parse_time(lapply(games, function(g) lc_value(g, "startDate", lc_value(g, "start_date")))),
    kickoff_tbd = vapply(games, function(g) isTRUE(lc_value(g, "startTimeTBD", FALSE)), logical(1)),
    neutral_site = vapply(games, function(g) isTRUE(lc_value(g, "neutralSite", FALSE)), logical(1)),
    stringsAsFactors = FALSE
  )
}

# CFBD /lines: one row per game and provider. CFBD spreads are home-team spreads;
# CFBD supplies no spread/total prices and no provider update time.
lc_parse_cfbd_lines <- function(lines, games, captured_at, season, season_type, week) {
  rows <- list()
  for (g in lines) {
    id <- as.character(lc_value(g, "id"))
    meta <- games[games$game_id == id, , drop = FALSE]
    kickoff <- if (nrow(meta)) meta$kickoff[1] else lc_parse_time(lc_value(g, "startDate"))
    for (q in lc_value(g, "lines", list())) {
      provider <- as.character(lc_value(q, "provider", "unknown"))
      rows[[length(rows) + 1L]] <- data.frame(
        source = "cfbd", book = lc_book_key(provider), book_title = provider,
        season = as.integer(lc_value(g, "season", season)),
        season_type = as.character(lc_value(g, "seasonType", season_type)),
        week = as.integer(lc_value(g, "week", week)), game_id = id,
        odds_event_id = NA_character_, match_method = "cfbd_id",
        kickoff = kickoff,
        kickoff_tbd = if (nrow(meta)) meta$kickoff_tbd[1] else NA,
        neutral_site = if (nrow(meta)) meta$neutral_site[1] else NA,
        home = as.character(lc_value(g, "homeTeam")), away = as.character(lc_value(g, "awayTeam")),
        source_home = as.character(lc_value(g, "homeTeam")),
        source_away = as.character(lc_value(g, "awayTeam")),
        home_spread = lc_num(q, "spread"), home_spread_price = NA_real_,
        away_spread_price = NA_real_, spread_open = lc_num(q, "spreadOpen"),
        total = lc_num(q, "overUnder"), over_price = NA_real_, under_price = NA_real_,
        total_open = lc_num(q, "overUnderOpen"),
        home_moneyline = lc_num(q, "homeMoneyline"), away_moneyline = lc_num(q, "awayMoneyline"),
        spread_updated_at = as.POSIXct(NA, tz = "UTC"), total_updated_at = as.POSIXct(NA, tz = "UTC"),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(NULL)
  lc_finish_quotes(do.call(rbind, rows), captured_at)
}

lc_aliases <- function(path) {
  if (is.null(path) || !file.exists(path)) return(stats::setNames(character(), character()))
  a <- utils::read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  if (!all(c("odds_api_name", "cfbd_name") %in% names(a))) {
    stop("Team aliases need odds_api_name and cfbd_name columns: ", path, call. = FALSE)
  }
  stats::setNames(a$cfbd_name, lc_team_key(a$odds_api_name))
}

# Odds API names are "School Mascot"; CFBD names are the school. Match both teams by
# longest school-name prefix within 36 hours of the CFBD kickoff, either orientation.
lc_match_event <- function(home, away, commence, games, aliases = character()) {
  none <- list(game_id = NA_character_, swapped = NA, method = "unmatched")
  if (!nrow(games)) return(none)
  name_score <- function(odds_name, cfbd_name) {
    key <- lc_team_key(odds_name)
    if (key %in% names(aliases)) return(ifelse(lc_team_key(aliases[[key]]) == lc_team_key(cfbd_name), 1000, 0))
    target <- lc_team_key(cfbd_name)
    ifelse(key == target | startsWith(key, paste0(target, " ")), nchar(target), 0)
  }
  near <- !is.na(games$kickoff) & !is.na(commence) &
    abs(as.numeric(difftime(games$kickoff, commence, units = "hours"))) <= 36
  if (!any(near)) return(none)
  g <- games[near, , drop = FALSE]
  same <- name_score(home, g$home) * (name_score(away, g$away) > 0)
  swap <- name_score(home, g$away) * (name_score(away, g$home) > 0)
  same_away <- name_score(away, g$away) * (name_score(home, g$home) > 0)
  swap_away <- name_score(away, g$home) * (name_score(home, g$away) > 0)
  score <- pmax(same + same_away, swap + swap_away)
  if (max(score) > 0) {
    best <- which(score == max(score))
    if (length(best) > 1L) return(list(game_id = NA_character_, swapped = NA, method = "ambiguous"))
    return(list(game_id = g$game_id[best], swapped = (swap + swap_away)[best] > (same + same_away)[best],
                method = "team_names"))
  }
  # One recognized team plus a kickoff within 3 hours (covers unaliased opponent names).
  close <- abs(as.numeric(difftime(g$kickoff, commence, units = "hours"))) <= 3
  one_same <- close & (name_score(home, g$home) > 0 | name_score(away, g$away) > 0)
  one_swap <- close & (name_score(home, g$away) > 0 | name_score(away, g$home) > 0)
  if (sum(one_same | one_swap) == 1L) {
    i <- which(one_same | one_swap)
    if (one_same[i] && one_swap[i]) return(list(game_id = NA_character_, swapped = NA, method = "ambiguous"))
    return(list(game_id = g$game_id[i], swapped = one_swap[i], method = "one_team_and_kickoff"))
  }
  none
}

# The Odds API v4 /odds: one row per event and bookmaker, oriented to the CFBD home team
# when matched. Prices are American odds; update times are the book's market last_update.
lc_parse_odds_events <- function(events, games, captured_at, season, season_type, week,
                                 aliases = character()) {
  rows <- list()
  for (e in events) {
    commence <- lc_parse_time(lc_value(e, "commence_time"))
    odds_home <- as.character(lc_value(e, "home_team"))
    odds_away <- as.character(lc_value(e, "away_team"))
    m <- lc_match_event(odds_home, odds_away, commence, games, aliases)
    meta <- games[games$game_id %in% m$game_id, , drop = FALSE]
    swapped <- isTRUE(m$swapped)
    home <- if (nrow(meta)) meta$home[1] else odds_home
    away <- if (nrow(meta)) meta$away[1] else odds_away
    home_name <- if (swapped) odds_away else odds_home
    away_name <- if (swapped) odds_home else odds_away
    for (b in lc_value(e, "bookmakers", list())) {
      market <- function(key) {
        for (mk in lc_value(b, "markets", list())) if (identical(lc_value(mk, "key"), key)) return(mk)
        NULL
      }
      outcome <- function(mk, name, field) {
        if (is.null(mk)) return(NA_real_)
        for (o in lc_value(mk, "outcomes", list())) {
          if (identical(as.character(lc_value(o, "name")), name)) return(lc_num(o, field))
        }
        NA_real_
      }
      spreads <- market("spreads")
      totals <- market("totals")
      h2h <- market("h2h")
      over <- outcome(totals, "Over", "point")
      rows[[length(rows) + 1L]] <- data.frame(
        source = "odds_api", book = lc_book_key(lc_value(b, "key", "unknown")),
        book_title = as.character(lc_value(b, "title", lc_value(b, "key", "unknown"))),
        season = as.integer(season), season_type = season_type, week = as.integer(week),
        game_id = m$game_id, odds_event_id = as.character(lc_value(e, "id")),
        match_method = m$method,
        kickoff = if (nrow(meta)) meta$kickoff[1] else commence,
        kickoff_tbd = if (nrow(meta)) meta$kickoff_tbd[1] else NA,
        neutral_site = if (nrow(meta)) meta$neutral_site[1] else NA,
        home = home, away = away, source_home = odds_home, source_away = odds_away,
        home_spread = outcome(spreads, home_name, "point"),
        home_spread_price = outcome(spreads, home_name, "price"),
        away_spread_price = outcome(spreads, away_name, "price"), spread_open = NA_real_,
        total = if (is.finite(over)) over else outcome(totals, "Under", "point"),
        over_price = outcome(totals, "Over", "price"), under_price = outcome(totals, "Under", "price"),
        total_open = NA_real_,
        home_moneyline = outcome(h2h, home_name, "price"), away_moneyline = outcome(h2h, away_name, "price"),
        spread_updated_at = lc_parse_time(if (is.null(spreads)) NA else lc_value(spreads, "last_update")),
        total_updated_at = lc_parse_time(if (is.null(totals)) NA else lc_value(totals, "last_update")),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(NULL)
  lc_finish_quotes(do.call(rbind, rows), captured_at)
}

lc_finish_quotes <- function(x, captured_at) {
  x$captured_at <- captured_at
  x$minutes_to_kickoff <- round(as.numeric(difftime(x$kickoff, captured_at, units = "mins")), 1)
  x$pregame <- !is.na(x$kickoff) & x$kickoff > captured_at
  x
}

# Latest calendar week whose last kickoff has not passed.
lc_current_week <- function(calendar, now) {
  if (!length(calendar)) stop("CFBD returned an empty calendar.", call. = FALSE)
  cal <- data.frame(
    week = vapply(calendar, function(x) as.integer(lc_value(x, "week")), integer(1)),
    season_type = vapply(calendar, function(x) as.character(lc_value(x, "seasonType")), character(1)),
    start = lc_parse_time(lapply(calendar, function(x) lc_value(x, "startDate", lc_value(x, "firstGameStart")))),
    end = lc_parse_time(lapply(calendar, function(x) lc_value(x, "lastGameStart", lc_value(x, "endDate")))),
    stringsAsFactors = FALSE
  )
  upcoming <- cal[!is.na(cal$end) & cal$end >= now, , drop = FALSE]
  if (!nrow(upcoming)) stop("No remaining CFBD calendar week; pass --week and --season-type.", call. = FALSE)
  upcoming[order(upcoming$start), , drop = FALSE][1, ]
}

lc_http_json <- function(url, query = list(), bearer = NULL, secret = NULL) {
  headers <- if (is.null(bearer)) httr::add_headers() else
    httr::add_headers(Authorization = paste("Bearer", bearer), Accept = "application/json")
  redact <- function(x) if (is.null(secret) || !nzchar(secret)) x else gsub(secret, "[REDACTED]", x, fixed = TRUE)
  response <- tryCatch(
    httr::GET(url, headers, query = query, httr::timeout(60)),
    error = function(e) stop(redact(conditionMessage(e)), call. = FALSE)
  )
  body <- httr::content(response, as = "raw")
  if (httr::status_code(response) >= 400) {
    text <- substr(rawToChar(body), 1, 300)
    stop("HTTP ", httr::status_code(response), " from ", sub("\\?.*$", "", url), ": ", redact(text),
         call. = FALSE)
  }
  list(body = body, headers = httr::headers(response))
}

lc_fetch_cfbd <- function(endpoint, query) {
  key <- Sys.getenv("CFBD_API_KEY")
  if (!nzchar(key)) stop("CFBD_API_KEY is not set.", call. = FALSE)
  lc_http_json(paste0("https://api.collegefootballdata.com/", endpoint), query, bearer = key, secret = key)
}

lc_fetch_odds <- function(regions, markets) {
  key <- Sys.getenv("ODDS_API_KEY")
  if (!nzchar(key)) stop("ODDS_API_KEY is not set.", call. = FALSE)
  lc_http_json("https://api.the-odds-api.com/v4/sports/americanfootball_ncaaf/odds",
               list(apiKey = key, regions = regions, markets = markets,
                    oddsFormat = "american", dateFormat = "iso"), secret = key)
}

lc_save_raw <- function(body, dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(name, ".json.gz"))
  con <- gzfile(path, "wb")
  on.exit(close(con))
  writeBin(body, con)
  path
}

lc_parse_body <- function(body) jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE)

# Append rows to a CSV whose header must already match; never rewrites existing rows.
lc_append_csv <- function(x, path, columns) {
  missing <- setdiff(columns, names(x))
  for (column in missing) x[[column]] <- NA
  x <- x[, columns, drop = FALSE]
  for (column in names(x)) if (inherits(x[[column]], "POSIXct")) x[[column]] <- lc_utc(x[[column]])
  exists <- file.exists(path) && file.info(path)$size > 0
  if (exists) {
    header <- strsplit(gsub('"', "", readLines(path, n = 1L, warn = FALSE)), ",", fixed = TRUE)[[1]]
    if (!identical(header, columns)) {
      stop("Existing store header differs from this script's schema: ", path,
           ". Start a new store rather than rewriting it.", call. = FALSE)
    }
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  if (!nrow(x) && exists) return(invisible(path))
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = !exists, append = exists,
                     na = "", qmethod = "double", fileEncoding = "UTF-8")
  invisible(path)
}

capture_lines <- function(store, label, season = NULL, week = NULL, season_type = NULL,
                          sources = "auto", aliases_path = NULL, odds_regions = "us",
                          odds_markets = "spreads,totals,h2h", clock = Sys.time,
                          fetch_cfbd = lc_fetch_cfbd, fetch_odds = lc_fetch_odds) {
  if (!grepl("^[a-z0-9_]+$", label)) stop("--label must be lowercase letters, digits, or _.", call. = FALSE)
  started <- clock()
  capture_id <- paste0(format(started, "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", label)
  raw_dir <- file.path(store, "raw", capture_id)
  if (dir.exists(raw_dir)) stop("Capture already exists: ", capture_id, call. = FALSE)
  sources <- strsplit(sources, ",", fixed = TRUE)[[1]]
  if (identical(sources, "auto")) {
    sources <- c("cfbd", if (nzchar(Sys.getenv("ODDS_API_KEY"))) "odds" else "odds_skipped")
  }
  if (!all(sources %in% c("cfbd", "odds", "odds_skipped"))) stop("Unknown --sources value.", call. = FALSE)
  if (is.null(season)) {
    season <- as.integer(format(started, "%Y", tz = "UTC")) -
      (as.integer(format(started, "%m", tz = "UTC")) < 3L)
  }
  logs <- list()
  log_row <- function(source, status, message = "", ...) {
    logs[[length(logs) + 1L]] <<- data.frame(
      capture_id = capture_id, captured_at = lc_utc(clock()), label = label, source = source,
      season = season, season_type = if (is.null(season_type)) NA else season_type,
      week = if (is.null(week)) NA else week, status = status,
      message = trimws(gsub("\\s+", " ", message)), ...,
      stringsAsFactors = FALSE)
  }
  quotes <- list()
  games <- lc_parse_cfbd_games(list())

  if ("cfbd" %in% sources) {
    result <- tryCatch({
      if (is.null(week)) {
        current <- lc_current_week(lc_parse_body(fetch_cfbd("calendar", list(year = season))$body), started)
        week <- current$week
        season_type <- current$season_type
      }
      if (is.null(season_type)) season_type <- "regular"
      query <- list(year = season, week = week, seasonType = season_type)
      games_response <- fetch_cfbd("games", query)
      lines_response <- fetch_cfbd("lines", query)
      captured_at <- clock()
      lc_save_raw(games_response$body, raw_dir, "cfbd_games")
      raw_file <- lc_save_raw(lines_response$body, raw_dir, "cfbd_lines")
      games <- lc_parse_cfbd_games(lc_parse_body(games_response$body))
      q <- lc_parse_cfbd_lines(lc_parse_body(lines_response$body), games, captured_at,
                               season, season_type, week)
      quotes$cfbd <- q
      log_row("cfbd", "ok", rows = if (is.null(q)) 0L else nrow(q),
              games = if (is.null(q)) 0L else length(unique(q$game_id)),
              raw_file = basename(raw_file), raw_md5 = unname(tools::md5sum(raw_file)))
    }, error = function(e) log_row("cfbd", "error", conditionMessage(e)))
  }

  if ("odds_skipped" %in% sources) log_row("odds_api", "skipped", "ODDS_API_KEY is not set.")
  if ("odds" %in% sources) {
    tryCatch({
      response <- fetch_odds(odds_regions, odds_markets)
      captured_at <- clock()
      raw_file <- lc_save_raw(response$body, raw_dir, "odds_api")
      events <- lc_parse_body(response$body)
      if (nrow(games)) {
        horizon <- max(games$kickoff, na.rm = TRUE) + 12 * 3600
        events <- Filter(function(e) isTRUE(lc_parse_time(lc_value(e, "commence_time")) <= horizon), events)
      }
      q <- lc_parse_odds_events(events, games, captured_at, season,
                                if (is.null(season_type)) NA_character_ else season_type,
                                if (is.null(week)) NA_integer_ else week, lc_aliases(aliases_path))
      quotes$odds <- q
      header <- function(name) {
        v <- response$headers[[name]]
        if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))
      }
      matched <- if (is.null(q)) 0L else length(unique(stats::na.omit(q$game_id)))
      log_row("odds_api", "ok", rows = if (is.null(q)) 0L else nrow(q), games = length(events),
              matched_games = matched,
              unmatched_events = if (is.null(q)) 0L else length(unique(q$odds_event_id[is.na(q$game_id)])),
              requests_remaining = header("x-requests-remaining"), requests_used = header("x-requests-used"),
              raw_file = basename(raw_file), raw_md5 = unname(tools::md5sum(raw_file)))
    }, error = function(e) log_row("odds_api", "error", conditionMessage(e)))
  }

  quotes <- Filter(Negate(is.null), quotes)
  all_quotes <- if (length(quotes)) do.call(rbind, unname(quotes)) else data.frame()
  if (nrow(all_quotes)) {
    all_quotes$capture_id <- capture_id
    all_quotes$label <- label
    lc_append_csv(all_quotes, file.path(store, "line_quotes.csv"), lc_store_columns)
  }
  log <- do.call(rbind, lapply(logs, function(x) {
    for (column in setdiff(lc_log_columns, names(x))) x[[column]] <- NA
    x[, lc_log_columns]
  }))
  lc_append_csv(log, file.path(store, "captures.csv"), lc_log_columns)
  invisible(list(capture_id = capture_id, quotes = all_quotes, log = log))
}

# Latest pregame quote per game, source, and book observed at or before `as_of`.
line_quotes_as_of <- function(quotes, as_of, books = NULL) {
  captured <- lc_parse_time(as.list(quotes$captured_at))
  kickoff <- lc_parse_time(as.list(quotes$kickoff))
  keep <- !is.na(captured) & captured <= as_of & (is.na(kickoff) | captured < kickoff) &
    !is.na(quotes$game_id) & nzchar(quotes$game_id)
  if (!is.null(books)) keep <- keep & quotes$book %in% lc_book_key(books)
  x <- quotes[keep, , drop = FALSE]
  x <- x[order(captured[keep], decreasing = TRUE), , drop = FALSE]
  x[!duplicated(x[, c("game_id", "source", "book")]), , drop = FALSE]
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--help")) {
    cat("Usage: Rscript cfb_v3/capture_lines.R --label=opener|midweek|friday|close|adhoc",
        "[--season=YYYY] [--week=N] [--season-type=regular|postseason]",
        "[--sources=auto|cfbd|odds|cfbd,odds] [--store=DIR] [--odds-regions=us]\n")
    quit(status = 0)
  }
  options <- list(label = NULL, season = NULL, week = NULL, `season-type` = NULL, sources = "auto",
                  store = NULL, `odds-regions` = "us", `odds-markets` = "spreads,totals,h2h")
  for (arg in args) {
    key <- sub("=.*$", "", sub("^--", "", arg))
    if (!grepl("^--[^=]+=.+$", arg) || !key %in% names(options)) {
      stop("Unknown or invalid argument: ", arg, call. = FALSE)
    }
    options[[key]] <- sub("^[^=]+=", "", arg)
  }
  if (is.null(options$label)) stop("--label is required (for example --label=friday).", call. = FALSE)
  if (!is.null(options$`season-type`) && is.null(options$week)) stop("--season-type requires --week.", call. = FALSE)
  for (pkg in c("httr", "jsonlite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Package ", pkg, " is required.")
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  project_dir <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
  if (file.exists(file.path(project_dir, ".Renviron"))) readRenviron(file.path(project_dir, ".Renviron"))
  store <- if (is.null(options$store)) file.path(project_dir, "cfb_v3", "data", "line_captures") else options$store
  result <- capture_lines(
    store, options$label,
    season = if (is.null(options$season)) NULL else as.integer(options$season),
    week = if (is.null(options$week)) NULL else as.integer(options$week),
    season_type = options$`season-type`, sources = options$sources,
    aliases_path = file.path(project_dir, "cfb_v3", "inbox", "odds_api_team_aliases.csv"),
    odds_regions = options$`odds-regions`, odds_markets = options$`odds-markets`
  )
  log <- result$log
  for (i in seq_len(nrow(log))) {
    cat(sprintf("%s %s: %s%s%s%s\n", result$capture_id, log$source[i], log$status[i],
                if (log$status[i] == "ok") sprintf(" rows=%s games=%s", log$rows[i], log$games[i]) else "",
                if (is.na(log$requests_remaining[i])) "" else paste0(" odds_requests_remaining=", log$requests_remaining[i]),
                if (nzchar(log$message[i])) paste0(" (", log$message[i], ")") else ""))
  }
  cat("Store:", normalizePath(store, winslash = "/"), "\n")
  if (any(log$status == "error") || !nrow(result$quotes)) quit(status = 1)
}
