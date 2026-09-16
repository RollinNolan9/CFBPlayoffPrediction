# Isolated prospective benchmark: no production fitting, imports, or promotion.
external_models <- function() c("v3", "sp", "sagarin_predictor", "fpi",
  "external_consensus", "v3_plus_consensus", "market_reference")

external_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  x <- as.character(x)
  valid <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z$", x)
  x[!valid] <- NA_character_
  parse_utc_datetime(x)
}

external_stamp <- function(x = Sys.time()) format(x, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

external_team <- function(x) {
  x <- canonical_team(x)
  aliases <- c("Miami-FL" = "Miami", "Miami-Florida" = "Miami",
    "Miami-OH" = "Miami (OH)", "Miami-Ohio" = "Miami (OH)",
    "UL-Lafayette" = "Louisiana", "Louisiana-Lafayette" = "Louisiana",
    "UL-Monroe" = "UL Monroe", "Louisiana-Monroe" = "UL Monroe",
    "USF" = "South Florida", "Appalachian State" = "App State", "Hawaii" = "Hawai'i",
    "Army West Point" = "Army", "Central Florida(UCF)" = "UCF",
    "Brigham Young" = "BYU", "Southern Methodist" = "SMU",
    "Middle Tennessee State" = "Middle Tennessee", "Fla. International" = "Florida International")
  hit <- match(x, names(aliases)); x[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  x
}

external_empty_ratings <- function() data.frame(model = character(), season = integer(),
  team = character(), source_team = character(), rating = numeric(), captured_at = character(),
  source_label = character(), source_url = character(), home_advantage = numeric())

external_parse_sp <- function(path, sheet) {
  if (!grepl("^FBS Week [0-9]+$", sheet)) stop("Explicit weekly FBS tab required; no final ratings.")
  x <- suppressMessages(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"))
  x <- og_extract_public_sp(x)
  data.frame(source_team = x$source_team, team = external_team(x$team), rating = x$rating)
}

external_parse_fpi <- function(path, season) {
  x <- jsonlite::fromJSON(path)
  assert_columns(x, c("year", "team", "fpi"), "FPI response")
  if (!nrow(x) || any(is.na(x$year) | x$year != season)) stop("Wrong or empty FPI season.")
  data.frame(source_team = x$team, team = external_team(x$team), rating = as.numeric(x$fpi))
}

external_parse_sagarin <- function(path, season) {
  text <- xml2::xml_text(xml2::read_html(path))
  label <- regmatches(text, regexpr("[0-9]{4} College Football through games of [^\\r\\n]+", text))
  if (length(label) != 1L || !startsWith(label, paste(season, "College Football")))
    stop("Sagarin season/as-of header missing or wrong; review source.")
  if (!grepl("PREDICTOR", text, fixed = TRUE)) stop("Sagarin Predictor header missing.")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  # The third pipe-delimited field is Predictor, not the leading composite rating.
  lines <- lines[grepl("^\\s*[0-9]+\\s+.+\\s+[AB]\\s*=", lines)]
  parts <- strsplit(lines, "|", fixed = TRUE)
  parts <- parts[lengths(parts) >= 4L]
  if (!length(parts)) stop("Sagarin table layout changed.")
  team <- vapply(parts, function(p) sub("^\\s*[0-9]+\\s+(.+?)\\s+[AB]\\s*=.*$", "\\1", p[1]), "")
  value <- vapply(parts, function(p) suppressWarnings(as.numeric(
    sub("^\\s*([-+]?[0-9]+\\.[0-9]+)\\s+.*$", "\\1", p[3]))), 0)
  x <- unique(data.frame(source_team = team, team = external_team(team), rating = value))
  attr(x, "source_label") <- label
  x
}

external_validate_ratings <- function(x) {
  assert_columns(x, names(external_empty_ratings()), "external ratings")
  assert_unique_keys(x, c("model", "season", "team"), "external ratings")
  if (any(!x$model %in% c("sp", "sagarin_predictor", "fpi")) ||
      any(!is.finite(x$rating)) || any(!is.finite(x$home_advantage)) ||
      any(is.na(x$team) | !nzchar(x$team)) || anyNA(external_time(x$captured_at)))
    stop("Invalid external ratings or capture timestamps.")
  invisible(x)
}

external_fetch <- function(url, path, query = NULL, authenticated = FALSE) {
  header <- NULL
  if (authenticated) {
    key <- Sys.getenv("CFBD_API_KEY")
    if (!nzchar(key)) stop("CFBD_API_KEY is not set.")
    header <- httr::add_headers(Authorization = paste("Bearer", key))
  }
  response <- httr::GET(url, header, query = query, httr::timeout(45))
  httr::stop_for_status(response)
  writeBin(httr::content(response, as = "raw"), path)
  external_stamp()
}

external_capture <- function(directory, sources, season, week) {
  if (season != sources$season) stop("Update external_sources.json for this season first.")
  if (!identical(as.numeric(sources$rating_to_margin_home_advantage), 2.4))
    stop("Protocol locks the rating-to-margin home adjustment at 2.4.")
  ratings <- external_empty_ratings(); ledger <- list()
  for (model in c("sp", "sagarin_predictor", "fpi")) {
    url <- switch(model, sp = sources$sp_url, sagarin_predictor = sources$sagarin_url,
                  fpi = sources$fpi_url)
    path <- file.path(directory, switch(model, sp = "sp.xlsx", sagarin_predictor = "sagarin.html", fpi = "fpi.json"))
    captured <- NA_character_; status <- "captured"; label <- NA_character_; count <- 0L
    tryCatch({
      captured <- external_fetch(url, path, if (model == "fpi") list(year = season) else NULL,
                                 authenticated = model == "fpi")
      label <- if (model == "sp") paste("FBS Week", week) else paste("current", season, model)
      x <- switch(model, sp = external_parse_sp(path, label),
        sagarin_predictor = external_parse_sagarin(path, season), fpi = external_parse_fpi(path, season))
      if (!is.null(attr(x, "source_label"))) label <- attr(x, "source_label")
      if (nrow(x) < 100L) stop("Fewer than 100 teams; source needs review.")
      x$model <- model; x$season <- season; x$captured_at <- captured
      x$source_label <- label; x$source_url <- url
      x$home_advantage <- sources$rating_to_margin_home_advantage
      external_validate_ratings(x)
      count <- nrow(x); ratings <- rbind(ratings, x[names(ratings)])
    }, error = function(e) { status <<- paste("unavailable:", conditionMessage(e)) })
    ledger[[model]] <- data.frame(model = model, source_url = url, captured_at = captured,
      source_label = label, rows = count, status = status,
      sha256 = if (file.exists(path)) digest::digest(file = path, algo = "sha256") else NA_character_)
  }
  list(ratings = ratings, sources = do.call(rbind, ledger))
}

external_build <- function(card, ratings, cutoff) {
  required <- c("game_id", "season", "week", "home", "away", "kickoff", "neutral_site",
    "expected_margin", "market_home_spread", "market_captured_at", "market_provider")
  assert_columns(card, required, "frozen v3 card")
  assert_unique_keys(card, "game_id", "frozen v3 card")
  external_validate_ratings(ratings)
  cutoff <- external_time(cutoff)
  if (length(cutoff) != 1L || is.na(cutoff)) stop("A single UTC cutoff is required.")
  if (any(external_time(ratings$captured_at) > cutoff)) stop("Ratings captured after cutoff.")
  card$home <- external_team(card$home); card$away <- external_team(card$away)
  card$neutral_site <- as.logical(card$neutral_site)
  kickoff <- external_time(card$kickoff)
  reason <- rep("included", nrow(card))
  reason[is.na(card$neutral_site)] <- "unknown_venue"
  reason[!is.finite(card$expected_margin)] <- "missing_v3_margin"
  reason[is.na(kickoff)] <- "unknown_kickoff"
  reason[!is.na(kickoff) & kickoff <= cutoff] <- "already_started_at_capture"
  if ("postseason_type" %in% names(card)) {
    cfp <- if ("is_cfp" %in% names(card)) !is.na(card$is_cfp) & as.logical(card$is_cfp) else rep(FALSE, nrow(card))
    reason[card$postseason_type %in% c("bowl", "non_cfp_bowl") & !cfp] <- "excluded_non_cfp_bowl"
  }
  common_fields <- c("game_id", "season", "week", "home", "away", "kickoff", "neutral_site")
  optional <- c("home_conference", "away_conference", "is_cfp", "postseason_type")
  base <- card[c(common_fields, intersect(optional, names(card)))]
  for (name in setdiff(optional, names(base))) base[[name]] <- if (name == "is_cfp") FALSE else NA_character_
  line_time <- external_time(card$market_captured_at)
  valid_line <- is.finite(card$market_home_spread) & !is.na(line_time) & line_time <= cutoff & line_time < kickoff
  valid_line <- valid_line & !is.na(card$market_provider) & nzchar(trimws(card$market_provider))
  valid_line[is.na(valid_line)] <- FALSE
  base$line_home_spread <- ifelse(valid_line, card$market_home_spread, NA_real_)
  base$market_provider <- card$market_provider; base$line_captured_at <- card$market_captured_at
  base$line_status <- ifelse(valid_line, "frozen_quote_not_closing", "missing_or_invalid_quote_time")
  # Only explicit spread prices qualify. Moneyline prices are never substitutes.
  for (side in c("home", "away")) {
    name <- paste0(side, "_spread_odds")
    base[[name]] <- if (name %in% names(card)) as.numeric(card[[name]]) else NA_real_
  }
  base$cutoff <- external_stamp(cutoff)
  forecasts <- list(v3 = card$expected_margin)
  coverage <- base[common_fields]; coverage$game_status <- reason; coverage$line_status <- base$line_status
  for (model in c("sp", "sagarin_predictor", "fpi")) {
    x <- ratings[ratings$model == model, ]; keys <- paste(x$season, x$team)
    h <- match(paste(card$season, card$home), keys); a <- match(paste(card$season, card$away), keys)
    forecasts[[model]] <- x$rating[h] - x$rating[a] + ifelse(card$neutral_site, 0, x$home_advantage[h])
    coverage[[model]] <- ifelse(is.na(h) & is.na(a), "missing_both", ifelse(is.na(h), "missing_home",
      ifelse(is.na(a), "missing_away", "included")))
  }
  # Complete-case fixed blend, never an opportunistic mean of available sources.
  forecasts$external_consensus <- (forecasts$sp + forecasts$sagarin_predictor + forecasts$fpi) / 3
  forecasts$v3_plus_consensus <- (forecasts$v3 + forecasts$external_consensus) / 2
  forecasts$market_reference <- -base$line_home_spread
  coverage$common_ratings <- reason == "included" & is.finite(forecasts$external_consensus)
  predictions <- do.call(rbind, lapply(names(forecasts), function(model) {
    x <- base; x$model <- model; x$expected_margin <- forecasts[[model]]
    x$eligible <- reason == "included" & is.finite(x$expected_margin)
    x$common_ratings <- coverage$common_ratings
    x$fair_home_spread <- -x$expected_margin
    x$edge_home <- x$expected_margin + x$line_home_spread
    x$su_pick <- ifelse(!x$eligible | x$expected_margin == 0, "NO_PICK", ifelse(x$expected_margin > 0, x$home, x$away))
    x$ats_pick <- ifelse(!x$eligible | model == "market_reference" | is.na(x$edge_home) | x$edge_home == 0,
      "NO_PICK", ifelse(x$edge_home > 0, x$home, x$away))
    x
  }))
  wide <- base
  for (model in names(forecasts)) wide[[paste0(model, "_home_margin")]] <- forecasts[[model]]
  wide$game_status <- reason; wide$common_ratings <- coverage$common_ratings
  list(predictions = predictions, coverage = coverage, comparison = wide)
}

external_new_dir <- function(root, prefix) {
  path <- file.path(root, paste0(prefix, format(Sys.time(), "%Y%m%dT%H%M%OS3Z", tz = "UTC")))
  if (dir.exists(path)) stop("Refusing to overwrite benchmark output.")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Cannot create benchmark output.")
  path
}

external_seal <- function(directory) {
  paths <- list.files(directory, full.names = TRUE, recursive = TRUE)
  paths <- paths[!dir.exists(paths) & basename(paths) != "checksums.csv"]
  write.csv(data.frame(path = substring(paths, nchar(directory) + 2L),
    sha256 = vapply(paths, function(p) digest::digest(file = p, algo = "sha256"), "")),
    file.path(directory, "checksums.csv"), row.names = FALSE)
}

external_verify <- function(directory) {
  hashes <- read.csv(file.path(directory, "checksums.csv"), stringsAsFactors = FALSE)
  if (any(grepl("(^[/\\\\]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", hashes$path))) stop("Unsafe checksum path.")
  for (i in seq_len(nrow(hashes))) {
    path <- file.path(directory, hashes$path[i])
    if (!file.exists(path) || !identical(digest::digest(file = path, algo = "sha256"), hashes$sha256[i]))
      stop("Snapshot changed: ", hashes$path[i])
  }
  invisible(TRUE)
}

external_card_metadata <- function(card, raw) {
  required <- c("id", "season", "homeTeam", "awayTeam", "homeConference", "awayConference")
  assert_columns(raw, required, "card line metadata")
  raw <- unique(raw[required])
  assert_unique_keys(raw, "id", "card line metadata")
  r <- raw[match(card$game_id, as.character(raw$id)), ]
  known <- !is.na(r$id)
  if (any(known & (r$season != card$season | external_team(r$homeTeam) != external_team(card$home) |
                  external_team(r$awayTeam) != external_team(card$away)))) stop("Card metadata mismatch.")
  if (!"home_conference" %in% names(card)) card$home_conference <- r$homeConference
  if (!"away_conference" %in% names(card)) card$away_conference <- r$awayConference
  card
}

external_snapshot <- function(project, card_path, sources_path) {
  directory <- external_new_dir(file.path(project, "cfb_v3/output/experiments/external_ratings"), "snapshot_")
  if (!file.copy(card_path, file.path(directory, "v3_card.csv"))) stop("Cannot freeze v3 card.")
  file.copy(sources_path, file.path(directory, "sources_config.json"))
  file.copy(file.path(project, "cfb_v3/experiments/EXTERNAL_RATINGS.md"), file.path(directory, "protocol.md"))
  file.copy(file.path(project, "cfb_v3/experiments/external_ratings.R"), file.path(directory, "implementation.R"))
  card <- read.csv(file.path(directory, "v3_card.csv"), stringsAsFactors = FALSE)
  metadata_path <- file.path(dirname(card_path), "cfbd_lines_raw.rds")
  if (file.exists(metadata_path)) {
    file.copy(metadata_path, file.path(directory, "card_line_metadata.rds"))
    card <- external_card_metadata(card, readRDS(file.path(directory, "card_line_metadata.rds")))
  }
  if (length(unique(card$season)) != 1L || length(unique(card$week)) != 1L) stop("One season/week per snapshot.")
  sources <- jsonlite::fromJSON(sources_path)
  captured <- external_capture(directory, sources, unique(card$season), unique(card$week))
  cutoff <- Sys.time()
  built <- external_build(card, captured$ratings, cutoff)
  write.csv(captured$ratings, file.path(directory, "ratings.csv"), row.names = FALSE)
  write.csv(captured$sources, file.path(directory, "source_ledger.csv"), row.names = FALSE)
  for (name in names(built)) write.csv(built[[name]], file.path(directory, paste0(name, ".csv")), row.names = FALSE)
  jsonlite::write_json(list(status = "complete", protocol = "external-ratings-v1", cutoff = external_stamp(cutoff),
    card_path = normalizePath(card_path, winslash = "/"), card_sha256 = digest::digest(file = file.path(directory, "v3_card.csv"), algo = "sha256"),
    model_refit = FALSE, automatic_promotion = FALSE, evidence_lane = "prospective_capture",
    rating_to_margin_home_advantage = sources$rating_to_margin_home_advantage,
    quote_note = "Original card quote, not a refreshed or closing line. Capture is not original publication time."),
    file.path(directory, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  text <- c("# External Ratings Benchmark", "", paste("Cutoff:", external_stamp(cutoff)),
    "Production v3 and its published card are unchanged. Positive margins favor home.", "",
    "SP+, Sagarin Predictor and FPI are rating-derived margins with a fixed 2.4-point home adjustment,",
    "zero on neutral fields. These are NOT the publishers' official matchup forecasts.",
    "Current captures cannot establish historical performance. No results or profitability claim yet.", "",
    paste("Pregame games:", sum(built$coverage$game_status == "included"), "/", nrow(card)),
    paste("Complete four-way games:", sum(built$coverage$common_ratings)), "",
    "## Sources", capture.output(print(captured$sources[c("model", "rows", "status")], row.names = FALSE)), "",
    "See comparison.csv for side-by-side margins, predictions.csv for picks, coverage.csv for exclusions,",
    "and source_ledger.csv plus retained raw files for provenance. Original quoted lines are shared by every model.",
    "Blank consensus means at least one external source is missing; no weights are reallocated.")
  writeLines(text, file.path(directory, "report.md"))
  external_seal(directory)
  directory
}

external_grade <- function(predictions, results) {
  assert_columns(results, c("game_id", "season", "home", "away", "kickoff", "home_score", "away_score", "completed"), "results")
  assert_unique_keys(results, "game_id", "results")
  assert_unique_keys(predictions, c("game_id", "model"), "frozen forecasts")
  index <- match(predictions$game_id, results$game_id); r <- results[index, ]
  present <- !is.na(index)
  for (name in c("home", "away", "season")) {
    lhs <- if (name == "season") predictions[[name]] else external_team(predictions[[name]])
    rhs <- if (name == "season") r[[name]] else external_team(r[[name]])
    if (any(present & (is.na(rhs) | lhs != rhs))) stop("Result metadata mismatch: ", name)
  }
  result_time <- external_time(r$kickoff)
  if (any(present & (is.na(result_time) | result_time <= external_time(predictions$cutoff))))
    stop("Result kickoff missing or not after the forecast cutoff.")
  predictions$actual_margin <- r$home_score - r$away_score
  predictions$scored <- present & !is.na(r$completed) & as.logical(r$completed) &
    is.finite(predictions$actual_margin) & predictions$eligible
  predictions$actual_margin[!predictions$scored] <- NA_real_
  predictions$absolute_error <- abs(predictions$actual_margin - predictions$expected_margin)
  predictions$su_correct <- ifelse(predictions$scored & predictions$expected_margin != 0 &
    predictions$actual_margin != 0, sign(predictions$expected_margin) == sign(predictions$actual_margin), NA)
  edge <- predictions$edge_home; cover <- predictions$actual_margin + predictions$line_home_spread
  predictions$selected <- predictions$scored & is.finite(edge) & edge != 0 & predictions$model != "market_reference"
  predictions$push <- predictions$selected & cover == 0
  predictions$win <- predictions$selected & cover != 0 & sign(cover) == sign(edge)
  predictions$loss <- predictions$selected & cover != 0 & sign(cover) != sign(edge)
  odds <- ifelse(edge > 0, predictions$home_spread_odds, predictions$away_spread_odds)
  priced <- predictions$selected & is.finite(odds) & abs(odds) >= 100
  predictions$profit <- ifelse(priced, ifelse(predictions$push, 0,
    ifelse(predictions$win, ifelse(odds > 0, odds/100, 100/abs(odds)), -1)), NA_real_)
  predictions
}

external_metrics <- function(x) {
  slices <- list(all = rep(TRUE, nrow(x)), week_0_1 = x$week <= 1, week_2_4 = x$week >= 2 & x$week <= 4,
    week_5_plus = x$week >= 5, cfp = !is.na(x$is_cfp) & x$is_cfp,
    neutral_site = x$neutral_site, spread_over_21 = abs(x$line_home_spread) > 21,
    sec_nonconference = xor(x$home_conference %in% c("SEC", "Southeastern"), x$away_conference %in% c("SEC", "Southeastern")) &
      !is.na(x$home_conference) & !is.na(x$away_conference))
  for (season in unique(x$season)) slices[[paste0("season_", season)]] <- x$season == season
  rows <- list()
  for (cohort in c("common_four_way", "common_four_way_lined", "available_per_model")) for (model in external_models()) for (slice in names(slices)) {
    common <- if (cohort == "available_per_model") rep(TRUE, nrow(x)) else x$common_ratings
    if (cohort == "common_four_way_lined") common <- common & is.finite(x$line_home_spread)
    keep <- x$scored & x$model == model & slices[[slice]] & common
    keep[is.na(keep)] <- FALSE; y <- x[keep, ]
    w <- sum(y$win); l <- sum(y$loss); n <- sum(y$selected); priced <- sum(is.finite(y$profit))
    ci <- if (w+l) stats::binom.test(w, w+l)$conf.int else c(NA_real_, NA_real_)
    mean_or_na <- function(z) if (any(is.finite(z))) mean(z, na.rm = TRUE) else NA_real_
    sec_sign <- ifelse(y$home_conference %in% c("SEC", "Southeastern"), 1, -1)
    rows[[length(rows)+1L]] <- data.frame(cohort = cohort, model = model, slice = slice, games = nrow(y),
      su_decisions = sum(!is.na(y$su_correct)), su_accuracy = mean_or_na(as.numeric(y$su_correct)),
      margin_mae = mean_or_na(y$absolute_error), margin_rmse = sqrt(mean_or_na(y$absolute_error^2)),
      home_margin_bias = mean_or_na(y$expected_margin-y$actual_margin),
      sec_optimism = if (slice == "sec_nonconference") mean_or_na(sec_sign*(y$expected_margin-y$actual_margin)) else NA_real_,
      wins = w, losses = l, pushes = sum(y$push), ats_accuracy = if (w+l) w/(w+l) else NA_real_,
      ats_ci_low_descriptive = ci[1], ats_ci_high_descriptive = ci[2], priced_bets = priced,
      profit_units = if (priced) sum(y$profit, na.rm = TRUE) else NA_real_,
      roi_priced_subset = if (priced) sum(y$profit, na.rm = TRUE)/priced else NA_real_,
      roi_all_picks = if (n > 0L && priced == n) sum(y$profit)/n else NA_real_)
  }
  do.call(rbind, rows)
}

external_paired <- function(x) {
  # Pairwise rows supplement, not replace, the common-four-way primary cohort.
  pairs <- lapply(setdiff(external_models(), "v3"), function(m) c(m, "v3"))
  pairs[[length(pairs)+1L]] <- c("v3_plus_consensus", "external_consensus")
  do.call(rbind, lapply(pairs, function(pair) {
    a <- x[x$model == pair[1] & x$scored, ]; b <- x[x$model == pair[2] & x$scored, ]
    y <- merge(a, b, by = "game_id", suffixes = c("_candidate", "_reference"))
    delta <- y$absolute_error_candidate - y$absolute_error_reference
    ats <- y$selected_candidate & y$selected_reference & !y$push_candidate & !y$push_reference
    # Season-block intervals require multiple seasons; weekly samples are not independent seasons.
    interval <- experiment_season_interval(delta, y$season_candidate)
    data.frame(candidate = pair[1], reference = pair[2], common_games = nrow(y),
      mae_change = if (nrow(y)) mean(delta) else NA_real_,
      mae_change_low = interval[1], mae_change_high = interval[2],
      ats_common_decisions = sum(ats),
      ats_accuracy_change = if (any(ats)) mean(as.numeric(y$win_candidate[ats])-as.numeric(y$win_reference[ats])) else NA_real_,
      seasons = length(unique(y$season_candidate)))
  }))
}

external_score <- function(project, snapshots, results_path = NULL) {
  snapshots <- normalizePath(snapshots, winslash = "/", mustWork = TRUE)
  predictions <- do.call(rbind, lapply(snapshots, function(path) {
    external_verify(path)
    manifest <- jsonlite::fromJSON(file.path(path, "manifest.json"))
    if (!identical(manifest$status, "complete") || !identical(manifest$protocol, "external-ratings-v1"))
      stop("Incomplete snapshot or different benchmark protocol.")
    read.csv(file.path(path, "predictions.csv"), stringsAsFactors = FALSE)
  }))
  assert_unique_keys(predictions, c("game_id", "model"), "combined snapshots; choose one capture per game")
  directory <- external_new_dir(file.path(project, "cfb_v3/output/experiments/external_ratings"), "score_")
  if (is.null(results_path)) {
    rows <- list()
    for (season in unique(predictions$season)) for (type in c("regular", "postseason")) {
      path <- file.path(directory, paste0("results_", season, "_", type, ".json"))
      external_fetch("https://api.collegefootballdata.com/games", path,
        list(year = season, seasonType = type), authenticated = TRUE)
      r <- jsonlite::fromJSON(path)
      if (!length(r)) next
      assert_columns(r, c("id", "season", "homeTeam", "awayTeam", "startDate", "homePoints", "awayPoints", "completed"), "CFBD results")
      rows[[length(rows)+1L]] <- data.frame(game_id = as.character(r$id), season = r$season,
        home = external_team(r$homeTeam), away = external_team(r$awayTeam), kickoff = r$startDate,
        home_score = as.numeric(r$homePoints), away_score = as.numeric(r$awayPoints), completed = r$completed)
    }
    if (!length(rows)) stop("No result schedule returned.")
    results <- do.call(rbind, rows)
  } else {
    file.copy(results_path, file.path(directory, "supplied_results.csv"))
    results <- read.csv(results_path, stringsAsFactors = FALSE)
  }
  write.csv(results, file.path(directory, "results.csv"), row.names = FALSE)
  implementation <- file.path(project, "cfb_v3/experiments/external_ratings.R")
  if (file.exists(implementation)) file.copy(implementation, file.path(directory, "scoring_implementation.R"))
  # Started games remain in coverage, but must not enter scoring or result timing checks.
  predictions <- predictions[predictions$eligible, ]
  graded <- external_grade(predictions, results)
  metrics <- external_metrics(graded); paired <- external_paired(graded)
  write.csv(graded, file.path(directory, "graded_predictions.csv"), row.names = FALSE)
  write.csv(metrics, file.path(directory, "metrics.csv"), row.names = FALSE)
  write.csv(paired, file.path(directory, "paired_comparisons.csv"), row.names = FALSE)
  jsonlite::write_json(list(status = "complete", scored_at = external_stamp(), snapshots = snapshots,
    automatic_promotion = FALSE), file.path(directory, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  summary <- metrics[metrics$slice == "all", c("cohort", "model", "games", "margin_mae", "su_accuracy", "wins", "losses", "pushes", "roi_all_picks")]
  writeLines(c("# External Benchmark Results", "", "No automatic promotion. Negative paired MAE change favors candidate.",
    "Primary: common-four-way cohort; use common-four-way-lined for the market reference comparison.",
    "Available-per-model totals have different games and are not a leaderboard.",
    "ATS uses the frozen article quote, not closing spreads. ROI requires actual spread prices; blanks mean unknown.",
    "ATS binomial intervals are descriptive, not selection-adjusted or evidence of independent bets.",
    "Probability calibration is not scored: these ratings do not supply comparable game probabilities.", "",
    capture.output(print(summary, row.names = FALSE)), "", capture.output(print(paired, row.names = FALSE)), "",
    "Missing conference metadata means no conference diagnosis. More prospective games are needed before drawing conclusions."),
    file.path(directory, "report.md"))
  external_seal(directory)
  directory
}
