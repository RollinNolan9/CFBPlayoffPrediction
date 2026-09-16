og_public_table <- function(directory, inventory, season, sheet) {
  item <- inventory[[paste0("connelly_", season)]]
  if (season == 2023L) {
    tab <- item$tab_sources[[sheet]]
    path <- file.path(directory, paste0("connelly_2023_", sub(".*gid=", "", tab$url), ".html"))
    stopifnot(identical(digest::digest(file = path, algo = "sha256"), tab$sha256))
    tables <- rvest::html_table(xml2::read_html(path), header = FALSE, convert = FALSE, fill = TRUE)
    stopifnot(length(tables) == 1L)
    raw <- as.data.frame(tables[[1]])
    header <- which(apply(raw, 1, function(x) all(c("Team", "SP+", "Off. SP+", "Def. SP+") %in% x)))
    stopifnot(length(header) == 1L)
    out <- raw[seq.int(header + 1L, nrow(raw)), , drop = FALSE]
    names(out) <- unlist(raw[header, ], use.names = FALSE)
    return(out)
  }
  path <- file.path(directory, paste0("connelly_", season, ".xlsx"))
  stopifnot(identical(digest::digest(file = path, algo = "sha256"), item$sha256))
  as.data.frame(suppressMessages(readxl::read_excel(path, sheet = sheet,
    col_types = "text", .name_repair = "minimal")))
}

og_public_dates <- function(x, season) {
  number <- suppressWarnings(as.numeric(x))
  result <- as.Date(rep(NA_character_, length(x)))
  serial <- is.finite(number) & number > 40000 & number < 50000
  result[serial] <- as.Date(number[serial], origin = "1899-12-30")
  for (format in c("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d", "%m/%d", "%d-%b")) {
    missing <- is.na(result) & !is.na(x) & nzchar(x)
    parsed <- suppressWarnings(as.Date(x[missing], format = format))
    if (format %in% c("%m/%d", "%d-%b")) parsed <- as.Date(paste0(season, format(parsed, "-%m-%d")), format = "%Y-%m-%d")
    result[missing] <- parsed
  }
  result
}

og_public_records <- function(x) {
  out <- data.frame(wins = rep(NA_integer_, length(x)), losses = rep(NA_integer_, length(x)))
  literal <- !is.na(x) & grepl("^[0-9]{1,2}-[0-9]{1,2}$", x)
  out$wins[literal] <- as.integer(sub("-.*", "", x[literal]))
  out$losses[literal] <- as.integer(sub(".*-", "", x[literal]))
  # Google Sheets sometimes coerced 11-1 into November 1. Preserve the decoding flag.
  number <- suppressWarnings(as.numeric(x))
  serial <- is.finite(number) & number > 40000 & number < 50000
  dates <- as.Date(number[serial], origin = "1899-12-30")
  out$wins[serial] <- as.integer(format(dates, "%m"))
  out$losses[serial] <- as.integer(format(dates, "%d"))
  out$decoded_date <- serial
  out
}

og_verify_anchors <- function(sp, anchors) {
  key <- paste(sp$season, sp$snapshot_label, sp$team)
  stopifnot(!anyDuplicated(key))
  index <- match(paste(anchors$season, anchors$snapshot_label, anchors$team), key)
  anchors$observed <- vapply(seq_len(nrow(anchors)), function(i) {
    value <- sp[[anchors$field[i]]][index[i]]
    if (!is.na(anchors$previous_label[i]) && nzchar(anchors$previous_label[i])) {
      prior <- match(paste(anchors$season[i], anchors$previous_label[i], anchors$team[i]), key)
      value <- value - sp[[anchors$field[i]]][prior]
    }
    value
  }, numeric(1))
  is_delta <- !is.na(anchors$previous_label) & nzchar(anchors$previous_label)
  tolerance <- ifelse(is_delta, .100001, .000001)
  anchors$passed <- is.finite(anchors$observed) & abs(anchors$observed - anchors$expected) <= tolerance
  anchors
}

og_verify_public <- function(directory, games, anchors, output) {
  inventory <- jsonlite::fromJSON(file.path(directory, "inventory.json"), simplifyVector = FALSE)
  sp <- read.csv(file.path(directory, "inspection", "recovered_sp_candidates.csv"))
  sp <- sp[sp$season %in% 2022:2025 & grepl("week", sp$snapshot_label, ignore.case = TRUE), ]
  checks <- og_verify_anchors(sp, anchors)
  write.csv(checks, file.path(output, "dated_numeric_checks.csv"), row.names = FALSE)
  groups <- split(checks$passed, paste(checks$season, checks$snapshot_label))
  verified_keys <- names(groups)[vapply(groups, all, logical(1))]
  # A dated delta also corroborates the explicitly named prior-week numeric value only
  # when that previous value is independently anchored; do not promote it implicitly.
  records <- list(); summaries <- list(); rows <- list()
  for (key in unique(paste(sp$season, sp$snapshot_label))) {
    x <- sp[paste(sp$season, sp$snapshot_label) == key, ]
    season <- x$season[1]; sheet <- x$snapshot_label[1]
    raw <- og_public_table(directory, inventory, season, sheet)
    dates <- og_public_dates(raw[["Date"]], season)
    dates <- dates[!is.na(dates) & as.integer(format(dates, "%Y")) %in% c(season, season + 1L)]
    if (!length(dates)) {
      summaries[[key]] <- data.frame(season = season, snapshot_label = sheet, cycle = NA_character_,
        teams = nrow(x), matching_records = 0L, numeric_anchor = FALSE, usable_rows = 0L)
      next
    }
    cycle <- as.Date(min(dates)) - (as.integer(format(min(dates), "%u")) - 1L)
    cutoff <- as.POSIXct(cycle, tz = "UTC")
    raw <- raw[!is.na(raw[["Team"]]) & nzchar(trimws(raw[["Team"]])), , drop = FALSE]
    stopifnot(identical(trimws(raw[["Team"]]), x$source_team))
    record <- og_public_records(raw[["Record"]])
    for (i in seq_len(nrow(x))) {
      past <- games[games$season == season & games$kickoff < cutoff &
        (games$home == x$team[i] | games$away == x$team[i]), ]
      margin <- ifelse(past$home == x$team[i], past$margin, -past$margin)
      record$observed_wins[i] <- sum(margin > 0, na.rm = TRUE)
      record$observed_losses[i] <- sum(margin < 0, na.rm = TRUE)
    }
    record$passed <- !is.na(record$wins) & !is.na(record$losses) &
      record$wins == record$observed_wins & record$losses == record$observed_losses
    record$team <- x$team; record$season <- season; record$snapshot_label <- sheet
    record$cycle <- as.character(cycle)
    x$cycle <- as.character(cycle)
    x$record_passed <- record$passed
    x$numeric_anchor <- key %in% verified_keys
    x$reconstruction_status <- ifelse(!record$passed, "record_mismatch",
      ifelse(x$numeric_anchor, "corroborated_reconstruction", "record_checked_not_numerically_corroborated"))
    x$independently_timestamped <- FALSE
    rows[[key]] <- x; records[[key]] <- record
    summaries[[key]] <- data.frame(season = season, snapshot_label = sheet, cycle = as.character(cycle),
      teams = nrow(x), matching_records = sum(record$passed), numeric_anchor = x$numeric_anchor[1],
      usable_rows = sum(record$passed & x$numeric_anchor))
  }
  rows <- do.call(rbind, rows); summaries <- do.call(rbind, summaries)
  write.csv(rows, file.path(output, "reconstructed_sp_ledger.csv"), row.names = FALSE)
  write.csv(do.call(rbind, records), file.path(output, "record_checks.csv"), row.names = FALSE)
  write.csv(summaries, file.path(output, "snapshot_coverage.csv"), row.names = FALSE)
  list(rows = rows, summaries = summaries, checks = checks)
}

og_compact_aggregate <- function(compact, source, window = 6L) {
  scores <- compact$games[order(compact$games$game_id), c("game_id", "pos_team_final_score",
    "def_team_final_score", "pos_team", "def_pos_team", "home", "away", "week", "year")]
  stats <- compact$stats %>% group_by(year, pos_team, week) %>% summarise(
    epa_per_play = sum(epa_sum) / sum(epa_n),
    epa_per_pass = sum(pass_sum) / sum(pass_n),
    epa_per_rush = sum(rush_sum) / sum(rush_n),
    wpa_per_play = sum(wpa_sum) / sum(wpa_n), .groups = "drop")
  expression <- source$expressions[[which(vapply(source$expressions, model1_assignment, character(1)) == "aggregate_game_data")]]
  fn <- eval(expression[[3]])
  body <- as.list(body(fn))
  stopifnot(length(body) == 6L, model1_assignment(body[[2]]) == "final_scores",
    model1_assignment(body[[3]]) == "team_stats", model1_assignment(body[[4]]) == "team_stats",
    model1_assignment(body[[5]]) == "game_data")
  e <- new.env(parent = environment())
  e$final_scores <- tibble::as_tibble(scores); e$team_stats <- stats; e$window <- window
  eval(body[[4]], envir = e); eval(body[[5]], envir = e)
  e$game_data
}

og_freeze_compact <- function(compact, season, cutoff) {
  keep <- compact$games$year == season & !is.na(compact$games$kickoff) & compact$games$kickoff < cutoff
  out <- compact
  out$games <- compact$games[keep, ]
  out$stats <- compact$stats[compact$stats$game_id %in% out$games$game_id, ]
  out
}

og_compact_table <- function(compact, inputs, source) {
  e <- model1_environment(inputs, source)
  e$sp_ratings_all_years <- inputs$sp
  e$elo_ratings_combined <- inputs$elo
  e$aggregate_game_data <- function(pbp_data, window) og_compact_aggregate(pbp_data, source, window)
  e$cfb_prediction2024 <- compact
  targets <- c("season_level_data_predict", "predict_dat", "predict_dat_clean")
  for (expression in source$expressions)
    if (model1_assignment(expression) %in% targets) eval(expression, envir = e)
  data <- e$predict_dat_clean
  full <- e$season_level_data_predict
  keep <- complete.cases(full[names(data)])
  stopifnot(nrow(data) == sum(keep))
  data$source_game_id <- full$game_id[keep]
  list(data = data, full = full)
}

og_legacy_name <- function(team, inputs, source) {
  raw <- inputs$team_names$raw[match(team, inputs$team_names$canonical)]
  if (is.na(raw)) return(NA_character_)
  e <- model1_environment(inputs, source)
  row <- e$clean_team_names(data.frame(home = raw, away = raw))
  if (!nrow(row)) NA_character_ else row$home[1]
}

og_matchup_row <- function(home, away, data) {
  home_rows <- data[data$home == home, ]
  away_rows <- data[data$away == away, ]
  if (!nrow(home_rows) || !nrow(away_rows)) return(NULL)
  h <- home_rows[order(-home_rows$year, -home_rows$week), ][1, ]
  a <- away_rows[order(-away_rows$year, -away_rows$week), ][1, ]
  out <- data.frame(home = home, away = away)
  for (name in grep("^home_team_", names(data), value = TRUE)) out[[name]] <- h[[name]]
  for (name in grep("^away_team_", names(data), value = TRUE)) out[[name]] <- a[[name]]
  attr(out, "source_games") <- c(home = h$source_game_id, away = a$source_game_id)
  out
}

og_metric_table <- function(predictions) {
  slices <- list(all_fbs = rep(TRUE, nrow(predictions)), cfp = predictions$is_cfp,
    spread_over_21 = abs(predictions$closing_home_spread) > 21)
  sides <- p4_or_independent_sides(predictions)
  slices$article_audience <- sides$home | sides$away
  for (year in sort(unique(predictions$season))) slices[[paste0("season_", year)]] <- predictions$season == year
  result <- list()
  for (model in unique(predictions$model)) for (slice in names(slices)) {
    x <- predictions[which(predictions$model == model & slices[[slice]]), ]
    n <- nrow(x); decisions <- sum(x$win | x$loss)
    result[[paste(model, slice)]] <- data.frame(model = model, slice = slice, games = n,
      su_wins = sum(x$su_correct), su_accuracy = if (n) mean(x$su_correct) else NA_real_,
      wins = sum(x$win), losses = sum(x$loss), pushes = sum(x$push), no_selection = sum(!x$selected),
      ats_accuracy = if (decisions) sum(x$win)/decisions else NA_real_,
      margin_mae = if (n) mean(x$mae_error) else NA_real_,
      margin_rmse = if (n) sqrt(mean((x$expected_margin-x$actual_margin)^2)) else NA_real_,
      margin_bias = if (n) mean(x$expected_margin-x$actual_margin) else NA_real_)
  }
  do.call(rbind, result)
}
