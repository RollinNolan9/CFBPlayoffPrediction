# CFBD player-game source helpers. Historical season aggregates are never predictors.
injury_player_fetch <- function(directory, label, endpoint, query = list()) {
  stopifnot(grepl("^[A-Za-z0-9_-]+$", label), startsWith(endpoint, "/"))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, paste0(label, ".json"))
  metadata_path <- file.path(directory, paste0(label, "_metadata.json"))
  if (file.exists(path) && file.exists(metadata_path)) {
    metadata <- jsonlite::fromJSON(metadata_path)
    if (!identical(metadata$endpoint, endpoint) ||
        !isTRUE(all.equal(metadata$query, query, check.attributes = FALSE))) {
      stop("Cached player source request does not match: ", label)
    }
    if (!identical(metadata$sha256, digest::digest(file = path, algo = "sha256"))) {
      stop("Cached player source hash mismatch: ", label)
    }
    return(jsonlite::fromJSON(path, flatten = TRUE))
  }
  key <- Sys.getenv("CFBD_API_KEY")
  if (!nzchar(key)) stop("CFBD_API_KEY is not set.")
  response <- httr::GET(paste0("https://api.collegefootballdata.com", endpoint),
    httr::add_headers(Authorization = paste("Bearer", key)), query = query,
    httr::timeout(60))
  status <- httr::status_code(response)
  metadata <- list(endpoint = endpoint, query = query, status = status,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (status != 200L) {
    jsonlite::write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
    stop("CFBD request failed with HTTP ", status, " for ", endpoint, call. = FALSE)
  }
  writeBin(httr::content(response, as = "raw"), path)
  metadata$sha256 <- digest::digest(file = path, algo = "sha256")
  jsonlite::write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
  jsonlite::fromJSON(path, flatten = TRUE)
}

injury_player_probe <- function(project_dir = getwd(), year = 2024L, team = "Ole Miss") {
  directory <- file.path(project_dir, "cfb_v3/output/experiments/injury_pilot/player_data")
  tag <- paste0(year, "_", gsub("[^A-Za-z0-9]+", "_", tolower(team)))
  requests <- list(
    rushing = list(endpoint = "/rushing/players/games", query = list(year = year,
      team = team, seasonType = "regular")),
    ppa = list(endpoint = "/ppa/players/games", query = list(year = year,
      team = team, position = "RB", seasonType = "regular", excludeGarbageTime = "false")),
    box = list(endpoint = "/games/players", query = list(year = year,
      team = team, seasonType = "regular"))
  )
  out <- lapply(names(requests), function(name) {
    request <- requests[[name]]
    value <- injury_player_fetch(directory, paste0(tag, "_", name), request$endpoint,
      request$query)
    cat(name, ": ", NROW(value), " top-level records; ",
      paste(names(value), collapse = ", "), "\n", sep = "")
    value
  })
  names(out) <- names(requests)
  invisible(out)
}

injury_player_box_counts <- function(box) {
  rows <- list()
  for (i in seq_len(nrow(box))) {
    teams <- box$teams[[i]]
    for (j in seq_len(nrow(teams))) {
      categories <- teams$categories[[j]]
      for (category in intersect(c("rushing", "receiving"), categories$name)) {
        types <- categories$types[[match(category, categories$name)]]
        stat_name <- if (category == "rushing") "CAR" else "REC"
        if (!stat_name %in% types$name) next
        athletes <- types$athletes[[match(stat_name, types$name)]]
        if (!nrow(athletes)) next
        rows[[length(rows) + 1L]] <- data.frame(game_id = as.character(box$id[i]),
          team = teams$team[j], athlete_id = as.character(athletes$id),
          player_name = athletes$name,
          rush_attempts = if (category == "rushing") as.numeric(athletes$stat) else 0,
          receptions = if (category == "receiving") as.numeric(athletes$stat) else 0,
          stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(rows)) return(data.frame())
  out <- aggregate(cbind(rush_attempts, receptions) ~ game_id + team + athlete_id +
    player_name, data = do.call(rbind, rows), FUN = sum)
  out$box_involvements <- out$rush_attempts + out$receptions
  out
}

# Provisional Rush + Reception reconstruction; strict matching rejects games
# where the provider aggregate differs. The larger prior-game audit also found
# Target associations and fourth-down/fumble discrepancies. This function must
# not be represented as a general exact CFBD population until reconciled.
# Never treat missing PPA as zero or receptions as targets.
injury_exact_rb_game <- function(plays, stats, player_ppa, season, week, team, game_id,
                                 mode = c("strict_api_replication", "raw_involvement_ppa")) {
  mode <- match.arg(mode)
  if (NROW(stats) >= 2000L) stop("Play-stat response may be truncated at the 2,000-row limit.")
  if (!is.data.frame(plays) || !nrow(plays) || !is.data.frame(stats) || !nrow(stats)) {
    stop("Exact reconstruction requires nonempty plays and play-stat associations.")
  }
  p <- plays[as.character(plays$gameId) == as.character(game_id) & plays$offense == team, ]
  s <- stats[as.character(stats$gameId) == as.character(game_id) & stats$team == team, ]
  rb <- player_ppa[player_ppa$season == season & player_ppa$week == week &
    player_ppa$team == team & player_ppa$position == "RB", ]
  if ("seasonType" %in% names(rb)) rb <- rb[rb$seasonType == "regular", ]
  if (!nrow(rb)) stop("No matching RB player-PPA rows for reconstruction validation.")
  if (anyDuplicated(p$id) || anyDuplicated(rb$id)) stop("Ambiguous play or RB identity keys.")
  s <- s[s$athleteId %in% rb$id, ]
  roles <- if (mode == "raw_involvement_ppa") c("Rush", "Reception", "Target", "Completion") else c("Rush", "Reception")
  # Audit all RB roles on finite-PPA offensive plays before selecting usage.
  # Incidental roles on an already represented athlete/play do not add usage.
  role_index <- match(s$playId, p$id)
  audit <- s[!is.na(role_index) & is.finite(p$ppa[role_index]),
    c("gameId", "playId", "athleteId", "athleteName", "statType")]
  pair <- function(x) paste(x$playId, x$athleteId, sep = "|")
  represented <- pair(audit[audit$statType %in% roles, ])
  audit$role_accounted <- pair(audit) %in% represented
  if (mode == "raw_involvement_ppa" && any(!audit$role_accounted)) {
    unknown <- audit[!audit$role_accounted, ]
    stop("Unaccounted finite-PPA offensive RB association(s): ",
      paste(unique(paste(unknown$athleteId, unknown$playId, unknown$statType, sep = ":")), collapse = ", "))
  }
  s <- s[s$statType %in% roles, ]
  s <- s[!duplicated(paste(s$gameId, s$playId, s$athleteId, sep = "|")), ]
  index <- match(s$playId, p$id)
  if (anyNA(index)) stop("RB player association has no matching offensive play.")
  s$ppa <- p$ppa[index]
  out <- do.call(rbind, lapply(seq_len(nrow(rb)), function(i) {
    g <- s[s$athleteId == rb$id[i], ]
    good <- is.finite(g$ppa)
    mean_ppa <- if (any(good)) mean(g$ppa[good]) else NA_real_
    data.frame(season = season, week = week, game_id = as.character(game_id),
      team = team, athlete_id = as.character(rb$id[i]), player_name = rb$name[i],
      rush_associations = sum(g$statType == "Rush"),
      reception_associations = sum(g$statType == "Reception"),
      target_associations = sum(g$statType == "Target"),
      completion_associations = sum(g$statType == "Completion"),
      raw_involvements = nrow(g), ppa_involvements = sum(good),
      missing_ppa_involvements = sum(!good),
      total_ppa = if (any(good)) sum(g$ppa[good]) else NA_real_, average_ppa = mean_ppa,
      api_average_ppa = rb$averagePPA.all[i],
      api_mean_absolute_difference = abs(mean_ppa - rb$averagePPA.all[i]),
      stringsAsFactors = FALSE)
  }))
  out$api_aggregate_reconciled <- is.finite(out$average_ppa) & is.finite(out$api_average_ppa) &
    out$api_mean_absolute_difference <= 0.00051
  if (mode == "strict_api_replication" && any(!out$api_aggregate_reconciled)) {
    stop("Reconstructed RB PPA does not match the same-game API average to rounding tolerance.")
  }
  if (any(!is.finite(out$average_ppa))) stop("RB player has no finite PPA; history is unavailable, not zero.")
  out$measure <- mode
  attr(out, "involvements") <- s[c("gameId", "playId", "athleteId", "athleteName", "statType", "ppa")]
  attr(out, "role_audit") <- audit
  out
}

injury_player_validate_probe <- function(project_dir = getwd()) {
  directory <- file.path(project_dir, "cfb_v3/output/experiments/injury_pilot/player_data")
  read <- function(name) jsonlite::fromJSON(file.path(directory, paste0(name, ".json")), flatten = TRUE)
  cases <- data.frame(season = c(2024L, 2024L, 2025L), week = c(1L, 4L, 2L),
    game_id = c("401628325", "401628366", "401752686"))
  audit <- lapply(seq_len(nrow(cases)), function(i) {
    case <- cases[i, ]
    tag <- paste0(case$season, "_ole_miss")
    exact <- injury_exact_rb_game(read(paste0(tag, "_w", case$week, "_plays")),
      read(paste0(tag, "_w", case$week, "_stats")), read(paste0(tag, "_ppa")),
      case$season, case$week, "Ole Miss", case$game_id)
    write.csv(attr(exact, "involvements"), file.path(directory,
      paste0(tag, "_w", case$week, "_rb_involvements.csv")), row.names = FALSE)
    boxes <- injury_player_box_counts(read(paste0(tag, "_box")))
    boxes <- boxes[boxes$team == "Ole Miss" & boxes$game_id == case$game_id, ]
    index <- match(exact$athlete_id, boxes$athlete_id)
    exact$box_rush_attempts <- boxes$rush_attempts[index]
    exact$box_receptions <- boxes$receptions[index]
    exact$box_involvements <- boxes$box_involvements[index]
    exact$box_minus_ppa_involvements <- exact$box_involvements - exact$ppa_involvements
    exact$box_weighted_total_ppa <- exact$box_involvements * exact$api_average_ppa
    exact$box_total_ppa_error <- exact$box_weighted_total_ppa - exact$total_ppa
    exact
  })
  audit <- do.call(rbind, audit)
  write.csv(audit, file.path(directory, "schema_validation.csv"), row.names = FALSE)
  print(audit[c("season", "week", "player_name", "ppa_involvements", "box_involvements",
    "missing_ppa_involvements", "average_ppa", "api_average_ppa", "box_total_ppa_error")], row.names = FALSE)
  invisible(audit)
}

injury_player_utc <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  as.POSIXct(sub("Z$", "", gsub("T", " ", as.character(x), fixed = TRUE)),
    format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

# Planning is the default. A caller must opt into fetching with an explicit
# request budget. Cutoff should be Friday noon America/Los_Angeles, converted to UTC.
injury_prior_player_history <- function(project_dir = getwd(), season, team,
                                       target_cutoff, max_new_requests = 0L,
                                       execute = FALSE, schedule_path = NULL,
                                       mode = c("strict_api_replication", "raw_involvement_ppa")) {
  mode <- match.arg(mode)
  if (!inherits(target_cutoff, "POSIXt") && !all(grepl("Z$", target_cutoff))) {
    stop("Text prediction cutoffs must explicitly end in Z (UTC).")
  }
  cutoff <- injury_player_utc(target_cutoff)
  if (length(cutoff) != 1L || is.na(cutoff)) stop("Supply one explicit UTC prediction cutoff.")
  if (length(max_new_requests) != 1L || !is.finite(max_new_requests) || max_new_requests < 0) {
    stop("max_new_requests must be a nonnegative number.")
  }
  api_team <- if (team %in% c("Mississippi", "Ole Miss")) "Ole Miss" else team
  schedule_team <- if (api_team == "Ole Miss") "Mississippi" else api_team
  if (is.null(schedule_path)) schedule_path <- file.path(project_dir, "cfb_v3/data/historical_games.csv")
  columns <- names(read.csv(schedule_path, nrows = 0L))
  classes <- setNames(rep("NULL", length(columns)), columns)
  classes[c("game_id", "kickoff", "home", "away", "source_season_type")] <- "character"
  classes[c("season", "week")] <- "integer"
  classes["completed"] <- "logical"
  schedule <- read.csv(schedule_path, stringsAsFactors = FALSE, colClasses = classes)
  kickoff <- injury_player_utc(schedule$kickoff)
  # Historical schedule has eventual completion flags, not final timestamps.
  # Require a conservative 12-hour gap to avoid treating a just-started game as final.
  take <- schedule$season == season & schedule$source_season_type == "regular" &
    (schedule$home == schedule_team | schedule$away == schedule_team) &
    schedule$completed %in% TRUE & !is.na(kickoff) & kickoff + 12 * 3600 < cutoff
  prior <- schedule[take, c("game_id", "kickoff", "season", "week", "home", "away", "completed")]
  prior <- prior[order(injury_player_utc(prior$kickoff)), ]
  prior$game_id <- as.character(prior$game_id)
  prior$team <- api_team
  if (anyDuplicated(prior$game_id) || anyDuplicated(prior$week)) {
    stop("Prior schedule must have unique game IDs and one regular game per team/week.")
  }
  directory <- file.path(project_dir, "cfb_v3/output/experiments/injury_pilot/player_data")
  tag <- paste0(season, "_", gsub("[^A-Za-z0-9]+", "_", tolower(api_team)))
  output <- file.path(directory, paste0("history_", tag, "_asof_",
    format(cutoff, "%Y%m%dT%H%M%SZ", tz = "UTC"),
    if (mode == "raw_involvement_ppa") "_raw_involvement_ppa" else ""))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  requests <- list()
  if (nrow(prior)) requests[[1L]] <- list(label = paste0(tag, "_ppa"),
    endpoint = "/ppa/players/games", query = list(year = season, team = api_team,
      position = "RB", seasonType = "regular", excludeGarbageTime = "false"))
  for (i in seq_len(nrow(prior))) {
    prefix <- paste0(tag, "_w", prior$week[i])
    requests[[length(requests) + 1L]] <- list(label = paste0(prefix, "_plays"),
      endpoint = "/plays", query = list(year = season, week = prior$week[i],
        offense = api_team, seasonType = "regular"))
    requests[[length(requests) + 1L]] <- list(label = paste0(prefix, "_stats"),
      endpoint = "/plays/stats", query = list(gameId = as.numeric(prior$game_id[i]), team = api_team))
  }
  cached <- vapply(requests, function(request) {
    yes <- all(file.exists(file.path(directory, paste0(request$label, c(".json", "_metadata.json")))))
    if (yes) injury_player_fetch(directory, request$label, request$endpoint, request$query)
    yes
  }, logical(1))
  needed <- sum(!cached)
  manifest <- list(season = season, team = api_team,
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    prediction_cutoff_utc = format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    completion_rule = "Completed flag plus kickoff at least 12 hours before cutoff; exact final timestamps unavailable.",
    prior_team_games = nrow(prior), new_requests_needed = needed,
    new_requests_allowed = max_new_requests, new_requests_used = 0L,
    execute = execute, requests = requests, cached = cached,
    measure = mode,
    opportunity_contract = if (mode == "raw_involvement_ppa")
      "Distinct Rush, Reception, Target or Completion RB athlete/play associations with finite offensive CFBD play PPA. Failed fourth downs and fumbles retained. Missing PPA is unavailable, not zero. This is not exact replication of CFBD player aggregates." else
      "Provisional distinct Rush or Reception athlete/play associations with finite offensive CFBD PPA, subject to strict same-game aggregate match. Missing PPA is unavailable, not zero.",
    role_audit_scope = "All finite-PPA offensive associations for RB IDs returned by the same-game player-PPA endpoint. Incidental roles on represented athlete/play pairs do not add usage; unknown pairs fail the game. Position classification outside those API RB IDs is unavailable.",
    future_performance_policy = "Season endpoint is cached raw; only regular player-game rows belonging to the strict prior schedule are reconstructed.",
    schedule_source = normalizePath(schedule_path, winslash = "/"),
    schedule_sha256 = digest::digest(file = schedule_path, algo = "sha256"))
  write_manifest <- function() jsonlite::write_json(manifest, file.path(output, "manifest.json"),
    pretty = TRUE, auto_unbox = TRUE, digits = 16)
  prior$rb_data_status <- "not_collected"
  prior$rb_rows <- 0L
  prior$missing_ppa_involvements <- NA_integer_
  prior$detail <- ""
  jsonlite::write_json(manifest, file.path(output, "fetch_plan.json"),
    pretty = TRUE, auto_unbox = TRUE, digits = 16)
  if (!execute) {
    write.csv(prior, file.path(output, "prior_team_games_plan.csv"), row.names = FALSE)
    return(invisible(list(directory = output, prior_games = prior,
      new_requests_needed = needed, requests = requests)))
  }
  if (needed > max_new_requests) stop("Batch needs ", needed, " new requests; budget is ", max_new_requests,
    ". No network requests made. Plan: ", output)
  write.csv(prior, file.path(output, "prior_team_games.csv"), row.names = FALSE)
  write_manifest()
  data <- list()
  for (i in seq_along(requests)) {
    request <- requests[[i]]
    if (!cached[i]) manifest$new_requests_used <- manifest$new_requests_used + 1L
    write_manifest()
    data[[request$label]] <- injury_player_fetch(directory, request$label, request$endpoint, request$query)
  }
  history <- data.frame(game_id = character(), kickoff = character(), season = integer(),
    week = integer(), team = character(), athlete_id = character(), player_name = character(),
    ppa_involvements = integer(), total_ppa = numeric(), average_ppa = numeric(),
    raw_involvements = integer(), missing_ppa_involvements = integer(),
    api_average_ppa = numeric(), api_mean_absolute_difference = numeric(),
    api_aggregate_reconciled = logical(), measure = character(),
    rush_associations = integer(), reception_associations = integer(),
    target_associations = integer(), completion_associations = integer())
  if (nrow(prior)) {
    ppa <- data[[paste0(tag, "_ppa")]]
    if (is.data.frame(ppa) && nrow(ppa)) ppa <- ppa[ppa$season == season &
      ppa$week %in% prior$week & ppa$seasonType == "regular" & ppa$team == api_team, ]
    for (i in seq_len(nrow(prior))) {
      if (!is.data.frame(ppa) || !nrow(ppa) || !any(ppa$week == prior$week[i])) {
        prior$rb_data_status[i] <- "no_rb_ppa_records"
        prior$detail[i] <- "Game remains in team-game denominator; no RB player-PPA rows available."
        next
      }
      prefix <- paste0(tag, "_w", prior$week[i])
      exact <- tryCatch(injury_exact_rb_game(data[[paste0(prefix, "_plays")]],
        data[[paste0(prefix, "_stats")]], ppa, season, prior$week[i], api_team, prior$game_id[i], mode),
        error = function(e) e)
      if (inherits(exact, "error")) {
        prior$rb_data_status[i] <- "reconstruction_failed"
        prior$detail[i] <- conditionMessage(exact)
        next
      }
      exact$kickoff <- prior$kickoff[i]
      prior$rb_data_status[i] <- if (mode == "raw_involvement_ppa") "raw_join_validated" else "matched"
      prior$rb_rows[i] <- nrow(exact)
      prior$missing_ppa_involvements[i] <- sum(exact$missing_ppa_involvements)
      history <- rbind(history, exact[names(history)])
      write.csv(attr(exact, "involvements"), file.path(output,
        paste0("game_", prior$game_id[i], "_involvements.csv")), row.names = FALSE)
      write.csv(attr(exact, "role_audit"), file.path(output,
        paste0("game_", prior$game_id[i], "_role_audit.csv")), row.names = FALSE)
    }
  }
  stopifnot(all(history$game_id %in% prior$game_id),
    all(injury_player_utc(history$kickoff) < cutoff))
  manifest$validated_games <- sum(prior$rb_data_status %in% c("matched", "raw_join_validated"))
  manifest$history_complete <- nrow(prior) > 0L && all(prior$rb_data_status %in% c("matched", "raw_join_validated"))
  manifest$api_aggregate_reconciled <- manifest$history_complete && all(history$api_aggregate_reconciled)
  manifest$api_aggregate_mismatch_rows <- sum(!history$api_aggregate_reconciled)
  manifest$player_game_rows <- nrow(history)
  raw_paths <- file.path(directory, paste0(vapply(requests, function(x) x$label, ""), ".json"))
  manifest$source_hashes <- data.frame(path = raw_paths,
    sha256 = vapply(raw_paths, function(path) digest::digest(file = path, algo = "sha256"), ""))
  write.csv(history, file.path(output, "player_game_history.csv"), row.names = FALSE)
  write.csv(prior, file.path(output, "prior_team_games.csv"), row.names = FALSE)
  write_manifest()
  invisible(list(directory = output, history = history, prior_games = prior,
    new_requests_used = manifest$new_requests_used, complete = manifest$history_complete))
}
