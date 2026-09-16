og_extract_public_sp <- function(table) {
  required <- c("Team", "SP+", "Off. SP+", "Def. SP+")
  assert_columns(table, required, "public FBS SP+ table")
  if (any(vapply(required, function(name) sum(names(table) == name) != 1L, logical(1))))
    stop("Ambiguous SP+ table columns.")
  team <- trimws(as.character(table[["Team"]]))
  keep <- !is.na(team) & nzchar(team)
  result <- data.frame(source_team = team[keep], team = canonical_team(team[keep]))
  aliases <- c("Miami-FL" = "Miami", "Miami-OH" = "Miami (OH)",
    "UL-Lafayette" = "Louisiana", "USF" = "South Florida", "Appalachian State" = "App State",
    "Hawaii" = "Hawai'i", "UL-Monroe" = "UL Monroe")
  hit <- match(result$team, names(aliases))
  result$team[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  for (i in seq_along(required[-1])) result[[c("rating", "offense_rating", "defense_rating")[i]]] <-
    suppressWarnings(as.numeric(table[[required[i + 1L]]][keep]))
  assert_unique_keys(result, "team", "public FBS SP+ table")
  result
}

og_sp_rejection <- function(rows, cutoff, season) {
  assert_columns(rows, c("team", "season", "rating", "offense_rating", "defense_rating",
    "available_at", "through_at", "source_url", "evidence_reviewed"), "SP snapshot evidence")
  stopifnot(length(cutoff) %in% c(1L, nrow(rows)), length(season) %in% c(1L, nrow(rows)))
  rows$team <- canonical_team(rows$team)
  assert_unique_keys(rows, c("team", "season", "available_at"), "SP snapshot evidence")
  available <- parse_utc_datetime(rows$available_at)
  through <- parse_utc_datetime(rows$through_at)
  reason <- rep("", nrow(rows))
  reject <- function(condition, value) {
    condition[is.na(condition)] <- TRUE
    reason[nzchar(reason) == FALSE & condition] <<- value
  }
  reject(is.na(rows$season) | rows$season != season, "wrong_or_missing_season")
  reject(is.na(rows$team) | !nzchar(rows$team), "missing_team")
  reject(!is.finite(rows$rating) | !is.finite(rows$offense_rating) |
    !is.finite(rows$defense_rating), "missing_numeric_rating")
  reject(is.na(available), "missing_availability_time")
  reject(is.na(through), "missing_data_through_time")
  # The shared parser is permissive about offsets; require explicit UTC evidence here.
  utc_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$"
  reject(!grepl(utc_pattern, rows$available_at) | !grepl(utc_pattern, rows$through_at),
    "timestamp_not_explicit_UTC")
  reject(is.na(cutoff), "missing_prediction_cutoff")
  reject(available >= cutoff | through >= cutoff, "not_before_cutoff")
  reject(through > available, "data_after_publication")
  reject(is.na(rows$source_url) | !nzchar(trimws(rows$source_url)) |
    is.na(rows$evidence_reviewed) | !rows$evidence_reviewed, "unreviewed_source")
  reason
}

og_history_ceiling <- function(games, stats, source_games) {
  stats$pos_team <- canonical_team(stats$pos_team)
  assert_unique_keys(stats, c("game_id", "pos_team"), "legacy team-game statistics")
  assert_unique_keys(source_games, "game_id", "legacy game dates")
  stats$kickoff <- parse_utc_datetime(source_games$start_date[match(stats$game_id, source_games$game_id)])
  index <- match(stats$game_id, games$game_id)
  hit <- !is.na(index)
  stats$kickoff[hit] <- parse_utc_datetime(games$kickoff[index[hit]])
  games <- add_chronological_model_week(games)
  games$cutoff <- as.POSIXct(games$feature_week_start, tz = "UTC")
  groups <- split(seq_len(nrow(stats)), paste(stats$year, stats$pos_team))
  for (side in c("home", "away")) {
    counts <- integer(nrow(games)); complete <- rep(FALSE, nrow(games))
    for (i in seq_len(nrow(games))) {
      if (is.na(games$cutoff[i])) next
      group <- groups[[paste(games$season[i], canonical_team(games[[side]][i]))]]
      prior <- group[!is.na(stats$kickoff[group]) & stats$kickoff[group] < games$cutoff[i]]
      prior <- prior[order(stats$kickoff[prior], stats$game_id[prior])]
      counts[i] <- length(prior)
      if (counts[i] >= 6L) complete[i] <- all(is.finite(as.matrix(
        stats[tail(prior, 6L), c("epa_play", "epa_pass", "epa_rush", "wpa_play")])) )
    }
    games[[paste0(side, "_prior_games")]] <- counts
    games[[paste0(side, "_six_complete")]] <- complete
  }
  games$six_game_history_ceiling <- games$home_six_complete & games$away_six_complete
  games
}

og_database_inventory <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  tables <- intersect(c("public_rating_challengers", "line_snapshots", "model_runs",
    "foundation_runs"), DBI::dbListTables(con))
  lapply(stats::setNames(tables, tables), function(table) {
    quoted <- as.character(DBI::dbQuoteIdentifier(con, table))
    list(columns = DBI::dbListFields(con, table),
      rows = DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", quoted))$n,
      sample = DBI::dbGetQuery(con, paste("SELECT * FROM", quoted, "LIMIT 3")))
  })
}

og_elo_alignment <- function(games, elo) {
  elo$team <- canonical_team(elo$team)
  assert_unique_keys(elo, c("team", "year", "week"), "legacy weekly Elo")
  key <- paste(elo$team, elo$year, elo$week)
  sides <- do.call(rbind, lapply(c("home", "away"), function(side) {
    lookup <- match(paste(canonical_team(games[[side]]), games$season, games$week), key)
    data.frame(game_id = games$game_id, season = games$season, week = games$week,
      side = side, team = games[[side]], postseason_type = games$postseason_type,
      pregame_elo = games[[paste0(side, "_pregame_elo")]], cached_same_week_elo = elo$elo[lookup])
  }))
  sides$comparable <- is.finite(sides$pregame_elo) & is.finite(sides$cached_same_week_elo)
  sides$equal <- sides$comparable & abs(sides$pregame_elo - sides$cached_same_week_elo) < 1e-8
  sides
}

run_og_input_audit <- function(config) {
  stopifnot(identical(config$version, "3.0.0"))
  protected <- experiment_file_hashes(config)
  v2_db <- file.path(config$project_dir, "cfb_v2", "data", "cfb_v2.duckdb")
  before_v2 <- tools::md5sum(v2_db)
  directory <- file.path(config$output_dir, "experiments", "og_audit",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  stopifnot(!dir.exists(directory))
  dir.create(directory, recursive = TRUE)
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  file.copy(file.path(root, "OG_COMPARISON_PROTOCOL.md"), file.path(directory, "PROTOCOL.md"))
  legacy_path <- file.path(config$output_dir, "experiments", "model1_inputs", "original_workflow_inputs.rds")
  game_cache <- file.path(config$output_dir, "experiments", "v1_inputs", "workspace_inputs.rds")
  stopifnot(file.exists(legacy_path), file.exists(game_cache))
  legacy <- readRDS(legacy_path)
  original <- readRDS(game_cache)
  sp <- legacy$sp
  sp$team <- canonical_team(sp$team)
  assert_unique_keys(sp, c("team", "year"), "annual cached SP+")
  stopifnot(identical(sort(names(legacy$sp)),
    sort(c("team", "year", "rating", "offense_rating", "defense_rating"))))
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  assert_unique_keys(games, "game_id", "historical games")
  games <- og_history_ceiling(games, original$stats, original$games)
  pool <- games[which(games$season %in% 2022:2025 & raw_history_eligible(games) &
    as.logical(games$completed) & is.finite(games$margin)), ]
  pool$pregame_elo_available <- is.finite(pool$home_pregame_elo) & is.finite(pool$away_pregame_elo)
  pool$closing_line_available <- is.finite(pool$closing_home_spread)
  sp_key <- paste(sp$team, sp$year)
  sp_complete <- complete.cases(sp[c("rating", "offense_rating", "defense_rating")])
  for (side in c("home", "away")) {
    index <- match(paste(canonical_team(pool[[side]]), pool$season), sp_key)
    pool[[paste0(side, "_annual_sp_available")]] <- !is.na(index) & sp_complete[index]
  }
  pool$annual_sp_available <- pool$home_annual_sp_available & pool$away_annual_sp_available
  evidence <- data.frame(team = sp$team, season = sp$year, rating = sp$rating,
    offense_rating = sp$offense_rating, defense_rating = sp$defense_rating,
    available_at = NA_character_, through_at = NA_character_,
    source_url = NA_character_, evidence_reviewed = FALSE)
  rejection <- og_sp_rejection(evidence, parse_utc_datetime("2026-01-01T00:00:00Z"), sp$year)
  stopifnot(all(nzchar(rejection)))
  sp$gate_rejection <- rejection
  pool$dated_sp_available <- FALSE
  pool$og_excluded_team <- pool$home %in% c("James Madison", "Jacksonville State", "Sam Houston") |
    pool$away %in% c("James Madison", "Jacksonville State", "Sam Houston")
  pool$ready_for_scored_comparison <- FALSE
  pool$blocking_reason <- "unverified_SP_publication_and_data_cutoff"
  cols <- c("game_id", "season", "week", "model_week", "cutoff", "home", "away", "is_cfp",
    "postseason_type", "home_prior_games", "away_prior_games", "six_game_history_ceiling",
    "og_excluded_team", "pregame_elo_available", "closing_line_available", "annual_sp_available",
    "dated_sp_available", "ready_for_scored_comparison", "blocking_reason")
  write.csv(pool[cols], file.path(directory, "game_coverage.csv"), row.names = FALSE)
  write.csv(sp, file.path(directory, "annual_sp_audit.csv"), row.names = FALSE)
  coverage <- do.call(rbind, lapply(2022:2025, function(season) {
    x <- pool[pool$season == season, ]
    data.frame(season = season, candidate_games = nrow(x), cfp_games = sum(x$is_cfp),
      conference_championships = sum(x$postseason_type == "conference_championship"),
      both_pregame_elo = sum(x$pregame_elo_available), closing_lines = sum(x$closing_line_available),
      annual_sp_both_teams = sum(x$annual_sp_available), six_game_history_ceiling = sum(x$six_game_history_ceiling),
      og_excluded_games = sum(x$og_excluded_team), dated_sp_games = sum(x$dated_sp_available),
      ready_games = sum(x$ready_for_scored_comparison))
  }))
  write.csv(coverage, file.path(directory, "coverage_by_season.csv"), row.names = FALSE)
  alignment <- og_elo_alignment(pool, legacy$elo)
  write.csv(alignment, file.path(directory, "elo_alignment.csv"), row.names = FALSE)
  databases <- list(v2 = og_database_inventory(v2_db), v3 = og_database_inventory(config$database))
  public_files <- file.path(config$project_dir, c("cfb_v2", "cfb_v3"), "inbox", "public_rating_challengers.csv")
  public_counts <- vapply(public_files, function(path) nrow(read.csv(path)), integer(1))
  if (any(public_counts > 0) || any(vapply(databases,
      function(db) db$public_rating_challengers$rows > 0, logical(1)))) {
    stop("New rating ledger rows found: review their provenance before certifying this audit.")
  }
  stopifnot(identical(protected, experiment_file_hashes(config)),
    identical(before_v2, tools::md5sum(v2_db)))
  verified <- list(status = "local_cache_rejected_external_source_review_pending", models_fitted = 0L, ready = FALSE,
    production_unchanged = TRUE, candidate_games = nrow(pool),
    sp_columns = names(legacy$sp), elo_columns = names(legacy$elo),
    sp_year_counts = as.list(table(legacy$sp$year)), elo_year_counts = as.list(table(legacy$elo$year)),
    public_csv_rows = as.list(public_counts), database_inventory = databases,
    source_hashes = as.list(tools::md5sum(c(legacy_path, game_cache,
      file.path(root, "OG_COMPARISON_PROTOCOL.md"), file.path(root, "audit_og_inputs.R")))),
    protected = protected, external_evidence = "See OG_DATA_AUDIT.md; online source review is manual and dated.")
  jsonlite::write_json(verified, file.path(directory, "verification.json"), pretty = TRUE, auto_unbox = TRUE)
  header <- c("| Season | Games | Pregame Elo | Closing lines | Six-game ceiling | Dated SP+ |",
    "|---|---:|---:|---:|---:|---:|")
  table <- vapply(seq_len(nrow(coverage)), function(i) {
    x <- coverage[i, ]
    sprintf("| %d | %d | %d | %d | %d | %d |", x$season, x$candidate_games,
      x$both_pregame_elo, x$closing_lines, x$six_game_history_ceiling, x$dated_sp_games)
  }, character(1))
  comparable <- alignment[alignment$comparable & alignment$postseason_type == "regular", ]
  report <- file.path(directory, "REPORT.md")
  writeLines(c("# OG Input Readiness Audit", "", "Status: local cache rejected; external source review is separate. No models fitted or predictions scored.", "",
    header, table, "", "Six-game history is an upper bound, not actual OG coverage. The original side-specific lookup and exclusions can reduce it further.",
    "Conference championships are included; non-CFP bowls are excluded from the evaluation pool. No training bowl policy was changed.", "",
    "The cached SP+ table has only team, year and three rating columns. Numeric coverage does not establish availability before a historical prediction cutoff. Dated SP+ counts above refer only to local production/cache inputs, not subsequently recovered public workbooks.",
    "Both v2/v3 public-rating CSVs and database tables are empty. This audit does not manufacture weekly timestamps for annual values.",
    sprintf("Regular-season Elo diagnostic: %d of %d comparable team-game rows match between cached same-week Elo and the schedule's explicitly pregame Elo. Differences are not silently replaced; see elo_alignment.csv.", sum(comparable$equal), nrow(comparable)),
    "Use explicit pregame Elo only after mapping it to the same historical lookup/cutoff. Weekly numbers alone cannot certify source timing.",
    "Closing-line values are available for most games, but are not original Friday provider/price snapshots. The database sample and schema are retained in verification.json.", "",
    "Scope: extracted original .RData tables, historical games/statistics, both v2/v3 public-rating ledgers and database schemas. Older .RDataTmp backups were not decoded, and filesystem timestamps were not treated as publication evidence.",
    "Production engine/data/card hashes and the v2 database hash are unchanged. No API quota, subscription purchase, model fit, commit or push is part of this audit.", "",
    "The locked comparison protocol is copied beside this report. External source findings are documented in cfb_v3/experiments/OG_DATA_AUDIT.md."), report)
  list(report = report, coverage = coverage, ready = FALSE, verification = verified)
}
