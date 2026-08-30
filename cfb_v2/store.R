require_v2_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required. Install it before running CFB v2.",
         call. = FALSE)
  }
}

v2_connect <- function(config, read_only = FALSE) {
  require_v2_package("DBI")
  require_v2_package("duckdb")
  ensure_v2_directories(config)
  DBI::dbConnect(duckdb::duckdb(), dbdir = config$database, read_only = read_only)
}

v2_disconnect <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(NULL)
}

v2_schema_sql <- function() {
  c(
    "CREATE TABLE IF NOT EXISTS model_runs (
       run_id VARCHAR PRIMARY KEY, run_type VARCHAR NOT NULL, season INTEGER,
       week INTEGER, as_of TIMESTAMP NOT NULL, created_at TIMESTAMP NOT NULL,
       config_json VARCHAR, status VARCHAR NOT NULL, message VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS foundation_runs (
       foundation_run_id VARCHAR PRIMARY KEY, seasons VARCHAR NOT NULL,
       created_at TIMESTAMP NOT NULL, schedule_source VARCHAR,
       coach_source VARCHAR, status VARCHAR NOT NULL, qa_json VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS game_schedule (
       game_id VARCHAR, season INTEGER, week INTEGER, kickoff TIMESTAMP,
       home VARCHAR, away VARCHAR, neutral_site BOOLEAN, venue VARCHAR,
       home_level VARCHAR, away_level VARCHAR, postseason_type VARCHAR,
       conference_championship BOOLEAN, is_cfp BOOLEAN, source VARCHAR,
       captured_at TIMESTAMP, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS line_snapshots (
       game_id VARCHAR, provider VARCHAR, captured_at TIMESTAMP,
       snapshot_type VARCHAR, home_spread DOUBLE, total DOUBLE,
       home_price INTEGER, away_price INTEGER, source_url VARCHAR,
       run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS injury_snapshots (
       game_id VARCHAR, team VARCHAR, player_id VARCHAR, player_name VARCHAR,
       position VARCHAR, status VARCHAR, usage_share DOUBLE, starter BOOLEAN,
       impact_points DOUBLE, availability_probability DOUBLE,
       source_type VARCHAR, source_url VARCHAR,
       source_timestamp TIMESTAMP, captured_at TIMESTAMP, confidence DOUBLE,
       conflict_flag BOOLEAN, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS coach_assignments (
       team VARCHAR, season INTEGER, start_week INTEGER, end_week INTEGER,
       coach_id VARCHAR, coach_name VARCHAR, interim BOOLEAN,
       source VARCHAR, assignment_confidence VARCHAR, source_games INTEGER,
       schedule_games INTEGER, needs_review BOOLEAN, inference_reason VARCHAR,
       effective_date DATE, source_url VARCHAR, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS coach_ratings (
       coach_id VARCHAR, season INTEGER, as_of_week INTEGER,
       recent_above_expectation DOUBLE, historical_win_value DOUBLE,
       experience_value DOUBLE, portability DOUBLE, current_season_value DOUBLE,
       rating_65_35 DOUBLE, rating_70_30 DOUBLE, games_available INTEGER,
       run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS team_week_features (
       team VARCHAR, season INTEGER, week INTEGER, as_of TIMESTAMP,
       offense_rating DOUBLE, defense_rating DOUBLE, special_teams_rating DOUBLE,
       recent_3 DOUBLE, recent_6 DOUBLE, season_to_date DOUBLE,
       prior_season DOUBLE, trailing_3yr DOUBLE, preseason_prior DOUBLE,
       qb_continuity DOUBLE, roster_continuity DOUBLE, staff_continuity DOUBLE,
       coach_rating DOUBLE, home_field_rating DOUBLE, source_games INTEGER,
       run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS public_rating_challengers (
       team VARCHAR, season INTEGER, week INTEGER, provider VARCHAR,
       rating DOUBLE, captured_at TIMESTAMP, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS rankings_snapshots (
       team VARCHAR, season INTEGER, week INTEGER, ranking_type VARCHAR,
       rank INTEGER, captured_at TIMESTAMP, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS team_membership (
       team VARCHAR, season INTEGER, subdivision VARCHAR, power_conference BOOLEAN,
       conference VARCHAR, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS cfp_probability_snapshots (
       team VARCHAR, season INTEGER, week INTEGER, cfp_probability DOUBLE,
       conference_leader BOOLEAN, captured_at TIMESTAMP, run_id VARCHAR
     )",
    "CREATE TABLE IF NOT EXISTS prediction_snapshots (
       run_id VARCHAR, game_id VARCHAR, snapshot_type VARCHAR, season INTEGER,
       week INTEGER, as_of TIMESTAMP, home VARCHAR, away VARCHAR,
       expected_margin DOUBLE, fair_spread DOUBLE, margin_sd DOUBLE,
       foundation_expected_margin DOUBLE, preseason_expected_margin DOUBLE,
       preseason_challenger_share DOUBLE, preseason_raw_delta DOUBLE,
       preseason_adjustment DOUBLE,
       expected_total DOUBLE, total_sd DOUBLE,
       home_win_probability DOUBLE, market_home_spread DOUBLE,
       ats_edge_home DOUBLE, home_cover_probability DOUBLE,
       straight_up_pick VARCHAR, ats_pick VARCHAR, pick_status VARCHAR,
       confidence_tier VARCHAR, forced_pick BOOLEAN, fbs_transition VARCHAR,
       injury_scenario VARCHAR,
       top_drivers VARCHAR, model_version VARCHAR, published BOOLEAN,
       created_at TIMESTAMP
     )",
    "CREATE TABLE IF NOT EXISTS model_artifacts (
       model_id VARCHAR PRIMARY KEY, run_id VARCHAR, target VARCHAR,
       trained_through_season INTEGER, trained_through_week INTEGER,
       feature_names_json VARCHAR, parameters_json VARCHAR,
       artifact_path VARCHAR, created_at TIMESTAMP
     )"
  )
}

v2_init_schema <- function(con) {
  for (sql in v2_schema_sql()) DBI::dbExecute(con, sql)
  DBI::dbExecute(
    con, "ALTER TABLE prediction_snapshots ADD COLUMN IF NOT EXISTS expected_total DOUBLE"
  )
  DBI::dbExecute(
    con, "ALTER TABLE prediction_snapshots ADD COLUMN IF NOT EXISTS total_sd DOUBLE"
  )
  blend_columns <- c(
    foundation_expected_margin = "DOUBLE",
    preseason_expected_margin = "DOUBLE",
    preseason_challenger_share = "DOUBLE",
    preseason_raw_delta = "DOUBLE",
    preseason_adjustment = "DOUBLE"
  )
  for (column in names(blend_columns)) {
    DBI::dbExecute(
      con,
      paste("ALTER TABLE prediction_snapshots ADD COLUMN IF NOT EXISTS", column,
            blend_columns[[column]])
    )
  }
  DBI::dbExecute(
    con, paste("ALTER TABLE injury_snapshots ADD COLUMN IF NOT EXISTS",
               "availability_probability DOUBLE")
  )
  DBI::dbExecute(
    con, "ALTER TABLE prediction_snapshots ADD COLUMN IF NOT EXISTS fbs_transition VARCHAR"
  )
  coach_columns <- c(
    assignment_confidence = "VARCHAR", source_games = "INTEGER",
    schedule_games = "INTEGER", needs_review = "BOOLEAN",
    inference_reason = "VARCHAR", effective_date = "DATE", source_url = "VARCHAR"
  )
  for (column in names(coach_columns)) {
    DBI::dbExecute(
      con,
      paste("ALTER TABLE coach_assignments ADD COLUMN IF NOT EXISTS", column,
            coach_columns[[column]])
    )
  }
  invisible(con)
}

v2_foundation_run_id <- function(seasons, created_at = Sys.time()) {
  paste0(
    "foundation_", min(seasons), "_", max(seasons), "_",
    format(as.POSIXct(created_at, tz = "UTC"), "%Y%m%dT%H%M%SZ")
  )
}

v2_replace_foundation_table <- function(con, table, data, foundation_run_id) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$foundation_run_id <- foundation_run_id
  data <- data[c("foundation_run_id", setdiff(names(data), "foundation_run_id"))]
  DBI::dbWithTransaction(
    con,
    DBI::dbWriteTable(con, table, data, overwrite = TRUE)
  )
  invisible(nrow(data))
}

v2_persist_foundation <- function(result, config, created_at = Sys.time()) {
  con <- v2_connect(config)
  on.exit(v2_disconnect(con), add = TRUE)
  v2_init_schema(con)
  run_id <- v2_foundation_run_id(result$seasons, created_at)
  tables <- list(
    foundation_games = result$games,
    foundation_team_games = result$team_games,
    foundation_team_snapshots = result$team_snapshots,
    foundation_fbs_membership = result$fbs_membership,
    foundation_training_games = result$training_games,
    foundation_coach_seasons = result$coach_seasons,
    foundation_coach_identities = result$coach_identities,
    foundation_coach_transitions = result$coach_transitions,
    foundation_coach_assignments = result$coach_assignments,
    foundation_coach_assignment_qa = result$coach_assignment_qa,
    foundation_coach_history = result$coach_history,
    foundation_coach_ratings = result$coach_ratings,
    foundation_qa = result$qa
  )
  for (table in names(tables)) {
    v2_replace_foundation_table(con, table, tables[[table]], run_id)
  }
  schedule_source <- paste(sort(unique(result$games$schedule_source)), collapse = "+")
  coach_source <- paste(sort(unique(result$coach_assignments$source)), collapse = "+")
  row <- data.frame(
    foundation_run_id = run_id,
    seasons = paste(result$seasons, collapse = ","),
    created_at = as.POSIXct(created_at, tz = "UTC"),
    schedule_source = schedule_source, coach_source = coach_source,
    status = "complete",
    qa_json = as.character(jsonlite::toJSON(result$qa, dataframe = "rows",
                                             auto_unbox = TRUE, na = "null")),
    stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(con, "foundation_runs", row)
  list(run_id = run_id, database = config$database, tables = names(tables))
}

v2_run_id <- function(run_type, season, week, as_of = Sys.time()) {
  clean_time <- format(as.POSIXct(as_of, tz = "UTC"), "%Y%m%dT%H%M%SZ")
  paste(run_type, season, sprintf("w%02d", as.integer(week)), clean_time, sep = "_")
}

v2_register_run <- function(con, run_id, run_type, season, week, as_of,
                            config, status = "started", message = NA_character_) {
  row <- data.frame(
    run_id = run_id, run_type = run_type, season = as.integer(season),
    week = as.integer(week), as_of = as.POSIXct(as_of, tz = "UTC"),
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    config_json = as.character(jsonlite::toJSON(config, auto_unbox = TRUE, null = "null")),
    status = status, message = message, stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(con, "model_runs", row)
  invisible(row)
}

v2_finish_run <- function(con, run_id, status = "complete", message = NA_character_) {
  DBI::dbExecute(
    con,
    "UPDATE model_runs SET status = ?, message = ? WHERE run_id = ?",
    params = list(status, message, run_id)
  )
  invisible(run_id)
}

v2_assert_article_slot_open <- function(con, season, week) {
  count <- DBI::dbGetQuery(
    con,
    paste("SELECT COUNT(*) AS n FROM prediction_snapshots",
          "WHERE season = ? AND week = ? AND published = TRUE"),
    params = list(as.integer(season), as.integer(week))
  )$n[[1]]
  if (count > 0) {
    stop("A published article snapshot already exists for ", season, " Week ", week,
         ". Use --mode=live for later information.", call. = FALSE)
  }
  invisible(TRUE)
}

assert_columns <- function(data, required, object_name = deparse(substitute(data))) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(object_name, " is missing required columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

assert_unique_keys <- function(data, keys, object_name = deparse(substitute(data))) {
  assert_columns(data, keys, object_name)
  if (!nrow(data)) return(invisible(TRUE))
  key <- do.call(paste, c(lapply(data[keys], function(x) ifelse(is.na(x), "<NA>", x)),
                          sep = "\r"))
  if (anyDuplicated(key)) {
    example <- unique(key[duplicated(key) | duplicated(key, fromLast = TRUE)])[1]
    stop(object_name, " contains duplicate key values for ", paste(keys, collapse = "+"),
         ". Example: ", gsub("\r", " | ", example), call. = FALSE)
  }
  invisible(TRUE)
}

v2_append_snapshot <- function(con, table, data, run_id, key_columns = character()) {
  if (!nrow(data)) return(invisible(0L))
  if (!"run_id" %in% names(data)) data$run_id <- run_id
  if (any(data$run_id != run_id)) stop("Snapshot rows must use the active run_id.", call. = FALSE)
  if (length(key_columns)) assert_unique_keys(data, c("run_id", key_columns), table)

  existing <- DBI::dbGetQuery(
    con, paste0("SELECT COUNT(*) AS n FROM ", DBI::dbQuoteIdentifier(con, table),
                " WHERE run_id = ?"), params = list(run_id)
  )$n[[1]]
  if (existing > 0) {
    stop("Immutable snapshot already exists in ", table, " for run_id ", run_id,
         call. = FALSE)
  }

  table_fields <- DBI::dbListFields(con, table)
  unknown <- setdiff(names(data), table_fields)
  if (length(unknown)) {
    stop("Unknown columns for ", table, ": ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  missing <- setdiff(table_fields, names(data))
  for (column in missing) data[[column]] <- NA
  data <- data[table_fields]

  DBI::dbWithTransaction(con, DBI::dbAppendTable(con, table, data))
  invisible(nrow(data))
}

v2_export_snapshot <- function(con, table, run_id, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  safe_path <- gsub("'", "''", normalizePath(path, winslash = "/", mustWork = FALSE))
  safe_run <- gsub("'", "''", run_id)
  sql <- sprintf("COPY (SELECT * FROM %s WHERE run_id = '%s') TO '%s' (FORMAT PARQUET)",
                 DBI::dbQuoteIdentifier(con, table), safe_run, safe_path)
  DBI::dbExecute(con, sql)
  invisible(path)
}
