args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x

parse_args <- function(args) {
  parsed <- list(mode = "article", season = as.integer(format(Sys.Date(), "%Y")),
                 week = NA_integer_, as_of = Sys.time(), force = character(), strict = TRUE,
                 seasons = 2020:2025, refresh_pbp = FALSE,
                 refresh_schedule = FALSE, refresh_coaches = FALSE,
                 refresh_espn = FALSE, refresh_preseason = FALSE, overwrite = FALSE)
  for (arg in args) {
    pair <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- gsub("-", "_", pair[1])
    value <- if (length(pair) > 1) paste(pair[-1], collapse = "=") else "true"
    if (key == "mode") parsed$mode <- value
    else if (key == "season") parsed$season <- as.integer(value)
    else if (key == "week") parsed$week <- as.integer(value)
    else if (key == "as_of") parsed$as_of <- as.POSIXct(value, tz = "America/New_York")
    else if (key == "force") parsed$force <- Filter(nzchar, trimws(strsplit(value, ",")[[1]]))
    else if (key == "strict") parsed$strict <- tolower(value) %in% c("true", "1", "yes")
    else if (key == "seasons") {
      if (grepl(":", value, fixed = TRUE)) {
        bounds <- as.integer(strsplit(value, ":", fixed = TRUE)[[1]])
        parsed$seasons <- seq.int(bounds[1], bounds[2])
      } else parsed$seasons <- as.integer(strsplit(value, ",", fixed = TRUE)[[1]])
    }
    else if (key == "refresh_pbp") parsed$refresh_pbp <- tolower(value) %in% c("true", "1", "yes")
    else if (key == "refresh_schedule") parsed$refresh_schedule <- tolower(value) %in% c("true", "1", "yes")
    else if (key == "refresh_coaches") parsed$refresh_coaches <- tolower(value) %in% c("true", "1", "yes")
    else if (key == "refresh_espn") parsed$refresh_espn <- tolower(value) %in% c("true", "1", "yes")
    else if (key == "refresh_preseason") parsed$refresh_preseason <- tolower(value) %in% c("true", "1", "yes")
    else if (key == "overwrite") parsed$overwrite <- tolower(value) %in% c("true", "1", "yes")
    else stop("Unknown argument: --", key, call. = FALSE)
  }
  parsed
}

script_path <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_path)) {
  project_dir <- dirname(normalizePath(sub("^--file=", "", script_path[1]),
                                      winslash = "/", mustWork = TRUE))
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(project_dir, "cfb_v2", "config.R"))
source(file.path(project_dir, "cfb_v2", "store.R"))
source(file.path(project_dir, "cfb_v2", "features.R"))
source(file.path(project_dir, "cfb_v2", "coaches.R"))
source(file.path(project_dir, "cfb_v2", "adapters.R"))
source(file.path(project_dir, "cfb_v2", "models.R"))
source(file.path(project_dir, "cfb_v2", "coach_migration.R"))
source(file.path(project_dir, "cfb_v2", "historical_data.R"))
source(file.path(project_dir, "cfb_v2", "preseason.R"))
source(file.path(project_dir, "cfb_v2", "bridge.R"))
source(file.path(project_dir, "cfb_v2", "workflow.R"))

options(warn = 1)
cli <- parse_args(args)
config <- cfb_v2_config(project_dir, cli$season)

if (cli$mode == "init") {
  initialized <- initialize_v2_project(config)
  cat("CFB v2 initialized. Inbox templates:", paste(initialized, collapse = ", "), "\n")
} else if (cli$mode == "build-foundation") {
  initialize_v2_project(config)
  result <- build_historical_foundation(
    config, seasons = cli$seasons,
    refresh_pbp = cli$refresh_pbp, refresh_schedule = cli$refresh_schedule,
    refresh_coaches = cli$refresh_coaches,
    refresh_espn = cli$refresh_espn
  )
  paths <- write_historical_foundation(result, config, overwrite = cli$overwrite)
  cat("Foundation seasons:", paste(result$seasons, collapse = ", "), "\n")
  cat("Training games:", nrow(result$training_games), "\n")
  cat("Coach assignments:", nrow(result$coach_assignments), "\n")
  cat("Foundation run:", paths$foundation_run_id, "\n")
  cat("DuckDB:", paths$database, "\n")
  cat("QA report:", paths$foundation_qa, "\n")
} else if (cli$mode == "backtest") {
  result <- run_v2_backtest(config)
  cat("Backtest report:", result$report, "\n")
  cat("Predictions:", result$predictions, "\n")
  cat("Summary:", result$summary, "\n")
  cat("Preseason challenger report:", result$preseason_report, "\n")
} else if (cli$mode == "build-preseason") {
  result <- build_preseason_priors(
    config, seasons = cli$seasons,
    refresh = cli$refresh_preseason, overwrite = cli$overwrite
  )
  cat("Preseason priors:", result$path, "\n")
  print(result$coverage)
} else if (cli$mode %in% c("article", "live")) {
  if (is.na(cli$week)) stop("--week is required for article/live runs.", call. = FALSE)
  result <- run_v2_week(
    config, cli$season, cli$week, cli$mode, cli$as_of,
    force_game_ids = cli$force, strict = cli$strict
  )
  cat("Run:", result$run_id, "\n")
  cat("CSV:", result$csv, "\n")
  cat("Parquet:", result$parquet, "\n")
  print(result$predictions[c("away", "home", "expected_margin", "fair_spread",
                             "straight_up_pick", "ats_pick", "pick_status",
                             "confidence_tier")])
} else {
  stop("--mode must be init, build-foundation, build-preseason, backtest, article, or live.",
       call. = FALSE)
}
