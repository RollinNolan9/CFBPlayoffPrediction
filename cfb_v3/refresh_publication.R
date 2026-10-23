# Refresh the market on an explicit frozen prediction card; never refit its model.
# Rscript cfb_v3/refresh_publication.R --predictions=cfb_v3/output/.../live_....csv

publication_utc <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

publication_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA", "N/A", "null"))
}

publication_align <- function(data, card, label) {
  assert_columns(data, c("game_id", "home", "away", "season", "week"), label)
  data$game_id <- as.character(data$game_id)
  assert_unique_keys(data, "game_id", label)
  hit <- match(card$game_id, data$game_id)
  if (anyNA(hit)) stop(label, " is missing games from the frozen card.", call. = FALSE)
  out <- data[hit, , drop = FALSE]
  matches <- canonical_team(out$home) == canonical_team(card$home) &
    canonical_team(out$away) == canonical_team(card$away) &
    out$season == card$season & out$week == card$week
  if (anyNA(matches) || !all(matches)) {
    stop(label, " game identities, season, or week disagree with the frozen card.",
         call. = FALSE)
  }
  out
}

publication_fresh_quotes <- function(raw, card, captured_at) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  column <- function(candidates, label) {
    raw[[first_existing_column(raw, candidates, label = label)]]
  }
  provider <- column(c("provider", "provider_name", "line_provider"), "provider")
  raw <- raw[tolower(gsub("\\s+", "", provider)) %in% "draftkings", , drop = FALSE]
  if (!nrow(raw)) stop("CFBD returned no DraftKings quotes.", call. = FALSE)
  metadata <- data.frame(
    game_id = as.character(column(c("game_id", "id", "id_game"), "game id")),
    home = column(c("homeTeam", "home_team", "home"), "home team"),
    away = column(c("awayTeam", "away_team", "away"), "away team"),
    season = as.integer(column(c("season", "year"), "season")),
    week = as.integer(column("week", "week")),
    kickoff = as.character(column(c("startDate", "start_date", "kickoff"), "kickoff")),
    stringsAsFactors = FALSE
  )
  selected <- metadata$game_id %in% card$game_id
  raw <- raw[selected, , drop = FALSE]
  metadata <- publication_align(metadata[selected, , drop = FALSE], card, "Fresh DraftKings quotes")
  raw_id <- as.character(raw[[first_existing_column(raw, c("game_id", "id", "id_game"))]])
  raw <- raw[match(card$game_id, raw_id), , drop = FALSE]
  kickoff <- parse_utc_datetime(metadata$kickoff)
  if (anyNA(kickoff) || any(kickoff <= captured_at)) {
    stop("Every publication game must have a known kickoff after the quote capture time.",
         call. = FALSE)
  }
  lines <- normalize_betting_lines(raw, captured_at)
  if (any(!is.finite(lines$home_spread))) {
    stop("Fresh DraftKings quotes must include a finite spread for every game.", call. = FALSE)
  }
  lines$captured_at <- publication_utc(captured_at)
  lines$home <- metadata$home
  lines$away <- metadata$away
  lines$season <- metadata$season
  lines$week <- metadata$week
  lines$kickoff <- publication_utc(kickoff)
  list(lines = lines, kickoff = kickoff, raw = raw)
}

refresh_saved_publication <- function(predictions_path, project_dir,
                                      bundle_path = NULL, features_path = NULL,
                                      fetch_lines = function(season, week) {
                                        cfbfastR::cfbd_betting_lines(year = season, week = week)
                                      }, render = render_cfb_dashboard,
                                      clock = Sys.time) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  predictions_path <- normalizePath(predictions_path, winslash = "/", mustWork = TRUE)
  if (is.null(bundle_path)) bundle_path <- sub("\\.csv$", "_model.rds", predictions_path,
                                             ignore.case = TRUE)
  if (is.null(features_path)) features_path <- sub("\\.csv$", "_features.csv", predictions_path,
                                                 ignore.case = TRUE)
  if (!file.exists(bundle_path) || !file.exists(features_path)) {
    stop("Matching model bundle and features are required; supply --bundle and --features ",
         "when they are not siblings of the raw live CSV.", call. = FALSE)
  }
  bundle_path <- normalizePath(bundle_path, winslash = "/", mustWork = TRUE)
  features_path <- normalizePath(features_path, winslash = "/", mustWork = TRUE)
  card <- publication_read_csv(predictions_path)
  assert_columns(card, c("game_id", "season", "week", "home", "away", "expected_margin",
                         "margin_sd", "straight_up_pick", "forced_pick", "pick_status", "as_of"),
                 "Frozen predictions")
  if (!nrow(card)) stop("The frozen prediction card is empty.", call. = FALSE)
  card$game_id <- as.character(card$game_id)
  if (anyNA(card$game_id) || any(!nzchar(card$game_id))) stop("Card game IDs must be present.")
  assert_unique_keys(card, "game_id", "Frozen predictions")
  season <- unique(as.integer(card$season))
  week <- unique(as.integer(card$week))
  if (length(season) != 1L || length(week) != 1L || anyNA(c(season, week))) {
    stop("The frozen card must contain exactly one season and one week.", call. = FALSE)
  }
  if (any(!is.finite(card$expected_margin)) || any(!is.finite(card$margin_sd)) ||
      any(card$margin_sd <= 0) || anyNA(card$forced_pick) ||
      anyNA(card$straight_up_pick) ||
      any(card$straight_up_pick != card$home & card$straight_up_pick != card$away)) {
    stop("The frozen card contains invalid forecasts or pick metadata.", call. = FALSE)
  }
  features <- publication_align(publication_read_csv(features_path), card, "Saved features")
  bundle <- readRDS(bundle_path)
  if (!is.list(bundle) || !all(c("model", "ats_model", "ats_threshold", "config", "as_of") %in%
                              names(bundle))) stop("The saved model bundle is incomplete.")
  frozen_time <- parse_utc_datetime(card$as_of)
  bundle_time <- parse_utc_datetime(bundle$as_of)
  if (length(bundle_time) != 1L || anyNA(c(frozen_time, bundle_time)) ||
      any(abs(as.numeric(frozen_time) - as.numeric(bundle_time)) >= 1)) {
    stop("The model bundle timestamp does not match the frozen prediction card.", call. = FALSE)
  }
  source_files <- c(predictions = predictions_path, bundle = bundle_path, features = features_path)
  source_hashes <- unname(tools::md5sum(source_files))

  # The response data alone is persisted; credentials remain in the environment.
  raw_response <- as.data.frame(fetch_lines(season, week), stringsAsFactors = FALSE)
  captured_at <- clock()
  quotes <- publication_fresh_quotes(raw_response, card, captured_at)
  card <- prepare_dashboard_predictions(card)
  picks <- reprice_publication_card(card, quotes$lines, bundle)
  picks$kickoff <- publication_utc(quotes$kickoff)
  picks$kickoff_display <- format(quotes$kickoff, "%a %m/%d %I:%M %p ET", tz = "America/New_York")
  hour <- as.integer(format(quotes$kickoff, "%H", tz = "America/New_York"))
  picks$primetime <- hour >= 19L | hour < 3L
  picks$schedule_source <- "https://api.collegefootballdata.com/lines"
  if ("neutral_site" %in% names(features)) picks$neutral_site <- features$neutral_site
  picks$line_source <- "DraftKings"
  picks$best_home_spread <- picks$market_home_spread
  picks$best_away_spread <- -picks$market_home_spread
  picks$publication_note <- paste0(
    "Model projections saved ", publication_utc(frozen_time[1]),
    "; DraftKings lines retrieved ", publication_utc(captured_at), "."
  )
  picks <- prepare_dashboard_predictions(picks)
  stopifnot(identical(picks$expected_margin, card$expected_margin),
            identical(picks$margin_sd, card$margin_sd),
            identical(picks$straight_up_pick, card$straight_up_pick))
  if (any(quotes$kickoff <= clock())) stop("A game has started; the publication was not written.")
  if (!identical(unname(tools::md5sum(source_files)), source_hashes)) {
    stop("A frozen source file changed during the refresh; the publication was not written.")
  }

  stamp <- format(captured_at, "%Y%m%dT%H%M%OS3Z", tz = "UTC")
  output <- file.path(project_dir, "cfb_v3", "output", as.character(season),
                       paste0("week_", week, "_publication"), stamp)
  if (dir.exists(output)) stop("Publication directory already exists: ", output)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(x, name) utils::write.csv(x, file.path(output, name), row.names = FALSE, na = "")
  write_csv(picks, "predictions.csv")
  write_csv(quotes$lines, "market_snapshot.csv")
  saveRDS(raw_response, file.path(output, "cfbd_lines_raw.rds"))
  changed_side <- ifelse(is.na(picks$ats_pick), "", picks$ats_pick) !=
    ifelse(is.na(card$ats_pick), "", card$ats_pick)
  changes <- data.frame(
    game_id = picks$game_id, away = picks$away, home = picks$home,
    old_home_spread = card$market_home_spread, new_home_spread = picks$market_home_spread,
    home_line_change = picks$market_home_spread - card$market_home_spread,
    old_ats_pick = card$ats_pick_line, new_ats_pick = picks$ats_pick_line,
    side_changed = changed_side, expected_margin = picks$expected_margin,
    point_gap = picks$pick_edge, captured_at = picks$market_captured_at,
    stringsAsFactors = FALSE
  )
  write_csv(changes, "line_changes.csv")
  feature_chart <- file.path(dirname(predictions_path), "feature_importance.png")
  if (file.exists(feature_chart)) file.copy(feature_chart, output)
  manifest <- list(
    status = "rendering", built_at = publication_utc(clock()), captured_at = publication_utc(captured_at),
    season = season, week = week, predictions = nrow(picks),
    evening_games = sum(picks$primetime), model_refit = FALSE,
    frozen_model_as_of = publication_utc(frozen_time[1]),
    quote_note = "Retrieval time, not provider last-update time. Spread juice is unavailable.",
    primetime_definition = "Kickoffs from 7 p.m. through 2:59 a.m. Eastern, including late-night games.",
    changed_spreads = sum(changes$home_line_change != 0, na.rm = TRUE),
    changed_sides = sum(changed_side),
    sources = lapply(seq_along(source_files), function(i) list(
      role = names(source_files)[i], path = source_files[[i]], md5 = source_hashes[[i]]))
  )
  manifest_path <- file.path(output, "publication_manifest.json")
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  dashboard <- render(file.path(output, "predictions.csv"), project_dir, output)
  manifest$status <- "complete"
  manifest$dashboard <- basename(dashboard)
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  cat("Publication:", normalizePath(output, winslash = "/"), "\n")
  cat("Games:", nrow(picks), "Changed spreads:", manifest$changed_spreads,
       "Changed ATS sides:", manifest$changed_sides, "\n")
  cat("Dashboard:", dashboard, "\n")
  invisible(list(output = output, dashboard = dashboard, predictions = picks, manifest = manifest))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--help")) {
    cat("Usage: Rscript cfb_v3/refresh_publication.R --predictions=PATH [--bundle=PATH] [--features=PATH]\n")
    quit(status = 0)
  }
  options <- list(predictions = NULL, bundle = NULL, features = NULL)
  for (arg in args) {
    key <- sub("=.*$", "", sub("^--", "", arg))
    if (!grepl("^--[^=]+=.+$", arg) || !key %in% names(options)) {
      stop("Unknown or invalid argument: ", arg, call. = FALSE)
    }
    options[[key]] <- sub("^[^=]+=", "", arg)
  }
  if (is.null(options$predictions)) stop("An explicit --predictions=PATH is required.")
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  project_dir <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
  for (file in c("config.R", "store.R", "features.R", "adapters.R", "models.R", "dashboard.R")) {
    source(file.path(project_dir, "cfb_v2", file))
  }
  source(file.path(project_dir, "cfb_v3", "publication.R"))
  if (!nzchar(Sys.getenv("CFBD_API_KEY"))) stop("CFBD_API_KEY must be set in the environment.")
  if (!requireNamespace("cfbfastR", quietly = TRUE)) stop("Package cfbfastR is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package jsonlite is required.")
  quarto_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin")
  if (!nzchar(Sys.which("quarto")) && dir.exists(quarto_dir)) {
    Sys.setenv(PATH = paste(quarto_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  }
  if (!nzchar(Sys.which("quarto"))) stop("Quarto is required to render the publication.")
  refresh_saved_publication(options$predictions, project_dir, options$bundle, options$features)
}
