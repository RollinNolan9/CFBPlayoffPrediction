dashboard_add_column <- function(data, name, value) {
  if (!name %in% names(data)) {
    data[[name]] <- if (length(value) == 1L) rep(value, nrow(data)) else value
  }
  data
}

dashboard_numeric <- function(x) suppressWarnings(as.numeric(x))

dashboard_line_text <- function(home, away, home_spread) {
  spread <- dashboard_numeric(home_spread)
  ifelse(
    !is.finite(spread), "No line",
    ifelse(
      spread <= 0,
      paste0(home, " ", sprintf("%.1f", spread)),
      paste0(away, " ", sprintf("%.1f", -spread))
    )
  )
}

dashboard_model_line_text <- function(home, away, margin) {
  margin <- dashboard_numeric(margin)
  ifelse(
    !is.finite(margin), "Unavailable",
    paste0(ifelse(margin >= 0, home, away), " -", sprintf("%.1f", abs(margin)))
  )
}

dashboard_pick_text <- function(pick, home, market_home_spread) {
  spread <- dashboard_numeric(market_home_spread)
  pick <- as.character(pick)
  value <- ifelse(pick == home, spread, -spread)
  ifelse(
    !nzchar(pick) | is.na(pick) | !is.finite(value), "Pass",
    paste0(pick, " ", ifelse(value > 0, "+", ""), sprintf("%.1f", value))
  )
}

prepare_dashboard_predictions <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("game_id", "home", "away", "expected_margin")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Dashboard predictions are missing columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  defaults <- list(
    season = NA_integer_, week = NA_integer_, market_home_spread = NA_real_,
    home_cover_probability = NA_real_, ats_pick = NA_character_,
    pick_status = "pass", confidence_tier = "low", reported_confidence = NA_character_,
    data_flag = "standard", injury_scenario = "most_likely", top_drivers = "",
    straight_up_pick = NA_character_,
    market_provider = "", line_source = "", market_captured_at = "", as_of = "",
    foundation_expected_margin = NA_real_, returning_expected_margin = NA_real_,
    preseason_expected_margin = NA_real_, home_coach = "", away_coach = "",
    home_coach_history = TRUE, away_coach_history = TRUE,
    preseason_profile = "", snapshot_type = "", model_version = "",
    fbs_transition = "", transition_game = FALSE
  )
  for (name in names(defaults)) data <- dashboard_add_column(data, name, defaults[[name]])

  text_defaults <- defaults[vapply(defaults, is.character, logical(1))]
  for (name in names(text_defaults)) {
    data[[name]] <- as.character(data[[name]])
    if (!is.na(text_defaults[[name]])) {
      data[[name]][is.na(data[[name]])] <- text_defaults[[name]]
    }
  }

  numeric_columns <- c(
    "expected_margin", "market_home_spread", "home_cover_probability",
    "foundation_expected_margin", "returning_expected_margin",
    "preseason_expected_margin"
  )
  for (name in numeric_columns) data[[name]] <- dashboard_numeric(data[[name]])

  home_edge <- data$expected_margin + data$market_home_spread
  missing_pick <- is.na(data$ats_pick) | !nzchar(trimws(data$ats_pick))
  can_pick <- missing_pick & is.finite(home_edge)
  data$ats_pick[can_pick] <- ifelse(
    home_edge[can_pick] >= 0, data$home[can_pick], data$away[can_pick]
  )
  data$pick_edge <- ifelse(
    data$ats_pick == data$home, home_edge,
    ifelse(data$ats_pick == data$away, -home_edge, NA_real_)
  )
  data$pick_cover_probability <- ifelse(
    data$ats_pick == data$home, data$home_cover_probability,
    ifelse(data$ats_pick == data$away, 1 - data$home_cover_probability, NA_real_)
  )
  missing_su <- is.na(data$straight_up_pick) |
    !nzchar(trimws(data$straight_up_pick))
  data$straight_up_pick[missing_su] <- ifelse(
    data$expected_margin[missing_su] >= 0,
    data$home[missing_su], data$away[missing_su]
  )

  data$game_label <- paste(data$away, "at", data$home)
  data$market_line <- dashboard_line_text(
    data$home, data$away, data$market_home_spread
  )
  data$model_line <- dashboard_model_line_text(
    data$home, data$away, data$expected_margin
  )
  data$ats_pick_line <- dashboard_pick_text(
    data$ats_pick, data$home, data$market_home_spread
  )
  data$foundation_model_line <- dashboard_model_line_text(
    data$home, data$away, data$foundation_expected_margin
  )
  data$returning_model_line <- dashboard_model_line_text(
    data$home, data$away, data$returning_expected_margin
  )
  data$preseason_model_line <- dashboard_model_line_text(
    data$home, data$away, data$preseason_expected_margin
  )

  data$reported_confidence[is.na(data$reported_confidence) |
                             !nzchar(data$reported_confidence)] <-
    data$confidence_tier[is.na(data$reported_confidence) |
                           !nzchar(data$reported_confidence)]
  status <- tolower(trimws(as.character(data$pick_status)))
  data$status_group <- ifelse(
    status == "official_pick", "official",
    ifelse(
      status == "forced_model_pick", "forced",
      ifelse(grepl("review", status), "review", "pass")
    )
  )
  data$status_label <- ifelse(
    status == "official_pick", "Official",
    ifelse(
      status == "forced_model_pick", "Forced side",
      ifelse(
        status == "large_spread_review", "Large spread",
        ifelse(
          status == "injury_conflict_review", "Injury review",
          ifelse(status == "transition_review", "Transition review", "Pass")
        )
      )
    )
  )

  scenario <- tolower(trimws(as.character(data$injury_scenario)))
  data$injury_flag <- grepl("injury", status) |
    (!is.na(scenario) & nzchar(scenario) &
       !scenario %in% c("most_likely", "none", "no_injuries", "no injuries"))
  data$provisional_flag <- tolower(data$reported_confidence) == "provisional" |
    (!is.na(data$data_flag) & nzchar(data$data_flag) & data$data_flag != "standard")
  data$market_provider_display <- ifelse(
    nzchar(trimws(data$market_provider)), data$market_provider,
    ifelse(nzchar(trimws(data$line_source)), data$line_source, "Market")
  )
  data$snapshot_time <- ifelse(
    nzchar(trimws(as.character(data$market_captured_at))),
    as.character(data$market_captured_at),
    as.character(data$as_of)
  )
  data$row_order <- seq_len(nrow(data))
  data
}

find_latest_prediction_csv <- function(output_root) {
  files <- list.files(
    output_root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE
  )
  files <- files[grepl("predictions|article_|live_", basename(files), ignore.case = TRUE)]
  if (!length(files)) {
    stop("No prediction CSV was found beneath ", output_root, call. = FALSE)
  }
  files[which.max(file.info(files)$mtime)]
}

render_cfb_dashboard <- function(predictions_csv, project_dir,
                                 output_dir = dirname(predictions_csv),
                                 output_file = "dashboard.html") {
  predictions_csv <- normalizePath(
    predictions_csv, winslash = "/", mustWork = TRUE
  )
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(
    output_dir, winslash = "/", mustWork = FALSE
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto is required to render the dashboard.", call. = FALSE)
  qmd <- file.path(project_dir, "cfb_v2", "dashboard", "dashboard.qmd")
  if (!file.exists(qmd)) stop("Dashboard source is missing: ", qmd, call. = FALSE)
  output_file <- basename(output_file)

  source_dir <- dirname(qmd)
  render_dir <- file.path(source_dir, paste0(".render-", Sys.getpid()))
  if (dir.exists(render_dir)) {
    stop("Dashboard staging directory already exists: ", render_dir, call. = FALSE)
  }
  if (!identical(
    normalizePath(dirname(render_dir), winslash = "/", mustWork = TRUE),
    normalizePath(source_dir, winslash = "/", mustWork = TRUE)
  )) {
    stop("Dashboard staging directory is outside the source directory.", call. = FALSE)
  }
  dir.create(render_dir, recursive = FALSE, showWarnings = FALSE)
  assets <- c("dashboard.qmd", "dashboard.css", "dashboard-after-body.html")
  copied <- file.copy(file.path(source_dir, assets), render_dir, overwrite = TRUE)
  if (!all(copied)) {
    stop("Dashboard render assets could not be staged.", call. = FALSE)
  }
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(render_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  old <- Sys.getenv(
    c("CFB_DASHBOARD_PROJECT_DIR", "CFB_DASHBOARD_PREDICTIONS"),
    unset = NA_character_
  )
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(old[[name]]), name))
    }
  }, add = TRUE)
  Sys.setenv(
    CFB_DASHBOARD_PROJECT_DIR = project_dir,
    CFB_DASHBOARD_PREDICTIONS = predictions_csv
  )
  setwd(render_dir)
  args <- c("render", "dashboard.qmd", "--output", shQuote(output_file))
  log_path <- file.path(render_dir, "quarto-render.log")
  status <- suppressWarnings(
    system2(quarto, args, stdout = log_path, stderr = log_path)
  )
  setwd(old_wd)
  log <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()
  if (!identical(status, 0L)) {
    log <- unlist(strsplit(log, "\r", fixed = TRUE), use.names = FALSE)
    log <- log[nzchar(trimws(log))]
    stop(
      "Dashboard render failed:\n",
      paste(tail(log, 40L), collapse = "\n"),
      call. = FALSE
    )
  }
  rendered_path <- file.path(render_dir, output_file)
  path <- file.path(output_dir, output_file)
  if (!file.exists(rendered_path) || file.info(rendered_path)$size <= 0) {
    stop("Dashboard render did not create ", rendered_path, call. = FALSE)
  }
  if (!file.copy(rendered_path, path, overwrite = TRUE)) {
    stop("Dashboard render could not write ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
