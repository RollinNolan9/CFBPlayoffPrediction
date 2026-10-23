# Export frozen historical controls for INJURY_PILOT_PROTOCOL.md. No fitting,
# network requests, outcome evaluation, database writes, or production changes.
# From the repository root:
# Rscript --vanilla cfb_v3/experiments/injury_baseline.R
injury_export_baseline <- function(project_dir = getwd()) {
  root <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  path <- function(relative) file.path(root, relative)
  engine <- new.env(parent = globalenv())
  for (module in c("config.R", "store.R", "features.R", "models.R")) {
    sys.source(path(file.path("cfb_v2", module)), envir = engine)
  }
  config <- engine$cfb_v2_config(root, season = 2025L)
  read_csv <- function(relative) read.csv(path(relative), stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = c(game_id = "character"))
  fail <- function(condition, message) {
    if (!isTRUE(condition)) stop(message, call. = FALSE)
  }
  predictions_file <- "cfb_v3/output/backtest/rolling_predictions.csv"
  training_file <- "cfb_v3/inbox/training_games.csv"
  historical_file <- "cfb_v3/data/historical_games.csv"
  report_file <- "cfb_v3/output/backtest/backtest_report.md"
  protocol_file <- "cfb_v3/experiments/INJURY_PILOT_PROTOCOL.md"
  # These identify the audited snapshot, not whichever future backtest happens
  # to occupy the same filename. Replacing it requires an explicit new audit.
  fail(unname(tools::md5sum(path(predictions_file))) ==
         "a3c658b295bd492968ea68cdfa1b90d2", "Frozen rolling-prediction hash changed.")
  training_md5 <- unname(tools::md5sum(path(training_file)))
  fail(training_md5 == "fc60746cc14385dad62a3552137d6585",
       "Frozen training-file hash changed.")
  fail(any(grepl(training_md5, readLines(path(report_file), warn = FALSE), fixed = TRUE)),
       "Training hash does not match the original backtest report.")

  predictions <- read_csv(predictions_file)
  training <- read_csv(training_file)
  historical <- read_csv(historical_file)
  fail(!anyDuplicated(training$game_id) && !anyDuplicated(historical$game_id),
       "Training or historical game IDs are not unique.")
  core <- predictions[predictions$model == "ridge_core", , drop = FALSE]
  fail(nrow(core) == 3511L && !anyDuplicated(core$game_id),
       "Frozen core coverage changed or has duplicate IDs.")
  training_index <- match(core$game_id, training$game_id)
  historical_index <- match(core$game_id, historical$game_id)
  fail(!anyNA(training_index) && !anyNA(historical_index), "Missing game join.")
  training_core <- training[training_index, , drop = FALSE]
  historical_core <- historical[historical_index, , drop = FALSE]
  fail(identical(core$home, training_core$home) && identical(core$away, training_core$away) &&
         identical(core$home, historical_core$home) && identical(core$away, historical_core$away),
       "Joined game identities disagree.")
  fail(all(core$season == training_core$season) && all(core$season == historical_core$season) &&
         all(core$week == training_core$week) && all(core$week == historical_core$week),
       "Joined season/week fields disagree.")
  fail(all(core$actual_margin == training_core$margin) &&
         all(core$actual_margin == historical_core$margin), "Actual-margin sources disagree.")
  fail(identical(training_core$kickoff, historical_core$kickoff),
       "Training and historical kickoffs disagree.")

  component <- function(name) {
    rows <- predictions[predictions$model == name, , drop = FALSE]
    fail(nrow(rows) == nrow(core) && !anyDuplicated(rows$game_id),
         paste("Invalid component coverage:", name))
    index <- match(core$game_id, rows$game_id)
    fail(!anyNA(index), paste("Missing component game:", name))
    rows <- rows[index, , drop = FALSE]
    fail(all(rows$actual_margin == core$actual_margin), "Component outcomes disagree.")
    rows$expected_margin
  }
  foundation <- component("preseason_ablation_challenger")
  preseason <- component("preseason_full_challenger")
  share <- engine$preseason_blend_share_for_data(training_core, config)
  reconstructed <- (1 - share) * foundation + share * preseason
  blend_error <- max(abs(reconstructed - core$expected_margin))
  fail(is.finite(blend_error) && blend_error == 0, "Exact saved-v3 blend identity failed.")
  fail(all(share %in% c(0, 1)), "Unexpected saved-v3 component shares.")

  eligible <- core$season %in% 2024:2025 & core$home_conference == "SEC" &
    core$away_conference == "SEC" & core$postseason_type == "regular" &
    core$home_level == "fbs" & core$away_level == "fbs"
  indices <- which(eligible)
  indices <- indices[order(core$season[indices], core$week[indices], core$game_id[indices])]
  selected <- core[indices, , drop = FALSE]
  fail(nrow(selected) == 128L && all(table(selected$season) == 64L),
       "Expected 64 regular-season conference games in each season.")
  kickoff <- as.POSIXct(historical_core$kickoff[indices],
    format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  fail(!anyNA(kickoff), "Missing/invalid kickoff.")
  pacific_date <- as.Date(format(kickoff, "%Y-%m-%d", tz = "America/Los_Angeles"))
  eastern_date <- as.Date(format(kickoff, "%Y-%m-%d", tz = "America/New_York"))
  fail(all(pacific_date == eastern_date), "US date ambiguity requires manual review.")
  weekday <- as.integer(format(kickoff, "%w", tz = "America/Los_Angeles"))
  saturday <- weekday == 6L
  friday_date <- pacific_date - ((weekday + 2L) %% 7L)
  cutoff <- as.POSIXct(paste(friday_date, "12:00:00"), tz = "America/Los_Angeles")
  fail(all(as.numeric(cutoff[saturday]) < as.numeric(kickoff[saturday])),
       "Publication cutoff is not before a target kickoff.")
  # Friday games are outside the protocol, including one before noon Pacific.
  # Do not assign these exclusions a potentially post-kickoff prediction cutoff.
  cutoff[!saturday] <- NA
  fail(sum(saturday) == 125L && sum(!saturday) == 3L,
       "Unexpected Saturday/non-Saturday coverage.")

  source_files <- c(predictions_file, training_file, historical_file, report_file,
    protocol_file, "cfb_v2/config.R", "cfb_v2/models.R", "cfb_v2/features.R",
    "cfb_v2/store.R", "cfb_v2/workflow.R", "cfb_v2/historical_data.R",
    "cfb_v3/experiments/injury_baseline.R")
  line_checks <- list()
  for (season in 2024:2025) {
    rows <- which(selected$season == season)
    schedule_file <- sprintf("cfb_v2/cache/schedules/cfbd_schedule_%d.rds", season)
    pbp_file <- sprintf("cfb_v2/cache/pbp/pbp_%d_compact.rds", season)
    source_files <- c(source_files, schedule_file, pbp_file)
    schedule <- readRDS(path(schedule_file))
    fail(!anyDuplicated(schedule$game_id), "Raw schedule has duplicate IDs.")
    si <- match(selected$game_id[rows], as.character(schedule$game_id))
    fail(!anyNA(si), "Target game absent from raw schedule.")
    schedule <- schedule[si, , drop = FALSE]
    fail(all(schedule$conference_game) && all(schedule$completed) &&
           all(schedule$season_type == "regular"), "Raw conference/completion status mismatch.")
    fail(all(engine$canonical_team(schedule$home_team) == selected$home[rows]) &&
           all(engine$canonical_team(schedule$away_team) == selected$away[rows]),
         "Raw schedule team mismatch.")
    raw_kickoff <- as.POSIXct(schedule$start_date, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    fail(all(as.numeric(raw_kickoff) == as.numeric(kickoff[rows])), "Raw schedule kickoff mismatch.")
    fail(all(schedule$home_points - schedule$away_points == selected$actual_margin[rows]),
         "Raw schedule score mismatch.")
    pbp <- readRDS(path(pbp_file))
    pbp <- pbp[as.character(pbp$game_id) %in% selected$game_id[rows], c("game_id", "spread")]
    raw_lines <- vapply(selected$game_id[rows], function(id) {
      values <- unique(pbp$spread[as.character(pbp$game_id) == id & is.finite(pbp$spread)])
      fail(length(values) == 1L, paste("Missing or varying PBP spread for", id))
      as.numeric(values[1])
    }, numeric(1))
    error <- max(abs(raw_lines - selected$closing_home_spread[rows]))
    fail(is.finite(error) && error == 0, "PBP reference spread mismatch.")
    line_checks[[as.character(season)]] <- list(games = length(rows), max_absolute_error = error)
  }
  fail(all(selected$line_source == "cfbfastR_pbp_embedded") &&
         all(is.finite(selected$closing_home_spread)) &&
         all(is.finite(selected$expected_margin)), "Unexpected/missing baseline or line source.")

  # Whitelist collector fields. No predictions, outcomes, odds, or derived
  # performance fields may enter schedule.csv or the exclusion ledger.
  schedule <- data.frame(game_id = selected$game_id, season = selected$season,
    week = selected$week, kickoff_utc = format(kickoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    home = selected$home, away = selected$away,
    game_date_pacific = as.character(pacific_date),
    prediction_cutoff_utc = format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    cutoff_timezone = "America/Los_Angeles", cutoff_local_time = "Friday 12:00:00",
    stringsAsFactors = FALSE)
  baseline <- schedule
  baseline$baseline_expected_margin <- selected$expected_margin
  baseline$actual_margin <- selected$actual_margin
  baseline$reference_home_spread <- selected$closing_home_spread
  baseline$line_source <- selected$line_source
  baseline$baseline_model_label <- "saved_chronological_v3_phase_blend"
  baseline$preseason_component_share <- share[indices]
  baseline$foundation_expected_margin <- foundation[indices]
  baseline$preseason_expected_margin <- preseason[indices]
  baseline$primary_saturday_eligible <- saturday
  exclusions <- schedule[!saturday, , drop = FALSE]
  exclusions$exclusion_reason <- "non_saturday_game"
  exclusions$weekday_number_sunday_zero <- weekday[!saturday]
  output_dir <- path("cfb_v3/output/experiments/injury_pilot/baseline")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv <- function(data, filename) write.csv(data, file.path(output_dir, filename),
    row.names = FALSE, na = "", fileEncoding = "UTF-8")
  write_csv(baseline, "baseline_all_games.csv")
  write_csv(baseline[saturday, , drop = FALSE], "baseline.csv")
  write_csv(schedule[saturday, , drop = FALSE], "schedule.csv")
  write_csv(exclusions, "exclusions.csv")
  identity <- data.frame(game_id = core$game_id, season = core$season, week = core$week,
    preseason_component_share = share, foundation_expected_margin = foundation,
    preseason_expected_margin = preseason, saved_expected_margin = core$expected_margin,
    reconstructed_expected_margin = reconstructed,
    difference = reconstructed - core$expected_margin, stringsAsFactors = FALSE)
  write_csv(identity, "blend_identity.csv")
  sha256 <- function(relative) digest::digest(file = path(relative), algo = "sha256")
  sources <- data.frame(path = source_files,
    sha256 = vapply(source_files, sha256, character(1)),
    bytes = as.numeric(file.info(path(source_files))$size), stringsAsFactors = FALSE)
  write_csv(sources, "source_hashes.csv")
  generated_files <- c("baseline_all_games.csv", "baseline.csv", "schedule.csv",
    "exclusions.csv", "blend_identity.csv", "source_hashes.csv")
  metadata <- list(
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    protocol = protocol_file,
    command = "Rscript --vanilla cfb_v3/experiments/injury_baseline.R",
    baseline_identity = "Saved chronological v3 phase blend; not contemporaneously issued live cards.",
    baseline_field = "expected_margin from model == ridge_core; NOT fair_margin",
    component_identity = "share * preseason_full_challenger + (1-share) * preseason_ablation_challenger",
    component_share = "1 through phase week 4 for regular season; 0 thereafter and in postseason",
    chronological_method = "Saved rolling folds train on strictly earlier seasons (cfb_v2/models.R). No refit in this export.",
    all_conference_games = 128L, primary_saturday_games = 125L,
    counts_by_season = data.frame(season = 2024:2025, all_conference_games = c(64L, 64L),
      primary_saturday_games = c(63L, 62L), non_saturday_exclusions = c(1L, 2L)),
    cutoff = "Friday noon America/Los_Angeles, converted with Olson timezone rules",
    line_provenance = "One finite spread per game in cfbfastR compact PBP, exactly matches saved closing_home_spread. Bookmaker and quote time unverified.",
    line_limit = "Reference-line diagnostic only; not a reconstructed Friday betting price or independently verified closing line.",
    absence_limit = "Baseline already reflects prior team performance/availability; it is no-new-adjustment, not an all-healthy counterfactual.",
    test_season_limit = "2025 outcomes were examined in previous project research; chronological test cohort, not a pristine holdout.",
    assertions = list(frozen_prediction_md5_matches = TRUE, training_md5_matches_report = TRUE,
      unique_game_joins = TRUE, exact_blend_rows = nrow(core), exact_blend_max_error = blend_error,
      schedules_conference_regular_completed = TRUE, raw_schedule_identities_kickoffs_scores_match = TRUE,
      actual_margin_matches_training_and_historical = TRUE, all_primary_cutoffs_before_kickoff = TRUE,
      pacific_and_eastern_game_dates_identical = TRUE, lines_match_raw_pbp = line_checks,
      collector_columns_whitelisted_without_outcomes_predictions_or_lines = TRUE),
    inputs = sources,
    output_sha256 = as.list(setNames(vapply(generated_files, function(filename)
      digest::digest(file = file.path(output_dir, filename), algo = "sha256"), character(1)), generated_files))
  )
  jsonlite::write_json(metadata, file.path(output_dir, "metadata.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = NA)
  cat("Exported", nrow(baseline), "conference games;", sum(saturday),
      "Saturday targets;", sum(!saturday), "exclusions. Exact blend:",
      nrow(core), "rows, max error", blend_error, "\n")
  invisible(metadata)
}

if (sys.nframe() == 0L) {
  Sys.unsetenv("LC_ALL")
  injury_export_baseline()
}
