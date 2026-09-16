v1_extract_workspace <- function(project_dir, directory) {
  path <- file.path(directory, "workspace_inputs.rds")
  if (file.exists(path)) return(readRDS(path))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  e <- new.env(parent = baseenv())
  message("Reading the original R workspace without executing its model/functions")
  load(file.path(project_dir, ".RData"), envir = e)
  sources <- c("cfb_pbp19_22_train", "cfb_pbp23_test", "cfb_prediction2024")
  tables <- lapply(sources, function(name) {
    x <- as.data.frame(e[[name]])
    assert_columns(x, c("game_id", "year", "week", "home", "away", "pos_team",
      "def_pos_team", "pos_team_score", "def_pos_team_score", "EPA", "wpa", "play_type"), name)
    x <- x[is.finite(x$year) & x$year <= 2025, ]
    for (column in c("home", "away", "pos_team", "def_pos_team")) x[[column]] <- canonical_team(x[[column]])
    x$game_id <- as.character(x$game_id)
    games <- x[!duplicated(x$game_id, fromLast = TRUE), ]
    games$margin <- ifelse(games$home == games$pos_team,
      games$pos_team_score-games$def_pos_team_score,
      games$def_pos_team_score-games$pos_team_score)
    keep <- intersect(c("game_id", "year", "week", "home", "away", "margin",
      "start_date", "home_team_pregame_elo", "away_team_pregame_elo"), names(games))
    games <- games[keep]
    stats <- dplyr::summarise(dplyr::group_by(x[is.finite(x$EPA), ], game_id, year, week, pos_team),
      epa_play = mean(EPA, na.rm = TRUE),
      epa_pass = mean(EPA[play_type %in% c("Pass", "Pass Incompletion", "Pass Reception", "Passing Touchdown")], na.rm = TRUE),
      epa_rush = mean(EPA[play_type %in% c("Rush", "Rushing Touchdown")], na.rm = TRUE),
      wpa_play = mean(wpa, na.rm = TRUE), .groups = "drop")
    list(games = games, stats = as.data.frame(stats))
  })
  games <- as.data.frame(dplyr::bind_rows(lapply(tables, `[[`, "games")))
  stats <- as.data.frame(dplyr::bind_rows(lapply(tables, `[[`, "stats")))
  games <- games[!duplicated(games$game_id), ]
  stats <- stats[!duplicated(stats[c("game_id", "pos_team")]), ]
  sp <- as.data.frame(e$sp_ratings_all_years)
  sp$team <- canonical_team(sp$team)
  sp <- sp[sp$year <= 2025, c("team", "year", "rating", "offense_rating", "defense_rating")]
  assert_unique_keys(sp, c("team", "year"), "original SP ratings")
  out <- list(games = games, stats = stats, sp = sp,
    metadata = list(source = ".RData", source_md5 = unname(tools::md5sum(file.path(project_dir, ".RData"))),
      source_years = sort(unique(games$year)), original_workspace_window = e$window_size,
      original_workspace_model_columns = names(e$train_dat_clean)))
  saveRDS(out, path)
  out
}

v1_numeric_features <- function() {
  unlist(lapply(c("home", "away"), function(side) paste0(side, "_",
    c("epa_play", "epa_pass", "epa_rush", "wpa_play", "sp", "sp_offense", "sp_defense", "elo"))))
}

v1_build_features <- function(inputs, official, window = 5L) {
  raw <- inputs$games
  raw$season <- raw$year
  raw$kickoff <- parse_utc_datetime(raw$start_date)
  raw$home_elo <- raw$home_team_pregame_elo
  raw$away_elo <- raw$away_team_pregame_elo
  official$kickoff <- parse_utc_datetime(official$kickoff)
  official$home_elo <- official$home_pregame_elo
  official$away_elo <- official$away_pregame_elo
  columns <- c("game_id", "season", "week", "home", "away", "kickoff", "margin", "home_elo", "away_elo")
  games <- rbind(raw[raw$season < 2020, columns], official[official$season <= 2025, columns])
  games <- games[order(games$season, games$kickoff, games$game_id), ]
  rownames(games) <- NULL
  assert_unique_keys(games, "game_id", "legacy reconstruction games")
  games <- add_chronological_model_week(games)
  games$cutoff <- as.POSIXct(games$feature_week_start, tz = "UTC")
  stats <- inputs$stats
  stats$kickoff <- raw$kickoff[match(stats$game_id, raw$game_id)]
  official_index <- match(stats$game_id, official$game_id)
  hit <- !is.na(official_index)
  stats$kickoff[hit] <- official$kickoff[official_index[hit]]
  groups <- split(seq_len(nrow(stats)), paste(stats$year, stats$pos_team, sep = "|"))
  efficiency <- c("epa_play", "epa_pass", "epa_rush", "wpa_play")
  for (side in c("home", "away")) {
    values <- matrix(NA_real_, nrow(games), length(efficiency))
    counts <- integer(nrow(games))
    latest <- rep(NA_real_, nrow(games))
    for (i in seq_len(nrow(games))) {
      group <- groups[[paste(games$season[i], games[[side]][i], sep = "|")]]
      prior <- group[!is.na(stats$kickoff[group]) & stats$kickoff[group] < games$cutoff[i]]
      prior <- prior[order(stats$kickoff[prior], stats$game_id[prior])]
      counts[i] <- length(prior)
      if (length(prior) < window) next
      recent <- tail(prior, window)
      values[i, ] <- colMeans(stats[recent, efficiency], na.rm = TRUE)
      latest[i] <- max(as.numeric(stats$kickoff[recent]))
    }
    for (j in seq_along(efficiency)) games[[paste0(side, "_", efficiency[j])]] <- values[, j]
    games[[paste0(side, "_prior_games")]] <- counts
    games[[paste0(side, "_latest_source")]] <- latest
    sp_index <- match(paste(games[[side]], games$season-1L), paste(inputs$sp$team, inputs$sp$year))
    for (pair in list(c("sp", "rating"), c("sp_offense", "offense_rating"), c("sp_defense", "defense_rating"))) {
      games[[paste0(side, "_", pair[1])]] <- inputs$sp[[pair[2]]][sp_index]
    }
  }
  games$sp_source_season <- games$season-1L
  excluded <- games$home == "James Madison" | games$away == "James Madison"
  complete <- rowSums(!is.finite(as.matrix(games[v1_numeric_features()]))) == 0
  games$omission_reason <- ifelse(excluded, "original_team_exclusion",
    ifelse(games$home_prior_games < window | games$away_prior_games < window, "incomplete_five_game_window",
    ifelse(!complete, "missing_rating_or_efficiency", "eligible")))
  games$eligible <- games$omission_reason == "eligible" & is.finite(games$margin)
  for (side in c("home", "away")) {
    latest <- games[[paste0(side, "_latest_source")]]
    stopifnot(all(latest[is.finite(latest)] < as.numeric(games$cutoff[is.finite(latest)])))
  }
  games
}

v1_design <- function(train, test) {
  columns <- c("home", "away", v1_numeric_features())
  x <- train[columns]; y <- test[columns]
  known <- rep(TRUE, nrow(test))
  for (side in c("home", "away")) {
    levels <- sort(unique(x[[side]]))
    known <- known & y[[side]] %in% levels
    x[[side]] <- factor(x[[side]], levels = levels)
    y[[side]] <- factor(y[[side]], levels = levels)
  }
  make <- function(data) {
    out <- stats::model.matrix(~ ., data)
    out[, colnames(out) != "(Intercept)", drop = FALSE]
  }
  a <- make(x); b <- make(y[known, , drop = FALSE])
  stopifnot(identical(colnames(a), colnames(b)))
  list(train = a, test = b, known = known)
}

v1_fit_season <- function(features, seed, season, ntree = 500L, mtry = NULL) {
    train <- features[features$eligible & features$season < season, ]
    test <- features[features$eligible & features$season == season, ]
    design <- v1_design(train, test)
    message("V1 seed ", seed, ", test ", season, ": ", nrow(train), " training games, ", sum(design$known), " predictions")
    set.seed(seed + season)
    if (is.null(mtry)) {
      tuned <- caret::train(x = design$train, y = train$margin, method = "rf",
        trControl = caret::trainControl(method = "cv", number = 10, allowParallel = FALSE),
        tuneGrid = data.frame(mtry = c(2, 4, 6)), ntree = ntree, nodesize = 5)
      mtry <- tuned$bestTune$mtry
      fit <- tuned$finalModel
      scores <- tuned$results
      scores$test_season <- season
    } else {
      fit <- randomForest::randomForest(x = design$train, y = train$margin,
        ntree = ntree, mtry = mtry, nodesize = 5)
      scores <- NULL
    }
    predictions <- data.frame(game_id = test$game_id[design$known],
      season = season, expected_margin = as.numeric(predict(fit, design$test)))
    choices <- data.frame(seed = seed, test_season = season,
      first_train_season = min(train$season), train_through_season = max(train$season),
      training_games = nrow(train), training_columns = ncol(design$train),
      test_predictions = sum(design$known), unseen_team_side = sum(!design$known),
      trees = ntree, mtry = mtry, nodesize = 5)
    list(predictions = predictions, choices = choices, cv_scores = scores,
      unseen_ids = test$game_id[!design$known])
}

v1_rolling <- function(features, seed, ntree = 500L, fixed_choices = NULL, cluster = NULL) {
  run <- function(season, features, seed, ntree, fixed_choices) {
    mtry <- if (is.null(fixed_choices)) NULL else fixed_choices$mtry[fixed_choices$test_season == season]
    v1_fit_season(features, seed, season, ntree, mtry)
  }
  fits <- if (is.null(cluster)) lapply(2022:2025, run, features, seed, ntree, fixed_choices) else
    parallel::parLapply(cluster, 2022:2025, run, features, seed, ntree, fixed_choices)
  list(predictions = do.call(rbind, lapply(fits, `[[`, "predictions")),
    choices = do.call(rbind, lapply(fits, `[[`, "choices")),
    cv_scores = do.call(rbind, lapply(fits, `[[`, "cv_scores")),
    unseen_ids = unlist(lapply(fits, `[[`, "unseen_ids"), use.names = FALSE))
}

v1_paired <- function(predictions) {
  reference <- predictions[predictions$model == "v3_control", ]
  do.call(rbind, lapply(setdiff(unique(predictions$model), "v3_control"), function(model) {
    candidate <- predictions[predictions$model == model, ]
    reference <- reference[match(candidate$game_id, reference$game_id), ]
    stopifnot(identical(candidate$game_id, reference$game_id))
    mae <- candidate$mae_error-reference$mae_error
    ats <- candidate$ats_correct-reference$ats_correct
    ci <- experiment_season_interval(mae, candidate$season)
    ats_ci <- experiment_season_interval(ats, candidate$season)
    data.frame(candidate = model, reference = "v3_control", games = nrow(candidate),
      mae_change = mean(mae), mae_low = ci[1], mae_high = ci[2],
      ats_change = mean(ats, na.rm = TRUE), ats_low = ats_ci[1], ats_high = ats_ci[2])
  }))
}

run_v1_comparison <- function(config) {
  protected <- experiment_file_hashes(config)
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  inputs <- v1_extract_workspace(config$project_dir, file.path(config$output_dir, "experiments", "v1_inputs"))
  official <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  reference <- read_csv_if_present(file.path(config$output_dir, "backtest", "rolling_predictions.csv"), TRUE)
  reference <- reference[reference$model == "ridge_core" & raw_history_eligible(reference), ]
  reference$model <- "v3_control"
  assert_unique_keys(reference, "game_id", "v3 reference")
  output <- file.path(config$output_dir, "experiments", "v1_comparison", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (dir.exists(output)) stop("Never overwrite a comparison run.")
  dir.create(output, recursive = TRUE)
  file.copy(file.path(root, "V1_PROTOCOL.md"), file.path(output, "protocol.md"))
  status <- system2("git", c("show", "365ca12:model.Rmd"), stdout = file.path(output, "original_model.Rmd"))
  stopifnot(status == 0L)
  original_source <- readLines(file.path(output, "original_model.Rmd"))
  stopifnot(any(grepl("mtry=c(2, 4, 6)", original_source, fixed = TRUE)),
    any(grepl("window_size <- 5", original_source, fixed = TRUE)))
  manifest_files <- c(file.path(root, c("V1_PROTOCOL.md", "v1_comparison.R", "controlled_ats.R")),
    file.path(config$project_dir, "run_cfb_v1_comparison.R"), file.path(output, "original_model.Rmd"),
    file.path(config$output_dir, "experiments", "v1_inputs", "workspace_inputs.rds"))
  jsonlite::write_json(list(original_commit = "365ca12", original_file = "model.Rmd",
    seeds = 20260909:20260911, window = 5, inputs = inputs$metadata,
    file_md5 = as.list(tools::md5sum(manifest_files))), file.path(output, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  write_foundation_csv(protected, file.path(output, "protected_hashes_before.csv"))
  features <- v1_build_features(inputs, official)
  write_foundation_csv(features, file.path(output, "reconstructed_features.csv"))
  cluster <- parallel::makePSOCKcluster(4L, outfile = file.path(output, "fit_log.txt"))
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterExport(cluster, c("v1_fit_season", "v1_design", "v1_numeric_features"), envir = environment())
  message("Running the four primary outer folds with the original 10-fold training CV")
  primary <- v1_rolling(features, 20260909L, cluster = cluster)
  write_foundation_csv(primary$cv_scores, file.path(output, "training_cv_scores.csv"))
  message("Refitting the same selected settings under two sensitivity seeds")
  fits <- c(list(primary), lapply(20260910:20260911, function(seed)
    v1_rolling(features, seed, fixed_choices = primary$choices, cluster = cluster)))
  ids <- intersect(reference$game_id, fits[[1]]$predictions$game_id)
  stopifnot(length(ids) > 0L)
  baseline <- reference[match(ids, reference$game_id), ]
  candidates <- lapply(seq_along(fits), function(i) {
    x <- baseline
    p <- fits[[i]]$predictions
    index <- match(ids, p$game_id)
    stopifnot(!anyNA(index))
    x$model <- paste0("v1_seed_", 20260908L+i)
    x$expected_margin <- p$expected_margin[index]
    x$margin_sd <- NA_real_
    x
  })
  predictions <- experiment_grade(do.call(rbind, c(list(baseline), candidates)))
  metrics <- experiment_metrics(predictions)
  paired <- v1_paired(predictions)
  coverage <- reference[c("game_id", "season", "week", "game_phase", "home", "away", "is_cfp", "closing_home_spread")]
  coverage$reason <- features$omission_reason[match(coverage$game_id, features$game_id)]
  coverage$reason[coverage$game_id %in% fits[[1]]$unseen_ids] <- "unseen_team_side"
  coverage$scored <- coverage$game_id %in% ids
  stopifnot(all(!is.na(coverage$reason)), all(coverage$scored == (coverage$reason == "eligible")))
  summary <- aggregate(game_id ~ season + reason, coverage, length)
  names(summary)[3] <- "games"
  full_metrics <- experiment_metrics(experiment_grade(reference))
  choices <- do.call(rbind, lapply(fits, `[[`, "choices"))
  write_foundation_csv(predictions, file.path(output, "experiment_predictions.csv"))
  write_foundation_csv(metrics, file.path(output, "metrics.csv"))
  write_foundation_csv(paired, file.path(output, "paired_comparisons.csv"))
  write_foundation_csv(coverage, file.path(output, "coverage_games.csv"))
  write_foundation_csv(summary, file.path(output, "coverage_summary.csv"))
  write_foundation_csv(full_metrics, file.path(output, "full_v3_metrics.csv"))
  write_foundation_csv(choices, file.path(output, "fold_choices.csv"))
  report <- file.path(output, "report.md")
  show <- function(x) {
    x <- x[c("model", "slice", "games", "wins", "losses", "pushes", "abstentions", "ats_accuracy", "margin_mae", "su_accuracy")]
    x$ats_accuracy <- 100*x$ats_accuracy; x$su_accuracy <- 100*x$su_accuracy
    names(x)[names(x) == "ats_accuracy"] <- "ats_pct"
    names(x)[names(x) == "su_accuracy"] <- "su_pct"
    markdown_table(x)
  }
  writeLines(c("# V1 Versus V3", "", "See protocol.md for the locked design and reconstruction limitations.", "",
    "## Matched Games", "", show(metrics[metrics$slice == "all_fbs", ]), "",
    "The primary v1 seed is 20260909. Other seeds are fixed sensitivity checks, not competing strategies.", "",
    "## Full V3 Coverage", "", show(full_metrics[full_metrics$slice == "all_fbs", ]), "",
    "Do not compare full-v3 metrics directly to the smaller late-season v1 sample.", "",
    "## Coverage", "", markdown_table(summary), "",
    "## Paired Differences", "", markdown_table(paired), "",
    "Negative MAE changes favor v1; positive ATS changes favor v1. Intervals resample whole seasons.", "",
    "## Season And Diagnostic Slices", "", show(metrics[metrics$slice != "all_fbs", ]), "",
    "## Fold Settings", "", markdown_table(choices), "",
    "V1 uses prior-season SP, pregame Elo, and strictly prior-week rolling EPA/WPA. It cannot",
    "cover games without five previous same-season observations. Missing 2013 SP can remove",
    "2014 from complete-case training. The saved workspace is a later source-data snapshot,",
    "not a verified original release archive. The reconstruction is not the old leaked test score.", "",
    "V3 already excludes non-CFP bowls from training and main form history. V1 retains its",
    "original bowl handling; no separate bowl ablation was run. No model was promoted.", ""), report)
  after <- experiment_file_hashes(config)
  stopifnot(identical(protected, after))
  write_foundation_csv(after, file.path(output, "protected_hashes_after.csv"))
  jsonlite::write_json(list(status = "complete", protected_files_unchanged = TRUE,
    common_games = length(ids), primary_seed = 20260909, no_promotion = TRUE),
    file.path(output, "verification.json"), auto_unbox = TRUE, pretty = TRUE)
  list(report = report, metrics = metrics[metrics$slice == "all_fbs", ], paired = paired, coverage = summary)
}
