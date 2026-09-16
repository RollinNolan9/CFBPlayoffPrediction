model1_source <- function(project_dir) {
  source <- system2("git", c("-C", shQuote(project_dir), "show", shQuote("365ca12:model (1).Rmd")), stdout = TRUE)
  stopifnot(is.null(attr(source, "status")), any(grepl("window_size <- 6", source, fixed = TRUE)),
    any(grepl("mtry=c(6)", source, fixed = TRUE)),
    any(grepl("load_cfb_pbp(2014:2021)", source, fixed = TRUE)),
    any(grepl("train_dat <- season_level_data_train[, c(6:7, 26:33, 36:44)]", source, fixed = TRUE)))
  inside <- FALSE
  code <- character()
  for (line in source) {
    if (grepl("^```\\{r", line)) { inside <- TRUE; next }
    if (grepl("^```", line)) { inside <- FALSE; next }
    if (inside) code <- c(code, line)
  }
  list(lines = source, expressions = parse(text = code))
}

model1_assignment <- function(expression) {
  if (!is.call(expression) || !identical(expression[[1]], as.name("<-"))) return("")
  left <- expression[[2]]
  if (is.symbol(left)) return(as.character(left))
  if (is.call(left) && identical(left[[1]], as.name("$"))) return(as.character(left[[2]]))
  ""
}

model1_archived_csv <- function(project_dir, name) {
  lines <- system2("git", c("-C", shQuote(project_dir), "show", shQuote(paste0("365ca12:", name))), stdout = TRUE)
  stopifnot(is.null(attr(lines, "status")))
  utils::read.csv(text = paste(lines, collapse = "\n"), stringsAsFactors = FALSE)
}

model1_archived_joins <- function(inputs, project_dir) {
  inputs$team_names <- data.frame(raw = unique(c(inputs$pbp$home, inputs$pbp$away)), stringsAsFactors = FALSE)
  inputs$team_names$canonical <- canonical_team(inputs$team_names$raw)
  inputs$pff <- model1_archived_csv(project_dir, "pff_team_data (2).csv")
  inputs$pff$RECORD <- NULL
  inputs$coaches <- model1_archived_csv(project_dir, "coach_ratings (1).csv")
  injury <- model1_archived_csv(project_dir, "final_for_sure.csv")
  inputs$injuries <- dplyr::summarise(dplyr::group_by(injury, team, season),
    injured_worth = sum(injured_worth, na.rm = TRUE), worth = sum(worth, na.rm = TRUE), .groups = "keep")
  inputs
}

model1_extract <- function(project_dir, directory) {
  path <- file.path(directory, "original_workflow_inputs.rds")
  if (file.exists(path)) return(readRDS(path))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  message("Extracting original workflow inputs from .RData")
  e <- new.env(parent = baseenv())
  load(file.path(project_dir, ".RData"), envir = e)
  columns <- c("game_id", "year", "week", "pos_team", "def_pos_team", "home", "away",
    "pos_team_score", "def_pos_team_score", "EPA", "wpa", "play_type", "start_date")
  parts <- lapply(c("cfb_pbp19_22_train", "cfb_pbp23_test", "cfb_prediction2024"), function(name) {
    x <- as.data.frame(e[[name]])
    assert_columns(x, columns, name)
    x[x$year %in% c(2014:2021, 2024), columns]
  })
  pbp <- do.call(rbind, parts)
  rownames(pbp) <- NULL
  out <- list(pbp = pbp, sp = as.data.frame(e$sp_ratings_all_years),
    elo = as.data.frame(e$elo_ratings_combined), pff = as.data.frame(e$pff_team_data),
    coaches = as.data.frame(e$coach_data), injuries = as.data.frame(e$injury_data),
    source_md5 = unname(tools::md5sum(file.path(project_dir, ".RData"))))
  saveRDS(out, path)
  out
}

model1_environment <- function(inputs, source) {
  e <- new.env(parent = .GlobalEnv)
  e$window_size <- 6L
  e$sp_ratings_all_years <- inputs$sp[inputs$sp$year <= 2024, ]
  e$elo_ratings_combined <- inputs$elo[inputs$elo$year <= 2024, ]
  e$pff_team_data <- inputs$pff
  e$coach_data <- inputs$coaches
  e$injury_data <- inputs$injuries
  functions <- c("aggregate_game_data", "clean_team_names", "predict_winner")
  for (expression in source$expressions) {
    if (model1_assignment(expression) %in% functions) eval(expression, envir = e)
  }
  e
}

model1_table <- function(pbp, inputs, source, phase) {
  stopifnot(phase %in% c("train", "predict"))
  e <- model1_environment(inputs, source)
  e[[if (phase == "train") "cfb_pbp19_22_train" else "cfb_prediction2024"]] <- pbp
  targets <- if (phase == "train") c("season_level_data_train", "train_dat", "train_dat_clean") else
    c("season_level_data_predict", "predict_dat", "predict_dat_clean")
  for (expression in source$expressions) {
    if (model1_assignment(expression) %in% targets) eval(expression, envir = e)
  }
  data <- e[[paste0(if (phase == "train") "train" else "predict", "_dat_clean")]]
  full <- e[[paste0("season_level_data_", phase)]]
  expected <- c("home", "away", paste0(rep(c("home", "away"), each = 4), "_team_",
    rep(c("epa_per_play_last_n", "epa_per_pass_last_n", "epa_per_rush_last_n", "wpa_per_play_last_n"), 2)),
    "home_away_score_dif", "home_team_sp_rating", "home_team_offense_rating", "home_team_defense_rating",
    "away_team_sp_rating", "away_team_offense_rating", "away_team_defense_rating",
    "home_team_elo_rating", "away_team_elo_rating")
  stopifnot(identical(setdiff(names(data), c("week", "year")), expected))
  list(data = data, full = full, environment = e)
}

model1_predict_matchup <- function(model, data, game, source, inputs) {
  e <- model1_environment(inputs, source)
  naming <- function(team) {
    map <- inputs$team_names
    raw <- map$raw[match(team, map$canonical)]
    if (is.na(raw)) stop("Cannot map legacy team: ", team)
    clean <- e$clean_team_names(data.frame(home = raw, away = raw))
    if (!nrow(clean)) stop("Team excluded by the original notebook: ", team)
    clean$home[1]
  }
  home <- naming(game$home); away <- naming(game$away)
  result <- e$predict_winner(home, away, model, data)$score_differential
  reverse <- if (isTRUE(game$neutral_site)) e$predict_winner(away, home, model, data)$score_differential else NA_real_
  margin <- if (isTRUE(game$neutral_site)) (result-reverse)/2 else result
  stopifnot(length(margin) == 1L, is.finite(margin))
  list(expected_margin = as.numeric(margin), home_orientation = as.numeric(result),
    reversed_orientation = as.numeric(reverse))
}

model1_bracket <- function() {
  data.frame(round = c(rep(1L, 4), rep(2L, 4), 3L, 3L, 4L),
    game_id = as.character(c(401677179,401677178,401677176,401677177,401677181,
      401677182,401677183,401677184,401677189,401677191,401677192)),
    home = c("Notre Dame","Penn State","Texas","Ohio State","Boise State",
      "Arizona State","Oregon","Georgia","","",""),
    away = c("Indiana","SMU","Clemson","Tennessee",rep("",7)),
    home_from = c(rep("",8),"401677181","401677182","401677189"),
    away_from = c(rep("",4),"401677178","401677176","401677177","401677179",
      "401677184","401677183","401677191"),
    neutral_site = c(rep(FALSE,4),rep(TRUE,7)), stringsAsFactors = FALSE)
}

model1_advance_bracket <- function(bracket, predictor) {
  winners <- character()
  rows <- lapply(seq_len(nrow(bracket)), function(i) {
    game <- bracket[i, ]
    for (side in c("home", "away")) {
      ref <- game[[paste0(side, "_from")]]
      if (nzchar(ref)) {
        stopifnot(ref %in% names(winners))
        game[[side]] <- unname(winners[ref])
      }
    }
    prediction <- predictor(game)
    winner <- if (prediction$expected_margin >= 0) game$home else game$away
    winners[game$game_id] <<- winner
    cbind(game, as.data.frame(prediction), winner = winner)
  })
  do.call(rbind, rows)
}

model1_v3_frozen <- function(config, actual, cutoff) {
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  games$kickoff <- parse_utc_datetime(games$kickoff)
  model_week <- min(actual$model_week)
  past <- games[which(games$season <= 2024 & games$kickoff < cutoff &
    (games$season < 2024 | games$model_week < model_week)), ]
  stopifnot(nrow(past) > 0, !anyNA(past$kickoff), all(past$kickoff < cutoff))
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  team_games$kickoff <- parse_utc_datetime(team_games$kickoff)
  # Bridge calibration also needs FCS rows not present in the FBS schedule.
  bridge_history <- team_games[team_games$season < 2024, ]
  team_games <- team_games[team_games$game_id %in% past$game_id, ]
  teams <- sort(unique(c(actual$home, actual$away)))
  schedule <- expand.grid(home = teams, away = teams, stringsAsFactors = FALSE)
  schedule <- schedule[schedule$home != schedule$away, ]
  schedule$neutral_site <- TRUE
  campus <- actual[!actual$neutral_site, c("home", "away", "neutral_site")]
  schedule <- rbind(schedule, campus)
  schedule$game_id <- paste0("frozen_", seq_len(nrow(schedule)))
  schedule$season <- 2024L; schedule$week <- 1L; schedule$model_week <- model_week
  schedule$kickoff <- cutoff; schedule$margin <- NA_real_; schedule$completed <- FALSE
  schedule$home_level <- schedule$away_level <- "fbs"
  schedule$is_cfp <- TRUE; schedule$postseason_type <- "cfp"; schedule$coach_lookup_week <- 99L
  power <- bind_rows_fill(cached_power_rows(past), power_rating_for_week(past, 2024, model_week, 99L, config))
  targets <- data.frame(game_id = rep(schedule$game_id,2), team = c(schedule$home,schedule$away),
    season = 2024L, week = 1L, model_week = model_week, kickoff = cutoff,
    offense_epa = NA_real_, defense_epa = NA_real_, net_efficiency = NA_real_)
  team_history <- team_games[team_games$team %in% teams & team_games$season >= 2021, ]
  built <- build_team_pregame_snapshots(bind_rows_fill(team_history, targets),
    bind_rows_fill(past, schedule), power, config)
  snapshots <- built$snapshots[built$snapshots$game_id %in% schedule$game_id, ]
  schedule <- built$games[match(schedule$game_id, built$games$game_id), ]
  features <- build_historical_matchup_table(schedule, snapshots, config)
  coach_history <- read_csv_if_present(file.path(config$inbox_dir, "coach_history.csv"), TRUE)
  coach_history <- coach_history[which(coach_history$season < 2024 |
    (coach_history$season == 2024 & coach_history$week < model_week)), ]
  assignments <- read_csv_if_present(file.path(config$inbox_dir, "coach_assignments.csv"), TRUE)
  features <- attach_coach_features_to_history(features, assignments, coach_history, config)$training
  stopifnot(!any(features$coach_mapping_missing))
  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  prepared <- prepare_training_data(training[training$season < 2024, ], config)
  members <- combine_membership(read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  prepared$data <- apply_fbs_bridge_backtest_features(prepared$data,
    bridge_history, members[members$season < 2024, ], config)
  choices <- read_csv_if_present(file.path(config$output_dir, "experiments", "controlled_ats",
    "20260909T182438Z", "fold_choices.csv"), TRUE)
  choice <- choices[choices$model == "v3_control" & choices$test_season == 2024, ]
  stopifnot(nrow(choice) == 1L, choice$train_through_season < 2024)
  column <- if (choice$coach_recent_share == .65) "coach_rating_65_35_diff" else "coach_rating_70_30_diff"
  prepared$data$coach_rating_diff <- prepared$data[[column]]
  features$coach_rating_diff <- features[[column]]
  columns <- setdiff(football_feature_names(prepared$data),
    c("coach_rating_65_35_diff", "coach_rating_70_30_diff"))
  fit <- fit_weighted_ridge(prepared$data, "margin", columns,
    prepared$weights/max(prepared$weights), lambda = choice$foundation_lambda)
  features$expected_margin <- as.numeric(predict(fit, features))
  key <- function(d) paste(d$home,d$away,d$neutral_site,sep="|")
  predictor <- function(game) {
    index <- match(key(game), key(features))
    stopifnot(length(index) == 1L, !is.na(index))
    list(expected_margin = features$expected_margin[index])
  }
  saved <- read_csv_if_present(file.path(config$output_dir,"backtest","rolling_predictions.csv"), TRUE)
  opening <- actual[!actual$neutral_site, ]
  saved <- saved[saved$model == "ridge_core", ]
  replay <- vapply(seq_len(nrow(opening)), function(i) predictor(opening[i, ])$expected_margin, numeric(1))
  error <- max(abs(replay-saved$expected_margin[match(opening$game_id,saved$game_id)]))
  if (!is.finite(error) || error > 1e-8) {
    stop("Frozen v3 does not reproduce first-round reference: ", error)
  }
  list(features = features, fit = fit, choice = choice, predictor = predictor,
    first_round_max_delta = error, history_last_kickoff = max(past$kickoff),
    training_seasons = sort(unique(prepared$data$season)))
}

model1_freeze_pbp <- function(pbp, cutoff, playoff_ids) {
  dates <- parse_utc_datetime(pbp$start_date)
  in_season <- pbp$year == 2024
  if (any(in_season & is.na(dates))) stop("Missing 2024 PBP kickoff; cannot certify the freeze.")
  frozen <- pbp[in_season & dates < cutoff, ]
  stopifnot(nrow(frozen) > 0, !any(frozen$game_id %in% playoff_ids),
    all(parse_utc_datetime(frozen$start_date) < cutoff))
  frozen
}

model1_resolve_dates <- function(pbp, games, required_teams) {
  stopifnot(!anyDuplicated(games$game_id), length(required_teams) > 0)
  missing <- pbp$year == 2024 & is.na(parse_utc_datetime(pbp$start_date))
  ids <- unique(pbp$game_id[missing])
  dates <- games$kickoff[match(ids, games$game_id)]
  audit <- data.frame(game_id = ids, resolved_kickoff = dates,
    status = ifelse(is.na(parse_utc_datetime(dates)), "omitted_non_CFP_team_game", "schedule_date_recovered"))
  pbp$start_date[missing] <- dates[match(pbp$game_id[missing], ids)]
  unresolved <- pbp$year == 2024 & is.na(parse_utc_datetime(pbp$start_date))
  affected <- canonical_team(c(pbp$home[unresolved], pbp$away[unresolved]))
  if (any(affected %in% required_teams)) stop("Unresolved kickoff affects a playoff participant.")
  list(pbp = pbp[!unresolved, ], audit = audit)
}

model1_grade_bracket <- function(picks, actual) {
  stopifnot(!anyDuplicated(picks$game_id), !anyDuplicated(actual$game_id))
  index <- match(picks$game_id, actual$game_id)
  stopifnot(!anyNA(index), all(is.finite(actual$margin[index])), all(actual$margin[index] != 0))
  picks$actual_home <- actual$home[index]
  picks$actual_away <- actual$away[index]
  picks$actual_winner <- ifelse(actual$margin[index] > 0, actual$home[index], actual$away[index])
  picks$correct <- picks$winner == picks$actual_winner
  picks$actual_matchup <- (picks$home == picks$actual_home & picks$away == picks$actual_away) |
    (picks$home == picks$actual_away & picks$away == picks$actual_home)
  picks
}

model1_conditional <- function(actual, predictor) {
  do.call(rbind, lapply(seq_len(nrow(actual)), function(i) {
    game <- actual[i, c("game_id", "home", "away", "neutral_site")]
    prediction <- predictor(game)
    cbind(game, as.data.frame(prediction),
      winner = if (prediction$expected_margin >= 0) game$home else game$away)
  }))
}

run_model1_replay <- function(config) {
  stopifnot(identical(config$version, "3.0.0"))
  protected <- experiment_file_hashes(config)
  root <- file.path(config$project_dir, "cfb_v3", "experiments")
  output <- file.path(config$output_dir, "experiments", "model1_replay",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  stopifnot(!dir.exists(output))
  dir.create(output, recursive = TRUE)
  source <- model1_source(config$project_dir)
  writeLines(source$lines, file.path(output, "archived_model_1.Rmd"))
  file.copy(file.path(root, "MODEL1_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  inputs <- model1_archived_joins(model1_extract(config$project_dir,
    file.path(config$output_dir, "experiments", "model1_inputs")), config$project_dir)
  stopifnot(identical(inputs$source_md5, unname(tools::md5sum(file.path(config$project_dir, ".RData")))))
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  bracket <- model1_bracket()
  actual <- games[match(bracket$game_id, games$game_id), ]
  stopifnot(nrow(actual) == 11L, !anyNA(actual$game_id), all(actual$season == 2024), all(actual$is_cfp))
  cutoff <- min(parse_utc_datetime(actual$kickoff))
  dates <- model1_resolve_dates(inputs$pbp, games, unique(c(actual$home, actual$away)))
  write.csv(dates$audit, file.path(output, "pbp_date_recovery.csv"), row.names = FALSE)
  frozen_pbp <- model1_freeze_pbp(dates$pbp, cutoff, actual$game_id)
  message("Rebuilding the original six-game tables; no playoff PBP in prediction inputs")
  train <- model1_table(inputs$pbp[inputs$pbp$year <= 2021, ], inputs, source, "train")
  frozen <- model1_table(frozen_pbp, inputs, source, "predict")
  saveRDS(train$data, file.path(output, "legacy_training_table.rds"))
  saveRDS(frozen$data, file.path(output, "legacy_frozen_table.rds"))
  train_ids <- train$full$game_id[complete.cases(train$full[, names(train$data)])]
  stopifnot(length(train_ids) == nrow(train$data))
  write.csv(data.frame(game_id = train_ids, train$data),
    file.path(output, "legacy_training_rows.csv"), row.names = FALSE)
  write.csv(frozen$data, file.path(output, "legacy_frozen_rows.csv"), row.names = FALSE)
  seeds <- c(20260909L, 20260910L, 20260911L)
  summaries <- list(); brackets <- list(); conditional <- list()
  record <- function(name, seed, predictor) {
    picked <- model1_grade_bracket(model1_advance_bracket(bracket, predictor), actual)
    given <- model1_grade_bracket(model1_conditional(actual, predictor), actual)
    picked$model <- given$model <- name
    picked$seed <- given$seed <- seed
    brackets[[name]] <<- picked
    conditional[[name]] <<- given
    summaries[[name]] <<- data.frame(model = name, seed = seed,
      frozen_bracket_correct = sum(picked$correct), games = nrow(picked),
      conditional_matchup_correct = sum(given$correct),
      first_round_correct = sum(picked$correct[picked$round == 1]),
      champion = tail(picked$winner, 1), stringsAsFactors = FALSE)
  }
  for (seed in seeds) {
    message("Fitting archived model (1), locked seed ", seed)
    set.seed(seed)
    # The original grid contains only mtry=6. CV cannot select another setting;
    # fit that final model directly, without claiming to reproduce its unknown RNG state.
    model <- caret::train(home_away_score_dif ~ ., data = train$data, method = "rf",
      trControl = caret::trainControl(method = "none"), tuneGrid = data.frame(mtry = 6),
      ntree = 500, nodesize = 5)
    name <- paste0("archived_model1_", seed)
    saveRDS(model, file.path(output, paste0(name, ".rds")))
    predictor <- function(game) model1_predict_matchup(model, frozen$data, game, source, inputs)
    record(name, seed, predictor)
    print(summaries[[name]])
  }
  message("Rebuilding the frozen v3 bracket and verifying first-round reference")
  v3 <- model1_v3_frozen(config, actual, cutoff)
  saveRDS(v3[setdiff(names(v3), "predictor")], file.path(output, "v3_frozen.rds"))
  write.csv(v3$features, file.path(output, "v3_frozen_features.csv"), row.names = FALSE)
  record("v3_frozen", NA_integer_, v3$predictor)
  summary <- do.call(rbind, summaries)
  picks <- do.call(bind_rows_fill, brackets)
  given <- do.call(bind_rows_fill, conditional)
  write.csv(summary, file.path(output, "summary.csv"), row.names = FALSE)
  write.csv(picks, file.path(output, "frozen_bracket_picks.csv"), row.names = FALSE)
  write.csv(given, file.path(output, "conditional_matchup_picks.csv"), row.names = FALSE)
  after <- experiment_file_hashes(config)
  stopifnot(identical(protected, after))
  verification <- list(production_unchanged = TRUE,
    pbp_cutoff_exclusive = format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    legacy_pbp_last_kickoff = format(max(parse_utc_datetime(frozen_pbp$start_date)), tz = "UTC"),
    legacy_training_rows = nrow(train$data), legacy_training_unique_games = length(unique(train_ids)),
    legacy_training_years = 2014:2021, legacy_selected_columns = names(train$data),
    pbp_date_recovery = as.list(table(dates$audit$status)),
    original_cv_note = "Fixed mtry=6; direct final refit, original RNG state unavailable",
    seeds = seeds, v3_first_round_max_delta = v3$first_round_max_delta,
    v3_training_seasons = v3$training_seasons,
    v3_history_last_kickoff = format(v3$history_last_kickoff, tz = "UTC"),
    ratings_snapshot_timing = "REJECTED FOR PREGAME COMPARISON: September 9 audit matched cached 2024 SP+ to all 134 post-playoff FBS FINAL rows; Elo release timing is unverified. See OG_DATA_AUDIT.md",
    archived_commit = "365ca12", workspace_md5 = inputs$source_md5,
    code_md5 = as.list(tools::md5sum(file.path(root, c("model1_replay.R", "MODEL1_PROTOCOL.md")))),
    protected_input_hashes = as.list(protected))
  jsonlite::write_json(verification, file.path(output, "verification.json"), auto_unbox = TRUE, pretty = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(output, "sessionInfo.txt"))
  table <- c("| Model | Frozen bracket | Given actual matchups | Champion |",
    "|---|---:|---:|---|", vapply(seq_len(nrow(summary)), function(i) {
      x <- summary[i, ]
      sprintf("| %s | %d/11 | %d/11 | %s |", x$model, x$frozen_bracket_correct,
        x$conditional_matchup_correct, x$champion)
    }, character(1)))
  report <- file.path(output, "REPORT.md")
  writeLines(c("# Archived Model (1): Frozen 2024 CFP Replay", "",
    "User workflow: entire bracket picked before the first playoff game. Both models advance their own winners with all inputs held fixed.", "",
    table, "", "## Interpretation", "",
    "This is an archival-workflow reconstruction, not a recovered live prediction record. The user's reported 11-0 remains separate from these results.",
    "The earlier five-game comparison used prior-year SP+ and updated v3 each round; it cannot establish what the original frozen bracket predicted.",
    "The September 9 source audit matched all 134 cached 2024 SP+ teams to Connelly's post-playoff FBS FINAL ratings, including offense and defense. This replay is contaminated by final ratings and cannot be treated as a pregame comparison. Weekly Elo release timing is also unverified. See OG_DATA_AUDIT.md. The user's saved bracket remains separate evidence.",
    "The three seeds were fixed before seeing results. None is selected or tuned to obtain 11-0. Original random-forest RNG state is unavailable.", "",
    "## Saved Bracket Evidence", "",
    "The separate original presentation, CFB Playoff  Bracket Prediction.pdf, page 18, contains all 11 correct winners, including Ohio State over Notre Dame. Page 19 also shows Ohio State as champion. Its local Git blob matches the January 2025 archive: 10b2cd775ba80d0cdb72b6d68a094e60826804ad.",
    "The PDF's creation metadata is December 13, 2024. This supports the user's pre-playoff account, although metadata alone is not independent proof of public publication time. The saved bracket is evidence distinct from this newly fitted replay.", "",
    "## Reproduction Details", "",
    sprintf("- Original Git notebook: 365ca12:model (1).Rmd; training 2014-2021, six-game window, mtry=6, 500 trees, node size 5; %d rows representing %d unique games.", nrow(train$data), length(unique(train_ids))),
    "- Original joins and complete-case filtering retained, including duplicate training rows. PFF, coach and injury columns are not direct predictors in this archived variant.",
    "- Join CSVs recovered from the same commit: pff_team_data (2).csv, coach_ratings (1).csv, final_for_sure.csv. The notebook references different filenames for the first two; identical live input versions cannot be certified.",
    "- Original latest-home/latest-away lookup retained; neutral margin = (home orientation - reversed orientation) / 2.",
    sprintf("- No 2024 CFP play-by-play in the legacy prediction table; exclusive cutoff %s.", verification$pbp_cutoff_exclusive),
    "- Missing 2024 PBP dates recovered by game ID from the cached schedule. Unresolved games not involving CFP participants are omitted and listed in pbp_date_recovery.csv; any unresolved CFP participant would stop the replay.",
    "- V3 trained through 2023 with its previously selected 2024 settings. Original trains only through 2021: this is a workflow comparison, not a single-factor ablation.",
    sprintf("- V3 first-round maximum difference from saved reference: %.3g points.", v3$first_round_max_delta),
    "- Conditional-matchup counts are diagnostic only: they supply actual later participants without updating features. Frozen-bracket counts do not.",
    "- Positive expected_margin means the listed home team is favored, including nominal home teams at neutral sites.",
    "- No production model, cached foundation, official picks or earlier experiment outputs were changed.", "",
    "Per-game outputs: frozen_bracket_picks.csv and conditional_matchup_picks.csv. Input tables, fitted models, hashes and session information are retained beside this report."), report)
  list(report = report, summary = summary, verification = verification)
}
