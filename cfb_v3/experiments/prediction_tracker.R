tracker_features <- function() c("challenger_tracker_sagarin", "challenger_tracker_fpi")

tracker_team <- function(x) {
  x <- external_team(gsub(" St\\.$", " State", trimws(x)))
  aliases <- c("Central Florida" = "UCF", "Central Mich." = "Central Michigan",
    "Eastern Mich." = "Eastern Michigan", "Western Mich." = "Western Michigan",
    "Northern Ill." = "Northern Illinois", "Florida Intl." = "Florida International",
    "Miami (Fla.)" = "Miami", "Middle Tenn." = "Middle Tennessee", "Kent" = "Kent State",
    "Southern Miss." = "Southern Miss", "Texas-San Antonio" = "UTSA",
    "Troy State" = "Troy", "West Va." = "West Virginia", "Sam Houston State" = "Sam Houston",
    "Southern Mississippi" = "Southern Miss", "LouisianaMonroe(ULM)" = "UL Monroe")
  i <- match(x, names(aliases)); x[!is.na(i)] <- unname(aliases[i[!is.na(i)]])
  x
}

tracker_number <- function(x) {
  text <- trimws(as.character(x)); blank <- is.na(x) | text %in% c("", ".", "NA")
  value <- suppressWarnings(as.numeric(text)); value[blank] <- NA_real_
  if (any(!blank & !is.finite(value))) stop("Invalid nonnumeric tracker value.")
  value
}

tracker_read <- function(path, season) {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  required <- c("Home", "Road", "week", "linesagpred", "lineespn", "lineopen", "linemidweek", "line", "hscore", "vscore", "actual")
  assert_columns(raw, required, "Prediction Tracker archive")
  duplicate <- duplicated(raw)
  x <- raw[required]
  for (name in setdiff(required, c("Home", "Road"))) x[[name]] <- tracker_number(x[[name]])
  if (anyNA(x$week) || any(x$week < 1 | x$week != floor(x$week))) stop("Invalid tracker week.")
  if (any(!is.finite(x$actual) | !is.finite(x$hscore) | !is.finite(x$vscore)) ||
      any(x$hscore-x$vscore != x$actual)) stop("Tracker score arithmetic mismatch.")
  x$season <- as.integer(season); x$source_row <- seq_len(nrow(x)); x$exact_duplicate <- duplicate
  x$tracker_home <- tracker_team(x$Home); x$tracker_away <- tracker_team(x$Road)
  x
}

tracker_pair <- function(season, home, away) paste(season, pmin(home, away), pmax(home, away), sep = "\r")
tracker_monday <- function(time) {
  date <- as.Date(parse_utc_datetime(time), tz = "America/New_York")
  # Monday-night games belong to the preceding football weekend, including CFP finals.
  date <- date - as.integer(!is.na(date) & format(date, "%u") == "1")
  date - (as.integer(format(date, "%u"))-1L)
}

tracker_join <- function(raw, games) {
  assert_unique_keys(games, "game_id", "canonical tracker schedule")
  g <- games; g$home <- canonical_team(g$home); g$away <- canonical_team(g$away)
  candidates <- split(seq_len(nrow(g)), tracker_pair(g$season, g$home, g$away))
  keys <- tracker_pair(raw$season, raw$tracker_home, raw$tracker_away)
  monday <- tracker_monday(g$kickoff)
  anchors <- list()
  for (season in sort(unique(raw$season))) {
    starts <- offsets <- numeric()
    for (j in which(raw$season == season & !raw$exact_duplicate)) {
      i <- candidates[[keys[j]]]
      if (length(i) != 1L || is.na(monday[i]) || g$postseason_type[i] != "regular") next
      starts <- c(starts, as.numeric(monday[i])-7*(raw$week[j]-1))
      offsets <- c(offsets, raw$week[j]-g$week[i])
    }
    if (!length(starts)) stop("Cannot establish tracker calendar for season ", season)
    count <- table(starts); winners <- names(count)[count == max(count)]
    if (length(winners) != 1L) stop("Ambiguous tracker calendar anchor.")
    anchors[[as.character(season)]] <- data.frame(season = season,
      monday_week_one = as.character(as.Date(as.numeric(winners), origin = "1970-01-01")),
      supporting_rows = as.integer(max(count)), unique_regular_rows = length(starts),
      regular_week_offset = as.numeric(names(sort(table(offsets), decreasing = TRUE))[1]))
  }
  anchors <- do.call(rbind, anchors)
  audit <- data.frame(season = raw$season, source_row = raw$source_row, tracker_week = raw$week,
    tracker_home = raw$Home, tracker_away = raw$Road, home_alias = raw$tracker_home,
    away_alias = raw$tracker_away, game_id = NA_character_, reason = "unmatched_teams",
    orientation = NA_real_, calendar_week = NA_real_, score_verified = FALSE)
  for (j in seq_len(nrow(raw))) {
    if (raw$exact_duplicate[j]) { audit$reason[j] <- "exact_duplicate_removed"; next }
    i <- candidates[[keys[j]]]
    if (!length(i)) next
    anchor <- as.Date(anchors$monday_week_one[match(raw$season[j], anchors$season)])
    calendar <- as.numeric(monday[i]-anchor)/7+1
    if (length(i) > 1L) {
      hit <- which(!is.na(calendar) & calendar == raw$week[j])
      if (length(hit) != 1L) { audit$reason[j] <- "ambiguous_repeated_matchup"; next }
      i <- i[hit]; calendar <- calendar[hit]
    }
    audit$game_id[j] <- as.character(g$game_id[i]); audit$calendar_week[j] <- calendar
    orientation <- if (raw$tracker_home[j] == g$home[i]) 1 else -1
    audit$orientation[j] <- orientation
    if (orientation == -1 && !isTRUE(g$neutral_site[i])) {
      audit$reason[j] <- "nonneutral_home_away_reversal"; next
    }
    hscore <- if (orientation == 1) raw$hscore[j] else raw$vscore[j]
    ascore <- if (orientation == 1) raw$vscore[j] else raw$hscore[j]
    if (!is.finite(g$home_score[i]) || !is.finite(g$away_score[i]) ||
        hscore != g$home_score[i] || ascore != g$away_score[i]) {
      audit$reason[j] <- "score_mismatch_after_identity_join"; next
    }
    audit$score_verified[j] <- TRUE
    audit$reason[j] <- if (is.na(monday[i])) "missing_kickoff" else "matched"
  }
  hit <- which(audit$reason == "matched")
  audit$conflicting_market_fields <- ""
  for (rows in split(hit, audit$game_id[hit])) {
    if (length(rows) < 2L) next
    signature <- raw[rows, c("tracker_home", "tracker_away", "linesagpred", "lineespn", "hscore", "vscore")]
    if (nrow(unique(signature)) != 1L) stop("Conflicting tracker predictions resolve to one game.")
    conflicts <- c("lineopen", "linemidweek", "line")[vapply(raw[rows, c("lineopen", "linemidweek", "line")],
      function(x) length(unique(x)) > 1L, logical(1))]
    for (field in conflicts) raw[[field]][rows] <- NA_real_
    audit$conflicting_market_fields[rows] <- paste(conflicts, collapse = ",")
    audit$reason[rows[-1]] <- "identical_forecast_duplicate_merged"
  }
  hit <- which(audit$reason == "matched")
  x <- g[match(audit$game_id[hit], as.character(g$game_id)), , drop = FALSE]
  x$game_id <- as.character(x$game_id)
  x$tracker_source_row <- raw$source_row[hit]; x$tracker_week <- raw$week[hit]
  for (pair in list(c("challenger_tracker_sagarin", "linesagpred"), c("challenger_tracker_fpi", "lineespn"),
    c("tracker_opening_margin", "lineopen"), c("tracker_midweek_margin", "linemidweek"),
    c("tracker_updated_margin", "line"))) x[[pair[1]]] <- raw[[pair[2]]][hit]*audit$orientation[hit]
  x$tracker_evidence <- "publisher_archive_no_per_game_timestamp"
  x$tracker_calendar_deviation <- audit$calendar_week[hit]-raw$week[hit]
  list(data = x, audit = audit, calendars = anchors)
}

tracker_sources <- function(project) {
  root <- file.path(project, "cfb_v3/output/experiments/external_history_sources")
  old <- file.path(root, "discovery_20260911T235344Z")
  warm <- file.path(root, "tracker_warmup_20260912")
  inventory <- rbind(jsonlite::fromJSON(file.path(warm, "inventory.json"))[c("season", "url", "sha256")],
    jsonlite::fromJSON(file.path(old, "predictiontracker_inventory.json"))[c("season", "url", "sha256")])
  inventory$path <- file.path(c(rep(warm, 2), rep(old, 4)), paste0("predictiontracker_ncaa", inventory$season, ".csv"))
  inventory$retrieved_at <- c(jsonlite::fromJSON(file.path(warm, "inventory.json"))$retrieved_at,
    "2026-09-11T23:56:19.7302450Z", "2026-09-11T23:56:20.1130167Z",
    "2026-09-11T23:56:20.9648892Z", "2026-09-11T23:56:21.3112661Z")
  for (i in seq_len(nrow(inventory))) {
    actual <- digest::digest(file = inventory$path[i], algo = "sha256")
    if (tolower(actual) != tolower(inventory$sha256[i])) stop("Tracker raw source hash changed: ", inventory$path[i])
  }
  raw <- do.call(rbind, lapply(seq_len(nrow(inventory)), function(i)
    tracker_read(inventory$path[i], inventory$season[i])))
  list(raw = raw, inventory = inventory)
}

tracker_prepare <- function(config, joined) {
  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  training <- training[training$season <= 2025, ]
  if (!all(training$feature_version == "3.0.0")) stop("Expected v3 cached features.")
  validation <- prepare_training_data(training, config)
  members <- combine_membership(read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  validation$data <- apply_fbs_bridge_backtest_features(validation$data,
    team_games[team_games$season <= 2025, ], members[members$season <= 2025, ], config)
  priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
  priors <- priors[priors$season <= 2025, ]
  validation$data <- attach_preseason_features(validation$data, priors, config,
    features = config$preseason$production_features, prefix = "ps_")
  i <- match(validation$data$game_id, joined$game_id)
  for (name in tracker_features()) validation$data[[name]] <- joined[[name]][i]
  complete <- Reduce(`&`, lapply(validation$data[tracker_features()], is.finite))
  eligible <- raw_history_eligible(validation$data)
  keep <- !is.na(i) & complete & eligible
  audit <- validation$data[c("game_id", "season", "home", "away")]
  audit$reason <- ifelse(!eligible, "not_fbs_or_non_cfp_bowl", ifelse(is.na(i), "no_tracker_match",
    ifelse(!complete, "missing_external_projection", "included")))
  list(full_data = validation$data, full_weights = validation$weights,
    data = validation$data[keep, ], weights = validation$weights[keep],
    covered_seasons = sort(unique(priors$season)), audit = audit)
}

tracker_validate <- function(data, features, weights, config, fold_features = NULL) {
  allowed <- c(football_feature_names(data), tracker_features())
  if (length(setdiff(features, allowed))) stop("Unexpected tracker challenger features.")
  if (length(weights) != nrow(data)) stop("Tracker training weights do not align.")
  splits <- rolling_season_splits(data)
  if (!length(splits)) stop("Tracker fitting requires at least three seasons.")
  all <- list()
  for (lambda in config$model$ridge_lambda_grid) for (split in splits) {
    fold <- data
    shares <- attr(data, "fold_coach_shares")
    if (!is.null(shares)) {
      share <- shares[[as.character(split$test_season)]]
      fold$coach_rating_diff <- fold[[if (share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
    }
    active <- if (is.null(fold_features)) features else fold_features(split$test_season, features)
    if (length(setdiff(active, allowed))) stop("Unexpected fold features.")
    w <- weights[split$train]/max(weights[split$train])
    fit <- fit_weighted_ridge(fold[split$train, ], "margin", active, w, lambda)
    expected <- predict(fit, fold[split$test, ])
    all[[length(all)+1L]] <- data.frame(row_id = split$test, test_season = split$test_season,
      lambda = lambda, actual = fold$margin[split$test], expected_margin = expected,
      fair_margin = expected, margin_sd = NA_real_, absolute_error = abs(fold$margin[split$test]-expected))
  }
  all <- do.call(rbind, all)
  selected <- lapply(sort(unique(all$test_season)), function(season) {
    lambda <- experiment_choose_lambda(all, season, config$model$ridge_lambda_default)
    all[all$test_season == season & all$lambda == lambda, ]
  })
  scores <- aggregate(absolute_error ~ lambda, all, mean)
  list(predictions = do.call(rbind, selected), best_lambda = scores$lambda[which.min(scores$absolute_error)], scores = scores)
}

tracker_fit_football <- function(data, weights, config, covered_seasons, external = TRUE) {
  name <- if (external) "football_plus_external" else "football_matched_control"
  extra <- if (external) tracker_features() else character()
  ps <- paste0("ps_", config$preseason$production_features, "_diff")
  gate <- experiment_preseason_gate(covered_seasons, ps)
  features_for_fold <- function(season, features) c(gate(season, setdiff(features, extra)), extra)
  # Hold the football-only coach selection fixed across the matched control and augmentation.
  coach <- select_coach_split_validation(data, weights, config, fit_nonlinear = FALSE,
    fold_features = gate)
  foundation <- tracker_validate(coach$data,
    features = c(setdiff(coach$features, ps), extra), weights = weights, config = config)
  preseason <- tracker_validate(coach$data, features = c(coach$features, extra), weights = weights,
    config = config, fold_features = features_for_fold)
  rolling <- blend_rolling_predictions(coach$data, foundation, preseason, config)
  choices <- do.call(rbind, lapply(sort(unique(rolling$predictions$test_season)), function(season) {
    data.frame(model = name, test_season = season, train_through_season = max(data$season[data$season < season]),
      training_rows = sum(data$season < season),
      coach_recent_share = attr(coach$data, "fold_coach_shares")[[as.character(season)]],
      foundation_lambda = unique(foundation$predictions$lambda[foundation$predictions$test_season == season]),
      preseason_lambda = unique(preseason$predictions$lambda[preseason$predictions$test_season == season]))
  }))
  # Final artifact uses only completed 2020-2025 inputs; it is not used to score those years.
  final_data <- coach$data
  final_data$coach_rating_diff <- final_data[[if (coach$recent_share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
  full_features <- features_for_fold(2026L, coach$features)
  base_features <- c(setdiff(coach$features, ps), extra)
  final <- list(name = name, trained_through = max(data$season), coach_share = coach$recent_share,
    foundation = fit_weighted_ridge(final_data, "margin", base_features, weights/max(weights), foundation$best_lambda),
    preseason = fit_weighted_ridge(final_data, "margin", full_features, weights/max(weights), preseason$best_lambda),
    config = config, external_features = extra,
    usage = "Experimental only; requires v3 pregame football features and same-cutoff tracker projections.")
  list(predictions = build_v2_backtest_predictions(coach$data, rolling, name),
    choices = choices, final = final, features = full_features)
}

predict_tracker_challenger <- function(object, new_data) {
  assert_columns(new_data, c("season", "week", "coach_rating_65_35_diff", "coach_rating_70_30_diff"), "challenger input")
  if (any(is.na(new_data$season) | new_data$season <= object$trained_through))
    stop("Final experimental model cannot predict training seasons; use rolling outputs.")
  for (name in object$external_features) {
    assert_columns(new_data, name, "external challenger input")
    if (any(!is.finite(new_data[[name]]))) stop("Explicit finite external projections required.")
  }
  new_data$coach_rating_diff <- new_data[[if (object$coach_share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
  share <- preseason_blend_share_for_data(new_data, object$config)
  (1-share)*predict(object$foundation, new_data)+share*predict(object$preseason, new_data)
}

tracker_forecasts <- function(reference, joined, fitted) {
  assert_unique_keys(joined, "game_id", "joined tracker projections")
  i <- match(reference$game_id, joined$game_id)
  keep <- !is.na(i) & is.finite(joined$challenger_tracker_sagarin[i]) & is.finite(joined$challenger_tracker_fpi[i])
  coverage <- reference[c("game_id", "season", "week", "home", "away", "is_cfp")]
  coverage$reason <- ifelse(is.na(i), "unmatched_tracker", ifelse(!keep, "missing_external_projection", "included"))
  base <- reference[keep, ]; j <- joined[i[keep], ]
  for (field in c("home", "away", "season", "neutral_site"))
    if (!isTRUE(all.equal(base[[field]], j[[field]], check.attributes = FALSE))) stop("Tracker evaluation identity mismatch.")
  if (any(base$actual_margin != j$margin)) stop("Tracker evaluation scores mismatch.")
  for (name in c(tracker_features(), "tracker_opening_margin", "tracker_midweek_margin", "tracker_updated_margin"))
    base[[name]] <- j[[name]]
  base$lane <- "publisher_archive"
  base$evidence_lane <- "publisher_archived_not_independently_timestamp_verified"
  margins <- list(v3_frozen = base$expected_margin, sagarin_predictor = j$challenger_tracker_sagarin,
    espn_fpi = j$challenger_tracker_fpi,
    external_equal = (j$challenger_tracker_sagarin+j$challenger_tracker_fpi)/2)
  margins$v3_external_50_50 <- (margins$v3_frozen+margins$external_equal)/2
  for (name in names(fitted)) {
    p <- fitted[[name]]$predictions
    assert_unique_keys(p, "game_id", name)
    index <- match(base$game_id, p$game_id)
    if (anyNA(index) || any(!is.finite(p$expected_margin[index]))) stop("Missing learned common-game forecast: ", name)
    for (field in c("home", "away", "season", "actual_margin", "closing_home_spread"))
      if (!isTRUE(all.equal(base[[field]], p[[field]][index], check.attributes = FALSE))) stop("Learned/reference mismatch: ", field)
    margins[[name]] <- p$expected_margin[index]
  }
  margins$market_reference <- -base$closing_home_spread
  predictions <- do.call(rbind, lapply(names(margins), function(name) {
    x <- base; x$model <- name; x$expected_margin <- margins[[name]]
    historical_sp_grade(x)
  }))
  assert_unique_keys(predictions, c("game_id", "model"), "tracker predictions")
  list(predictions = predictions, coverage = coverage)
}

tracker_metrics <- function(predictions) {
  sides <- p4_or_independent_sides(predictions)
  p <- predictions
  regular <- p$postseason_type == "regular"
  slices <- list(all_fbs = rep(TRUE, nrow(p)), common_lined = is.finite(p$closing_home_spread),
    regular_season = regular, week_0_1 = regular & p$week <= 1L,
    weeks_2_4 = regular & p$week >= 2L & p$week <= 4L, week_5_plus = regular & p$week >= 5L,
    cfp = p$is_cfp, neutral_cfp = p$is_cfp & p$neutral_site,
    conference_championship = p$postseason_type == "conference_championship",
    article_audience = sides$home | sides$away, spread_0_7 = abs(p$closing_home_spread) <= 7,
    spread_over_7_to_21 = abs(p$closing_home_spread) > 7 & abs(p$closing_home_spread) <= 21,
    spread_over_21 = abs(p$closing_home_spread) > 21,
    alabama_clemson = p$home %in% c("Alabama", "Clemson") | p$away %in% c("Alabama", "Clemson"),
    indiana_smu = p$home %in% c("Indiana", "SMU") | p$away %in% c("Indiana", "SMU"),
    g5_or_other_independent = !(sides$home & sides$away),
    notre_dame = p$home == "Notre Dame" | p$away == "Notre Dame")
  for (year in sort(unique(p$season))) slices[[paste0("season_", year)]] <- p$season == year
  result <- list(); mean_na <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  for (lane in unique(p$lane)) for (model in unique(p$model[p$lane == lane])) for (slice in names(slices)) {
    x <- p[which(p$lane == lane & p$model == model & slices[[slice]]), ]
    n <- sum(x$win)+sum(x$loss); wins <- sum(x$win)
    ci <- if (n) stats::binom.test(wins, n)$conf.int else c(NA_real_, NA_real_)
    result[[length(result)+1L]] <- data.frame(lane = lane, model = model, slice = slice, games = nrow(x),
      margin_games = sum(is.finite(x$mae_error)), margin_mae = mean_na(x$mae_error),
      margin_rmse = sqrt(mean_na((x$expected_margin-x$actual_margin)^2)),
      margin_bias = mean_na(x$expected_margin-x$actual_margin),
      su_wins = sum(x$su_correct, na.rm = TRUE), su_decisions = sum(!is.na(x$su_correct)), su_accuracy = mean_na(x$su_correct),
      wins = wins, losses = sum(x$loss), pushes = sum(x$push), no_selection = sum(!x$selected),
      ats_accuracy = if (n) wins/n else NA_real_, ats_low = ci[1], ats_high = ci[2])
  }
  do.call(rbind, result)
}

tracker_paired <- function(predictions) {
  comparisons <- list(c("sagarin_predictor", "v3_frozen"), c("espn_fpi", "v3_frozen"),
    c("external_equal", "v3_frozen"), c("v3_external_50_50", "v3_frozen"),
    c("football_matched_control", "v3_frozen"), c("football_plus_external", "football_matched_control"),
    c("football_plus_external", "v3_frozen"), c("football_plus_external", "external_only_ridge"),
    c("external_only_ridge", "external_equal"), c("external_equal", "market_reference"))
  result <- omissions <- list()
  mean_na <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  for (lane in unique(predictions$lane)) for (pair in comparisons) {
    a <- predictions[predictions$lane == lane & predictions$model == pair[1], ]
    b <- predictions[predictions$lane == lane & predictions$model == pair[2], ]
    if (!nrow(a) || !nrow(b)) next
    assert_unique_keys(a, "game_id", "paired tracker candidate"); assert_unique_keys(b, "game_id", "paired tracker control")
    index <- match(a$game_id, b$game_id)
    if (nrow(a) != nrow(b) || anyNA(index)) stop("Paired cohorts differ.")
    b <- b[index, ]
    if (!isTRUE(all.equal(a$closing_home_spread, b$closing_home_spread))) stop("Paired lines differ.")
    mae <- a$mae_error-b$mae_error; ats <- a$ats_correct-b$ats_correct
    ci <- experiment_season_interval(mae, a$season); aci <- experiment_season_interval(ats, a$season)
    result[[length(result)+1L]] <- data.frame(lane = lane, candidate = pair[1], reference = pair[2],
      games = nrow(a), mae_change = mean_na(mae), mae_low = ci[1], mae_high = ci[2],
      ats_common_decisions = sum(is.finite(ats)), ats_change = mean_na(ats), ats_low = aci[1], ats_high = aci[2])
    for (year in sort(unique(a$season))) omissions[[length(omissions)+1L]] <- data.frame(lane = lane,
      candidate = pair[1], reference = pair[2], omitted_season = year,
      mae_change = mean_na(mae[a$season != year]), ats_change = mean_na(ats[a$season != year]))
  }
  list(paired = do.call(rbind, result), leave_one_season_out = do.call(rbind, omissions))
}

tracker_line_sensitivity <- function(predictions) {
  result <- list()
  for (name in c("opening", "midweek", "updated")) {
    field <- paste0("tracker_", name, "_margin")
    x <- predictions[is.finite(predictions[[field]]), ]
    x$closing_home_spread <- -x[[field]]
    market <- x$model == "market_reference"
    x$expected_margin[market] <- x[[field]][market]
    x$lane <- paste0("tracker_", name, "_line")
    result[[name]] <- tracker_metrics(historical_sp_grade(x))
  }
  do.call(rbind, result)
}

tracker_pairwise_available <- function(reference, joined) {
  result <- list()
  i <- match(reference$game_id, joined$game_id)
  for (name in c("sagarin_predictor", "espn_fpi")) {
    field <- if (name == "sagarin_predictor") tracker_features()[1] else tracker_features()[2]
    keep <- !is.na(i) & is.finite(joined[[field]][i])
    x <- reference[keep, ]; x$lane <- paste0("available_", name)
    x$model <- "v3_frozen"
    candidate <- x; candidate$model <- name; candidate$expected_margin <- joined[[field]][i[keep]]
    result[[name]] <- historical_sp_grade(rbind(x, candidate))
  }
  do.call(rbind, result)
}

tracker_sp_overlap <- function(project, predictions) {
  directory <- file.path(project, "cfb_v3/output/experiments/external_backtest/20260911T231911.953Z")
  external_verify(directory)
  sp <- read.csv(file.path(directory, "predictions.csv"), stringsAsFactors = FALSE)
  sp <- sp[sp$lane == "same_cycle_corroborated" & sp$model == "sp_rating", ]
  sp$game_id <- as.character(sp$game_id); assert_unique_keys(sp, "game_id", "qualified SP predictions")
  x <- predictions[predictions$game_id %in% sp$game_id, ]
  x$lane <- "qualified_sp_overlap"
  candidate <- x[x$model == "v3_frozen", ]; i <- match(candidate$game_id, sp$game_id)
  for (name in c("home", "away", "season", "actual_margin", "closing_home_spread"))
    if (!isTRUE(all.equal(candidate[[name]], sp[[name]][i], check.attributes = FALSE))) stop("SP-overlap identity mismatch.")
  four <- candidate
  four$model <- "four_system_equal"
  four$expected_margin <- (candidate$expected_margin+sp$expected_margin[i]+
    candidate$challenger_tracker_sagarin+candidate$challenger_tracker_fpi)/4
  candidate$model <- "sp_rating"; candidate$expected_margin <- sp$expected_margin[i]
  historical_sp_grade(rbind(x, candidate, four))
}

tracker_sagarin_page <- function(path, stamp) {
  text <- xml2::xml_text(xml2::read_html(path))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  header <- unique(trimws(lines[grepl("COLLEGE FOOTBALL [0-9]{4} through results of [0-9]{4} [A-Z]+ [0-9]+", lines)]))
  if (length(header) != 1L) stop("Ambiguous archived Sagarin header.")
  year <- as.integer(sub(".*COLLEGE FOOTBALL ([0-9]{4}).*", "\\1", header))
  date_text <- sub(".*through results of ([0-9]{4} [A-Z]+ [0-9]+).*", "\\1", header)
  through <- as.Date(date_text, format = "%Y %B %d")
  capture <- as.POSIXct(stamp, format = "%Y%m%d%H%M%S", tz = "UTC")
  if (is.na(through) || is.na(capture) || as.Date(capture) <= through) stop("Invalid archive capture chronology.")
  hfa <- unique(trimws(lines[grepl("HOME ADVANTAGE=[", lines, fixed = TRUE)]))
  if (length(hfa) != 1L || !grepl("PREDICTOR", text, fixed = TRUE)) stop("Missing Sagarin Predictor header.")
  brackets <- regmatches(hfa, gregexpr("\\[\\s*[-+]?[0-9]+\\.[0-9]+\\]", hfa))[[1]]
  if (length(brackets) < 3L) stop("Sagarin home-advantage layout changed.")
  home_advantage <- as.numeric(gsub("[^0-9.+-]", "", brackets[2]))
  rows <- lines[grepl("^\\s*[0-9]+\\s+.+\\s+[aA]{1,2}\\s*=", lines)]
  parts <- strsplit(rows, "|", fixed = TRUE)
  if (!length(parts) || any(lengths(parts) < 4L)) stop("Sagarin archived table layout changed.")
  values <- lapply(parts, function(p) {
    name <- sub("^\\s*[0-9]+\\s+(.+?)\\s+[aA]{1,2}\\s*=.*", "\\1", p[1])
    numbers <- regmatches(sub(".*=", "", p[1]), gregexpr("[-+]?[0-9]+(?:\\.[0-9]+)?", sub(".*=", "", p[1]), perl = TRUE))[[1]]
    predictor <- as.numeric(sub("^\\s*([-+]?[0-9]+\\.[0-9]+).*", "\\1", p[3]))
    data.frame(team = tracker_team(name), predictor = predictor, wins = as.numeric(numbers[2]), losses = as.numeric(numbers[3]))
  })
  ratings <- unique(do.call(rbind, values))
  assert_unique_keys(ratings, "team", "archived Sagarin team table")
  if (any(!is.finite(ratings$predictor)) || !is.finite(home_advantage)) stop("Invalid archived numeric Predictor.")
  list(season = year, through = through, capture = capture, home_advantage = home_advantage, ratings = ratings, header = header)
}

tracker_sagarin_checks <- function(project, joined, games) {
  root <- file.path(project, "cfb_v3/output/experiments/external_history_sources/discovery_20260911T235344Z")
  hashes <- jsonlite::fromJSON(file.path(root, "checksums.json"))
  files <- hashes[grepl("^sagarin_[0-9]+\\.html$", hashes$file), ]
  checks <- list()
  for (k in seq_len(nrow(files))) {
    path <- file.path(root, files$file[k])
    if (tolower(digest::digest(file = path, algo = "sha256")) != tolower(files$sha256[k])) stop("Archived Sagarin page changed.")
    stamp <- sub("^sagarin_([0-9]+)\\.html$", "\\1", files$file[k])
    page <- tracker_sagarin_page(path, stamp)
    earliest <- as.POSIXct(page$through, tz = "UTC")+36*3600
    latest <- as.POSIXct(page$through, tz = "UTC")+9*86400
    i <- which(joined$season == page$season & joined$kickoff > page$capture &
      joined$kickoff >= earliest & joined$kickoff < latest & raw_history_eligible(joined))
    if (!length(i)) next
    x <- joined[i, ]; r <- page$ratings
    h <- match(tracker_team(x$home), r$team); a <- match(tracker_team(x$away), r$team)
    prior <- games[games$season == page$season & !is.na(games$kickoff) & games$kickoff < earliest &
      !is.na(games$completed) & games$completed & is.finite(games$margin), ]
    record <- vapply(seq_len(nrow(r)), function(j) {
      margins <- c(prior$margin[tracker_team(prior$home) == r$team[j]], -prior$margin[tracker_team(prior$away) == r$team[j]])
      r$wins[j] == sum(margins > 0) && r$losses[j] == sum(margins < 0)
    }, logical(1))
    calculated <- r$predictor[h]-r$predictor[a]+ifelse(x$neutral_site, 0, page$home_advantage)
    delta <- calculated-x$challenger_tracker_sagarin
    verified <- !is.na(h) & !is.na(a) & record[h] & record[a] & is.finite(delta) & abs(delta) <= .011
    verified[is.na(verified)] <- FALSE
    reason <- ifelse(is.na(h) | is.na(a), "unmapped_archived_team",
      ifelse(!record[h] | !record[a], "record_mismatch", ifelse(verified, "verified",
        ifelse(is.finite(delta) & abs(abs(delta)-page$home_advantage) <= .011, "venue_convention_difference", "numeric_mismatch"))))
    checks[[length(checks)+1L]] <- data.frame(game_id = x$game_id, season = x$season, home = x$home, away = x$away,
      capture = external_stamp(page$capture), through = as.character(page$through), kickoff = external_stamp(x$kickoff),
      predictor_hfa = page$home_advantage, calculated_margin = calculated, tracker_margin = x$challenger_tracker_sagarin,
      difference = delta, home_record_verified = record[h], away_record_verified = record[a],
      sagarin_verified = verified, reason = reason, source_file = files$file[k],
      source_url = paste0("https://web.archive.org/web/", stamp, "/http://sagarin.com/sports/cfsend.htm"))
  }
  do.call(rbind, checks)
}

tracker_report <- function(output, metrics, pairs, coverage, choices, parity, available, sp, witness) {
  display <- function(x) {
    x$ats_accuracy <- round(100*x$ats_accuracy, 2); x$su_accuracy <- round(100*x$su_accuracy, 2)
    x$margin_mae <- round(x$margin_mae, 3)
    x <- x[c("model", "slice", "games", "wins", "losses", "pushes", "no_selection", "ats_accuracy", "su_accuracy", "margin_mae")]
    names(x)[names(x) == "ats_accuracy"] <- "ATS_percent"; names(x)[names(x) == "su_accuracy"] <- "SU_percent"
    markdown_table(x)
  }
  primary <- metrics[metrics$lane == "publisher_archive", ]
  writeLines(c("# Prediction Tracker historical challenger", "",
    "## Scope", "",
    "Frozen v3 versus archived Sagarin Predictor/FPI, fixed blends and past-only learned challengers.",
    "Primary: common FBS games, canonical closing lines, non-CFP bowls excluded. No production model promotion.",
    "The tracker archive has no per-game publication timestamps: this is NOT verified Friday execution or actual betting ROI.",
    "Tracker game projections already include venue adjustments. No additional home-field points are added.", "",
    "## Common-game results", "", display(primary[primary$slice == "common_lined", ]), "",
    "No-picks include exactly zero model edges; pushes do not enter ATS accuracy. A market-reference forecast has no ATS picks.",
    "Straight-up pick ties are excluded. Margin MAE is the leading criterion; ATS is secondary.", "",
    "## Seasons", "", display(primary[grepl("^season_", primary$slice), ]), "",
    "## Early season and playoff", "", display(primary[primary$slice %in% c("week_0_1", "weeks_2_4", "week_5_plus", "cfp", "neutral_cfp"), ]), "",
    "CFP means actual round matchups, not an entire bracket frozen before the tournament. Small CFP samples are descriptive.", "",
    "The all-system cohort has NO 2025 CFP games: the tracker omits FPI for that entire postseason.",
    "The available-Sagarin comparison below retains those games, with a separately matched v3 denominator.", "",
    "## Broader pairwise playoff coverage", "", display(available[available$slice == "cfp", ]), "",
    "These are separate available-source cohorts, not the shared learned-model cohort above. See available_pairwise_metrics.csv.", "",
    "## Qualified SP+ overlap", "", display(sp[sp$slice == "common_lined", ]), "",
    "This smaller cohort inherits the SP+ reconstruction limits and has no early-season or CFP coverage. Equal four-system blend is fixed, not tuned.", "",
    "## Independent Sagarin checks", "",
    paste("Candidate page/game checks:", nrow(witness), "; numeric plus pregame-record checks passed:", sum(witness$sagarin_verified),
      "; unique verified games:", length(unique(witness$game_id[witness$sagarin_verified]))),
    "Four original Wayback pages, one per evaluation season, are checked against numeric Predictor/HFA and pregame team records.",
    "Four numeric disagreements equal the page's HFA exactly, indicating different venue conventions. Archived projections are preserved, not edited after results.",
    "This authenticates only the matched Sagarin values, not FPI, every training input, or Friday market availability.",
    "See sagarin_source_checks.csv and independently_checked_sagarin_metrics.csv. A failed check is not proof of leakage; ratings may have updated.", "",
    "## Paired differences", "", markdown_table(pairs$paired), "",
    "Negative MAE change favors the candidate; positive ATS change favors the candidate. ATS deltas use only common decisions.",
    "Intervals resample seasons (5,000 draws). Four clusters and repeated historical experimentation limit inference; these are not confirmatory tests.",
    "Inspect leave_one_season_out.csv for dependence on one season, and line_sensitivity_metrics.csv for alternative quoted lines.", "",
    "## Fitting", "", markdown_table(choices), "",
    "2020-2021 warm-up; predict 2022-2025 with earlier seasons only. Default penalty 8; subsequent choices use earlier validation MAE.",
    "Same football features, decay, coach selection and preseason blending as v3. External projections are explicit additional features.",
    "External providers may internally retain preseason/talent/brand priors late in the season; this challenger cannot remove those components.",
    "Final RDS artifacts trained through 2025 are for future experiments only; they were not used to produce historical scores.", "",
    "## Coverage and integrity", "", markdown_table(coverage), "",
    paste("Maximum regenerated v3/frozen prediction difference:", format(parity, scientific = TRUE)),
    "See join_audit.csv, tracker_calendars.csv, training_coverage.csv, evaluation_coverage.csv and source_inventory.csv.",
    "Conflicting market-only duplicate quotes are missing in sensitivity scoring. Score-mismatched or nonneutral-reversed source rows are quarantined.",
    "Protected production files were hashed before and after. All changes are isolated experiment files.", "",
    "## Other diagnostic outputs", "",
    "metrics.csv includes spread-size, Alabama/Clemson, Indiana/SMU, Notre Dame, G5/other-independent and audience slices.",
    "line_sensitivity_metrics.csv uses tracker opening/midweek/updated lines, never relabeled as bookmaker-verified closings.",
    "G5/other-independent means at least one team outside the power-conference/ND/UConn audience definition, not a new training predictor.",
    "Source details: EXTERNAL_HISTORY_SOURCES.md; locked design: copied PROTOCOL.md."), file.path(output, "REPORT.md"))
}

run_prediction_tracker <- function(project) {
  config <- cfb_v2_config(project, 2026L)
  stopifnot(config$version == "3.0.0", identical(config$model$ridge_lambda_grid, c(.5, 2, 8, 32)), config$model$ridge_lambda_default == 8)
  protected <- experiment_file_hashes(config)
  output <- external_new_dir(file.path(config$output_dir, "experiments/prediction_tracker"), "")
  message("Tracker experiment output: ", output)
  root <- file.path(project, "cfb_v3/experiments")
  code <- c(file.path(root, c("prediction_tracker.R", "PREDICTION_TRACKER_PROTOCOL.md", "controlled_ats.R", "four_way.R", "external_backtest.R", "external_ratings.R")),
    file.path(project, "run_cfb_prediction_tracker.R"))
  code_before <- vapply(code, function(path) digest::digest(file = path, algo = "sha256"), "")
  write.csv(data.frame(path = code, sha256 = code_before), file.path(output, "code_hashes.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
  file.copy(file.path(root, "PREDICTION_TRACKER_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  write.csv(protected, file.path(output, "protected_before.csv"), row.names = FALSE)
  jsonlite::write_json(list(status = "started", automatic_promotion = FALSE), file.path(output, "manifest.json"), auto_unbox = TRUE)
  source <- tracker_sources(project)
  write.csv(source$inventory, file.path(output, "source_inventory.csv"), row.names = FALSE)
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  games$game_id <- as.character(games$game_id); games$kickoff <- parse_utc_datetime(games$kickoff)
  joined <- tracker_join(source$raw, games)
  write.csv(joined$audit, file.path(output, "join_audit.csv"), row.names = FALSE)
  write.csv(joined$calendars, file.path(output, "tracker_calendars.csv"), row.names = FALSE)
  write.csv(joined$data, file.path(output, "joined_inputs.csv"), row.names = FALSE)
  counts <- as.data.frame(xtabs(~ season + reason, joined$audit))
  counts <- counts[counts$Freq > 0, ]; write.csv(counts, file.path(output, "join_counts.csv"), row.names = FALSE)
  message("Preparing unchanged cached football features")
  prepared <- tracker_prepare(config, joined$data)
  write.csv(prepared$audit, file.path(output, "training_coverage.csv"), row.names = FALSE)
  saveRDS(prepared[c("data", "weights", "covered_seasons")], file.path(output, "training_inputs.rds"))
  message("Reproducing frozen v3 before fitting challengers")
  control <- experiment_football(prepared$full_data, prepared$full_weights, config, prepared$covered_seasons, "v3_frozen")
  frozen <- read_csv_if_present(file.path(config$output_dir, "backtest/rolling_predictions.csv"), TRUE)
  frozen <- frozen[frozen$model == "ridge_core", ]; frozen$game_id <- as.character(frozen$game_id)
  assert_unique_keys(frozen, "game_id", "frozen v3 predictions")
  idx <- match(control$predictions$game_id, frozen$game_id)
  if (anyNA(idx) || nrow(frozen) != nrow(control$predictions)) stop("V3 reproduction population mismatch.")
  parity <- max(abs(control$predictions$expected_margin-frozen$expected_margin[idx]))
  if (parity > 1e-8) stop("V3 no longer reproduces its frozen reference.")
  message("Frozen v3 parity: ", parity)
  fitted <- list()
  for (external in c(FALSE, TRUE)) {
    name <- if (external) "football_plus_external" else "football_matched_control"
    message("Fitting ", name)
    fitted[[name]] <- tracker_fit_football(prepared$data, prepared$weights, config, prepared$covered_seasons, external)
    saveRDS(fitted[[name]]$final, file.path(output, paste0(name, "_model.rds")))
  }
  message("Fitting external-only ridge")
  small <- tracker_validate(prepared$data, features = tracker_features(), weights = prepared$weights, config = config)
  fitted$external_only_ridge <- list(predictions = build_v2_backtest_predictions(prepared$data, small, "external_only_ridge"))
  fitted$external_only_ridge$choices <- do.call(rbind, lapply(sort(unique(small$predictions$test_season)), function(year) {
    data.frame(model = "external_only_ridge", test_season = year,
      train_through_season = max(prepared$data$season[prepared$data$season < year]),
      training_rows = sum(prepared$data$season < year), coach_recent_share = NA_real_,
      foundation_lambda = unique(small$predictions$lambda[small$predictions$test_season == year]), preseason_lambda = NA_real_)
  }))
  saveRDS(fit_weighted_ridge(prepared$data, "margin", tracker_features(), prepared$weights/max(prepared$weights), small$best_lambda),
    file.path(output, "external_only_ridge_model.rds"))
  reference <- historical_sp_reference(frozen, games)
  built <- tracker_forecasts(reference, joined$data, fitted)
  predictions <- built$predictions
  write.csv(built$coverage, file.path(output, "evaluation_coverage.csv"), row.names = FALSE)
  write.csv(predictions, file.path(output, "predictions.csv"), row.names = FALSE)
  metrics <- tracker_metrics(predictions)
  pairs <- tracker_paired(predictions)
  choices <- do.call(rbind, lapply(fitted, function(x) x$choices))
  stopifnot(all(choices$train_through_season < choices$test_season))
  feature_manifest <- do.call(rbind, lapply(names(fitted), function(name) {
    features <- if (name == "external_only_ridge") tracker_features() else fitted[[name]]$features
    data.frame(model = name, feature = features)
  }))
  stopifnot(!any(feature_manifest$feature %in% c("home", "away", "margin", "actual_margin", "season", "week")),
    !any(grepl("spread|score|market|^line", feature_manifest$feature)))
  write.csv(feature_manifest, file.path(output, "feature_manifest.csv"), row.names = FALSE)
  write.csv(choices, file.path(output, "fold_choices.csv"), row.names = FALSE)
  write.csv(metrics, file.path(output, "metrics.csv"), row.names = FALSE)
  for (name in names(pairs)) write.csv(pairs[[name]], file.path(output, paste0(name, ".csv")), row.names = FALSE)
  write.csv(tracker_line_sensitivity(predictions), file.path(output, "line_sensitivity_metrics.csv"), row.names = FALSE)
  available_predictions <- tracker_pairwise_available(reference, joined$data)
  available <- tracker_metrics(available_predictions)
  write.csv(available_predictions, file.path(output, "available_pairwise_predictions.csv"), row.names = FALSE)
  write.csv(available, file.path(output, "available_pairwise_metrics.csv"), row.names = FALSE)
  sp_predictions <- tracker_sp_overlap(project, predictions)
  sp <- tracker_metrics(sp_predictions)
  write.csv(sp_predictions, file.path(output, "sp_overlap_predictions.csv"), row.names = FALSE)
  write.csv(sp, file.path(output, "sp_overlap_metrics.csv"), row.names = FALSE)
  witness <- tracker_sagarin_checks(project, joined$data, games)
  write.csv(witness, file.path(output, "sagarin_source_checks.csv"), row.names = FALSE)
  verified_ids <- unique(witness$game_id[witness$sagarin_verified])
  verified <- available_predictions[available_predictions$lane == "available_sagarin_predictor" &
    available_predictions$game_id %in% verified_ids, ]
  if (!nrow(verified)) stop("No independently corroborated Sagarin games.")
  verified$lane <- "independently_checked_sagarin_only"
  write.csv(tracker_metrics(verified), file.path(output, "independently_checked_sagarin_metrics.csv"), row.names = FALSE)
  tracker_report(output, metrics, pairs, counts, choices, parity, available, sp, witness)
  after <- experiment_file_hashes(config)
  write.csv(after, file.path(output, "protected_after.csv"), row.names = FALSE)
  if (!identical(protected, after)) stop("Protected production files changed during tracker experiment.")
  code_after <- vapply(code, function(path) digest::digest(file = path, algo = "sha256"), "")
  if (!identical(code_before, code_after)) stop("Experiment code changed while running.")
  jsonlite::write_json(list(status = "complete", production_unchanged = TRUE, automatic_promotion = FALSE,
    v3_reproduction_max_delta = parity, training_rows = nrow(prepared$data),
    common_test_games = length(unique(predictions$game_id)),
    sagarin_independently_checked_games = length(verified_ids),
    evidence = "publisher_archive_no_per_game_timestamp", code_sha256 = as.list(setNames(code_before, basename(code)))),
    file.path(output, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  external_seal(output)
  list(output = output, summary = subset(metrics, slice == "common_lined"))
}
