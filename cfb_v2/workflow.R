v2_input_templates <- function() {
  list(
    schedule = data.frame(
      game_id = character(), season = integer(), week = integer(), kickoff = character(),
      home = character(), away = character(), neutral_site = logical(), venue = character(),
      home_level = character(), away_level = character(), postseason_type = character(),
      conference_championship = logical(), is_cfp = logical()
    ),
    lines = data.frame(
      game_id = character(), provider = character(), captured_at = character(),
      home_spread = numeric(), total = numeric(), home_price = integer(),
      away_price = integer(), source_url = character()
    ),
    injuries = data.frame(
      game_id = character(), team = character(), player_id = character(),
      player_name = character(), position = character(), status = character(),
      usage_share = numeric(), starter = logical(), impact_points = numeric(),
      availability_probability = numeric(),
      source_type = character(), source_url = character(), source_timestamp = character(),
      confidence = numeric(), conflict_flag = logical()
    ),
    coach_assignments = data.frame(
      team = character(), season = integer(), start_week = integer(), end_week = integer(),
      coach_id = character(), coach_name = character(), interim = logical(), source = character()
    ),
    coach_history = data.frame(
      coach_id = character(), season = integer(), week = integer(), games = integer(),
      wins = numeric(), above_expectation = numeric(), level = character(),
      context_strength = numeric(), target_context_strength = numeric(),
      playoff_appearances = integer(), titles = integer()
    ),
    coach_aliases = data.frame(
      source_name = character(), canonical_name = character(),
      coach_id = character(), notes = character()
    ),
    coach_assignment_overrides = data.frame(
      team = character(), season = integer(), start_week = integer(),
      end_week = integer(), coach_id = character(), coach_name = character(),
      interim = logical(), source = character(), assignment_confidence = character(),
      source_games = integer(), schedule_games = integer(), needs_review = logical(),
      inference_reason = character(), effective_date = character(), source_url = character()
    ),
    team_week_features = data.frame(
      team = character(), season = integer(), week = integer(), offense_rating = numeric(),
      defense_rating = numeric(), special_teams_rating = numeric(), recent_3 = numeric(),
      recent_6 = numeric(), season_to_date = numeric(), prior_season = numeric(),
      trailing_3yr = numeric(), preseason_prior = numeric(), qb_continuity = numeric(),
      roster_continuity = numeric(), staff_continuity = numeric(), coach_rating = numeric(),
      home_field_rating = numeric(), source_games = integer(), games_played = integer()
    ),
    training_games = data.frame(
      game_id = character(), season = integer(), week = integer(), game_phase = character(),
      margin = numeric(), total_points = numeric(), home_level = character(),
      away_level = character(), postseason_type = character(), is_cfp = logical(),
      conference_championship = logical(), neutral_site = logical(),
      offense_rating_diff = numeric(), defense_rating_diff = numeric(),
      special_teams_rating_diff = numeric(), recent_3_diff = numeric(),
      recent_6_diff = numeric(), season_to_date_diff = numeric(),
      prior_season_diff = numeric(), trailing_3yr_diff = numeric(),
      preseason_prior_diff = numeric(), qb_continuity_diff = numeric(),
      roster_continuity_diff = numeric(), staff_continuity_diff = numeric(),
      coach_rating_diff = numeric(), coach_rating_65_35_diff = numeric(),
      coach_rating_70_30_diff = numeric(), home_field_points = numeric(),
      closing_home_spread = numeric()
    ),
    membership = data.frame(
      team = character(), season = integer(), subdivision = character(),
      power_conference = logical(), conference = character()
    ),
    rankings = data.frame(
      team = character(), season = integer(), week = integer(), ranking_type = character(),
      rank = integer(), captured_at = character()
    ),
    cfp_probabilities = data.frame(
      team = character(), season = integer(), week = integer(), cfp_probability = numeric(),
      conference_leader = logical(), captured_at = character()
    ),
    public_rating_challengers = data.frame(
      team = character(), season = integer(), week = integer(), provider = character(),
      rating = numeric(), captured_at = character()
    )
  )
}

initialize_v2_project <- function(config) {
  ensure_v2_directories(config)
  templates <- v2_input_templates()
  for (name in names(templates)) {
    path <- file.path(config$inbox_dir, paste0(name, ".csv"))
    if (!file.exists(path)) utils::write.csv(templates[[name]], path, row.names = FALSE, na = "")
  }
  con <- v2_connect(config)
  on.exit(v2_disconnect(con), add = TRUE)
  v2_init_schema(con)
  invisible(names(templates))
}

read_v2_inputs <- function(config, strict = TRUE) {
  template_names <- names(v2_input_templates())
  inputs <- setNames(lapply(template_names, function(name) {
    read_csv_if_present(file.path(config$inbox_dir, paste0(name, ".csv")), required = FALSE)
  }), template_names)
  required <- c("schedule", "lines", "coach_assignments", "coach_history",
                "team_week_features", "training_games", "membership")
  empty <- required[vapply(inputs[required], function(x) is.null(x) || !nrow(x), logical(1))]
  if (strict && length(empty)) {
    stop("Production run cannot continue. Populate these v2 inbox files: ",
         paste(paste0(empty, ".csv"), collapse = ", "), call. = FALSE)
  }
  inputs
}

attach_coach_ratings <- function(schedule, team_features, assignments, history,
                                 recent_share, config) {
  mapped <- map_coaches_as_of(schedule, assignments)
  team_features$team <- canonical_team(team_features$team)
  if (any(is.na(c(mapped$home_coach_id, mapped$away_coach_id)))) {
    missing_games <- mapped$game_id[
      is.na(mapped$home_coach_id) | is.na(mapped$away_coach_id)
    ]
    stop("Missing week-effective coach assignment for games: ",
         paste(missing_games, collapse = ", "), call. = FALSE)
  }
  assert_unique_keys(team_features, c("team", "season", "week"), "team_week_features")
  rating_rows <- list()
  combinations <- unique(schedule[c("season", "week")])
  for (i in seq_len(nrow(combinations))) {
    rating_rows[[i]] <- build_coach_ratings(
      history, combinations$season[i], combinations$week[i], recent_share, config
    )
  }
  ratings <- do.call(rbind, rating_rows)
  if (!nrow(ratings)) stop("No leakage-safe coach ratings were available.", call. = FALSE)

  home_key <- data.frame(
    team = mapped$home, season = mapped$season, week = mapped$week,
    coach_id = mapped$home_coach_id, stringsAsFactors = FALSE
  )
  away_key <- data.frame(
    team = mapped$away, season = mapped$season, week = mapped$week,
    coach_id = mapped$away_coach_id, stringsAsFactors = FALSE
  )
  key <- unique(rbind(home_key, away_key))
  key <- merge(key, ratings[c("coach_id", "season", "as_of_week", "rating")],
               by.x = c("coach_id", "season", "week"),
               by.y = c("coach_id", "season", "as_of_week"), all.x = TRUE)
  key <- key[c("team", "season", "week", "rating")]
  names(key)[4] <- "computed_coach_rating"
  out <- merge(team_features, key, by = c("team", "season", "week"), all.x = TRUE,
               sort = FALSE)
  if (any(!is.finite(out$computed_coach_rating))) {
    missing <- unique(out$team[!is.finite(out$computed_coach_rating)])
    stop("No leakage-safe coach history was available for: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  out$coach_rating <- out$computed_coach_rating
  out$computed_coach_rating <- NULL
  out
}

build_coach_snapshot <- function(schedule, history, config) {
  combinations <- unique(schedule[c("season", "week")])
  rows <- lapply(seq_len(nrow(combinations)), function(i) {
    comparison <- compare_coach_weight_splits(
      history, combinations$season[i], combinations$week[i], config
    )
    if (!nrow(comparison)) return(NULL)
    data.frame(
      coach_id = comparison$coach_id,
      season = comparison$season,
      as_of_week = comparison$as_of_week,
      recent_above_expectation = comparison$recent_above_expectation,
      historical_win_value = comparison$historical_win_value,
      experience_value = comparison$experience_value,
      portability = comparison$portability,
      current_season_value = comparison$current_season_value,
      rating_65_35 = comparison$rating_65_35,
      rating_70_30 = comparison$rating_70_30,
      games_available = comparison$games_available,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

prepare_matchup_features <- function(schedule, team_features, assignments, history,
                                     recent_share, config) {
  team_features <- attach_coach_ratings(
    schedule, team_features, assignments, history, recent_share, config
  )
  required <- c("team", "season", "week", "recent_3", "recent_6",
                "season_to_date", "prior_season", "trailing_3yr", "preseason_prior")
  assert_columns(team_features, required, "team_week_features")
  if (!"games_played" %in% names(team_features)) {
    team_features$games_played <- pmax(0, team_features$source_games)
  }
  team_features$blended_team_form <- blend_team_form(team_features, config)
  make_matchup_features(schedule, team_features, team_features, recent_share, config)
}

prepare_training_data <- function(training_games, config) {
  assert_columns(training_games, c("season", "week", "margin"), "training_games")
  training_games$season <- as.integer(training_games$season)
  training_games$week <- as.integer(training_games$week)
  if (!"game_phase" %in% names(training_games)) {
    training_games$game_phase <- game_phase(training_games$week)
  }
  schedule_columns <- c("game_id", "season", "week", "home", "away", "neutral_site",
                        "home_level", "away_level", "postseason_type",
                        "conference_championship", "is_cfp")
  missing <- setdiff(schedule_columns, names(training_games))
  defaults <- list(game_id = seq_len(nrow(training_games)), home = "Home", away = "Away",
                   neutral_site = FALSE, home_level = "fbs", away_level = "fbs",
                   postseason_type = "regular", conference_championship = FALSE, is_cfp = FALSE)
  for (column in missing) training_games[[column]] <- defaults[[column]]
  weights <- training_game_weights(training_games, max(training_games$season) + 1L, config)
  list(data = training_games[weights > 0, , drop = FALSE], weights = weights[weights > 0])
}

select_coach_split_validation <- function(data, weights, config,
                                          fit_nonlinear = TRUE,
                                          fold_features = NULL) {
  columns <- c(`0.65` = "coach_rating_65_35_diff",
               `0.70` = "coach_rating_70_30_diff")
  available <- columns[columns %in% names(data)]
  if (length(available) < 2L) {
    rolling <- rolling_validate_ensemble(data, config = config, weights = weights,
                                         fit_nonlinear = fit_nonlinear,
                                         fold_features = fold_features)
    return(list(data = data, recent_share = 0.65, rolling = rolling,
                features = football_feature_names(data),
                comparison = data.frame(recent_share = .65,
                                        margin_mae = min(rolling$scores$absolute_error))))
  }

  base_features <- setdiff(football_feature_names(data),
                           c(unname(columns), "coach_rating_diff"))
  candidates <- lapply(names(available), function(share) {
    variant <- data
    variant$coach_rating_diff <- variant[[available[[share]]]]
    features <- c(base_features, "coach_rating_diff")
    rolling <- rolling_validate_ensemble(
      variant, features = features, weights = weights, config = config,
      fit_nonlinear = fit_nonlinear, fold_features = fold_features
    )
    list(share = as.numeric(share), data = variant, rolling = rolling,
         mae = min(rolling$scores$absolute_error))
  })
  maes <- vapply(candidates, `[[`, numeric(1), "mae")
  best_index <- which.min(maes)
  default_index <- which(vapply(candidates, `[[`, numeric(1), "share") == 0.65)
  if (length(default_index) && maes[default_index] <= maes[best_index] + 0.01) {
    best_index <- default_index
  }
  best <- candidates[[best_index]]
  list(
    data = best$data, recent_share = best$share, rolling = best$rolling,
    features = c(base_features, "coach_rating_diff"),
    comparison = do.call(rbind, lapply(candidates, function(x) {
      data.frame(recent_share = x$share, margin_mae = x$mae)
    }))
  )
}

new_coach_team_seasons <- function(data) {
  sides <- rbind(
    data.frame(team = data$home, season = data$season, model_week = data$model_week,
               coach_id = data$home_coach_id, stringsAsFactors = FALSE),
    data.frame(team = data$away, season = data$season, model_week = data$model_week,
               coach_id = data$away_coach_id, stringsAsFactors = FALSE)
  )
  sides <- sides[!is.na(sides$coach_id) & nzchar(sides$coach_id), ]
  sides <- sides[order(sides$team, sides$season, sides$model_week), ]
  primary <- sides[!duplicated(paste(sides$team, sides$season, sep = "\r")), ]
  key <- paste(primary$team, primary$season, sep = "\r")
  prior <- match(paste(primary$team, primary$season - 1L, sep = "\r"), key)
  changed <- !is.na(prior) & primary$coach_id != primary$coach_id[prior]
  key[changed]
}

build_v2_backtest_predictions <- function(data, rolling, model_name) {
  prediction <- rolling$predictions
  rows <- data[prediction$row_id, , drop = FALSE]
  value <- function(name, default = NA) {
    if (name %in% names(rows)) rows[[name]] else rep(default, nrow(rows))
  }
  new_coach <- new_coach_team_seasons(data)
  closing <- as.numeric(value("closing_home_spread", NA_real_))
  actual <- prediction$actual
  expected <- prediction$expected_margin
  actual_cover_margin <- actual + closing
  model_edge <- expected + closing
  ats_valid <- is.finite(actual_cover_margin) & actual_cover_margin != 0 &
    is.finite(model_edge) & model_edge != 0
  ats_correct <- rep(NA, nrow(rows))
  ats_correct[ats_valid] <- (model_edge[ats_valid] > 0) ==
    (actual_cover_margin[ats_valid] > 0)

  data.frame(
    model = model_name, game_id = as.character(value("game_id")),
    season = rows$season, week = rows$week, game_phase = rows$game_phase,
    home = value("home", ""), away = value("away", ""),
    home_level = value("home_level", ""), away_level = value("away_level", ""),
    home_conference = value("home_conference", ""),
    away_conference = value("away_conference", ""),
    postseason_type = value("postseason_type", "regular"),
    is_cfp = as.logical(value("is_cfp", FALSE)),
    neutral_site = as.logical(value("neutral_site", FALSE)),
    new_coach_game = paste(value("home", ""), rows$season, sep = "\r") %in% new_coach |
      paste(value("away", ""), rows$season, sep = "\r") %in% new_coach,
    actual_margin = actual, expected_margin = expected,
    fair_margin = prediction$fair_margin, margin_sd = prediction$margin_sd,
    absolute_error = abs(actual - expected),
    winner_correct = sign(actual) == sign(expected),
    closing_home_spread = closing, model_edge = model_edge,
    ats_correct = ats_correct,
    market_absolute_error = abs(actual + closing),
    market_winner_correct = sign(actual) == sign(-closing),
    line_source = value("line_source", ""),
    lambda = prediction$lambda, stringsAsFactors = FALSE
  )
}

p4_or_independent_sides <- function(predictions) {
  p4 <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12")
  independent <- c("Notre Dame", "Connecticut", "UConn")
  list(
    home = predictions$home_conference %in% p4 | predictions$home %in% independent,
    away = predictions$away_conference %in% p4 | predictions$away %in% independent
  )
}

backtest_slice_metrics <- function(predictions, slices) {
  metric <- function(model, slice, keep) {
    keep[is.na(keep)] <- FALSE
    eligible <- keep & predictions$model == model & is.finite(predictions$actual_margin) &
      is.finite(predictions$expected_margin)
    ats <- eligible & !is.na(predictions$ats_correct)
    market <- eligible & is.finite(predictions$market_absolute_error)
    data.frame(
      model = model, slice = slice, games = sum(eligible),
      margin_mae = if (any(eligible)) mean(predictions$absolute_error[eligible]) else NA,
      winner_accuracy = if (any(eligible)) mean(predictions$winner_correct[eligible]) else NA,
      ats_graded = sum(ats),
      ats_accuracy = if (any(ats)) mean(predictions$ats_correct[ats]) else NA,
      market_margin_mae = if (any(market))
        mean(predictions$market_absolute_error[market]) else NA,
      uncertainty_coverage = if (any(eligible)) mean(
        predictions$absolute_error[eligible] <= predictions$margin_sd[eligible], na.rm = TRUE
      ) else NA,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, lapply(unique(predictions$model), function(model) {
    do.call(rbind, lapply(names(slices), function(slice) {
      metric(model, slice, slices[[slice]])
    }))
  }))
}

summarize_v2_backtest <- function(predictions) {
  sides <- p4_or_independent_sides(predictions)
  home_p4 <- sides$home
  away_p4 <- sides$away
  teams <- paste(predictions$home, predictions$away)
  slices <- list(
    all_weighted_games = rep(TRUE, nrow(predictions)),
    fbs_vs_fbs = predictions$home_level == "fbs" & predictions$away_level == "fbs",
    p4_or_independent = home_p4 | away_p4,
    p4_vs_fbs = (home_p4 | away_p4) & predictions$home_level == "fbs" &
      predictions$away_level == "fbs",
    fbs_vs_fcs = xor(predictions$home_level == "fbs", predictions$away_level == "fbs"),
    g5_vs_p4 = xor(home_p4, away_p4) & predictions$home_level == "fbs" &
      predictions$away_level == "fbs",
    preseason = predictions$game_phase == "preseason",
    early_season = predictions$game_phase == "early_season",
    in_season = predictions$game_phase == "in_season",
    postseason = predictions$game_phase == "postseason",
    cfp = predictions$is_cfp,
    neutral_site = predictions$neutral_site,
    new_coach = predictions$new_coach_game,
    alabama_clemson = grepl("Alabama|Clemson", teams),
    indiana_smu = grepl("Indiana|SMU", teams),
    market_spread_21_plus = is.finite(predictions$closing_home_spread) &
      abs(predictions$closing_home_spread) > 21,
    ats_edge_2_plus = is.finite(predictions$model_edge) & abs(predictions$model_edge) >= 2,
    ats_edge_3_plus = is.finite(predictions$model_edge) & abs(predictions$model_edge) >= 3,
    ats_edge_4_plus = is.finite(predictions$model_edge) & abs(predictions$model_edge) >= 4
  )
  for (season in sort(unique(predictions$season))) {
    slices[[paste0("season_", season)]] <- predictions$season == season
    slices[[paste0("cfp_", season)]] <- predictions$season == season & predictions$is_cfp
  }
  market_bin <- margin_bucket(predictions$closing_home_spread)
  model_bin <- margin_bucket(predictions$expected_margin)
  for (bin in levels(market_bin)) {
    label <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(bin)))
    slices[[paste0("market_bin_", label)]] <- market_bin == bin
    slices[[paste0("model_bin_", label)]] <- model_bin == bin
  }

  backtest_slice_metrics(predictions, slices)
}

markdown_table <- function(data) {
  formatted <- data
  formatted[] <- lapply(formatted, function(x) {
    if (is.numeric(x)) ifelse(is.na(x), "", format(round(x, 3), trim = TRUE))
    else gsub("\\|", "\\\\|", as.character(x))
  })
  c(
    paste0("| ", paste(names(formatted), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(formatted)), collapse = " | "), " |"),
    apply(formatted, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  )
}

write_v2_backtest_report <- function(summary, predictions, coach_comparison, path,
                                     training_path) {
  format_metrics <- function(data) {
    data$winner_accuracy <- 100 * data$winner_accuracy
    data$ats_accuracy <- 100 * data$ats_accuracy
    names(data)[names(data) == "winner_accuracy"] <- "winner_pct"
    names(data)[names(data) == "ats_accuracy"] <- "ats_pct"
    data
  }
  metric_columns <- c("model", "slice", "games", "margin_mae", "winner_accuracy",
                      "ats_graded", "ats_accuracy", "market_margin_mae")
  focus <- c("all_weighted_games", "fbs_vs_fbs", "p4_or_independent", "p4_vs_fbs",
             "fbs_vs_fcs", "preseason",
             "postseason", "cfp", "new_coach", "alabama_clemson", "indiana_smu",
             "market_spread_21_plus", "ats_edge_3_plus")
  display <- format_metrics(summary[summary$slice %in% focus, metric_columns])
  seasons <- format_metrics(summary[grepl("^season_", summary$slice), metric_columns])
  cfp_seasons <- format_metrics(summary[grepl("^cfp_[0-9]", summary$slice) &
                                          summary$games > 0, metric_columns])
  spreads <- format_metrics(summary[grepl("^(market|model)_bin_", summary$slice),
                                    metric_columns])
  edges <- format_metrics(summary[grepl("^ats_edge_", summary$slice), metric_columns])
  ranges <- aggregate(expected_margin ~ model, predictions, function(x) {
    paste0(round(min(x), 1), " to ", round(max(x), 1))
  })
  names(ranges)[2] <- "projected_margin_range"
  lines <- unique(predictions$line_source[nzchar(predictions$line_source)])
  report <- c(
    "# CFB v2 Cached Foundation Backtest",
    "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0("Training file MD5: `", unname(tools::md5sum(training_path)), "`"),
    "",
    "Rolling folds train only on earlier seasons. The ridge model is the production core;",
    "team-specific HFA and the residual forest are challengers. Margin MAE is the lead metric.",
    "ATS accuracy forces a side from the model edge and does not represent a published-pick threshold.",
    paste0("Cached historical line source: ",
           if (length(lines)) paste(lines, collapse = ", ") else "unknown", "."),
    "",
    "## Core Diagnostics",
    "",
    markdown_table(display),
    "",
    "## Prediction Range",
    "",
    markdown_table(ranges),
    "",
    "## Season Results",
    "",
    markdown_table(seasons),
    "",
    "## CFP By Season",
    "",
    markdown_table(cfp_seasons),
    "",
    "## Spread Diagnostics",
    "",
    markdown_table(spreads),
    "",
    "## Forced ATS Edge Diagnostics",
    "",
    markdown_table(edges),
    "",
    "## Coach Split Screen",
    "",
    markdown_table(coach_comparison),
    "",
    "## Known Foundation Limits",
    "",
    "- Cached spreads have no provider timestamp, so they are market benchmarks rather than verified closes.",
    "- Historical QB, portal, and returning-production fields are unavailable and are not evaluated here.",
    "- Rankings and historical CFP probabilities are unavailable, so `p4_or_independent` approximates the article card."
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(report, path)
  path
}

write_preseason_challenger_report <- function(predictions, output_dir,
                                              challengers_available,
                                              coefficients = NULL) {
  report_path <- file.path(output_dir, "preseason_challenger_report.md")
  summary_path <- file.path(output_dir, "preseason_challenger_summary.csv")
  header <- c(
    "# CFB v2 Preseason Challenger Report (Weeks 0-1)",
    "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    paste0("The ridge core is the production model and includes the promoted ",
           "preseason features (returning production, retained quality, 247 talent ",
           "percentile, replacement capacity), which fade to zero from Week 5. ",
           "ATS accuracy forces a side from the model edge. `uncertainty_coverage` ",
           "is the share of games landing inside one modeled standard deviation."),
    "",
    paste0("`preseason_ablation_challenger` removes the promoted preseason features ",
           "and shows what they keep earning. `preseason_hype_challenger` adds the ",
           "diagnostic Week 1 poll features, which stay out of production by the ",
           "design contract; a `hype_gap` coefficient near zero means polls add ",
           "nothing the objective inputs did not already carry, and a negative one ",
           "means the model would fade poll hype."),
    "",
    paste0("Folds with fewer than two frozen prior seasons evaluate the ",
           "pre-promotion feature set, so the earliest test season matches the ",
           "ablation by construction."),
    ""
  )
  if (!challengers_available) {
    writeLines(c(
      header,
      "Preseason challengers were skipped: `cfb_v2/data/preseason_team_priors.csv`",
      "is empty or missing. Run `--mode=build-preseason --seasons=2021:2025",
      "--overwrite=true` first."
    ), report_path)
    return(list(report = report_path, summary = NULL))
  }
  pre <- predictions[predictions$game_phase == "preseason", , drop = FALSE]
  if (!nrow(pre)) {
    writeLines(c(header, "No Week 0/1 games were present in the rolling folds."),
               report_path)
    return(list(report = report_path, summary = NULL))
  }
  sides <- p4_or_independent_sides(pre)
  fbs_pair <- pre$home_level == "fbs" & pre$away_level == "fbs"
  slices <- list(
    all_week_0_1 = rep(TRUE, nrow(pre)),
    week_0 = pre$week == 0,
    week_1 = pre$week == 1,
    p4_vs_p4 = sides$home & sides$away & fbs_pair,
    p4_vs_g5 = xor(sides$home, sides$away) & fbs_pair,
    g5_vs_g5 = !sides$home & !sides$away & fbs_pair,
    fbs_vs_fcs = xor(pre$home_level == "fbs", pre$away_level == "fbs")
  )
  for (season in sort(unique(pre$season))) {
    slices[[paste0("season_", season)]] <- pre$season == season
  }
  summary <- backtest_slice_metrics(pre, slices)
  utils::write.csv(summary, summary_path, row.names = FALSE, na = "")
  display <- summary
  display$winner_accuracy <- 100 * display$winner_accuracy
  display$ats_accuracy <- 100 * display$ats_accuracy
  names(display)[names(display) == "winner_accuracy"] <- "winner_pct"
  names(display)[names(display) == "ats_accuracy"] <- "ats_pct"
  report <- c(header, "## Week 0/1 Diagnostics", "", markdown_table(display))
  if (!is.null(coefficients) && nrow(coefficients)) {
    report <- c(
      report, "",
      "## Learned Preseason Effects (points per standard deviation)",
      "",
      paste0("Descriptive full-sample ridge coefficients at each variant's selected ",
             "lambda. Direction and size only; the out-of-sample verdict is the ",
             "table above."),
      "",
      markdown_table(coefficients)
    )
  }
  writeLines(report, report_path)
  list(report = report_path, summary = summary_path)
}

run_v2_backtest <- function(config) {
  training_path <- file.path(config$inbox_dir, "training_games.csv")
  if (!file.exists(training_path)) stop("Missing cached training_games.csv.", call. = FALSE)
  training <- utils::read.csv(training_path, check.names = FALSE,
                              na.strings = c("", "NA"))
  validation <- prepare_training_data(training, config)
  priors <- read_csv_if_present(
    file.path(config$data_dir, "preseason_team_priors.csv"), required = FALSE
  )
  challengers_available <- !is.null(priors) && nrow(priors) > 0
  production_ps <- paste0("ps_", config$preseason$production_features, "_diff")
  poll_columns <- paste0("challenger_ps_", config$preseason$diagnostic_features,
                         "_diff")
  gate_for <- function(drop) NULL
  if (challengers_available) {
    covered_seasons <- sort(unique(as.integer(priors$season)))
    validation$data <- attach_preseason_features(
      validation$data, priors, config,
      features = config$preseason$production_features, prefix = "ps_"
    )
    # A fold learns the preseason features only from covered earlier seasons;
    # with fewer than two, the handful of faded rows cannot support the extra
    # collinear features and the fold evaluates the pre-promotion set instead.
    gate_for <- function(drop) {
      function(test_season, features) {
        if (sum(covered_seasons < test_season) >= 2L) features else
          setdiff(features, drop)
      }
    }
  }
  coach <- select_coach_split_validation(
    validation$data, validation$weights, config, fit_nonlinear = FALSE,
    fold_features = gate_for(production_ps)
  )
  ridge <- coach$rolling
  forest <- rolling_validate_ensemble(
    coach$data, features = coach$features, weights = validation$weights,
    config = config, fit_nonlinear = TRUE, fold_features = gate_for(production_ps)
  )
  prediction_sets <- list(
    build_v2_backtest_predictions(coach$data, ridge, "ridge_core"),
    build_v2_backtest_predictions(coach$data, forest, "residual_forest_challenger")
  )
  if ("challenger_team_home_field_points" %in% names(coach$data)) {
    hfa_data <- coach$data
    hfa_data$home_field_points <- hfa_data$challenger_team_home_field_points
    hfa <- rolling_validate_ensemble(
      hfa_data, features = coach$features, weights = validation$weights,
      config = config, fit_nonlinear = FALSE, fold_features = gate_for(production_ps)
    )
    prediction_sets <- append(
      prediction_sets,
      list(build_v2_backtest_predictions(hfa_data, hfa, "team_hfa_challenger")),
      after = 1L
    )
  }
  preseason_coefficients <- NULL
  if (challengers_available) {
    ablation <- rolling_validate_ensemble(
      coach$data, features = setdiff(coach$features, production_ps),
      weights = validation$weights, config = config, fit_nonlinear = FALSE
    )
    poll_data <- attach_preseason_features(
      coach$data, priors, config,
      features = config$preseason$diagnostic_features
    )
    hype <- rolling_validate_ensemble(
      poll_data, features = c(coach$features, poll_columns),
      weights = validation$weights, config = config, fit_nonlinear = FALSE,
      fold_features = gate_for(c(production_ps, poll_columns))
    )
    prediction_sets <- append(prediction_sets, list(
      build_v2_backtest_predictions(coach$data, ablation,
                                    "preseason_ablation_challenger"),
      build_v2_backtest_predictions(poll_data, hype, "preseason_hype_challenger")
    ))
    core_ridge <- fit_weighted_ridge(
      coach$data, "margin", coach$features, weights = validation$weights,
      lambda = ridge$best_lambda
    )
    hype_ridge <- fit_weighted_ridge(
      poll_data, "margin", c(coach$features, poll_columns),
      weights = validation$weights, lambda = hype$best_lambda
    )
    preseason_coefficients <- rbind(
      data.frame(model = "ridge_core", feature = production_ps,
                 points_per_sd = as.numeric(core_ridge$coefficients[production_ps]),
                 stringsAsFactors = FALSE),
      data.frame(model = "preseason_hype_challenger", feature = poll_columns,
                 points_per_sd = as.numeric(hype_ridge$coefficients[poll_columns]),
                 stringsAsFactors = FALSE)
    )
  }
  predictions <- do.call(rbind, prediction_sets)
  summary <- summarize_v2_backtest(predictions)
  coach_comparison <- coach$comparison
  coach_comparison$selected <- abs(coach_comparison$recent_share - coach$recent_share) <
    sqrt(.Machine$double.eps)
  output_dir <- file.path(config$output_dir, "backtest")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  predictions_path <- file.path(output_dir, "rolling_predictions.csv")
  summary_path <- file.path(output_dir, "summary.csv")
  coach_path <- file.path(output_dir, "coach_split_comparison.csv")
  report_path <- file.path(output_dir, "backtest_report.md")
  utils::write.csv(predictions, predictions_path, row.names = FALSE, na = "")
  utils::write.csv(summary, summary_path, row.names = FALSE, na = "")
  utils::write.csv(coach_comparison, coach_path, row.names = FALSE, na = "")
  write_v2_backtest_report(summary, predictions, coach_comparison, report_path,
                           training_path)
  preseason <- write_preseason_challenger_report(predictions, output_dir,
                                                 challengers_available,
                                                 preseason_coefficients)
  list(report = report_path, predictions = predictions_path, summary = summary_path,
       coach_comparison = coach_path, preseason_report = preseason$report,
       preseason_summary = preseason$summary)
}

validate_article_as_of <- function(as_of, config) {
  local <- as.POSIXlt(as.POSIXct(as_of), tz = config$article_timezone)
  weekday <- weekdays(as.Date(as_of, tz = config$article_timezone))
  if (weekday != config$article_freeze_weekday || local$hour < config$article_freeze_hour) {
    warning("Article snapshot is not at/after the configured Friday 1 PM Eastern freeze.",
            call. = FALSE)
  }
  invisible(TRUE)
}

write_prediction_outputs <- function(predictions, config, run_id, snapshot_type) {
  directory <- file.path(config$output_dir, paste0(config$season), paste0("week_", predictions$week[1]))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(directory, paste0(snapshot_type, "_", run_id, ".csv"))
  utils::write.csv(predictions, csv_path, row.names = FALSE, na = "")
  csv_path
}

run_v2_week <- function(config, season, week, mode = c("article", "live"),
                        as_of = Sys.time(), force_game_ids = character(), strict = TRUE) {
  mode <- match.arg(mode)
  config$season <- as.integer(season)
  if (mode == "article") validate_article_as_of(as_of, config)
  if (mode == "article") {
    slot_con <- v2_connect(config)
    on.exit(v2_disconnect(slot_con), add = TRUE)
    v2_init_schema(slot_con)
    v2_assert_article_slot_open(slot_con, season, week)
    v2_disconnect(slot_con)
    slot_con <- NULL
  }
  inputs <- read_v2_inputs(config, strict)
  schedule <- normalize_public_schedule(inputs$schedule)
  schedule <- schedule[schedule$season == season & schedule$week == week, , drop = FALSE]
  if (!nrow(schedule)) stop("No scheduled games match the requested season/week.", call. = FALSE)

  eligible <- weekly_game_eligibility(
    schedule, inputs$membership, inputs$rankings, inputs$cfp_probabilities, config
  )
  schedule <- schedule[eligible, , drop = FALSE]
  if (!nrow(schedule)) stop("No games passed the weekly card eligibility rules.", call. = FALSE)

  validation <- prepare_training_data(inputs$training_games, config)
  priors <- read_csv_if_present(
    file.path(config$data_dir, "preseason_team_priors.csv"), required = TRUE
  )
  if (is.null(priors) || !nrow(priors)) {
    stop("preseason_team_priors.csv has no rows. ",
         "Run --mode=build-preseason before production runs.", call. = FALSE)
  }
  validation$data <- attach_preseason_features(
    validation$data, priors, config,
    features = config$preseason$production_features, prefix = "ps_"
  )
  covered_priors_seasons <- sort(unique(as.integer(priors$season)))
  production_ps <- paste0("ps_", config$preseason$production_features, "_diff")
  preseason_gate <- function(test_season, features) {
    if (sum(covered_priors_seasons < test_season) >= 2L) features else
      setdiff(features, production_ps)
  }
  coach_selection <- select_coach_split_validation(
    validation$data, validation$weights, config, fit_nonlinear = FALSE,
    fold_features = preseason_gate
  )
  validation$data <- coach_selection$data
  rolling <- coach_selection$rolling
  model <- fit_cfb_ensemble(
    validation$data, features = coach_selection$features,
    weights = validation$weights, lambda = rolling$best_lambda, config = config,
    fit_nonlinear = FALSE
  )
  model$calibration <- fit_error_calibration(
    rolling$predictions$expected_margin, rolling$predictions$actual,
    validation$data$game_phase[rolling$predictions$row_id], config
  )
  ats_model <- NULL
  ats_threshold <- NULL
  if ("closing_home_spread" %in% names(validation$data)) {
    ats_rows <- rolling$predictions$row_id
    ats_eligible <- ats_training_eligible(validation$data[ats_rows, , drop = FALSE])
    ats_validation <- data.frame(
      actual_margin = rolling$predictions$actual[ats_eligible],
      expected_margin = rolling$predictions$expected_margin[ats_eligible],
      closing_home_spread = validation$data$closing_home_spread[ats_rows][ats_eligible],
      margin_sd = rolling$predictions$margin_sd[ats_eligible]
    )
    ats_model <- fit_ats_residual_model(ats_validation)
    cover_probability <- predict_ats_home_cover(
      ats_model, ats_validation$expected_margin, ats_validation$closing_home_spread,
      ats_validation$margin_sd
    )
    home_covered <- ats_validation$actual_margin + ats_validation$closing_home_spread > 0
    threshold_validation <- data.frame(
      edge = ats_validation$expected_margin + ats_validation$closing_home_spread,
      cover_probability = cover_probability,
      covered = ifelse(cover_probability >= .5, home_covered, !home_covered)
    )
    finite <- is.finite(threshold_validation$edge) &
      is.finite(threshold_validation$cover_probability)
    if (sum(finite) >= 20) {
      ats_threshold <- select_ats_threshold(threshold_validation[finite, ], config)
    }
  }
  matchup <- prepare_matchup_features(
    schedule, inputs$team_week_features, inputs$coach_assignments,
    inputs$coach_history, coach_selection$recent_share, config
  )
  matchup <- attach_preseason_features(
    matchup, priors, config,
    features = config$preseason$production_features, prefix = "ps_",
    require_coverage = TRUE
  )
  lines <- inputs$lines
  lines$captured_at <- as.POSIXct(lines$captured_at, tz = "UTC")
  selected_lines <- select_article_lines(lines, schedule$game_id, as_of)
  selected_lines <- selected_lines[match(schedule$game_id, selected_lines$game_id), ]

  injuries <- inputs$injuries
  predictions <- predict_week(
    model, schedule, matchup, selected_lines$market_home_spread,
    force_game_ids = force_game_ids, injuries = injuries,
    ats_threshold = ats_threshold, ats_model = ats_model, config = config
  )
  predictions$expected_total <- NA_real_
  predictions$total_sd <- NA_real_
  for (column in setdiff(names(selected_lines), "game_id")) {
    predictions[[column]] <- selected_lines[[column]]
  }
  predictions$as_of <- as.POSIXct(as_of, tz = "UTC")
  predictions$snapshot_type <- mode
  predictions$model_version <- config$version
  predictions$published <- mode == "article"

  con <- v2_connect(config)
  on.exit(v2_disconnect(con), add = TRUE)
  v2_init_schema(con)
  run_id <- v2_run_id(mode, season, week, as_of)
  v2_register_run(con, run_id, mode, season, week, as_of, config)
  tryCatch({
    schedule_store <- schedule
    schedule_store$source <- "weekly_input"
    schedule_store$captured_at <- as.POSIXct(as_of, tz = "UTC")
    schedule_store$home_score <- NULL
    schedule_store$away_score <- NULL
    v2_append_snapshot(con, "game_schedule", schedule_store, run_id, "game_id")

    assignment_store <- inputs$coach_assignments[
      inputs$coach_assignments$season <= season, , drop = FALSE
    ]
    v2_append_snapshot(con, "coach_assignments", assignment_store, run_id,
                       c("team", "season", "start_week"))
    coach_store <- build_coach_snapshot(schedule, inputs$coach_history, config)
    if (nrow(coach_store)) {
      v2_append_snapshot(con, "coach_ratings", coach_store, run_id,
                         c("coach_id", "season", "as_of_week"))
    }

    feature_store <- inputs$team_week_features[
      inputs$team_week_features$season == season &
        inputs$team_week_features$week == week, , drop = FALSE
    ]
    if (nrow(feature_store)) {
      feature_store$as_of <- as.POSIXct(as_of, tz = "UTC")
      feature_store$games_played <- NULL
      v2_append_snapshot(con, "team_week_features", feature_store, run_id,
                         c("team", "season", "week"))
    }

    challenger_store <- inputs$public_rating_challengers
    if (!is.null(challenger_store) && nrow(challenger_store)) {
      challenger_store <- challenger_store[
        challenger_store$season == season & challenger_store$week <= week, , drop = FALSE
      ]
      challenger_store$captured_at <- as.POSIXct(challenger_store$captured_at, tz = "UTC")
      v2_append_snapshot(con, "public_rating_challengers", challenger_store, run_id,
                         c("team", "season", "week", "provider"))
    }

    line_store <- lines[lines$game_id %in% schedule$game_id &
                          lines$captured_at <= as.POSIXct(as_of, tz = "UTC"), , drop = FALSE]
    if (nrow(line_store)) {
      if (!"snapshot_type" %in% names(line_store)) line_store$snapshot_type <- mode
      v2_append_snapshot(con, "line_snapshots", line_store, run_id,
                         c("game_id", "provider", "captured_at"))
    }
    if (!is.null(injuries) && nrow(injuries)) {
      injury_store <- injuries[injuries$game_id %in% schedule$game_id, , drop = FALSE]
      if (nrow(injury_store)) {
        injury_store$captured_at <- as.POSIXct(as_of, tz = "UTC")
        injury_store$source_timestamp <- as.POSIXct(injury_store$source_timestamp, tz = "UTC")
        v2_append_snapshot(con, "injury_snapshots", injury_store, run_id,
                           c("game_id", "team", "player_id", "source_timestamp"))
      }
    }

    prediction_store <- predictions
    prediction_store$run_id <- run_id
    prediction_store$created_at <- as.POSIXct(Sys.time(), tz = "UTC")
    prediction_store$base_margin <- NULL
    prediction_store$nonlinear_adjustment <- NULL
    prediction_store$home_field_points <- NULL
    prediction_store$line_source <- NULL
    prediction_store$market_total <- NULL
    prediction_store$opening_home_spread <- NULL
    prediction_store$draftkings_home_spread <- NULL
    prediction_store$fanduel_home_spread <- NULL
    prediction_store$best_home_spread <- NULL
    prediction_store$best_away_spread <- NULL
    v2_append_snapshot(con, "prediction_snapshots", prediction_store, run_id, "game_id")
    output <- write_prediction_outputs(predictions, config, run_id, mode)
    parquet <- sub("\\.csv$", ".parquet", output)
    v2_export_snapshot(con, "prediction_snapshots", run_id, parquet)
    v2_finish_run(con, run_id, "complete", output)
    list(run_id = run_id, predictions = predictions, csv = output, parquet = parquet,
         validation = rolling$scores, coach_split = coach_selection$comparison)
  }, error = function(e) {
    v2_finish_run(con, run_id, "failed", conditionMessage(e))
    stop(e)
  })
}
