week3_efficiency_metrics <- function() {
  c("offense_epa", "defense_epa", "special_teams_rating", "pass_epa", "rush_epa",
    "success_rate", "havoc_allowed", "havoc_generated", "turnover_rate_regressed")
}

week3_efficiency_builder <- function(prior_weight) {
  if (length(prior_weight) != 1L || !is.finite(prior_weight) ||
      !prior_weight %in% c(.3, .5)) stop("Only the locked 0.3/0.5 weights are allowed.")
  production_blend <- blend_current_with_history
  builder <- build_team_pregame_snapshots
  scope <- new.env(parent = environment(builder))
  # Override only the metric blend inside a private copy of the existing builder.
  scope$blend_current_with_history <- function(current, history, week, config) {
    if (isTRUE(week == 3L)) config$phase$preseason_by_week["3"] <- prior_weight
    production_blend(current, history, week, config)
  }
  environment(builder) <- scope
  builder
}

week3_assert_feature_scope <- function(base, candidate) {
  if (!identical(names(base), names(candidate)) || !identical(base$game_id, candidate$game_id))
    stop("Feature schema or game identity changed.")
  allowed <- c(paste0("home_", week3_efficiency_metrics()),
    paste0("away_", week3_efficiency_metrics()), paste0(week3_efficiency_metrics(), "_diff"))
  changed <- names(base)[!vapply(names(base), function(f)
    identical(base[[f]], candidate[[f]]), logical(1))]
  if (length(setdiff(changed, allowed))) stop("Non-efficiency fields changed.")
  active <- !is.na(base$postseason_type) & base$postseason_type == "regular" & base$week == 3L
  active[is.na(active)] <- FALSE
  for (f in changed) if (!identical(base[[f]][!active], candidate[[f]][!active]))
    stop("Efficiency changed outside regular Week 3.")
  invisible(changed)
}

week3_sample_group <- function(home, away) {
  if (any(!is.finite(home) | !is.finite(away) | home < 0 | away < 0 |
          home != floor(home) | away != floor(away))) stop("Unknown or invalid sample counts.")
  n <- pmin(home, away)
  ifelse(n == 0, "zero", ifelse(n == 1, "one", ifelse(n == 2, "two", "three_plus")))
}

week3_paired_summary <- function(base, candidate, slice) {
  stopifnot(identical(base$game_id, candidate$game_id),
    identical(base$actual_margin, candidate$actual_margin),
    identical(base$closing_home_spread, candidate$closing_home_spread))
  delta <- candidate$mae_error - base$mae_error
  ats <- candidate$ats_correct - base$ats_correct
  ci <- four_way_intervals(delta, base$season, seed = 20260913L)
  ai <- four_way_intervals(ats, base$season, seed = 20260913L)
  data.frame(slice = slice, games = nrow(base), paired_ats_decisions = sum(is.finite(ats)),
    mae_change = mean(delta), mae_low = ci["low"], mae_high = ci["high"],
    ats_change = if (any(is.finite(ats))) mean(ats, na.rm = TRUE) else NA_real_,
    ats_low = ai["low"], ats_high = ai["high"],
    side_changes = sum(base$selected & candidate$selected & sign(base$edge) != sign(candidate$edge)),
    wins_to_losses = sum(base$win & candidate$loss), losses_to_wins = sum(base$loss & candidate$win),
    row.names = NULL)
}

week3_score_pair <- function(reference, forecasts, data) {
  assert_unique_keys(reference, "game_id", "Week 3 evaluation cohort")
  i <- match(reference$game_id, data$game_id)
  if (anyNA(i)) stop("Evaluation games are missing feature inputs.")
  for (field in c("home", "away", "season"))
    if (!isTRUE(all.equal(reference[[field]], data[[field]][i], check.attributes = FALSE)))
      stop("Evaluation identity mismatch.")
  stopifnot(isTRUE(all.equal(reference$actual_margin, data$margin[i], check.attributes = FALSE)))
  reference$home_source_games <- data$home_source_games[i]
  reference$away_source_games <- data$away_source_games[i]
  reference$sample_group <- week3_sample_group(reference$home_source_games, reference$away_source_games)
  if ("margin_sd" %in% names(reference))
    names(reference)[names(reference) == "margin_sd"] <- "reference_margin_sd"
  parts <- lapply(names(forecasts), function(name) {
    p <- forecasts[[name]]
    assert_unique_keys(p, "game_id", "Week 3 forecasts")
    x <- reference
    x$expected_margin <- p$expected_margin[match(x$game_id, p$game_id)]
    if (any(!is.finite(x$expected_margin))) stop("Missing evaluation forecast.")
    x$model <- name
    x <- experiment_grade(x)
    # Grade from the selected team's score plus its handicap, independently of edge_grade.
    home <- x$expected_margin > -x$closing_home_spread
    selected_margin <- ifelse(home, x$actual_margin + x$closing_home_spread,
      -x$actual_margin - x$closing_home_spread)
    if (any(x$win[x$selected] != (selected_margin[x$selected] > 0)) ||
        any(x$loss[x$selected] != (selected_margin[x$selected] < 0)) ||
        any(x$push[x$selected] != (selected_margin[x$selected] == 0))) stop("Grade mismatch.")
    x
  })
  do.call(rbind, parts)
}

week3_summaries <- function(scored) {
  base <- scored[scored$model == "v3_30", ]; candidate <- scored[scored$model == "efficiency_50", ]
  candidate <- candidate[match(base$game_id, candidate$game_id), ]
  active <- base$postseason_type == "regular" & base$week == 3L
  sides <- p4_or_independent_sides(base)
  slices <- list(week_3 = active, all_fbs = rep(TRUE, nrow(base)), other_weeks = !active,
    week_3_large_spread = active & abs(base$closing_home_spread) > 21,
    week_3_article_audience = active & (sides$home | sides$away))
  for (group in c("zero", "one", "two", "three_plus"))
    slices[[paste0("week_3_min_sample_", group)]] <- active & base$sample_group == group
  for (season in sort(unique(base$season)))
    slices[[paste0("week_3_", season)]] <- active & base$season == season
  metrics <- paired <- omissions <- list()
  for (name in names(slices)) {
    keep <- which(!is.na(slices[[name]]) & slices[[name]])
    a <- base[keep, ]; b <- candidate[keep, ]
    if (length(keep)) {
      m <- four_way_metrics(rbind(a, b)); m <- m[m$slice == "all_fbs", ]
    } else {
      m <- four_way_metrics(rbind(base, candidate))
      m <- m[m$slice == "all_fbs", ]
      m[c("games", "lined_games", "wins", "losses", "pushes", "no_selection", "su_wins")] <- 0
      m[c("ats_accuracy", "su_accuracy", "margin_mae", "margin_rmse", "margin_bias", "mean_absolute_prediction")] <- NA_real_
    }
    m$slice <- name; metrics[[name]] <- m
    paired[[name]] <- week3_paired_summary(a, b, name)
  }
  for (season in sort(unique(base$season[active]))) {
    keep <- which(active & base$season != season)
    row <- week3_paired_summary(base[keep, ], candidate[keep, ], "week_3")
    row$omitted_season <- season; omissions[[as.character(season)]] <- row
  }
  list(metrics = do.call(rbind, metrics), paired = do.call(rbind, paired),
    omissions = do.call(rbind, omissions))
}

run_week3_efficiency <- function(config) {
  stopifnot(config$version == "3.0.0", preseason_feature_weight(3, config) == .3)
  project <- config$project_dir
  root <- file.path(project, "cfb_v3/experiments")
  prior_root <- file.path(config$output_dir, "experiments")
  four_way <- file.path(prior_root, "four_way/20260909T214828Z")
  qualified <- file.path(prior_root, "matchup_prepared/20260912T015157.387Z")
  schema_path <- file.path(prior_root, "prediction_tracker/20260912T003231.716Z/feature_manifest.csv")
  source_paths <- c(file.path(config$data_dir, c("historical_games.csv", "historical_team_games.csv",
    "historical_fbs_membership.csv", "preseason_team_priors.csv")),
    file.path(config$inbox_dir, "training_games.csv"), schema_path,
    file.path(four_way, c("predictions.csv", "fold_choices.csv")),
    file.path(qualified, "model_inputs.rds"), file.path(config$output_dir, "backtest/rolling_predictions.csv"))
  code_paths <- c(file.path(root, c("week3_efficiency.R", "WEEK3_EFFICIENCY_PROTOCOL.md",
    "controlled_ats.R", "four_way.R", "matchup_foundation_audit.R", "external_ratings.R")),
    file.path(project, c("run_cfb_week3_efficiency.R", "cfb_v2/tests/test_week3_efficiency.R")))
  if (!all(file.exists(c(source_paths, code_paths)))) stop("Retained local foundation/research inputs required.")
  protected <- experiment_file_hashes(config)
  inventory <- function(paths) data.frame(path = paths, md5 = unname(tools::md5sum(paths)))
  inputs <- inventory(source_paths); code <- inventory(code_paths)
  external_verify(qualified)
  output <- external_new_dir(file.path(prior_root, "week3_efficiency"), "")
  message("Week 3 efficiency output: ", output)
  file.copy(file.path(root, "WEEK3_EFFICIENCY_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  write.csv(inputs, file.path(output, "input_hashes.csv"), row.names = FALSE)
  write.csv(code, file.path(output, "code_hashes.csv"), row.names = FALSE)
  write.csv(protected, file.path(output, "protected_before.csv"), row.names = FALSE)
  saveRDS(config, file.path(output, "config.rds"))

  training <- read_csv_if_present(file.path(config$inbox_dir, "training_games.csv"), TRUE)
  training <- training[training$season <= 2025, ]
  stopifnot(all(training$feature_version == config$version))
  input <- prepare_training_data(training, config)
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  games <- games[games$season <= 2025, ]; games$kickoff <- parse_utc_datetime(games$kickoff)
  tg <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  tg <- tg[tg$season <= 2025, ]; tg$kickoff <- parse_utc_datetime(tg$kickoff)
  assert_unique_keys(games, "game_id", "historical games")
  assert_unique_keys(tg, c("game_id", "team"), "historical team-game cells")
  members <- read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE)
  members <- members[members$season <= 2025, ]
  flags <- combine_membership(members)
  input$data <- apply_fbs_bridge_backtest_features(input$data, tg, flags, config)
  priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
  priors <- priors[priors$season <= 2025, ]
  input$data <- attach_preseason_features(input$data, priors, config,
    features = config$preseason$production_features, prefix = "ps_")
  input$data$coach_rating_diff <- input$data$coach_rating_65_35_diff
  input$covered_seasons <- sort(unique(priors$season))
  fields <- read.csv(schema_path, stringsAsFactors = FALSE)
  fields <- fields$feature[fields$model == "football_matched_control"]
  choices <- read.csv(file.path(four_way, "fold_choices.csv"), stringsAsFactors = FALSE)
  choices <- choices[choices$model == "v3_control", ]
  stopifnot(identical(sort(choices$test_season), 2022:2025),
    all(choices$train_through_season < choices$test_season), all(input$data$season <= 2025))
  baseline <- foundation_fixed_forecasts(input, choices, fields, config)
  frozen <- read_csv_if_present(file.path(config$output_dir, "backtest/rolling_predictions.csv"), TRUE)
  frozen <- frozen[frozen$model == "ridge_core", ]
  stopifnot(nrow(frozen) == nrow(baseline))
  parity <- foundation_compare(frozen, baseline, "game_id", "expected_margin", 1e-8)
  if (any(parity$changed + parity$missing_mismatch > 0)) stop("Frozen v3 replay failed.")
  write.csv(parity, file.path(output, "baseline_forecast_parity.csv"), row.names = FALSE)

  power <- unique(rbind(data.frame(team = games$home, season = games$season,
    model_week = games$model_week, power_rating = games$home_power),
    data.frame(team = games$away, season = games$season,
    model_week = games$model_week, power_rating = games$away_power)))
  assert_unique_keys(power, c("team", "season", "model_week"), "frozen power")
  bridge <- fbs_bridge_prior_rows(tg, membership_fbs_flags(members), config)
  rebuilt <- lapply(c(.3, .5), function(weight) {
    message("Rebuilding historical efficiency at prior weight ", weight)
    built <- week3_efficiency_builder(weight)(tg, games, power, config, bridge)
    table <- build_historical_matchup_table(built$games, built$snapshots, config)
    apply_fbs_bridge_backtest_features(table, tg, flags, config)
  })
  week3_assert_feature_scope(rebuilt[[1]], rebuilt[[2]])
  replay_fields <- intersect(fields, names(rebuilt[[1]]))
  parity <- foundation_compare(input$data, rebuilt[[1]], "game_id", replay_fields, 1e-8)
  if (any(parity$changed + parity$missing_mismatch > 0)) stop("Original feature replay failed.")
  write.csv(parity, file.path(output, "baseline_feature_parity.csv"), row.names = FALSE)
  candidate <- input
  changed_fields <- intersect(c(paste0("home_", week3_efficiency_metrics()),
    paste0("away_", week3_efficiency_metrics()), paste0(week3_efficiency_metrics(), "_diff")), names(input$data))
  i <- match(input$data$game_id, rebuilt[[2]]$game_id)
  if (anyNA(i)) stop("Rebuild lost training rows.")
  active <- input$data$postseason_type == "regular" & input$data$week == 3L
  active[is.na(active)] <- FALSE
  candidate$data[active, changed_fields] <- rebuilt[[2]][i[active], changed_fields]
  week3_assert_feature_scope(input$data, candidate$data)
  forecasts <- list(v3_30 = baseline,
    efficiency_50 = foundation_fixed_forecasts(candidate, choices, fields, config))
  repeat_forecast <- foundation_fixed_forecasts(candidate, choices, fields, config)
  stopifnot(identical(forecasts$efficiency_50, repeat_forecast))
  saveRDS(list(baseline = input, candidate = candidate), file.path(output, "training_inputs.rds"))
  saveRDS(forecasts, file.path(output, "full_forecasts.rds"))
  write.csv(choices, file.path(output, "fold_choices.csv"), row.names = FALSE)
  manifest <- do.call(rbind, lapply(choices$test_season, function(year) {
    active <- experiment_preseason_gate(input$covered_seasons,
      paste0("ps_", config$preseason$production_features, "_diff"))(year, fields)
    data.frame(test_season = year, feature = active)
  }))
  write.csv(manifest, file.path(output, "feature_manifest.csv"), row.names = FALSE)
  changes <- foundation_compare(input$data, candidate$data, "game_id", fields)
  write.csv(changes, file.path(output, "feature_changes.csv"), row.names = FALSE)
  changed <- unlist(lapply(changed_fields, function(f) {
    a <- input$data[[f]]; b <- candidate$data[[f]]
    which(is.finite(a) & is.finite(b) & abs(a-b) > 1e-10)
  }), use.names = FALSE)
  ledger <- input$data[c("game_id", "season", "week", "home", "away", "home_source_games", "away_source_games")]
  ledger$efficiency_changed <- seq_len(nrow(ledger)) %in% changed
  ledger$roster_weight <- preseason_feature_weight(ifelse(input$data$postseason_type == "regular", input$data$week, 99), config)
  write.csv(ledger, file.path(output, "sample_ledger.csv"), row.names = FALSE)

  ref <- read_csv_if_present(file.path(four_way, "predictions.csv"), TRUE)
  ref <- ref[ref$model == "v3_control", ]
  scored <- week3_score_pair(ref, forecasts, input$data)
  matched <- readRDS(file.path(qualified, "model_inputs.rds"))
  matched <- matched[matched$season %in% 2022:2025, ]
  assert_unique_keys(matched, "game_id", "qualified quotes")
  qi <- match(matched$game_id, ref$game_id)
  if (anyNA(qi)) stop("Qualified games outside evaluation cohort.")
  for (field in c("home", "away", "season"))
    if (!isTRUE(all.equal(matched[[field]], ref[[field]][qi], check.attributes = FALSE)))
      stop("Qualified quote identity mismatch.")
  stopifnot(isTRUE(all.equal(matched$margin, ref$actual_margin[qi], check.attributes = FALSE)))
  qref <- ref[qi, ]; qref$closing_home_spread <- matched$closing_home_spread
  qscored <- week3_score_pair(qref, forecasts, input$data)
  all <- week3_summaries(scored); corroborated <- week3_summaries(qscored)
  summary <- rbind(cbind(cohort = "original_lines", all$metrics),
    cbind(cohort = "corroborated_quotes", corroborated$metrics))
  paired <- rbind(cbind(cohort = "original_lines", all$paired),
    cbind(cohort = "corroborated_quotes", corroborated$paired))
  omissions <- rbind(cbind(cohort = "original_lines", all$omissions),
    cbind(cohort = "corroborated_quotes", corroborated$omissions))
  write.csv(scored, file.path(output, "predictions.csv"), row.names = FALSE)
  write.csv(qscored, file.path(output, "corroborated_predictions.csv"), row.names = FALSE)
  write.csv(summary, file.path(output, "metrics.csv"), row.names = FALSE)
  write.csv(paired, file.path(output, "paired_comparisons.csv"), row.names = FALSE)
  write.csv(omissions, file.path(output, "leave_one_season_out.csv"), row.names = FALSE)
  headline <- paired[paired$slice == "week_3", ]
  signal <- all(is.finite(headline$mae_change) & headline$mae_change < 0) &&
    all(is.finite(headline$ats_change) & headline$ats_change >= 0) && all(omissions$mae_change < 0)
  display <- function(x) {
    x <- x[c("cohort", "model", "slice", "games", "wins", "losses", "pushes", "ats_accuracy", "margin_mae", "su_accuracy")]
    for (f in c("ats_accuracy", "margin_mae", "su_accuracy")) x[[f]] <- round(x[[f]], 4)
    markdown_table(x)
  }
  writeLines(c("# Week 3 Efficiency Retention Results", "",
    "One predeclared 50% efficiency-history challenger; roster weight unchanged at 30%.",
    "Earlier-season refits, original coach/lambda choices, no production promotion.", "",
    "## Primary Week 3 Comparison", "", display(summary[summary$slice == "week_3", ]), "",
    markdown_table(headline), "", paste("Historical signal screen passed:", signal), "",
    "## Week 3 By Season", "", display(summary[grepl("^week_3_20", summary$slice), ]), "",
    "## Usable FBS Samples", "", display(summary[grepl("min_sample", summary$slice), ]), "",
    "Minimum of the two teams' prior eligible FBS net-efficiency game counts; individual metrics can have fewer valid cells.", "",
    "## Other-Week Refit Effects", "", display(summary[summary$slice %in% c("all_fbs", "other_weeks"), ]), "",
    "No input-policy change outside Week 3, but refitted coefficients can change other-week predictions.",
    "Full forecasts are diagnostics, not authorization for an all-season deployment change.", "",
    "## Limits", "", "Four historical seasons already examined in earlier research, not a fresh holdout.",
    "Bootstrap intervals are descriptive; no grid search or post-hoc subgroup promotion.",
    "Canonical lines have weak timing provenance. Corroborated quotes improve source agreement,",
    "not proof of Friday execution or actual prices. No realized ROI claim; no 2026 results used.",
    "reference_margin_sd is baseline uncertainty, not recalibrated challenger confidence.",
    "Original PBP classification and missing-value behavior retained to isolate the requested change.", "",
    "## Reproduce", "", "`Rscript run_cfb_week3_efficiency.R` (offline, retained caches required)."),
    file.path(output, "REPORT.md"))
  if (!identical(inputs, inventory(source_paths)) || !identical(code, inventory(code_paths)))
    stop("Experiment inputs or code changed during execution.")
  after <- experiment_file_hashes(config)
  if (!identical(protected, after)) stop("Protected production files changed.")
  write.csv(after, file.path(output, "protected_after.csv"), row.names = FALSE)
  jsonlite::write_json(list(status = "complete", production_unchanged = TRUE,
    refit_repeat_identical = TRUE, automatic_promotion = FALSE, historical_signal_screen = signal,
    fitting_rows = nrow(input$data), evaluation_games = nrow(ref), qualified_games = nrow(qref),
    independently_graded_rows = nrow(scored) + nrow(qscored), roster_features_unchanged = TRUE,
    only_regular_week3_input_metrics_changed = TRUE), file.path(output, "verification.json"),
    pretty = TRUE, auto_unbox = TRUE)
  writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
  for (p in code_paths) file.copy(p, file.path(output, basename(p)))
  external_seal(output)
  list(output = output, metrics = summary, paired = paired)
}
