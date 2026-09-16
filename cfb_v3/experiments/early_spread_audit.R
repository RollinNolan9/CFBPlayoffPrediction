audit_margin_summary <- function(x) {
  x <- experiment_grade(x)
  lined <- is.finite(x$closing_home_spread)
  direction <- sign(-x$closing_home_spread)
  slope <- if (nrow(x) > 2 && stats::var(x$expected_margin) > 0)
    stats::cov(x$expected_margin, x$actual_margin) / stats::var(x$expected_margin) else NA_real_
  data.frame(games = nrow(x), lined = sum(lined), wins = sum(x$win), losses = sum(x$loss),
    pushes = sum(x$push), mae = mean(abs(x$expected_margin-x$actual_margin)),
    lined_model_mae = mean(abs(x$expected_margin[lined]-x$actual_margin[lined])),
    market_mae = mean(abs(-x$closing_home_spread-x$actual_margin), na.rm = TRUE),
    prediction_slope = slope,
    mean_market_favorite_prediction = mean(direction*x$expected_margin, na.rm = TRUE),
    mean_market_favorite_actual = mean(direction*x$actual_margin, na.rm = TRUE),
    mean_market_margin = mean(abs(x$closing_home_spread), na.rm = TRUE),
    mean_model_favorite_error = mean(sign(x$expected_margin)*(x$expected_margin-x$actual_margin)),
    predictions_over_21 = sum(abs(x$expected_margin) > 21),
    max_absolute_prediction = max(abs(x$expected_margin)))
}

audit_prediction_groups <- function(predictions) {
  rows <- list()
  for (model in unique(predictions$model)) {
    x <- predictions[predictions$model == model, ]
    regular <- x$postseason_type == "regular"
    slices <- list(all = rep(TRUE, nrow(x)), week_0_1 = regular & x$week <= 1,
      weeks_2_4 = regular & x$week >= 2 & x$week <= 4,
      week_5_plus = regular & x$week >= 5, market_over_21 = abs(x$closing_home_spread) > 21,
      model_over_21 = abs(x$expected_margin) > 21)
    for (year in sort(unique(x$season))) {
      slices[[paste0("week_0_1_", year)]] <- regular & x$week <= 1 & x$season == year
      slices[[paste0("market_over_21_", year)]] <- abs(x$closing_home_spread) > 21 & x$season == year
    }
    for (slice in names(slices)) {
      part <- x[which(slices[[slice]]), ]
      if (nrow(part)) rows[[paste(model, slice)]] <- cbind(model = model, slice = slice,
        audit_margin_summary(part))
    }
  }
  do.call(rbind, rows)
}

audit_fixed_fold_replay <- function(common, full, config, covered) {
  data <- full$coach$data
  stopifnot(identical(data$game_id, common$data$game_id))
  ps <- paste0("ps_", config$preseason$production_features, "_diff")
  gate <- experiment_preseason_gate(covered, ps)
  ledger <- list(); coefficients <- list(); contributions <- list()
  for (season in full$choices$test_season) {
    choice <- full$choices[full$choices$test_season == season, ]
    fold <- data
    column <- if (choice$coach_recent_share == .70) "coach_rating_70_30_diff" else "coach_rating_65_35_diff"
    fold$coach_rating_diff <- fold[[column]]
    train <- which(fold$season < season); test <- which(fold$season == season)
    weights <- common$weights[train] / max(common$weights[train])
    base_features <- setdiff(full$coach$features, ps)
    full_features <- gate(season, full$coach$features)
    base <- fit_cfb_ensemble(fold[train, ], features = base_features, weights = weights,
      lambda = choice$foundation_lambda, config = config, fit_nonlinear = FALSE)
    roster <- fit_cfb_ensemble(fold[train, ], features = full_features, weights = weights,
      lambda = choice$preseason_lambda, config = config, fit_nonlinear = FALSE)
    share <- preseason_blend_share_for_data(fold[test, ], config)
    bp <- predict(base, fold[test, ])$expected_margin
    rp <- predict(roster, fold[test, ])$expected_margin
    expected <- (1-share)*bp + share*rp
    saved <- full$predictions$expected_margin[match(fold$game_id[test], full$predictions$game_id)]
    stopifnot(all(is.finite(saved)), max(abs(saved-expected)) < 1e-8)
    rc <- ridge_feature_contributions(roster, fold[test, ])
    bc <- ridge_feature_contributions(base, fold[test, ])
    feature_names <- union(colnames(bc), colnames(rc))
    blended <- sweep(align_contribution_columns(bc, feature_names), 1, 1-share, `*`) +
      sweep(align_contribution_columns(rc, feature_names), 1, share, `*`)
    intercept <- (1-share)*ridge_intercept_contribution(base, fold[test, ]) +
      share*ridge_intercept_contribution(roster, fold[test, ])
    stopifnot(max(abs(rowSums(blended)+intercept-expected)) < 1e-8)
    contribution <- data.frame(game_id = as.character(fold$game_id[test]), season = season,
      blended, check.names = FALSE)
    contribution$intercept <- intercept
    contributions[[as.character(season)]] <- contribution
    early <- fold$phase_week[test] <= 4
    ledger[[as.character(season)]] <- data.frame(game_id = as.character(fold$game_id[test]),
      foundation_margin = bp, roster_margin = rp, expected_margin = expected,
      roster_active = early & any(ps %in% full_features), roster_weight = share,
      roster_model_delta = expected-bp,
      roster_direct_contribution = rowSums(blended[, intersect(ps, colnames(blended)), drop = FALSE]),
      replay_delta = expected-saved)
    for (variant in c("foundation", "roster")) {
      fit <- if (variant == "foundation") base$ridge else roster$ridge
      f <- fit$recipe$features
      z <- bake_numeric_recipe(fit$recipe, fold[test, ])
      coefficients[[paste(season, variant)]] <- data.frame(season = season, variant = variant,
        feature = f, coefficient = unname(fit$coefficients[-1]),
        training_median = unname(fit$recipe$medians), training_mean = unname(fit$recipe$means),
        training_sd = unname(fit$recipe$scales),
        training_missing = vapply(fold[train, f, drop = FALSE], function(x) sum(!is.finite(x)), integer(1)),
        test_missing = vapply(fold[test, f, drop = FALSE], function(x) sum(!is.finite(x)), integer(1)),
        max_test_absolute_z = apply(abs(z), 2, max),
        max_test_absolute_contribution = apply(abs(sweep(z, 2, fit$coefficients[-1], `*`)), 2, max))
    }
  }
  all_features <- unique(unlist(lapply(contributions, names)))
  contributions <- lapply(contributions, function(x) {
    for (name in setdiff(all_features, names(x))) x[[name]] <- 0
    x[all_features]
  })
  list(ledger = do.call(rbind, ledger), coefficients = do.call(rbind, coefficients),
    contributions = do.call(rbind, contributions))
}

audit_preseason_sources <- function(config, priors) {
  membership <- read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE)
  team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  strength <- eligible_team_strength(team_games, games)
  source_checks <- list(); differences <- list(); feature_checks <- list()
  for (season in sort(unique(priors$season))) {
    raw <- lapply(c("returning", "talent", "portal", "player_ppa", "polls"), function(name) {
      year <- if (name == "player_ppa") season-1L else season
      readRDS(preseason_cache_path(config, name, year))
    })
    names(raw) <- c("returning", "talent", "portal", "player_ppa", "polls")
    returning <- normalize_returning_production(raw$returning, season)
    talent <- normalize_team_talent(raw$talent, season)
    portal <- normalize_transfer_portal(raw$portal, season)
    members <- preseason_membership_for_season(membership, raw$talent, season)
    teams <- canonical_team(members$team[members$season == season & members$classification == "fbs"])
    fbs_talent <- talent[talent$team %in% teams, ]
    source_checks[[as.character(season)]] <- data.frame(season = season,
      returning_duplicate_teams = sum(duplicated(returning$team)),
      talent_duplicate_teams = sum(duplicated(talent$team)),
      fbs_talent_duplicate_teams = sum(duplicated(fbs_talent$team)),
      portal_rows = nrow(portal), portal_unknown_date = sum(is.na(portal$transfer_date)),
      portal_missing_rating = sum(!is.finite(portal$rating)))
    stopifnot(!anyDuplicated(returning$team), !anyDuplicated(fbs_talent$team))
    reconstructed <- build_preseason_team_priors(raw$returning, raw$talent, raw$polls,
      members, strength, season,
      config, raw$portal, raw$player_ppa)
    current <- priors[priors$season == season, ]
    index <- match(current$team, reconstructed$team)
    stopifnot(!anyNA(index), setequal(current$team, reconstructed$team))
    for (f in config$preseason$production_features) {
      old <- current[[f]]; new <- reconstructed[[f]][index]
      different <- xor(is.finite(old), is.finite(new)) |
        (is.finite(old) & is.finite(new) & abs(new-old) > 1e-8)
      if (any(different)) differences[[paste(season, f)]] <- data.frame(team = current$team[different],
        season = season, feature = f, frozen = old[different], rebuilt = new[different])
      feature_checks[[paste(season, f)]] <- data.frame(season = season, feature = f,
        teams = nrow(current), missing = sum(!is.finite(old)), zero = sum(old == 0, na.rm = TRUE),
        minimum = min(old, na.rm = TRUE), maximum = max(old, na.rm = TRUE),
        frozen_reconstruction_differences = sum(different))
    }
  }
  list(sources = do.call(rbind, source_checks), features = do.call(rbind, feature_checks),
    differences = if (length(differences)) do.call(rbind, differences) else
      data.frame(team = character(), season = integer(), feature = character(), frozen = numeric(), rebuilt = numeric()))
}

audit_frozen_week_two <- function(config) {
  root <- file.path(config$output_dir, "2026")
  stem <- file.path(root, "week_2", "live_live_2026_w02_20260909T005125Z")
  saved <- readRDS(paste0(stem, "_model.rds"))
  features <- read_csv_if_present(paste0(stem, "_features.csv"), TRUE)
  card <- read_csv_if_present(file.path(root, "week_2_preliminary", "20260909T032636Z", "predictions.csv"), TRUE)
  assert_unique_keys(features, "game_id", "frozen live features")
  assert_unique_keys(card, "game_id", "frozen article card")
  index <- match(card$game_id, features$game_id)
  stopifnot(!anyNA(index), nrow(card) == nrow(features))
  features <- features[index, ]
  expected <- predict(saved$model, features)$expected_margin
  stopifnot(max(abs(expected-card$expected_margin)) < 1e-8)
  contributions <- ridge_feature_contributions(saved$model, features)
  cache <- file.path(config$project_dir, "cfb_v2", "cache")
  universe <- derive_games_from_pbp(readRDS(file.path(cache, "pbp", "pbp_2026_compact.rds")),
    schedule = readRDS(file.path(cache, "schedules", "cfbd_schedule_2026.rds")),
    fbs_membership = readRDS(file.path(cache, "teams", "cfbd_fbs_teams_2026.rds")))
  current <- universe[which(universe$completed & as.numeric(universe$kickoff) < as.numeric(saved$as_of)), ]
  stopifnot(length(unique(features$model_week)) == 1L)
  before <- current[current$model_week < features$model_week[1], ]
  same_cycle <- current[current$model_week >= features$model_week[1], ]
  sides <- list()
  for (side in c("home", "away")) for (i in seq_len(nrow(card))) {
    team <- card[[side]][i]
    past <- before[before$home == team | before$away == team, ]
    eligible <- raw_history_eligible(past)
    observed <- features[[paste0(side, "_source_games")]][i]
    reason <- if (observed > 0) "has_fbs_efficiency_sample" else if (!nrow(past))
      "no_previous_cutoff_game" else if (!any(eligible)) "fcs_only_before_cutoff" else "unexpected_missing_fbs_sample"
    sides[[paste(side, i)]] <- data.frame(game_id = card$game_id[i], side = side, team = team,
      completed_before_cutoff = nrow(past), eligible_fbs_games = sum(eligible),
      eligible_games_without_pbp = sum(eligible & !past$pbp_available),
      efficiency_source_games = observed, reason = reason,
      same_cycle_games_excluded = sum(same_cycle$home == team | same_cycle$away == team))
  }
  sides <- do.call(rbind, sides)
  queue <- card[c("game_id", "home", "away", "expected_margin", "market_home_spread",
    "preseason_adjustment", "data_flag", "market_captured_at")]
  for (side in c("home", "away")) {
    subset <- sides[sides$side == side, ]
    subset <- subset[match(card$game_id, subset$game_id), ]
    queue[[paste0(side, "_efficiency_source_games")]] <- subset$efficiency_source_games
    queue[[paste0(side, "_evidence_status")]] <- subset$reason
  }
  queue$no_current_fbs_sample <- queue$home_efficiency_source_games == 0 | queue$away_efficiency_source_games == 0
  queue$passing_ppa_ratio_contribution <- contributions[, "ps_returning_passing_ppa_pct_diff"]
  queue$talent_contribution <- contributions[, "ps_talent_percentile_diff"]
  queue$ats_edge <- queue$expected_margin+queue$market_home_spread
  queue$absolute_market_spread <- abs(queue$market_home_spread)
  queue <- queue[order(-as.integer(queue$no_current_fbs_sample), -abs(queue$ats_edge)), ]
  list(queue = queue, team_evidence = sides, same_cycle_excluded_games = same_cycle,
    replay_max_delta = max(abs(expected-card$expected_margin)))
}

run_early_spread_audit <- function(config) {
  protected <- experiment_file_hashes(config)
  source <- file.path(config$output_dir, "experiments", "four_way", "20260909T214828Z")
  output <- file.path(config$output_dir, "experiments", "early_spread_audit",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
  if (dir.exists(output)) stop("Refusing to overwrite an audit.")
  dir.create(output, recursive = TRUE)
  message("Audit output: ", output)
  priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
  common <- readRDS(file.path(source, "common_inputs.rds"))
  full <- readRDS(file.path(source, "v3_matched_validation.rds"))
  predictions <- read_csv_if_present(file.path(source, "predictions.csv"), TRUE)
  message("Replaying fixed folds and reconciling feature contributions")
  replay <- audit_fixed_fold_replay(common, full, config, sort(unique(priors$season)))
  index <- match(full$predictions$game_id, replay$ledger$game_id)
  stopifnot(!anyNA(index))
  details <- cbind(full$predictions, replay$ledger[index,
    setdiff(names(replay$ledger), c("game_id", "expected_margin"))])
  input <- common$data[match(details$game_id, common$data$game_id), ]
  details <- cbind(details, input[c("home_games_played", "away_games_played", "home_source_games", "away_source_games")])
  foundation <- full$predictions
  foundation$model <- "v3_matched_foundation_diagnostic"
  foundation$expected_margin <- details$foundation_margin
  metrics <- audit_prediction_groups(rbind(predictions, experiment_grade(foundation)))
  message("Reconstructing preseason inputs from existing local raw caches")
  coverage <- audit_preseason_sources(config, priors)
  message("Checking frozen Week 2 evidence and replay parity")
  live <- audit_frozen_week_two(config)
  artifacts <- list(game_diagnostics = details, feature_contributions = replay$contributions,
    coefficients = replay$coefficients, slice_metrics = metrics, source_checks = coverage$sources,
    preseason_coverage = coverage$features, frozen_reconstruction_differences = coverage$differences,
    week_2_review_queue = live$queue, week_2_team_evidence = live$team_evidence,
    same_cycle_excluded_games = live$same_cycle_excluded_games)
  for (name in names(artifacts)) write_foundation_csv(artifacts[[name]], file.path(output, paste0(name, ".csv")))
  after <- experiment_file_hashes(config)
  stopifnot(identical(protected, after))
  write_foundation_csv(protected, file.path(output, "protected_before.csv"))
  write_foundation_csv(after, file.path(output, "protected_after.csv"))
  jsonlite::write_json(list(status = "complete", production_unchanged = TRUE,
    replay_max_delta = max(abs(replay$ledger$replay_delta)),
    reconstruction_differences = nrow(coverage$differences),
    live_replay_max_delta = live$replay_max_delta,
    source_run = source, diagnostic_only = TRUE,
    code_md5 = as.list(tools::md5sum(c(file.path(config$project_dir, "run_cfb_early_spread_audit.R"),
      file.path(config$project_dir, "cfb_v3", "experiments", "early_spread_audit.R"))))),
    file.path(output, "verification.json"), pretty = TRUE, auto_unbox = TRUE)
  list(output = output, metrics = metrics, coverage = coverage)
}
