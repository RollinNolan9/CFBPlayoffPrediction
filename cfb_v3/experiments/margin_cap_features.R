# Experimental feature rebuild only. Source the existing cfb_v2 modules first.
# No files are written and no network/data-refresh functions are called here.

margin_cap_load_inputs <- function(config) {
  paths <- c(
    data = file.path(config$inbox_dir, "training_games.csv"),
    games = file.path(config$data_dir, "historical_games.csv"),
    assignments = file.path(config$inbox_dir, "coach_assignments.csv"),
    coach_history = file.path(config$inbox_dir, "coach_history.csv")
  )
  inputs <- lapply(paths, read_csv_if_present, required = TRUE)
  assert_unique_keys(inputs$data, "game_id", "margin-cap cached training")
  assert_unique_keys(inputs$games, "game_id", "margin-cap historical games")
  index <- match(inputs$data$game_id, inputs$games$game_id)
  if (anyNA(index)) stop("Training rows lack a frozen historical game.")
  fields <- c("season", "week", "model_week", "home", "away", "margin",
              "home_score", "away_score", "neutral_site", "postseason_type")
  for (field in fields) {
    if (!isTRUE(all.equal(inputs$data[[field]], inputs$games[[field]][index],
                         tolerance = 1e-12, check.attributes = FALSE))) {
      stop("Frozen training and historical games disagree: ", field)
    }
  }
  inputs$source_paths <- paths
  inputs$source_md5 <- tools::md5sum(paths)
  inputs
}

margin_cap_validate <- function(cap) {
  if (!is.numeric(cap) || length(cap) != 1L || is.na(cap) || cap <= 0) {
    stop("Margin cap must be one positive number or Inf.")
  }
  invisible(cap)
}

margin_cap_power_snapshots <- function(games, config, cap) {
  margin_cap_validate(cap)
  # Rebind copies so lexical name resolution reaches the experimental solver.
  # Production functions and their environments remain unchanged.
  scope <- new.env(parent = environment(power_rating_for_week))
  state <- new.env(parent = emptyenv())
  state$audit <- list()
  state$target <- NULL
  original_solver <- opponent_adjusted_rating
  original_week <- power_rating_for_week
  environment(original_week) <- scope
  snapshots <- power_rating_snapshots
  environment(snapshots) <- scope

  scope$opponent_adjusted_rating <- function(games, value_col, ridge = 10,
                                            home_field = 2.4, weights = NULL) {
    if (!identical(value_col, "margin")) stop("Unexpected power target.")
    target <- state$target
    prior <- games$season < target$season |
      (games$season == target$season & games$model_week < target$model_week)
    if (anyNA(prior) || !all(prior)) stop("Power fit received current/future games.")
    usable <- is.finite(games[[value_col]]) & is.finite(weights) & weights > 0
    clipped <- is.finite(games[[value_col]]) & abs(games[[value_col]]) > cap
    same_season <- games$season == target$season
    source_time <- parse_utc_datetime(games$kickoff)
    latest_source <- if (any(!is.na(source_time))) max(source_time, na.rm = TRUE) else
      as.POSIXct(NA, tz = "UTC")
    cutoff_pass <- is.na(target$cutoff) || is.na(latest_source) || latest_source < target$cutoff
    if (!cutoff_pass) stop("Power source kickoff violates the target feature cutoff.")
    state$fit_audit <- data.frame(
      season = target$season, model_week = target$model_week,
      phase_week = target$phase_week, source_games = nrow(games),
      positive_weight_games = sum(usable), clipped_games = sum(clipped),
      positive_weight_clipped_games = sum(clipped & usable),
      max_same_season_source_week = if (any(same_season)) max(games$model_week[same_season]) else NA_real_,
      cutoff = format(target$cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      latest_source_kickoff = format(latest_source, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      chronological_pass = all(prior), kickoff_cutoff_pass = cutoff_pass,
      stringsAsFactors = FALSE
    )
    # The production solver subtracts HFA. Clip the observed margin first.
    adjusted <- games
    adjusted[[value_col]] <- pmax(-cap, pmin(cap, adjusted[[value_col]]))
    original_solver(adjusted, value_col, ridge = ridge,
                    home_field = home_field, weights = weights)
  }
  scope$power_rating_for_week <- function(games, season, model_week, phase_week, config) {
    target_rows <- games$season == season & games$model_week == model_week
    dates <- as.POSIXct(as.Date(games$feature_week_start[target_rows]), tz = "UTC")
    cutoff <- if (any(!is.na(dates))) min(dates, na.rm = TRUE) else as.POSIXct(NA, tz = "UTC")
    state$target <- list(season = season, model_week = model_week,
                         phase_week = phase_week, cutoff = cutoff)
    state$fit_audit <- NULL
    result <- original_week(games, season, model_week, phase_week, config)
    audit <- state$fit_audit
    if (is.null(audit)) audit <- data.frame(
      season = season, model_week = model_week, phase_week = phase_week,
      source_games = 0L, positive_weight_games = 0L, clipped_games = 0L,
      positive_weight_clipped_games = 0L, max_same_season_source_week = NA_real_,
      cutoff = format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      latest_source_kickoff = NA_character_, chronological_pass = TRUE,
      kickoff_cutoff_pass = TRUE, stringsAsFactors = FALSE
    )
    state$audit[[length(state$audit) + 1L]] <- audit
    result
  }
  power <- snapshots(games, config)
  audit <- if (length(state$audit)) do.call(rbind, state$audit) else data.frame()
  list(power = power, audit = audit)
}

margin_cap_compare_fields <- function(reference, candidate, fields, tolerance = 1e-9) {
  do.call(rbind, lapply(fields, function(field) {
    a <- reference[[field]]
    b <- candidate[[field]]
    finite <- is.finite(a) & is.finite(b)
    missing_mismatch <- is.na(a) != is.na(b)
    changed <- missing_mismatch | (finite & abs(a - b) > tolerance)
    data.frame(field = field, changed_rows = sum(changed),
      missing_mismatches = sum(missing_mismatch),
      max_abs_difference = if (any(finite)) max(abs(a[finite] - b[finite])) else 0,
      stringsAsFactors = FALSE)
  }))
}

margin_cap_build <- function(config, cap, cache = NULL) {
  margin_cap_validate(cap)
  if (is.null(cache)) cache <- margin_cap_load_inputs(config)
  before <- cache$data
  data <- before
  rebuilt <- margin_cap_power_snapshots(cache$games, config, cap)
  power <- rebuilt$power
  assert_unique_keys(power, c("team", "season", "model_week"), "margin-cap power snapshots")
  key <- paste(power$team, power$season, power$model_week, sep = "\r")
  for (side in c("home", "away")) {
    value <- power$power_rating[match(paste(data[[side]], data$season,
      data$model_week, sep = "\r"), key)]
    value[!is.finite(value)] <- 0
    data[[paste0(side, "_power")]] <- value
    rating_field <- paste0(side, "_power_rating")
    # A missing team-game snapshot must remain missing, even if its team has a
    # schedule-derived power rating. This reproduces the original left join.
    value[is.na(before[[rating_field]])] <- NA_real_
    data[[rating_field]] <- value
  }
  data$power_rating_diff <- data$home_power_rating - data$away_power_rating
  data$pregame_expected_margin <- data$power_rating_diff + data$home_field_points

  modern <- build_modern_coach_history(data, cache$assignments)
  if (nrow(modern$missing)) stop("Rebuilt coach history has unmapped games.")
  # Legacy SRS priors are not power-fit margins; retain those frozen values.
  history <- cache$coach_history
  replace <- history$source == "pregame_game_residual" &
    history$season %in% unique(data$season)
  replace[is.na(replace)] <- FALSE
  history <- bind_rows_fill(history[!replace, , drop = FALSE], modern$history)
  history <- history[order(history$season, history$week, history$coach_id), , drop = FALSE]
  coaches <- attach_coach_features_to_history(data, cache$assignments, history, config)
  data <- coaches$training

  changed_fields <- c("home_power", "away_power", "home_power_rating", "away_power_rating",
    "power_rating_diff", "pregame_expected_margin", "coach_rating_65_35_diff",
    "coach_rating_70_30_diff", "coach_rating_diff")
  preserved <- setdiff(names(before), changed_fields)
  preserved_equal <- vapply(preserved, function(field) isTRUE(all.equal(
    before[[field]], data[[field]], tolerance = 0, check.attributes = FALSE)), logical(1))
  if (!all(preserved_equal)) {
    different <- preserved[!preserved_equal]
    stop("Margin-cap builder changed protected fields: ", paste(different, collapse = ", "))
  }
  comparison <- margin_cap_compare_fields(before, data, changed_fields)
  qa <- list(
    cap = if (is.infinite(cap)) "uncapped" else as.character(cap),
    input_rows = nrow(before), output_rows = nrow(data), power_rows = nrow(power),
    snapshot_count = nrow(rebuilt$audit),
    actual_scores_and_targets_unchanged = TRUE,
    efficiency_and_count_inputs_unchanged = TRUE,
    protected_fields_unchanged = TRUE,
    chronological_pass = all(rebuilt$audit$chronological_pass),
    kickoff_cutoff_pass = all(rebuilt$audit$kickoff_cutoff_pass),
    clipping_stage = "symmetric observed margin before power-fit home-field subtraction",
    coach_policy = "Rebuilt actual-minus-capped-power expectations and power-based context; actual outcomes retained.",
    retained_excluded_fields = c("home_home_field_rating", "away_home_field_rating", "challenger_team_home_field_points"),
    legacy_coach_rows_preserved = sum(!replace),
    rebuilt_coach_history_rows = nrow(modern$history),
    uncapped_cached_parity = if (is.infinite(cap)) all(comparison$changed_rows == 0L) else NA,
    cached_comparison = comparison,
    source_md5 = as.list(cache$source_md5)
  )
  list(data = data, power = power, coach_history = history,
       coach_ratings = coaches$ratings, snapshot_audit = rebuilt$audit, qa = qa)
}
