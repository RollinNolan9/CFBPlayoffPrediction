# Offline current-snapshot companion to margin_cap_features.R. This helper reads
# frozen sources and returns data; it never writes, fetches, or refits a model.

margin_cap_current_load_inputs <- function(config, published_dir = file.path(
    config$output_dir, "2026", "week_5_rankings", "20260927_espn_supplement"),
    historical_experiment_dir = file.path(config$output_dir, "experiments",
      "margin_caps", "20260927T211431Z")) {
  published_dir <- normalizePath(published_dir, winslash = "/", mustWork = TRUE)
  historical_experiment_dir <- normalizePath(historical_experiment_dir,
                                             winslash = "/", mustWork = TRUE)
  qa_path <- file.path(published_dir, "qa.json")
  qa <- jsonlite::read_json(qa_path, simplifyVector = TRUE)
  resolve_path <- function(path) {
    if (grepl("^([A-Za-z]:|/)", path)) path else file.path(config$project_dir, path)
  }
  published_hashes <- unlist(qa$source_hashes)
  source_paths <- vapply(names(published_hashes), resolve_path, character(1))
  current_hashes <- tools::md5sum(source_paths)
  if (anyNA(current_hashes) || any(unname(current_hashes) != unname(published_hashes))) {
    bad <- names(published_hashes)[is.na(current_hashes) |
      unname(current_hashes) != unname(published_hashes)]
    stop("Published ranking source changed or is missing: ", paste(bad, collapse = ", "))
  }
  snapshots_path <- file.path(published_dir, "team_snapshots.csv")
  included_path <- file.path(published_dir, "included_games.csv")
  features <- read_csv_if_present(snapshots_path, TRUE)
  included <- read_csv_if_present(included_path, TRUE)
  assert_unique_keys(features, "team", "published current team features")
  assert_unique_keys(included, "game_id", "published included games")
  season <- as.integer(qa$season)
  week <- as.integer(qa$week)
  model_week <- as.integer(qa$model_week)
  as_of <- parse_utc_datetime(qa$as_of)
  stopifnot(length(season) == 1L, length(week) == 1L, length(model_week) == 1L,
            length(as_of) == 1L, !is.na(as_of), all(features$season == season),
            all(features$week == week), all(features$model_week == model_week),
            nrow(features) == qa$team_count)
  bundle <- readRDS(qa$model_path)
  for (key in c("training", "phase", "coach", "bridge")) {
    if (!isTRUE(all.equal(config[[key]], bundle$config[[key]]))) {
      stop("Current feature settings differ from published model: ", key)
    }
  }
  saved <- read_csv_if_present(sub("_model\\.rds$", "_features.csv", qa$model_path), TRUE)
  recent_share <- unique(saved$coach_recent_share)
  stopifnot(length(recent_share) == 1L, is.finite(recent_share))
  cache_path <- function(group, name) file.path(config$project_dir, "cfb_v2", "cache", group, name)
  raw <- readRDS(cache_path("schedules", paste0("cfbd_schedule_", season, ".rds")))
  membership <- readRDS(cache_path("teams", paste0("cfbd_fbs_teams_", season, ".rds")))
  membership$team <- canonical_team(membership$team)
  membership <- membership[membership$season == season & membership$classification == "fbs", ]
  assert_unique_keys(membership, "team", "frozen current FBS membership")
  teams <- sort(membership$team)
  stopifnot(setequal(teams, features$team))
  pbp <- readRDS(cache_path("pbp", paste0("pbp_", season, "_compact.rds")))
  coaches <- readRDS(cache_path("coaches", paste0("current_fbs_coaches_", season, ".rds")))
  universe <- derive_games_from_pbp(pbp, schedule = raw, fbs_membership = membership)
  completed <- filter_completed_historical_games(universe)
  completed <- completed[completed$model_week < model_week & completed$kickoff < as_of, ]
  if (any(raw_history_eligible(completed) & !completed$pbp_available)) {
    stop("Prior-week FBS game is missing the frozen PBP supplement/feed.")
  }
  stopifnot(nrow(completed) == qa$current_season_games_included,
            sum(raw_history_eligible(completed)) == qa$completed_fbs_games_included,
            all(completed$model_week < model_week), all(completed$kickoff < as_of),
            setequal(completed$game_id, included$game_id))
  index <- match(included$game_id, completed$game_id)
  for (field in names(included)) {
    expected <- included[[field]]
    observed <- completed[[field]][index]
    if (field == "game_id") {
      expected <- as.character(expected)
      observed <- as.character(observed)
    }
    if (!isTRUE(all.equal(expected, observed,
                         tolerance = 0, check.attributes = FALSE))) {
      stop("Frozen current game differs from published inclusion: ", field)
    }
  }
  history_games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
  history_games$kickoff <- parse_utc_datetime(history_games$kickoff)
  history_games <- history_games[history_games$season < season, ]
  historical_team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
  historical_team_games <- historical_team_games[historical_team_games$season < season, ]
  flags <- combine_membership(membership, read_csv_if_present(
    file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  transitions <- fbs_transition_teams(flags)
  bridge <- if (nrow(transitions)) calibrate_fbs_bridge(historical_team_games, flags, config) else NULL
  overrides <- read_csv_if_present(file.path(config$inbox_dir, "coach_assignment_overrides.csv"))
  manual <- read_csv_if_present(file.path(config$inbox_dir, "coach_history_manual.csv"))
  assignments <- current_fbs_coach_assignments(teams, coaches, season, overrides)
  coverage <- standardize_schedule(data.frame(
    game_id = paste0("ranking_snapshot_", seq_along(teams)), season = season,
    week = week, model_week = model_week, coach_lookup_week = week, kickoff = as_of,
    home = teams, away = c(teams[-1], teams[1]), neutral_site = TRUE,
    venue = "Neutral field", home_level = "fbs", away_level = "fbs",
    postseason_type = "regular", conference_championship = FALSE, is_cfp = FALSE,
    home_score = NA_real_, away_score = NA_real_, stringsAsFactors = FALSE))
  source_paths <- unique(c(source_paths, qa_path, snapshots_path, included_path))
  list(published_dir = published_dir, historical_experiment_dir = historical_experiment_dir,
       published_qa = qa, team_features = features, completed = completed,
       games = bind_rows_fill(history_games, completed), membership = membership,
       assignments = assignments, manual_coach_history = manual, coverage = coverage,
       teams = teams, season = season, week = week, model_week = model_week, as_of = as_of,
       recent_share = recent_share, transitions = transitions, bridge = bridge,
       source_paths = source_paths, source_md5 = tools::md5sum(source_paths))
}

margin_cap_current_build <- function(config, cap, cache = NULL, historical_built = NULL) {
  margin_cap_validate(cap)
  if (is.null(cache)) cache <- margin_cap_current_load_inputs(config)
  label <- if (is.infinite(cap)) "uncapped" else paste0("cap_", cap)
  historical_path <- file.path(cache$historical_experiment_dir, paste0(label, "_rebuilt.rds"))
  if (is.null(historical_built)) historical_built <- readRDS(historical_path)
  expected_label <- if (is.infinite(cap)) "uncapped" else as.character(cap)
  stopifnot(identical(historical_built$qa$cap, expected_label),
            isTRUE(historical_built$qa$chronological_pass),
            isTRUE(historical_built$qa$kickoff_cutoff_pass),
            isTRUE(historical_built$qa$protected_fields_unchanged))
  historical_hashes <- unlist(historical_built$qa$source_md5)
  if (!identical(unname(historical_hashes), unname(tools::md5sum(names(historical_hashes))))) {
    stop("Historical cap rebuild source files have changed.")
  }
  if (!identical(cache$source_md5, tools::md5sum(cache$source_paths))) {
    stop("Current published source files changed after loading.")
  }
  # Only this private copy is clipped. It is used exclusively as the power-fit
  # source; completed below remains the original-score coach residual target.
  power_games <- cache$games
  power_games$margin <- pmax(-cap, pmin(cap, power_games$margin))
  current_power <- power_rating_for_week(power_games, cache$season, cache$model_week,
                                         cache$week, config)
  before <- cache$team_features
  features <- before
  features$power_rating <- current_power$power_rating[match(features$team, current_power$team)]
  features$power_rating[!is.finite(features$power_rating)] <- 0
  if (!is.null(cache$bridge)) {
    bridge_input <- features[c("team", "season", "source_games", "games_played", "power_rating")]
    bridge_output <- apply_fbs_bridge_features(bridge_input, cache$transitions, cache$bridge)
    features$power_rating <- bridge_output$power_rating
  }
  history <- historical_built$coach_history
  if (!is.null(cache$manual_coach_history) && nrow(cache$manual_coach_history)) {
    history <- merge_manual_coach_history(history, cache$manual_coach_history)
  }
  history <- extend_current_coach_history(history, cache$completed, power_games,
    cache$assignments, cache$season, config)
  features <- attach_coach_ratings(cache$coverage, features, cache$assignments,
                                   history, cache$recent_share, config)
  features <- features[match(before$team, features$team), names(before), drop = FALSE]
  rownames(features) <- rownames(before)
  preserved <- setdiff(names(before), c("power_rating", "coach_rating"))
  stopifnot(isTRUE(all.equal(before[preserved], features[preserved],
                           tolerance = 0, check.attributes = FALSE)))
  comparison <- margin_cap_compare_fields(before, features, c("power_rating", "coach_rating"))
  parity <- all(comparison$changed_rows == 0L)
  if (is.infinite(cap) && !parity) stop("Uncapped current power/coach features do not reproduce published snapshot.")
  eligible <- is.finite(cache$games$margin) & !is.na(cache$games$completed) &
    cache$games$completed & is.finite(cache$games$model_week) &
    cache$games$season >= cache$season - 3L & cache$games$season <= cache$season &
    (cache$games$season < cache$season | cache$games$model_week < cache$model_week)
  eligible[is.na(eligible)] <- FALSE
  current_history <- history[history$season == cache$season, , drop = FALSE]
  stopifnot(all(current_history$week < cache$model_week),
            identical(cache$source_md5, tools::md5sum(cache$source_paths)))
  qa <- list(cap = expected_label, season = cache$season, week = cache$week,
    model_week = cache$model_week, as_of = cache$published_qa$as_of,
    team_count = nrow(features), current_completed_games = nrow(cache$completed),
    power_source_games = sum(eligible), power_clipped_source_games = sum(
      eligible & is.finite(cache$games$margin) & abs(cache$games$margin) > cap),
    actual_game_targets_unchanged = TRUE, efficiency_and_counts_unchanged = TRUE,
    published_espn_efficiency_supplement_preserved = TRUE,
    current_coach_rows = nrow(current_history), historical_rebuild = historical_path,
    historical_rebuild_md5 = unname(tools::md5sum(historical_path)),
    uncapped_published_feature_parity = if (is.infinite(cap)) parity else NA,
    cached_comparison = comparison, source_files_unchanged = TRUE,
    source_md5 = as.list(cache$source_md5),
    method = "Only private power-fit margins are clipped before HFA; current coach outcomes and published efficiency/count inputs remain unchanged.")
  list(team_features = features, power = current_power, coach_history = history, qa = qa)
}
