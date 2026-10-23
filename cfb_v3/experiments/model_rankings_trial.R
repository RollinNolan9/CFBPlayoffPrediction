# Offline, frozen-model power rankings. Run from the repository root:
# Rscript cfb_v3/experiments/model_rankings_trial.R [saved_model.rds] [output_directory] [previous_rankings.csv] [target_week]
# This does not retrain, refresh source data, or publish the result.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "cfb_v2", "config.R"))) stop("Run from the repository root.")
`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x
for (module in c("config", "store", "features", "coaches", "adapters", "models",
                 "coach_migration", "historical_data", "preseason", "bridge",
                 "workflow", "live_data")) source(file.path("cfb_v2", paste0(module, ".R")))
model_path <- if (length(args)) args[1] else
  "cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z_model.rds"
stem <- sub("_model\\.rds$", "", model_path)
if (identical(stem, model_path)) stop("Expected a production _model.rds bundle.")
bundle <- readRDS(model_path)
config <- bundle$config
config$project_dir <- root
config$data_dir <- file.path(root, "cfb_v3", "data")
config$inbox_dir <- file.path(root, "cfb_v3", "inbox")
source_model_as_of <- parse_utc_datetime(bundle$as_of)
saved <- read_csv_if_present(paste0(stem, "_features.csv"), TRUE)
published <- read_csv_if_present(paste0(stem, ".csv"), TRUE)
season <- unique(saved$season)
source_week <- unique(saved$week)
week <- source_week
model_week <- unique(saved$model_week)
recent_share <- unique(saved$coach_recent_share)
stopifnot(length(season) == 1L, length(week) == 1L, length(model_week) == 1L,
          length(recent_share) == 1L, length(source_model_as_of) == 1L,
          !is.na(source_model_as_of))
updated_snapshot <- length(args) >= 4L
if (updated_snapshot) {
  target_week <- suppressWarnings(as.numeric(args[4]))
  if (!is.finite(target_week) || target_week != floor(target_week) || target_week <= source_week) {
    stop("target_week must be an integer later than the saved model's source week.")
  }
  week <- as.integer(target_week)
}
as_of <- if (updated_snapshot) Sys.time() else source_model_as_of
# Reusing fitted parameters does not require repeating the historical fit. This
# replay checks prediction-code compatibility against the serialized source card.
assert_unique_keys(saved, "game_id", "saved source features")
assert_unique_keys(published, "game_id", "saved source predictions")
published_margin <- published$expected_margin[match(saved$game_id, published$game_id)]
saved_margin <- predict(bundle$model, saved)$expected_margin
source_prediction_error <- max(abs(saved_margin - published_margin))
stopifnot(all(is.finite(published_margin)), all(is.finite(saved_margin)),
          is.finite(source_prediction_error), source_prediction_error < 1e-8)
out_dir <- if (length(args) >= 2L) args[2] else file.path(
  "cfb_v3", "output", season, paste0("week_", week, "_rankings_trial"),
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir, winslash = "/")
say <- function(...) { cat(..., "\n"); flush.console() }
write.csv(data.frame(game_id = saved$game_id, expected_margin = published_margin,
  replayed_margin = saved_margin, error = saved_margin - published_margin),
  file.path(out_dir, "source_prediction_parity.csv"), row.names = FALSE)
say("Replayed", nrow(saved), "source Week", source_week,
    "saved margins; maximum difference", source_prediction_error)

# Reconstruct all FBS teams from the same cached inputs and chronological cutoff.
# Cache reads are explicit: a missing file fails rather than pulling newer data.
cache_files <- c(
  schedule = file.path("cfb_v2/cache/schedules", paste0("cfbd_schedule_", season, ".rds")),
  membership = file.path("cfb_v2/cache/teams", paste0("cfbd_fbs_teams_", season, ".rds")),
  pbp = file.path("cfb_v2/cache/pbp", paste0("pbp_", season, "_compact.rds")),
  coaches = file.path("cfb_v2/cache/coaches", paste0("current_fbs_coaches_", season, ".rds")))
if (!all(file.exists(cache_files))) stop("Missing offline source cache: ",
  paste(cache_files[!file.exists(cache_files)], collapse = ", "))
source_files <- c(model_path, paste0(stem, "_features.csv"), paste0(stem, ".csv"),
  unname(cache_files), file.path(config$data_dir, c("historical_games.csv",
  "historical_team_games.csv", "historical_fbs_membership.csv", "preseason_team_priors.csv")),
  file.path(config$inbox_dir, c("coach_history.csv", "coach_history_manual.csv",
                              "coach_assignment_overrides.csv")),
  list.files("cfb_v2", pattern = "\\.R$", full.names = TRUE),
  "cfb_v3/experiments/model_rankings_trial.R",
  if (length(args) >= 3L) args[3] else character())
source_files <- source_files[file.exists(source_files)]
source_hashes <- tools::md5sum(source_files)
raw <- readRDS(cache_files["schedule"])
membership <- readRDS(cache_files["membership"])
membership$team <- canonical_team(membership$team)
membership <- membership[membership$season == season & membership$classification == "fbs", ]
assert_unique_keys(membership, "team", "current FBS membership")
teams <- sort(membership$team)
n <- length(teams)
pbp <- readRDS(cache_files["pbp"])
coaches <- readRDS(cache_files["coaches"])
universe <- derive_games_from_pbp(pbp, schedule = raw, fbs_membership = membership)
if (updated_snapshot) {
  target_rows <- which(universe$season == season & universe$week == week &
                         universe$postseason_type == "regular")
  model_week <- unique(universe$model_week[target_rows])
  if (length(model_week) != 1L || !is.finite(model_week)) {
    stop("The target regular-season week must have one chronological model week in the cache.")
  }
  if (model_week <= unique(saved$model_week)) stop("Target chronological cutoff did not advance.")
}
completed <- filter_completed_historical_games(universe)
completed <- completed[completed$model_week < model_week & completed$kickoff < as_of, ]
stopifnot(nrow(completed) > 0L, all(completed$model_week < model_week),
          all(completed$kickoff < as_of))
if (any(raw_history_eligible(completed) & !completed$pbp_available)) {
  stop("Prior-week FBS games are missing play-by-play.")
}
unfinished <- universe[universe$model_week < model_week & universe$kickoff < as_of &
  !is.na(universe$completed) & !universe$completed, ]
if (nrow(unfinished)) stop("Earlier scheduled FBS games are not marked final; verify source completeness.")
history_games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
history_games$kickoff <- parse_utc_datetime(history_games$kickoff)
history_games <- history_games[history_games$season < season, ]
historical_team_games <- read_csv_if_present(
  file.path(config$data_dir, "historical_team_games.csv"), TRUE)
historical_team_games$kickoff <- parse_utc_datetime(historical_team_games$kickoff)
historical_team_games <- historical_team_games[historical_team_games$season < season, ]
say("Building", n, "FBS snapshots; current-season history ends at model week", model_week - 1L)
efficiency <- build_team_game_efficiencies_v2(
  pbp[pbp$game_id %in% completed$game_id, ], completed,
  turnover_prior_as_of(historical_team_games, history_games, season))
games <- bind_rows_fill(history_games, completed)
team_games <- bind_rows_fill(historical_team_games, efficiency)
flags <- combine_membership(membership, read_csv_if_present(
  file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
transition_priors <- fbs_bridge_prior_rows(team_games, flags, config, season)
power <- bind_rows_fill(cached_power_rows(history_games),
  power_rating_for_week(games, season, model_week, week, config))

neutral_schedule <- function(home, away, prefix) standardize_schedule(data.frame(
  game_id = paste0(prefix, seq_along(home)), season = season, week = week,
  model_week = model_week, coach_lookup_week = week, kickoff = as_of,
  home = home, away = away, neutral_site = TRUE, venue = "Neutral field",
  home_level = "fbs", away_level = "fbs", postseason_type = "regular",
  conference_championship = FALSE, is_cfp = FALSE,
  home_score = NA_real_, away_score = NA_real_, stringsAsFactors = FALSE))
# One target per team. The synthetic cycle covers teams missing from the real slate.
coverage <- neutral_schedule(teams, c(teams[-1], teams[1]), "ranking_snapshot_")
target_rows <- data.frame(game_id = coverage$game_id, team = teams, season = season,
  week = week, model_week = model_week, kickoff = as_of, offense_epa = NA_real_,
  defense_epa = NA_real_, net_efficiency = NA_real_)
team_history <- team_games[team_games$team %in% teams & team_games$season >= season - 3L, ]
snapshots <- build_team_pregame_snapshots(bind_rows_fill(team_history, target_rows),
  bind_rows_fill(games, coverage), power, config, transition_priors)$snapshots
snapshots <- snapshots[snapshots$game_id %in% coverage$game_id, ]
team_features <- snapshots[setdiff(names(snapshots), "game_id")]
transitions <- fbs_transition_teams(flags)
if (nrow(transitions)) {
  bridge <- calibrate_fbs_bridge(historical_team_games, flags, config)
  team_features <- apply_fbs_bridge_features(team_features, transitions, bridge)
}
inputs <- read_v2_inputs(config, strict = FALSE)
assignments <- current_fbs_coach_assignments(teams, coaches, season,
                                          inputs$coach_assignment_overrides)
coach_history <- extend_current_coach_history(inputs$coach_history, completed,
                                              games, assignments, season, config)
team_features <- attach_coach_ratings(coverage, team_features, assignments,
                                      coach_history, recent_share, config)
stopifnot(nrow(team_features) == n, setequal(team_features$team, teams))
priors <- read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE)
make_features <- function(schedule) attach_preseason_features(
  make_matchup_features(schedule, team_features, team_features, recent_share, config),
  priors, config, features = config$preseason$production_features,
  prefix = "ps_", require_coverage = TRUE)

# Frozen replay mode must rebuild the exact saved slate. Updated mode deliberately
# builds a later week's features, so its replay is limited to the original saved
# feature CSV above; it must not claim those later features equal the old ones.
feature_errors <- NULL
max_prediction_error <- source_prediction_error
replay_scope <- if (updated_snapshot) "source_snapshot" else "rebuilt_snapshot"
if (!updated_snapshot) {
  schedule_columns <- c(names(v2_input_templates()$schedule), "model_week", "coach_lookup_week")
  actual_schedule <- saved[intersect(schedule_columns, names(saved))]
  actual_schedule$kickoff <- parse_utc_datetime(actual_schedule$kickoff)
  rebuilt <- make_features(actual_schedule)
  comparison_columns <- intersect(football_feature_names(saved), names(rebuilt))
  feature_errors <- vapply(comparison_columns, function(column) {
    a <- saved[[column]]; b <- rebuilt[[column]]
    if (!identical(is.finite(a), is.finite(b))) return(Inf)
    if (!any(is.finite(a))) return(0)
    max(abs(a[is.finite(a)] - b[is.finite(b)]))
  }, numeric(1))
  write.csv(data.frame(feature = names(feature_errors), max_error = feature_errors),
            file.path(out_dir, "feature_parity.csv"), row.names = FALSE)
  rebuilt_margin <- predict(bundle$model, rebuilt)$expected_margin
  max_prediction_error <- max(abs(rebuilt_margin - published_margin))
  if (!is.finite(max_prediction_error) || max_prediction_error > 1e-6 ||
      any(feature_errors > 1e-6)) {
    print(sort(feature_errors, decreasing = TRUE)[seq_len(min(12L, length(feature_errors)))])
    stop("Rebuilt features do not reproduce the saved slate; max margin error = ",
         max_prediction_error, ". See feature_parity.csv. No ranking was issued.")
  }
  say("Rebuilt all", nrow(saved), "saved weekly margins; maximum difference", max_prediction_error)
} else {
  say("Fresh Week", week, "features use the saved Week", source_week,
      "fit; source-card replay verified, no target-week feature parity claimed.")
}

pair_index <- expand.grid(home_index = seq_len(n), away_index = seq_len(n))
pair_index <- pair_index[pair_index$home_index != pair_index$away_index, ]
pairs <- neutral_schedule(teams[pair_index$home_index], teams[pair_index$away_index], "ranking_pair_")
pair_features <- make_features(pairs)
stopifnot(all(pair_features$home_field_points == 0))
pair_prediction <- predict(bundle$model, pair_features)
raw_margin <- pair_prediction$expected_margin
if (week >= 5L) {
  stopifnot(all(pair_features$preseason_weight == 0),
            all(pair_prediction$preseason_challenger_share == 0))
}
stopifnot(length(raw_margin) == n * (n - 1L), all(is.finite(raw_margin)))
raw_matrix <- matrix(0, n, n, dimnames = list(teams, teams))
raw_matrix[cbind(pair_index$home_index, pair_index$away_index)] <- raw_margin
# Remove nominal home-label bias using both orientations of each neutral matchup.
neutral_matrix <- (raw_matrix - t(raw_matrix)) / 2
ratings <- rowMeans(neutral_matrix) # Identical opponent universe; self matchup is zero.
residuals <- neutral_matrix - outer(ratings, ratings, "-")
stopifnot(max(abs(neutral_matrix + t(neutral_matrix))) < 1e-10,
          all(diag(neutral_matrix) == 0), abs(mean(ratings)) < 1e-10)
record <- function(team) {
  home <- completed$home == team; away <- completed$away == team
  c(wins = sum(completed$margin[home] > 0) + sum(completed$margin[away] < 0),
    losses = sum(completed$margin[home] < 0) + sum(completed$margin[away] > 0))
}
records <- t(vapply(teams, record, numeric(2)))
rankings <- data.frame(team = teams, conference = membership$conference[match(teams, membership$team)],
  wins = records[, "wins"], losses = records[, "losses"], rating = unname(ratings),
  source_games = team_features$source_games[match(teams, team_features$team)])
rankings <- rankings[order(-rankings$rating, rankings$team), ]
rankings <- cbind(rank = seq_len(n), rankings)
rownames(rankings) <- NULL
previous_path <- if (length(args) >= 3L && nzchar(args[3])) args[3] else NULL
if (!is.null(previous_path)) {
  previous <- read_csv_if_present(previous_path, TRUE)
  assert_columns(previous, c("rank", "team", "rating"), "previous rankings")
  assert_unique_keys(previous, "team", "previous rankings")
  previous$team <- canonical_team(previous$team)
  previous_index <- match(rankings$team, previous$team)
  if (anyNA(previous_index) || !setequal(previous$team, rankings$team)) {
    stop("Weekly movement requires the same complete FBS team universe.")
  }
  rankings$previous_rank <- previous$rank[previous_index]
  rankings$rank_change <- rankings$previous_rank - rankings$rank
  rankings$rating_change <- rankings$rating - previous$rating[previous_index]
}
pair_results <- data.frame(team = pairs$home, opponent = pairs$away,
  raw_margin = raw_margin,
  neutral_margin = neutral_matrix[cbind(pair_index$home_index, pair_index$away_index)])
write.csv(rankings, file.path(out_dir, "rankings.csv"), row.names = FALSE)
write.csv(head(rankings, 25L), file.path(out_dir, "top25.csv"), row.names = FALSE)
write.csv(team_features, file.path(out_dir, "team_snapshots.csv"), row.names = FALSE)
write.csv(pair_results, file.path(out_dir, "neutral_matchups.csv"), row.names = FALSE)
write.csv(completed[c("game_id", "week", "model_week", "away", "home", "away_score",
                      "home_score", "pbp_available")],
          file.path(out_dir, "included_games.csv"), row.names = FALSE)
if (!identical(unname(source_hashes), unname(tools::md5sum(source_files)))) {
  stop("A source file changed during the run; repeat on a frozen snapshot.")
}
qa <- list(season = season, week = week, model_week = model_week,
  as_of = format(as_of, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), team_count = n,
  source_week = source_week,
  source_model_as_of = format(source_model_as_of, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  snapshot_mode = if (updated_snapshot) "updated_features_saved_fit" else "frozen_replay",
  model_retrained = FALSE, replay_scope = replay_scope,
  pair_count = nrow(pairs), unique_matchups = nrow(pairs) / 2L,
  actual_games_verified = nrow(saved), max_prediction_error = max_prediction_error,
  max_feature_error = if (is.null(feature_errors)) NULL else max(feature_errors),
  neutral_label_bias = median((raw_matrix + t(raw_matrix))[upper.tri(raw_matrix)] / 2),
  max_matchup_residual = max(abs(residuals)),
  mean_matchup_residual = mean(abs(residuals[upper.tri(residuals)])),
  model_path = normalizePath(model_path, winslash = "/"),
  method = "Mean symmetric neutral margin over all FBS teams, including a zero self matchup.",
  cutoff_policy = "Current-season results strictly before the target chronological model week and snapshot as_of.",
  production_model = if (week >= 5L) "Saved v3 foundation ridge; preseason roster blend disabled; no residual forest."
    else "Saved v3 ridge foundation and preseason roster blend; no residual forest.",
  preseason_component_share = unique(pair_prediction$preseason_challenger_share),
  preseason_feature_weight = unique(preseason_feature_weight(week, config)),
  completed_week = max(completed$week),
  latest_completed_week = max(completed$week),
  current_season_games_included = nrow(completed),
  completed_fbs_games_included = sum(raw_history_eligible(completed)),
  missing_completed_fbs_pbp = sum(raw_history_eligible(completed) & !completed$pbp_available),
  previous_rankings_path = previous_path,
  ap_poll_used_as_predictor = FALSE,
  source_hashes = as.list(source_hashes), source_files_unchanged = TRUE)
jsonlite::write_json(qa, file.path(out_dir, "qa.json"), pretty = TRUE, auto_unbox = TRUE,
                     digits = 12, null = "null")
say("Output:", out_dir)
print(head(rankings, 25L), row.names = FALSE)
say("Largest difference between rating gap and direct neutral forecast:", max(abs(residuals)))
