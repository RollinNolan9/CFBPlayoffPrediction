# Isolated Week 6 rankings from refreshed official feeds and the saved relative fit.
# Rscript --vanilla cfb_v3/update_week6_rankings_relative.R --phase=fetch
# Rscript --vanilla cfb_v3/update_week6_rankings_relative.R --phase=build --output=PATH
args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 1L) stop("Duplicate option: ", name)
  if (length(hit)) sub("^[^=]+=", "", hit) else default
}
phase <- option("phase", "fetch")
stopifnot(phase %in% c("fetch", "probe", "preflight", "build"))
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
modules <- c("config", "store", "features", "coaches", "adapters", "models",
  "coach_migration", "historical_data", "preseason", "bridge", "workflow", "live_data")
for (module in modules) source(file.path(root, "cfb_v2", paste0(module, ".R")))
source(file.path(root, "cfb_v3/experiments/opponent_relative_features.R"))
say <- function(...) { cat(..., "\n"); flush.console() }
utc <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
old_dir <- file.path(root, "cfb_v3/output/2026/week_5_rankings_relative/20260927T221943Z")
config_bundle_path <- file.path(root,
  "cfb_v3/output/2026/week_5_relative_preliminary/20260930T211701Z/model.rds")
config <- readRDS(config_bundle_path)$config
config$project_dir <- root
config$data_dir <- file.path(root, "cfb_v3/data")
config$inbox_dir <- file.path(root, "cfb_v3/inbox")
out <- option("output", file.path(root, "cfb_v3/output/2026/week_6_rankings_relative",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")))
if (phase == "fetch" && dir.exists(out)) stop("Refusing to overwrite an existing snapshot.")
if (phase != "fetch" && !dir.exists(out)) stop("Fetch the isolated inputs first.")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, winslash = "/", mustWork = TRUE)
write_csv <- function(x, filename) write.csv(x, file.path(out, filename), row.names = FALSE, na = "")
write_json <- function(x, filename) jsonlite::write_json(x, file.path(out, filename),
  pretty = TRUE, auto_unbox = TRUE, digits = 14, null = "null")
say("OUTPUT:", out)

if (phase == "fetch") {
  if (file.exists(file.path(root, ".Renviron"))) readRenviron(file.path(root, ".Renviron"))
  stopifnot(nzchar(Sys.getenv("CFBD_API_KEY")), requireNamespace("cfbfastR", quietly = TRUE))
  protected <- c(config_bundle_path, file.path(old_dir, c("rankings.csv", "ranking_fit.rds")),
    list.files(file.path(root, "cfb_v2/cache"), recursive = TRUE, full.names = TRUE),
    file.path(config$data_dir, c("historical_games.csv", "historical_team_games.csv",
      "historical_fbs_membership.csv")),
    file.path(config$inbox_dir, c("coach_history.csv", "coach_history_manual.csv",
      "coach_assignment_overrides.csv")))
  protected <- protected[file.exists(protected) & !dir.exists(protected)]
  hashes <- unname(tools::md5sum(protected))
  write_csv(data.frame(path = protected, md5 = hashes), "protected_input_hashes.csv")
  isolated <- config
  isolated$project_dir <- out
  cache_root <- file.path(out, "cfb_v2/cache")
  dir.create(file.path(cache_root, "schedules"), recursive = TRUE)
  say("Retrieving authoritative CFBD regular-season schedule.")
  schedule <- as.data.frame(cfbfastR::cfbd_game_info(2026L, season_type = "regular"),
                            stringsAsFactors = FALSE)
  stopifnot(nrow(schedule) > 0L, !anyDuplicated(schedule$game_id))
  saveRDS(schedule, file.path(cache_root, "schedules/cfbd_schedule_2026.rds"), compress = "xz")
  schedule_at <- Sys.time()
  say("Retrieving CFBD FBS membership and official cfbfastR EPA feed.")
  membership <- load_cfbd_fbs_teams_season(2026L, isolated, refresh = TRUE)
  pbp <- load_compact_pbp_season(2026L, isolated, refresh = TRUE)
  pbp_at <- Sys.time()
  # Preserve the current known coaching assignments; dates and original source
  # are retained. Recomputing coach ratings below uses newly completed games.
  coach_source <- file.path(root, "cfb_v2/cache/coaches/current_fbs_coaches_2026.rds")
  dir.create(file.path(cache_root, "coaches"), recursive = TRUE)
  stopifnot(file.copy(coach_source, file.path(cache_root, "coaches/current_fbs_coaches_2026.rds")))
  universe <- derive_games_from_pbp(pbp, schedule = schedule, fbs_membership = membership)
  target_week <- unique(universe$model_week[universe$week == 6L & universe$postseason_type == "regular"])
  stopifnot(length(target_week) == 1L, is.finite(target_week))
  as_of <- Sys.time()
  earlier <- universe[universe$model_week < target_week & universe$kickoff < as_of, ]
  completed <- filter_completed_historical_games(earlier)
  counts <- function(ids) as.integer(table(factor(ids, levels = completed$game_id)))
  coverage <- completed[c("game_id", "season", "week", "model_week", "kickoff", "home", "away",
    "home_score", "away_score", "home_level", "away_level", "completed", "pbp_available")]
  coverage$required_fbs_history <- raw_history_eligible(completed)
  coverage$pbp_rows <- counts(pbp$game_id)
  coverage$finite_epa_rows <- counts(pbp$game_id[is.finite(pbp$EPA)])
  competitive <- historical_competitive_plays(pbp[pbp$game_id %in% completed$game_id, ])
  coverage$competitive_rows <- counts(competitive$game_id)
  coverage$competitive_finite_epa_rows <- counts(competitive$game_id[is.finite(competitive$EPA)])
  coverage$source <- "Official cfbfastR season EPA feed; no calculated-EPA supplement"
  missing <- coverage$required_fbs_history & (coverage$pbp_rows == 0L |
    coverage$competitive_finite_epa_rows == 0L)
  unfinished <- earlier[is.na(earlier$completed) | !earlier$completed, ]
  write_csv(coverage, "input_coverage.csv")
  write_csv(coverage[missing, ], "missing_required_pbp.csv")
  write_csv(unfinished, "unfinished_earlier_games.csv")
  write_csv(completed, "source_games.csv")
  saveRDS(universe, file.path(out, "schedule_universe.rds"))
  info <- list(status = if (any(missing) || nrow(unfinished)) "coverage_incomplete" else "inputs_ready",
    season = 2026L, week = 6L, model_week = target_week, as_of = utc(as_of),
    schedule_retrieved_at = utc(schedule_at), pbp_retrieved_at = utc(pbp_at),
    cfbd_api_requests = 2L, official_pbp_rows = nrow(pbp),
    completed_prior_games = nrow(completed), required_fbs_games = sum(coverage$required_fbs_history),
    missing_required_games = sum(missing), unfinished_earlier_games = nrow(unfinished),
    previous_espn_supplement_used = FALSE, coaching_assignments = "Existing cached assignments retained; not refreshed",
    sources = list(schedule = "https://api.collegefootballdata.com/games?year=2026&seasonType=regular",
      membership = "https://api.collegefootballdata.com/teams/fbs?year=2026",
      epa = "cfbfastR::load_cfb_pbp(2026) official season feed",
      coaches_original_path = coach_source))
  stopifnot(identical(unname(tools::md5sum(protected)), hashes))
  write_json(info, "input_manifest.json")
  say("Coverage:", nrow(completed), "completed games;", sum(coverage$required_fbs_history),
      "required FBS games;", sum(missing), "missing official EPA;", nrow(unfinished), "unfinished.")
  if (any(missing)) print(coverage[missing, c("game_id", "week", "away", "home", "pbp_rows", "finite_epa_rows")], row.names = FALSE)
  say("INPUT STATUS:", info$status)
  quit(status = 0L)
}

if (phase == "probe") {
  info <- jsonlite::read_json(file.path(out, "input_manifest.json"), simplifyVector = TRUE)
  coach_path <- file.path(out, "cfb_v2/cache/coaches/current_fbs_coaches_2026.rds")
  old_coaches <- readRDS(coach_path)
  saveRDS(old_coaches, file.path(out, "coaches_before_refresh.rds"))
  fresh_coaches <- pull_current_fbs_coaches(2026L, coach_path, refresh = TRUE)
  info$coaching_assignments <- "Refreshed public current FBS coaching assignments; manual week-effective overrides retained"
  info$coaches_retrieved_at <- utc(Sys.time())
  write_csv(fresh_coaches, "current_coaches.csv")
  write_json(info, "input_manifest.json")
  say("Refreshed coaching source:", nrow(fresh_coaches), "rows.")
  release_url <- "https://api.github.com/repos/sportsdataverse/sportsdataverse-data/releases/tags/cfbfastR_cfb_pbp"
  response <- httr::GET(release_url, httr::user_agent("CFB-ranking-source-audit"))
  httr::stop_for_status(response)
  release <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  assets <- release$assets
  selected <- assets[grepl("2026", assets$name), c("name", "size", "updated_at", "browser_download_url")]
  write_csv(selected, "official_release_assets.csv")
  say("Official current-season release assets:")
  print(selected, row.names = FALSE)
  # Probe the live CFBD-based function contract only; running its estimator would
  # be a calculated-EPA alternative, not an already-published official feed.
  suppressPackageStartupMessages(library(cfbfastR))
  writeLines(deparse(cfbfastR::cfbd_pbp_data), file.path(out, "official_per_game_function.txt"))
  say("Per-game function contract saved; no substitute EPA applied.")
  quit(status = 0L)
}

info <- jsonlite::read_json(file.path(out, "input_manifest.json"), simplifyVector = TRUE)
has_supplement <- isTRUE(info$calculated_supplement_games == 1L)
if (has_supplement) {
  supplement <- jsonlite::read_json(file.path(out, "epa_supplement.json"), simplifyVector = TRUE)
  stopifnot(supplement$epa_status == "calculated_supplement", supplement$game_id == "401860899",
    supplement$official_rows_unchanged, supplement$school_boxscore_exact_match)
}
if (phase == "build" && info$status != "inputs_ready") stop("Official EPA coverage is incomplete; no ranking issued.")
if (file.exists(file.path(out, "rankings.csv"))) stop("Refusing to overwrite completed rankings.")
season <- 2026L
week <- 6L
model_week <- as.integer(info$model_week)
as_of <- parse_utc_datetime(info$as_of)
old_qa <- jsonlite::read_json(file.path(old_dir, "qa.json"), simplifyVector = TRUE)
previous <- read_csv_if_present(file.path(old_dir, "rankings.csv"), TRUE)
research <- readRDS(file.path(old_dir, "ranking_fit.rds"))
fit <- research$model
removed <- c("source_games_diff", "games_played_diff")
stopifnot(length(fit$recipe$features) == 19L,
  identical(fit$recipe$features, old_qa$features), !any(removed %in% fit$recipe$features),
  !any(grepl("ap_poll|ap_rank", fit$recipe$features)), fit$lambda == 32,
  fit$training_rows == 4908L, identical(sort(unique(research$training$season)), 2020:2025),
  old_qa$coach_recent_share == .65, all(research$training$season < season))
protected <- read_csv_if_present(file.path(out, "protected_input_hashes.csv"), TRUE)
stopifnot(identical(unname(tools::md5sum(protected$path)), protected$md5))
cache <- file.path(out, "cfb_v2/cache")
cache_files <- c(schedule = file.path(cache, "schedules/cfbd_schedule_2026.rds"),
  membership = file.path(cache, "teams/cfbd_fbs_teams_2026.rds"),
  pbp = file.path(cache, "pbp/pbp_2026_compact.rds"),
  coaches = file.path(cache, "coaches/current_fbs_coaches_2026.rds"))
source_files <- unique(c(unname(cache_files), config_bundle_path,
  file.path(old_dir, c("rankings.csv", "ranking_fit.rds", "qa.json")),
  file.path(config$data_dir, c("historical_games.csv", "historical_team_games.csv", "historical_fbs_membership.csv")),
  file.path(config$inbox_dir, c("coach_history.csv", "coach_history_manual.csv", "coach_assignment_overrides.csv")),
  file.path(root, "cfb_v2", paste0(modules, ".R")),
  file.path(root, c("cfb_v3/experiments/opponent_relative_features.R", "cfb_v3/update_week6_rankings_relative.R"))))
source_hashes <- unname(tools::md5sum(source_files))
if (has_supplement) {
  source_files <- unique(c(source_files, file.path(out, "epa_supplement.json"),
    file.path(root,c("cfb_v3/week6_school_reconstruction.py","cfb_v3/week6_calculate_school_epa.R",
      "cfb_v3/week6_append_school_supplement.R")),
    list.files(file.path(out, "espn_supplement"), full.names = TRUE)))
  source_hashes <- unname(tools::md5sum(source_files))
}
stopifnot(!anyNA(source_hashes))
write_csv(data.frame(path = source_files, md5 = source_hashes), "input_hashes.csv")
membership <- readRDS(cache_files[["membership"]])
membership$team <- canonical_team(membership$team)
membership <- membership[membership$season == season & membership$classification == "fbs", ]
assert_unique_keys(membership, "team", "Current FBS membership")
teams <- sort(membership$team)
n <- length(teams)
stopifnot(n == 138L, setequal(teams, previous$team))
pair_index <- expand.grid(home_index = seq_len(n), away_index = seq_len(n))
pair_index <- pair_index[pair_index$home_index != pair_index$away_index, ]
rank_matrix <- function(x) {
  raw_margin <- predict(fit, x)
  stopifnot(length(raw_margin) == n * (n - 1L), all(is.finite(raw_margin)),
    !anyDuplicated(paste(x$home, x$away, sep = "|")), all(x$home != x$away))
  raw <- matrix(0, n, n, dimnames = list(teams, teams))
  indices <- cbind(match(x$home, teams), match(x$away, teams))
  stopifnot(!anyNA(indices))
  raw[indices] <- raw_margin
  neutral <- (raw - t(raw)) / 2
  ratings <- rowMeans(neutral)
  stopifnot(max(abs(neutral + t(neutral))) < 1e-10, abs(mean(ratings)) < 1e-10)
  list(raw_margin = raw_margin, ratings = ratings, raw = raw, neutral = neutral,
    pair_neutral = neutral[indices], residual = neutral - outer(ratings, ratings, "-"))
}
# Replay the exact saved pair features before computing any fresh-week features.
replay <- rank_matrix(research$matchup_features)
replay_error <- max(abs(replay$ratings - previous$rating[match(teams, previous$team)]))
stopifnot(replay_error < 1e-8)
say("Saved Week 5 rankings replay error:", replay_error)
if (phase == "preflight") {
  write_json(list(status = "saved_fit_verified_awaiting_complete_inputs",
    checked_at = utc(Sys.time()), source_ranking_replay_error = replay_error,
    model_family = "uncapped_opponent_relative", feature_count = length(fit$recipe$features),
    lambda = fit$lambda, training_rows = fit$training_rows, team_count = n,
    model_retrained = FALSE, protected_inputs_unchanged = TRUE,
    inputs_status = info$status, missing_required_games = info$missing_required_games,
    rankings_issued = FALSE), "preflight.json")
  say("Saved fit, 138-team membership, source replay, and protected hashes verified; no rankings issued.")
  quit(status = 0L)
}
pbp <- readRDS(cache_files[["pbp"]])
raw_schedule <- readRDS(cache_files[["schedule"]])
coaches <- readRDS(cache_files[["coaches"]])
universe <- derive_games_from_pbp(pbp, schedule = raw_schedule, fbs_membership = membership)
completed <- filter_completed_historical_games(universe)
completed <- completed[completed$model_week < model_week & completed$kickoff < as_of, ]
stopifnot(nrow(completed) == info$completed_prior_games, max(completed$week) == 5L,
  all(!is.na(completed$feature_week_start)), all(completed$kickoff < as_of),
  all(completed$model_week < model_week), !any(raw_history_eligible(completed) & !completed$pbp_available))
history_games <- read_csv_if_present(file.path(config$data_dir, "historical_games.csv"), TRUE)
history_games$kickoff <- parse_utc_datetime(history_games$kickoff)
history_games <- history_games[history_games$season < season, ]
historical_team_games <- read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE)
historical_team_games$kickoff <- parse_utc_datetime(historical_team_games$kickoff)
historical_team_games <- historical_team_games[historical_team_games$season < season, ]
say("Building 138 current FBS snapshots through Week 5.")
efficiency <- build_team_game_efficiencies_v2(pbp[pbp$game_id %in% completed$game_id, ], completed,
  turnover_prior_as_of(historical_team_games, history_games, season))
required <- completed[raw_history_eligible(completed), ]
needed <- c(paste(required$game_id, required$home), paste(required$game_id, required$away))
hit <- match(needed, paste(efficiency$game_id, efficiency$team))
stopifnot(!anyNA(hit), all(is.finite(efficiency$offense_epa[hit])),
  all(is.finite(efficiency$defense_epa[hit])), all(efficiency$scrimmage_plays[hit] > 0))
write_csv(efficiency, "current_team_games.csv")
games <- bind_rows_fill(history_games, completed)
team_games <- bind_rows_fill(historical_team_games, efficiency)
flags <- combine_membership(membership, read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
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
if (nrow(transitions)) team_features <- apply_fbs_bridge_features(team_features,
  transitions, calibrate_fbs_bridge(historical_team_games, flags, config))
coach_history <- read_csv_if_present(file.path(config$inbox_dir, "coach_history.csv"), TRUE)
manual <- read_csv_if_present(file.path(config$inbox_dir, "coach_history_manual.csv"), TRUE)
if (nrow(manual)) coach_history <- merge_manual_coach_history(coach_history, manual)
overrides <- read_csv_if_present(file.path(config$inbox_dir, "coach_assignment_overrides.csv"), TRUE)
assignments <- current_fbs_coach_assignments(teams, coaches, season, overrides)
coach_history <- extend_current_coach_history(coach_history, completed, games, assignments, season, config)
team_features <- attach_coach_ratings(coverage, team_features, assignments, coach_history, .65, config)
stopifnot(nrow(team_features) == n, setequal(team_features$team, teams))
coverage$feature_week_start <- as_of
relative <- opponent_relative_build(completed, coverage)
values <- setNames(relative$features$home_opponent_relative_margin, teams)
stopifnot(max(abs(relative$features$away_opponent_relative_margin - values[coverage$away])) == 0)
team_features$opponent_relative_margin <- unname(values[team_features$team])
pairs <- neutral_schedule(teams[pair_index$home_index], teams[pair_index$away_index], "relative_rank_pair_")
x <- make_matchup_features(pairs, team_features, team_features, .65, config)
x$opponent_relative_margin_diff <- unname(values[x$home] - values[x$away])
stopifnot(all(x$home_field_points == 0), all(x$preseason_weight == 0),
  all(is.finite(x$opponent_relative_margin_diff)))
result <- rank_matrix(x)
changed_counts <- x
changed_counts$source_games_diff <- 999
changed_counts$games_played_diff <- -999
count_error <- max(abs(predict(fit, changed_counts) - result$raw_margin))
stopifnot(count_error == 0)
record <- function(team) {
  home <- completed$home == team; away <- completed$away == team
  c(wins = sum(completed$margin[home] > 0) + sum(completed$margin[away] < 0),
    losses = sum(completed$margin[home] < 0) + sum(completed$margin[away] > 0))
}
records <- t(vapply(teams, record, numeric(2)))
rankings <- data.frame(team = teams, conference = membership$conference[match(teams, membership$team)],
  wins = records[, "wins"], losses = records[, "losses"], rating = unname(result$ratings),
  source_games = team_features$source_games[match(teams, team_features$team)])
rankings <- rankings[order(-rankings$rating, rankings$team), ]
rankings <- cbind(rank = seq_len(n), rankings)
idx <- match(rankings$team, previous$team)
rankings$previous_rank <- previous$rank[idx]
rankings$rank_change <- rankings$previous_rank - rankings$rank
rankings$rating_change <- rankings$rating - previous$rating[idx]
rownames(rankings) <- NULL
write_csv(rankings, "rankings.csv")
write_csv(head(rankings, 25L), "top25.csv")
write_csv(team_features, "team_snapshots.csv")
write_csv(completed, "included_games.csv")
write_csv(relative$game_audit, "game_audit.csv")
write_csv(relative$team_audit, "team_audit.csv")
write_csv(assignments, "coach_assignments.csv")
write_csv(data.frame(team = x$home, opponent = x$away, raw_margin = result$raw_margin,
  neutral_margin = result$pair_neutral), "neutral_matchups.csv")
write_csv(data.frame(feature = names(fit$coefficients), coefficient = unname(fit$coefficients)), "coefficients.csv")
saveRDS(list(model = fit, config = config, baseline_model = research$baseline_model,
  training = research$training, weights = research$weights,
  current_relative = relative, matchup_features = x,
  source_fit = file.path(old_dir, "ranking_fit.rds")), file.path(out, "ranking_fit.rds"))
stopifnot(identical(unname(tools::md5sum(source_files)), source_hashes),
  identical(unname(tools::md5sum(protected$path)), protected$md5))
qa <- list(status = "complete", season = season, week = week, model_week = model_week,
  completed_week = 5L, latest_completed_week = 5L, as_of = utc(as_of), generated_at = utc(Sys.time()),
  team_count = n, pair_count = nrow(x), unique_matchups = nrow(x) / 2L,
  source_week = old_qa$week, source_model_as_of = old_qa$as_of,
  replay_scope = "source_snapshot", snapshot_mode = "updated_features_saved_fit",
  max_prediction_error = replay_error, source_ranking_replay_error = replay_error,
  replay_note = "Exact saved Week 5 pair features reproduce prior relative ratings; this is rating replay, not a live score-card replay.",
  model_family = "uncapped_opponent_relative", model_retrained = FALSE,
  model_path = file.path(old_dir, "ranking_fit.rds"), features = fit$recipe$features,
  removed_features = removed, margin_cap = "uncapped", training_rows = fit$training_rows,
  training_seasons = sort(unique(research$training$season)), lambda = 32, coach_recent_share = .65,
  count_perturbation_prediction_error = count_error,
  max_matchup_residual = max(abs(result$residual)),
  mean_matchup_residual = mean(abs(result$residual[upper.tri(result$residual)])),
  current_season_games_included = nrow(completed), completed_fbs_games_included = nrow(required),
  missing_completed_fbs_pbp = 0L, both_teams_finite_epa = TRUE,
  both_teams_finite_official_epa = !has_supplement,
  official_fbs_games = nrow(required) - as.integer(has_supplement),
  calculated_supplement_games = as.integer(has_supplement),
  current_relative = relative$qa,
  method = "Mean symmetric neutral margin across all 138 FBS teams, including a zero self matchup.",
  cutoff_policy = "Completed games strictly before Week 6 chronological cutoff and snapshot as_of.",
  relative_policy = relative$qa$definition,
  movement_note = "Arrows compare with the prior saved Week 5 opponent-relative run, which differs from the original-model rankings previously published. The fit is unchanged; movement reflects Week 5 games and upstream revisions, including official EPA now replacing the Oklahoma State–West Virginia supplement.",
  data_note = if (has_supplement) "Official cfbfastR EPA and CFBD finals through Week 5, with one labeled calculated-EPA supplement: Oregon State at Colorado State reconstructed from official CSU play-by-play after corrupted ESPN data failed validation. Oklahoma State–West Virginia now uses official EPA, replacing its prior calculated supplement." else "Refreshed official cfbfastR EPA feed and CFBD finals through Week 5; no calculated-EPA supplements. Oklahoma State–West Virginia now uses official EPA, replacing the prior snapshot's calculated supplement.",
  previous_rankings_path = file.path(old_dir, "rankings.csv"),
  preseason_roster_component_used = FALSE, ap_poll_used_as_predictor = FALSE,
  source_files_unchanged = TRUE, production_model_changed = FALSE, published_site_changed = FALSE)
write_json(qa, "qa.json")
say("RANKINGS:", file.path(out, "rankings.csv"))
print(head(rankings, 25L), row.names = FALSE)
