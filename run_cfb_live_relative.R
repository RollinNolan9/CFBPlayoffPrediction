# Offline opponent-relative companion to a frozen live Week 5+ prediction card.
# Rscript --vanilla run_cfb_live_relative.R BASELINE_CSV [NEW_OUTPUT_DIRECTORY]
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Supply the frozen baseline prediction CSV.")
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
for (module in c("config", "store", "features", "coaches", "adapters", "models",
  "coach_migration", "historical_data", "preseason", "bridge", "workflow", "live_data", "dashboard"))
  source(file.path("cfb_v2", paste0(module, ".R")))
source("cfb_v3/experiments/opponent_relative_features.R")
source("cfb_v3/injury_overlay.R")
say <- function(...) { cat(..., "\n"); flush.console() }
base_path <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
feature_path <- sub("\\.csv$", "_features.csv", base_path)
bundle_path <- sub("\\.csv$", "_model.rds", base_path)
base <- read_csv_if_present(base_path, TRUE)
x <- read_csv_if_present(feature_path, TRUE)
bundle <- readRDS(bundle_path)
config <- bundle$config
season <- unique(base$season); week <- unique(base$week)
stopifnot(length(season) == 1L, length(week) == 1L, week >= 5,
  all(base$injury_scenario == "most_likely"), all(base$forced_pick),
  all(base$preseason_challenger_share == 0), all(base$preseason_adjustment == 0))
x <- x[match(base$game_id, x$game_id), ]
stopifnot(identical(as.character(base$game_id), as.character(x$game_id)),
  all(x$home == base$home), all(x$away == base$away), all(x$preseason_weight == 0))
as_of <- unique(parse_utc_datetime(base$as_of))
stopifnot(length(as_of) == 1L, !is.na(as_of), all(parse_utc_datetime(x$kickoff) > as_of))

# Reuse the exact audited full-training fit that generated the relative rankings.
research_dir <- "cfb_v3/output/2026/week_5_rankings_relative/20260927T221943Z"
relative_dir <- "cfb_v3/output/experiments/opponent_relative/20260927T215407Z"
research <- readRDS(file.path(research_dir, "ranking_fit.rds"))
historical_hashes <- read.csv(file.path(relative_dir, "input_hashes.csv"), stringsAsFactors = FALSE)
stopifnot(identical(unname(tools::md5sum(historical_hashes$path)), historical_hashes$md5))
fit <- research$model
removed <- c("source_games_diff", "games_played_diff")
expected_features <- c(setdiff(bundle$model$foundation$features, removed), "opponent_relative_margin_diff")
stopifnot(identical(fit$recipe$features, expected_features), length(expected_features) == 19L,
  all(research$training$season < season), fit$training_rows == 4908L)
refit <- fit_weighted_ridge(research$training, "margin", expected_features,
  research$weights, bundle$model$foundation$ridge$lambda)
fit_error <- max(abs(refit$coefficients - fit$coefficients))
control_fit <- fit_weighted_ridge(research$training, "margin", bundle$model$foundation$features,
  research$weights, bundle$model$foundation$ridge$lambda)
control_coef_error <- max(abs(control_fit$coefficients - bundle$model$foundation$ridge$coefficients))
control_error <- max(abs(predict(bundle$model, x)$expected_margin - base$expected_margin))
stopifnot(fit_error < 1e-10, control_coef_error < 1e-10, control_error < 1e-10)

cache_root <- file.path(root, "cfb_v2/cache")
cache_paths <- c(schedule = file.path(cache_root, "schedules", paste0("cfbd_schedule_", season, ".rds")),
  membership = file.path(cache_root, "teams", paste0("cfbd_fbs_teams_", season, ".rds")),
  pbp = file.path(cache_root, "pbp", paste0("pbp_", season, "_compact.rds")))
registry_path <- file.path(config$inbox_dir, "player_availability.csv")
paths <- unique(c(base_path, feature_path, bundle_path, cache_paths, historical_hashes$path,
  file.path(research_dir, "ranking_fit.rds"), "run_cfb_live_relative.R", "cfb_v3/injury_overlay.R",
  registry_path[file.exists(registry_path)]))
hashes <- tools::md5sum(paths)
stopifnot(!anyNA(hashes))
selected_pbp <- readRDS(cache_paths[["pbp"]])
if (as_of >= parse_utc_datetime("2026-10-07T19:47:28Z") &&
    any(selected_pbp$game_id == "401858207" & selected_pbp$id_play == 40185820762, na.rm = TRUE))
  stop("Pitt-Miami (OH) contains verified false play 40185820762. Apply the documented school-PBP exclusion before rebuilding.")
universe <- derive_games_from_pbp(selected_pbp,
  schedule = readRDS(cache_paths[["schedule"]]), fbs_membership = readRDS(cache_paths[["membership"]]))
model_week <- unique(x$model_week)
stopifnot(length(model_week) == 1L)
completed <- filter_completed_historical_games(universe)
completed <- completed[completed$model_week < model_week & completed$kickoff < as_of, ]
stopifnot(!any(raw_history_eligible(completed) & !completed$pbp_available),
  all(!is.na(completed$feature_week_start)))
targets <- x
targets$feature_week_start <- as_of
relative <- opponent_relative_build(completed, targets)
candidate_x <- x
for (field in setdiff(names(relative$features), "game_id"))
  candidate_x[[field]] <- relative$features[[field]]
stopifnot(identical(candidate_x[names(x)], x))
predicted <- predict(fit, candidate_x)
count_changed <- candidate_x
count_changed$source_games_diff <- 999
count_changed$games_played_diff <- -999
count_error <- max(abs(predict(fit, count_changed) - predicted))
stopifnot(count_error == 0, all(is.finite(predicted)))

# Only point forecasts are requested. Do not copy production confidence, odds,
# uncertainty or preseason-component diagnostics into this different model.
card <- base
card$expected_margin <- predicted
card$fair_spread <- -predicted
card$ats_edge_home <- predicted + card$market_home_spread
card$straight_up_pick <- ifelse(predicted >= 0, card$home, card$away)
card$ats_pick <- ifelse(card$ats_edge_home >= 0, card$home, card$away)
card$pick_status <- ifelse(abs(card$market_home_spread) > config$ats$large_spread_review,
  "large_spread_review", "article_pick")
for (field in c("margin_sd", "home_win_probability", "home_cover_probability",
  "preseason_expected_margin", "preseason_raw_delta")) card[[field]] <- NA_real_
card$confidence_tier <- NA_character_
card$ats_calibrated <- FALSE
card$foundation_expected_margin <- predicted
card$preseason_challenger_share <- 0
card$preseason_adjustment <- 0
card$top_drivers <- ridge_feature_drivers(list(ridge = fit), candidate_x)
card$model_version <- "3.0.0-opponent-relative-experiment"
card$snapshot_type <- "experimental_live"
card$published <- FALSE
card$preseason_profile <- "Uncapped + opponent-relative; no direct game-count predictors"
card$publication_note <- "Preliminary opponent-relative model | Uncapped margins | Direct game-count predictors removed"
card$kickoff <- x$kickoff
card$kickoff_display <- format(parse_utc_datetime(x$kickoff), "%a %I:%M %p ET", tz = config$article_timezone)
local_hour <- as.integer(format(parse_utc_datetime(x$kickoff), "%H", tz = config$article_timezone))
card$primetime <- local_hour >= 19L | local_hour < 3L
card$neutral_site <- x$neutral_site
unadjusted_card <- card
availability <- data.frame()
if (file.exists(registry_path)) {
  availability <- resolve_season_availability(
    read.csv(registry_path, stringsAsFactors = FALSE, colClasses = c(player_id = "character")),
    card, as_of)
  if (nrow(availability)) {
    card <- apply_injury_overlay(card, availability, as_of, config)$predictions
    card$publication_note <- paste(card$publication_note,
      "| Known absences flagged; fresh per-game injury estimates required")
  }
}
no_counts_prediction <- predict(research$baseline_model, candidate_x)
comparison <- data.frame(game_id = base$game_id, away = base$away, home = base$home,
  market_home_spread = base$market_home_spread, market_captured_at = base$market_captured_at,
  baseline_margin = base$expected_margin, no_counts_margin = no_counts_prediction,
  opponent_relative_margin = predicted, margin_change = predicted - base$expected_margin,
  relative_incremental_change = predicted - no_counts_prediction,
  baseline_pick = dashboard_pick_text(base$ats_pick, base$home, base$market_home_spread),
  opponent_relative_pick = dashboard_pick_text(card$ats_pick, card$home, card$market_home_spread),
  ats_side_changed = card$ats_pick != base$ats_pick,
  baseline_su = base$straight_up_pick, opponent_relative_su = card$straight_up_pick,
  su_changed = card$straight_up_pick != base$straight_up_pick)
out <- if (length(args) >= 2L) args[2] else file.path(config$output_dir, season,
  paste0("week_", week, "_relative_preliminary"), format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
if (dir.exists(out)) stop("Refusing to overwrite an existing experiment.")
dir.create(out, recursive = TRUE)
out <- normalizePath(out, winslash = "/")
write.csv(data.frame(path = paths, md5 = unname(hashes)), file.path(out, "input_hashes.csv"), row.names = FALSE)
write.csv(card, file.path(out, "predictions.csv"), row.names = FALSE, na = "")
write.csv(unadjusted_card, file.path(out, "unadjusted_predictions.csv"), row.names = FALSE, na = "")
write.csv(availability, file.path(out, "injury_reports.csv"), row.names = FALSE, na = "")
write.csv(candidate_x, file.path(out, "features.csv"), row.names = FALSE, na = "")
write.csv(comparison, file.path(out, "comparison.csv"), row.names = FALSE, na = "")
write.csv(relative$game_audit, file.path(out, "game_audit.csv"), row.names = FALSE, na = "")
write.csv(relative$team_audit, file.path(out, "team_audit.csv"), row.names = FALSE, na = "")
saveRDS(list(model = fit, relative = relative, config = config, as_of = as_of,
  baseline_csv = base_path, source_games = completed), file.path(out, "model.rds"))
qa <- list(status = "predictions_complete", season = season, week = week, model_week = model_week,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), as_of = base$as_of[1],
  predictions = nrow(card), source_card = base_path, model_features = expected_features,
  removed_features = removed, margin_cap = "uncapped", new_training_rows = FALSE,
  historical_fit_replay_error = fit_error, baseline_coefficient_error = control_coef_error,
  baseline_margin_replay_error = control_error, count_perturbation_error = count_error,
  ats_side_changes = sum(comparison$ats_side_changed), su_changes = sum(comparison$su_changed),
  mean_absolute_projection_change = mean(abs(comparison$margin_change)),
  maximum_absolute_projection_change = max(abs(comparison$margin_change)),
  lines_unchanged = identical(card$market_home_spread, base$market_home_spread),
  line_times_unchanged = identical(card$market_captured_at, base$market_captured_at),
  existing_matchup_features_unchanged = TRUE, source_hashes_unchanged = TRUE,
  production_model_changed = FALSE, public_site_changed = FALSE,
  known_availability_reports = nrow(availability), injuries_adjusted = FALSE,
  probability_policy = "Point forecasts only; production probabilities, uncertainty and confidence are not reused.",
  bridge_policy = "Existing feature-level bridge preserved; no probability-only bridge recalculation needed for point forecasts.",
  current_relative = relative$qa)
jsonlite::write_json(qa, file.path(out, "qa.json"), pretty = TRUE, auto_unbox = TRUE, digits = 14)
say("OUTPUT:", out)
dashboard <- render_cfb_dashboard(file.path(out, "predictions.csv"), root, out, "dashboard.html")
stopifnot(identical(hashes, tools::md5sum(paths)))
qa$status <- "complete"
jsonlite::write_json(qa, file.path(out, "qa.json"), pretty = TRUE, auto_unbox = TRUE, digits = 14)
say("DASHBOARD:", dashboard)
say("ATS sides changed:", qa$ats_side_changes, "; SU sides changed:", qa$su_changes)
print(comparison[comparison$ats_side_changed | comparison$su_changed, ], row.names = FALSE)
