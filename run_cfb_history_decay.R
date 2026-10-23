# Offline historical-decay experiment, using frozen relative-model inputs.
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
for (m in c("config", "store", "features", "coaches", "adapters", "models", "coach_migration",
  "historical_data", "preseason", "bridge", "workflow", "live_data")) source(file.path(root, "cfb_v2", paste0(m, ".R")))
source("cfb_v3/experiments/margin_cap_features.R")
source("cfb_v3/experiments/history_decay_features.R")
say <- function(...) { cat(..., "\n"); flush.console() }
snapshot <- file.path(root, "cfb_v3/output/2026/week_7_rankings_relative/20261011T145259Z")
old_exp <- file.path(root, "cfb_v3/output/experiments/opponent_relative/20260927T215407Z")
old_caps <- file.path(root, "cfb_v3/output/experiments/margin_caps/20260927T211431Z")
bundle <- readRDS(file.path(snapshot, "ranking_fit.rds"))
data <- bundle$training; weights <- bundle$weights; features <- bundle$model$recipe$features; config <- bundle$config
games <- read_csv_if_present(file.path(root, "cfb_v3/data/historical_games.csv"), TRUE)
games$kickoff <- parse_utc_datetime(games$kickoff)
team_games <- read_csv_if_present(file.path(root, "cfb_v3/data/historical_team_games.csv"), TRUE)
membership <- combine_membership(read_csv_if_present(file.path(root, "cfb_v3/data/historical_fbs_membership.csv"), TRUE))
choices <- read_csv_if_present(file.path(old_caps, "fold_choices.csv"), TRUE)
reference <- read_csv_if_present(file.path(old_exp, "predictions.csv"), TRUE)
reference <- reference[reference$model == "no_counts_opponent_relative", ]
current_games <- read_csv_if_present(file.path(snapshot, "included_games.csv"), TRUE)
current_games$kickoff <- parse_utc_datetime(current_games$kickoff)
current_all <- bind_rows_fill(games[games$season < 2026, ], current_games)
previous <- read_csv_if_present(file.path(snapshot, "rankings.csv"), TRUE)
out <- if (length(args)) args[1] else file.path(root, "cfb_v3/output/experiments/history_decay",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
if (dir.exists(out)) stop("Refusing to overwrite an existing experiment.")
dir.create(out, recursive = TRUE); out <- normalizePath(out, winslash = "/")
say("OUTPUT:", out)
paths <- c(file.path(snapshot, c("ranking_fit.rds", "rankings.csv", "included_games.csv")),
  file.path(old_exp, "predictions.csv"), file.path(old_caps, "fold_choices.csv"),
  file.path(root, "cfb_v3/data/historical_games.csv"),
  file.path(root, "cfb_v3/data/historical_team_games.csv"),
  file.path(root, "cfb_v3/data/historical_fbs_membership.csv"),
  file.path(root, c("run_cfb_history_decay.R", "cfb_v3/experiments/history_decay_features.R",
    "cfb_v3/experiments/HISTORY_DECAY_PROTOCOL.md", "cfb_v3/experiments/margin_cap_features.R")),
  file.path(root, "cfb_v2", paste0(c("config", "store", "features", "models", "historical_data"), ".R")))
hashes <- unname(tools::md5sum(paths)); stopifnot(!anyNA(hashes))
write.csv(data.frame(path = paths, md5 = hashes), file.path(out, "input_hashes.csv"), row.names = FALSE)
file.copy("cfb_v3/experiments/HISTORY_DECAY_PROTOCOL.md", file.path(out, "PROTOCOL.md"))
write.csv(choices, file.path(out, "fold_choices.csv"), row.names = FALSE)
stopifnot(length(features) == 19L, nrow(data) == 4908L, length(weights) == nrow(data),
  !any(c("source_games_diff", "games_played_diff") %in% features),
  all(data$season < 2026), identical(sort(unique(choices$test_season)), 2022:2025),
  all(choices$coach_recent_share == .65), all(current_games$model_week < 8),
  max(current_games$week) == 6L)
data$coach_rating_diff <- data$coach_rating_65_35_diff
teams <- sort(previous$team); n <- length(teams)
rank_result <- function(model, x) {
  p <- predict(model, x)
  mat <- matrix(0, n, n, dimnames = list(teams, teams))
  mat[cbind(match(x$home, teams), match(x$away, teams))] <- p
  symmetric <- (mat - t(mat))/2
  rating <- rowMeans(symmetric)
  stopifnot(all(is.finite(p)), abs(mean(rating)) < 1e-10, max(abs(symmetric+t(symmetric))) < 1e-10)
  r <- previous[match(teams, previous$team), c("team", "conference", "wins", "losses")]
  r$rating <- as.numeric(rating); r <- r[order(-r$rating, r$team), ]; r <- cbind(rank = seq_len(n), r)
  idx <- match(r$team, previous$team)
  r$baseline_rank <- previous$rank[idx]; r$rank_change <- r$baseline_rank - r$rank
  r$rating_change <- r$rating - previous$rating[idx]
  r
}
predictions <- fits <- qa_rows <- list()
for (label in history_decay_models) {
  say("Building chronological power snapshots:", label)
  scope <- history_decay_scope(label)
  rebuild <- margin_cap_power_snapshots; environment(rebuild) <- scope
  power <- rebuild(games, config, Inf)
  candidate <- history_decay_rebuild(data, power$power, label, config)
  candidate <- apply_fbs_bridge_backtest_features(candidate, team_games, membership, config)
  affected <- c("power_rating_diff", "prior_season_diff", "trailing_3yr_diff")
  unchanged <- setdiff(features, affected)
  stopifnot(identical(data[unchanged], candidate[unchanged]),
    all(power$audit$chronological_pass), all(power$audit$kickoff_cutoff_pass))
  baseline_delta <- margin_cap_compare_fields(data, candidate, affected)
  write.csv(baseline_delta, file.path(out, paste0(label, "_feature_changes.csv")), row.names = FALSE)
  if (label == "baseline") print(baseline_delta)
  if (label == "baseline") stopifnot(all(baseline_delta$max_abs_difference < 1e-8), all(baseline_delta$missing_mismatches == 0))
  untouched <- history_decay_cap(label, data$phase_week) == history_decay_cap("baseline", data$phase_week)
  stopifnot(all(margin_cap_compare_fields(data[untouched, ], candidate[untouched, ], affected)$max_abs_difference < 1e-8))
  write.csv(power$audit, file.path(out, paste0(label, "_chronology.csv")), row.names = FALSE)
  write.csv(baseline_delta, file.path(out, paste0(label, "_feature_changes.csv")), row.names = FALSE)
  saveRDS(candidate, file.path(out, paste0(label, "_training.rds")))
  results <- models <- list()
  for (i in seq_len(nrow(choices))) {
    choice <- choices[i, ]; year <- choice$test_season
    train <- which(candidate$season < year); test <- which(candidate$season == year)
    stopifnot(length(train) == choice$training_rows, max(candidate$season[train]) == choice$train_through_season,
      all(candidate$season[train] < min(candidate$season[test])))
    model <- fit_weighted_ridge(candidate[train, ], "margin", features,
      weights[train]/max(weights[train]), choice$foundation_lambda)
    p <- predict(model, candidate[test, ]); stopifnot(all(is.finite(p)))
    cols <- c("game_id", "season", "week", "model_week", "home", "away", "home_level", "away_level",
      "postseason_type", "is_cfp", "neutral_site", "closing_home_spread")
    results[[as.character(year)]] <- cbind(model = label, candidate[test, cols],
      actual_margin = candidate$margin[test], expected_margin = p)
    models[[as.character(year)]] <- model
  }
  predicted <- do.call(rbind, results)
  if (label == "baseline") {
    j <- match(predicted$game_id, reference$game_id)
    stopifnot(!anyNA(j), nrow(predicted) == nrow(reference), all(predicted$actual_margin == reference$actual_margin[j]))
    control_error <- max(abs(predicted$expected_margin - reference$expected_margin[j]))
    stopifnot(control_error < 1e-8)
    say("Historical baseline replay error:", control_error)
  }
  predictions[[label]] <- predicted; fits[[label]] <- models
  full <- fit_weighted_ridge(candidate, "margin", features, weights, 32)
  current_power <- scope$power_rating_for_week(current_all, 2026L, 8L, 7L, config)
  x <- history_decay_rebuild(bundle$matchup_features, current_power, label, config)
  ranks <- rank_result(full, x)
  if (label == "baseline") {
    current_error <- max(abs(ranks$rating_change))
    stopifnot(current_error < 1e-8, max(abs(full$coefficients-bundle$model$coefficients)) < 1e-8)
    say("Current ranking replay error:", current_error)
  }
  write.csv(ranks, file.path(out, paste0(label, "_rankings.csv")), row.names = FALSE)
  write.csv(do.call(rbind, scope$weight_audit), file.path(out, paste0(label, "_weight_audit.csv")), row.names = FALSE)
  saveRDS(list(model=full, matchup_features=x), file.path(out, paste0(label, "_current_fit.rds")))
  qa_rows[[label]] <- list(model=label, unchanged_predictors=unchanged, chronology_pass=TRUE,
    protected_fields_unchanged=TRUE, predictions=nrow(predicted), training_rows=nrow(candidate))
  say("Finished:", label)
}
stopifnot(identical(hashes, unname(tools::md5sum(paths))))
write.csv(do.call(rbind, predictions), file.path(out, "predictions.csv"), row.names=FALSE, na="")
saveRDS(fits, file.path(out, "fold_models.rds"))
jsonlite::write_json(list(status="fitting_complete", models=history_decay_models,
  baseline_prediction_replay_error=control_error, baseline_current_ranking_replay_error=current_error,
  source_inputs_unchanged=TRUE, production_changed=FALSE, published_site_changed=FALSE,
  evaluation_seasons=2022:2025, previously_researched_seasons=TRUE,
  current_snapshot=snapshot, features=features, candidates=qa_rows,
  experiment_scope="Power/direct-prior decay only; coaching and other predictors held fixed; all coefficients refitted on earlier seasons per fold."),
  file.path(out, "manifest.json"), pretty=TRUE, auto_unbox=TRUE, digits=14)
say("Fitting complete:", out)
