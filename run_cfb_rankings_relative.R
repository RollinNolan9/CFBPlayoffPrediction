# Frozen Week 5 rankings for uncapped + opponent-relative, without direct counts.
# Rscript --vanilla run_cfb_rankings_relative.R [new_output_directory]
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists("cfb_v2/config.R")) stop("Run from repository root.")
for (module in c("config", "store", "features", "coaches", "adapters", "models",
  "coach_migration", "historical_data", "preseason", "bridge", "workflow", "live_data"))
  source(file.path("cfb_v2", paste0(module, ".R")))
source("cfb_v3/experiments/margin_cap_current_snapshots.R")
source("cfb_v3/experiments/opponent_relative_features.R")
say <- function(...) { cat(..., "\n"); flush.console() }
published_dir <- "cfb_v3/output/2026/week_5_rankings/20260927_espn_supplement"
historical_dir <- "cfb_v3/output/experiments/margin_caps/20260927T211431Z"
relative_dir <- "cfb_v3/output/experiments/opponent_relative/20260927T215407Z"
no_counts_dir <- "cfb_v3/output/2026/week_5_rankings_caps/20260927T214008Z"
metadata <- jsonlite::read_json(file.path(published_dir, "qa.json"), simplifyVector = TRUE)
published <- read_csv_if_present(file.path(published_dir, "rankings.csv"), TRUE)
baseline <- read_csv_if_present(file.path(no_counts_dir, "uncapped_rankings.csv"), TRUE)
bundle <- readRDS(metadata$model_path)
reference <- bundle$model$foundation
config <- bundle$config
config$project_dir <- root
config$data_dir <- file.path(root, "cfb_v3/data")
config$inbox_dir <- file.path(root, "cfb_v3/inbox")
stopifnot(is.null(reference$residual_forest), metadata$week >= 5,
  reference$ridge$lambda == 32, all(reference$trained_seasons < metadata$season))

# Validate the frozen current schedule and every original published source hash.
cache <- margin_cap_current_load_inputs(config, published_dir, historical_dir)
old_hashes <- read.csv(file.path(relative_dir, "input_hashes.csv"), stringsAsFactors = FALSE)
stopifnot(identical(unname(tools::md5sum(old_hashes$path)), old_hashes$md5))
paths <- unique(c(cache$source_paths, old_hashes$path,
  file.path(relative_dir, "feature_build.rds"),
  file.path(no_counts_dir, "uncapped_rankings.csv"),
  "run_cfb_rankings_relative.R", "cfb_v3/experiments/margin_cap_current_snapshots.R"))
hashes <- tools::md5sum(paths)
stopifnot(!anyNA(hashes))
out <- if (length(args)) args[1] else file.path("cfb_v3/output/2026/week_5_rankings_relative",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
if (dir.exists(out)) stop("Refusing to overwrite an existing ranking snapshot.")
dir.create(out, recursive = TRUE)
out <- normalizePath(out, winslash = "/")
say("OUTPUT:", out)
write.csv(data.frame(path = paths, md5 = unname(hashes)),
  file.path(out, "input_hashes.csv"), row.names = FALSE)

input <- readRDS(file.path(historical_dir, "uncapped_training_inputs.rds"))
historical_relative <- readRDS(file.path(relative_dir, "feature_build.rds"))
stopifnot(identical(as.character(input$data$game_id),
  as.character(historical_relative$features$game_id)))
training <- input$data
training$coach_rating_diff <- training$coach_rating_65_35_diff
training$opponent_relative_margin_diff <- historical_relative$features$opponent_relative_margin_diff
removed <- c("source_games_diff", "games_played_diff")
base_features <- setdiff(reference$features, removed)
features <- c(base_features, "opponent_relative_margin_diff")
stopifnot(length(features) == 19L, all(is.finite(training$opponent_relative_margin_diff)))
published_fit <- fit_weighted_ridge(training, "margin", reference$features,
  input$weights, reference$ridge$lambda)
control_fit_error <- max(abs(published_fit$coefficients - reference$ridge$coefficients))
stopifnot(control_fit_error < 1e-10)
baseline_fit <- fit_weighted_ridge(training, "margin", base_features,
  input$weights, reference$ridge$lambda)
fit <- fit_weighted_ridge(training, "margin", features, input$weights, reference$ridge$lambda)
stopifnot(fit$training_rows == reference$trained_rows,
  !any(removed %in% fit$recipe$features), all(training$season < metadata$season))

teams <- sort(published$team)
n <- length(teams)
stopifnot(n == metadata$team_count, setequal(teams, baseline$team))
# Same frozen game-level baseline semantics as the historical experiment.
# Only current-season source games are needed for this same-season feature.
targets <- data.frame(game_id = paste0("relative_snapshot_", seq_len(n)),
  season = metadata$season, model_week = metadata$model_week,
  home = teams, away = c(teams[-1], teams[1]),
  feature_week_start = metadata$as_of)
stopifnot(all(!is.na(cache$completed$feature_week_start)),
  all(cache$completed$model_week < metadata$model_week),
  all(cache$completed$kickoff < cache$as_of))
current_relative <- opponent_relative_build(cache$completed, targets)
values <- setNames(current_relative$features$home_opponent_relative_margin, teams)
stopifnot(max(abs(current_relative$features$away_opponent_relative_margin - values[targets$away])) == 0)
team_features <- cache$team_features
team_features$opponent_relative_margin <- unname(values[team_features$team])
stopifnot(all(is.finite(team_features$opponent_relative_margin)))

pair_index <- expand.grid(home_index = seq_len(n), away_index = seq_len(n))
pair_index <- pair_index[pair_index$home_index != pair_index$away_index, ]
schedule <- data.frame(game_id = paste0("relative_rank_pair_", seq_len(nrow(pair_index))),
  season = metadata$season, week = metadata$week, model_week = metadata$model_week,
  home = teams[pair_index$home_index], away = teams[pair_index$away_index],
  neutral_site = TRUE, postseason_type = "regular")
x <- make_matchup_features(schedule, team_features, team_features, .65, config)
x$opponent_relative_margin_diff <- unname(values[schedule$home] - values[schedule$away])
stopifnot(all(x$home_field_points == 0), all(is.finite(x$opponent_relative_margin_diff)))
rank_model <- function(model) {
  prediction <- predict(model, x)
  stopifnot(length(prediction) == nrow(pair_index), all(is.finite(prediction)))
  raw <- matrix(0, n, n, dimnames = list(teams, teams))
  raw[cbind(pair_index$home_index, pair_index$away_index)] <- prediction
  neutral <- (raw - t(raw)) / 2
  ratings <- rowMeans(neutral)
  stopifnot(max(abs(neutral + t(neutral))) < 1e-10, abs(mean(ratings)) < 1e-10)
  list(rating = ratings, prediction = prediction, neutral = neutral,
    matchup_residual = max(abs(neutral - outer(ratings, ratings, "-"))))
}
published_replay <- rank_model(published_fit)
baseline_replay <- rank_model(baseline_fit)
published_error <- max(abs(published_replay$rating - published$rating[match(teams, published$team)]))
baseline_error <- max(abs(baseline_replay$rating - baseline$rating[match(teams, baseline$team)]))
stopifnot(published_error < 1e-8, baseline_error < 1e-8)
result <- rank_model(fit)
count_changed <- x
count_changed$source_games_diff <- 999
count_changed$games_played_diff <- -999
count_error <- max(abs(predict(fit, count_changed) - result$prediction))
stopifnot(count_error == 0)
ordering <- order(-result$rating, teams)
rankings <- published[match(teams[ordering], published$team), c("team", "conference", "wins", "losses")]
rankings <- cbind(rank = seq_len(n), rankings, rating = unname(result$rating[ordering]))
pub_idx <- match(rankings$team, published$team)
base_idx <- match(rankings$team, baseline$team)
rankings$published_rank <- published$rank[pub_idx]
rankings$published_rating <- published$rating[pub_idx]
rankings$no_counts_rank <- baseline$rank[base_idx]
rankings$no_counts_rating <- baseline$rating[base_idx]
rankings$rank_change_vs_published <- rankings$published_rank - rankings$rank
rankings$rank_change_vs_no_counts <- rankings$no_counts_rank - rankings$rank
rownames(rankings) <- NULL
write.csv(rankings, file.path(out, "rankings.csv"), row.names = FALSE)
write.csv(head(rankings, 25), file.path(out, "top25.csv"), row.names = FALSE)
write.csv(team_features, file.path(out, "team_snapshots.csv"), row.names = FALSE)
write.csv(cache$completed, file.path(out, "source_games.csv"), row.names = FALSE, na = "")
write.csv(current_relative$game_audit, file.path(out, "game_audit.csv"), row.names = FALSE, na = "")
write.csv(current_relative$team_audit, file.path(out, "team_audit.csv"), row.names = FALSE, na = "")
write.csv(data.frame(team = schedule$home, opponent = schedule$away,
  raw_margin = result$prediction,
  neutral_margin = result$neutral[cbind(pair_index$home_index, pair_index$away_index)]),
  file.path(out, "neutral_matchups.csv"), row.names = FALSE)
write.csv(data.frame(feature = names(fit$coefficients), coefficient = unname(fit$coefficients)),
  file.path(out, "coefficients.csv"), row.names = FALSE)
saveRDS(list(model = fit, baseline_model = baseline_fit, training = training,
  weights = input$weights, current_relative = current_relative, matchup_features = x),
  file.path(out, "ranking_fit.rds"))
stopifnot(identical(hashes, tools::md5sum(paths)))
qa <- list(season = metadata$season, week = metadata$week, model_week = metadata$model_week,
  completed_week = metadata$completed_week, as_of = metadata$as_of,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  team_count = n, pair_count = nrow(pair_index), features = features,
  removed_features = removed, margin_cap = "uncapped", model_retrained = TRUE,
  training_rows = fit$training_rows, training_seasons = sort(unique(training$season)),
  lambda = reference$ridge$lambda, coach_recent_share = .65,
  source_fit_coefficient_error = control_fit_error, published_rating_replay_error = published_error,
  no_counts_rating_replay_error = baseline_error, count_perturbation_prediction_error = count_error,
  max_matchup_residual = result$matchup_residual, current_relative = current_relative$qa,
  method = "Mean symmetric neutral margin across all FBS teams, including a zero self matchup.",
  relative_policy = current_relative$qa$definition,
  data_note = "Same frozen data through Week 4 as the published rankings, including the labeled Oklahoma State vs. West Virginia ESPN calculated-EPA supplement.",
  preseason_roster_component_used = FALSE, ap_poll_used_as_predictor = FALSE,
  source_inputs_unchanged = TRUE, production_model_changed = FALSE, published_site_changed = FALSE)
jsonlite::write_json(qa, file.path(out, "qa.json"), pretty = TRUE, auto_unbox = TRUE, digits = 14)
say("Published replay error:", published_error, "; no-count baseline replay error:", baseline_error)
print(head(rankings, 25), row.names = FALSE)
say("Complete:", out)
