args <- commandArgs(trailingOnly = TRUE)
if (length(args)) stop("Run from the project root without arguments.")
Sys.unsetenv("LC_ALL")
suppressPackageStartupMessages({ library(dplyr); library(zoo); library(caret) })
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- dirname(normalizePath(sub("^--file=", "", script[1]), winslash = "/"))
setwd(project_dir)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R", "models.R",
  "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R"))
  source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "model1_replay.R", "audit_og_inputs.R", "og_reconstruction.R"))
  source(file.path("cfb_v3", "experiments", file))
config <- cfb_v2_config(project_dir, 2026L)
protected <- experiment_file_hashes(config)
output <- file.path(config$output_dir, "experiments", "og_verification",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"))
stopifnot(!dir.exists(output))
dir.create(output, recursive = TRUE)
message("Verification output: ", output)
root <- "cfb_v3/experiments"
file.copy(file.path(root, "OG_COMPARISON_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
input_path <- "cfb_v3/output/experiments/model1_inputs/compact_raw_inputs.rds"
if (!file.exists(input_path)) stop("First run Rscript cfb_v3/experiments/extract_og_compact.R")
compact <- readRDS(input_path)
stopifnot(identical(compact$source_md5, "7b7898800226b33ed8d307405f7553f0"),
  identical(compact$script_md5, unname(tools::md5sum(file.path(root, "extract_og_compact.R")))))
source <- model1_source(project_dir)
inputs <- model1_archived_joins(list(pbp = compact$games, sp = compact$sp, elo = compact$elo), project_dir)
official <- read.csv(file.path(config$data_dir, "historical_games.csv"))
compact$games$kickoff <- parse_utc_datetime(compact$games$start_date)
missing <- is.na(compact$games$kickoff)
compact$games$kickoff[missing] <- parse_utc_datetime(official$kickoff[
  match(compact$games$game_id[missing], official$game_id)])
write.csv(compact$games[is.na(compact$games$kickoff), ], file.path(output, "unresolved_game_dates.csv"), row.names = FALSE)
replay <- "cfb_v3/output/experiments/model1_replay/20260909T195945Z"
prior <- jsonlite::fromJSON(file.path(replay, "verification.json"))
parity_data <- og_freeze_compact(compact, 2024L, parse_utc_datetime(prior$pbp_cutoff_exclusive))
rebuilt <- og_compact_table(parity_data, inputs, source)$data
saved <- readRDS(file.path(replay, "legacy_frozen_table.rds"))
parity <- all.equal(as.data.frame(saved), as.data.frame(rebuilt[names(saved)]),
  tolerance = 1e-12, check.attributes = FALSE)
writeLines(c("Archived 2024 complete prediction table parity:", as.character(parity)), file.path(output, "feature_parity.txt"))
if (!isTRUE(parity)) stop("Original feature parity failed: ", paste(parity, collapse = "; "))
message("Original feature parity passed: ", nrow(saved), " rows x ", ncol(saved), " columns")

record_games <- compact$games
record_games$season <- record_games$year
record_games$margin <- ifelse(record_games$home == record_games$pos_team,
  record_games$pos_team_final_score-record_games$def_team_final_score,
  record_games$def_team_final_score-record_games$pos_team_final_score)
record_games$home <- canonical_team(record_games$home); record_games$away <- canonical_team(record_games$away)
tg <- read.csv(file.path(config$data_dir, "historical_team_games.csv"))
tg <- tg[!duplicated(tg$game_id), ]
tg$margin <- tg$home_score-tg$away_score; tg$kickoff <- parse_utc_datetime(tg$kickoff)
cols <- c("game_id", "season", "kickoff", "home", "away", "margin")
record_games <- bind_rows(tg[cols], record_games[!record_games$game_id %in% tg$game_id, cols])
record_games <- record_games[!is.na(record_games$kickoff) & is.finite(record_games$margin), ]
public <- "cfb_v3/output/experiments/og_audit/20260909T203217Z/public_sources"
anchors <- read.csv(file.path(root, "og_sp_anchors.csv"))
verified <- og_verify_public(public, record_games, anchors, output)
print(verified$summaries[verified$summaries$numeric_anchor, ], row.names = FALSE)

# Check same-week cached Elo against the next game's pregame number without
# treating that next game's outcome as an input to the prediction.
elo <- inputs$elo; elo$canonical <- canonical_team(elo$team)
g <- compact$games[!is.na(compact$games$kickoff), ]
alignment <- list()
for (side in c("home", "away")) {
  x <- data.frame(game_id = g$game_id, season = g$year, week = g$week, kickoff = g$kickoff,
    team = canonical_team(g[[side]]), pregame_elo = g[[paste0(side, "_team_pregame_elo")]])
  alignment[[side]] <- x
}
alignment <- bind_rows(alignment) %>% arrange(season, team, kickoff) %>% group_by(season, team) %>%
  mutate(next_pregame_elo = lead(pregame_elo), next_kickoff = lead(kickoff)) %>% ungroup()
alignment$cached_elo <- elo$elo[match(paste(alignment$team, alignment$season, alignment$week),
  paste(elo$canonical, elo$year, elo$week))]
alignment$comparable <- is.finite(alignment$cached_elo) & is.finite(alignment$next_pregame_elo)
alignment$equal <- alignment$comparable & abs(alignment$cached_elo-alignment$next_pregame_elo) < 1e-8
write.csv(alignment, file.path(output, "elo_next_pregame_checks.csv"), row.names = FALSE)

reference <- read.csv(file.path(config$output_dir, "backtest", "rolling_predictions.csv"))
reference <- reference[reference$model == "ridge_core" & reference$season %in% 2022:2025 &
  raw_history_eligible(reference), ]
stopifnot(!anyDuplicated(reference$game_id))
reference$model <- "v3_control"
index <- match(reference$game_id, official$game_id)
stopifnot(!anyNA(index))
reference$cutoff <- as.POSIXct(official$feature_week_start[index], tz = "UTC")
stopifnot(!anyNA(reference$cutoff), all(reference$cutoff < parse_utc_datetime(official$kickoff[index])))
all_names <- sort(unique(c(reference$home, reference$away)))
names_map <- setNames(vapply(all_names, og_legacy_name, character(1), inputs, source), all_names)
training <- readRDS(file.path(replay, "legacy_training_table.rds"))
features <- setdiff(names(training), "home_away_score_dif")
# Exercise the actual archived predict_winner construction without fitting a model.
probe <- model1_environment(inputs, source)
probe$captured <- NULL
probe$predict <- function(model, newdata) { probe$captured <- newdata; 0 }
for (pair in list(c("Ohio State", "Notre Dame"), c("Notre Dame", "Ohio State"))) {
  probe$predict_winner(pair[1], pair[2], NULL, rebuilt)
  row <- og_matchup_row(pair[1], pair[2], rebuilt)
  stopifnot(isTRUE(all.equal(probe$captured[features], row[features], check.attributes = FALSE)))
}
designs <- list(); reverse_designs <- list(); coverage <- list(); origins <- list()
for (delay in 0:1) {
  lane <- if (delay == 0L) "same_cycle_assumed" else "one_cycle_delayed"
  for (date in sort(unique(as.character(as.Date(reference$cutoff))))) {
    games <- reference[as.character(as.Date(reference$cutoff)) == date, ]
    season <- unique(games$season); stopifnot(length(season) == 1L)
    snapshot_cycle <- as.character(as.Date(date) - delay*7L)
    snapshot <- verified$rows[verified$rows$season == season & verified$rows$cycle == snapshot_cycle &
      verified$rows$reconstruction_status == "corroborated_reconstruction", ]
    audit <- data.frame(game_id = games$game_id, season = season, lane = lane, cutoff = date,
      snapshot_cycle = snapshot_cycle, home = games$home, away = games$away,
      is_cfp = games$is_cfp, reason = "no_corroborated_weekly_snapshot")
    if (nrow(snapshot)) {
      stopifnot(!anyDuplicated(snapshot$team))
      snapshot$legacy <- unname(names_map[snapshot$team])
      # Non-target opponents also affect the original complete-case lookup.
      unknown <- is.na(snapshot$legacy)
      snapshot$legacy[unknown] <- vapply(snapshot$team[unknown], og_legacy_name, character(1), inputs, source)
      snapshot <- snapshot[!is.na(snapshot$legacy), ]
      current <- inputs
      current$sp <- data.frame(team = snapshot$legacy, year = season, rating = snapshot$rating,
        offense_rating = snapshot$offense_rating, defense_rating = snapshot$defense_rating)
      frozen <- og_freeze_compact(compact, season, as.POSIXct(date, tz = "UTC"))
      stopifnot(!any(frozen$games$game_id %in% games$game_id))
      table <- suppressWarnings(og_compact_table(frozen, current, source))$data
      for (i in seq_len(nrow(games))) {
        game <- games[i, ]; h <- unname(names_map[game$home]); a <- unname(names_map[game$away])
        if (is.na(h) || is.na(a)) { audit$reason[i] <- "original_exclusion_or_unknown_name"; next }
        if (!(h %in% training$home && a %in% training$away) ||
          (game$neutral_site && !(a %in% training$home && h %in% training$away))) {
          audit$reason[i] <- "unseen_training_team_side"; next
        }
        row <- og_matchup_row(h, a, table)
        reverse <- if (game$neutral_site) og_matchup_row(a, h, table) else row
        if (is.null(row) || is.null(reverse)) { audit$reason[i] <- "no_complete_six_game_side_history_or_rating"; next }
        stopifnot(setequal(names(row), features), setequal(names(reverse), features))
        audit$reason[i] <- "scored"
        key <- paste(lane, game$game_id)
        designs[[key]] <- row[features]; reverse_designs[[key]] <- reverse[features]
        ids <- attr(row, "source_games"); reverse_ids <- attr(reverse, "source_games")
        origins[[key]] <- data.frame(game_id = game$game_id, lane = lane, cutoff = date,
          snapshot_label = snapshot$snapshot_label[1], snapshot_cycle = snapshot_cycle,
          home_source_game = ids[1], away_source_game = ids[2],
          reverse_home_source_game = reverse_ids[1], reverse_away_source_game = reverse_ids[2])
      }
    }
    coverage[[paste(lane, date)]] <- audit
  }
}
coverage <- bind_rows(coverage); origins <- bind_rows(origins)
write.csv(coverage, file.path(output, "game_coverage.csv"), row.names = FALSE)
write.csv(origins, file.path(output, "feature_sources.csv"), row.names = FALSE)
if (!length(designs)) stop("No qualified OG matchups survived. See coverage diagnostics.")
design <- bind_rows(designs); reverse_design <- bind_rows(reverse_designs)
write.csv(cbind(origins, design), file.path(output, "og_features.csv"), row.names = FALSE)
write.csv(cbind(origins, reverse_design), file.path(output, "og_reverse_features.csv"), row.names = FALSE)
elo_sources <- list()
for (orientation in c("forward", "reverse")) for (side in c("home", "away")) {
  rows <- if (orientation == "forward") design else reverse_design
  id_column <- paste0(if (orientation == "reverse") "reverse_" else "", side, "_source_game")
  idx <- match(paste(origins[[id_column]], canonical_team(rows[[side]])),
    paste(alignment$game_id, alignment$team))
  elo_sources[[paste(orientation, side)]] <- data.frame(game_id = origins$game_id,
    lane = origins$lane, orientation = orientation, side = side, team = rows[[side]],
    source_game_id = origins[[id_column]], selected_elo = rows[[paste0(side, "_team_elo_rating")]],
    next_pregame_elo = alignment$next_pregame_elo[idx], corroborated = alignment$equal[idx])
}
write.csv(bind_rows(elo_sources), file.path(output, "selected_elo_checks.csv"), row.names = FALSE)
base <- reference[match(origins$game_id, reference$game_id), ]
base$lane <- origins$lane
predictions <- list(base)
fit_hashes <- list()
for (seed in 20260909:20260911) {
  path <- file.path(replay, paste0("archived_model1_", seed, ".rds"))
  fit_hashes[[as.character(seed)]] <- unname(tools::md5sum(path))
  fit <- readRDS(path)
  stopifnot(fit$bestTune$mtry == 6L, fit$finalModel$ntree == 500L,
    identical(as.numeric(fit$trainingData$.outcome), as.numeric(training$home_away_score_dif)))
  for (column in features) stopifnot(isTRUE(all.equal(as.character(fit$trainingData[[column]]),
    as.character(training[[column]]), check.attributes = FALSE)))
  expected <- as.numeric(predict(fit, design))
  reversed <- as.numeric(predict(fit, reverse_design))
  x <- base; x$model <- paste0("og_", seed)
  x$expected_margin <- ifelse(x$neutral_site, (expected-reversed)/2, expected)
  stopifnot(all(is.finite(x$expected_margin)))
  predictions[[length(predictions) + 1L]] <- x
  rm(fit); gc()
}
predictions <- experiment_grade(bind_rows(predictions))
stopifnot(!anyDuplicated(predictions[c("game_id", "lane", "model")]))
write.csv(predictions, file.path(output, "predictions.csv"), row.names = FALSE)
metrics <- list(); paired <- list(); loso <- list()
for (lane in unique(predictions$lane)) {
  p <- predictions[predictions$lane == lane, ]
  m <- og_metric_table(p); m$lane <- lane; metrics[[lane]] <- m
  for (model in setdiff(unique(p$model), "v3_control")) {
    x <- p[p$model == model, ]; control <- p[p$model == "v3_control", ]
    control <- control[match(x$game_id, control$game_id), ]
    stopifnot(identical(x$game_id, control$game_id), identical(x$closing_home_spread, control$closing_home_spread))
    delta <- x$mae_error-control$mae_error; ats <- x$ats_correct-control$ats_correct
    ci <- experiment_season_interval(delta, x$season); aci <- experiment_season_interval(ats, x$season)
    paired[[paste(lane, model)]] <- data.frame(lane = lane, model = model, games = nrow(x),
      mae_change = mean(delta), mae_low = ci[1], mae_high = ci[2],
      ats_change = mean(ats, na.rm = TRUE), ats_low = aci[1], ats_high = aci[2])
    for (season in unique(x$season)) loso[[paste(lane, model, season)]] <- data.frame(lane = lane,
      model = model, omitted_season = season, games = sum(x$season != season),
      mae_change = mean(delta[x$season != season]), ats_change = mean(ats[x$season != season], na.rm = TRUE))
  }
}
metrics <- bind_rows(metrics); paired <- bind_rows(paired); loso <- bind_rows(loso)
write.csv(metrics, file.path(output, "metrics.csv"), row.names = FALSE)
write.csv(paired, file.path(output, "paired_intervals.csv"), row.names = FALSE)
write.csv(loso, file.path(output, "leave_one_season_out.csv"), row.names = FALSE)
common_ids <- Reduce(intersect, split(base$game_id, base$lane))
common_metrics <- list()
for (lane in unique(predictions$lane)) {
  m <- og_metric_table(predictions[predictions$lane == lane & predictions$game_id %in% common_ids, ])
  m$lane <- lane; common_metrics[[lane]] <- m
}
write.csv(bind_rows(common_metrics), file.path(output, "timing_common_game_metrics.csv"), row.names = FALSE)
stopifnot(identical(protected, experiment_file_hashes(config)))
files <- c(input_path, file.path(replay, c("verification.json", "legacy_training_table.rds", "legacy_frozen_table.rds")),
  file.path(root, c("OG_COMPARISON_PROTOCOL.md", "og_reconstruction.R", "og_sp_anchors.csv", "extract_og_compact.R")),
  "run_cfb_og_verification.R", file.path(public, "inventory.json"))
jsonlite::write_json(list(status = "corroborated_reconstruction_not_timestamp_verified",
  production_unchanged = TRUE, feature_parity = TRUE, archived_matchup_row_parity = TRUE, seed_fit_md5 = fit_hashes,
  source_md5 = as.list(tools::md5sum(files)), numeric_checks = nrow(verified$checks),
  numeric_checks_passed = sum(verified$checks$passed),
  elo_comparable = sum(alignment$comparable), elo_next_pregame_matches = sum(alignment$equal),
  protected = protected), file.path(output, "verification.json"), pretty = TRUE, auto_unbox = TRUE)
counts <- as.data.frame(xtabs(~ lane + season + reason, coverage))
write.csv(counts[counts$Freq > 0, ], file.path(output, "coverage_counts.csv"), row.names = FALSE)
display <- metrics[metrics$slice == "all_fbs", c("lane", "model", "games", "wins", "losses", "pushes", "margin_mae", "su_accuracy")]
writeLines(c("# OG Reconstructed Verification", "",
  "Exploratory corroborated weekly inputs; NOT independently timestamped. No production changes.", "",
  "Same-cycle Monday availability is assumed. The delayed lane uses exactly the previous weekly cycle; no gap interpolation.",
  "The two timing lanes cover different games. See timing_common_game_metrics.csv for the common-game timing check.",
  "Only directly numerically corroborated tables and record-matching team rows are eligible. Failed anchors reject the entire table.", "",
  markdown_table(display), "", "## Paired Differences (OG Minus V3)", "", markdown_table(paired), "",
  "Whole-season bootstrap: 5,000 draws, seed 20260909. Coverage is sparse and uneven; intervals are descriptive, not proof of general superiority.",
  "See coverage_counts.csv and game_coverage.csv for missing snapshots, exclusions and original side-history lookup failures.",
  "See dated_numeric_checks.csv for conflicting source values; do not discard failed checks to improve scores.",
  "See selected_elo_checks.csv for cached Elo disagreements with next-game pregame values. Elo release vintages remain unverified.",
  "Original fixed 2014-2021 fits retain same-game training features and duplicate joins. V3 uses prior-season rolling fits.",
  "Closing-line ATS is retrospective, not Friday execution or realized betting ROI.",
  "This weekly lane does not establish a frozen whole-bracket CFP comparison. No unsupported CFP score is inferred.",
  "All dates, source hashes, input rows, paired intervals and leave-one-season-out results are saved alongside this report."),
  file.path(output, "REPORT.md"))
print(display, row.names = FALSE)
cat("Report:", file.path(output, "REPORT.md"), "\n")
