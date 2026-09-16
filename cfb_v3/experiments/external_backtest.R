historical_sp_recheck <- function(ledger, candidates, anchors, games, public) {
  assert_unique_keys(ledger, c("season", "snapshot_label", "team"), "SP reconstruction ledger")
  inventory <- jsonlite::fromJSON(file.path(public, "inventory.json"), simplifyVector = FALSE)
  required <- unique(rbind(anchors[c("season", "snapshot_label")], data.frame(season = anchors$season,
    snapshot_label = anchors$previous_label)))
  required <- required[!is.na(required$snapshot_label) & nzchar(required$snapshot_label), ]
  raw_tables <- list()
  for (j in seq_len(nrow(required))) {
    key <- paste(required$season[j], required$snapshot_label[j])
    raw <- og_public_table(public, inventory, required$season[j], required$snapshot_label[j])
    raw_tables[[key]] <- raw
    values <- og_extract_public_sp(raw)
    target <- which(paste(candidates$season, candidates$snapshot_label) == key)
    i <- match(candidates$team[target], values$team)
    if (!length(target) || anyNA(i)) stop("Raw anchor table identity changed: ", key)
    for (field in c("rating", "offense_rating", "defense_rating")) {
      if (any(!is.finite(candidates[[field]][target]) |
        abs(candidates[[field]][target]-values[[field]][i]) > 1e-8)) stop("Raw anchor input changed: ", key)
    }
  }
  checks <- og_verify_anchors(candidates, anchors)
  groups <- split(seq_len(nrow(checks)), paste(checks$season, checks$snapshot_label))
  accepted <- names(groups)[vapply(groups, function(i) all(checks$passed[i]), logical(1))]
  keys <- paste(ledger$season, ledger$snapshot_label)
  rows <- list()
  for (key in accepted) {
    x <- ledger[keys == key, ]
    if (!nrow(x)) stop("Anchored table missing from reconstruction ledger: ", key)
    season <- unique(x$season); sheet <- unique(x$snapshot_label)
    raw <- raw_tables[[key]]
    values <- og_extract_public_sp(raw)
    i <- match(x$team, values$team)
    if (anyNA(i)) stop("Raw SP team identity changed.")
    for (field in c("rating", "offense_rating", "defense_rating"))
      if (any(!is.finite(x[[field]]) | abs(x[[field]] - values[[field]][i]) > 1e-8)) stop("Raw SP value changed: ", key)
    group <- groups[[key]]
    dates <- og_public_dates(raw[["Date"]], season)
    dates <- dates[!is.na(dates) & as.integer(format(dates, "%Y")) %in% c(season, season+1L)]
    if (!length(dates)) stop("Missing weekly schedule dates: ", key)
    cycle <- min(dates) - (as.integer(format(min(dates), "%u"))-1L)
    if (any(is.na(x$cycle) | x$cycle != as.character(cycle))) stop("SP weekly cycle changed.")
    raw <- raw[!is.na(raw[["Team"]]) & nzchar(trimws(raw[["Team"]])), , drop = FALSE]
    record <- og_public_records(raw[["Record"]])
    record <- record[i, ]
    cutoff <- as.POSIXct(cycle, tz = "UTC")
    past <- games[!is.na(games$kickoff) & games$kickoff < cutoff & games$season == season &
      !is.na(games$completed) & games$completed & is.finite(games$margin), ]
    x$record_rechecked <- vapply(seq_len(nrow(x)), function(j) {
      m <- c(past$margin[past$home == x$team[j]], -past$margin[past$away == x$team[j]])
      !is.na(record$wins[j]) && !is.na(record$losses[j]) &&
        record$wins[j] == sum(m > 0) && record$losses[j] == sum(m < 0)
    }, logical(1))
    evidence <- as.Date(checks$release_date[group])
    if (anyNA(evidence)) stop("Missing dated corroboration: ", key)
    x$corroborated_by <- external_stamp(as.POSIXct(max(evidence), tz = "UTC") + 36*3600)
    x$qualified <- x$reconstruction_status == "corroborated_reconstruction" &
      !is.na(x$record_passed) & x$record_passed & x$record_rechecked & is.finite(x$rating)
    rows[[key]] <- x
  }
  list(rows = do.call(rbind, rows), checks = checks)
}

historical_sp_reference <- function(reference, games) {
  assert_unique_keys(games, "game_id", "historical schedule")
  reference <- reference[reference$model == "ridge_core" & reference$season %in% 2022:2025 &
    raw_history_eligible(reference), ]
  assert_unique_keys(reference, "game_id", "frozen v3 rolling forecasts")
  i <- match(reference$game_id, games$game_id)
  if (anyNA(i)) stop("Frozen game missing from schedule.")
  for (name in c("home", "away", "season", "neutral_site", "closing_home_spread"))
    if (!isTRUE(all.equal(reference[[name]], games[[name]][i], check.attributes = FALSE))) stop("V3/schedule mismatch: ", name)
  if (!isTRUE(all.equal(reference$actual_margin, games$margin[i], check.attributes = FALSE))) stop("Final margins disagree.")
  reference$kickoff <- games$kickoff[i]
  reference$cycle <- as.character(as.Date(games$feature_week_start[i]))
  cutoff <- as.POSIXct(reference$cycle, tz = "UTC")
  if (anyNA(cutoff) || anyNA(reference$kickoff) || any(reference$kickoff <= cutoff) ||
      any(reference$kickoff >= cutoff + 7*86400) || any(!is.finite(reference$expected_margin)))
    stop("Invalid v3 week boundary, kickoff, or frozen forecast.")
  reference
}

historical_sp_grade <- function(data) {
  x <- experiment_grade(data)
  # Decimal rating subtraction can leave ~1e-15 at a mathematically zero edge.
  tied <- is.finite(x$edge) & abs(x$edge) < 1e-8
  x$selected[tied] <- FALSE; x$win[tied] <- FALSE; x$loss[tied] <- FALSE; x$push[tied] <- FALSE
  x$edge[tied] <- 0; x$model_edge[tied] <- 0; x$ats_correct[tied] <- NA_real_
  x$su_correct[!is.finite(x$expected_margin) | abs(x$expected_margin) < 1e-8 | x$actual_margin == 0] <- NA
  x
}

historical_sp_forecasts <- function(reference, rows) {
  assert_unique_keys(reference, "game_id", "historical reference")
  assert_unique_keys(rows, c("season", "cycle", "team"), "qualified SP rows")
  predictions <- list(); coverage <- list()
  for (lag in 0:1) {
    lane <- if (lag == 0L) "same_cycle_corroborated" else "one_cycle_delayed"
    cycle <- as.character(as.Date(reference$cycle) - lag*7L)
    key <- paste(rows$season, rows$cycle, rows$team)
    h <- match(paste(reference$season, cycle, reference$home), key)
    a <- match(paste(reference$season, cycle, reference$away), key)
    reason <- rep("included", nrow(reference))
    present <- !is.na(h) & !is.na(a)
    qualified <- present & rows$qualified[h] & rows$qualified[a]
    qualified[is.na(qualified)] <- FALSE
    reason[!qualified] <- "row_failed_record_or_prior_evidence_checks"
    reason[!present] <- "missing_corroborated_team_cycle"
    evidence <- pmax(external_time(rows$corroborated_by[h]), external_time(rows$corroborated_by[a]))
    before <- !is.na(evidence) & evidence < reference$kickoff
    reason[qualified & !before] <- "corroboration_not_before_kickoff"
    sp <- rows$rating[h] - rows$rating[a] + ifelse(reference$neutral_site, 0, 2.4)
    reason[qualified & before & !is.finite(sp)] <- "missing_numeric_rating"
    audit <- reference[c("game_id", "season", "week", "home", "away", "kickoff", "is_cfp")]
    audit$lane <- lane; audit$snapshot_cycle <- cycle; audit$reason <- reason
    audit$home_rating <- rows$rating[h]; audit$away_rating <- rows$rating[a]
    audit$corroborated_by <- external_stamp(evidence)
    coverage[[lane]] <- audit
    keep <- reason == "included"
    base <- reference[keep, ]; base$lane <- rep(lane, nrow(base))
    base$evidence_lane <- rep("corroborated_reconstruction_not_timestamp_verified", nrow(base))
    margins <- list(v3_control = base$expected_margin, sp_rating = sp[keep],
      v3_sp_50_50 = (base$expected_margin + sp[keep])/2, market_reference = -base$closing_home_spread)
    predictions[[lane]] <- do.call(rbind, lapply(names(margins), function(model) {
      x <- base; x$model <- rep(model, nrow(x)); x$expected_margin <- margins[[model]]
      historical_sp_grade(x)
    }))
  }
  list(predictions = do.call(rbind, predictions), coverage = do.call(rbind, coverage))
}

historical_sp_metrics <- function(predictions) {
  all <- list()
  for (lane in unique(predictions$lane)) {
    x <- predictions[predictions$lane == lane, ]
    sides <- p4_or_independent_sides(x)
    slices <- list(all_fbs = rep(TRUE, nrow(x)), common_lined = is.finite(x$closing_home_spread),
      week_0_1 = x$week <= 1L, week_2_4 = x$week >= 2L & x$week <= 4L, week_5_plus = x$week >= 5L,
      cfp = x$is_cfp, neutral_site = x$neutral_site, spread_over_21 = abs(x$closing_home_spread) > 21,
      article_audience = sides$home | sides$away)
    for (season in sort(unique(x$season))) slices[[paste0("season_", season)]] <- x$season == season
    mean_or_na <- function(v) if (any(is.finite(v))) mean(v[is.finite(v)]) else NA_real_
    for (slice in names(slices)) for (model in unique(x$model)) {
      y <- x[which(x$model == model & slices[[slice]]), ]
      su <- y$su_correct
      su[!is.finite(y$expected_margin) | y$expected_margin == 0 | y$actual_margin == 0] <- NA
      wins <- sum(y$win); losses <- sum(y$loss)
      all[[length(all)+1L]] <- data.frame(lane = lane, model = model, slice = slice, games = nrow(y),
        margin_games = sum(is.finite(y$mae_error)), margin_mae = mean_or_na(y$mae_error),
        margin_rmse = sqrt(mean_or_na((y$expected_margin-y$actual_margin)^2)),
        margin_bias = mean_or_na(y$expected_margin-y$actual_margin), su_wins = sum(su, na.rm = TRUE),
        su_decisions = sum(!is.na(su)), su_accuracy = mean_or_na(as.numeric(su)), wins = wins, losses = losses,
        pushes = sum(y$push), no_selection = sum(!y$selected), ats_accuracy = if (wins+losses) wins/(wins+losses) else NA_real_)
    }
  }
  do.call(rbind, all)
}

historical_sp_paired <- function(predictions) {
  comparisons <- list(c("sp_rating", "v3_control"), c("v3_sp_50_50", "v3_control"),
    c("market_reference", "v3_control"), c("v3_sp_50_50", "sp_rating"))
  summary <- list(); loso <- list()
  for (lane in unique(predictions$lane)) for (pair in comparisons) {
    a <- predictions[predictions$lane == lane & predictions$model == pair[1], ]
    b <- predictions[predictions$lane == lane & predictions$model == pair[2], ]
    assert_unique_keys(a, "game_id", "paired candidate"); assert_unique_keys(b, "game_id", "paired reference")
    b <- b[match(a$game_id, b$game_id), ]
    if (!identical(a$game_id, b$game_id) || !identical(a$closing_home_spread, b$closing_home_spread)) stop("Unmatched backtest rows/lines.")
    delta <- a$mae_error-b$mae_error; ats <- a$ats_correct-b$ats_correct
    ci <- experiment_season_interval(delta, a$season); aci <- experiment_season_interval(ats, a$season)
    mean_or_na <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
    summary[[length(summary)+1L]] <- data.frame(lane = lane, candidate = pair[1], reference = pair[2],
      margin_common_games = sum(is.finite(delta)), mae_change = mean_or_na(delta), mae_low = ci[1], mae_high = ci[2],
      ats_common_decisions = sum(is.finite(ats)), ats_change = mean_or_na(ats), ats_low = aci[1], ats_high = aci[2])
    for (season in unique(a$season)) loso[[length(loso)+1L]] <- data.frame(lane = lane, candidate = pair[1], reference = pair[2],
      omitted_season = season, mae_change = mean_or_na(delta[a$season != season]), ats_change = mean_or_na(ats[a$season != season]))
  }
  list(paired = do.call(rbind, summary), leave_one_season_out = do.call(rbind, loso))
}

run_external_backtest <- function(project) {
  config <- cfb_v2_config(project, 2026L)
  protected <- experiment_file_hashes(config)
  public <- file.path(project, "cfb_v3/output/experiments/og_audit/20260909T203217Z/public_sources")
  inputs <- c(ledger = file.path(project, "cfb_v3/output/experiments/og_verification/20260909T212008Z/reconstructed_sp_ledger.csv"),
    candidates = file.path(public, "inspection/recovered_sp_candidates.csv"),
    anchors = file.path(project, "cfb_v3/experiments/og_sp_anchors.csv"),
    games = file.path(config$data_dir, "historical_games.csv"),
    reference = file.path(config$output_dir, "backtest/rolling_predictions.csv"))
  if (!all(file.exists(inputs))) stop("Required historical caches missing: ", paste(names(inputs)[!file.exists(inputs)], collapse = ", "))
  output <- external_new_dir(file.path(config$output_dir, "experiments/external_backtest"), "")
  file.copy(file.path(project, "cfb_v3/experiments/EXTERNAL_BACKTEST_PROTOCOL.md"), file.path(output, "protocol.md"))
  files <- c(inputs, file.path(project, c("run_cfb_external_backtest.R", "cfb_v3/experiments/external_backtest.R",
    "cfb_v3/experiments/og_reconstruction.R", "cfb_v3/experiments/controlled_ats.R")), file.path(public, "inventory.json"))
  write.csv(data.frame(path = files, sha256 = vapply(files, function(p) digest::digest(file = p, algo = "sha256"), "")),
    file.path(output, "input_hashes.csv"), row.names = FALSE)
  data <- lapply(inputs, read.csv, stringsAsFactors = FALSE)
  games <- data$games; games$kickoff <- parse_utc_datetime(games$kickoff)
  reference <- historical_sp_reference(data$reference, games)
  checked <- historical_sp_recheck(data$ledger, data$candidates, data$anchors, games, public)
  write.csv(checked$rows, file.path(output, "source_rechecks.csv"), row.names = FALSE)
  write.csv(checked$checks, file.path(output, "numeric_checks.csv"), row.names = FALSE)
  built <- historical_sp_forecasts(reference, checked$rows)
  write.csv(built$coverage, file.path(output, "coverage.csv"), row.names = FALSE)
  if (!nrow(built$predictions)) stop("No qualified historical matchups; inspect coverage.csv.")
  predictions <- built$predictions
  # Missing market margins have no SU/MAE score; a zero forecast is not a SU pick.
  predictions$su_correct[!is.finite(predictions$expected_margin) | predictions$expected_margin == 0 | predictions$actual_margin == 0] <- NA
  write.csv(predictions, file.path(output, "predictions.csv"), row.names = FALSE)
  metrics <- historical_sp_metrics(predictions)
  paired <- historical_sp_paired(predictions)
  write.csv(metrics, file.path(output, "metrics.csv"), row.names = FALSE)
  for (name in names(paired)) write.csv(paired[[name]], file.path(output, paste0(name, ".csv")), row.names = FALSE)
  common <- Reduce(intersect, split(predictions$game_id, predictions$lane))
  if (length(common)) write.csv(historical_sp_metrics(predictions[predictions$game_id %in% common, ]),
    file.path(output, "timing_common_game_metrics.csv"), row.names = FALSE)
  counts <- aggregate(game_id ~ lane + season + reason, built$coverage, length)
  names(counts)[names(counts) == "game_id"] <- "games"
  write.csv(counts, file.path(output, "coverage_counts.csv"), row.names = FALSE)
  unchanged <- identical(protected, experiment_file_hashes(config))
  if (!unchanged) stop("Protected production files changed during backtest.")
  display <- metrics[metrics$slice == "common_lined", c("lane", "model", "games", "wins", "losses", "pushes", "ats_accuracy", "su_accuracy", "margin_mae", "margin_rmse")]
  report <- c("# Historical External Ratings Backtest", "",
    "Corroborated reconstruction, NOT independently timestamped originals. No model refit or production change.",
    "SP+ is a rating-derived baseline, not the publisher's official matchup projection. Blend is fixed 50/50 v3-SP+.",
    "Primary lane requires corroborating evidence before kickoff; no postgame mention is backdated.",
    "Delayed lane uses exactly the previous weekly table; cohorts differ. Common-game timing results are separate.", "",
    "## Identical Lined Games", "", markdown_table(display), "",
    "## Paired Differences", "", markdown_table(paired$paired), "",
    "Negative MAE change favors the candidate. ATS change is a fraction, not percentage points.",
    "Intervals are descriptive whole-season bootstrap (5,000 draws); sparse cycles and previously examined data limit inference.",
    "No actual-price ROI, Friday execution claim, calibration claim, or automatic promotion.",
    "The primary and delayed results are not directly comparable without their common-game timing subset.", "",
    paste("Available reference games:", nrow(reference)),
    paste("Games shared across timing lanes:", length(common)),
    paste("Qualified primary CFP games:", sum(predictions$model == "v3_control" & predictions$lane == "same_cycle_corroborated" & predictions$is_cfp)),
    "Historical FPI and Sagarin are unavailable in this accepted evidence lane; no annual final ratings substituted.",
    "See metrics.csv for season/phase/audience/large-spread slices, coverage.csv for every omission,",
    "source_rechecks.csv for raw-value/record gates, and leave_one_season_out.csv for robustness.")
  writeLines(report, file.path(output, "report.md"))
  jsonlite::write_json(list(status = "complete", evidence_lane = "corroborated_reconstruction_not_timestamp_verified",
    production_unchanged = unchanged, automatic_promotion = FALSE, model_refit = FALSE,
    primary_lane = "same_cycle_corroborated", created_at = external_stamp()),
    file.path(output, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
  external_seal(output)
  list(output = output, metrics = display)
}
