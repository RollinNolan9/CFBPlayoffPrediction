compare_model_versions <- function(config) {
  current_path <- file.path(config$output_dir, "backtest", "rolling_predictions.csv")
  if (!file.exists(current_path)) stop("Run the v3 backtest before comparing versions.")
  reference <- config
  reference$data_dir <- file.path(config$project_dir, "cfb_v2", "data")
  reference$inbox_dir <- file.path(config$project_dir, "cfb_v2", "inbox")
  directory <- file.path(config$output_dir, "comparison")
  reference$output_dir <- file.path(directory, "v2_corrected_validation")
  message("Re-evaluating frozen v2 features with the same chronological validation")
  baseline <- run_v2_backtest(reference, include_challengers = FALSE)
  old <- utils::read.csv(baseline$predictions)
  new <- utils::read.csv(current_path)
  old <- old[old$model == "ridge_core", ]
  new <- new[new$model == "ridge_core", ]
  paired <- merge(old, new, by = c("game_id", "season"), suffixes = c("_v2", "_v3"))
  stopifnot(nrow(paired) == nrow(new), nrow(paired) == nrow(old),
            all(paired$actual_margin_v2 == paired$actual_margin_v3))
  fbs <- paired$home_level_v3 == "fbs" & paired$away_level_v3 == "fbs"
  slices <- list(
    fbs_vs_fbs = fbs,
    week_0_1 = fbs & paired$week_v3 <= 1L & paired$game_phase_v3 == "preseason",
    week_2 = fbs & paired$week_v3 == 2L & paired$postseason_type_v3 == "regular",
    week_5_plus = fbs & paired$week_v3 >= 5L & paired$postseason_type_v3 == "regular",
    cfp = as.logical(paired$is_cfp_v3),
    large_spreads = fbs & abs(paired$closing_home_spread_v3) > 21,
    transitions = as.logical(paired$fbs_transition_game_v3)
  )
  set.seed(config$seed)
  summary <- do.call(rbind, lapply(names(slices), function(slice) {
    keep <- slices[[slice]]; keep[is.na(keep)] <- FALSE
    d <- paired[keep, ]; delta <- d$absolute_error_v3-d$absolute_error_v2
    seasons <- unique(d$season)
    interval <- if (length(seasons) > 1L) {
      totals <- tapply(delta, d$season, sum)
      counts <- table(d$season)
      boot <- replicate(2000, {
        draw <- sample(seq_along(totals), replace = TRUE)
        sum(totals[draw])/sum(counts[draw])
      })
      as.numeric(stats::quantile(boot, c(.025, .975)))
    } else c(NA_real_, NA_real_)
    data.frame(slice = slice, games = nrow(d),
      v2_mae = mean(d$absolute_error_v2), v3_mae = mean(d$absolute_error_v3),
      mae_change = mean(delta), change_low = interval[1], change_high = interval[2],
      v2_ats = mean(d$ats_correct_v2, na.rm = TRUE),
      v3_ats = mean(d$ats_correct_v3, na.rm = TRUE),
      v2_su = mean(d$winner_correct_v2), v3_su = mean(d$winner_correct_v3))
  }))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, file.path(directory, "summary.csv"), row.names = FALSE)
  utils::write.csv(paired, file.path(directory, "paired_predictions.csv"), row.names = FALSE)
  report <- c(
    "# CFB 3.0.0 Validation", "",
    "The frozen v2 feature set and the rebuilt v3 feature set use the same chronological",
    "penalty/coach selection and out-of-fold uncertainty procedure. V2 source files and",
    "published snapshots were not overwritten. These v2 figures can differ from the old",
    "report, which selected settings using all of its reported test seasons.", "",
    "Negative MAE change favors v3. Confidence intervals resample entire seasons;",
    "there are only four outer test seasons, so uncertainty remains substantial.", "",
    markdown_table(summary), "",
    "## Interpretation", "",
    "ATS values above are requested model sides against historical closing lines,",
    "excluding pushes. They are not a record of validated official bets. The separate",
    "ATS holdout check locks thresholds before the last test season. A null calibration",
    "or unsuccessful holdout produces no official plays; requested article picks remain available.", "",
    "Historical preseason source values are backfilled API data, not independently",
    "verified archives of what was available at each original publication time.", "",
    "## Reproduce", "",
    "Run `run_cfb_v3.R --mode=backtest`, then `run_cfb_v3.R --mode=compare`.", ""
  )
  path <- file.path(directory, "report.md")
  writeLines(report, path)
  list(report = path, summary = summary)
}
