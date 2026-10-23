args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1L]] else "cfb_v3/output/2026/week_4/lacy_scenarios"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
suppressPackageStartupMessages(library(cfbfastR))
inputs <- list(
  fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  season = 2026L, through_week = 3L,
  usage = cfbd_player_usage(year = 2026, team = "Ole Miss", excl_garbage_time = FALSE),
  ppa = cfbd_metrics_ppa_players_season(year = 2026, team = "Ole Miss", excl_garbage_time = FALSE),
  team_stats = cfbd_stats_season_team(year = 2026, team = "Ole Miss", season_type = "regular", start_week = 1, end_week = 3),
  games = cfbd_game_info(year = 2026, team = "Ole Miss", season_type = "regular")
)
stopifnot(all(vapply(inputs[c("usage", "ppa", "team_stats", "games")], function(x) is.data.frame(x) && nrow(x) > 0, logical(1))))
if (any(inputs$games$completed & inputs$games$week > inputs$through_week, na.rm = TRUE)) {
  stop("Season-level player data now includes games after the Week 4 prediction cutoff. Use the saved snapshot.")
}
inputs$fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
saveRDS(inputs, file.path(out_dir, "player_inputs.rds"))
for (key in c("usage", "ppa", "team_stats", "games")) {
  dat <- inputs[[key]]
  write.csv(dat, file.path(out_dir, paste0(key, ".csv")), row.names = FALSE)
  cat("\n", key, ": ", nrow(dat), " rows\n", sep = "")
  print(names(dat))
  if (key %in% c("usage", "ppa")) print(as.data.frame(dat[dat$position == "RB", ]))
  if (key == "team_stats") print(as.data.frame(dat))
}
