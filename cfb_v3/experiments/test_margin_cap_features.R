# Run from the repository root with Rscript; uses only small synthetic games.
for (module in c("config", "store", "features", "coaches", "adapters", "models",
                 "coach_migration", "historical_data")) {
  source(file.path("cfb_v2", paste0(module, ".R")))
}
source("cfb_v3/experiments/margin_cap_features.R")
config <- cfb_v2_config(getwd(), 2026)
games <- data.frame(
  game_id = 1:6, season = 2020L, week = c(1L, 1L, 1L, 2L, 3L, 3L),
  model_week = c(1L, 1L, 1L, 2L, 3L, 3L),
  home = c("A", "C", "E", "B", "A", "C"),
  away = c("B", "D", "F", "A", "D", "F"),
  margin = c(50, -50, 50, -9, 100, -100),
  home_level = "fbs", away_level = "fbs", completed = TRUE,
  neutral_site = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  postseason_type = "regular", is_cfp = FALSE,
  kickoff = c(rep("2020-09-05T18:00:00Z", 3), "2020-09-12T18:00:00Z",
              rep("2020-09-19T18:00:00Z", 2)),
  feature_week_start = c(rep("2020-08-31", 3), "2020-09-07", rep("2020-09-14", 2)),
  stringsAsFactors = FALSE
)
original_games <- games
original_solver <- opponent_adjusted_rating
original_week <- power_rating_for_week
for (cap in c(Inf, 24, 25, 35, 42)) {
  actual <- margin_cap_power_snapshots(games, config, cap)
  manually_clipped <- games
  manually_clipped$margin <- pmax(-cap, pmin(cap, manually_clipped$margin))
  expected <- power_rating_snapshots(manually_clipped, config)
  stopifnot(isTRUE(all.equal(actual$power, expected, tolerance = 1e-12)),
            all(actual$audit$chronological_pass), all(actual$audit$kickoff_cutoff_pass))
}
# This fixture distinguishes clipping before HFA from clipping HFA-adjusted y.
cap24 <- margin_cap_power_snapshots(games, config, 24)
week2 <- cap24$power[cap24$power$model_week == 2L, ]
stopifnot(week2$power_rating[week2$team == "A"] < week2$power_rating[week2$team == "E"])
future_changed <- games
future_changed$margin[future_changed$model_week >= 2L] <- 10000
future_power <- margin_cap_power_snapshots(future_changed, config, 24)$power
stopifnot(isTRUE(all.equal(week2, future_power[future_power$model_week == 2L, ],
                          tolerance = 1e-12)),
          identical(games, original_games), identical(opponent_adjusted_rating, original_solver),
          identical(power_rating_for_week, original_week))
bad_dates <- games
bad_dates$kickoff[1] <- "2020-09-08T18:00:00Z"
rejected <- try(margin_cap_power_snapshots(bad_dates, config, 24), silent = TRUE)
stopifnot(inherits(rejected, "try-error"))
cat("Margin-cap tests passed: symmetric pre-HFA clipping, all five caps, chronological isolation, input/function immutability, and cutoff rejection.\n")
