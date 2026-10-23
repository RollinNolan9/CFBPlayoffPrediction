# Run from the repository root. No dependencies or real-data modifications.
source("cfb_v3/experiments/opponent_relative_features.R")
times <- opponent_relative_time(c("2026-09-01T01:00:00Z", "2026-08-31T18:00:00-07:00",
                                 "2026-09-01 01:00:00 UTC"), "fixture")
stopifnot(length(unique(times)) == 1L,
          inherits(try(opponent_relative_time("2026-09-01 invalid", "fixture"), silent = TRUE), "try-error"))
games <- data.frame(
  game_id = paste0("g", 1:6), season = 2026L,
  model_week = c(1L, 1L, 2L, 2L, 3L, 1L),
  home = c("B", "D", "A", "C", "A", "FCS Team"),
  away = c("C", "E", "B", "D", "D", "A"),
  home_level = c(rep("fbs", 5), "fcs"), away_level = "fbs", completed = TRUE,
  margin = c(10, -6, 7, -4, 3, 100),
  home_score = c(20, 7, 17, 14, 24, 100), away_score = c(10, 13, 10, 18, 21, 0),
  postseason_type = "regular", is_cfp = FALSE,
  kickoff = c(rep("2026-08-29T18:00:00Z", 2), rep("2026-09-05T18:00:00Z", 2),
              "2026-09-12T18:00:00Z", "2026-08-29T18:00:00Z"),
  feature_week_start = c(rep("2026-08-24", 2), rep("2026-08-31", 2),
                         "2026-09-07", "2026-08-24"), stringsAsFactors = FALSE)
targets <- data.frame(game_id = c("t3", "t1", "t4", "reset", "reverse"),
  season = c(2026L, 2026L, 2026L, 2027L, 2026L), model_week = c(3L, 1L, 4L, 1L, 3L),
  home = c("A", "A", "A", "A", "C"), away = c("C", "B", "C", "C", "A"),
  feature_week_start = c("2026-09-07", "2026-08-24", "2026-09-14", "2027-08-23", "2026-09-07"),
  home_score = 99, away_score = 7, margin = 92, stringsAsFactors = FALSE)
before_games <- games
before_targets <- targets
result <- opponent_relative_build(games, targets)
values <- setNames(result$features$opponent_relative_margin_diff, result$features$game_id)
stopifnot(identical(result$features$game_id, targets$game_id),
  isTRUE(all.equal(unname(values), c(6.75, 0, 6.3, 0, -6.75), tolerance = 1e-12)),
  identical(games, before_games), identical(targets, before_targets),
  result$qa$eligible_source_games == 5L, result$qa$shrinkage_prior_games == 3L)
g3 <- subset(result$game_audit, game_id == "g3" & team == "A")
stopifnot(g3$team_margin == 7, g3$opponent_prior_mean_margin == 10,
          g3$relative_performance == 17, g3$opponent_prior_games == 1L)
away <- subset(result$game_audit, game_id == "g4" & side == "away")
stopifnot(away$team == "D", away$team_margin == 4,
          away$opponent_prior_mean_margin == -10, away$relative_performance == -6)
first <- subset(result$game_audit, model_week == 1L)
stopifnot(all(first$opponent_prior_games == 0L), all(is.na(first$relative_performance)))
a4 <- subset(result$team_audit, game_id == "t4" & side == "home")
stopifnot(a4$usable_relative_games == 2L, a4$shrinkage_factor == 2/5,
          a4$mean_relative_performance == 9.5, a4$opponent_relative_margin == 3.8)
# A future result cannot alter a prior-week forecast, even when its outcome is huge.
future <- games
future$margin[future$game_id == "g5"] <- -1000
future$home_score[future$game_id == "g5"] <- 0
future$away_score[future$game_id == "g5"] <- 1000
early_targets <- targets[targets$game_id %in% c("t3", "t1", "reverse"), ]
stopifnot(identical(opponent_relative_build(future, early_targets)$features,
                    opponent_relative_build(games, early_targets)$features))
# Same model-week games never enter each other's baselines.
same_week <- games
same_week$margin[same_week$game_id == "g4"] <- 999
changed <- opponent_relative_build(same_week, targets)
stopifnot(subset(changed$game_audit, game_id == "g3" & team == "A")$relative_performance == 17)
# Explicit game-ID exclusion also protects a target if its timing fields differ.
own_target <- games[games$game_id == "g3", ]
own_target$model_week <- 4L
own_target$feature_week_start <- "2026-09-14"
own <- opponent_relative_build(games, own_target)
stopifnot(own$features$home_opponent_relative_margin == 0.5,
          !any(grepl("g3", own$team_audit$relative_source_game_ids, fixed = TRUE)))
# Both baseline and target time filters are strict in addition to model-week cuts.
late <- games
late$kickoff[late$game_id == "g1"] <- "2026-08-31T00:00:00Z"
late_result <- opponent_relative_build(late, targets)
stopifnot(is.na(subset(late_result$game_audit, game_id == "g3" & team == "A")$relative_performance))
tight_target <- targets[targets$game_id == "t4", ]
tight_target$feature_week_start <- "2026-09-12T18:00:00Z"
tight <- opponent_relative_build(games, tight_target)
stopifnot(tight$features$home_opponent_relative_margin == 4.25)
# With no eligible games, all target-team values are neutral and auditable.
none <- games
none$completed <- FALSE
empty <- opponent_relative_build(none, targets)
stopifnot(nrow(empty$game_audit) == 0L,
          all(empty$features$opponent_relative_margin_diff == 0),
          all(empty$team_audit$usable_relative_games == 0L))
cat("Opponent-relative feature tests passed: orientation, opponent-baseline sign, shrinkage, no history, season reset, future/same-week isolation, own-game exclusion, strict time cutoffs, and immutable scores/targets.\n")
