# Walk-forward rating baseline: tune on 2021 by margin MAE, test frozen on 2022-2025.
# Usage: Rscript run_cfb_baseline.R
source("cfb_baseline/ratings_baseline.R")

g <- load_baseline_games()
dir.create("cfb_baseline/output", showWarnings = FALSE, recursive = TRUE)

grid <- expand.grid(w_prev = c(0.1, 0.25, 0.5), lambda = c(0.5, 1, 2, 5), cap = c(28, 999))
grid$mae_2021 <- vapply(seq_len(nrow(grid)), function(i) {
  p <- walk_forward(g, 2021, grid$w_prev[i], grid$lambda[i], grid$cap[i])
  mean(abs(p$margin - p$pred_margin))
}, numeric(1))
best <- grid[which.min(grid$mae_2021), ]
cat("2021 tuning grid (margin MAE):\n"); print(grid[order(grid$mae_2021), ], row.names = FALSE)

test_seasons <- 2022:2025
p <- walk_forward(g, test_seasons, best$w_prev, best$lambda, best$cap, with_elo = TRUE)
p$market_margin <- -p$closing_home_spread
p <- add_win_prob(p, "pred_margin", "p_baseline")
p <- add_win_prob(p, "elo_margin", "p_elo")
p <- add_win_prob(p, "market_margin", "p_market")
write.csv(p[, c("game_id", "season", "week", "kickoff", "home", "away", "neutral_site",
                "home_score", "away_score", "margin", "closing_home_spread", "pred_margin",
                "elo_margin", "p_baseline", "p_elo", "p_market")],
          "cfb_baseline/output/predictions_2022_2025.csv", row.names = FALSE)

lined <- p[!is.na(p$closing_home_spread) & !is.na(p$elo_margin), ]
mae <- function(col) mean(abs(lined$margin - lined[[col]]))
winners <- rbind(
  cbind(model = "baseline_ratings", winner_metrics(lined, "p_baseline"), margin_mae = mae("pred_margin")),
  cbind(model = "cfbd_elo", winner_metrics(lined, "p_elo"), margin_mae = mae("elo_margin")),
  cbind(model = "market_closing_spread", winner_metrics(lined, "p_market"), margin_mae = mae("market_margin")))

ats <- rbind(
  cbind(model = "baseline_ratings", do.call(rbind, lapply(c(0, 3, 7), function(e) ats_metrics(lined, "pred_margin", e)))),
  cbind(model = "cfbd_elo", do.call(rbind, lapply(c(0, 3, 7), function(e) ats_metrics(lined, "elo_margin", e)))))

by_season <- do.call(rbind, lapply(test_seasons, function(s) {
  q <- lined[lined$season == s, ]
  cbind(season = s, winner_metrics(q, "p_baseline")[, c("games", "su_accuracy", "brier")],
        margin_mae = mean(abs(q$margin - q$pred_margin)),
        ats_metrics(q, "pred_margin")[, c("wins", "losses", "ats_rate", "roi")])
}))

fmt <- function(df) {
  num <- vapply(df, is.double, logical(1))
  df[num] <- lapply(df[num], round, 4)
  df
}
cat("\nSelected:", sprintf("w_prev=%s lambda=%s cap=%s", best$w_prev, best$lambda, best$cap), "\n")
cat("\nWinner metrics, 2022-2025 lined games:\n"); print(fmt(winners), row.names = FALSE)
cat("\nATS (break-even 52.38% at -110):\n"); print(fmt(ats), row.names = FALSE)
cat("\nBaseline by season:\n"); print(fmt(by_season), row.names = FALSE)
