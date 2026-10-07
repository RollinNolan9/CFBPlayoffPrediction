# Minimal walk-forward margin-rating baseline (base R only).
#
# Model: margin_home = hfa * !neutral + r_home - r_away + fcs * (fcs_home - fcs_away)
# fit by weighted ridge least squares. Before each kickoff date the model is refit
# on games with kickoff strictly before that date (current + prior season), so no
# prediction can see its own game or any later result.

load_baseline_games <- function(path = "cfb_v2/data/historical_games.csv") {
  g <- read.csv(path, stringsAsFactors = FALSE)
  g$kickoff <- as.Date(g$kickoff)
  g$neutral_site <- as.logical(g$neutral_site)
  g$margin <- g$home_score - g$away_score
  g <- g[!is.na(g$margin) & !is.na(g$kickoff), ]
  g[order(g$kickoff, g$game_id), ]
}

fit_ratings <- function(train, season, w_prev, lambda, cap) {
  teams <- sort(unique(c(train$home, train$away)))
  n <- nrow(train)
  X <- matrix(0, n, length(teams) + 2, dimnames = list(NULL, c(teams, ".hfa", ".fcs")))
  X[cbind(seq_len(n), match(train$home, teams))] <- 1
  X[cbind(seq_len(n), match(train$away, teams))] <- -1
  X[, ".hfa"] <- as.numeric(!train$neutral_site)
  X[, ".fcs"] <- (train$home_level == "fcs") - (train$away_level == "fcs")
  y <- pmax(pmin(train$margin, cap), -cap)
  w <- ifelse(train$season == season, 1, w_prev)
  penalty <- c(rep(lambda, length(teams)), 1e-6, 1e-6)
  XtW <- t(X * w)
  beta <- solve(XtW %*% X + diag(penalty), XtW %*% y)[, 1]
  list(ratings = beta[teams], hfa = beta[[".hfa"]], fcs = beta[[".fcs"]])
}

predict_margin <- function(fit, games) {
  r <- function(team) {
    v <- fit$ratings[team]
    ifelse(is.na(v), 0, v)
  }
  fit$hfa * (!games$neutral_site) + r(games$home) - r(games$away) +
    fit$fcs * ((games$home_level == "fcs") - (games$away_level == "fcs"))
}

# Fits margin ~ elo_diff + home on prior games, a walk-forward map from CFBD
# pregame Elo to a margin so Elo can be scored against the spread as well.
elo_margin <- function(train, test) {
  train <- train[!is.na(train$home_pregame_elo) & !is.na(train$away_pregame_elo), ]
  if (nrow(train) < 50) return(rep(NA_real_, nrow(test)))
  m <- lm(margin ~ I(home_pregame_elo - away_pregame_elo) + I(as.numeric(!neutral_site)), train)
  predict(m, test)
}

walk_forward <- function(g, seasons, w_prev, lambda, cap, with_elo = FALSE) {
  out <- list()
  for (s in seasons) {
    for (d in sort(unique(g$kickoff[g$season == s]))) {
      test <- g[g$season == s & g$kickoff == d, ]
      train <- g[g$kickoff < d & g$season >= s - 1, ]
      stopifnot(all(train$kickoff < min(test$kickoff)))
      fit <- fit_ratings(train, s, w_prev, lambda, cap)
      test$pred_margin <- predict_margin(fit, test)
      if (with_elo) test$elo_margin <- elo_margin(train, test)
      out[[length(out) + 1]] <- test
    }
  }
  do.call(rbind, out)
}

# Win probability uses a normal error whose sd comes only from earlier
# out-of-sample residuals (16 points until 200 residuals exist).
add_win_prob <- function(p, margin_col, prob_col) {
  p <- p[order(p$kickoff), ]
  sigma <- rep(16, nrow(p))
  for (d in unique(p$kickoff)) {
    past <- p$kickoff < d & !is.na(p[[margin_col]])
    if (sum(past) >= 200) sigma[p$kickoff == d] <- sd(p$margin[past] - p[[margin_col]][past])
  }
  p[[prob_col]] <- pnorm(p[[margin_col]] / sigma)
  p
}

winner_metrics <- function(p, prob_col) {
  p <- p[!is.na(p[[prob_col]]) & p$margin != 0, ]
  y <- as.numeric(p$margin > 0)
  q <- pmin(pmax(p[[prob_col]], 1e-6), 1 - 1e-6)
  data.frame(games = nrow(p), su_accuracy = mean((q > 0.5) == (y == 1)),
             brier = mean((q - y)^2), log_loss = -mean(y * log(q) + (1 - y) * log(1 - q)))
}

# ATS: back home when predicted margin beats the market's (-spread). Pushes and
# zero-edge games are excluded; ROI assumes a flat -110 price.
ats_metrics <- function(p, margin_col, min_edge = 0) {
  p <- p[!is.na(p$closing_home_spread) & !is.na(p[[margin_col]]), ]
  edge <- p[[margin_col]] + p$closing_home_spread
  result <- p$margin + p$closing_home_spread
  keep <- abs(edge) > 0 & abs(edge) >= min_edge
  edge <- edge[keep]; result <- result[keep]
  pushes <- sum(result == 0)
  win <- sign(edge[result != 0]) == sign(result[result != 0])
  n <- length(win); k <- sum(win)
  z <- 1.96; ph <- k / n
  half <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  centre <- (ph + z^2 / (2 * n)) / (1 + z^2 / n)
  data.frame(min_edge = min_edge, bets = n, wins = k, losses = n - k, pushes = pushes,
             ats_rate = ph, ci_low = centre - half, ci_high = centre + half,
             roi = (k * 100 / 110 - (n - k)) / n)
}
