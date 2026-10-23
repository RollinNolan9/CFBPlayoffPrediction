# Isolated power/direct-prior decay experiments; coaching and other inputs stay fixed.
history_decay_models <- c("baseline", "early_week5", "smooth_moderate", "smooth_fast", "current_only_week5")

history_decay_cap <- function(model, week) {
  stopifnot(model %in% history_decay_models, all(is.finite(week)))
  week <- as.integer(week)
  caps <- switch(model,
    baseline = c(rep(1, 8), .10),
    early_week5 = c(rep(1, 5), .10),
    smooth_moderate = c(1, 1, .60, .45, .30, .20, .15, .10),
    smooth_fast = c(1, 1, .50, .30, .20, .10),
    current_only_week5 = c(rep(1, 5), 0))
  caps[pmin(pmax(week, 0L) + 1L, length(caps))]
}

history_decay_scope <- function(model) {
  stopifnot(model %in% history_decay_models)
  scope <- new.env(parent = environment(power_rating_for_week))
  original_weights <- power_history_weights
  scope$weight_audit <- list()
  scope$power_history_weights <- function(past, season, phase_week, config) {
    w <- original_weights(past, season, phase_week, config)
    current <- past$season == season
    cap <- history_decay_cap(model, phase_week)
    current_mass <- sum(w[current]); historical_mass <- sum(w[!current])
    # With no current observations, defer the experimental cap to avoid an empty fit.
    deferred <- cap < 1 && current_mass == 0 && historical_mass > 0
    if (cap < 1 && current_mass > 0 && historical_mass > 0) {
      allowed <- current_mass * cap / (1 - cap)
      w[!current] <- w[!current] * min(1, allowed / historical_mass)
    }
    total <- sum(w)
    share <- if (total > 0) sum(w[!current]) / total else 0
    stopifnot(all(is.finite(w)), all(w >= 0), all(w[current] == original_weights(past, season, phase_week, config)[current]))
    if (!deferred && cap < 1 && total > 0) stopifnot(share <= cap + 1e-10)
    scope$weight_audit[[length(scope$weight_audit) + 1L]] <- data.frame(
      model = model, season = season, phase_week = phase_week, cap = cap,
      current_mass = current_mass, historical_mass_before = historical_mass,
      historical_mass_after = sum(w[!current]), historical_share = share,
      deferred_no_current_games = deferred)
    w
  }
  scope$power_rating_for_week <- power_rating_for_week
  environment(scope$power_rating_for_week) <- scope
  scope
}

history_decay_rebuild <- function(data, power, model, config) {
  before <- data
  key <- paste(power$team, power$season, power$model_week, sep = "\r")
  assert_unique_keys(power, c("team", "season", "model_week"), "decay power snapshots")
  for (side in c("home", "away")) {
    value <- power$power_rating[match(paste(data[[side]], data$season, data$model_week, sep = "\r"), key)]
    value[!is.finite(value)] <- 0
    data[[paste0(side, "_power")]] <- value
    field <- paste0(side, "_power_rating")
    value[is.na(before[[field]])] <- NA_real_
    data[[field]] <- value
  }
  # Labor Day Monday Week 1 games share a Week 2 power snapshot; preserve declared Week 0-1 inputs.
  early <- data$phase_week <= 1L
  for (field in intersect(c("home_power", "away_power", "home_power_rating", "away_power_rating"), names(before))) {
    data[[field]][early] <- before[[field]][early]
  }
  data$power_rating_diff <- data$home_power_rating - data$away_power_rating
  data$pregame_expected_margin <- data$power_rating_diff + data$home_field_points
  cap <- history_decay_cap(model, data$phase_week)
  data$prior_history_weight <- rowMeans(cbind(
    pmin(prior_season_feature_weight(data$phase_week, data$home_games_played, config), cap),
    pmin(prior_season_feature_weight(data$phase_week, data$away_games_played, config), cap)))
  for (field in c("prior_season", "trailing_3yr")) {
    data[[paste0(field, "_diff")]] <- (data[[paste0("home_", field)]] -
      data[[paste0("away_", field)]]) * data$prior_history_weight
  }
  changed <- c("home_power", "away_power", "home_power_rating", "away_power_rating",
    "power_rating_diff", "pregame_expected_margin", "prior_history_weight", "prior_season_diff", "trailing_3yr_diff")
  stopifnot(identical(before[setdiff(names(before), changed)], data[setdiff(names(before), changed)]))
  data
}

# Calendar-week boundaries and the ablation tail must not shift to chronological week.
stopifnot(identical(history_decay_cap("baseline", c(0, 1, 7, 8, 99)), c(1, 1, 1, .1, .1)),
  identical(history_decay_cap("early_week5", c(4, 5, 99)), c(1, .1, .1)),
  identical(history_decay_cap("smooth_moderate", 2:7), c(.6, .45, .3, .2, .15, .1)),
  identical(history_decay_cap("smooth_fast", 2:5), c(.5, .3, .2, .1)),
  identical(history_decay_cap("current_only_week5", c(4, 5, 99)), c(1, 0, 0)))
