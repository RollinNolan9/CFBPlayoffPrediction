# Offline, venue-unadjusted opponent-relative margin candidate. This file has no
# production side effects and uses base R only. Counts are audit data, not model
# predictors. The shrinkage prior is fixed at three games before evaluation.

opponent_relative_time <- function(x, label) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  text <- as.character(x)
  missing <- is.na(text) | text %in% c("", "NA")
  out <- as.POSIXct(rep(NA_real_, length(text)), origin = "1970-01-01", tz = "UTC")
  date <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  time <- "[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?"
  patterns <- c(paste0("^", date, "T", time, "Z$"),
    paste0("^", date, "T", time, "[+-][0-9]{2}:?[0-9]{2}$"),
    paste0("^", date, "[ T]", time, "( UTC)?$"), paste0("^", date, "$"))
  formats <- c("%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%dT%H:%M:%OS%z",
               "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d")
  for (i in seq_along(formats)) {
    remaining <- which(!missing & is.na(out) & grepl(patterns[i], text))
    if (!length(remaining)) next
    value <- text[remaining]
    if (i == 2L) value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
    if (i == 3L) value <- sub(" UTC$", "", gsub("T", " ", value, fixed = TRUE))
    out[remaining] <- as.POSIXct(value, format = formats[i], tz = "UTC")
  }
  if (any(!missing & is.na(out))) stop("Unparseable ", label, " timestamp.")
  out
}

opponent_relative_build <- function(games, targets) {
  required_games <- c("game_id", "season", "model_week", "home", "away",
                      "home_level", "away_level", "completed", "margin")
  required_targets <- c("game_id", "season", "model_week", "home", "away")
  for (field in setdiff(required_games, names(games))) stop("Games lack ", field, ".")
  for (field in setdiff(required_targets, names(targets))) stop("Targets lack ", field, ".")
  if (anyNA(games$game_id) || anyDuplicated(games$game_id) ||
      anyNA(targets$game_id) || anyDuplicated(targets$game_id)) {
    stop("Source and target game IDs must each be present and unique.")
  }
  if (any(!is.finite(targets$season)) || any(!is.finite(targets$model_week))) {
    stop("Every target requires a finite season and chronological model week.")
  }
  if (anyNA(games$home) || anyNA(games$away) || anyNA(targets$home) || anyNA(targets$away)) {
    stop("Team names cannot be missing.")
  }
  source_kickoff <- if ("kickoff" %in% names(games)) {
    opponent_relative_time(games$kickoff, "source kickoff")
  } else as.POSIXct(rep(NA_real_, nrow(games)), origin = "1970-01-01", tz = "UTC")
  source_cutoff <- if ("feature_week_start" %in% names(games)) {
    opponent_relative_time(games$feature_week_start, "source feature cutoff")
  } else as.POSIXct(rep(NA_real_, nrow(games)), origin = "1970-01-01", tz = "UTC")
  target_cutoff <- if ("feature_week_start" %in% names(targets)) {
    opponent_relative_time(targets$feature_week_start, "target feature cutoff")
  } else as.POSIXct(rep(NA_real_, nrow(targets)), origin = "1970-01-01", tz = "UTC")
  completed <- !is.na(games$completed) & as.logical(games$completed)
  eligible <- completed & is.finite(games$margin) & is.finite(games$season) &
    is.finite(games$model_week) & tolower(games$home_level) == "fbs" &
    tolower(games$away_level) == "fbs"
  # Match production's raw FBS history eligibility where postseason metadata exists.
  if ("postseason_type" %in% names(games)) {
    cfp <- if ("is_cfp" %in% names(games)) !is.na(games$is_cfp) & as.logical(games$is_cfp) else FALSE
    eligible <- eligible & !(games$postseason_type %in% c("bowl", "non_cfp_bowl") & !cfp)
  }
  eligible[is.na(eligible)] <- FALSE
  source <- which(eligible)
  # One team-oriented row on each side: the opponent's signed average margin is
  # ADDED, so beating an opponent that usually wins earns more credit.
  index <- rep(source, 2L)
  side <- rep(c("home", "away"), each = length(source))
  team <- c(as.character(games$home[source]), as.character(games$away[source]))
  opponent <- c(as.character(games$away[source]), as.character(games$home[source]))
  margin <- c(games$margin[source], -games$margin[source])
  audit <- data.frame(game_id = as.character(games$game_id[index]),
    season = games$season[index], model_week = games$model_week[index], side = side,
    team = team, opponent = opponent, team_margin = margin,
    kickoff = source_kickoff[index], baseline_cutoff = source_cutoff[index],
    opponent_prior_games = integer(length(index)),
    opponent_prior_mean_margin = rep(NA_real_, length(index)),
    relative_performance = rep(NA_real_, length(index)),
    opponent_prior_game_ids = rep("", length(index)),
    stringsAsFactors = FALSE)
  group_key <- function(team, season) paste(team, season, sep = "\r")
  groups <- split(seq_len(nrow(audit)), group_key(audit$team, audit$season))
  prior_rows <- function(team, season, week, cutoff, game_id) {
    rows <- groups[[group_key(team, season)]]
    if (is.null(rows)) return(integer())
    keep <- audit$model_week[rows] < week & audit$game_id[rows] != as.character(game_id)
    if (!is.na(cutoff)) keep <- keep & !is.na(audit$kickoff[rows]) & audit$kickoff[rows] < cutoff
    rows[which(keep)]
  }
  for (i in seq_len(nrow(audit))) {
    rows <- prior_rows(audit$opponent[i], audit$season[i], audit$model_week[i],
                       audit$baseline_cutoff[i], audit$game_id[i])
    audit$opponent_prior_games[i] <- length(rows)
    audit$opponent_prior_game_ids[i] <- paste(audit$game_id[rows], collapse = ",")
    if (length(rows)) {
      audit$opponent_prior_mean_margin[i] <- mean(audit$team_margin[rows])
      audit$relative_performance[i] <- audit$team_margin[i] + audit$opponent_prior_mean_margin[i]
    }
  }
  target_index <- rep(seq_len(nrow(targets)), 2L)
  target_team <- c(as.character(targets$home), as.character(targets$away))
  team_audit <- data.frame(game_id = as.character(targets$game_id[target_index]),
    season = targets$season[target_index], model_week = targets$model_week[target_index],
    side = rep(c("home", "away"), each = nrow(targets)), team = target_team,
    target_cutoff = target_cutoff[target_index], prior_team_games = integer(length(target_index)),
    usable_relative_games = integer(length(target_index)),
    mean_relative_performance = rep(NA_real_, length(target_index)),
    shrinkage_factor = numeric(length(target_index)),
    opponent_relative_margin = numeric(length(target_index)),
    relative_source_game_ids = rep("", length(target_index)), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(team_audit))) {
    rows <- prior_rows(team_audit$team[i], team_audit$season[i], team_audit$model_week[i],
                       team_audit$target_cutoff[i], team_audit$game_id[i])
    team_audit$prior_team_games[i] <- length(rows)
    rows <- rows[is.finite(audit$relative_performance[rows])]
    n <- length(rows)
    team_audit$usable_relative_games[i] <- n
    team_audit$shrinkage_factor[i] <- n / (n + 3)
    team_audit$relative_source_game_ids[i] <- paste(audit$game_id[rows], collapse = ",")
    if (n) {
      value <- mean(audit$relative_performance[rows])
      team_audit$mean_relative_performance[i] <- value
      team_audit$opponent_relative_margin[i] <- value * n / (n + 3)
    }
  }
  n <- nrow(targets)
  home_value <- team_audit$opponent_relative_margin[seq_len(n)]
  away_value <- team_audit$opponent_relative_margin[n + seq_len(n)]
  features <- data.frame(game_id = targets$game_id,
    home_opponent_relative_margin = home_value,
    away_opponent_relative_margin = away_value,
    opponent_relative_margin_diff = home_value - away_value,
    stringsAsFactors = FALSE)
  stopifnot(all(is.finite(features$opponent_relative_margin_diff)),
            identical(features$game_id, targets$game_id))
  list(features = features, game_audit = audit, team_audit = team_audit,
    qa = list(source_games = nrow(games), eligible_source_games = length(source),
      target_games = nrow(targets), historical_team_performances = nrow(audit),
      defined_relative_performances = sum(is.finite(audit$relative_performance)),
      performances_without_opponent_history = sum(audit$opponent_prior_games == 0L),
      target_team_rows_without_usable_history = sum(team_audit$usable_relative_games == 0L),
      source_team_rows_with_time_cutoff = sum(!is.na(audit$baseline_cutoff)),
      target_team_rows_with_time_cutoff = sum(!is.na(team_audit$target_cutoff)),
      shrinkage_prior_games = 3L, actual_scores_and_targets_unchanged = TRUE,
      count_predictors_added = FALSE, venue_adjustment = FALSE, margin_caps = FALSE,
      definition = "Team margin plus opponent mean signed margin available before that game; mean of prior same-season relative performances shrunk by n/(n+3).",
      cutoff_policy = "Strictly earlier same-season model weeks; also kickoff strictly before each supplied feature-week-start cutoff."))
}
