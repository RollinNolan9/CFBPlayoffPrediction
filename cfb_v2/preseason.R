preseason_cache_path <- function(config, name, season) {
  file.path(config$project_dir, "cfb_v2", "cache", "preseason",
            paste0(name, "_", season, ".rds"))
}

pull_preseason_source <- function(config, name, season, refresh, puller) {
  path <- preseason_cache_path(config, name, season)
  if (!refresh && file.exists(path)) return(readRDS(path))
  require_v2_package("cfbfastR")
  if (!nzchar(Sys.getenv("CFBD_API_KEY"))) {
    stop("CFBD_API_KEY is not set and no cached preseason ", name,
         " snapshot exists for ", season, ".", call. = FALSE)
  }
  raw <- puller(as.integer(season))
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) {
    stop("CFBD preseason ", name, " returned no rows for ", season,
         "; the empty response was not cached.", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(raw, path)
  raw
}

pull_cfbd_returning_production <- function(season, config, refresh = FALSE) {
  pull_preseason_source(config, "returning", season, refresh, function(year) {
    cfbfastR::cfbd_player_returning(year = year)
  })
}

pull_cfbd_team_talent <- function(season, config, refresh = FALSE) {
  pull_preseason_source(config, "talent", season, refresh, function(year) {
    cfbfastR::cfbd_team_talent(year = year)
  })
}

pull_cfbd_transfer_portal <- function(season, config, refresh = FALSE) {
  pull_preseason_source(config, "portal", season, refresh, function(year) {
    cfbfastR::cfbd_recruiting_transfer_portal(year = year)
  })
}

pull_cfbd_player_ppa <- function(season, config, refresh = FALSE) {
  pull_preseason_source(config, "player_ppa", season, refresh, function(year) {
    cfbfastR::cfbd_metrics_ppa_players_season(
      year = year, excl_garbage_time = TRUE
    )
  })
}

pull_cfbd_preseason_polls <- function(season, config, refresh = FALSE) {
  pull_preseason_source(config, "polls", season, refresh, function(year) {
    cfbfastR::cfbd_rankings(year = year, week = 1, season_type = "regular")
  })
}

as_share <- function(x) {
  x <- as.numeric(x)
  finite <- x[is.finite(x)]
  # CFBD PPA shares can legitimately exceed 1 or be strongly negative when
  # the team denominator is near zero. Detect the dominant unit scale instead
  # of letting one PPA outlier rescale every team.
  if (length(finite) && mean(abs(finite) <= 2) < 0.5) x <- x / 100
  x
}

percentile_rank <- function(x) {
  x <- as.numeric(x)
  n <- sum(is.finite(x))
  if (!n) return(rep(NA_real_, length(x)))
  rank(x, ties.method = "average", na.last = "keep") / n
}

normal_score_rank <- function(x) {
  x <- as.numeric(x)
  finite <- is.finite(x)
  out <- rep(NA_real_, length(x))
  n <- sum(finite)
  if (!n) return(out)
  probability <- (rank(x[finite], ties.method = "average") - 0.5) / n
  out[finite] <- stats::qnorm(probability)
  out
}

preseason_challenger_feature_names <- function() {
  c("returning_ppa_pct", "returning_passing_ppa_pct", "returning_usage_pct",
    "talent_percentile", "preseason_poll_vote_share", "retained_quality",
    "replacement_capacity", "portal_replacement_capacity",
    "portal_offense_replacement", "hype_gap")
}

preseason_feature_profile <- function(config,
                                      profile = c("full", "returning_only")) {
  profile <- match.arg(profile)
  if (profile == "full") config$preseason$production_features else
    config$preseason$fallback_features
}

normalize_returning_production <- function(raw, season) {
  team <- first_existing_column(raw, c("team", "school"), label = "returning team")
  ppa <- first_existing_column(raw, c("percent_ppa", "percentPPA"),
                               label = "returning percent PPA")
  passing <- first_existing_column(raw, c("percent_passing_ppa", "percentPassingPPA"),
                                   label = "returning percent passing PPA")
  usage <- first_existing_column(raw, c("usage", "percent_usage", "usagePercent"),
                                 label = "returning usage")
  season_column <- first_existing_column(raw, c("season", "year"), required = FALSE)
  out <- data.frame(
    team = canonical_team(raw[[team]]),
    returning_ppa_pct = as_share(raw[[ppa]]),
    returning_passing_ppa_pct = as_share(raw[[passing]]),
    returning_usage_pct = as_share(raw[[usage]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(season_column)) {
    out <- out[as.integer(raw[[season_column]]) == as.integer(season), , drop = FALSE]
  }
  out
}

normalize_team_talent <- function(raw, season) {
  team <- first_existing_column(raw, c("school", "team"), label = "talent team")
  talent <- first_existing_column(raw, c("talent"), label = "talent composite")
  season_column <- first_existing_column(raw, c("year", "season"), required = FALSE)
  out <- data.frame(
    team = canonical_team(raw[[team]]),
    talent = as.numeric(raw[[talent]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(season_column)) {
    out <- out[as.integer(raw[[season_column]]) == as.integer(season), , drop = FALSE]
  }
  out
}

build_returning_only_priors <- function(returning, prior_strength, season,
                                        teams = character()) {
  season <- as.integer(season)
  assert_columns(prior_strength, c("team", "season", "strength"), "prior strength")
  returning <- normalize_returning_production(returning, season)
  assert_unique_keys(returning, "team", "returning production")

  team_universe <- sort(unique(c(returning$team, canonical_team(teams))))
  team_universe <- team_universe[!is.na(team_universe) & nzchar(team_universe)]
  if (!length(team_universe)) {
    stop("Returning-production fallback has no teams for ", season, ".", call. = FALSE)
  }

  prior <- prior_strength[as.integer(prior_strength$season) == season - 1L, , drop = FALSE]
  prior_teams <- canonical_team(prior$team)
  prior_value <- as.numeric(prior$strength[match(team_universe, prior_teams)])
  index <- match(team_universe, returning$team)
  returning_ppa <- returning$returning_ppa_pct[index]

  out <- data.frame(
    team = team_universe,
    season = season,
    returning_ppa_pct = returning_ppa,
    returning_passing_ppa_pct = returning$returning_passing_ppa_pct[index],
    returning_usage_pct = returning$returning_usage_pct[index],
    stringsAsFactors = FALSE
  )
  out$retained_quality <- returning_ppa * percentile_rank(prior_value)
  out$returning_data_available <- !is.na(index)
  out$preseason_profile <- "returning_only_no_current_talent"
  out$source <- "cfbd_returning_no_current_talent"
  out$captured_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  assert_unique_keys(out, c("team", "season"), "returning-only preseason priors")
  out
}

normalize_transfer_portal <- function(raw, season) {
  if (is.null(raw) || !nrow(raw)) {
    return(data.frame(
      first_name = character(), last_name = character(), position = character(),
      origin = character(), destination = character(), transfer_date = as.Date(character()),
      rating = numeric(), stringsAsFactors = FALSE
    ))
  }
  origin <- first_existing_column(
    raw, c("origin", "from_team", "fromTeam"), label = "portal origin"
  )
  destination <- first_existing_column(
    raw, c("destination", "to_team", "toTeam"), label = "portal destination"
  )
  rating <- first_existing_column(
    raw, c("rating", "transfer_rating", "transferRating"), label = "portal rating"
  )
  optional <- function(candidates, default = NA_character_) {
    column <- first_existing_column(raw, candidates, required = FALSE)
    if (is.na(column)) rep(default, nrow(raw)) else raw[[column]]
  }
  season_column <- first_existing_column(raw, c("season", "year"), required = FALSE)
  out <- data.frame(
    first_name = as.character(optional(c("first_name", "firstName"))),
    last_name = as.character(optional(c("last_name", "lastName"))),
    position = toupper(as.character(optional("position"))),
    origin = canonical_team(raw[[origin]]),
    destination = canonical_team(raw[[destination]]),
    transfer_date = as.Date(optional(c("transfer_date", "transferDate"))),
    rating = as.numeric(raw[[rating]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(season_column)) {
    out <- out[as.integer(raw[[season_column]]) == as.integer(season), , drop = FALSE]
  }
  cutoff <- as.Date(sprintf("%d-08-20", as.integer(season)))
  out <- out[is.na(out$transfer_date) | out$transfer_date <= cutoff, , drop = FALSE]
  finite <- is.finite(out$rating)
  if (any(finite) && stats::median(out$rating[finite]) > 2) {
    out$rating[finite] <- out$rating[finite] / 100
  }
  out$rating[finite] <- pmax(0, pmin(1, out$rating[finite]))
  out
}

summarize_transfer_portal <- function(raw, teams, season, roster_slots = 22L) {
  portal <- normalize_transfer_portal(raw, season)
  teams <- canonical_team(teams)
  roster_slots <- as.integer(roster_slots)
  if (!is.finite(roster_slots) || roster_slots < 1L) {
    stop("Portal roster_slots must be a positive integer.", call. = FALSE)
  }
  incoming_count <- vapply(teams, function(team) {
    sum(portal$destination == team, na.rm = TRUE)
  }, integer(1))
  outgoing_count <- vapply(teams, function(team) {
    sum(portal$origin == team, na.rm = TRUE)
  }, integer(1))
  rated_count <- vapply(teams, function(team) {
    sum(portal$destination == team & is.finite(portal$rating), na.rm = TRUE)
  }, integer(1))
  incoming_value <- vapply(teams, function(team) {
    ratings <- portal$rating[portal$destination == team & is.finite(portal$rating)]
    sum(head(sort(ratings, decreasing = TRUE), roster_slots)) / roster_slots
  }, numeric(1))
  incoming_percentile <- percentile_rank(incoming_value)
  incoming_percentile[incoming_value <= 0] <- 0
  data.frame(
    team = teams,
    portal_incoming_count = incoming_count,
    portal_outgoing_count = outgoing_count,
    portal_rated_incoming_count = rated_count,
    portal_incoming_value = incoming_value,
    portal_incoming_percentile = incoming_percentile,
    stringsAsFactors = FALSE
  )
}

canonical_player_name <- function(x) {
  original <- as.character(x)
  transliterated <- suppressWarnings(iconv(
    original, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""
  ))
  transliterated[is.na(transliterated)] <- original[is.na(transliterated)]
  normalized <- tolower(transliterated)
  normalized <- gsub("[^a-z0-9 ]", "", normalized)
  normalized <- gsub("[ ]+(jr|sr|ii|iii|iv)$", "", normalized)
  gsub("[ ]+", "", normalized)
}

normalize_player_ppa <- function(raw, season) {
  if (is.null(raw) || !nrow(raw)) {
    return(data.frame(
      player_name = character(), team = character(), position = character(),
      total_ppa = numeric(), stringsAsFactors = FALSE
    ))
  }
  name <- first_existing_column(raw, c("name", "player", "athlete_name"),
                                label = "player PPA name")
  team <- first_existing_column(raw, c("team", "school"), label = "player PPA team")
  position <- first_existing_column(raw, "position", label = "player PPA position")
  total_ppa <- first_existing_column(
    raw, c("total_PPA_all", "total_ppa_all", "total_ppa"),
    label = "player total PPA"
  )
  season_column <- first_existing_column(raw, c("season", "year"), required = FALSE)
  out <- data.frame(
    player_name = canonical_player_name(raw[[name]]),
    team = canonical_team(raw[[team]]),
    position = toupper(as.character(raw[[position]])),
    total_ppa = as.numeric(raw[[total_ppa]]),
    stringsAsFactors = FALSE
  )
  if (!is.na(season_column)) {
    out <- out[as.integer(raw[[season_column]]) == as.integer(season), , drop = FALSE]
  }
  out
}

summarize_transfer_production <- function(portal_raw, player_ppa_raw, teams, season,
                                          prior_strength) {
  portal <- normalize_transfer_portal(portal_raw, season)
  players <- normalize_player_ppa(player_ppa_raw, as.integer(season) - 1L)
  teams <- canonical_team(teams)
  eligible_positions <- c("QB", "RB", "FB", "WR", "TE")
  portal <- portal[portal$position %in% eligible_positions, , drop = FALSE]

  player_key <- paste(players$player_name, players$team, sep = "\r")
  if (anyDuplicated(player_key)) {
    order_index <- order(abs(players$total_ppa), decreasing = TRUE, na.last = TRUE)
    players <- players[order_index, , drop = FALSE]
    player_key <- paste(players$player_name, players$team, sep = "\r")
    keep <- !duplicated(player_key)
    players <- players[keep, , drop = FALSE]
    player_key <- player_key[keep]
  }
  portal_name <- canonical_player_name(paste(portal$first_name, portal$last_name))
  portal_key <- paste(portal_name, portal$origin, sep = "\r")
  index <- match(portal_key, player_key)
  portal$total_ppa <- players$total_ppa[index]

  prior <- prior_strength[
    as.integer(prior_strength$season) == as.integer(season) - 1L, , drop = FALSE
  ]
  prior_team <- canonical_team(prior$team)
  prior_percentile <- percentile_rank(as.numeric(prior$strength))
  origin_percentile <- prior_percentile[match(portal$origin, prior_team)]
  origin_percentile[!is.finite(origin_percentile)] <- 0.5
  portal$adjusted_ppa <- portal$total_ppa * (0.5 + 0.5 * origin_percentile)

  incoming_ppa <- vapply(teams, function(team) {
    sum(portal$adjusted_ppa[portal$destination == team], na.rm = TRUE)
  }, numeric(1))
  eligible_count <- vapply(teams, function(team) {
    sum(portal$destination == team, na.rm = TRUE)
  }, integer(1))
  matched_count <- vapply(teams, function(team) {
    sum(portal$destination == team & is.finite(portal$total_ppa), na.rm = TRUE)
  }, integer(1))
  data.frame(
    team = teams,
    portal_offense_ppa = incoming_ppa,
    portal_offense_score = normal_score_rank(incoming_ppa),
    portal_offense_eligible_count = eligible_count,
    portal_offense_matched_count = matched_count,
    portal_offense_match_rate = ifelse(eligible_count > 0,
                                       matched_count / eligible_count, 1),
    stringsAsFactors = FALSE
  )
}

normalize_preseason_polls <- function(raw, config) {
  team <- first_existing_column(raw, c("school", "team"), label = "poll team")
  poll <- first_existing_column(raw, c("poll"), label = "poll name")
  points <- first_existing_column(raw, c("points"), label = "poll points")
  out <- data.frame(
    team = canonical_team(raw[[team]]),
    poll = as.character(raw[[poll]]),
    points = as.numeric(raw[[points]]),
    stringsAsFactors = FALSE
  )
  out <- out[out$poll %in% config$preseason$polls, , drop = FALSE]
  missing_polls <- setdiff(config$preseason$polls, unique(out$poll))
  if (length(missing_polls)) {
    stop("Preseason poll source is missing configured polls: ",
         paste(missing_polls, collapse = ", "), call. = FALSE)
  }
  out
}

poll_vote_share <- function(polls, teams) {
  shares <- vapply(unique(polls$poll), function(name) {
    rows <- polls[polls$poll == name, , drop = FALSE]
    top <- max(rows$points, na.rm = TRUE)
    share <- rows$points[match(teams, rows$team)] / top
    ifelse(is.finite(share), share, 0)
  }, numeric(length(teams)))
  rowMeans(matrix(shares, nrow = length(teams)))
}

build_preseason_team_priors <- function(returning, talent, polls, membership,
                                        prior_strength, season, config,
                                        portal = NULL, player_ppa = NULL) {
  season <- as.integer(season)
  assert_columns(membership, c("team", "season", "classification"), "membership")
  assert_columns(prior_strength, c("team", "season", "strength"), "prior strength")
  fbs <- membership[tolower(membership$classification) == "fbs" &
                      as.integer(membership$season) == season, , drop = FALSE]
  fbs_teams <- sort(unique(canonical_team(fbs$team)))
  if (!length(fbs_teams)) {
    stop("No FBS membership rows were available for ", season, ".", call. = FALSE)
  }

  returning <- normalize_returning_production(returning, season)
  prior <- prior_strength[as.integer(prior_strength$season) == season - 1L, , drop = FALSE]
  prior_teams <- canonical_team(prior$team)

  # A team absent from returning production is only legitimate when it was not
  # an FBS member with games the season before: new FBS members appear in the
  # foundation through FCS crossover games, and 2020 opt-outs were members with
  # no games, so both conditions are required to call an absence an error.
  missing <- setdiff(fbs_teams, returning$team)
  prior_fbs <- membership[tolower(membership$classification) == "fbs" &
                            as.integer(membership$season) == season - 1L, , drop = FALSE]
  prior_fbs_teams <- unique(canonical_team(prior_fbs$team))
  unexplained <- missing[missing %in% prior_fbs_teams & missing %in% prior_teams]
  if (length(unexplained)) {
    stop("Returning production for ", season, " is missing FBS teams: ",
         paste(unexplained, collapse = ", "), call. = FALSE)
  }
  if (length(missing)) {
    message("Returning production for ", season, " leaves teams without prior ",
            "FBS participation unknown: ", paste(missing, collapse = ", "))
  }
  index <- match(fbs_teams, returning$team)

  talent <- normalize_team_talent(talent, season)
  talent_value <- talent$talent[match(fbs_teams, talent$team)]

  portal_summary <- summarize_transfer_portal(portal, fbs_teams, season)
  portal_production <- summarize_transfer_production(
    portal, player_ppa, fbs_teams, season, prior_strength
  )

  polls <- normalize_preseason_polls(polls, config)
  vote_share <- poll_vote_share(polls, fbs_teams)

  prior_value <- as.numeric(prior$strength[match(fbs_teams, prior_teams)])
  prior_percentile <- percentile_rank(prior_value)

  returning_ppa <- returning$returning_ppa_pct[index]
  out <- data.frame(
    team = fbs_teams,
    season = season,
    returning_ppa_pct = returning_ppa,
    returning_passing_ppa_pct = returning$returning_passing_ppa_pct[index],
    returning_usage_pct = returning$returning_usage_pct[index],
    talent_percentile = percentile_rank(talent_value),
    preseason_poll_vote_share = vote_share,
    stringsAsFactors = FALSE
  )
  out$retained_quality <- out$returning_ppa_pct * prior_percentile
  out$replacement_capacity <- out$talent_percentile * (1 - out$returning_ppa_pct)
  out <- merge(out, portal_summary, by = "team", all.x = TRUE, sort = FALSE)
  out <- merge(out, portal_production, by = "team", all.x = TRUE, sort = FALSE)
  out <- out[match(fbs_teams, out$team), , drop = FALSE]
  returning_usage <- pmax(0, pmin(1, out$returning_usage_pct))
  out$portal_replacement_capacity <-
    out$portal_incoming_percentile * (1 - returning_usage)
  out$portal_offense_replacement <-
    out$portal_offense_score * (1 - returning_usage)
  out$roster_rebuild_score <- pmax(0, out$portal_offense_replacement)
  out$roster_rebuild_flag <-
    out$roster_rebuild_score > config$preseason$rebuild_score_threshold
  out$hype_gap <- percentile_rank(out$preseason_poll_vote_share) - prior_percentile
  out$source <- paste(
    "cfbd_returning+247_talent+transfer_portal",
    "+prior_player_ppa+week1_polls", sep = ""
  )
  out$captured_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  assert_unique_keys(out, c("team", "season"), "preseason priors")
  out
}

preseason_membership_for_season <- function(membership, talent, season,
                                             minimum_teams = 100L) {
  assert_columns(membership, c("team", "season", "classification"), "membership")
  season <- as.integer(season)
  existing <- membership[
    as.integer(membership$season) == season &
      tolower(membership$classification) == "fbs", , drop = FALSE
  ]
  if (nrow(existing)) return(membership)

  current <- normalize_team_talent(talent, season)
  current <- current[!is.na(current$team) & nzchar(current$team), , drop = FALSE]
  assert_unique_keys(current, "team", "current-season talent")
  if (nrow(current) < as.integer(minimum_teams)) {
    stop("No FBS membership rows exist for ", season,
         " and the current talent source has only ", nrow(current),
         " unique teams.", call. = FALSE)
  }

  message("Using the ", nrow(current), "-team 247 talent universe as ", season,
          " FBS membership because the historical foundation ends before that season.")
  rbind(
    membership[c("team", "season", "classification")],
    data.frame(team = current$team, season = season, classification = "fbs",
               stringsAsFactors = FALSE)
  )
}

merge_preseason_priors <- function(existing, replacement, overwrite = FALSE) {
  if (is.null(existing)) existing <- data.frame()
  assert_columns(replacement, c("team", "season"), "replacement preseason priors")
  assert_unique_keys(replacement, c("team", "season"), "replacement preseason priors")
  seasons <- sort(unique(as.integer(replacement$season)))

  if (nrow(existing)) {
    assert_columns(existing, c("team", "season"), "existing preseason priors")
    assert_unique_keys(existing, c("team", "season"), "existing preseason priors")
    overlap <- intersect(seasons, unique(as.integer(existing$season)))
    if (length(overlap) && !overwrite) {
      stop("preseason_team_priors.csv already contains season(s) ",
           paste(overlap, collapse = ", "),
           ". Rerun with overwrite=TRUE only after reviewing them.", call. = FALSE)
    }
    existing <- existing[!as.integer(existing$season) %in% seasons, , drop = FALSE]
  }

  if (!nrow(existing)) {
    replacement <- replacement[order(as.integer(replacement$season),
                                     replacement$team), , drop = FALSE]
    rownames(replacement) <- NULL
    return(replacement)
  }

  columns <- union(names(existing), names(replacement))
  for (column in setdiff(columns, names(existing))) {
    existing[[column]] <- rep(NA, nrow(existing))
  }
  for (column in setdiff(columns, names(replacement))) {
    replacement[[column]] <- rep(NA, nrow(replacement))
  }
  combined <- rbind(existing[columns], replacement[columns])
  combined <- combined[order(as.integer(combined$season), combined$team), , drop = FALSE]
  rownames(combined) <- NULL
  assert_unique_keys(combined, c("team", "season"), "preseason priors")
  combined
}

build_preseason_priors <- function(config, seasons, refresh = FALSE, overwrite = FALSE) {
  target <- file.path(config$data_dir, "preseason_team_priors.csv")
  existing <- read_csv_if_present(target, required = FALSE)
  membership <- read_csv_if_present(
    file.path(config$data_dir, "historical_fbs_membership.csv"), required = TRUE
  )
  team_games <- read_csv_if_present(
    file.path(config$data_dir, "historical_team_games.csv"), required = TRUE
  )
  assert_columns(team_games, c("team", "season", "net_efficiency"), "historical team games")
  prior_strength <- stats::aggregate(
    net_efficiency ~ team + season, team_games, mean, na.rm = TRUE
  )
  names(prior_strength)[names(prior_strength) == "net_efficiency"] <- "strength"

  rows <- lapply(seasons, function(season) {
    returning <- pull_cfbd_returning_production(season, config, refresh)
    talent <- pull_cfbd_team_talent(season, config, refresh)
    portal <- pull_cfbd_transfer_portal(season, config, refresh)
    player_ppa <- pull_cfbd_player_ppa(season - 1L, config, refresh)
    polls <- pull_cfbd_preseason_polls(season, config, refresh)
    season_membership <- preseason_membership_for_season(
      membership, talent, season
    )
    build_preseason_team_priors(
      returning = returning, talent = talent, polls = polls,
      membership = season_membership, prior_strength = prior_strength,
      season = season, config = config, portal = portal,
      player_ppa = player_ppa
    )
  })
  priors <- merge_preseason_priors(existing, do.call(rbind, rows), overwrite)
  write_foundation_csv(priors, target)
  coverage <- stats::aggregate(
    list(teams = priors$team), list(season = priors$season), length
  )
  list(path = target, priors = priors, coverage = coverage)
}

attach_preseason_features <- function(training, priors, config,
                                      features = preseason_challenger_feature_names(),
                                      prefix = "challenger_ps_",
                                      require_coverage = FALSE) {
  assert_columns(training, c("season", "week", "home", "away"), "training games")
  assert_columns(priors, c("team", "season", features), "preseason priors")
  priors$team <- canonical_team(priors$team)
  assert_unique_keys(priors, c("team", "season"), "preseason priors")

  season <- as.integer(training$season)
  week <- as.integer(training$week)
  postseason <- if ("postseason_type" %in% names(training)) {
    as.character(training$postseason_type)
  } else rep("regular", nrow(training))
  phase_week <- ifelse(!is.na(postseason) & postseason != "regular", 99L, week)
  weight <- preseason_feature_weight(phase_week, config)
  covered <- season %in% unique(as.integer(priors$season))
  if (require_coverage) {
    uncovered <- !covered & weight > 0
    if (any(uncovered)) {
      stop("Preseason priors do not cover season(s) ",
           paste(unique(season[uncovered]), collapse = ", "),
           " required for Weeks 0-4. Freeze them with --mode=build-preseason.",
           call. = FALSE)
    }
  }

  side_level <- function(column) {
    if (column %in% names(training)) tolower(as.character(training[[column]])) else
      rep("fbs", nrow(training))
  }
  key <- paste(priors$team, as.integer(priors$season), sep = "\r")
  side_index <- function(teams) match(paste(canonical_team(teams), season, sep = "\r"), key)
  home_index <- side_index(training$home)
  away_index <- side_index(training$away)
  check_side <- function(teams, level, index) {
    missing <- covered & level == "fbs" & is.na(index) & weight > 0
    if (any(missing)) {
      stop("Missing preseason priors for FBS team-seasons: ",
           paste(unique(paste(teams[missing], season[missing])), collapse = ", "),
           call. = FALSE)
    }
  }
  check_side(training$home, side_level("home_level"), home_index)
  check_side(training$away, side_level("away_level"), away_index)

  rebuild_column <- if ("roster_rebuild_score" %in% names(priors)) {
    "roster_rebuild_score"
  } else if ("portal_offense_replacement" %in% names(priors)) {
    "portal_offense_replacement"
  } else NA_character_
  if (!is.na(rebuild_column)) {
    home_rebuild <- as.numeric(priors[[rebuild_column]][home_index])
    away_rebuild <- as.numeric(priors[[rebuild_column]][away_index])
    rebuild_score <- pmax(home_rebuild, away_rebuild, na.rm = TRUE)
    rebuild_score[!is.finite(rebuild_score) | !covered] <- 0
    training$preseason_roster_rebuild_score <- rebuild_score
  }

  for (feature in features) {
    difference <- as.numeric(priors[[feature]][home_index]) -
      as.numeric(priors[[feature]][away_index])
    difference[!covered] <- 0
    training[[paste0(prefix, feature, "_diff")]] <- difference * weight
  }
  training
}
