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

pull_cfbd_preseason_polls <- function(season, config, refresh = FALSE) {
  pull_preseason_source(config, "polls", season, refresh, function(year) {
    cfbfastR::cfbd_rankings(year = year, week = 1, season_type = "regular")
  })
}

as_share <- function(x) {
  x <- as.numeric(x)
  finite <- x[is.finite(x)]
  if (length(finite) && max(finite) > 1.5) x <- x / 100
  x
}

percentile_rank <- function(x) {
  x <- as.numeric(x)
  n <- sum(is.finite(x))
  if (!n) return(rep(NA_real_, length(x)))
  rank(x, ties.method = "average", na.last = "keep") / n
}

preseason_challenger_feature_names <- function() {
  c("returning_ppa_pct", "returning_passing_ppa_pct", "returning_usage_pct",
    "talent_percentile", "preseason_poll_vote_share", "retained_quality",
    "replacement_capacity", "hype_gap")
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
                                        prior_strength, season, config) {
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
  out$hype_gap <- percentile_rank(out$preseason_poll_vote_share) - prior_percentile
  out$source <- "cfbd_returning+247_talent+week1_polls"
  out$captured_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  assert_unique_keys(out, c("team", "season"), "preseason priors")
  out
}

build_preseason_priors <- function(config, seasons, refresh = FALSE, overwrite = FALSE) {
  target <- file.path(config$data_dir, "preseason_team_priors.csv")
  if (file.exists(target) && !overwrite) {
    existing <- tryCatch(utils::read.csv(target, nrows = 1),
                         error = function(e) data.frame())
    if (nrow(existing)) {
      stop("preseason_team_priors.csv already contains rows. ",
           "Rerun with overwrite=TRUE only after reviewing it.", call. = FALSE)
    }
  }
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
    build_preseason_team_priors(
      returning = pull_cfbd_returning_production(season, config, refresh),
      talent = pull_cfbd_team_talent(season, config, refresh),
      polls = pull_cfbd_preseason_polls(season, config, refresh),
      membership = membership, prior_strength = prior_strength,
      season = season, config = config
    )
  })
  priors <- do.call(rbind, rows)
  write_foundation_csv(priors, target)
  coverage <- stats::aggregate(
    list(teams = priors$team), list(season = priors$season), length
  )
  list(path = target, priors = priors, coverage = coverage)
}

attach_preseason_challenger_features <- function(training, priors, config) {
  assert_columns(training, c("season", "week", "home", "away"), "training games")
  features <- preseason_challenger_feature_names()
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

  for (feature in features) {
    difference <- as.numeric(priors[[feature]][home_index]) -
      as.numeric(priors[[feature]][away_index])
    difference[!covered] <- 0
    training[[paste0("challenger_ps_", feature, "_diff")]] <- difference * weight
  }
  training
}

preseason_challenger_variants <- function() {
  returning <- c("returning_ppa_pct", "returning_passing_ppa_pct",
                 "returning_usage_pct", "retained_quality")
  talent <- c(returning, "talent_percentile", "replacement_capacity")
  hype <- c(talent, "preseason_poll_vote_share", "hype_gap")
  list(
    preseason_returning_challenger = returning,
    preseason_talent_challenger = talent,
    preseason_hype_challenger = hype
  )
}
