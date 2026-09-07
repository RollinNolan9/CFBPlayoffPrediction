cfb_v2_config <- function(project_dir = getwd(), season = as.integer(format(Sys.Date(), "%Y"))) {
  root <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)

  list(
    version = "2.1.0",
    project_dir = root,
    data_dir = file.path(root, "cfb_v2", "data"),
    inbox_dir = file.path(root, "cfb_v2", "inbox"),
    output_dir = file.path(root, "cfb_v2", "output"),
    database = file.path(root, "cfb_v2", "data", "cfb_v2.duckdb"),
    season = as.integer(season),
    article_timezone = "America/New_York",
    article_freeze_weekday = "Friday",
    article_freeze_hour = 13L,
    seed = 20260712L,
    training = list(
      first_full_season = 2021L,
      covid_season = 2020L,
      covid_weight = 0.50,
      season_decay = 0.82,
      fcs_rating_weight = 0.25,
      non_cfp_bowl_weight = 0,
      conference_championship_weight = 1,
      cfp_weight = 1
    ),
    phase = list(
      preseason_by_week = c(`0` = 1.00, `1` = 1.00, `2` = 0.60,
                            `3` = 0.30, `4` = 0.10),
      prior_season_cap_after_week = 8L,
      prior_season_max_weight = 0.10
    ),
    coach = list(
      recent_share_candidates = c(0.65, 0.70),
      title_experience_cap = 0.10,
      portability_cap = 0.70,
      context_power_scale = 10,
      current_season_prior_games = 10,
      lower_level_strength = c(fbs = 1.00, fcs = 0.72, d2 = 0.48, d3 = 0.35,
                               naia = 0.35, nfl = 0.90, unknown = 0.50)
    ),
    preseason = list(
      polls = c("AP Top 25", "Coaches Poll"),
      prior_seasons = 2021:2025,
      challenger_share = 1.00,
      rebuild_score_threshold = 0.50,
      production_features = c(
        "returning_ppa_pct", "returning_passing_ppa_pct", "returning_usage_pct",
        "retained_quality", "talent_percentile", "replacement_capacity",
        "portal_replacement_capacity", "portal_offense_replacement"
      ),
      fallback_features = c(
        "returning_ppa_pct", "returning_passing_ppa_pct", "returning_usage_pct",
        "retained_quality"
      ),
      diagnostic_features = c("preseason_poll_vote_share", "hype_gap")
    ),
    bridge = list(
      minimum_movers = 2L,
      sd_floor = 8,
      dominance_share = 0.5
    ),
    ats = list(
      large_spread_review = 21,
      break_even_accuracy = 110 / 210,
      minimum_validation_picks = 100L,
      threshold_grid = data.frame(
        min_edge = c(2, 3, 4),
        min_cover_probability = c(0.54, 0.55, 0.56)
      )
    ),
    eligibility = list(
      independent_always = c("Notre Dame", "Connecticut", "UConn"),
      cfp_probability_min = 0.02
    ),
    injuries = list(
      skill_opportunity_min = 0.25,
      defender_snap_share_min = 0.50,
      max_scenarios = 4L,
      trusted_source_types = c(
        "conference_official", "school_official", "game_notes",
        "depth_chart", "verified_beat_reporter", "manual_exception"
      )
    ),
    model = list(
      ridge_lambda_grid = c(0.5, 2, 8, 32),
      residual_trees = 500L,
      minimum_training_rows = 150L,
      uncertainty_floor = 7
    )
  )
}

ensure_v2_directories <- function(config) {
  paths <- c(config$data_dir, config$inbox_dir, config$output_dir)
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
  invisible(config)
}
