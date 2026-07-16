# Recency-weighted, as-of CFB margin model utilities.

suppressPackageStartupMessages({
  library(caret)
  library(dplyr)
  library(purrr)
  library(randomForest)
  library(tidyr)
  library(zoo)
})

PASS_TYPES <- c("Pass", "Pass Incompletion", "Pass Reception", "Passing Touchdown", "Sack")
RUSH_TYPES <- c("Rush", "Rushing Touchdown")
P5_CONFERENCES <- c("ACC", "Big Ten", "Big 12", "Pac-12", "SEC")

CURRENT_FORM_FEATURES <- c(
  "off_epa_last3", "off_epa_last6", "off_pass_epa_last6", "off_rush_epa_last6",
  "off_wpa_last6", "off_success_rate_last6",
  "def_epa_allowed_last3", "def_epa_allowed_last6",
  "def_pass_epa_allowed_last6", "def_rush_epa_allowed_last6",
  "def_success_rate_allowed_last6",
  "win_pct_before", "avg_margin_before", "margin_last3", "margin_last6"
)

PRIOR_FORM_FEATURES <- c(
  "prior_season_off_epa", "prior_season_def_epa_allowed",
  "prior_season_win_pct", "prior_season_margin",
  "prior3_off_epa", "prior3_def_epa_allowed", "prior3_win_pct", "prior3_margin"
)

TEAM_SNAPSHOT_FEATURES <- c(CURRENT_FORM_FEATURES, PRIOR_FORM_FEATURES)
SP_FEATURES <- c("sp_rating_prior", "sp_offense_prior", "sp_defense_prior")
ELO_FEATURES <- c("elo_prior")
COACH_FEATURES <- c(
  "coach_rating_pre", "coach_career_win_pct_before", "coach_srs_mean_before",
  "coach_recent3_srs_before", "coach_recent3_win_pct_before",
  "coach_years_before", "coach_school_tenure_before"
)
INJURY_FEATURES <- c("injured_worth", "injury_worth")
PRESEASON_INPUT_COLUMNS <- c(
  "team", "season", "projected_qb_tier", "returning_production_rank",
  "returning_starter_value", "returning_starter_value_rank",
  "returning_starters_total", "off_returning_snap_pct", "def_returning_snap_pct",
  "ol_returning_snap_pct", "quality_context_score",
  "portal_net_rating", "portal_team_rank", "ol_returning_starts", "def_returning_production",
  "coach_continuity", "oc_continuity", "dc_continuity",
  "preseason_power_adjustment", "input_source", "notes"
)
PRESEASON_FEATURES <- c(
  "projected_qb_tier", "returning_production_score", "returning_starter_value",
  "returning_starters_total", "off_returning_snap_pct", "def_returning_snap_pct",
  "ol_returning_snap_pct", "quality_context_score", "portal_net_rating",
  "ol_returning_starts", "def_returning_production", "coach_continuity",
  "oc_continuity", "dc_continuity", "staff_continuity_score",
  "preseason_power_adjustment"
)
PHASE_FEATURES <- c("phase_preseason", "phase_early_season", "phase_in_season")
MARKET_FEATURES <- c("open_spread_home", "close_spread_home")
PRESEASON_RETURNING_SOURCE_COLUMNS <- c(
  "team", "season", "returning_starters_total", "returning_starters_offense",
  "returning_starters_defense", "returning_snaps_pct", "off_returning_snap_pct",
  "def_returning_snap_pct", "ol_returning_starts", "ol_returning_snap_pct", "source"
)
PRESEASON_PORTAL_SOURCE_COLUMNS <- c(
  "team", "season", "portal_team_rank", "portal_team_score",
  "portal_additions", "portal_departures", "source"
)
PRESEASON_STAFF_SOURCE_COLUMNS <- c(
  "team", "season", "head_coach", "offensive_coordinator", "defensive_coordinator",
  "previous_head_coach", "previous_offensive_coordinator", "previous_defensive_coordinator", "source"
)
PRESEASON_OVERRIDE_COLUMNS <- c(
  "team", "season", "projected_qb_tier", "preseason_power_adjustment",
  "coach_continuity", "oc_continuity", "dc_continuity", "notes"
)

PFF_FEATURE_GROUPS <- list(
  none = character(),
  overall = c("pff_overall_prior"),
  unit = c("pff_offense_prior", "pff_defense_prior"),
  trenches = c("pff_pass_block_prior", "pff_run_block_prior", "pff_pass_rush_prior", "pff_run_defense_prior"),
  pass_rush = c("pff_passing_prior", "pff_receiving_prior", "pff_pass_rush_prior", "pff_coverage_prior"),
  all = c(
    "pff_overall_prior", "pff_offense_prior", "pff_defense_prior",
    "pff_passing_prior", "pff_pass_block_prior", "pff_receiving_prior",
    "pff_running_prior", "pff_run_block_prior", "pff_run_defense_prior",
    "pff_tackling_prior", "pff_pass_rush_prior", "pff_coverage_prior"
  )
)

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

cummean_safe <- function(x) {
  out <- rep(NA_real_, length(x))
  total <- 0
  count <- 0
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      total <- total + x[i]
      count <- count + 1
    }
    out[i] <- if (count == 0) NA_real_ else total / count
  }
  out
}

rolling_mean <- function(x, n) {
  zoo::rollapplyr(x, width = n, FUN = safe_mean, partial = TRUE, fill = NA_real_)
}

last_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[length(x)]
}

normalize_team_name_vec <- function(x) {
  dplyr::case_when(
    x == "UT San Antonio" ~ "UTSA",
    x == "Appalachian State" ~ "App State",
    x == "UMass" ~ "Massachusetts",
    x == "Southern Mississippi" ~ "Southern Miss",
    x == "Connecticut" ~ "UConn",
    x == "Louisiana Monroe" ~ "UL Monroe",
    x == "Louisiana-Monroe" ~ "UL Monroe",
    TRUE ~ as.character(x)
  )
}

as_bool <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

as_numeric_or_binary <- function(x) {
  if (is.logical(x)) return(as.numeric(replace_na(x, FALSE)))
  if (is.numeric(x)) return(as.numeric(x))
  x_chr <- trimws(tolower(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("true", "t", "yes", "y") ~ 1,
    x_chr %in% c("false", "f", "no", "n") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
}

parse_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "-", "--")] <- NA_character_
  suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", x_chr)))
}

parse_pct_fraction <- function(x) {
  value <- parse_number(x)
  ifelse(!is.na(value) & abs(value) > 1, value / 100, value)
}

clamp01 <- function(x) {
  pmin(1, pmax(0, x))
}

safe_percent_rank <- function(x, higher_is_better = TRUE) {
  value <- parse_number(x)
  out <- rep(NA_real_, length(value))
  ok <- !is.na(value)
  if (sum(ok) == 1) {
    out[ok] <- 0.5
  } else if (sum(ok) > 1) {
    ranked <- dplyr::percent_rank(value[ok])
    out[ok] <- if (higher_is_better) ranked else 1 - ranked
  }
  out
}

rank_to_percentile <- function(rank) {
  value <- parse_number(rank)
  out <- rep(NA_real_, length(value))
  ok <- !is.na(value)
  if (sum(ok) == 1) {
    out[ok] <- 0.5
  } else if (sum(ok) > 1) {
    max_rank <- max(value[ok], na.rm = TRUE)
    out[ok] <- if (max_rank <= 1) 0.5 else 1 - ((value[ok] - 1) / (max_rank - 1))
  }
  out
}

weighted_row_score <- function(df, weights) {
  cols <- intersect(names(weights), names(df))
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  value_mat <- as.matrix(df[, cols, drop = FALSE])
  weight_vec <- as.numeric(weights[cols])
  weighted_sum <- rowSums(sweep(value_mat, 2, weight_vec, `*`), na.rm = TRUE)
  weight_sum <- rowSums((!is.na(value_mat)) * matrix(weight_vec, nrow(value_mat), length(weight_vec), byrow = TRUE))
  ifelse(weight_sum == 0, NA_real_, weighted_sum / weight_sum)
}

normalize_staff_name <- function(x) {
  x_chr <- trimws(tolower(as.character(x)))
  x_chr <- gsub("[^a-z ]", "", x_chr)
  x_chr <- gsub("\\s+", " ", x_chr)
  x_chr[x_chr %in% c("", "na", "n/a", "unknown")] <- NA_character_
  x_chr
}

ensure_columns <- function(df, cols, value = NA) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- value
  }
  df
}

add_game_phase <- function(df) {
  df %>%
    mutate(
      game_phase = case_when(
        week <= 1 ~ "preseason",
        week >= 2 & week <= 4 ~ "early_season",
        TRUE ~ "in_season"
      ),
      phase_preseason = as.numeric(game_phase == "preseason"),
      phase_early_season = as.numeric(game_phase == "early_season"),
      phase_in_season = as.numeric(game_phase == "in_season")
    )
}

add_phase_weighted_features <- function(df) {
  df <- add_game_phase(df)
  df$current_form_weight <- case_when(
    df$game_phase == "preseason" ~ 0,
    df$game_phase == "early_season" ~ 0.4,
    TRUE ~ 1
  )
  df$prior_form_weight <- case_when(
    df$game_phase == "preseason" ~ 1.15,
    df$game_phase == "early_season" ~ 0.85,
    TRUE ~ 0.45
  )

  for (feature in CURRENT_FORM_FEATURES) {
    diff_col <- paste0(feature, "_diff")
    weighted_col <- paste0(feature, "_phase_weighted")
    if (diff_col %in% names(df)) {
      df[[weighted_col]] <- coalesce(df[[diff_col]], 0) * df$current_form_weight
    }
  }

  for (feature in PRIOR_FORM_FEATURES) {
    diff_col <- paste0(feature, "_diff")
    weighted_col <- paste0(feature, "_phase_weighted")
    if (diff_col %in% names(df)) {
      df[[weighted_col]] <- coalesce(df[[diff_col]], 0) * df$prior_form_weight
    }
  }

  df
}

prefix_non_keys <- function(df, prefix, keys) {
  key_cols <- intersect(keys, names(df))
  df %>% rename_with(~ paste0(prefix, .x), .cols = -all_of(key_cols))
}

add_home_away_diffs <- function(df, feature_names) {
  for (feature in feature_names) {
    home_col <- paste0("home_", feature)
    away_col <- paste0("away_", feature)
    diff_col <- paste0(feature, "_diff")
    if (home_col %in% names(df) && away_col %in% names(df)) {
      df[[diff_col]] <- df[[home_col]] - df[[away_col]]
    }
  }
  df
}

prepare_pbp <- function(pbp) {
  required <- c(
    "game_id", "pos_team", "def_pos_team", "home", "away", "week", "year",
    "pos_team_score", "def_pos_team_score", "EPA", "wpa", "play_type",
    "season_type", "neutral_site", "home_conference", "away_conference"
  )
  pbp <- ensure_columns(pbp, required)
  pbp %>%
    mutate(
      home = normalize_team_name_vec(home),
      away = normalize_team_name_vec(away),
      pos_team = normalize_team_name_vec(pos_team),
      def_pos_team = normalize_team_name_vec(def_pos_team),
      week = as.integer(week),
      year = as.integer(year),
      neutral_site = as_bool(neutral_site)
    )
}

build_game_results <- function(pbp) {
  pbp <- prepare_pbp(pbp)
  pbp$.row_order <- seq_len(nrow(pbp))

  pbp %>%
    arrange(game_id, .row_order) %>%
    group_by(game_id) %>%
    summarise(
      pos_team_final_score = last_non_na(pos_team_score),
      def_team_final_score = last_non_na(def_pos_team_score),
      pos_team = last_non_na(pos_team),
      def_pos_team = last_non_na(def_pos_team),
      home = last_non_na(home),
      away = last_non_na(away),
      week = as.integer(last_non_na(week)),
      year = as.integer(last_non_na(year)),
      season_type = last_non_na(season_type),
      neutral_site = as_bool(last_non_na(neutral_site)),
      home_conference = last_non_na(home_conference),
      away_conference = last_non_na(away_conference),
      .groups = "drop"
    ) %>%
    mutate(
      home_score = if_else(home == pos_team, pos_team_final_score, def_team_final_score),
      away_score = if_else(away == pos_team, pos_team_final_score, def_team_final_score),
      score_diff = home_score - away_score,
      playoff_game = tolower(season_type) == "postseason" | week >= 15,
      home_field = if_else(neutral_site, 0, 1),
      has_g5_team = !(home_conference %in% P5_CONFERENCES) | !(away_conference %in% P5_CONFERENCES)
    ) %>%
    filter(!is.na(home), !is.na(away), !is.na(year), !is.na(week))
}

build_team_game_results <- function(games) {
  home_rows <- games %>%
    transmute(
      game_id, team = as.character(home), opponent = as.character(away), year, week,
      points_for = home_score, points_against = away_score,
      margin = score_diff, win = as.numeric(score_diff > 0),
      neutral_site
    )

  away_rows <- games %>%
    transmute(
      game_id, team = as.character(away), opponent = as.character(home), year, week,
      points_for = away_score, points_against = home_score,
      margin = -score_diff, win = as.numeric(score_diff < 0),
      neutral_site
    )

  bind_rows(home_rows, away_rows)
}

build_team_week_stats <- function(pbp, games) {
  pbp <- prepare_pbp(pbp)
  team_results <- build_team_game_results(games)

  offense <- pbp %>%
    filter(!is.na(EPA), !is.na(pos_team), !is.na(year), !is.na(week)) %>%
    group_by(team = pos_team, year, week) %>%
    summarise(
      off_epa = safe_mean(EPA),
      off_pass_epa = safe_mean(EPA[play_type %in% PASS_TYPES]),
      off_rush_epa = safe_mean(EPA[play_type %in% RUSH_TYPES]),
      off_wpa = safe_mean(wpa),
      off_success_rate = safe_mean(as.numeric(EPA > 0)),
      .groups = "drop"
    )

  defense <- pbp %>%
    filter(!is.na(EPA), !is.na(def_pos_team), !is.na(year), !is.na(week)) %>%
    group_by(team = def_pos_team, year, week) %>%
    summarise(
      def_epa_allowed = safe_mean(EPA),
      def_pass_epa_allowed = safe_mean(EPA[play_type %in% PASS_TYPES]),
      def_rush_epa_allowed = safe_mean(EPA[play_type %in% RUSH_TYPES]),
      def_success_rate_allowed = safe_mean(as.numeric(EPA > 0)),
      .groups = "drop"
    )

  full_join(offense, defense, by = c("team", "year", "week")) %>%
    left_join(team_results, by = c("team", "year", "week")) %>%
    arrange(team, year, week)
}

build_team_season_history <- function(team_week_stats) {
  team_week_stats %>%
    group_by(team, year) %>%
    summarise(
      season_off_epa = safe_mean(off_epa),
      season_def_epa_allowed = safe_mean(def_epa_allowed),
      season_win_pct = safe_mean(win),
      season_margin = safe_mean(margin),
      .groups = "drop"
    ) %>%
    arrange(team, year) %>%
    group_by(team) %>%
    mutate(
      prior_season_off_epa = lag(season_off_epa),
      prior_season_def_epa_allowed = lag(season_def_epa_allowed),
      prior_season_win_pct = lag(season_win_pct),
      prior_season_margin = lag(season_margin),
      prior3_off_epa = rolling_mean(lag(season_off_epa), 3),
      prior3_def_epa_allowed = rolling_mean(lag(season_def_epa_allowed), 3),
      prior3_win_pct = rolling_mean(lag(season_win_pct), 3),
      prior3_margin = rolling_mean(lag(season_margin), 3)
    ) %>%
    ungroup() %>%
    select(team, year, starts_with("prior_"), starts_with("prior3_"))
}

build_team_snapshots <- function(team_week_stats, max_week = 18) {
  season_history <- build_team_season_history(team_week_stats)

  weekly_snapshots <- team_week_stats %>%
    arrange(team, year, week) %>%
    group_by(team, year) %>%
    mutate(
      game_week = week + 1L,
      games_before = row_number(),
      wins_before = cumsum(replace_na(win, 0)),
      win_pct_before = wins_before / games_before,
      avg_margin_before = cummean_safe(margin),
      margin_last3 = rolling_mean(margin, 3),
      margin_last6 = rolling_mean(margin, 6),
      off_epa_last3 = rolling_mean(off_epa, 3),
      off_epa_last6 = rolling_mean(off_epa, 6),
      off_pass_epa_last6 = rolling_mean(off_pass_epa, 6),
      off_rush_epa_last6 = rolling_mean(off_rush_epa, 6),
      off_wpa_last6 = rolling_mean(off_wpa, 6),
      off_success_rate_last6 = rolling_mean(off_success_rate, 6),
      def_epa_allowed_last3 = rolling_mean(def_epa_allowed, 3),
      def_epa_allowed_last6 = rolling_mean(def_epa_allowed, 6),
      def_pass_epa_allowed_last6 = rolling_mean(def_pass_epa_allowed, 6),
      def_rush_epa_allowed_last6 = rolling_mean(def_rush_epa_allowed, 6),
      def_success_rate_allowed_last6 = rolling_mean(def_success_rate_allowed, 6)
    ) %>%
    ungroup()

  snapshot_cols <- intersect(TEAM_SNAPSHOT_FEATURES, names(weekly_snapshots))
  weekly_snapshots <- weekly_snapshots %>%
    select(team, year, game_week, all_of(snapshot_cols))

  grid <- tidyr::expand_grid(
    team = sort(unique(team_week_stats$team)),
    year = sort(unique(team_week_stats$year)),
    game_week = seq_len(max_week)
  )

  grid %>%
    left_join(weekly_snapshots, by = c("team", "year", "game_week")) %>%
    arrange(team, year, game_week) %>%
    group_by(team, year) %>%
    tidyr::fill(all_of(intersect(TEAM_SNAPSHOT_FEATURES, names(.))), .direction = "down") %>%
    ungroup() %>%
    left_join(season_history, by = c("team", "year"))
}

fetch_sp_ratings <- function(years) {
  if (!requireNamespace("cfbfastR", quietly = TRUE)) {
    stop("Package cfbfastR is required to fetch SP+ ratings.")
  }

  purrr::map_dfr(years, function(y) {
    tryCatch({
      cfbfastR::cfbd_ratings_sp(year = y) %>% mutate(year = y)
    }, error = function(e) {
      message("SP+ fetch failed for ", y, ": ", conditionMessage(e))
      tibble()
    })
  })
}

build_sp_prior_features <- function(sp_ratings) {
  if (is.null(sp_ratings) || nrow(sp_ratings) == 0) {
    return(tibble(team = character(), year = integer()))
  }

  sp_ratings %>%
    select(any_of(c("team", "year", "rating", "offense_rating", "defense_rating"))) %>%
    mutate(team = normalize_team_name_vec(team), year = as.integer(year) + 1L) %>%
    rename(
      sp_rating_prior = rating,
      sp_offense_prior = offense_rating,
      sp_defense_prior = defense_rating
    ) %>%
    distinct(team, year, .keep_all = TRUE)
}

fetch_weekly_elo_ratings <- function(years, weeks = 1:18) {
  if (!requireNamespace("cfbfastR", quietly = TRUE)) {
    stop("Package cfbfastR is required to fetch ELO ratings.")
  }

  purrr::map_dfr(years, function(y) {
    purrr::map_dfr(weeks, function(w) {
      tryCatch({
        cfbfastR::cfbd_ratings_elo(year = y, week = w) %>%
          mutate(year = y, week = w)
      }, error = function(e) {
        message("ELO fetch failed for year ", y, " week ", w, ": ", conditionMessage(e))
        tibble()
      })
    })
  })
}

build_elo_snapshots <- function(elo_ratings, max_week = 18) {
  if (is.null(elo_ratings) || nrow(elo_ratings) == 0) {
    return(tibble(team = character(), year = integer(), game_week = integer()))
  }

  elo_weekly <- elo_ratings %>%
    select(any_of(c("team", "year", "week", "elo"))) %>%
    mutate(
      team = normalize_team_name_vec(team),
      year = as.integer(year),
      game_week = as.integer(week) + 1L,
      elo_prior = as.numeric(elo)
    ) %>%
    select(team, year, game_week, elo_prior) %>%
    distinct(team, year, game_week, .keep_all = TRUE)

  grid <- tidyr::expand_grid(
    team = sort(unique(elo_weekly$team)),
    year = sort(unique(elo_weekly$year)),
    game_week = seq_len(max_week)
  )

  grid %>%
    left_join(elo_weekly, by = c("team", "year", "game_week")) %>%
    arrange(team, year, game_week) %>%
    group_by(team, year) %>%
    tidyr::fill(elo_prior, .direction = "down") %>%
    ungroup()
}

read_coach_data <- function(path = "coach_ratings.csv") {
  if (!file.exists(path)) return(tibble())
  read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
}

build_clean_coach_features <- function(coach_raw) {
  if (is.null(coach_raw) || nrow(coach_raw) == 0) {
    return(tibble(team = character(), year = integer()))
  }

  required <- c("FullName", "school", "year", "games", "wins", "srs")
  missing <- setdiff(required, names(coach_raw))
  if (length(missing) > 0) {
    stop("Coach file is missing required columns: ", paste(missing, collapse = ", "))
  }

  raw <- coach_raw %>%
    mutate(
      FullName = as.character(FullName),
      school = normalize_team_name_vec(school),
      year = as.integer(year),
      games = as.numeric(games),
      wins = as.numeric(wins),
      srs = as.numeric(srs),
      win_per = if_else(!is.na(games) & games > 0, wins / games, NA_real_)
    ) %>%
    filter(!is.na(school), !is.na(year), !is.na(FullName))

  primary <- raw %>%
    group_by(school, year) %>%
    arrange(desc(replace_na(games, -Inf)), desc(replace_na(wins, -Inf)), FullName, .by_group = TRUE) %>%
    mutate(
      coach_count = n(),
      midseason_coach_change = coach_count > 1
    ) %>%
    slice(1) %>%
    ungroup()

  primary %>%
    arrange(FullName, year) %>%
    group_by(FullName) %>%
    mutate(
      coach_career_games_before = lag(cumsum(replace_na(games, 0)), default = 0),
      coach_career_wins_before = lag(cumsum(replace_na(wins, 0)), default = 0),
      coach_career_win_pct_before = if_else(
        coach_career_games_before > 0,
        coach_career_wins_before / coach_career_games_before,
        NA_real_
      ),
      coach_srs_mean_before = lag(cummean_safe(srs)),
      coach_recent3_srs_before = rolling_mean(lag(srs), 3),
      coach_recent3_win_pct_before = rolling_mean(lag(win_per), 3),
      coach_years_before = row_number() - 1L
    ) %>%
    ungroup() %>%
    arrange(FullName, school, year) %>%
    group_by(FullName, school) %>%
    mutate(coach_school_tenure_before = row_number() - 1L) %>%
    ungroup() %>%
    mutate(
      coach_rating_pre =
        coalesce(coach_srs_mean_before, 0) +
        8 * coalesce(coach_recent3_win_pct_before, coach_career_win_pct_before, 0.5) +
        0.35 * coach_years_before +
        0.25 * coach_school_tenure_before +
        if_else(midseason_coach_change, -0.75, 0)
    ) %>%
    transmute(
      team = school,
      year,
      coach_name = FullName,
      coach_rating_pre,
      coach_career_win_pct_before,
      coach_srs_mean_before,
      coach_recent3_srs_before,
      coach_recent3_win_pct_before,
      coach_years_before,
      coach_school_tenure_before,
      midseason_coach_change = as.numeric(midseason_coach_change)
    ) %>%
    distinct(team, year, .keep_all = TRUE)
}

read_pff_data <- function(path = "pff_team_data.csv") {
  if (!file.exists(path)) return(tibble())
  read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
}

build_pff_prior_features <- function(pff_raw) {
  if (is.null(pff_raw) || nrow(pff_raw) == 0) {
    return(tibble(team = character(), year = integer()))
  }

  pff_raw %>%
    mutate(
      team = normalize_team_name_vec(team),
      year = as.integer(year) + 1L
    ) %>%
    transmute(
      team, year,
      pff_overall_prior = as.numeric(Overall),
      pff_offense_prior = as.numeric(Offense),
      pff_passing_prior = as.numeric(Passing),
      pff_pass_block_prior = as.numeric(Pass_Block),
      pff_receiving_prior = as.numeric(Receiving),
      pff_running_prior = as.numeric(Running),
      pff_run_block_prior = as.numeric(Run_Block),
      pff_defense_prior = as.numeric(Defense),
      pff_run_defense_prior = as.numeric(Run_Defense),
      pff_tackling_prior = as.numeric(Tackling),
      pff_pass_rush_prior = as.numeric(Pass_Rush),
      pff_coverage_prior = as.numeric(Coverage)
    ) %>%
    distinct(team, year, .keep_all = TRUE)
}

read_preseason_source_csv <- function(path, columns) {
  empty <- as_tibble(setNames(replicate(length(columns), logical(0), simplify = FALSE), columns))
  if (!file.exists(path)) return(empty)
  read.csv(path, stringsAsFactors = FALSE, check.names = TRUE) %>%
    ensure_columns(columns)
}

standardize_preseason_keys <- function(df, columns) {
  df %>%
    ensure_columns(columns) %>%
    mutate(
      team = normalize_team_name_vec(team),
      season = as.integer(parse_number(season))
    ) %>%
    filter(!is.na(team), team != "", !is.na(season))
}

build_preseason_quality_context <- function(team_context = NULL, season = NULL) {
  if (is.null(team_context) || nrow(team_context) == 0 || !"team" %in% names(team_context)) {
    return(tibble(team = character(), season = integer(), quality_context_score = numeric()))
  }

  ctx <- team_context %>%
    ensure_columns(c(
      "team", "year", "game_week", "prior_season_margin", "prior3_margin",
      "prior_season_win_pct", "prior3_win_pct"
    )) %>%
    mutate(
      team = normalize_team_name_vec(team),
      season = as.integer(year)
    )

  if ("game_week" %in% names(team_context)) {
    ctx <- ctx %>% filter(is.na(game_week) | game_week == 1)
  }
  if (!is.null(season)) {
    ctx <- ctx %>% filter(season == as.integer(season))
  }

  ctx %>%
    group_by(season) %>%
    mutate(
      prior_season_margin_pct = safe_percent_rank(prior_season_margin),
      prior3_margin_pct = safe_percent_rank(prior3_margin),
      prior_season_win_pct_rank = safe_percent_rank(prior_season_win_pct),
      prior3_win_pct_rank = safe_percent_rank(prior3_win_pct)
    ) %>%
    ungroup() %>%
    mutate(
      quality_context_score = rowMeans(
        cbind(
          prior_season_margin_pct, prior3_margin_pct,
          prior_season_win_pct_rank, prior3_win_pct_rank
        ),
        na.rm = TRUE
      ),
      quality_context_score = if_else(is.nan(quality_context_score), NA_real_, quality_context_score)
    ) %>%
    select(team, season, quality_context_score) %>%
    distinct(team, season, .keep_all = TRUE)
}

build_automated_preseason_inputs <- function(
  season,
  returning_path = "preseason_source_returning_starters.csv",
  portal_path = "preseason_source_portal.csv",
  staff_path = "preseason_source_staff.csv",
  overrides_path = "preseason_manual_overrides.csv",
  team_context = NULL,
  output_path = NULL
) {
  season <- as.integer(season)

  returning <- read_preseason_source_csv(returning_path, PRESEASON_RETURNING_SOURCE_COLUMNS) %>%
    standardize_preseason_keys(PRESEASON_RETURNING_SOURCE_COLUMNS) %>%
    filter(season == !!season) %>%
    mutate(
      returning_starters_total = parse_number(returning_starters_total),
      returning_starters_offense = parse_number(returning_starters_offense),
      returning_starters_defense = parse_number(returning_starters_defense),
      returning_snaps_frac = parse_pct_fraction(returning_snaps_pct),
      off_returning_snap_frac = coalesce(parse_pct_fraction(off_returning_snap_pct), returning_snaps_frac, clamp01(returning_starters_offense / 11)),
      def_returning_snap_frac = coalesce(parse_pct_fraction(def_returning_snap_pct), returning_snaps_frac, clamp01(returning_starters_defense / 11)),
      ol_returning_starts = parse_number(ol_returning_starts),
      ol_returning_snap_frac = coalesce(parse_pct_fraction(ol_returning_snap_pct), clamp01(ol_returning_starts / 5)),
      off_returning_snap_pct = 100 * off_returning_snap_frac,
      def_returning_snap_pct = 100 * def_returning_snap_frac,
      ol_returning_snap_pct = 100 * ol_returning_snap_frac,
      source_returning = source
    ) %>%
    select(
      team, season, returning_starters_total, returning_starters_offense,
      returning_starters_defense, off_returning_snap_pct, def_returning_snap_pct,
      ol_returning_starts, ol_returning_snap_pct, source_returning
    ) %>%
    distinct(team, season, .keep_all = TRUE)

  portal <- read_preseason_source_csv(portal_path, PRESEASON_PORTAL_SOURCE_COLUMNS) %>%
    standardize_preseason_keys(PRESEASON_PORTAL_SOURCE_COLUMNS) %>%
    filter(season == !!season) %>%
    mutate(
      portal_team_rank = parse_number(portal_team_rank),
      portal_team_score = parse_number(portal_team_score),
      source_portal = source
    ) %>%
    group_by(season) %>%
    mutate(
      portal_rank_score = 100 * (rank_to_percentile(portal_team_rank) - 0.5),
      portal_net_rating = coalesce(portal_team_score, portal_rank_score)
    ) %>%
    ungroup() %>%
    select(team, season, portal_team_rank, portal_net_rating, source_portal) %>%
    distinct(team, season, .keep_all = TRUE)

  staff <- read_preseason_source_csv(staff_path, PRESEASON_STAFF_SOURCE_COLUMNS) %>%
    standardize_preseason_keys(PRESEASON_STAFF_SOURCE_COLUMNS) %>%
    filter(season == !!season) %>%
    mutate(
      coach_continuity = as.numeric(!is.na(normalize_staff_name(head_coach)) &
        normalize_staff_name(head_coach) == normalize_staff_name(previous_head_coach)),
      oc_continuity = as.numeric(!is.na(normalize_staff_name(offensive_coordinator)) &
        normalize_staff_name(offensive_coordinator) == normalize_staff_name(previous_offensive_coordinator)),
      dc_continuity = as.numeric(!is.na(normalize_staff_name(defensive_coordinator)) &
        normalize_staff_name(defensive_coordinator) == normalize_staff_name(previous_defensive_coordinator)),
      source_staff = source
    ) %>%
    select(team, season, coach_continuity, oc_continuity, dc_continuity, source_staff) %>%
    distinct(team, season, .keep_all = TRUE)

  source_tables <- list(returning, portal, staff)
  source_tables <- source_tables[vapply(source_tables, nrow, integer(1)) > 0]
  auto <- if (length(source_tables) == 0) {
    tibble(team = character(), season = integer())
  } else {
    purrr::reduce(source_tables, full_join, by = c("team", "season"))
  }

  context <- build_preseason_quality_context(team_context, season = season)
  if (nrow(context) > 0) {
    auto <- auto %>% full_join(context, by = c("team", "season"))
  }

  auto <- auto %>%
    ensure_columns(c(
      "returning_starters_total", "off_returning_snap_pct", "def_returning_snap_pct",
      "ol_returning_snap_pct", "returning_starters_defense", "quality_context_score", "portal_net_rating",
      "portal_team_rank", "ol_returning_starts", "coach_continuity",
      "oc_continuity", "dc_continuity", "source_returning", "source_portal", "source_staff"
    ))

  if (nrow(auto) > 0) {
    quality_for_multiplier <- coalesce(auto$quality_context_score, 0.5)
    score_inputs <- tibble(
      returning_starters_score = clamp01(parse_number(auto$returning_starters_total) / 22),
      off_snap_score = clamp01(parse_pct_fraction(auto$off_returning_snap_pct)),
      def_snap_score = clamp01(parse_pct_fraction(auto$def_returning_snap_pct)),
      ol_snap_score = clamp01(coalesce(parse_pct_fraction(auto$ol_returning_snap_pct), parse_number(auto$ol_returning_starts) / 5)),
      quality_score = quality_for_multiplier
    )
    base_returning_score <- 100 * weighted_row_score(
      score_inputs,
      c(
        returning_starters_score = 0.25,
        off_snap_score = 0.25,
        def_snap_score = 0.25,
        ol_snap_score = 0.15,
        quality_score = 0.10
      )
    )
    auto$returning_starter_value <- base_returning_score * (0.65 + 0.70 * quality_for_multiplier)

    auto <- auto %>%
      group_by(season) %>%
      mutate(
        returning_starter_value_rank = if_else(
          is.na(returning_starter_value),
          NA_integer_,
          as.integer(min_rank(desc(returning_starter_value)))
        ),
        returning_production_rank = returning_starter_value_rank,
        def_returning_production = coalesce(def_returning_snap_pct, returning_starters_defense),
        input_source = purrr::pmap_chr(
          list(source_returning, source_portal, source_staff),
          function(source_returning, source_portal, source_staff) {
            values <- unique(na.omit(trimws(c(source_returning, source_portal, source_staff))))
            values <- values[nzchar(values)]
            if (length(values) == 0) NA_character_ else paste(values, collapse = "; ")
          }
        ),
        notes = NA_character_
      ) %>%
      ungroup()
  }

  overrides <- read_preseason_source_csv(overrides_path, PRESEASON_INPUT_COLUMNS) %>%
    standardize_preseason_keys(PRESEASON_INPUT_COLUMNS) %>%
    filter(season == !!season) %>%
    distinct(team, season, .keep_all = TRUE)

  if (nrow(overrides) > 0) {
    numeric_override_cols <- setdiff(PRESEASON_INPUT_COLUMNS, c("team", "season", "input_source", "notes"))
    for (col in numeric_override_cols) {
      if (col %in% names(overrides)) {
        if (col %in% c("coach_continuity", "oc_continuity", "dc_continuity")) {
          overrides[[col]] <- as_numeric_or_binary(overrides[[col]])
        } else {
          overrides[[col]] <- parse_number(overrides[[col]])
        }
      }
    }

    overrides_prefixed <- overrides %>%
      rename_with(~ paste0(.x, "__override"), .cols = -all_of(c("team", "season")))
    auto <- full_join(auto, overrides_prefixed, by = c("team", "season"))
    auto <- ensure_columns(auto, setdiff(PRESEASON_INPUT_COLUMNS, c("team", "season")))

    for (col in setdiff(PRESEASON_INPUT_COLUMNS, c("team", "season", "input_source", "notes"))) {
      override_col <- paste0(col, "__override")
      if (override_col %in% names(auto)) {
        auto[[col]] <- coalesce(auto[[override_col]], auto[[col]])
      }
    }
    if ("input_source__override" %in% names(auto)) {
      auto$input_source <- coalesce(auto$input_source__override, auto$input_source)
    }
    if ("notes__override" %in% names(auto)) {
      override_notes <- trimws(as.character(auto$notes__override))
      auto$notes <- ifelse(!is.na(override_notes) & nzchar(override_notes), override_notes, auto$notes)
    }
  }

  auto <- auto %>%
    ensure_columns(PRESEASON_INPUT_COLUMNS) %>%
    select(all_of(PRESEASON_INPUT_COLUMNS)) %>%
    arrange(season, team)

  if (!is.null(output_path)) {
    write.csv(auto, output_path, row.names = FALSE, na = "")
  }

  auto
}

read_preseason_team_inputs <- function(path = "preseason_team_inputs.csv") {
  if (!file.exists(path)) {
    out <- as_tibble(setNames(replicate(length(PRESEASON_INPUT_COLUMNS), logical(0), simplify = FALSE), PRESEASON_INPUT_COLUMNS))
    return(out)
  }

  preseason <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
  ensure_columns(preseason, PRESEASON_INPUT_COLUMNS)
}

build_preseason_features <- function(preseason_raw) {
  if (is.null(preseason_raw) || nrow(preseason_raw) == 0) {
    return(tibble(team = character(), year = integer()))
  }

  preseason_raw %>%
    ensure_columns(PRESEASON_INPUT_COLUMNS) %>%
    mutate(
      team = normalize_team_name_vec(team),
      year = as.integer(season),
      projected_qb_tier = as_numeric_or_binary(projected_qb_tier),
      returning_production_rank = as.numeric(returning_production_rank),
      returning_starter_value = as.numeric(returning_starter_value),
      returning_starter_value_rank = as.numeric(returning_starter_value_rank),
      returning_production_rank = coalesce(returning_production_rank, returning_starter_value_rank),
      returning_production_score = -returning_production_rank,
      returning_starters_total = as.numeric(returning_starters_total),
      off_returning_snap_pct = as.numeric(off_returning_snap_pct),
      def_returning_snap_pct = as.numeric(def_returning_snap_pct),
      ol_returning_snap_pct = as.numeric(ol_returning_snap_pct),
      quality_context_score = as.numeric(quality_context_score),
      portal_net_rating = as.numeric(portal_net_rating),
      portal_team_rank = as.numeric(portal_team_rank),
      ol_returning_starts = as.numeric(ol_returning_starts),
      def_returning_production = as.numeric(def_returning_production),
      coach_continuity = as_numeric_or_binary(coach_continuity),
      oc_continuity = as_numeric_or_binary(oc_continuity),
      dc_continuity = as_numeric_or_binary(dc_continuity),
      staff_continuity_score = rowMeans(
        cbind(coach_continuity, oc_continuity, dc_continuity),
        na.rm = TRUE
      ),
      preseason_power_adjustment = as.numeric(preseason_power_adjustment)
    ) %>%
    mutate(staff_continuity_score = if_else(is.nan(staff_continuity_score), NA_real_, staff_continuity_score)) %>%
    select(team, year, all_of(PRESEASON_FEATURES), notes) %>%
    distinct(team, year, .keep_all = TRUE)
}

read_injury_data <- function(path = "final_for_sure_1.csv") {
  if (!file.exists(path)) return(tibble())
  read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
}

build_injury_features <- function(injury_raw) {
  if (is.null(injury_raw) || nrow(injury_raw) == 0) {
    return(tibble(team = character(), year = integer()))
  }

  injury_raw %>%
    mutate(
      team = normalize_team_name_vec(team),
      year = as.integer(season),
      injured_worth = as.numeric(injured_worth),
      worth = as.numeric(worth)
    ) %>%
    group_by(team, year) %>%
    summarise(
      injured_worth = sum(injured_worth, na.rm = TRUE),
      injury_worth = sum(worth, na.rm = TRUE),
      .groups = "drop"
    )
}

load_game_lines <- function(path = "game_lines.csv") {
  cols <- c(
    "game_id", "season", "week", "home_team", "away_team",
    "open_spread_home", "close_spread_home", "open_total", "close_total", "source"
  )
  if (!file.exists(path)) {
    out <- as_tibble(setNames(replicate(length(cols), logical(0), simplify = FALSE), cols))
    return(out)
  }

  lines <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE)
  lines <- ensure_columns(lines, cols)
  lines %>%
    mutate(
      game_id = suppressWarnings(as.numeric(game_id)),
      season = as.integer(season),
      week = as.integer(week),
      home_team = normalize_team_name_vec(home_team),
      away_team = normalize_team_name_vec(away_team),
      open_spread_home = as.numeric(open_spread_home),
      close_spread_home = as.numeric(close_spread_home),
      open_total = as.numeric(open_total),
      close_total = as.numeric(close_total),
      source = as.character(source)
    )
}

add_game_lines <- function(model_table, lines) {
  line_cols <- c("open_spread_home", "close_spread_home", "open_total", "close_total", "source")
  if (is.null(lines) || nrow(lines) == 0) {
    for (col in line_cols) model_table[[col]] <- NA
    return(model_table)
  }

  if ("game_id" %in% names(lines) && any(!is.na(lines$game_id))) {
    model_table %>%
      left_join(lines %>% select(game_id, all_of(line_cols)), by = "game_id")
  } else {
    model_table %>%
      left_join(
        lines %>%
          select(
            year = season, week,
            home = home_team, away = away_team,
            all_of(line_cols)
          ),
        by = c("year", "week", "home", "away")
      )
  }
}

build_game_model_table <- function(
  pbp,
  sp_ratings = NULL,
  elo_ratings = NULL,
  coach_raw = NULL,
  pff_raw = NULL,
  preseason_raw = NULL,
  injury_raw = NULL,
  lines = NULL,
  pff_group = "none",
  include_season_injuries = FALSE,
  max_week = 18
) {
  if (!pff_group %in% names(PFF_FEATURE_GROUPS)) {
    stop("Unknown pff_group: ", pff_group)
  }

  pbp <- prepare_pbp(pbp)
  games <- build_game_results(pbp)
  team_week_stats <- build_team_week_stats(pbp, games)
  team_snapshots <- build_team_snapshots(team_week_stats, max_week = max_week)
  sp_features <- build_sp_prior_features(sp_ratings)
  elo_snapshots <- build_elo_snapshots(elo_ratings, max_week = max_week)
  coach_features <- build_clean_coach_features(coach_raw)
  pff_features <- build_pff_prior_features(pff_raw)
  preseason_features <- build_preseason_features(preseason_raw)
  injury_features <- build_injury_features(injury_raw)

  home_snap <- team_snapshots %>%
    rename(home = team, week = game_week) %>%
    prefix_non_keys("home_", c("home", "year", "week"))
  away_snap <- team_snapshots %>%
    rename(away = team, week = game_week) %>%
    prefix_non_keys("away_", c("away", "year", "week"))

  model_table <- games %>%
    left_join(home_snap, by = c("home", "year", "week")) %>%
    left_join(away_snap, by = c("away", "year", "week")) %>%
    add_home_away_diffs(TEAM_SNAPSHOT_FEATURES)

  if (nrow(sp_features) > 0) {
    model_table <- model_table %>%
      left_join(
        sp_features %>% rename(home = team) %>% prefix_non_keys("home_", c("home", "year")),
        by = c("home", "year")
      ) %>%
      left_join(
        sp_features %>% rename(away = team) %>% prefix_non_keys("away_", c("away", "year")),
        by = c("away", "year")
      ) %>%
      add_home_away_diffs(SP_FEATURES)
  }

  if (nrow(elo_snapshots) > 0) {
    model_table <- model_table %>%
      left_join(
        elo_snapshots %>% rename(home = team, week = game_week) %>% prefix_non_keys("home_", c("home", "year", "week")),
        by = c("home", "year", "week")
      ) %>%
      left_join(
        elo_snapshots %>% rename(away = team, week = game_week) %>% prefix_non_keys("away_", c("away", "year", "week")),
        by = c("away", "year", "week")
      ) %>%
      add_home_away_diffs(ELO_FEATURES)
  }

  if (nrow(coach_features) > 0) {
    model_table <- model_table %>%
      left_join(
        coach_features %>% rename(home = team) %>% prefix_non_keys("home_", c("home", "year")),
        by = c("home", "year")
      ) %>%
      left_join(
        coach_features %>% rename(away = team) %>% prefix_non_keys("away_", c("away", "year")),
        by = c("away", "year")
      )
    model_table <- model_table %>% add_home_away_diffs(COACH_FEATURES)
  }

  selected_pff <- PFF_FEATURE_GROUPS[[pff_group]]
  if (length(selected_pff) > 0 && nrow(pff_features) > 0) {
    pff_keep <- c("team", "year", selected_pff)
    pff_selected <- pff_features %>% select(any_of(pff_keep))
    model_table <- model_table %>%
      left_join(
        pff_selected %>% rename(home = team) %>% prefix_non_keys("home_", c("home", "year")),
        by = c("home", "year")
      ) %>%
      left_join(
        pff_selected %>% rename(away = team) %>% prefix_non_keys("away_", c("away", "year")),
        by = c("away", "year")
      ) %>%
      add_home_away_diffs(selected_pff)
  }

  if (nrow(preseason_features) > 0) {
    preseason_selected <- preseason_features %>%
      select(team, year, all_of(PRESEASON_FEATURES))
    model_table <- model_table %>%
      left_join(
        preseason_selected %>% rename(home = team) %>% prefix_non_keys("home_", c("home", "year")),
        by = c("home", "year")
      ) %>%
      left_join(
        preseason_selected %>% rename(away = team) %>% prefix_non_keys("away_", c("away", "year")),
        by = c("away", "year")
      ) %>%
      add_home_away_diffs(PRESEASON_FEATURES)
  }

  if (include_season_injuries && nrow(injury_features) > 0) {
    model_table <- model_table %>%
      left_join(
        injury_features %>% rename(home = team) %>% prefix_non_keys("home_", c("home", "year")),
        by = c("home", "year")
      ) %>%
      left_join(
        injury_features %>% rename(away = team) %>% prefix_non_keys("away_", c("away", "year")),
        by = c("away", "year")
      ) %>%
      add_home_away_diffs(INJURY_FEATURES)
  }

  model_table <- add_game_lines(model_table, lines)
  model_table <- add_phase_weighted_features(model_table)

  if (!"home_midseason_coach_change" %in% names(model_table)) model_table$home_midseason_coach_change <- 0
  if (!"away_midseason_coach_change" %in% names(model_table)) model_table$away_midseason_coach_change <- 0
  model_table$home_midseason_coach_change <- as.numeric(coalesce(model_table$home_midseason_coach_change, 0))
  model_table$away_midseason_coach_change <- as.numeric(coalesce(model_table$away_midseason_coach_change, 0))

  attr(model_table, "model_components") <- list(
    team_snapshots = team_snapshots,
    sp_features = sp_features,
    elo_snapshots = elo_snapshots,
    coach_features = coach_features,
    pff_features = pff_features,
    preseason_features = preseason_features,
    injury_features = injury_features,
    pff_group = pff_group,
    include_season_injuries = include_season_injuries
  )

  validate_no_duplicate_games(model_table)
  model_table
}

validate_no_duplicate_games <- function(model_table) {
  if (!"game_id" %in% names(model_table)) return(invisible(TRUE))
  duplicates <- model_table %>% count(game_id) %>% filter(n > 1)
  if (nrow(duplicates) > 0) {
    stop("Join validation failed: duplicate game_id rows found after feature joins.")
  }
  invisible(TRUE)
}

default_feature_columns <- function(model_table, pff_group = "none", include_season_injuries = FALSE) {
  weighted_team_features <- paste0(TEAM_SNAPSHOT_FEATURES, "_phase_weighted")
  team_feature_candidates <- if (any(weighted_team_features %in% names(model_table))) {
    weighted_team_features
  } else {
    paste0(TEAM_SNAPSHOT_FEATURES, "_diff")
  }

  candidates <- c(
    "home_field",
    PHASE_FEATURES,
    team_feature_candidates,
    paste0(SP_FEATURES, "_diff"),
    paste0(ELO_FEATURES, "_diff"),
    paste0(COACH_FEATURES, "_diff"),
    paste0(PRESEASON_FEATURES, "_diff"),
    "home_midseason_coach_change",
    "away_midseason_coach_change",
    paste0(PFF_FEATURE_GROUPS[[pff_group]], "_diff"),
    if (include_season_injuries) paste0(INJURY_FEATURES, "_diff") else character()
  )

  feature_cols <- intersect(candidates, names(model_table))
  feature_cols[vapply(model_table[feature_cols], function(x) is.numeric(x) && any(!is.na(x)), logical(1))]
}

preseason_baseline_feature_columns <- function(model_table) {
  weighted_prior <- paste0(PRIOR_FORM_FEATURES, "_phase_weighted")
  raw_prior <- paste0(PRIOR_FORM_FEATURES, "_diff")
  prior_candidates <- if (any(weighted_prior %in% names(model_table))) weighted_prior else raw_prior
  candidates <- c("home_field", prior_candidates, paste0(SP_FEATURES, "_diff"), paste0(ELO_FEATURES, "_diff"))
  feature_cols <- intersect(candidates, names(model_table))
  feature_cols[vapply(model_table[feature_cols], function(x) is.numeric(x) && any(!is.na(x)), logical(1))]
}

preseason_feature_columns <- function(model_table, include_market = FALSE, pff_group = "none") {
  candidates <- c(
    "home_field",
    PHASE_FEATURES,
    paste0(PRIOR_FORM_FEATURES, "_phase_weighted"),
    paste0(SP_FEATURES, "_diff"),
    paste0(ELO_FEATURES, "_diff"),
    paste0(COACH_FEATURES, "_diff"),
    paste0(PRESEASON_FEATURES, "_diff"),
    "home_midseason_coach_change",
    "away_midseason_coach_change",
    paste0(PFF_FEATURE_GROUPS[[pff_group]], "_diff"),
    if (include_market) MARKET_FEATURES else character()
  )
  feature_cols <- intersect(candidates, names(model_table))
  feature_cols[vapply(model_table[feature_cols], function(x) is.numeric(x) && any(!is.na(x)), logical(1))]
}

apply_time_decay_rows <- function(df, half_life_years = 3, max_multiplier = 6) {
  max_year <- max(df$year, na.rm = TRUE)
  age <- pmax(0, max_year - df$year)
  weights <- 0.5 ^ (age / half_life_years)
  reps <- pmax(1L, round(1 + (max_multiplier - 1) * weights))
  df[rep(seq_len(nrow(df)), reps), , drop = FALSE]
}

train_margin_model <- function(
  model_table,
  train_years,
  feature_cols = NULL,
  seed = 2026,
  half_life_years = 3,
  max_multiplier = 6
) {
  if (is.null(feature_cols)) {
    feature_cols <- default_feature_columns(model_table)
  }

  train_df <- model_table %>%
    filter(year %in% train_years, !is.na(score_diff)) %>%
    select(year, score_diff, all_of(feature_cols))

  if (nrow(train_df) == 0) stop("No training rows available.")
  feature_cols <- feature_cols[vapply(train_df[feature_cols], function(x) is.numeric(x) && any(!is.na(x)), logical(1))]
  if (length(feature_cols) == 0) stop("No numeric feature columns available.")

  train_df <- apply_time_decay_rows(train_df, half_life_years, max_multiplier)
  x <- train_df %>% select(all_of(feature_cols))
  y <- train_df$score_diff

  mtry_values <- unique(pmin(length(feature_cols), pmax(1, round(sqrt(length(feature_cols)) + c(-2, 0, 2)))))
  tune_grid <- expand.grid(mtry = mtry_values)

  set.seed(seed)
  fit <- caret::train(
    x = x,
    y = y,
    method = "rf",
    trControl = caret::trainControl(method = "cv", number = 5),
    tuneGrid = tune_grid,
    preProcess = "medianImpute",
    na.action = na.pass
  )

  list(
    model = fit,
    feature_cols = feature_cols,
    train_years = train_years,
    half_life_years = half_life_years,
    max_multiplier = max_multiplier
  )
}

predict_model_table <- function(model_bundle, newdata) {
  feature_cols <- model_bundle$feature_cols
  for (col in setdiff(feature_cols, names(newdata))) newdata[[col]] <- NA_real_
  newdata$pred_margin <- as.numeric(predict(model_bundle$model, newdata[, feature_cols, drop = FALSE]))
  newdata$pred_winner <- if_else(newdata$pred_margin >= 0, newdata$home, newdata$away)
  add_prediction_diagnostics(newdata)
}

confidence_tier <- function(pred_margin, spread_edge_home = NA_real_) {
  confidence_basis <- if_else(!is.na(spread_edge_home), abs(spread_edge_home), abs(pred_margin))
  case_when(
    is.na(confidence_basis) ~ "unknown",
    confidence_basis >= 7 ~ "high",
    confidence_basis >= 3.5 ~ "medium",
    confidence_basis >= 1.5 ~ "lean",
    TRUE ~ "pass"
  )
}

add_prediction_diagnostics <- function(pred_df) {
  if (!"close_spread_home" %in% names(pred_df)) pred_df$close_spread_home <- NA_real_
  pred_df %>%
    mutate(
      spread_edge_home = pred_margin + close_spread_home,
      ats_pick = case_when(
        is.na(close_spread_home) ~ NA_character_,
        abs(spread_edge_home) < 1e-9 ~ "Push / no edge",
        spread_edge_home > 0 ~ paste(home, "ATS"),
        TRUE ~ paste(away, "ATS")
      ),
      confidence_tier = confidence_tier(pred_margin, spread_edge_home)
    )
}

evaluate_predictions <- function(pred_df) {
  pred_df <- pred_df %>%
    filter(!is.na(score_diff), !is.na(pred_margin)) %>%
    mutate(
      actual_home_win = score_diff > 0,
      pred_home_win = pred_margin > 0,
      su_correct = actual_home_win == pred_home_win,
      spread_edge_home = pred_margin + close_spread_home,
      actual_cover_margin_home = score_diff + close_spread_home,
      ats_push = abs(actual_cover_margin_home) < 1e-9,
      ats_pick_push = abs(spread_edge_home) < 1e-9,
      actual_home_cover = actual_cover_margin_home > 0,
      pred_home_cover = spread_edge_home > 0,
      ats_correct = actual_home_cover == pred_home_cover
    )

  ats_rows <- pred_df %>% filter(!is.na(close_spread_home), !ats_push, !ats_pick_push)

  tibble(
    n_games = nrow(pred_df),
    su_accuracy = safe_mean(as.numeric(pred_df$su_correct)),
    margin_mae = safe_mean(abs(pred_df$pred_margin - pred_df$score_diff)),
    margin_rmse = sqrt(safe_mean((pred_df$pred_margin - pred_df$score_diff)^2)),
    ats_games = nrow(ats_rows),
    ats_accuracy = if (nrow(ats_rows) == 0) NA_real_ else safe_mean(as.numeric(ats_rows$ats_correct)),
    ats_roi_units_per_bet = if (nrow(ats_rows) == 0) NA_real_ else safe_mean(if_else(ats_rows$ats_correct, 100 / 110, -1))
  )
}

run_rolling_backtest <- function(
  model_table,
  first_eval_year = 2018,
  last_eval_year = max(model_table$year, na.rm = TRUE),
  min_train_year = min(model_table$year, na.rm = TRUE),
  feature_cols = NULL
) {
  metric_rows <- list()
  prediction_rows <- list()

  for (eval_year in seq(first_eval_year, last_eval_year)) {
    train_years <- seq(min_train_year, eval_year - 1L)
    if (length(train_years) == 0) next

    bundle <- train_margin_model(model_table, train_years = train_years, feature_cols = feature_cols)
    preds <- model_table %>%
      filter(year == eval_year) %>%
      predict_model_table(bundle, .)

    metric_rows[[as.character(eval_year)]] <- evaluate_predictions(preds) %>%
      mutate(eval_year = eval_year, .before = 1)
    prediction_rows[[as.character(eval_year)]] <- preds
  }

  list(
    metrics = bind_rows(metric_rows),
    predictions = bind_rows(prediction_rows)
  )
}

run_week01_backtest <- function(
  model_table,
  first_eval_year = 2018,
  last_eval_year = max(model_table$year, na.rm = TRUE),
  min_train_year = min(model_table$year, na.rm = TRUE),
  feature_cols = NULL
) {
  metric_rows <- list()
  prediction_rows <- list()

  for (eval_year in seq(first_eval_year, last_eval_year)) {
    train_years <- seq(min_train_year, eval_year - 1L)
    if (length(train_years) == 0) next

    bundle <- train_margin_model(model_table, train_years = train_years, feature_cols = feature_cols)
    preds <- model_table %>%
      filter(year == eval_year, week <= 1) %>%
      predict_model_table(bundle, .)

    metric_rows[[as.character(eval_year)]] <- evaluate_predictions(preds) %>%
      mutate(eval_year = eval_year, .before = 1)
    prediction_rows[[as.character(eval_year)]] <- preds
  }

  list(
    metrics = bind_rows(metric_rows),
    predictions = bind_rows(prediction_rows)
  )
}

compare_week01_models <- function(
  model_table,
  first_eval_year = 2018,
  last_eval_year = max(model_table$year, na.rm = TRUE),
  pff_group = "none"
) {
  feature_sets <- list(
    prior_year_baseline = preseason_baseline_feature_columns(model_table),
    preseason_no_market = preseason_feature_columns(model_table, include_market = FALSE, pff_group = pff_group),
    preseason_with_market = preseason_feature_columns(model_table, include_market = TRUE, pff_group = pff_group)
  )

  purrr::imap_dfr(feature_sets, function(features, model_name) {
    if (length(features) == 0) {
      return(tibble(model_name = model_name, eval_year = integer(), n_games = integer()))
    }
    bt <- run_week01_backtest(
      model_table,
      first_eval_year = first_eval_year,
      last_eval_year = last_eval_year,
      feature_cols = features
    )
    bt$metrics %>%
      mutate(model_name = model_name, feature_count = length(features), .before = 1)
  })
}

diagnostic_slices <- function(pred_df) {
  groups <- list(
    all_games = pred_df,
    playoff_games = pred_df %>% filter(playoff_game),
    historic_decline_watch = pred_df %>% filter(home %in% c("Alabama", "Clemson") | away %in% c("Alabama", "Clemson")),
    rapid_riser_watch = pred_df %>% filter(home %in% c("Indiana", "SMU") | away %in% c("Indiana", "SMU")),
    g5_games = pred_df %>% filter(has_g5_team),
    neutral_site = pred_df %>% filter(neutral_site)
  )

  purrr::imap_dfr(groups, function(df, name) {
    if (nrow(df) == 0) return(tibble(slice = name, n_games = 0))
    evaluate_predictions(df) %>% mutate(slice = name, .before = 1)
  })
}

week01_diagnostic_slices <- function(pred_df) {
  pred_df <- pred_df %>% filter(week <= 1)
  high_transfer <- if ("portal_net_rating_diff" %in% names(pred_df)) {
    pred_df %>% filter(abs(coalesce(portal_net_rating_diff, 0)) >= 5)
  } else {
    pred_df[0, , drop = FALSE]
  }
  qb_mismatch <- if ("projected_qb_tier_diff" %in% names(pred_df)) {
    pred_df %>% filter(abs(coalesce(projected_qb_tier_diff, 0)) >= 1)
  } else {
    pred_df[0, , drop = FALSE]
  }
  new_coach <- if (all(c("home_coach_school_tenure_before", "away_coach_school_tenure_before") %in% names(pred_df))) {
    pred_df %>% filter(coalesce(home_coach_school_tenure_before, 1) == 0 | coalesce(away_coach_school_tenure_before, 1) == 0)
  } else {
    pred_df[0, , drop = FALSE]
  }
  groups <- list(
    week01_all = pred_df,
    new_head_coach = new_coach,
    high_transfer_churn = high_transfer,
    qb_tier_mismatch = qb_mismatch,
    g5_games = pred_df %>% filter(has_g5_team),
    neutral_site = pred_df %>% filter(neutral_site)
  )

  purrr::imap_dfr(groups, function(df, name) {
    if (nrow(df) == 0) return(tibble(slice = name, n_games = 0))
    evaluate_predictions(df) %>% mutate(slice = name, .before = 1)
  })
}

lookup_asof <- function(df, team, year, week = NULL, week_col = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  out <- df %>%
    filter(.data$team == team, .data$year == year)
  if (!is.null(week_col) && week_col %in% names(out)) {
    out <- out %>% filter(.data[[week_col]] <= week) %>% arrange(desc(.data[[week_col]]))
  }
  if (nrow(out) == 0) return(NULL)
  out[1, , drop = FALSE]
}

append_diff_from_lookup <- function(row, source_df, feature_names, home_team, away_team, season, week, week_col = NULL) {
  home_row <- lookup_asof(source_df, home_team, season, week, week_col)
  away_row <- lookup_asof(source_df, away_team, season, week, week_col)
  for (feature in feature_names) {
    diff_col <- paste0(feature, "_diff")
    home_val <- if (!is.null(home_row) && feature %in% names(home_row)) home_row[[feature]][1] else NA_real_
    away_val <- if (!is.null(away_row) && feature %in% names(away_row)) away_row[[feature]][1] else NA_real_
    row[[diff_col]] <- home_val - away_val
  }
  row
}

feature_importance_vector <- function(model_bundle) {
  imp <- tryCatch(caret::varImp(model_bundle$model)$importance, error = function(e) NULL)
  if (is.null(imp) || nrow(imp) == 0) return(setNames(numeric(0), character(0)))
  if ("Overall" %in% names(imp)) {
    out <- imp$Overall
  } else {
    out <- rowMeans(imp, na.rm = TRUE)
  }
  names(out) <- rownames(imp)
  out
}

top_feature_drivers <- function(pred_row, model_bundle, n = 5) {
  importance <- feature_importance_vector(model_bundle)
  feature_cols <- intersect(model_bundle$feature_cols, names(pred_row))
  if (length(feature_cols) == 0 || length(importance) == 0) return(NA_character_)

  values <- unlist(pred_row[feature_cols], use.names = TRUE)
  values <- suppressWarnings(as.numeric(values))
  names(values) <- feature_cols
  common <- intersect(names(values), names(importance))
  if (length(common) == 0) return(NA_character_)

  driver_score <- abs(coalesce(values[common], 0)) * coalesce(importance[common], 0)
  driver_score <- sort(driver_score, decreasing = TRUE)
  driver_score <- driver_score[driver_score > 0]
  if (length(driver_score) == 0) return(NA_character_)

  labels <- vapply(names(driver_score)[seq_len(min(n, length(driver_score)))], function(feature) {
    value <- values[[feature]]
    side <- if (is.na(value) || abs(value) < 1e-9) "neutral" else if (value > 0) "home" else "away"
    paste0(feature, " (", side, ")")
  }, character(1))
  paste(labels, collapse = "; ")
}

predict_matchup <- function(
  home_team,
  away_team,
  season,
  week,
  model_bundle,
  model_table,
  neutral_site = FALSE,
  open_spread_home = NA_real_,
  spread_home = NA_real_
) {
  components <- attr(model_table, "model_components")
  if (is.null(components)) stop("model_table is missing model_components attributes.")

  home_team <- normalize_team_name_vec(home_team)
  away_team <- normalize_team_name_vec(away_team)
  row <- tibble(
    home = home_team,
    away = away_team,
    year = as.integer(season),
    week = as.integer(week),
    neutral_site = as_bool(neutral_site),
    home_field = if_else(as_bool(neutral_site), 0, 1),
    open_spread_home = as.numeric(open_spread_home),
    close_spread_home = as.numeric(spread_home)
  )

  row <- append_diff_from_lookup(row, components$team_snapshots, TEAM_SNAPSHOT_FEATURES, home_team, away_team, season, week, "game_week")
  row <- append_diff_from_lookup(row, components$sp_features, SP_FEATURES, home_team, away_team, season, week)
  row <- append_diff_from_lookup(row, components$elo_snapshots, ELO_FEATURES, home_team, away_team, season, week, "game_week")
  row <- append_diff_from_lookup(row, components$coach_features, COACH_FEATURES, home_team, away_team, season, week)
  row <- append_diff_from_lookup(row, components$preseason_features, PRESEASON_FEATURES, home_team, away_team, season, week)

  selected_pff <- PFF_FEATURE_GROUPS[[components$pff_group]]
  if (length(selected_pff) > 0) {
    row <- append_diff_from_lookup(row, components$pff_features, selected_pff, home_team, away_team, season, week)
  }
  if (isTRUE(components$include_season_injuries)) {
    row <- append_diff_from_lookup(row, components$injury_features, INJURY_FEATURES, home_team, away_team, season, week)
  }

  row <- add_phase_weighted_features(row)
  for (col in setdiff(model_bundle$feature_cols, names(row))) row[[col]] <- NA_real_
  row$pred_margin <- as.numeric(predict(model_bundle$model, row[, model_bundle$feature_cols, drop = FALSE]))
  row$pred_winner <- if_else(row$pred_margin >= 0, home_team, away_team)
  row <- add_prediction_diagnostics(row)
  row$top_feature_drivers <- top_feature_drivers(row, model_bundle)
  row
}

predict_week <- function(schedule, model_bundle, model_table, top_n_drivers = 5) {
  required <- c("season", "week", "home", "away")
  missing <- setdiff(required, names(schedule))
  if (length(missing) > 0) {
    stop("schedule is missing required columns: ", paste(missing, collapse = ", "))
  }

  schedule <- ensure_columns(schedule, c("neutral_site", "open_spread_home", "close_spread_home", "spread_home"))

  preds <- purrr::map_dfr(seq_len(nrow(schedule)), function(i) {
    row <- schedule[i, , drop = FALSE]
    close_spread <- if (!is.na(row$close_spread_home)) row$close_spread_home else row$spread_home
    pred <- predict_matchup(
      home_team = row$home,
      away_team = row$away,
      season = row$season,
      week = row$week,
      model_bundle = model_bundle,
      model_table = model_table,
      neutral_site = row$neutral_site,
      open_spread_home = row$open_spread_home,
      spread_home = close_spread
    )
    pred$top_feature_drivers <- top_feature_drivers(pred, model_bundle, n = top_n_drivers)
    pred
  })

  preds %>%
    select(
      year, week, game_phase, home, away, neutral_site,
      pred_margin, pred_winner, open_spread_home, close_spread_home,
      spread_edge_home, ats_pick, confidence_tier, top_feature_drivers,
      any_of(c("home_field", PHASE_FEATURES))
    )
}

evaluate_pff_groups <- function(
  pbp,
  sp_ratings,
  elo_ratings,
  coach_raw,
  pff_raw,
  preseason_raw = NULL,
  injury_raw = NULL,
  lines = NULL,
  groups = c("none", "overall", "unit", "trenches", "pass_rush"),
  first_eval_year = 2018
) {
  purrr::map_dfr(groups, function(group) {
    table <- build_game_model_table(
      pbp = pbp,
      sp_ratings = sp_ratings,
      elo_ratings = elo_ratings,
      coach_raw = coach_raw,
      pff_raw = pff_raw,
      preseason_raw = preseason_raw,
      injury_raw = injury_raw,
      lines = lines,
      pff_group = group,
      include_season_injuries = FALSE
    )
    features <- default_feature_columns(table, pff_group = group, include_season_injuries = FALSE)
    bt <- run_rolling_backtest(table, first_eval_year = first_eval_year, feature_cols = features)
    bt$metrics %>%
      summarise(
        pff_group = group,
        avg_su_accuracy = safe_mean(su_accuracy),
        avg_margin_mae = safe_mean(margin_mae),
        avg_ats_accuracy = safe_mean(ats_accuracy),
        avg_ats_roi = safe_mean(ats_roi_units_per_bet),
        .groups = "drop"
      )
  })
}
