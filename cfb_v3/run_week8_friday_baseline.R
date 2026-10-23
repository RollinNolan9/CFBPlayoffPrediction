# Isolated Friday Week 8 full upcoming FBS-involving slate; fresh schedule, coaches
# and quotes, frozen fit. Completed-game PBP is the accepted Oct 18 Week 8 rankings
# source snapshot (all Week 1-7 games final; documented exceptions retained).
# Standalone task copy of run_week7_friday_baseline.R; no shared pipeline or prior snapshot changes.
# --phase=fetch [--output=NEW_DIRECTORY], then --phase=build --output=DIRECTORY --injury-review=PATH.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) > 1L) stop("Duplicate option: ", name)
  if (length(hit)) sub("^[^=]+=", "", hit) else default
}
phase <- opt("phase", "fetch")
stopifnot(phase %in% c("fetch", "audit", "build", "render"))
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
modules <- c("config", "store", "features", "coaches", "adapters", "models",
  "coach_migration", "historical_data", "preseason", "bridge", "workflow", "live_data", "dashboard")
for (module in modules) source(file.path(root, "cfb_v2", paste0(module, ".R")))
source(file.path(root, "cfb_v3/experiments/opponent_relative_features.R"))
source(file.path(root, "cfb_v3/injury_overlay.R"))
utc <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
say <- function(...) { cat(..., "\n"); flush.console() }
out <- opt("output", file.path(root, "cfb_v3/output/2026/week_8_friday_baseline",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")))
if (phase == "fetch" && dir.exists(out)) stop("Refusing to overwrite a prior snapshot.")
if (phase != "fetch" && !dir.exists(out)) stop("Fetch the isolated inputs first.")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, winslash = "/", mustWork = TRUE)
csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
json <- function(x, name) jsonlite::write_json(x, file.path(out, name), pretty = TRUE,
  auto_unbox = TRUE, digits = 14, null = "null")
say("OUTPUT:", out)
config_path <- file.path(root, "cfb_v3/output/2026/week_5_relative_preliminary/20260930T211701Z/model.rds")
previous <- readRDS(config_path)
config <- previous$config
config$project_dir <- out
config$data_dir <- file.path(root, "cfb_v3/data")
config$inbox_dir <- file.path(root, "cfb_v3/inbox")
config$output_dir <- out
config$database <- file.path(out, "unused_weekly_database.duckdb")
config$omit_thursday <- FALSE
season <- 2026L; week <- 8L
# Saved bundles record the original workstation prefix; resolve to the copied files.
original_prefix <- "C:/Users/n.santacroce/Documents/CFB Predictions"
remap <- function(path) sub(original_prefix, dirname(root), path, fixed = TRUE)
rankings_dir <- file.path(root, "cfb_v3/output/2026/week_8_rankings_relative/20261018T181642Z")
ap_path <- opt("ap-poll", file.path(rankings_dir, "ap_poll.json"))
ap <- jsonlite::read_json(ap_path, simplifyVector = TRUE)
ap$teams$team <- canonical_team(ap$teams$team)
stopifnot(ap$season == season, ap$week == week, nrow(ap$teams) == 25L,
  !anyDuplicated(ap$teams$team), setequal(ap$teams$rank, 1:25))
ranked_slate <- function(universe, as_of) {
  keep <- universe$season == season & universe$week == week &
    (universe$home_level == "fbs" | universe$away_level == "fbs") &
    !is.na(universe$kickoff) & universe$kickoff > as_of & !universe$completed
  selected <- universe[which(keep), ]
  selected$home_ap_rank <- ap$teams$rank[match(selected$home, ap$teams$team)]
  selected$away_ap_rank <- ap$teams$rank[match(selected$away, ap$teams$team)]
  selected[order(selected$kickoff, selected$game_id), ]
}
fit_path <- file.path(root, "cfb_v3/output/2026/week_7_rankings_relative/20261011T145259Z/ranking_fit.rds")
baseline_bundle_path <- remap(sub("\\.csv$", "_model.rds", previous$baseline_csv))
stopifnot(file.exists(baseline_bundle_path))
accepted_pbp_path <- file.path(rankings_dir, "cfb_v2/cache/pbp/pbp_2026_compact.rds")
cache <- file.path(out, "cfb_v2/cache")
paths <- c(schedule = file.path(cache, "schedules/cfbd_schedule_2026.rds"),
  membership = file.path(cache, "teams/cfbd_fbs_teams_2026.rds"),
  pbp = file.path(cache, "pbp/pbp_2026_compact.rds"),
  coaches = file.path(cache, "coaches/current_fbs_coaches_2026.rds"),
  lines = file.path(cache, "lines/cfbd_week_8_2026.rds"))

if (phase == "fetch") {
  readRenviron(file.path(root, ".Renviron"))
  stopifnot(nzchar(Sys.getenv("CFBD_API_KEY")), requireNamespace("cfbfastR", quietly = TRUE))
  protected <- unique(c(config_path, fit_path, baseline_bundle_path, ap_path,
    list.files(file.path(root, "cfb_v2/cache"), recursive = TRUE, full.names = TRUE),
    list.files(config$data_dir, full.names = TRUE), list.files(config$inbox_dir, full.names = TRUE),
    list.files(file.path(root, "cfb_v3/output/2026/week_7_rankings_relative/20261011T145259Z"), full.names = TRUE),
    list.files(rankings_dir, full.names = TRUE), accepted_pbp_path))
  protected <- protected[file.exists(protected) & !dir.exists(protected)]
  hashes <- unname(tools::md5sum(protected))
  csv(data.frame(path = protected, md5 = hashes), "protected_input_hashes.csv")
  for (directory in unique(dirname(paths))) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  say("Fetching CFBD schedule and membership.")
  schedule <- as.data.frame(cfbfastR::cfbd_game_info(season, season_type = "regular"))
  stopifnot(nrow(schedule) > 0L, !anyDuplicated(schedule$game_id))
  saveRDS(schedule, paths[["schedule"]], compress = "xz")
  membership <- load_cfbd_fbs_teams_season(season, config, refresh = TRUE)
  schedule_at <- Sys.time()
  say("Reusing accepted Oct 18 Week 8 rankings PBP (all prior games final).")
  prior_acceptance <- jsonlite::read_json(file.path(rankings_dir, "source_acceptance.json"), simplifyVector = TRUE)
  stopifnot(prior_acceptance$status == "accepted",
    prior_acceptance$accepted_pbp_md5 == unname(tools::md5sum(accepted_pbp_path)))
  stopifnot(file.copy(accepted_pbp_path, paths[["pbp"]]))
  pbp <- readRDS(paths[["pbp"]])
  raw_sha <- prior_acceptance$official_raw_sha256
  raw_path <- file.path(rankings_dir, "official_2026_raw.rds")
  pbp_at <- parse_utc_datetime(prior_acceptance$checked_at)
  acceptance <- prior_acceptance
  acceptance$checked_at <- utc(Sys.time())
  acceptance$accepted_pbp_md5 <- unname(tools::md5sum(paths[["pbp"]]))
  acceptance$carried_forward_from <- file.path(rankings_dir, "source_acceptance.json")
  acceptance$carry_forward_note <- paste("Week 8 card uses only completed Week 1-7 games (strictly earlier model weeks).",
    "The Oct 18 accepted selection already covers all of them with zero missing required games.")
  json(acceptance, "source_acceptance.json")
  json(prior_acceptance, "prior_source_acceptance.json")
  say("Fetching current coaching assignments and Week 8 market quotes.")
  coaches <- pull_current_fbs_coaches(season, paths[["coaches"]], refresh = TRUE)
  raw_lines <- cfbfastR::cfbd_betting_lines(year = season, week = week)
  captured_at <- Sys.time()
  saveRDS(raw_lines, file.path(out, "cfbd_lines_raw.rds"))
  lines <- normalize_betting_lines(raw_lines, captured_at)
  saveRDS(lines, paths[["lines"]])
  csv(lines, "all_market_quotes.csv")
  json(ap, "ap_poll.json")
  csv(coaches, "current_coaches.csv")
  as_of <- Sys.time()
  universe <- derive_games_from_pbp(pbp, schedule = schedule, fbs_membership = membership)
  model_week <- unique(universe$model_week[universe$week == week & universe$postseason_type == "regular"])
  stopifnot(length(model_week) == 1L, is.finite(model_week))
  earlier <- universe[universe$model_week < model_week & universe$kickoff < as_of, ]
  completed <- filter_completed_historical_games(earlier)
  counts <- function(ids) as.integer(table(factor(ids, levels = completed$game_id)))
  coverage <- completed[c("game_id", "season", "week", "model_week", "kickoff", "home", "away",
    "home_score", "away_score", "home_level", "away_level", "completed", "pbp_available")]
  coverage$required_fbs_history <- raw_history_eligible(completed)
  coverage$pbp_rows <- counts(pbp$game_id)
  coverage$finite_epa_rows <- counts(pbp$game_id[is.finite(pbp$EPA)])
  plays <- historical_competitive_plays(pbp[pbp$game_id %in% completed$game_id, ])
  finite <- plays[is.finite(plays$EPA), ]
  coverage$competitive_finite_epa_rows <- counts(finite$game_id)
  pairs <- unique(paste(finite$game_id, finite$pos_team))
  coverage$both_teams_finite_official_epa <- paste(coverage$game_id, coverage$home) %in% pairs &
    paste(coverage$game_id, coverage$away) %in% pairs
  missing <- coverage$required_fbs_history & !coverage$both_teams_finite_official_epa
  unfinished <- earlier[is.na(earlier$completed) | !earlier$completed, ]
  csv(coverage, "input_coverage.csv"); csv(coverage[missing, ], "missing_required_pbp.csv")
  csv(unfinished, "unfinished_earlier_games.csv"); csv(completed, "source_games.csv")
  saveRDS(universe, file.path(out, "schedule_universe.rds"))
  csv(ranked_slate(universe, as_of), "ap_ranked_slate.csv")
  info <- list(status = if (any(missing) || nrow(unfinished)) "coverage_incomplete" else "inputs_ready",
    season = season, week = week, model_week = model_week, as_of = utc(as_of),
    schedule_retrieved_at = utc(schedule_at), pbp_retrieved_at = utc(pbp_at),
    market_retrieved_at = utc(captured_at), cfbd_api_requests = 4L,
    official_raw_sha256 = raw_sha, official_pbp_rows = prior_acceptance$official_pbp_rows,
    selected_pbp_rows = nrow(pbp), completed_prior_games = nrow(completed),
    required_fbs_games = sum(coverage$required_fbs_history), missing_required_games = sum(missing),
    unfinished_earlier_games = nrow(unfinished),
    calculated_supplement_games = as.integer(prior_acceptance$calculated_supplement_games),
    source_selection_reason = paste(prior_acceptance$source_policy,
      "Missouri State-Kennesaw State six school-corroborated non-scrimmage events retained (EPA NA)."),
    coaching_assignments = "Refreshed normal public coaching source; existing manual overrides retained",
    source_paths = as.list(paths), raw_pbp_path = raw_path, accepted_pbp_source = accepted_pbp_path,
    sources = list(schedule = "https://api.collegefootballdata.com/games?year=2026&seasonType=regular",
      membership = "https://api.collegefootballdata.com/teams/fbs?year=2026",
      lines = "https://api.collegefootballdata.com/lines?year=2026&week=8",
      epa = "Accepted Oct 18 Week 8 rankings selection of the official cfbfastR 2026 release"))
  stopifnot(identical(unname(tools::md5sum(protected)), hashes))
  json(info, "input_manifest.json")
  say("Coverage:", nrow(completed), "completed prior games;", sum(coverage$required_fbs_history),
    "required FBS games;", sum(missing), "missing official EPA;", nrow(unfinished), "unfinished.")
  started <- universe[universe$season == season & universe$week == week &
    (universe$home_level == "fbs" | universe$away_level == "fbs") &
    !is.na(universe$kickoff) & universe$kickoff <= as_of, ]
  csv(started, "excluded_started_week8_games.csv")
  say("Week 8 games already started before capture (not predicted):", nrow(started))
  say("INPUT STATUS:", info$status)
  quit(status = 0L)
}

if (phase == "audit") {
  stop("Week 8 carries forward the accepted Oct 18 source audit; there is no separate audit phase.")
  verified_path <- file.path(root, "cfb_v3/output/2026/week_7_preliminary_research/ranked_slate_20261012.csv")
  verified <- read.csv(verified_path, stringsAsFactors = FALSE)
  corrections <- verified[!is.na(verified$kickoff_correction) & nzchar(verified$kickoff_correction), ]
  raw_schedule <- readRDS(paths[["schedule"]])
  if (nrow(corrections)) {
    j <- match(corrections$game_id, raw_schedule$game_id)
    stopifnot(!anyNA(j), all(raw_schedule$week[j] == week))
    corrections$cfbd_kickoff_utc <- utc(parse_utc_datetime(raw_schedule$start_date[j]))
    saveRDS(raw_schedule, file.path(out, "cfbd_schedule_before_school_override.rds"))
    raw_schedule$start_date[j] <- if (inherits(raw_schedule$start_date, "POSIXt"))
      parse_utc_datetime(corrections$kickoff_utc) else corrections$kickoff_utc
    stopifnot(all(!is.na(parse_utc_datetime(raw_schedule$start_date[j]))))
    saveRDS(raw_schedule, paths[["schedule"]], compress = "xz")
    csv(corrections, "schedule_overrides.csv")
  }
  # Reuse the independently audited Oct 11 selection rules against this fresh release.
  prior_audit_dir <- file.path(root, "cfb_v3/output/2026/week_7_rankings_relative/20261011T145259Z")
  for (name in c("audit_sources.R")) {
    code <- readLines(file.path(prior_audit_dir, name), warn = FALSE)
    stopifnot(sum(grepl("^out <-", code)) == 1L)
    code[grepl("^out <-", code)] <- paste0("out <- ", deparse(out))
    if (name == "audit_sources.R") {
      code <- sub('stopifnot(!file.exists(backup),file.copy(path,backup))',
        'if (!file.exists(backup)) stopifnot(file.copy(path,backup)); stopifnot(identical(readRDS(path),readRDS(backup)))',
        code, fixed = TRUE)
      first <- which(grepl("^response <- httr::GET", code))
      last <- which(startsWith(code, 'cat("RAW DOWNLOAD VERIFIED'))
      stopifnot(length(first) == 1L, length(last) == 1L, last > first)
      code <- c(code[seq_len(first - 1L)],
        'raw_path <- file.path(out,"official_2026_raw.rds")',
        'raw <- readRDS(raw_path)',
        'stopifnot(identical(compact_cfb_pbp(raw),pbp))', code[(last + 1L):length(code)])
    } else {
      code <- gsub("file.copy(", "copy_or_verify(", code, fixed = TRUE)
      code <- c('copy_or_verify <- function(from,to) { if(file.exists(to)) identical(unname(tools::md5sum(from)),unname(tools::md5sum(to))) else file.copy(from,to) }', code)
    }
    saved <- file.path(out, name)
    writeLines(code, saved)
    sys.source(saved, envir = new.env(parent = globalenv()))
  }
  universe <- readRDS(file.path(out, "schedule_universe.rds"))
  slate <- ranked_slate(universe, Sys.time())
  stopifnot(all(as.character(verified$game_id) %in% as.character(slate$game_id)))
  csv(slate, "ap_ranked_slate.csv")
  say("Fresh source probe complete; acceptance requires separate review before build.")
  quit(status = 0L)
}

if (phase == "build") {
  if (file.exists(file.path(out, "predictions.csv"))) stop("Refusing to overwrite predictions.")
  info <- jsonlite::read_json(file.path(out, "input_manifest.json"), simplifyVector = TRUE)
  stopifnot(info$status == "inputs_ready", info$calculated_supplement_games %in% c(0L, 1L))
  acceptance <- jsonlite::read_json(file.path(out, "source_acceptance.json"), simplifyVector = TRUE)
  stopifnot(acceptance$status == "accepted", acceptance$accepted_pbp_md5 == unname(tools::md5sum(paths[["pbp"]])))
  selected_pbp <- readRDS(paths[["pbp"]])
  if (any(selected_pbp$game_id == "401858207" & selected_pbp$id_play == 40185820762, na.rm = TRUE))
    stop("Pitt-Miami (OH) contains verified false play 40185820762. Apply the documented school-PBP exclusion before rebuilding.")
  protected <- read.csv(file.path(out, "protected_input_hashes.csv"), stringsAsFactors = FALSE)
  stopifnot(identical(unname(tools::md5sum(protected$path)), protected$md5))
  research <- readRDS(fit_path); fit <- research$model
  baseline <- readRDS(baseline_bundle_path)
  removed <- c("source_games_diff", "games_played_diff")
  expected <- c(setdiff(baseline$model$foundation$features, removed), "opponent_relative_margin_diff")
  stopifnot(identical(fit$recipe$features, expected), length(expected) == 19L,
    fit$lambda == 32, fit$training_rows == 4908L, all(research$training$season < season))
  info$source_capture_as_of <- info$as_of
  as_of <- Sys.time()
  info$as_of <- utc(as_of)
  universe <- readRDS(file.path(out, "schedule_universe.rds"))
  target_slate <- ranked_slate(universe, as_of)
  stopifnot(nrow(target_slate) > 0L, !anyDuplicated(target_slate$game_id))
  # Preserve every FBS-involving game in coverage; only the existing FBS-v-FBS model can predict.
  intended_slate <- target_slate
  unsupported <- target_slate[target_slate$home_level != "fbs" | target_slate$away_level != "fbs", ]
  unsupported$reason <- rep("FCS opponent outside fitted model scope; no prediction fabricated", nrow(unsupported))
  csv(unsupported, "unsupported_fcs_games.csv")
  csv(intended_slate, "full_upcoming_fbs_involving_slate.csv")
  target_slate <- target_slate[target_slate$home_level == "fbs" & target_slate$away_level == "fbs", ]
  stopifnot(nrow(target_slate) > 0L)
  review_path <- opt("injury-review")
  if (is.null(review_path) || !file.exists(review_path)) stop("Complete injury research before build; supply --injury-review=PATH.")
  review <- jsonlite::read_json(review_path, simplifyVector = TRUE)
  stopifnot(review$status == "ready_for_preliminary",
    setequal(as.character(review$game_ids), as.character(target_slate$game_id)),
    parse_utc_datetime(review$reviewed_at) <= as_of)
  json(review, "injury_research_gate.json")
  json(info, "input_manifest.json")
  inputs <- read_v2_inputs(config, strict = FALSE)
  inputs$lines <- NULL # Only freshly retrieved quotes may supply this preliminary card.
  say("Building standard live snapshots, bridge and coach features from isolated inputs.")
  inputs <- prepare_live_inputs(inputs, config, season, week, as_of, refresh = FALSE)
  schedule <- normalize_public_schedule(inputs$schedule)
  stopifnot(all(target_slate$game_id %in% schedule$game_id))
  schedule <- schedule[match(target_slate$game_id, schedule$game_id), ]
  stopifnot(identical(as.character(schedule$game_id), as.character(target_slate$game_id)))
  csv(target_slate, "ap_ranked_slate.csv")
  membership_flags <- combine_membership(inputs$membership,
    read_csv_if_present(file.path(config$data_dir, "historical_fbs_membership.csv"), TRUE))
  transitions <- fbs_transition_teams(membership_flags)
  team_features <- inputs$team_week_features
  if (nrow(transitions)) {
    bridge <- calibrate_fbs_bridge(read_csv_if_present(file.path(config$data_dir, "historical_team_games.csv"), TRUE),
      membership_flags, config)
    team_features <- apply_fbs_bridge_features(team_features, transitions, bridge)
  }
  x <- prepare_matchup_features(schedule, team_features, inputs$coach_assignments,
    inputs$coach_history, .65, config)
  x <- attach_preseason_features(x, read_csv_if_present(file.path(config$data_dir, "preseason_team_priors.csv"), TRUE),
    config, features = config$preseason$production_features, prefix = "ps_", require_coverage = TRUE)
  universe <- readRDS(file.path(out, "schedule_universe.rds"))
  completed <- filter_completed_historical_games(universe)
  completed <- completed[completed$model_week < info$model_week & completed$kickoff < as_of, ]
  targets <- x; targets$feature_week_start <- as_of
  relative <- opponent_relative_build(completed, targets)
  for (field in setdiff(names(relative$features), "game_id")) x[[field]] <- relative$features[[field]]
  stopifnot(all(x$preseason_weight == 0), identical(as.character(x$game_id), as.character(schedule$game_id)))
  selected <- select_article_lines(readRDS(paths[["lines"]]), schedule$game_id, as_of)
  selected <- selected[match(schedule$game_id, selected$game_id), ]
  slate <- target_slate
  slate$omission_reason <- ifelse(is.finite(selected$market_home_spread), "included_with_line", "included_projection_no_fresh_DK_FD_line")
  csv(slate, "slate_coverage.csv")
  lined <- is.finite(selected$market_home_spread)
  csv(schedule[!lined, ], "unlined_games.csv")
  stopifnot(nrow(x) == nrow(target_slate), all(parse_utc_datetime(x$kickoff) > as_of))
  predicted <- predict(fit, x)
  baseline_margin <- predict(baseline$model, x)$expected_margin
  changed <- x; changed$source_games_diff <- 999; changed$games_played_diff <- -999
  count_error <- max(abs(predict(fit, changed) - predicted))
  stopifnot(count_error == 0, all(is.finite(predicted)), all(is.finite(baseline_margin)))
  card <- schedule[c("game_id", "season", "week", "home", "away", "neutral_site")]
  for (name in setdiff(names(selected), "game_id")) card[[name]] <- selected[[name]]
  card$expected_margin <- predicted; card$fair_spread <- -predicted
  card$ats_edge_home <- predicted + card$market_home_spread
  card$straight_up_pick <- ifelse(predicted >= 0, card$home, card$away)
  card$ats_pick <- ifelse(is.finite(card$ats_edge_home), ifelse(card$ats_edge_home >= 0, card$home, card$away), NA_character_)
  card$pick_status <- ifelse(!lined, "pass", ifelse(abs(card$market_home_spread) > config$ats$large_spread_review,
    "large_spread_review", "article_pick"))
  for (name in c("margin_sd", "home_win_probability", "home_cover_probability", "preseason_expected_margin",
    "preseason_raw_delta", "expected_total", "total_sd")) card[[name]] <- NA_real_
  card$confidence_tier <- NA_character_; card$ats_calibrated <- FALSE
  card$foundation_expected_margin <- predicted
  card$preseason_challenger_share <- 0; card$preseason_adjustment <- 0
  card$injury_scenario <- "most_likely"; card$injuries_adjusted <- FALSE; card$forced_pick <- lined
  card$top_drivers <- ridge_feature_drivers(list(ridge = fit), x)
  card$model_version <- "3.0.0-opponent-relative-experiment"
  card$snapshot_type <- "experimental_live"; card$published <- FALSE
  card$preseason_profile <- "Uncapped + opponent-relative; no direct game-count predictors"
  card$publication_note <- paste0("Friday Week 8 | Full remaining FBS-v-FBS model slate | FCS crossovers recorded separately as unsupported | ",
    "Injuries are not adjusted | Accepted Oct 18 source audit retains the documented OSU-CSU supplement and Pitt false-play exclusion")
  card$kickoff <- utc(parse_utc_datetime(x$kickoff))
  card$kickoff_display <- format(parse_utc_datetime(x$kickoff), "%a %I:%M %p ET", tz = config$article_timezone)
  hour <- as.integer(format(parse_utc_datetime(x$kickoff), "%H", tz = config$article_timezone))
  card$primetime <- hour >= 19L | hour < 3L
  card$as_of <- utc(as_of); card$market_captured_at <- utc(card$market_captured_at)
  missing_coach <- rep(FALSE, nrow(x))
  for (name in intersect(c("home_coach_history_missing", "away_coach_history_missing"), names(x)))
    missing_coach <- missing_coach | as.logical(x[[name]])
  card$data_flag <- ifelse(missing_coach, "new_coach_no_model_history", "standard")
  card$home_ap_rank <- target_slate$home_ap_rank
  card$away_ap_rank <- target_slate$away_ap_rank
  card$ap_poll_date <- ap$poll_date
  card <- prepare_dashboard_predictions(card)
  card$confidence_tier <- NA_character_; card$reported_confidence <- NA_character_
  control <- card; control$expected_margin <- baseline_margin
  control$fair_spread <- -baseline_margin; control$foundation_expected_margin <- baseline_margin
  control$ats_edge_home <- baseline_margin + control$market_home_spread
  control$ats_pick <- ifelse(baseline_margin + control$market_home_spread >= 0, control$home, control$away)
  control$straight_up_pick <- ifelse(baseline_margin >= 0, control$home, control$away)
  control$top_drivers <- ridge_feature_drivers(baseline$model$foundation, x)
  control$publication_note <- "Diagnostic original-model control using identical fresh inputs and quotes"
  control <- prepare_dashboard_predictions(control)
  control$confidence_tier <- NA_character_; control$reported_confidence <- NA_character_
  csv(card, "unadjusted_predictions.csv")
  # Carry verified availability forward, but never reuse a prior game's point penalty.
  registry_path <- file.path(config$inbox_dir, "player_availability.csv")
  availability <- data.frame()
  if (file.exists(registry_path)) {
    availability <- resolve_season_availability(
      read.csv(registry_path, stringsAsFactors = FALSE, colClasses = c(player_id = "character")),
      card, as_of)
    if (nrow(availability)) {
      card <- apply_injury_overlay(card, availability, as_of, config)$predictions
      card$publication_note <- sub("Injuries are not adjusted",
        "Known absences flagged; fresh per-game injury estimates required", card$publication_note, fixed = TRUE)
    }
  }
  csv(availability, "injury_reports.csv")
  comparison <- data.frame(game_id = card$game_id, away = card$away, home = card$home,
    market_home_spread = card$market_home_spread, baseline_margin = baseline_margin,
    opponent_relative_margin = predicted, margin_change = predicted - baseline_margin,
    ats_side_changed = card$ats_pick != control$ats_pick, su_changed = card$straight_up_pick != control$straight_up_pick)
  csv(card, "predictions.csv"); csv(x, "features.csv"); csv(control, "baseline_control.csv")
  csv(comparison, "comparison.csv"); csv(selected, "market_snapshot.csv")
  csv(team_features, "team_week_features.csv"); csv(inputs$coach_assignments, "coach_assignments.csv")
  csv(relative$game_audit, "game_audit.csv"); csv(relative$team_audit, "team_audit.csv")
  saveRDS(list(model = fit, relative = relative, config = config, as_of = as_of, source_fit = fit_path,
    baseline_csv = file.path(out, "baseline_control.csv"), source_games = completed), file.path(out, "model.rds"))
  sources <- unique(c(unname(paths), config_path, fit_path, baseline_bundle_path,
    file.path(out, c("source_acceptance.json", "prior_source_acceptance.json", "ap_poll.json")),
    accepted_pbp_path, file.path(rankings_dir, "source_acceptance.json"), ap_path, review_path, protected$path,
    file.path(root, "cfb_v2", paste0(modules, ".R")),
    file.path(root, c("cfb_v3/experiments/opponent_relative_features.R", "cfb_v3/injury_overlay.R")),
    opt("runner-path"), registry_path[file.exists(registry_path)]))
  csv(data.frame(path = sources, md5 = unname(tools::md5sum(sources))), "input_hashes.csv")
  stopifnot(identical(unname(tools::md5sum(protected$path)), protected$md5),
    max(abs(read.csv(file.path(out, "predictions.csv"))$expected_margin - predicted)) < 1e-10)
  qa <- list(status = "predictions_complete", season = season, week = week, model_week = info$model_week,
    generated_at = utc(Sys.time()), as_of = utc(as_of), predictions = nrow(card),
    source_card = file.path(out, "baseline_control.csv"), source_fit = fit_path,
    model_features = expected, removed_features = removed, margin_cap = "uncapped",
    model_refit = FALSE, training_rows = fit$training_rows, training_seasons = 2020:2025,
    ridge_lambda = fit$lambda, coach_recent_share = .65, count_perturbation_error = count_error,
    saved_fit_coefficients_identical = identical(fit$coefficients, readRDS(fit_path)$model$coefficients),
    ats_side_changes = sum(comparison$ats_side_changed, na.rm = TRUE), su_changes = sum(comparison$su_changed),
    lines_unchanged = TRUE, line_times_unchanged = TRUE, source_hashes_unchanged = TRUE,
    production_model_changed = FALSE, public_site_changed = FALSE, injuries_adjusted = FALSE,
    known_availability_reports = nrow(availability),
    injury_review_games = sum(card$injury_flag, na.rm = TRUE),
    ap_poll_used_as_predictor = FALSE, ap_poll_date = ap$poll_date, fbs_model_games = nrow(target_slate), unsupported_fcs_games = nrow(unsupported), full_fbs_involving_slate = nrow(intended_slate),
    unlined_games_retained = sum(!lined), injury_research_gate = review_path,
    omit_thursday = FALSE, required_fbs_games = info$required_fbs_games, missing_required_games = 0L,
    calculated_supplement_games = info$calculated_supplement_games, current_relative = relative$qa,
    source_data_note = info$source_selection_reason,
    omissions = as.list(table(slate$omission_reason)),
    probability_policy = "Point forecasts only; no probabilities, uncertainty or confidence classifications.",
    baseline_note = "Diagnostic only: saved original-model fit on the same fresh features and quotes; no refit.",
    quote_note = "Normal DraftKings/FanDuel latest-quote consensus. Timestamp is retrieval time, not provider update time.")
  json(qa, "qa.json")
  say("PREDICTIONS:", file.path(out, "predictions.csv"))
  say("Games:", nrow(card), "; Thursday:", sum(weekdays(parse_utc_datetime(card$kickoff)) == "Thursday"),
    "; ATS differences from original control:", qa$ats_side_changes)
  quit(status = 0L)
}

if (phase == "render") {
  quarto_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin")
  Sys.setenv(PATH = paste(quarto_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  dashboard <- render_cfb_dashboard(file.path(out, "predictions.csv"), root, out, "dashboard.html")
  qa <- jsonlite::read_json(file.path(out, "qa.json"), simplifyVector = FALSE)
  qa$status <- "complete"; qa$rendered_at <- utc(Sys.time())
  json(qa, "qa.json")
  say("DASHBOARD:", dashboard)
}
