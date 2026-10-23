# Bounded source selection audit: fresh official release first, validated exceptions only.
Sys.setlocale("LC_ALL", "English_United States.utf8")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
for (m in c("config", "store", "features", "coaches", "adapters", "models",
  "coach_migration", "historical_data", "preseason", "bridge", "workflow", "live_data"))
  source(file.path(root, "cfb_v2", paste0(m, ".R")))
csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
json <- function(x, name) jsonlite::write_json(x, file.path(out, name), pretty = TRUE,
  auto_unbox = TRUE, digits = 15, null = "null")
stopifnot(!file.exists(file.path(out, "source_acceptance.json")))
info <- jsonlite::read_json(file.path(out, "input_manifest.json"), simplifyVector = TRUE)
stopifnot(info$season == 2026L, info$week == 8L)
path <- file.path(out, "cfb_v2/cache/pbp/pbp_2026_compact.rds")
backup <- file.path(out, "official_compact_before_source_selection.rds")
if (!file.exists(backup)) stopifnot(file.copy(path, backup))
official <- readRDS(backup)
raw_path <- file.path(out, "official_2026_raw.rds")
asset <- jsonlite::read_json(file.path(out, "official_release_asset.json"), simplifyVector = TRUE)
raw_sha <- digest::digest(file = raw_path, algo = "sha256")
stopifnot(paste0("sha256:", raw_sha) == asset$digest, file.info(raw_path)$size == asset$size)
raw <- readRDS(raw_path)
stopifnot(identical(compact_cfb_pbp(raw), official))

# Do not retain the old calculated EPA when the fresh official game is repaired.
g <- as.data.frame(raw[as.character(raw$game_id) == "401860899", ])
g$source_row <- seq_len(nrow(g))
g <- g[order(g$game_play_number, g$source_row), ]
g$home_score_audit <- ifelse(g$pos_team == g$home, g$pos_team_score, g$def_pos_team_score)
g$away_score_audit <- ifelse(g$pos_team == g$away, g$pos_team_score, g$def_pos_team_score)
last <- function(x) tail(x[is.finite(x)], 1)
drops <- function(x) sum(diff(x[is.finite(x)]) < 0)
source_qa <- list(rows = nrow(g), duplicate_game_play_number = sum(duplicated(g$game_play_number)),
  score_bounds_pass = all(g$home_score_audit[is.finite(g$home_score_audit)] >= 0 & g$home_score_audit[is.finite(g$home_score_audit)] <= 26) &&
    all(g$away_score_audit[is.finite(g$away_score_audit)] >= 0 & g$away_score_audit[is.finite(g$away_score_audit)] <= 56),
  final_score_pass = isTRUE(last(g$home_score_audit) == 26 && last(g$away_score_audit) == 56),
  home_last = last(g$home_score_audit), away_last = last(g$away_score_audit),
  home_score_decreases = drops(g$home_score_audit), away_score_decreases = drops(g$away_score_audit),
  identity_valid = nrow(g) > 0 && all(g$home == "Colorado State" & g$away == "Oregon State" &
    g$pos_team %in% c("Colorado State", "Oregon State") & g$def_pos_team %in% c("Colorado State", "Oregon State") & g$pos_team != g$def_pos_team),
  source_sha256 = raw_sha)
source_qa$accepted <- source_qa$identity_valid && source_qa$duplicate_game_play_number == 0 &&
  source_qa$score_bounds_pass && source_qa$final_score_pass &&
  source_qa$home_score_decreases == 0 && source_qa$away_score_decreases == 0
json(source_qa, "fresh_official_401860899_qa.json")
csv(g[intersect(c("game_id", "id_play", "game_play_number", "period", "pos_team", "def_pos_team", "play_type", "play_text", "EPA", "home_score_audit", "away_score_audit"), names(g))], "fresh_official_401860899_plays.csv")
supplement_used <- !source_qa$accepted
replacement <- official[FALSE, ]
if (supplement_used) {
  prior <- file.path(root, "cfb_v3/output/2026/week_6_rankings_relative/20261004T163129Z")
  compact_path <- file.path(prior, "espn_supplement/verified_school_game_compact.rds")
  full_path <- file.path(prior, "espn_supplement/school_401860899_processed_full.rds")
  independent <- jsonlite::read_json(file.path(prior, "espn_supplement/independent_adapter_qa.json"), simplifyVector = TRUE)
  stopifnot(independent$status == "passed",
    digest::digest(file = compact_path, algo = "sha256") == "98778bc91211e9bc36ea1559c3a2dcbd828e86cd302748f96c940c1dd99b1abf",
    digest::digest(file = full_path, algo = "sha256") == "a6898a8d67d72f166cdaf4ccf400a792b23ad66d3806c4e987019651a51db39f")
  replacement <- readRDS(compact_path)
  stopifnot(nrow(replacement) == 155L, all(replacement$game_id == "401860899"),
    !anyDuplicated(replacement$id_play), all(is.finite(replacement$EPA)), identical(names(replacement), names(official)))
  for (name in c("spread", "over_under")) {
    stopifnot(all(is.na(replacement[[name]])))
    replacement[[name]] <- as.numeric(replacement[[name]])
  }
  retained <- file.path(out, "espn_supplement")
  dir.create(retained, showWarnings = FALSE)
  for (name in c("verified_school_game_compact.rds", "independent_adapter_qa.json")) {
    target <- file.path(retained, name)
    source <- file.path(prior, "espn_supplement", name)
    if (!file.exists(target)) stopifnot(file.copy(source, target))
    stopifnot(identical(unname(tools::md5sum(source)), unname(tools::md5sum(target))))
  }
}

false_row <- as.character(official$game_id) == "401858207" & as.character(official$id_play) == "40185820762"
stopifnot(sum(false_row) <= 1L)
if (any(false_row)) stopifnot(abs(official$EPA[false_row] - 6.012249140068889) < 1e-10,
  official$play_type[false_row] == "Passing Touchdown", official$play_text[false_row] == "M. Heintschel pass to C. Lee")
csv(official[false_row, ], "verified_false_play_excluded.csv")
keep <- !false_row & (!supplement_used | official$game_id != "401860899")
other <- official[keep, ]
selected <- if (supplement_used) rbind(other, replacement) else other
stopifnot(identical(as.list(selected[seq_len(nrow(other)), ]), as.list(other)),
  !any(as.character(selected$id_play) == "40185820762", na.rm = TRUE))
saveRDS(selected, path, compress = "xz")

schedule <- readRDS(file.path(out, "cfb_v2/cache/schedules/cfbd_schedule_2026.rds"))
membership <- readRDS(file.path(out, "cfb_v2/cache/teams/cfbd_fbs_teams_2026.rds"))
universe <- derive_games_from_pbp(selected, schedule = schedule, fbs_membership = membership)
earlier <- universe[universe$model_week < info$model_week & universe$kickoff < parse_utc_datetime(info$as_of), ]
completed <- filter_completed_historical_games(earlier)
unfinished <- earlier[is.na(earlier$completed) | !earlier$completed, ]
counts <- function(ids) as.integer(table(factor(ids, levels = completed$game_id)))
comp <- historical_competitive_plays(selected[selected$game_id %in% completed$game_id, ])
normal <- comp[which(comp$rush == 1 | comp$pass == 1 | comp$sack == 1 | grepl("pass|rush|run|sack", comp$play_type, ignore.case = TRUE)), ]
pairs <- unique(paste(normal$game_id[is.finite(normal$EPA)], normal$pos_team[is.finite(normal$EPA)]))
coverage <- completed[c("game_id", "season", "week", "model_week", "kickoff", "home", "away", "home_score", "away_score", "home_level", "away_level", "completed", "pbp_available")]
coverage$required_fbs_history <- raw_history_eligible(completed)
coverage$pbp_rows <- counts(selected$game_id)
coverage$finite_epa_rows <- counts(selected$game_id[is.finite(selected$EPA)])
coverage$competitive_rows <- counts(comp$game_id)
coverage$competitive_finite_epa_rows <- counts(comp$game_id[is.finite(comp$EPA)])
coverage$normal_rows <- counts(normal$game_id)
coverage$missing_normal_epa_rows <- counts(normal$game_id[!is.finite(normal$EPA)])
coverage$home_finite_normal_epa <- paste(coverage$game_id, coverage$home) %in% pairs
coverage$away_finite_normal_epa <- paste(coverage$game_id, coverage$away) %in% pairs
coverage$both_teams_finite_epa <- coverage$home_finite_normal_epa & coverage$away_finite_normal_epa
coverage$both_teams_finite_official_epa <- coverage$both_teams_finite_epa & (!supplement_used | coverage$game_id != "401860899")
coverage$source <- "Official published cfbfastR EPA"
if (supplement_used) coverage$source[coverage$game_id == "401860899"] <- "Validated school PBP; calculated EPA retained because fresh official stream remains corrupt"
if (any(false_row)) coverage$source[coverage$game_id == "401858207"] <- "Official EPA with documented false touchdown row excluded; no EPA recalculated"
missing <- coverage$required_fbs_history & !coverage$both_teams_finite_epa
old_games <- read.csv(file.path(out, "source_games.csv"), stringsAsFactors = FALSE)
stopifnot(setequal(as.character(old_games$game_id), as.character(completed$game_id)), max(completed$week) == 7L)
csv(coverage, "input_coverage.csv")
csv(coverage[missing, ], "missing_required_pbp.csv")
csv(unfinished, "unfinished_earlier_games.csv")
csv(completed, "source_games.csv")
saveRDS(universe, file.path(out, "schedule_universe.rds"))
normal_missing <- normal[which(!is.finite(normal$EPA) & normal$game_id %in% coverage$game_id[coverage$required_fbs_history]), ]
csv(normal_missing, "required_missing_normal_epa_details.csv")
info$status <- if (any(missing) || nrow(unfinished)) "coverage_incomplete" else "inputs_ready"
info$calculated_supplement_games <- as.integer(supplement_used)
info$official_required_games <- sum(coverage$required_fbs_history) - as.integer(supplement_used)
info$missing_required_games <- sum(missing)
info$selected_pbp_rows <- nrow(selected)
info$selected_pbp_md5 <- unname(tools::md5sum(path))
info$official_401860899_rejected <- supplement_used
info$source_reviewed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
info$verified_false_row_exclusion <- list(game_id = "401858207", id_play = "40185820762", rows_excluded = sum(false_row),
  source_url = "https://pittsburghpanthers.com/sports/football/stats/2026/miami-oh-/boxscore/15187",
  reason = "Previously disproven extra touchdown excluded only when its exact unchanged row remains; no other game EPA recalculated.")
info$all_other_official_rows_except_documented_exclusions_unchanged <- TRUE
info$source_selection_reason <- if (supplement_used) "Fresh official OSU-CSU sequence fails known-corruption checks; retain previously validated 155-row school supplement. Exact unchanged false Pitt touchdown excluded if present. All other official rows unchanged." else "Fresh official OSU-CSU sequence accepted; no calculated-EPA supplement. Exact unchanged false Pitt touchdown excluded if present. All other official rows unchanged."
json(info, "input_manifest.json")
if (supplement_used) {
  supp <- jsonlite::read_json(file.path(prior, "epa_supplement.json"), simplifyVector = TRUE)
  supp$retained_at <- info$source_reviewed_at
  supp$original_validated_run <- prior
  supp$compact_source_sha256 <- digest::digest(file = compact_path, algo = "sha256")
  supp$official_rows_unchanged <- !any(false_row)
  supp$all_other_official_rows_except_documented_exclusions_unchanged <- TRUE
  supp$additional_source_selection <- info$verified_false_row_exclusion
  supp$rejected_official_audit <- file.path(out, "fresh_official_401860899_qa.json")
  json(supp, "epa_supplement.json")
}
protected <- read.csv(file.path(out, "protected_input_hashes.csv"), stringsAsFactors = FALSE)
stopifnot(identical(unname(tools::md5sum(protected$path)), protected$md5), identical(readRDS(backup), official))
acceptance <- list(status = if (info$status == "inputs_ready") "accepted" else "blocked",
  checked_at = info$source_reviewed_at, accepted_pbp_md5 = info$selected_pbp_md5,
  official_pbp_rows = nrow(official), selected_pbp_rows = nrow(selected), official_raw_sha256 = raw_sha,
  official_raw_matches_fetched_compact = TRUE, raw_release_digest_verified = TRUE,
  completed_prior_games = nrow(completed), required_fbs_games = sum(coverage$required_fbs_history),
  required_team_sides = 2L * sum(coverage$required_fbs_history),
  all_required_teams_have_finite_competitive_normal_epa = !any(missing), missing_required_games = sum(missing),
  unfinished_prior_games = nrow(unfinished), calculated_supplement_games = as.integer(supplement_used),
  official_required_games = info$official_required_games, official_known_corruption_audit = source_qa,
  retained_validated_school_rows = if (supplement_used) 155L else 0L,
  verified_false_row_exclusion = info$verified_false_row_exclusion,
  all_other_official_rows_except_documented_exclusions_unchanged = TRUE, protected_inputs_unchanged = TRUE,
  required_missing_normal_epa_rows = nrow(normal_missing), missing_normal_epa_periods = sort(unique(normal_missing$period)),
  source_policy = info$source_selection_reason,
  bounded_scope = "Both-team competitive normal-play coverage and previously documented source defects; not a full play-by-play boxscore audit of every game. Missing individual EPA rows retained and logged; no imputation.")
json(acceptance, "source_acceptance.json")
cat("SOURCE", acceptance$status, "games", nrow(completed), "required", sum(coverage$required_fbs_history),
  "missing", sum(missing), "unfinished", nrow(unfinished), "supplements", as.integer(supplement_used), "\n")
if (info$status != "inputs_ready") print(coverage[missing, c("game_id", "week", "home", "away", "pbp_rows")], row.names = FALSE)
