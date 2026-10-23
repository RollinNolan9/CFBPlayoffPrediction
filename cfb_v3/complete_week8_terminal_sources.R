# Append school-corroborated missing non-scrimmage events without inventing EPA.
# The existing rankings stay valid only if the football features remain identical.
Sys.setlocale("LC_ALL", "English_United States.utf8")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
for (m in c("config", "store", "features", "coaches", "adapters", "models", "coach_migration", "historical_data", "preseason", "bridge", "workflow", "live_data")) source(file.path(root, "cfb_v2", paste0(m, ".R")))
json <- function(x, name) jsonlite::write_json(x, file.path(out, name), pretty = TRUE, auto_unbox = TRUE, digits = 15, null = "null")
csv <- function(x, name) write.csv(x, file.path(out, name), row.names = FALSE, na = "")
stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
pbp_path <- file.path(out, "cfb_v2/cache/pbp/pbp_2026_compact.rds")
stopifnot(!file.exists(file.path(out, "terminal_completion_qa.json")))
protected <- read.csv(file.path(out, "protected_input_hashes.csv"), stringsAsFactors = FALSE)
stopifnot(identical(unname(tools::md5sum(protected$path)), protected$md5))
old_hashes <- read.csv(file.path(out, "input_hashes.csv"), stringsAsFactors = FALSE)
stopifnot(identical(unname(tools::md5sum(old_hashes$path)), old_hashes$md5))
copy_or_verify <- function(from, to) {
  if (!file.exists(to)) stopifnot(file.copy(from, to))
  stopifnot(identical(unname(tools::md5sum(from)), unname(tools::md5sum(to))))
}
for (name in c("input_hashes.csv", "source_acceptance.json", "input_manifest.json", "qa.json", "input_coverage.csv"))
  copy_or_verify(file.path(out, name), file.path(out, sub("([.][^.]+)$", "_before_pbp_completion\\1", name)))
copy_or_verify(pbp_path, file.path(out, "selected_compact_before_pbp_completion.rds"))
before <- readRDS(pbp_path)
rank_files <- file.path(out, c("rankings.csv", "top25.csv", "ranking_fit.rds", "team_snapshots.csv", "current_team_games.csv", "included_games.csv", "neutral_matchups.csv"))
rank_hashes <- unname(tools::md5sum(rank_files))
evidence_path <- file.path(out, "recovery_401871054/espn_summary.json")
evidence <- jsonlite::read_json(evidence_path, simplifyVector = FALSE)
drives <- c(evidence$drives$previous, if (!is.null(evidence$drives$current)) list(evidence$drives$current) else list())
plays <- unlist(lapply(drives, function(d) d$plays), recursive = FALSE)
ids <- vapply(plays, function(p) p$id, "")
approved <- c("40187105489", "401871054316", "401871054812", "401871054818", "401871054816", "401871054822")
rejected <- "401871054714"
stopifnot(setequal(setdiff(ids, as.character(before$id_play[before$game_id == "401871054"])), c(approved, rejected)), !anyDuplicated(ids))
g <- before[before$game_id == "401871054", ]
rows <- g[rep(1L, length(approved)), ]
for (i in seq_along(approved)) {
  p <- plays[[match(approved[i], ids)]]
  home_possession <- p$start$team$id == "2623"
  rows$id_play[i] <- as.numeric(p$id)
  rows$pos_team[i] <- if (home_possession) "Missouri State" else "Kennesaw State"
  rows$def_pos_team[i] <- if (home_possession) "Kennesaw State" else "Missouri State"
  rows$pos_team_score[i] <- as.integer(if (home_possession) p$homeScore else p$awayScore)
  rows$def_pos_team_score[i] <- as.integer(if (home_possession) p$awayScore else p$homeScore)
  rows$pos_score_diff[i] <- rows$pos_team_score[i] - rows$def_pos_team_score[i]
  rows$period[i] <- as.integer(p$period$number)
  rows$play_type[i] <- p$type$text
  rows$play_text[i] <- p$text
  for (name in c("EPA", "home_EPA", "away_EPA", "success")) rows[[name]][i] <- NA_real_
  for (name in c("rush", "pass", "sack", "turnover_indicator", "turnover", "stuffed_run")) rows[[name]][i] <- 0
  rows$kick_play[i] <- as.numeric(grepl("Punt|Field Goal", p$type$text))
  rows$punt_play[i] <- as.numeric(grepl("Punt", p$type$text))
  rows$fg_inds[i] <- as.numeric(p$type$text == "Field Goal Good")
  rows$garbage_time[i] <- FALSE
}
stopifnot(all(rows$play_type %in% c("Punt Return", "Punt", "Penalty", "Timeout", "Field Goal Good", "End of Game")),
  all(is.na(rows$EPA)), all(is.na(rows$home_EPA)), all(is.na(rows$away_EPA)))
after <- rbind(before, rows)
stopifnot(identical(as.list(after[seq_len(nrow(before)), ]), as.list(before)),
  !any(approved %in% as.character(before$id_play)),
  sum(duplicated(paste(after$game_id, after$id_play))) == sum(duplicated(paste(before$game_id, before$id_play))),
  !any(as.character(after$id_play) == rejected))
games <- read.csv(file.path(out, "source_games.csv"), stringsAsFactors = FALSE)
games$kickoff <- parse_utc_datetime(games$kickoff)
game <- games[games$game_id == "401871054", ]
feature_before <- build_team_game_efficiencies_v2(g, game, .02)
feature_after <- build_team_game_efficiencies_v2(after[after$game_id == "401871054", ], game, .02)
stopifnot(identical(feature_before, feature_after))
saveRDS(after, pbp_path, compress = "xz")
csv(rows, "recovery_401871054/appended_non_scrimmage_rows.csv")
csv(feature_before, "recovery_401871054/team_features_before.csv")
csv(feature_after, "recovery_401871054/team_features_after.csv")
csv(data.frame(path = rank_files, md5 = rank_hashes), "ranking_artifact_hashes_before_pbp_completion.csv")

# Check real terminal plays; timeout sequence ordering can otherwise appear stale.
raw <- readRDS(file.path(out, "official_2026_raw.rds"))
latest <- games[games$week == 7L & raw_history_eligible(games), ]
last <- function(v) { v <- v[is.finite(v)]; if (length(v)) tail(v, 1) else NA_real_ }
terminal <- do.call(rbind, lapply(seq_len(nrow(latest)), function(i) {
  r <- latest[i, ]; p <- raw[as.character(raw$game_id) == as.character(r$game_id), ]
  p <- p[!p$play_type %in% c("Timeout", "End Period"), ]
  p <- p[order(p$game_play_number), ]
  h <- last(ifelse(p$pos_team == p$home, p$pos_team_score, p$def_pos_team_score))
  a <- last(ifelse(p$pos_team == p$away, p$pos_team_score, p$def_pos_team_score))
  original_h <- h; original_a <- a
  if (r$game_id == "401871054") {
    end <- rows[rows$play_type == "End of Game", ]
    h <- end$pos_team_score; a <- end$def_pos_team_score
  }
  data.frame(game_id = r$game_id, home = r$home, away = r$away, home_final = r$home_score, away_final = r$away_score,
    official_terminal_home = original_h, official_terminal_away = original_a,
    verified_terminal_home = h, verified_terminal_away = a, scores_match = h == r$home_score & a == r$away_score,
    source = if (r$game_id == "401871054") "School-corroborated ESPN ending; EPA unavailable" else "Official real-play terminal score; stale timeouts ignored")
}))
stopifnot(nrow(terminal) == 59L, all(terminal$scores_match))
csv(terminal, "week7_terminal_score_audit_resolved.csv")
note <- "Missouri State-Kennesaw State: two punts and the winning field goal were absent from the official EPA release. Six school-corroborated non-scrimmage events were appended with EPA unavailable; no EPA was invented. ESPN's unsupported extra Collins rush was rejected. Existing football features and numeric rankings are exactly unchanged."
coverage <- read.csv(file.path(out, "input_coverage.csv"), stringsAsFactors = FALSE)
j <- match("401871054", as.character(coverage$game_id))
coverage$pbp_rows[j] <- coverage$pbp_rows[j] + nrow(rows)
comp <- historical_competitive_plays(after[after$game_id == "401871054", ])
coverage$competitive_rows[j] <- nrow(comp)
coverage$source[j] <- paste("Official published EPA;", note)
csv(coverage, "input_coverage.csv")
info <- jsonlite::read_json(file.path(out, "input_manifest.json"), simplifyVector = TRUE)
info$selected_pbp_rows <- nrow(after)
info$selected_pbp_md5 <- unname(tools::md5sum(pbp_path))
info$non_epa_source_completion <- list(game_id = "401871054", appended_rows = nrow(rows), missing_special_teams_epa_plays = 3L,
  no_confirmed_missing_normal_offensive_snap = TRUE, features_identical = TRUE, note = note, checked_at = stamp)
json(info, "input_manifest.json")
acceptance <- jsonlite::read_json(file.path(out, "source_acceptance.json"), simplifyVector = TRUE)
acceptance$accepted_pbp_md5 <- info$selected_pbp_md5
acceptance$selected_pbp_rows <- nrow(after)
acceptance$non_epa_source_completion <- info$non_epa_source_completion
acceptance$week7_terminal_score_matches <- nrow(terminal)
acceptance$complete_special_teams_epa <- FALSE
json(acceptance, "source_acceptance.json")
qa <- jsonlite::read_json(file.path(out, "qa.json"), simplifyVector = TRUE)
qa$data_note <- paste(qa$data_note, note)
qa$non_epa_source_completion <- info$non_epa_source_completion
qa$week7_terminal_score_matches <- nrow(terminal)
qa$complete_special_teams_epa <- FALSE
json(qa, "qa.json")
terminal_qa <- list(status = "accepted_with_documented_missing_special_teams_epa", checked_at = stamp,
  game_id = "401871054", approved_append_ids = approved, rejected_unsupported_espn_id = rejected,
  school_source = "https://missouristatebears.com/sports/football/stats/2026/kennesaw-state/boxscore/7864",
  corroborating_school_source = "https://ksuowls.com/sports/football/stats/2026/missouri-state/boxscore/15432",
  espn_source = "https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=401871054",
  espn_sha256 = digest::digest(file = evidence_path, algo = "sha256"),
  missing_special_teams_epa_plays = 3L, no_epa_imputed = TRUE,
  comparison_of_espn_only_ids_to_school_finds_no_missing_normal_snap = TRUE,
  all_previous_selected_rows_unchanged = TRUE, team_game_features_exactly_identical = TRUE,
  previous_ranking_artifacts_unchanged = TRUE, protected_inputs_unchanged = TRUE,
  terminal_score_match_count = 59L, terminal_score_expected_count = 59L,
  selected_pbp_md5_before = unname(tools::md5sum(file.path(out, "selected_compact_before_pbp_completion.rds"))),
  selected_pbp_md5_after = info$selected_pbp_md5, note = note)
json(terminal_qa, "terminal_completion_qa.json")
hash_paths <- unique(c(old_hashes$path, file.path(out, "terminal_completion_qa.json"),
  file.path(out, "recovery_401871054", c("espn_summary.json", "school_ending_evidence.json", "appended_non_scrimmage_rows.csv")),
  file.path(root, "cfb_v3/complete_week8_terminal_sources.R")))
stopifnot(all(file.exists(hash_paths)))
csv(data.frame(path = hash_paths, md5 = unname(tools::md5sum(hash_paths))), "input_hashes.csv")
stopifnot(identical(unname(tools::md5sum(rank_files)), rank_hashes),
  identical(unname(tools::md5sum(protected$path)), protected$md5))
cat("Completion accepted: 6 non-scrimmage rows appended; 3 special EPA values explicitly missing; features and rankings unchanged; all 59 Week7 terminal scores verified.\n")
