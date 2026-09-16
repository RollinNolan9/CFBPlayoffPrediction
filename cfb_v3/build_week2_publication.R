# Rscript cfb_v3/build_week2_publication.R <explicit market snapshot directory>
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply the directory containing the refreshed market snapshot.")
project <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
for (file in c("config.R", "features.R", "models.R", "dashboard.R")) source(file.path("cfb_v2", file))
source("cfb_v3/publication.R")
read_csv <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                                   na.strings = c("", "NA"))
base_dir <- "cfb_v3/output/2026/week_2_preliminary/20260909T032636Z"
card_path <- file.path(base_dir, "predictions.csv")
feature_path <- "cfb_v3/output/2026/week_2/live_live_2026_w02_20260909T005125Z_features.csv"
bundle_path <- "cfb_v3/output/2026/week_2/live_live_2026_w02_20260909T005125Z_model.rds"
evidence_path <- "cfb_v3/output/experiments/early_spread_audit/20260909T221201Z/week_2_review_queue.csv"
context_path <- "cfb_v3/week2_publication_context.json"
card <- read_csv(card_path)
features <- read_csv(feature_path)
evidence <- read_csv(evidence_path)
bundle <- readRDS(bundle_path)
lines <- read_csv(file.path(out, "market_snapshot.csv"))
raw_lines <- readRDS(file.path(out, "cfbd_lines_raw.rds"))
stopifnot(nrow(card) == 49L, all(card$season == 2026L), all(card$week == 2L))
lines <- lines[tolower(gsub(" ", "", lines$provider)) == "draftkings" &
                 lines$game_id %in% card$game_id, , drop = FALSE]
raw_lines <- raw_lines[tolower(gsub(" ", "", raw_lines$provider)) == "draftkings" &
                         raw_lines$id %in% card$game_id, , drop = FALSE]
stopifnot(!anyDuplicated(raw_lines$id))
raw_lines <- raw_lines[match(card$game_id, raw_lines$id), ]
stopifnot(identical(card$home, canonical_team(raw_lines$homeTeam)),
          identical(card$away, canonical_team(raw_lines$awayTeam)), all(raw_lines$season == 2026L))
align <- function(x) {
  stopifnot(!anyDuplicated(x$game_id))
  hit <- match(card$game_id, x$game_id)
  stopifnot(!anyNA(hit))
  x <- x[hit, , drop = FALSE]
  stopifnot(identical(x$home, card$home), identical(x$away, card$away))
  x
}
features <- align(features)
evidence <- align(evidence)
picks <- reprice_publication_card(card, lines, bundle)
context <- jsonlite::fromJSON(context_path)
meta <- context$games
stopifnot(!anyDuplicated(meta$game_id), all(meta$game_id %in% picks$game_id))
picks$kickoff <- raw_lines$startDate
for (i in which(!is.na(meta$kickoff_override))) {
  picks$kickoff[picks$game_id == meta$game_id[i]] <- meta$kickoff_override[i]
}
kickoff <- as.POSIXct(picks$kickoff, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
captured <- as.POSIXct(picks$market_captured_at, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
stopifnot(!anyNA(kickoff), !anyNA(captured), all(captured < kickoff))
picks$kickoff_display <- format(kickoff, "%a %m/%d %I:%M %p ET", tz = "America/New_York")
picks$primetime <- picks$game_id %in% meta$game_id
picks$broadcast <- ""
picks$schedule_source <- ""
picks$availability_note <- ""
picks$availability_source <- ""
for (i in seq_len(nrow(meta))) {
  j <- match(meta$game_id[i], picks$game_id)
  picks$broadcast[j] <- meta$broadcast[i]
  picks$schedule_source[j] <- context$sources[[meta$source[i]]]
  if (!is.na(meta$availability_note[i])) {
    picks$availability_note[j] <- meta$availability_note[i]
    picks$availability_source[j] <- meta$availability_source[i]
  }
}
picks$home_efficiency_source_games <- evidence$home_efficiency_source_games
picks$away_efficiency_source_games <- evidence$away_efficiency_source_games
picks$evidence_note <- vapply(seq_len(nrow(picks)), function(i) {
  teams <- c(picks$home[i], picks$away[i])
  counts <- c(picks$home_efficiency_source_games[i], picks$away_efficiency_source_games[i])
  if (anyNA(counts)) return("Current FBS efficiency sample count unavailable.")
  if (!any(counts == 0)) return("")
  paste0(paste(teams[counts == 0], collapse = " and "),
    ": no current FBS efficiency sample; prior-year efficiency fallback. FCS scores still inform power at reduced weight.")
}, character(1))
picks$publication_note <- paste(
  "Margins frozen Sept 9; quotes retrieved Sept 11. CFBD does not supply book-update times or ATS juice.",
  "Availability notes are not point adjustments. Article sides are not validated best bets."
)
picks <- prepare_dashboard_predictions(picks)
stopifnot(nrow(picks) == 49L, sum(picks$primetime) == 22L,
          identical(picks$expected_margin, card$expected_margin))
write.csv(picks, file.path(out, "predictions.csv"), row.names = FALSE, na = "")
jsonlite::write_json(picks[, c("game_id", "game_label", "market_line", "model_line",
  "ats_pick_line", "straight_up_pick", "pick_edge", "primetime", "kickoff_display")],
  file.path(out, "verification_expected.json"), auto_unbox = TRUE, pretty = TRUE)
comparison <- data.frame(
  game_id = card$game_id, away = card$away, home = card$home,
  old_home_spread = card$market_home_spread, new_home_spread = picks$market_home_spread,
  home_line_change = picks$market_home_spread - card$market_home_spread,
  old_ats_pick = card$ats_pick_line, new_ats_pick = picks$ats_pick_line,
  side_changed = picks$ats_pick != card$ats_pick, expected_margin = picks$expected_margin,
  point_gap = picks$pick_edge, captured_at = picks$market_captured_at
)
write.csv(comparison, file.path(out, "line_changes.csv"), row.names = FALSE)
file.copy(file.path(base_dir, "feature_importance.png"), out, overwrite = TRUE)

pretty_team <- function(x) {
  aliases <- c("Southern California" = "USC", "Mississippi" = "Ole Miss", "Hawai'i" = "Hawaii")
  ifelse(x %in% names(aliases), aliases[x], x)
}
md <- c("# Week 2 Primetime Brief", "",
  "September 11-12, 2026 | 22 evening games in the 49-game model card", "",
  paste0("**DraftKings quotes via CFBD retrieved ",
    format(captured[1], "%b %d, %I:%M %p ET", tz = "America/New_York"),
    ".** Margins and straight-up picks remain frozen from September 9. All 49 sides were repriced; ",
    sum(comparison$home_line_change != 0), " spreads changed and ",
    sum(comparison$side_changed), " ATS sides flipped."), "",
  "Point gap means model disagreement with the spread, not a calibrated probability or demonstrated betting advantage. ATS juice is unavailable. These are requested article selections, not validated best bets. The short explanations interpret the saved feature contributions; they are not causal claims or a fresh injury-adjusted simulation.", "",
  "Scope: all Friday/Saturday evening kickoffs from 7 p.m. Eastern in this card, including streaming games. Late-night games are separate; Hawaii's Saturday evening kickoff is midnight Sunday ET. Other FCS matchups outside the model card are not covered. Schedule links appear with each game.", "")
indices <- which(picks$primetime)
indices <- indices[order(kickoff[indices])]
last_group <- ""
for (i in indices) {
  row <- picks[i, ]
  m <- meta[match(row$game_id, meta$game_id), ]
  day <- format(kickoff[i], "%Y-%m-%d", tz = "America/New_York")
  hour <- as.integer(format(kickoff[i], "%H", tz = "America/New_York"))
  group <- if (day == "2026-09-11") "Friday Night" else
    if (hour >= 22L || hour == 0L) "Saturday Late Night" else "Saturday Primetime"
  if (group != last_group) md <- c(md, paste0("## ", group), "")
  last_group <- group
  md <- c(md,
    paste0("### ", pretty_team(row$away), " at ", pretty_team(row$home)), "",
    paste0("[", row$kickoff_display, " | ", row$broadcast, "](", row$schedule_source, ")"), "",
    paste0("**ATS: ", row$ats_pick_line, " | Projection: ",
      pretty_team(row$straight_up_pick), " by ", sprintf("%.1f", abs(row$expected_margin)),
      " | Point gap: ", sprintf("%.1f", row$pick_edge), ".**"), "", m$summary, "")
  if (nzchar(row$availability_note)) {
    md <- c(md, paste0("**Availability:** ", row$availability_note,
      " [Report](", row$availability_source, ").",
      if (!is.na(m$additional_source)) paste0(" [Kansas report](", m$additional_source, ").") else ""), "")
  }
  if (nzchar(row$evidence_note)) md <- c(md, paste0("*Data caution:* ", row$evidence_note), "")
}
writeLines(md, file.path(out, "primetime_summary.md"), useBytes = TRUE)
manifest <- list(
  built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  model_refit = FALSE, injury_adjustments_applied = FALSE,
  quote_note = "Retrieval time, not provider last-update time. No spread juice or FanDuel returned.",
  predictions = 49L, evening_games = 22L, changed_spreads = sum(comparison$home_line_change != 0),
  changed_sides = sum(comparison$side_changed),
  source_md5 = as.list(tools::md5sum(c(card_path, feature_path, bundle_path, evidence_path,
    context_path, file.path(out, "market_snapshot.csv"), file.path(out, "cfbd_lines_raw.rds"))))
)
jsonlite::write_json(manifest, file.path(out, "publication_manifest.json"), pretty = TRUE, auto_unbox = TRUE)
quarto_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin")
Sys.setenv(PATH = paste(quarto_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
cat(render_cfb_dashboard(file.path(out, "predictions.csv"), project, out), "\n")
summary_html <- paste0("<!doctype html><html lang='en'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'><title>Week 2 Primetime Brief</title>",
  "<style>body{font:16px/1.6 system-ui,sans-serif;color:#24323a;max-width:850px;margin:36px auto;padding:0 22px;letter-spacing:0}",
  "h1{font-size:30px}h2{font-size:23px;border-top:2px solid #17745a;padding-top:24px;margin-top:36px}",
  "h3{font-size:18px;margin-bottom:4px}p{margin:10px 0}a{color:#225b8a}strong{font-weight:650}",
  "@media print{h2,h3{break-after:avoid}body{font-size:11pt;margin:0}}",
  "</style></head><body>", commonmark::markdown_html(paste(md, collapse = "\n")), "</body></html>")
writeLines(summary_html, file.path(out, "primetime_summary.html"), useBytes = TRUE)
cat("Publication complete:", nrow(picks), "games;", sum(picks$primetime), "evening summaries.\n")
cat("Changed spreads:", sum(comparison$home_line_change != 0), "Changed sides:", sum(comparison$side_changed), "\n")
