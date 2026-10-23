# Week 5 article publication: fresh DraftKings quotes, frozen relative point forecasts.
# Run from the repository root: Rscript --vanilla cfb_v3/refresh_week5_relative_publication.R
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
for (module in c("config", "store", "features", "adapters", "dashboard"))
  source(file.path(root, "cfb_v2", paste0(module, ".R")))
source(file.path(root, "cfb_v3", "refresh_publication.R"))
if (file.exists(file.path(root, ".Renviron"))) readRenviron(file.path(root, ".Renviron"))
stopifnot(nzchar(Sys.getenv("CFBD_API_KEY")))
for (package in c("cfbfastR", "jsonlite"))
  if (!requireNamespace(package, quietly = TRUE)) stop("Missing package: ", package)
quarto_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin")
if (!nzchar(Sys.which("quarto")) && dir.exists(quarto_dir))
  Sys.setenv(PATH = paste(quarto_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
stopifnot(nzchar(Sys.which("quarto")))
say <- function(...) { cat(..., "\n"); flush.console() }

frozen_dir <- file.path(root, "cfb_v3/output/2026/week_5_relative_preliminary/20260930T211701Z")
source_files <- c(predictions = file.path(frozen_dir, "predictions.csv"),
  bundle = file.path(frozen_dir, "model.rds"), features = file.path(frozen_dir, "features.csv"))
hashes <- unname(tools::md5sum(source_files))
stopifnot(!anyNA(hashes))
card <- publication_read_csv(source_files[["predictions"]])
card$game_id <- as.character(card$game_id)
assert_unique_keys(card, "game_id", "Frozen Week 5 card")
features <- publication_align(publication_read_csv(source_files[["features"]]), card, "Frozen features")
bundle <- readRDS(source_files[["bundle"]])
frozen_time <- parse_utc_datetime(card$as_of)
stopifnot(nrow(card) == 54L, all(card$season == 2026), all(card$week == 5),
  all(is.finite(card$expected_margin)), all(card$forced_pick),
  all(is.na(card$margin_sd)), all(is.na(card$home_cover_probability)),
  all(is.na(card$home_win_probability)), !any(card$ats_calibrated),
  all(card$injury_scenario == "most_likely"),
  length(unique(frozen_time)) == 1L, !anyNA(frozen_time),
  abs(as.numeric(parse_utc_datetime(bundle$as_of)) - as.numeric(frozen_time[1])) < 1,
  all(card$straight_up_pick == ifelse(card$expected_margin >= 0, card$home, card$away)),
  all(grepl("opponent-relative", card$model_version, fixed = TRUE)))
old <- prepare_dashboard_predictions(card)

# One market request. No model fitting, player retrieval, probability calculation or inbox edits.
say("Fetching Week 5 DraftKings quotes for the frozen 54-game card.")
raw <- as.data.frame(cfbfastR::cfbd_betting_lines(year = 2026, week = 5), stringsAsFactors = FALSE)
captured_at <- Sys.time()
quotes <- publication_fresh_quotes(raw, card, captured_at)
stopifnot(nrow(quotes$lines) == 54L,
  identical(as.character(quotes$lines$game_id), card$game_id),
  all(format(quotes$kickoff, "%u", tz = "America/New_York") %in% c("5", "6")))
picks <- card
picks$market_home_spread <- quotes$lines$home_spread
picks$fair_spread <- -picks$expected_margin
picks$ats_edge_home <- picks$expected_margin + picks$market_home_spread
picks$ats_pick <- ifelse(picks$ats_edge_home >= 0, picks$home, picks$away)
picks$pick_status <- ifelse(abs(picks$market_home_spread) > bundle$config$ats$large_spread_review,
  "large_spread_review", "article_pick")
protected <- card$pick_status %in% c("injury_conflict_review", "transition_review")
picks$pick_status[protected] <- card$pick_status[protected]
picks$market_provider <- "DraftKings via CFBD"
picks$line_source <- "DraftKings"
picks$market_captured_at <- publication_utc(captured_at)
picks$market_total <- quotes$lines$total
picks$opening_home_spread <- quotes$lines$opening_home_spread
picks$draftkings_home_spread <- picks$market_home_spread
picks$fanduel_home_spread <- NA_real_
picks$best_home_spread <- picks$market_home_spread
picks$best_away_spread <- -picks$market_home_spread
picks$market_source <- "https://api.collegefootballdata.com/lines"
picks$schedule_source <- picks$market_source
picks$previous_market_home_spread <- card$market_home_spread
picks$previous_market_captured_at <- card$market_captured_at
picks$previous_ats_pick <- card$ats_pick
picks$market_change_note <- paste0("Previous: ", old$ats_pick_line, ". Refreshed: ",
  dashboard_pick_text(picks$ats_pick, picks$home, picks$market_home_spread), ".")
picks$kickoff <- publication_utc(quotes$kickoff)
picks$kickoff_display <- format(quotes$kickoff, "%a %m/%d %I:%M %p ET", tz = "America/New_York")
hour <- as.integer(format(quotes$kickoff, "%H", tz = "America/New_York"))
picks$primetime <- hour >= 19L | hour < 3L
picks$snapshot_type <- "opponent_relative_publication"
picks$publication_note <- paste0("Opponent-relative model saved ", publication_utc(frozen_time[1]),
  "; DraftKings lines retrieved ", publication_utc(captured_at),
  ". Frozen point forecasts; injuries not adjusted.")
# Empty confidence fields stay empty; this model supplies point forecasts only.
picks$confidence_tier <- NA_character_
picks$reported_confidence <- NA_character_
sdsu <- picks$home == "San Diego State" | picks$away == "San Diego State"
stopifnot(sum(sdsu) == 1L)
picks$availability_note <- ""
picks$availability_source <- ""
picks$availability_note[sdsu] <- paste0("San Diego State: mumps outbreak; county reported 14 confirmed cases ",
  "and one probable case as of Sept. 30. Availability changes are not included in this projection.")
picks$availability_source[sdsu] <- paste0("https://www.sandiegocounty.gov/content/sdc/hhsa/programs/",
  "phs/community_epidemiology/dc/Mumps/2026-local-mumps-situation.html")
picks <- prepare_dashboard_predictions(picks)
# The generic preparer fills absent confidence labels with "low"; do not export
# that default as a classification for a model that has none.
picks$confidence_tier <- NA_character_
picks$reported_confidence <- NA_character_
stopifnot(identical(picks$expected_margin, card$expected_margin),
  identical(picks$straight_up_pick, card$straight_up_pick),
  all(is.na(picks$margin_sd)), all(is.na(picks$home_cover_probability)),
  all(is.na(picks$home_win_probability)), all(is.na(picks$pick_cover_probability)),
  all(quotes$kickoff > Sys.time()), identical(unname(tools::md5sum(source_files)), hashes))

output <- file.path(root, "cfb_v3/output/2026/week_5_publication",
  format(captured_at, "%Y%m%dT%H%M%OS3Z", tz = "UTC"))
if (dir.exists(output)) stop("Refusing to overwrite publication: ", output)
dir.create(output, recursive = TRUE)
write_csv <- function(x, file) write.csv(x, file.path(output, file), row.names = FALSE, na = "")
write_csv(picks, "predictions.csv")
write_csv(quotes$lines, "market_snapshot.csv")
saveRDS(raw, file.path(output, "cfbd_lines_raw.rds"))
stopifnot(file.copy(source_files[["features"]], file.path(output, "features.csv")),
  file.copy(source_files[["bundle"]], file.path(output, "model.rds")))
file.copy(file.path(frozen_dir, "qa.json"), file.path(output, "frozen_model_qa.json"))
changes <- data.frame(game_id = picks$game_id, away = picks$away, home = picks$home,
  old_home_spread = card$market_home_spread, new_home_spread = picks$market_home_spread,
  home_line_change = picks$market_home_spread - card$market_home_spread,
  old_ats_pick = old$ats_pick_line, new_ats_pick = picks$ats_pick_line,
  side_changed = picks$ats_pick != card$ats_pick, expected_margin = picks$expected_margin,
  point_gap = picks$pick_edge, captured_at = picks$market_captured_at)
write_csv(changes, "line_changes.csv")
saved <- publication_read_csv(file.path(output, "predictions.csv"))
margin_error <- max(abs(saved$expected_margin - card$expected_margin))
stopifnot(margin_error < 1e-10, identical(as.character(saved$game_id), card$game_id),
  identical(unname(tools::md5sum(file.path(output, c("model.rds", "features.csv")))), hashes[2:3]))
manifest <- list(status = "rendering", built_at = publication_utc(Sys.time()),
  captured_at = publication_utc(captured_at), season = 2026L, week = 5L,
  predictions = nrow(picks), evening_games = sum(picks$primetime), model_refit = FALSE,
  model_family = "Uncapped opponent-relative point forecast; direct game-count predictors removed",
  frozen_model_as_of = publication_utc(frozen_time[1]), injuries_adjusted = FALSE,
  probability_policy = "Point forecasts only; no uncertainty or cover/win probabilities added.",
  quote_note = "Retrieval time, not provider last-update time. Spread juice is unavailable.",
  primetime_definition = "Kickoffs from 7 p.m. through 2:59 a.m. Eastern, including late-night games.",
  changed_spreads = sum(changes$home_line_change != 0), changed_sides = sum(changes$side_changed),
  validation = list(game_ids_unchanged = TRUE, straight_up_picks_unchanged = TRUE,
    maximum_margin_difference = margin_error, features_and_model_copied_byte_for_byte = TRUE,
    every_game_unstarted_at_capture = TRUE, source_inputs_unchanged = TRUE),
  sources = lapply(seq_along(source_files), function(i) list(role = names(source_files)[i],
    path = source_files[[i]], md5 = hashes[[i]])))
manifest_path <- file.path(output, "publication_manifest.json")
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, digits = 14)
say("PREDICTIONS:", file.path(output, "predictions.csv"))
say("Changed spreads:", manifest$changed_spreads, "; Changed ATS sides:", manifest$changed_sides)
print(changes[changes$side_changed, ], row.names = FALSE)
dashboard <- render_cfb_dashboard(file.path(output, "predictions.csv"), root, output, "dashboard.html")
stopifnot(identical(unname(tools::md5sum(source_files)), hashes))
manifest$status <- "complete"
manifest$dashboard <- basename(dashboard)
jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, digits = 14)
say("DASHBOARD:", dashboard)
