# Offline injury scenarios for a frozen prediction card. No network or model refit.
# Rscript --vanilla cfb_v3/apply_live_injuries.R --baseline=DIR_OR_CSV \
#   --registry=CSV --estimates=CSV --as-of=2026-10-07T20:00:00Z --output=NEW_DIR
# Estimates are optional. Finite adjustments require input_verified=TRUE; this
# verifies the supplied inputs, not the predictive accuracy of the estimate.
args <- commandArgs(trailingOnly = TRUE)
allowed <- c("baseline", "registry", "estimates", "as-of", "output")
if (any(!grepl("^--[^=]+=.+$", args))) stop("Use --name=value arguments.", call. = FALSE)
keys <- sub("^--([^=]+)=.*$", "\\1", args)
if (anyDuplicated(keys) || any(!keys %in% allowed)) stop("Unknown or repeated argument.", call. = FALSE)
options <- setNames(sub("^--[^=]+=", "", args), keys)
option <- function(name, default = NULL) if (name %in% names(options)) options[[name]] else default
for (name in c("baseline", "registry", "output")) {
  if (is.null(option(name))) stop("Missing --", name, ".", call. = FALSE)
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
stopifnot(length(script_arg) == 1L)
script <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
root <- dirname(dirname(script))
modules <- file.path(root, "cfb_v2", paste0(c("config", "store", "features", "dashboard"), ".R"))
for (path in modules) source(path)
helper <- file.path(root, "cfb_v3/injury_overlay.R")
source(helper)
for (package in c("jsonlite", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Required package unavailable: ", package)
}
utc <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
truth <- function(x) !is.na(x) & tolower(trimws(as.character(x))) %in% c("true", "1")
text <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }
read_table <- function(path) {
  header <- names(read.csv(path, nrows = 0L, check.names = FALSE))
  classes <- rep(NA_character_, length(header)); names(classes) <- header
  classes[intersect(c("game_id", "estimate_game_id", "player_id"), header)] <- "character"
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = classes,
           na.strings = c("", "NA"))
}
hash <- function(paths) vapply(paths, function(path) digest::digest(file = path, algo = "sha256"), "")
baseline_arg <- normalizePath(option("baseline"), winslash = "/", mustWork = TRUE)
baseline_csv <- if (dir.exists(baseline_arg)) {
  pristine <- file.path(baseline_arg, "unadjusted_predictions.csv")
  if (file.exists(pristine)) pristine else file.path(baseline_arg, "predictions.csv")
} else baseline_arg
baseline_csv <- normalizePath(baseline_csv, winslash = "/", mustWork = TRUE)
registry_path <- normalizePath(option("registry"), winslash = "/", mustWork = TRUE)
estimates_path <- if (is.null(option("estimates"))) character() else
  normalizePath(option("estimates"), winslash = "/", mustWork = TRUE)
output <- option("output")
if (!grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", output)) output <- file.path(getwd(), output)
if (file.exists(output) || dir.exists(output)) stop("Output must be a new directory: ", output)

as_of_text <- option("as-of", utc(Sys.time()))
if (!grepl("(Z|[+]00:00|[+]0000| UTC)$", as_of_text)) stop("--as-of must explicitly identify UTC.")
injury_as_of <- parse_utc_datetime(as_of_text)
if (length(injury_as_of) != 1L || is.na(injury_as_of)) stop("Invalid --as-of timestamp.")
card <- read_table(baseline_csv)
assert_columns(card, c("game_id", "season", "week", "home", "away", "expected_margin",
                       "market_home_spread", "as_of", "straight_up_pick", "ats_pick"), "baseline card")
if (!nrow(card) || any(!nzchar(text(card$game_id))) || anyDuplicated(card$game_id) ||
    any(!nzchar(text(card$home))) || any(!nzchar(text(card$away))) || any(card$home == card$away)) {
  stop("Baseline requires unique game IDs and valid home/away identities.")
}
if (any(!is.finite(card$expected_margin)) || any(!is.finite(card$season)) ||
    any(card$season < 1) || any(!is.finite(card$week)) || any(card$week < 1) ||
    length(unique(card$season)) != 1L) stop("Invalid baseline season, week, or point forecasts.")
baseline_times <- parse_utc_datetime(card$as_of)
if (anyNA(baseline_times) || any(baseline_times > injury_as_of)) stop("Injury cutoff precedes baseline data.")
if ("kickoff" %in% names(card)) {
  kickoff <- parse_utc_datetime(card$kickoff)
  if (anyNA(kickoff) || any(kickoff <= injury_as_of)) stop("Baseline includes games already started by the injury cutoff.")
}
model_candidates <- unique(c(file.path(dirname(baseline_csv), "model.rds"),
  sub("\\.csv$", "_model.rds", baseline_csv, ignore.case = TRUE)))
model_paths <- model_candidates[file.exists(model_candidates)]
bundle <- if (length(model_paths)) readRDS(model_paths[1]) else NULL
config <- if (!is.null(bundle$config)) bundle$config else cfb_v2_config(root, unique(card$season))
config$project_dir <- root
source_fit <- if (!is.null(bundle$source_fit) && file.exists(bundle$source_fit)) bundle$source_fit else character()
protected <- unique(c(baseline_csv, registry_path, estimates_path, model_paths, source_fit,
  file.path(dirname(baseline_csv), c("features.csv", "market_snapshot.csv"))))
protected <- protected[file.exists(protected)]
protected_hash <- hash(protected)
implementation <- unique(c(script, modules, helper,
  file.path(root, "cfb_v2/dashboard", c("dashboard.qmd", "dashboard.css", "dashboard-after-body.html"))))
implementation <- implementation[file.exists(implementation)]
implementation_hash <- hash(implementation)

registry <- read_table(registry_path)
reports <- resolve_season_availability(registry, card, injury_as_of)
if (!is.data.frame(reports)) stop("Availability resolver must return a data frame.")
if (nrow(reports)) assert_columns(reports, c("game_id", "team", "player_id", "player_name", "status"), "resolved availability")
# Points are always per-game estimates, never inherited from season-long status.
reports$impact_points <- rep(NA_real_, nrow(reports))
reports$input_verified <- rep(FALSE, nrow(reports))
reports$estimate_game_id <- rep(NA_character_, nrow(reports))
reports$baseline_as_of <- rep(NA_character_, nrow(reports))
reports$baseline_sha256 <- rep(NA_character_, nrow(reports))
reports$estimate_method <- rep(NA_character_, nrow(reports))
reports$estimate_source <- rep(NA_character_, nrow(reports))
reports$estimate_note <- rep(NA_character_, nrow(reports))
estimates <- data.frame()
join_key <- function(x) paste(text(x$game_id), text(x$team), text(x$player_id), sep = "\r")
if (length(estimates_path)) {
  estimates <- read_table(estimates_path)
  assert_columns(estimates, c("game_id", "team", "player_id", "impact_points"), "per-game estimates")
  if (nrow(estimates)) {
    if (any(!nzchar(text(estimates$game_id))) || any(!nzchar(text(estimates$team))) ||
        any(!nzchar(text(estimates$player_id))) || anyDuplicated(join_key(estimates))) {
      stop("Estimates require unique nonempty game/team/player keys.")
    }
    index <- match(join_key(estimates), join_key(reports))
    if (anyNA(index)) stop("An estimate has no exact resolved game/team/player match.")
    value <- suppressWarnings(as.numeric(estimates$impact_points))
    if (any(is.infinite(value)) || any(!is.na(estimates$impact_points) & is.na(value))) stop("Non-numeric or infinite impact_points.")
    verified <- if ("input_verified" %in% names(estimates)) truth(estimates$input_verified) else rep(FALSE, nrow(estimates))
    numeric_case <- is.finite(value) & verified
    if (any(numeric_case)) {
      assert_columns(estimates, c("estimate_game_id", "baseline_as_of", "baseline_sha256", "estimate_method", "estimate_source"), "verified per-game estimates")
      if (any(text(estimates$estimate_game_id[numeric_case]) != text(estimates$game_id[numeric_case])) ||
          any(!nzchar(text(estimates$estimate_method[numeric_case]))) || any(!nzchar(text(estimates$estimate_source[numeric_case])))) {
        stop("Verified estimates need the exact game ID, method, and source.")
      }
      estimated_at <- parse_utc_datetime(estimates$baseline_as_of[numeric_case])
      frozen_at <- baseline_times[match(estimates$game_id[numeric_case], card$game_id)]
      if (anyNA(estimated_at) || any(estimated_at != frozen_at)) stop("Estimate baseline_as_of does not match frozen card.")
      if (any(tolower(text(estimates$baseline_sha256[numeric_case])) != unname(hash(baseline_csv))))
        stop("Estimate baseline_sha256 does not match the exact frozen prediction file.")
    }
    for (name in intersect(c("estimate_game_id", "baseline_as_of", "baseline_sha256", "estimate_method", "estimate_source", "estimate_note"), names(estimates)))
      reports[[name]][index] <- as.character(estimates[[name]])
    if (!"estimate_note" %in% names(estimates) && "note" %in% names(estimates)) reports$estimate_note[index] <- as.character(estimates$note)
    reports$input_verified[index] <- verified
    reports$impact_points[index] <- ifelse(numeric_case, value, NA_real_)
    invalid <- !numeric_case
    if (any(invalid)) reports$estimate_note[index[invalid]] <- paste(
      text(reports$estimate_note[index[invalid]]), "No verified finite per-game estimate; availability review only.")
  }
}
full <- apply_injury_overlay(card, reports, injury_as_of, config)
half_reports <- reports
half_reports$impact_points <- half_reports$impact_points * .5
half <- apply_injury_overlay(card, half_reports, injury_as_of, config)
if (!all(c("predictions", "reports", "audit") %in% names(full))) stop("Invalid injury helper result.")
full_card <- full$predictions; half_card <- half$predictions
for (result in list(full_card, half_card)) {
  if (!identical(as.character(result$game_id), as.character(card$game_id)) ||
      !identical(as.character(result$home), as.character(card$home)) ||
      !identical(as.character(result$away), as.character(card$away)) ||
      any(!is.finite(result$expected_margin))) stop("Overlay changed slate identity or produced a nonfinite forecast.")
}
full_shift <- full_card$expected_margin - card$expected_margin
half_shift <- half_card$expected_margin - card$expected_margin
if (any(abs(half_shift - .5 * full_shift) > 1e-9)) stop("Half sensitivity does not reproduce half the signed shift.")
reviewed_ids <- if (nrow(reports)) unique(as.character(reports$game_id)) else character()
if (any(abs(full_shift[!card$game_id %in% reviewed_ids]) > 1e-9)) stop("An unreviewed game was adjusted.")
market_columns <- grep("spread|market|line_source|opening|draftkings|fanduel", names(card), value = TRUE)
market_columns <- setdiff(market_columns, c("fair_spread", "market_change_note"))
preserve_market <- function(result) {
  for (name in market_columns) {
    before <- card[[name]]; after <- result[[name]]
    if (!isTRUE(all.equal(before, after, check.attributes = FALSE, tolerance = 0))) {
      # Dashboard decoration turns absent text (CSV logical NA) into "".
      # Permit only that empty-cell representation change; every populated
      # cell must still match exactly, then restore the entire frozen column.
      blank <- (is.na(before) | as.character(before) == "") &
        (is.na(after) | as.character(after) == "")
      if (length(after) != length(before) ||
          (any(!blank) && !isTRUE(all.equal(before[!blank], after[!blank],
             check.attributes = FALSE, tolerance = 0))))
        stop("Overlay changed frozen market field: ", name)
    }
    result[[name]] <- before
  }
  result
}
full_card <- preserve_market(full_card)
half_card <- preserve_market(half_card)
for (name in intersect(c("margin_sd", "home_win_probability", "home_cover_probability", "pick_cover_probability"), names(full_card)))
  full_card[[name]] <- NA_real_
full_card$confidence_tier <- NA_character_; full_card$reported_confidence <- NA_character_
full_card$ats_calibrated <- FALSE
full_card$injury_baseline_margin <- card$expected_margin
full_card$injury_full_margin <- full_card$expected_margin
full_card$injury_half_margin <- half_card$expected_margin
full_card$injury_half_ats_pick <- half_card$ats_pick
full_card$injury_half_straight_up_pick <- half_card$straight_up_pick
full_card$injury_as_of <- utc(injury_as_of)
review_labels <- if (nrow(reports)) unique(paste(text(reports$player_name), paste0("(", text(reports$team), ": ", text(reports$status), ")"))) else "none"
review_summary <- paste(review_labels, collapse = "; ")
absence_labels <- if (any(reports$status == "out")) unique(paste(text(reports$player_name[reports$status == "out"]),
  paste0("(", text(reports$team[reports$status == "out"]), ")"))) else "none"
original_notes <- if ("publication_note" %in% names(card)) text(card$publication_note) else rep("", nrow(card))
retained_note <- vapply(strsplit(original_notes, "|", fixed = TRUE), function(parts) {
  parts <- trimws(parts)
  parts <- parts[nzchar(parts) & !grepl("injuries are not adjusted", parts, ignore.case = TRUE)]
  paste(parts, collapse = " | ")
}, "")
full_card$publication_note <- paste0(retained_note, ifelse(nzchar(retained_note), " | ", ""),
  "Confirmed absences reviewed: ", paste(absence_labels, collapse = "; "),
  " | Usage/EPA replacement scenarios; other injuries not comprehensively screened")
full_card <- prepare_dashboard_predictions(full_card)
full_card <- preserve_market(full_card)
full_card$confidence_tier <- NA_character_; full_card$reported_confidence <- NA_character_

scenario_rows <- function(label, predictions, multiplier) data.frame(
  game_id = card$game_id, season = card$season, week = card$week, home = card$home, away = card$away,
  scenario = label, multiplier = multiplier, reviewed = card$game_id %in% reviewed_ids,
  baseline_expected_margin = card$expected_margin, expected_margin = predictions$expected_margin,
  signed_home_margin_shift = predictions$expected_margin - card$expected_margin,
  market_home_spread = card$market_home_spread,
  ats_edge_home = predictions$expected_margin + card$market_home_spread,
  straight_up_pick = predictions$straight_up_pick, ats_pick = predictions$ats_pick,
  baseline_as_of = card$as_of, injury_as_of = utc(injury_as_of), stringsAsFactors = FALSE)
scenarios <- rbind(scenario_rows("baseline", card, 0), scenario_rows("half_replacement_sensitivity", half_card, .5),
                   scenario_rows("full_replacement_scenario", full_card, 1))
comparison <- data.frame(game_id = card$game_id, home = card$home, away = card$away,
  reviewed = card$game_id %in% reviewed_ids, market_home_spread = card$market_home_spread,
  baseline_margin = card$expected_margin, half_margin = half_card$expected_margin, full_margin = full_card$expected_margin,
  half_signed_shift = half_shift, full_signed_shift = full_shift,
  baseline_su_pick = card$straight_up_pick, half_su_pick = half_card$straight_up_pick, full_su_pick = full_card$straight_up_pick,
  baseline_ats_pick = card$ats_pick, half_ats_pick = half_card$ats_pick, full_ats_pick = full_card$ats_pick,
  baseline_as_of = card$as_of, injury_as_of = utc(injury_as_of), stringsAsFactors = FALSE)
stopifnot(identical(hash(protected), protected_hash))
if (!dir.create(output, recursive = TRUE, showWarnings = FALSE)) stop("Could not create new output directory.")
output <- normalizePath(output, winslash = "/", mustWork = TRUE)
csv <- function(value, name) write.csv(value, file.path(output, name), row.names = FALSE, na = "")
json <- function(value, name) jsonlite::write_json(value, file.path(output, name), pretty = TRUE, auto_unbox = TRUE, digits = 16, na = "null", null = "null")
csv(full_card, "predictions.csv")
stopifnot(file.copy(baseline_csv, file.path(output, "baseline_control.csv"), overwrite = FALSE))
csv(comparison, "comparison.csv"); csv(scenarios, "scenarios.csv")
csv(full$reports, "injury_reports.csv"); csv(full$audit, "injury_audit.csv")
if (length(estimates_path)) csv(estimates, "supplied_estimates.csv")
source_hashes <- data.frame(path = c(protected, implementation), sha256 = c(unname(protected_hash), unname(implementation_hash)),
  role = c(rep("protected_input", length(protected)), rep("implementation", length(implementation))), stringsAsFactors = FALSE)
csv(source_hashes, "source_hashes.csv")
qa <- list(status = "predictions_complete", predictions = nrow(card), reviewed_games = length(reviewed_ids),
  resolved_player_cases = nrow(reports), games_with_nonzero_point_shift = sum(abs(full_shift) > 1e-9),
  frozen_market_preserved = TRUE, baseline_identity_preserved = TRUE, half_shift_exact = TRUE,
  unreviewed_margins_unchanged = TRUE, source_hashes_unchanged = TRUE, model_refit = FALSE,
  production_model_changed = FALSE, public_site_changed = FALSE, all_slate_injuries_screened = FALSE,
  source_baseline = baseline_csv, baseline_sha256 = unname(hash(baseline_csv)),
  injury_as_of = utc(injury_as_of), generated_at = utc(Sys.time()),
  method_policy = "Main card uses supplied verified-input full replacement scenarios. Half is an arithmetic sensitivity, not a calibrated estimate or confidence bound.")
json(qa, "qa.json")
manifest <- list(created_at = utc(Sys.time()), baseline_csv = baseline_csv,
  baseline_sha256 = unname(hash(baseline_csv)), baseline_model_paths = model_paths,
  source_fit = source_fit, registry = registry_path, estimates = estimates_path, injury_as_of = utc(injury_as_of),
  baseline_as_of = unique(card$as_of), cutoff_policy = "Each estimate references this exact baseline timestamp; evidence is resolved at the separate injury cutoff.",
  selected_scenario = "full_replacement_scenario", multipliers = c(0, .5, 1),
  reviewed_players = review_labels, comprehensive_screening = FALSE,
  availability_policy = "Season-long availability persists; numerical impacts require explicit verified per-game estimates. Registry impacts are never carried forward.",
  probability_policy = "No calibrated injury effect, cover probability, or confidence rating is supplied.",
  model_refit = FALSE, source_hashes = source_hashes)
json(manifest, "manifest.json")
md <- function(x) gsub("|", "\\|", text(x), fixed = TRUE)
focus <- comparison[comparison$reviewed, ]
table <- if (nrow(focus)) c("| Game | Baseline home margin | Half sensitivity | Full scenario | Baseline ATS | Full ATS |",
  "|---|---:|---:|---:|---|---|", sprintf("| %s at %s | %+.2f | %+.2f | %+.2f | %s | %s |",
  md(focus$away), md(focus$home), focus$baseline_margin, focus$half_margin, focus$full_margin, md(focus$baseline_ats_pick), md(focus$full_ats_pick))) else "No availability cases matched this frozen slate."
writeLines(c("# Injury replacement scenarios", "", paste("Evidence cutoff:", utc(injury_as_of)), "",
  paste("Reviewed:", review_summary), "", table, "",
  "Margins use the home team's perspective. Full raw replacement scenarios drive the main card only when a verified-input, game-specific estimate was supplied. Half is a sensitivity, not a confidence interval or learned default. Missing estimates remain availability review only.", "",
  "The baseline already reflects actual prior player availability. A season-long absence does not justify repeating an unchanged penalty after replacement performance enters team history. Other slate injuries have not been comprehensively screened.", "",
  paste0("[Frozen baseline](", baseline_csv, ") · [Dashboard](dashboard.html) · [Side-by-side comparison](comparison.csv) · [All scenarios](scenarios.csv) · [Injury audit](injury_audit.csv) · [Source hashes](source_hashes.csv)")),
  file.path(output, "injury_report.md"), useBytes = TRUE)
quarto_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin")
if (dir.exists(quarto_dir)) Sys.setenv(PATH = paste(quarto_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
dashboard <- render_cfb_dashboard(file.path(output, "predictions.csv"), root, output, "dashboard.html")
stopifnot(identical(hash(protected), protected_hash))
qa$status <- "complete"; qa$rendered_at <- utc(Sys.time()); qa$dashboard <- dashboard
json(qa, "qa.json")
cat("DASHBOARD:", dashboard, "\nReviewed games:", length(reviewed_ids), "; changed margins:", qa$games_with_nonzero_point_shift, "\n")
