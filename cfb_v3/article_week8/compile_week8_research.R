# Week 8 full-slate injury research log (Thursday-night refresh) and the Pitt at Miami review.
root <- normalizePath(getwd(), winslash = "/")
research <- normalizePath(file.path(root, "../outputs/article-week-8/research"), winslash = "/")
scen <- file.path(research, "scenarios")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, dir.exists(args[1]))
injury_dir <- normalizePath(args[1], winslash = "/")
read <- function(p, ...) read.csv(p, stringsAsFactors = FALSE, ...)
card <- read(file.path(injury_dir, "predictions.csv"), colClasses = c(game_id = "character"))
src <- read(file.path(research, "game_sources.csv"), colClasses = c(game_id = "character"))
usage <- read(file.path(research, "official_availability_usage.csv"), colClasses = c(game_id = "character"))
leads <- read(file.path(research, "unofficial_notes.csv"))
qb <- read(file.path(scen, "qb_replacement_estimates_raw.csv"), colClasses = c(estimate_game_id = "character"))
est <- read(file.path(scen, "week8_qb_estimates.csv"), colClasses = c(game_id = "character"))
registry <- read(file.path(scen, "week8_player_availability.csv"), colClasses = c(player_id = "character"))
stopifnot(nrow(card) == 49L, setequal(card$game_id, src$game_id))
utc <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
reviewed_at <- utc(Sys.time())
retrieval <- read(file.path(research, "kickoffstatus/_retrieval_log.csv"))

# Narrative decisions for cases that need more than the official designation.
decisions <- list(
  "401858495" = list(summary = "Both starting QBs out. Illinois: Katin Houser (broken throwing hand) Out; Michael Clayton II named starter (Ducks Wire Oct 14; Yahoo Oct 22). Oregon: Dante Moore Out (concussion protocol, 3rd straight game); Dylan Raiola expected starter (On3, Ducks Wire Oct 22). Oregon WR Evan Stewart and Jeremiah McClellan Out; Illinois RB Ca'Lil Valentine Out.",
    priced = "Joint QB estimate: +3.60 net points against Illinois (Houser loss 3.80 minus Moore loss 0.20).",
    open = c("Raiola is 'expected'; Oregon has not officially named a starter.", "Clayton sample is 8 EPA plays from one relief game."),
    sources = c("https://duckswire.usatoday.com/story/sports/college/ducks/football/2026/10/14/oregon-football-illinois-katin-houser-injury-michael-clayton-ii/92250852007/",
      "https://www.on3.com/news/oregon-expected-to-start-qb-dylan-raiola-at-illinois/",
      "https://duckswire.usatoday.com/story/sports/college/ducks/football/2026/10/22/oregon-football-dylan-raiola-starter-qb-illinois/92385084007/")),
  "401858499" = list(summary = "UCLA Nico Iamaleava Out (season-ending knee surgery); Ty Dieffenbach second start (AP, UCLA release). Michigan State QB Alessio Milivojevic and RB Jaziun Patterson Questionable; QB2 Cam Fancher Out, so freshman Kayd Coffman would start if Milivojevic sits.",
    priced = "UCLA QB estimate: -4.87 (replacement scenario raises UCLA's margin).",
    open = c("Milivojevic Questionable: material; no Coffman FBS sample (3 rushes), so no unavailable scenario."),
    sources = c("https://apnews.com/article/michigan-state-spartans-ucla-bruins-7ff133e52b976ad28ad3fa28ccd052bb",
      "https://www.si.com/college/michiganstate/football/status-msu-spartans-qb-alessio-milivojevic-in-the-air-for-ucla-game")),
  "401858274" = list(summary = "Pitt Mason Heintschel Out (torn ACL, season). CBS (Zenitz) via SI/On3 Oct 22: Holden Geriner expected to start, Corey Dailey expected to see time; Pitt has not announced. See scenarios/pitt/pitt_at_miami_review.json.",
    priced = "Pitt QB estimate (Geriner starts): +7.79 against Pitt. Dailey-only alternative +2.51; half scenario +3.89.",
    open = c("Starter unannounced and a two-QB plan is possible; keep out of featured picks."),
    sources = c("https://www.si.com/college/pittsburgh/football/pitt-panthers-make-qb-change-miami-game-holden-geriner",
      "https://www.on3.com/sites/pitt-sports-news/news/report-pitt-lands-on-starting-qb-vs-no-1-miami/",
      "https://pittsburghpanthers.com/news/2026/10/19/football-narduzzi-notebook-pitt-leans-on-effort-ahead-of-no-1-miami")),
  "401864525" = list(summary = "Air Force Liam Szarka Out (season, lower leg); Evan Medders third start in the triple option (Air Force game notes Oct 19; Gazette Oct 5, Oct 21).",
    priced = "Air Force QB estimate: -3.17 (replacement scenario raises Air Force's margin).",
    open = character(),
    sources = c("https://gazette.com/2026/10/05/air-force-qb-liam-szarka-out-for-the-rest-of-the-year-with-injury-3/",
      "https://goairforcefalcons.com/documents/download/2026/10/19/2026wyofbgamenotevsAirForce.pdf")),
  "401858275" = list(summary = "Syracuse Malachi Nelson Out (high-ankle tight-rope surgery) and Amari Odom Out (season). Fran Brown named Steve Angeli the starter Thursday night (syracuse.com, Yahoo) but the ACC report still lists Angeli Questionable; Danny Lauter starts if Angeli cannot.",
    priced = NULL,
    open = c("Angeli Questionable: Nelson-to-Angeli scenario +0.68 against Syracuse computed but not applied (box counts differ from CFBD by 1 play in 3 games)."),
    sources = c("https://www.syracuse.com/orangefootball/2026/10/syracuses-qb-picture-becomes-more-clear-as-fran-brown-names-starter-for-north-carolina-game.html")),
  "401871093" = list(summary = "Old Dominion Quinn Henicle Out (Sun Belt report); Ryan Huff Questionable; Jaxon Potter next (RotoWire Oct 21-22).",
    priced = NULL,
    open = c("Huff Questionable: Henicle-to-Huff scenario +2.60 against ODU computed but not applied; Potter has 5 career pass attempts."),
    sources = c("https://www.rotowire.com/cfootball/headlines/quinn-henicle-injury-will-not-play-saturday-236145",
      "https://www.rotowire.com/cfootball/headlines/ryan-huff-injury-status-in-question-versus-louisiana-tech-236301")),
  "401858273" = list(summary = "Boston College RB Evan Dickens Out (new; 31% of BC FBS competitive rushes). No replacement estimate.",
    priced = NULL, open = character(), sources = character()),
  "401856730" = list(summary = "Mississippi State QB Kamario Taylor Questionable (turf toe; missed Week 7). On3 Oct 22: 'more optimism', decision expected near kickoff. RB Fluff Bothwell Questionable. Oklahoma has six out, including OL Eddy Pierre-Louis and Ryan Fodje.",
    priced = NULL, open = c("Taylor Questionable: 79% of MSU dropbacks; Puckett started Week 7 (31-0 loss). Keep out of featured picks until Saturday's report."),
    sources = c("https://www.on3.com/news/mississippi-state-qb-kamario-taylor-remains-questionable-for-oklahoma-but-optimism-growing/")),
  "401860905" = list(summary = "San Diego State QB Jayden Denegal Questionable (ankle, left Fresno State game); Stone Saunders probable and would start.",
    priced = NULL, open = c("Denegal Questionable: material; keep out of featured picks until Friday's report."),
    sources = c("https://www.rotowire.com/cfootball/headlines/jayden-denegal-injury-listed-as-questionable-for-week-8-236240")),
  "401856839" = list(summary = "Kansas State QB Avery Johnson Questionable (shoulder/neck, Thursday update unchanged); Jacob Knuth likely backup. Arizona State expected to start Mikey Keene.",
    priced = NULL, open = c("Johnson Questionable: 97% of K-State dropbacks; Knuth has 3 career passes."),
    sources = c("https://sports.yahoo.com/articles/k-state-qb-avery-johnson-020227501.html"))
)

players <- lapply(seq_len(nrow(usage)), function(i) {
  u <- usage[i, ]; s <- src[src$slug == u$slug, ][1, ]
  stamp <- sub("^Latest: ", "", s$latest_report)
  list(team = u$model_team, player_name = u$player_name, position = u$position, status = u$status, source_status = u$status,
    game_id = u$game_id, source_id = paste0("kickoffstatus:", u$slug), source_url = s$official_url, mirror_url = s$page_url,
    source_timestamp_or_date = stamp, retrieved_at = s$retrieved_at,
    usage = list(dropback_share = u$dropback_share, rush_share = u$rush_share, target_share = u$target_share,
      last_usage_week = u$last_usage_week, absence_new = u$absence_new, material_by_usage_screen = u$material),
    temporal_class = "current_game",
    numeric_adjustment = {
      r <- registry[registry$player_name == u$player_name & registry$team == u$model_team, ]
      e <- if (nrow(r)) est[est$player_id == r$player_id[1] & est$game_id == u$game_id, ] else est[0, ]
      if (nrow(e)) e$impact_points else NULL
    })
})
games <- lapply(seq_len(nrow(card)), function(i) {
  p <- card[i, ]; s <- src[src$game_id == p$game_id, ]; stopifnot(nrow(s) == 1L)
  u <- usage[usage$game_id == p$game_id, ]
  l <- leads[leads$slug == s$slug, ]
  report_state <- sub(" posted.*$", "", sub("^Latest: ", "", s$latest_report))
  coverage_status <- switch(report_state,
    Update = "current_official_report_plus_context", Initial = "initial_official_report_only_plus_context",
    Pending = "official_report_not_yet_posted_unofficial_leads_only")
  d <- decisions[[p$game_id]]
  material <- u[u$material %in% TRUE & u$status %in% c("Out", "Doubtful", "Questionable"), ]
  list(game_id = p$game_id, away = p$away, home = p$home, kickoff_utc = p$kickoff,
    coverage = list(teams_reviewed = c(p$away, p$home), coverage_status = coverage_status, official_report_state = s$latest_report,
      official_source = s$official_url, mirror = s$page_url, mirror_retrieved_at = s$retrieved_at,
      official_rows = nrow(u), status_counts = as.list(table(factor(u$status, c("Out", "Doubtful", "Questionable", "Probable", "Available", "Not listed")))),
      unofficial_leads = nrow(l), summary = if (!is.null(d)) d$summary else NULL,
      context_sources = if (!is.null(d)) d$sources else character()),
    material_reported = lapply(seq_len(nrow(material)), function(j) list(team = material$model_team[j], player = material$player_name[j],
      position = material$position[j], status = material$status[j], new_absence = material$absence_new[j])),
    adjustment_applied = if (!is.null(d) && !is.null(d$priced)) d$priced else NULL,
    open_cases = if (!is.null(d)) d$open else character(),
    card = list(pick_status = p$pick_status, injury_estimate_status = p$injury_estimate_status,
      baseline_margin = p$baseline_expected_margin, full_margin = p$expected_margin, half_margin = p$injury_half_margin,
      market_home_spread = p$market_home_spread, ats_pick = p$ats_pick, straight_up_pick = p$straight_up_pick))
})
open_material <- usage[usage$material %in% TRUE & usage$status %in% c("Questionable", "Doubtful"), c("model_team", "player_name", "position", "status", "game_id")]
log <- list(status = "ready_for_preliminary", season = 2026, week = 8, reviewed_at = reviewed_at,
  research_cutoff = reviewed_at, friday_noon_cutoff = "2026-10-23T19:00:00Z",
  cutoff_note = "Thursday-night refresh (Oct 22 PT), before the Friday-noon PT article cutoff. Friday conference updates (ACC/Big Ten/SEC ~5 PM PT Friday; game-day reports) are not included and must be revisited.",
  game_ids = card$game_id, games_reviewed = nrow(card), teams_reviewed = length(unique(c(card$home, card$away))),
  complete_official_availability = FALSE,
  official_report_states = as.list(table(sub(" posted.*$", "", sub("^Latest: ", "", src$latest_report)))),
  official_rows = nrow(usage), official_status_counts = as.list(table(usage$status)),
  user_assumption = "No assume-in instruction was given for this run. Questionable and Doubtful players stay open cases; they are not converted to Out or Active. Unlisted players are not proven healthy.",
  adjustments_applied = lapply(seq_len(nrow(est)), function(i) list(game_id = est$game_id[i], team = est$team[i], player_id = est$player_id[i],
    impact_points = est$impact_points[i], method = est$estimate_method[i], note = est$estimate_note[i])),
  flagged_unpriced = c("Syracuse Malachi Nelson (QB) + Amari Odom (QB): Out; Angeli Questionable.",
    "Old Dominion Quinn Henicle (QB): Out; Huff Questionable.", "Boston College Evan Dickens (RB): Out, new."),
  documented_not_flagged = c("Southern Miss Ethan Hampton (QB) Out since Week 3; Landry Lyddy has started 4+ games. Scenario -3.48 computed but not applied: CFBD box counts disagree by 1-3 plays in 5 games (DPI/duplicate-row conventions). Matches the Week 7 treatment.",
    "Previously absent material non-QBs already in team history: Illinois RB Ca'Lil Valentine, Michigan State RB Cam Edwards, Cal RB Adam Mohammed, NC State RB Duke Scott, Oregon WR Evan Stewart, Alabama WR Ryan Coleman-Williams."),
  open_material_cases = lapply(seq_len(nrow(open_material)), function(i) as.list(open_material[i, ])),
  pending_official_reports = src$slug[grepl("Pending", src$latest_report)],
  source_hashes = as.list(setNames(vapply(file.path(research, c("official_availability.csv", "official_availability_usage.csv", "unofficial_notes.csv", "game_sources.csv")),
    function(f) digest::digest(file = f, algo = "sha256"), ""), c("official_availability.csv", "official_availability_usage.csv", "unofficial_notes.csv", "game_sources.csv"))),
  mirror_retrieval = list(pages = nrow(retrieval), all_http_200 = all(retrieval$status == 200), first = min(retrieval$retrieved_at), last = max(retrieval$retrieved_at)),
  injury_card = injury_dir, games = games, players = players,
  note = "Official designations were read from kickoffstatus.com game pages, which mirror the conference availability reports; source_url records the conference report page. Unofficial Covers leads are context only.")
jsonlite::write_json(log, file.path(research, "injury_research.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null", na = "null")

pitt <- qb[qb$case %in% c("pitt", "pitt_dailey_alt"), ]
p <- card[card$game_id == "401858274", ]
review <- list(team = "Pittsburgh", opponent = "Miami", estimate_game_id = "401858274", reviewed_at = reviewed_at,
  status = "priced_reported_starter_open_usage_split", player = "Mason Heintschel", player_id = "5208095",
  availability = list(registry = "Out through season (inbox registry, school_official, 2026-10-04)",
    acc_report = usage[usage$game_id == "401858274" & usage$player_name == "Mason Heintschel", "status"],
    acc_report_state = src$latest_report[src$game_id == "401858274"],
    conflicting_lead = "Covers lists Heintschel 'questionable (leg)' dated 2026-10-17; superseded by the ACC report and the torn-ACL reporting."),
  starter = list(expected = "Holden Geriner", basis = "CBS Sports (Matt Zenitz) report relayed by SI and On3, Oct 22; Dailey 'expected to see time as well'. Narduzzi on Oct 19: decision open, both QBs could have packages.",
    week6 = "Geriner started vs North Carolina (27/44, 219 yds, 2 INT)", week7 = "Dailey started at Boston College (11/25, 193 yds, 1 TD, 2 INT)"),
  method = "native_epa_fbs_competitive_pass_attempt_sack_rush_replacement",
  formula = "(Heintschel competitive QB plays / all six prior FBS team games) * (Heintschel mean EPA - replacement mean EPA)",
  scenarios = lapply(seq_len(nrow(pitt)), function(i) list(case = pitt$case[i], replacement = pitt$replacement[i],
    starter_plays = pitt$starter_plays[i], starter_mean_epa = pitt$starter_mean_epa[i], replacement_plays = pitt$replacement_plays[i],
    replacement_games = pitt$replacement_games[i], replacement_mean_epa = pitt$replacement_mean_epa[i],
    opportunities = pitt$opportunities[i], loss_points = pitt$loss_points[i], box_all_matched = pitt$box_all_matched[i])),
  applied = list(impact_points = pitt$loss_points[pitt$case == "pitt"], half = pitt$loss_points[pitt$case == "pitt"] / 2,
    baseline_margin = p$baseline_expected_margin, full_margin = p$expected_margin, half_margin = p$injury_half_margin,
    market_home_spread = p$market_home_spread, baseline_ats_pick = "Pittsburgh", full_ats_pick = p$ats_pick, half_ats_pick = p$injury_half_ats_pick),
  box_check = "Heintschel and Geriner counts match CFBD player box scores in every game. Dailey's Week 7 total is one attempt short because one Pitt pass row has no passer name.",
  old_estimates_not_reused = c("Week 6: +5.69 (Geriner 13-play sample)", "Week 7: unpriced (replacement reconciliation pending)"),
  other_pitt_reports = usage[usage$game_id == "401858274" & usage$model_team == "Pittsburgh" & usage$status != "Available", c("player_name", "position", "status")],
  miami_reports = usage[usage$game_id == "401858274" & usage$model_team == "Miami" & usage$status != "Available", c("player_name", "position", "status")],
  recommendation = "The ATS side flips from Pitt +19.5 (baseline) to Miami -19.5 under both the half and full scenarios. With the starter unannounced and a two-QB plan possible, treat this as an injury-review game, not a featured pick.",
  sources = decisions[["401858274"]]$sources)
dir.create(file.path(scen, "pitt"), showWarnings = FALSE)
jsonlite::write_json(review, file.path(scen, "pitt", "pitt_at_miami_review.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
cat("games:", length(games), " players:", length(players), " open material:", nrow(open_material), "\n")
print(open_material, row.names = FALSE)
