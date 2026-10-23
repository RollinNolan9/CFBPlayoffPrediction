# Week 8 registry copy and verified per-game estimates for apply_live_injuries.R.
# The shared inbox registry is copied, never edited.
root <- normalizePath(getwd(), winslash = "/")
research <- normalizePath(file.path(root, "../outputs/article-week-8/research"), winslash = "/")
out <- file.path(research, "scenarios")
snapshot <- file.path(root, "cfb_v3/output/2026/week_8_friday_baseline/20261023T055652Z")
baseline_csv <- file.path(snapshot, "unadjusted_predictions.csv")
card <- read.csv(baseline_csv, stringsAsFactors = FALSE, colClasses = c(game_id = "character"))
audit <- read.csv(file.path(out, "qb_replacement_estimates_raw.csv"), stringsAsFactors = FALSE,
  colClasses = c(estimate_game_id = "character"))
inbox <- read.csv(file.path(root, "cfb_v3/inbox/player_availability.csv"), stringsAsFactors = FALSE,
  colClasses = c(player_id = "character"))
stopifnot(nrow(inbox) == 1L, inbox$player_name == "Mason Heintschel")

b10 <- "https://bigten.org/fb/availability-reports/"
acc <- "https://theacc.com/sports/2025/8/28/availability-reporting-football.aspx"
mw <- "https://themw.com/sports/2026/8/21/football_reports.aspx"
sbc <- "https://sunbeltsports.org/news/2025/8/11/football-availability-report-new.aspx"
kind <- "conference_report_posting_time_PT_converted_to_UTC_via_kickoffstatus_mirror"
new_rows <- read.csv(text = paste0("season,team,player_id,player_name,position,starter,status,through_week,source_type,source_url,source_timestamp,timestamp_kind,note
2026,Illinois,4795295,Katin Houser,QB,TRUE,out,8,conference_official,", b10, ",2026-10-23T00:00:00Z,", kind, ",Broken right throwing hand (Oct 10 at Michigan State); Big Ten Thursday update Out. Michael Clayton II named starter.
2026,UCLA,4870799,Nico Iamaleava,QB,TRUE,out,8,conference_official,", b10, ",2026-10-23T00:00:00Z,", kind, ",Season-ending knee surgery; Big Ten Thursday update Out. Ty Dieffenbach second start.
2026,Air Force,5238868,Liam Szarka,QB,TRUE,out,8,conference_official,", mw, ",2026-10-21T17:44:00Z,", kind, ",Season-ending lower-leg injury (announced Oct 5); MW initial report Out. Evan Medders third start.
2026,Syracuse,4870928,Malachi Nelson,QB,TRUE,out,8,conference_official,", acc, ",2026-10-23T00:00:00Z,", kind, ",High-ankle tight-rope surgery; ACC Thursday update Out. Amari Odom also Out (season). Steve Angeli named starter but listed Questionable.
2026,Old Dominion,5193346,Quinn Henicle,QB,TRUE,out,8,conference_official,", sbc, ",2026-10-22T00:06:00Z,", kind, ",Undisclosed injury from Oct 15; Sun Belt initial report Out. Ryan Huff Questionable; Jaxon Potter next.
2026,Boston College,,Evan Dickens,RB,TRUE,out,8,conference_official,", acc, ",2026-10-23T00:00:00Z,", kind, ",New absence; 31% of BC FBS competitive rushes through Week 7. No replacement estimate."),
  stringsAsFactors = FALSE, colClasses = c(player_id = "character"))
new_rows$player_id[new_rows$player_name == "Evan Dickens"] <- "Evan Dickens"
registry <- rbind(inbox[, names(new_rows)], new_rows)
registry_path <- file.path(out, "week8_player_availability.csv")
write.csv(registry, registry_path, row.names = FALSE, na = "")

sha <- digest::digest(file = baseline_csv, algo = "sha256")
as_of <- unique(card$as_of); stopifnot(length(as_of) == 1L)
get <- function(case) { r <- audit[audit$case == case, ]; stopifnot(nrow(r) == 1L, isTRUE(r$box_all_matched), r$missing_epa == 0); r }
il <- get("illinois"); or <- get("oregon"); uc <- get("ucla"); pt <- get("pitt"); af <- get("air_force")
alt <- audit[audit$case == "pitt_dailey_alt", ]
fmt <- function(r) sprintf("%s %d FBS competitive QB plays / %d prior FBS team games = %.2f replaced plays/game; EPA/play %+.3f vs %s %+.3f (%d plays, %d games)",
  r$starter, r$starter_plays, r$fbs_team_games, r$opportunities, r$starter_mean_epa, r$replacement, r$replacement_mean_epa,
  r$replacement_plays, r$replacement_games)
method <- "native_epa_fbs_competitive_pass_attempt_sack_rush_replacement"
source <- file.path(out, "qb_replacement_estimates_raw.csv")
caveat <- "Box-count check vs CFBD player box scores matched every game. Signed, uncapped, no shrinkage; experimental scenario, not a fitted or causal value. Existing replacement exposure stays in team history."
estimates <- data.frame(
  game_id = c(il$estimate_game_id, uc$estimate_game_id, pt$estimate_game_id, af$estimate_game_id),
  team = c("Illinois", "UCLA", "Pittsburgh", "Air Force"),
  player_id = c("4795295", "4870799", "5208095", "5238868"),
  impact_points = c(il$loss_points - or$loss_points, uc$loss_points, pt$loss_points, af$loss_points),
  estimate_game_id = c(il$estimate_game_id, uc$estimate_game_id, pt$estimate_game_id, af$estimate_game_id),
  baseline_as_of = as_of, baseline_sha256 = sha, input_verified = TRUE,
  estimate_method = c(paste0(method, "_joint_two_team"), method, method, method),
  estimate_source = source,
  estimate_note = c(
    sprintf("JOINT Oregon-Illinois QB estimate keyed to Houser: Illinois loss %.2f minus Oregon Dante Moore loss %.2f = %.2f net points against Illinois. %s. %s. Clayton sample is one relief game; a single TD drive dominates it, and his overtime TD has no native EPA. Moore out (concussion protocol, Big Ten Out); Raiola expected starter per On3/Ducks Wire Oct 22. %s",
      il$loss_points, or$loss_points, il$loss_points - or$loss_points, fmt(il), fmt(or), caveat),
    sprintf("%s. Ty Dieffenbach confirmed second start (AP/UCLA, Oct 20-22). Negative value means the replacement scenario raises UCLA's margin. %s", fmt(uc), caveat),
    sprintf("%s. Geriner expected starter per CBS (Zenitz) via SI/On3 Oct 22; Pitt has not announced and Corey Dailey is expected to play too. Dailey-only alternative: %.2f. Half scenario approximates an even split. %s", fmt(pt), alt$loss_points, caveat),
    sprintf("%s. Medders is the confirmed triple-option starter (Air Force notes Oct 19). Negative value means the replacement scenario raises Air Force's margin. %s", fmt(af), caveat)),
  stringsAsFactors = FALSE)
write.csv(estimates, file.path(out, "week8_qb_estimates.csv"), row.names = FALSE, na = "")
print(estimates[, c("team", "impact_points")], row.names = FALSE)
cat("registry rows:", nrow(registry), " baseline sha256:", sha, "\n")
