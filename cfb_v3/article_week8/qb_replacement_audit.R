# Week 8 QB replacement audit: the Week 7 UCLA method applied to every reviewed QB case.
# Native EPA, FBS-eligible competitive pass attempts/sacks/QB rushes; no shrinkage or cap.
root <- normalizePath(getwd(), winslash = "/")
out <- normalizePath(file.path(root, "../outputs/article-week-8/research/scenarios"), winslash = "/")
snapshot <- file.path(root, "cfb_v3/output/2026/week_8_friday_baseline/20261023T055652Z")
rankings <- file.path(root, "cfb_v3/output/2026/week_8_rankings_relative/20261018T181642Z")
for (m in c("config", "store", "features", "coaches", "adapters", "models", "coach_migration", "historical_data"))
  source(file.path(root, "cfb_v2", paste0(m, ".R")))
x <- readRDS(file.path(snapshot, "cfb_v2/cache/pbp/pbp_2026_compact.rds"))
raw <- readRDS(file.path(rankings, "official_2026_raw.rds"))
g <- read.csv(file.path(snapshot, "source_games.csv"), stringsAsFactors = FALSE)
card <- read.csv(file.path(snapshot, "unadjusted_predictions.csv"), stringsAsFactors = FALSE)
key <- function(z) paste(z$game_id, z$id_play, sep = ":")
competitive <- historical_competitive_plays(x)
x$baseline_competitive <- key(x) %in% key(competitive)
x$fbs_eligible <- x$game_id %in% g$game_id[raw_history_eligible(g)]

qbs <- read.csv(text = "team,player,initial,surname,jersey
Illinois,Katin Houser,K,Houser,4
Illinois,Michael Clayton II,M,Clayton II,7
Oregon,Dante Moore,D,Moore,5
Oregon,Dylan Raiola,D,Raiola,8
Oregon,Brock Thomas,B,Thomas,12
Oregon,Akili Smith Jr.,A,Smith Jr.,11
UCLA,Nico Iamaleava,N,Iamaleava,9
UCLA,Ty Dieffenbach,T,Dieffenbach,14
UCLA,Madden Iamaleava,M,Iamaleava,10
Pittsburgh,Mason Heintschel,M,Heintschel,6
Pittsburgh,Holden Geriner,H,Geriner,15
Pittsburgh,Corey Dailey,C,Dailey,16
Air Force,Liam Szarka,L,Szarka,9
Air Force,Evan Medders,E,Medders,5
Air Force,Kale Hodges,K,Hodges,4
Southern Miss,Ethan Hampton,E,Hampton,5
Southern Miss,Landry Lyddy,L,Lyddy,18
Syracuse,Steve Angeli,S,Angeli,9
Syracuse,Malachi Nelson,M,Nelson,16
Syracuse,Amari Odom,A,Odom,12
Syracuse,Danny Lauter,D,Lauter,21
Old Dominion,Quinn Henicle,Q,Henicle,10
Old Dominion,Ryan Huff,R,Huff,4
Old Dominion,Jaxon Potter,J,Potter,2", stringsAsFactors = FALSE)
teams <- unique(qbs$team)
x <- x[x$pos_team %in% teams & x$game_id %in% g$game_id[g$week < 8], ]
stopifnot(!anyDuplicated(key(x)), !anyDuplicated(key(raw[raw$game_id %in% unique(x$game_id), ])))
idx <- match(key(x), key(raw))
x$raw_matched <- !is.na(idx)
meta <- c("penalty_no_play", "pass_attempt", "completion_player", "incompletion_player",
  "interception_thrown_player", "sack_taken_player", "rush_player")
for (col in meta) x[[col]] <- raw[[col]][idx]
stopifnot(all(x$EPA[x$raw_matched] == raw$EPA[idx[x$raw_matched]] | (is.na(x$EPA) & is.na(raw$EPA[idx]))[x$raw_matched]))
# Conversion tries and PATs are appended to touchdown text; they are not scrimmage plays.
x$scrimmage_text <- sub("(?:#\\d+ )?[A-Z][A-Za-z.'\\-]*(?: [A-Z][A-Za-z.'\\-]*)?(?: (?:II|III|Jr[.]?))? (?:pass|rush|run|kick) attempt.*$",
  "", sub("\\(Original Play:.*", "", x$play_text, ignore.case = TRUE), perl = TRUE)
# NCAA scoring voids an attempt when defensive pass interference is accepted, even without NO PLAY text.
accepted_dpi <- grepl("Pass Interference(?! declined)(?: \\([^)]*\\))? \\d+ yards", x$scrimmage_text, perl = TRUE)
# The raw no-play flag also fires for penalties on an appended conversion try.
has_try <- nchar(x$scrimmage_text) < nchar(sub("\\(Original Play:.*", "", x$play_text, ignore.case = TRUE))
x$no_play <- (!is.na(x$penalty_no_play) & as.logical(x$penalty_no_play) & !has_try) |
  grepl("NO PLAY", x$scrimmage_text, ignore.case = TRUE) | accepted_dpi
x$clock_kill <- grepl("kneel|spike|clock stop|end of game|end of half", paste(x$play_type, x$play_text), ignore.case = TRUE)
yes <- function(z) !is.na(z) & z == 1
x$is_sack <- yes(x$sack) | grepl("sack", x$play_type, ignore.case = TRUE)
x$is_pass_attempt <- (yes(x$pass_attempt) | yes(x$pass) | grepl("pass", x$play_type, ignore.case = TRUE)) & !x$is_sack
x$is_qb_rush <- (yes(x$rush) | grepl("rush|run", x$play_type, ignore.case = TRUE)) & !x$is_sack & !x$is_pass_attempt
x$play_kind <- ifelse(x$is_sack, "sack", ifelse(x$is_pass_attempt, "pass_attempt", ifelse(x$is_qb_rush, "rush", "other")))
x$player <- NA_character_; x$attribution <- NA_character_
roles <- c("completion_player", "incompletion_player", "interception_thrown_player", "sack_taken_player", "rush_player")
verbs <- " +(?:pass|sacked|rush|run|scrambl)"
subject_prefix <- function(p) {
  surname <- gsub(".", "[.]", p$surname, fixed = TRUE)
  # Oregon WR Dakorien Moore shares "D.Moore"; require Dante's #5 or full name.
  if (p$player == "Dante Moore") return("(?:Dante Moore|#5 D[.]? ?Moore)")
  paste0("(?:", p$player, "|(?:#", p$jersey, " )?", p$initial, "[.]? ?", surname, ")")
}
subject_of <- function(p) paste0(subject_prefix(p), verbs)
for (i in seq_len(nrow(qbs))) {
  p <- qbs[i, ]; onteam <- x$pos_team == p$team
  exact <- rep(FALSE, nrow(x))
  for (role in roles) exact <- exact | (!is.na(x[[role]]) & x[[role]] == p$player)
  text_subject <- grepl(subject_of(p), x$scrimmage_text, ignore.case = FALSE, perl = TRUE)
  hit <- onteam & (exact | text_subject)
  clash <- hit & !is.na(x$player)
  if (any(clash)) stop("Ambiguous QB attribution: ", p$player, " vs ", paste(unique(x$player[clash]), collapse = ","))
  x$player[hit] <- p$player
  x$attribution[hit] <- ifelse(exact[hit], "native_role_full_name", "action_subject_text")
}
final_text <- x$scrimmage_text
for (i in seq_len(nrow(qbs))) {
  p <- qbs[i, ]
  subject <- paste0(subject_prefix(p), " +")
  onplayer <- !is.na(x$player) & x$player == p$player
  # Intentional grounding is scored as a sack when the native play type says so.
  sack_action <- onplayer & (grepl(paste0(subject, "sacked"), final_text, perl = TRUE) | grepl("^Sack$", x$play_type))
  pass_action <- onplayer & grepl(paste0(subject, "pass "), final_text, perl = TRUE)
  rush_action <- onplayer & grepl(paste0(subject, "(?:rush|run|scrambl)"), final_text, perl = TRUE)
  x$play_kind[sack_action] <- "sack"
  x$play_kind[pass_action & !sack_action] <- "pass_attempt"
  x$play_kind[rush_action & !sack_action & !pass_action] <- "rush"
}
x$finite_epa <- is.finite(x$EPA)
# Source feeds occasionally repeat a clock-stamped play under a new id.
dedupe_text <- gsub("(?:short|deep) (?:left|right|middle) ", "", x$play_text, perl = TRUE)
x$duplicate_source_row <- grepl("^\\(\\d+:\\d+\\)", x$play_text) & duplicated(paste(x$game_id, x$period, dedupe_text))
x$include <- !is.na(x$player) & x$play_kind != "other" & !x$no_play & !x$clock_kill & !x$duplicate_source_row
x$week <- g$week[match(x$game_id, g$game_id)]
gi <- match(x$game_id, g$game_id)
x$opponent <- ifelse(g$home[gi] == x$pos_team, g$away[gi], g$home[gi])
cols <- c("pos_team", "game_id", "id_play", "week", "opponent", "period", "player", "attribution", "play_kind", "EPA",
  "finite_epa", "fbs_eligible", "baseline_competitive", "raw_matched", "no_play", "clock_kill", "duplicate_source_row", "include",
  "play_type", "play_text")
write.csv(x[!is.na(x$player), cols], file.path(out, "qb_play_audit.csv"), row.names = FALSE, na = "")
unattributed <- x[is.na(x$player) & (x$is_pass_attempt | x$is_sack) & !x$no_play & !x$clock_kill, cols]
write.csv(unattributed, file.path(out, "qb_unattributed_passes.csv"), row.names = FALSE, na = "")
qb <- x[x$include, ]
summarize <- function(z) data.frame(plays = nrow(z), pass_attempts = sum(z$play_kind == "pass_attempt"),
  rushes_excluding_sacks = sum(z$play_kind == "rush"), sacks = sum(z$play_kind == "sack"),
  completions = sum(z$play_kind == "pass_attempt" & (grepl("Pass Reception|Passing Touchdown", z$play_type) |
    grepl("pass complete", sub("\\(Original Play:.*", "", z$play_text, ignore.case = TRUE), ignore.case = TRUE))),
  missing_epa = sum(!z$finite_epa), total_epa = sum(z$EPA[z$finite_epa]),
  mean_epa = if (any(z$finite_epa)) mean(z$EPA[z$finite_epa]) else NA_real_)
# Overtime plays carry no native EPA: kept for box counts, excluded from the EPA cohort.
scopes <- list(all_games = rep(TRUE, nrow(qb)), fbs_competitive = qb$fbs_eligible & qb$baseline_competitive & qb$finite_epa)
summary <- list(); pergame <- list()
for (scope in names(scopes)) for (i in seq_len(nrow(qbs))) {
  p <- qbs[i, ]; z <- qb[scopes[[scope]] & qb$player == p$player & qb$pos_team == p$team, ]
  summary[[length(summary) + 1L]] <- cbind(data.frame(team = p$team, scope = scope, player = p$player,
    games = length(unique(z$game_id))), summarize(z))
  for (id in unique(z$game_id)) pergame[[length(pergame) + 1L]] <- cbind(data.frame(team = p$team, scope = scope,
    player = p$player, game_id = id, week = z$week[z$game_id == id][1], opponent = z$opponent[z$game_id == id][1]),
    summarize(z[z$game_id == id, ]))
}
summary <- do.call(rbind, summary); pergame <- do.call(rbind, pergame)
write.csv(summary, file.path(out, "qb_scope_summary.csv"), row.names = FALSE, na = "")
write.csv(pergame, file.path(out, "qb_game_summary.csv"), row.names = FALSE, na = "")
games <- do.call(rbind, lapply(teams, function(team) {
  z <- g[(g$home == team | g$away == team) & g$week < 8, ]
  data.frame(team = team, z[, c("game_id", "week", "home", "away")], fbs_eligible = raw_history_eligible(z))
}))
write.csv(games, file.path(out, "qb_source_games.csv"), row.names = FALSE, na = "")

# Independent count check against CFBD player box scores (ESPN-derived; NCAA rushes include sacks).
readRenviron(file.path(root, ".Renviron"))
stopifnot(nzchar(Sys.getenv("CFBD_API_KEY")))
box <- do.call(rbind, lapply(teams, function(team) {
  pull <- function(category) {
    r <- cfbfastR::cfbd_game_player_stats(year = 2026, team = team, category = category, season_type = "regular")
    r[r$team == team, , drop = FALSE]
  }
  pa <- pull("passing"); ru <- pull("rushing")
  pass <- data.frame(game_id = pa$game_id, team = team, athlete_id = pa$athlete_id, box_name = pa$athlete_name,
    box_completions = as.integer(pa$passing_completions), box_pass_attempts = as.integer(pa$passing_attempts))
  rush <- data.frame(game_id = ru$game_id, team = team, athlete_id = ru$athlete_id, box_name = ru$athlete_name,
    box_rushes_including_sacks = as.integer(ru$rushing_car))
  merge(pass, rush, by = c("game_id", "team", "athlete_id", "box_name"), all = TRUE)
}))
box$box_completions[is.na(box$box_completions)] <- 0L; box$box_pass_attempts[is.na(box$box_pass_attempts)] <- 0L
box$box_rushes_including_sacks[is.na(box$box_rushes_including_sacks)] <- 0L
surname_key <- function(n) tolower(sub("^.* ([A-Za-z'\\-]+)( (II|III|Jr\\.?))?$", "\\1", n))
box$key <- paste(box$team, surname_key(box$box_name), substr(box$box_name, 1, 1))
actual <- pergame[pergame$scope == "all_games", ]
actual$key <- paste(actual$team, surname_key(actual$player), substr(actual$player, 1, 1))
check <- merge(actual, box[, setdiff(names(box), "team")], by = c("game_id", "key"), all.x = TRUE)
check$box_found <- !is.na(check$box_name)
check$matched <- with(check, box_found & completions == box_completions & pass_attempts == box_pass_attempts &
  rushes_excluding_sacks + sacks == box_rushes_including_sacks)
qb_box_missing <- box[box$box_pass_attempts > 0 & !paste(box$game_id, box$key) %in% paste(actual$game_id, actual$key) &
  box$game_id %in% g$game_id[g$week < 8], ]
write.csv(check, file.path(out, "qb_box_reconciliation.csv"), row.names = FALSE, na = "")
write.csv(qb_box_missing, file.path(out, "qb_box_passers_not_attributed.csv"), row.names = FALSE, na = "")

cases <- read.csv(text = "case,team,starter,replacement,estimate_game_id,applied
illinois,Illinois,Katin Houser,Michael Clayton II,401858495,joint
oregon,Oregon,Dante Moore,Dylan Raiola,401858495,joint
ucla,UCLA,Nico Iamaleava,Ty Dieffenbach,401858499,yes
pitt,Pittsburgh,Mason Heintschel,Holden Geriner,401858274,yes
pitt_dailey_alt,Pittsburgh,Mason Heintschel,Corey Dailey,401858274,sensitivity
air_force,Air Force,Liam Szarka,Evan Medders,401864525,yes
southern_miss,Southern Miss,Ethan Hampton,Landry Lyddy,401869967,yes
syracuse_angeli,Syracuse,Malachi Nelson,Steve Angeli,401858275,sensitivity
odu_huff,Old Dominion,Quinn Henicle,Ryan Huff,401871093,sensitivity", stringsAsFactors = FALSE)
fbs <- summary[summary$scope == "fbs_competitive", ]
row_for <- function(team, player) fbs[fbs$team == team & fbs$player == player, ]
results <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  k <- cases[i, ]; s <- row_for(k$team, k$starter); r <- row_for(k$team, k$replacement)
  denom <- sum(games$fbs_eligible[games$team == k$team])
  box_rows <- check[check$team == k$team & check$player %in% c(k$starter, k$replacement), ]
  data.frame(k, fbs_team_games = denom, starter_plays = s$plays, starter_games = s$games, starter_mean_epa = s$mean_epa,
    replacement_plays = r$plays, replacement_games = r$games, replacement_mean_epa = r$mean_epa,
    opportunities = s$plays / denom, loss_points = s$plays / denom * (s$mean_epa - r$mean_epa),
    missing_epa = s$missing_epa + r$missing_epa, box_rows = nrow(box_rows), box_all_matched = all(box_rows$matched),
    unattributed_team_pass_rows = sum(unattributed$pos_team == k$team), stringsAsFactors = FALSE)
}))
write.csv(results, file.path(out, "qb_replacement_estimates_raw.csv"), row.names = FALSE, na = "")
saveRDS(list(plays = x, scope_summary = summary, game_summary = pergame, games = games, box = check, results = results),
  file.path(out, "qb_replacement_audit.rds"))
print(results[, c("case", "fbs_team_games", "starter_plays", "starter_mean_epa", "replacement_plays", "replacement_games",
  "replacement_mean_epa", "loss_points", "box_rows", "box_all_matched", "unattributed_team_pass_rows")], row.names = FALSE, digits = 4)
cat("\nBox mismatches:\n"); print(check[!check$matched, c("team", "player", "game_id", "week", "completions", "box_completions",
  "pass_attempts", "box_pass_attempts", "rushes_excluding_sacks", "sacks", "box_rushes_including_sacks")], row.names = FALSE)
cat("\nBox passers not attributed:\n"); print(qb_box_missing[, c("team", "game_id", "box_name", "box_completions", "box_pass_attempts", "box_rushes_including_sacks")], row.names = FALSE)
cat("\nRaw-unmatched compact rows among attributed plays:", sum(!x$raw_matched & !is.na(x$player)), "\n")
