args <- commandArgs(trailingOnly = TRUE)
if (length(args)) stop("Run from the project root without arguments.")
suppressPackageStartupMessages(library(dplyr))
path <- "cfb_v3/output/experiments/model1_inputs/compact_raw_inputs.rds"
if (file.exists(path)) stop("Compact inputs already exist; refusing to overwrite.")
e <- new.env(parent = baseenv())
message("Loading original workspace for raw-name, weighted sufficient statistics")
load(".RData", envir = e)
parts <- list()
sources <- c("cfb_pbp19_22_train", "cfb_pbp23_test", "cfb_prediction2024")
for (name in sources) {
  x <- e[[name]]
  message(name, ": ", nrow(x), " plays")
  x <- x[x$year %in% 2022:2025, ]
  last <- x[!duplicated(x$game_id, fromLast = TRUE), ]
  games <- as.data.frame(last[, c("game_id", "pos_team_score", "def_pos_team_score",
    "pos_team", "def_pos_team", "home", "away", "week", "year", "start_date")])
  names(games)[2:3] <- c("pos_team_final_score", "def_team_final_score")
  elo_columns <- grep("elo", names(last), value = TRUE)
  for (column in elo_columns) games[[column]] <- last[[column]]
  x <- x[!is.na(x$EPA), ]
  x$pass <- x$play_type %in% c("Pass", "Pass Incompletion", "Pass Reception", "Passing Touchdown")
  x$rush <- x$play_type %in% c("Rush", "Rushing Touchdown")
  stats <- x %>% group_by(game_id, year, pos_team, week) %>% summarise(
    epa_sum = sum(EPA), epa_n = n(),
    pass_sum = sum(EPA[pass]), pass_n = sum(pass),
    rush_sum = sum(EPA[rush]), rush_n = sum(rush),
    wpa_sum = sum(wpa, na.rm = TRUE), wpa_n = sum(!is.na(wpa)), .groups = "drop")
  parts[[name]] <- list(games = games, stats = stats)
}
games <- bind_rows(lapply(parts, `[[`, "games"))
stats <- bind_rows(lapply(parts, `[[`, "stats"))
stopifnot(!anyDuplicated(games$game_id), !anyDuplicated(stats[c("game_id", "pos_team")]))
result <- list(games = as.data.frame(games), stats = as.data.frame(stats),
  sp = as.data.frame(e$sp_ratings_all_years), elo = as.data.frame(e$elo_ratings_combined),
  source_md5 = unname(tools::md5sum(".RData")),
  script_md5 = unname(tools::md5sum("cfb_v3/experiments/extract_og_compact.R")))
saveRDS(result, path)
print(table(games$year))
print(names(games))
message("Compact source cache: ", path)
