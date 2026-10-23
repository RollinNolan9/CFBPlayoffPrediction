# Informational line refresh: compares current CFBD DraftKings/FanDuel quotes with the frozen card.
# The frozen baseline and the injury-adjusted card keep their captured markets.
root <- normalizePath(getwd(), winslash = "/")
research <- normalizePath(file.path(root, "../outputs/article-week-8/research"), winslash = "/")
for (m in c("config", "store", "features", "adapters")) source(file.path(root, "cfb_v2", paste0(m, ".R")))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
card <- read.csv(args[1], stringsAsFactors = FALSE, colClasses = c(game_id = "character"))
readRenviron(file.path(root, ".Renviron"))
captured <- Sys.time()
lines <- pull_cfbd_lines(2026, 8, captured)
fresh <- select_article_lines(lines, card$game_id, captured + 1)
stamp <- format(captured, "%Y%m%dT%H%M%SZ", tz = "UTC")
x <- merge(card[, c("game_id", "away", "home", "expected_margin", "market_home_spread", "market_captured_at", "ats_pick", "pick_status")],
  fresh[, c("game_id", "market_home_spread", "draftkings_home_spread", "fanduel_home_spread", "line_source")],
  by = "game_id", suffixes = c("_frozen", "_fresh"), sort = FALSE)
x$line_move <- x$market_home_spread_fresh - x$market_home_spread_frozen
x$frozen_gap <- abs(x$expected_margin + x$market_home_spread_frozen)
x$fresh_gap <- abs(x$expected_margin + x$market_home_spread_fresh)
x$fresh_ats_pick <- ifelse(x$expected_margin + x$market_home_spread_fresh >= 0, x$home, x$away)
x$ats_pick_changes <- !is.na(x$fresh_ats_pick) & x$fresh_ats_pick != x$ats_pick
x$fresh_captured_at <- format(captured, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
write.csv(x, file.path(research, paste0("line_refresh_", stamp, ".csv")), row.names = FALSE, na = "")
moved <- x[is.finite(x$line_move) & abs(x$line_move) >= 0.5, ]
print(moved[order(-abs(moved$line_move)), c("away", "home", "market_home_spread_frozen", "market_home_spread_fresh", "line_source",
  "expected_margin", "frozen_gap", "fresh_gap", "ats_pick", "fresh_ats_pick")], row.names = FALSE, digits = 3)
cat("games:", nrow(x), " fresh lines:", sum(is.finite(x$market_home_spread_fresh)), " moved>=0.5:", nrow(moved),
  " ATS changes:", sum(x$ats_pick_changes), "\n")
