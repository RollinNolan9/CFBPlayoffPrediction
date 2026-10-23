# Copies the injury-scenario card into the article folder and rebuilds the gaps table.
# Baseline article copies move to baseline-20261023T055652Z/; the frozen snapshot is untouched.
root <- normalizePath(getwd(), winslash = "/")
article <- normalizePath(file.path(root, "../outputs/article-week-8"), winslash = "/")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, dir.exists(args[1]))
injury_dir <- normalizePath(args[1], winslash = "/")
keep <- file.path(article, "baseline-20261023T055652Z")
if (!dir.exists(keep)) {
  dir.create(keep)
  for (f in c("dashboard.html", "predictions.csv", "qa.json", "injury_reports.csv", "model_market_gaps.csv"))
    stopifnot(file.rename(file.path(article, f), file.path(keep, f)))
}
copies <- c(predictions.csv = "predictions.csv", dashboard.html = "dashboard.html", qa.json = "qa.json",
  injury_reports.csv = "injury_reports.csv", injury_audit.csv = "injury_audit.csv", comparison.csv = "scenario_comparison.csv",
  scenarios.csv = "scenarios.csv", injury_report.md = "injury_report.md", supplied_estimates.csv = "supplied_estimates.csv")
for (f in names(copies)) stopifnot(file.copy(file.path(injury_dir, f), file.path(article, copies[[f]]), overwrite = TRUE))
p <- read.csv(file.path(article, "predictions.csv"), stringsAsFactors = FALSE, colClasses = c(game_id = "character"))
log <- jsonlite::read_json(file.path(article, "research/injury_research.json"))
open_games <- unique(vapply(log$open_material_cases, function(x) as.character(x$game_id), ""))
gaps <- data.frame(game_id = p$game_id, away = p$away, home = p$home, kickoff_display = p$kickoff_display,
  market_home_spread = p$market_home_spread, expected_margin = round(p$expected_margin, 1),
  baseline_margin = round(p$baseline_expected_margin, 1),
  gap = round(abs(p$expected_margin + p$market_home_spread), 1),
  baseline_gap = round(abs(p$baseline_expected_margin + p$market_home_spread), 1),
  ats_pick = p$ats_pick, pick_line = ifelse(p$ats_pick == p$home, p$market_home_spread, -p$market_home_spread),
  straight_up_pick = p$straight_up_pick, pick_status = p$pick_status, injury_estimate_status = p$injury_estimate_status,
  open_material_case = p$game_id %in% open_games, data_flag = p$data_flag,
  home_ap_rank = p$home_ap_rank, away_ap_rank = p$away_ap_rank, stringsAsFactors = FALSE)
gaps <- gaps[order(-gaps$gap), ]
write.csv(gaps, file.path(article, "model_market_gaps.csv"), row.names = FALSE, na = "")
board <- gaps[gaps$pick_status == "article_pick" & !gaps$open_material_case, ]
cat("Clean board (no injury review, no open material case):\n")
print(head(board[, c("away", "home", "ats_pick", "pick_line", "expected_margin", "gap")], 12), row.names = FALSE)
cat("\nReview / open-case games in the top 15 by gap:\n")
print(head(gaps[gaps$pick_status != "article_pick" | gaps$open_material_case, c("away", "home", "ats_pick", "pick_line", "gap", "baseline_gap", "pick_status", "open_material_case")], 15), row.names = FALSE)
