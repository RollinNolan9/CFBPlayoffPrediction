args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_arg)) {
  dirname(dirname(normalizePath(
    sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE
  )))
} else normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_dir, "cfb_v2", "dashboard.R"))

prediction_path <- NULL
for (arg in args) {
  if (grepl("^--predictions=", arg)) {
    prediction_path <- sub("^--predictions=", "", arg)
  } else stop("Unknown argument: ", arg, call. = FALSE)
}
if (is.null(prediction_path)) {
  prediction_path <- find_latest_prediction_csv(
    file.path(project_dir, "cfb_v2", "output")
  )
}
if (!grepl("^([A-Za-z]:|/)", prediction_path)) {
  prediction_path <- file.path(project_dir, prediction_path)
}
prediction_path <- normalizePath(prediction_path, winslash = "/", mustWork = TRUE)

predictions <- utils::read.csv(
  prediction_path, stringsAsFactors = FALSE, check.names = FALSE,
  na.strings = c("", "NA", "N/A", "null")
)
required <- c(
  "game_id", "season", "home", "away", "expected_margin",
  "market_home_spread", "straight_up_pick", "ats_pick"
)
missing <- setdiff(required, names(predictions))
if (length(missing)) {
  stop("Prediction snapshot is missing: ", paste(missing, collapse = ", "),
       call. = FALSE)
}
seasons <- unique(stats::na.omit(as.integer(predictions$season)))
if (length(seasons) != 1L) stop("Snapshot must contain exactly one season.", call. = FALSE)

if (!requireNamespace("cfbfastR", quietly = TRUE)) {
  stop("Package 'cfbfastR' is required.", call. = FALSE)
}
if (!cfbfastR::has_cfbd_key()) {
  stop("CFBD_API_KEY is required to retrieve final scores.", call. = FALSE)
}

pull_games <- function(season_type) {
  as.data.frame(
    cfbfastR::cfbd_game_info(seasons, season_type = season_type),
    stringsAsFactors = FALSE
  )
}
regular <- pull_games("regular")
postseason <- pull_games("postseason")
columns <- union(names(regular), names(postseason))
for (column in setdiff(columns, names(regular))) regular[[column]] <- rep(NA, nrow(regular))
for (column in setdiff(columns, names(postseason))) postseason[[column]] <- rep(NA, nrow(postseason))
games <- rbind(regular[columns], postseason[columns])
games <- games[!duplicated(as.character(games$game_id)), , drop = FALSE]
games <- games[match(as.character(predictions$game_id), as.character(games$game_id)), ]

graded <- predictions
graded$completed <- as.logical(games$completed) &
  is.finite(as.numeric(games$home_points)) &
  is.finite(as.numeric(games$away_points))
graded$home_score <- as.numeric(games$home_points)
graded$away_score <- as.numeric(games$away_points)
graded$actual_margin <- graded$home_score - graded$away_score
graded$actual_winner <- ifelse(
  graded$actual_margin > 0, graded$home,
  ifelse(graded$actual_margin < 0, graded$away, "tie")
)
graded$su_result <- ifelse(
  !graded$completed, NA_character_,
  ifelse(graded$straight_up_pick == graded$actual_winner, "W", "L")
)
graded$home_cover_margin <- graded$actual_margin + graded$market_home_spread
pick_is_home <- graded$ats_pick == graded$home
pick_is_away <- graded$ats_pick == graded$away
graded$pick_cover_margin <- ifelse(
  pick_is_home, graded$home_cover_margin,
  ifelse(pick_is_away, -graded$home_cover_margin, NA_real_)
)
graded$ats_result <- ifelse(
  !graded$completed | !is.finite(graded$pick_cover_margin), NA_character_,
  ifelse(graded$pick_cover_margin > 0, "W",
         ifelse(graded$pick_cover_margin < 0, "L", "P"))
)
graded$margin_error <- graded$actual_margin - graded$expected_margin

done <- graded[graded$completed, , drop = FALSE]
wins <- sum(done$ats_result == "W", na.rm = TRUE)
losses <- sum(done$ats_result == "L", na.rm = TRUE)
pushes <- sum(done$ats_result == "P", na.rm = TRUE)
ats_rate <- wins / (wins + losses)
su_wins <- sum(done$su_result == "W", na.rm = TRUE)
su_losses <- sum(done$su_result == "L", na.rm = TRUE)

model_columns <- intersect(
  c("foundation_expected_margin", "returning_expected_margin",
    "preseason_expected_margin", "expected_margin"),
  names(done)
)
model_labels <- c(
  foundation_expected_margin = "Foundation",
  returning_expected_margin = "Returning only",
  preseason_expected_margin = "Full preseason",
  expected_margin = "Production model"
)
comparison <- do.call(rbind, lapply(model_columns, function(column) {
  expected <- as.numeric(done[[column]])
  edge <- expected + done$market_home_spread
  actual_cover <- done$home_cover_margin
  valid_ats <- is.finite(edge) & edge != 0 &
    is.finite(actual_cover) & actual_cover != 0
  data.frame(
    model = unname(model_labels[[column]]),
    margin_mae = mean(abs(done$actual_margin - expected), na.rm = TRUE),
    su_accuracy = mean(sign(expected) == sign(done$actual_margin), na.rm = TRUE),
    ats_graded = sum(valid_ats),
    ats_accuracy = mean((edge[valid_ats] > 0) == (actual_cover[valid_ats] > 0)),
    stringsAsFactors = FALSE
  )
}))

output_dir <- dirname(prediction_path)
stamp <- format(Sys.Date(), "%Y%m%d")
csv_path <- file.path(output_dir, paste0("grading_", stamp, ".csv"))
report_path <- file.path(output_dir, paste0("grading_", stamp, ".md"))
utils::write.csv(graded, csv_path, row.names = FALSE, na = "")

fmt_pct <- function(value) ifelse(is.finite(value), sprintf("%.1f%%", 100 * value), "NA")
comparison_rows <- apply(comparison, 1, function(row) {
  paste0(
    "| ", row[["model"]], " | ", sprintf("%.2f", as.numeric(row[["margin_mae"]])),
    " | ", fmt_pct(as.numeric(row[["su_accuracy"]])), " | ",
    row[["ats_graded"]], " | ", fmt_pct(as.numeric(row[["ats_accuracy"]])), " |"
  )
})

result_rows <- apply(done, 1, function(row) {
  score <- paste0(row[["away_score"]], "-", row[["home_score"]])
  cover_margin <- sprintf("%+.1f", as.numeric(row[["pick_cover_margin"]]))
  paste0(
    "| ", row[["away"]], " at ", row[["home"]], " | ", score,
    " | ", row[["model_line"]], " | ", row[["ats_pick_line"]],
    " | ", row[["ats_result"]], " | ", cover_margin,
    " | ", row[["straight_up_pick"]], " | ", row[["su_result"]], " |"
  )
})

remaining <- graded[!graded$completed, , drop = FALSE]
remaining_lines <- if (nrow(remaining)) {
  c("", "## Remaining", "", paste0("- ", remaining$away, " at ", remaining$home))
} else character()

report <- c(
  "# CFB Snapshot Grade",
  "",
  paste0("Prediction snapshot: `", basename(prediction_path), "`"),
  paste0("Completed: ", nrow(done), " of ", nrow(graded), " games."),
  "",
  "## Record",
  "",
  paste0("- Straight up: **", su_wins, "-", su_losses, "** (",
         fmt_pct(su_wins / (su_wins + su_losses)), ")"),
  paste0("- ATS at the saved article lines: **", wins, "-", losses, "-", pushes,
         "** (", fmt_pct(ats_rate), " excluding pushes)"),
  paste0("- Production margin MAE: **",
         sprintf("%.2f", mean(abs(done$margin_error), na.rm = TRUE)), " points**"),
  paste0("- Saved market-line MAE: **",
         sprintf("%.2f", mean(abs(done$home_cover_margin), na.rm = TRUE)), " points**"),
  "",
  "## Model Comparison",
  "",
  "| Model | Margin MAE | SU accuracy | ATS graded | ATS accuracy |",
  "|---|---:|---:|---:|---:|",
  comparison_rows,
  "",
  "## Game Results",
  "",
  "| Game | Score (away-home) | Model line | Article ATS pick | ATS | Cover margin | SU pick | SU |",
  "|---|---:|---:|---:|:---:|---:|---|:---:|",
  result_rows,
  remaining_lines
)
writeLines(report, report_path, useBytes = TRUE)

cat("Completed:", nrow(done), "of", nrow(graded), "\n")
cat("Straight up:", paste0(su_wins, "-", su_losses),
    paste0("(", fmt_pct(su_wins / (su_wins + su_losses)), ")\n"))
cat("ATS:", paste0(wins, "-", losses, "-", pushes),
    paste0("(", fmt_pct(ats_rate), ")\n"))
cat("Margin MAE:", sprintf("%.2f", mean(abs(done$margin_error), na.rm = TRUE)), "\n")
cat("Market MAE:", sprintf("%.2f", mean(abs(done$home_cover_margin), na.rm = TRUE)), "\n")
print(comparison, row.names = FALSE)
if (nrow(remaining)) {
  cat("Remaining:", paste(paste(remaining$away, "at", remaining$home), collapse = ", "), "\n")
}
cat("Grade CSV:", csv_path, "\n")
cat("Grade report:", report_path, "\n")
