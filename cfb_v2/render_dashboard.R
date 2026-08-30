args <- commandArgs(trailingOnly = TRUE)
script_path <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_path)) {
  dirname(dirname(normalizePath(
    sub("^--file=", "", script_path[1]), winslash = "/", mustWork = TRUE
  )))
} else normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_dir, "cfb_v2", "dashboard.R"))

predictions <- NULL
output <- NULL
for (arg in args) {
  if (grepl("^--predictions=", arg)) predictions <- sub("^--predictions=", "", arg)
  else if (grepl("^--output=", arg)) output <- sub("^--output=", "", arg)
  else stop("Unknown argument: ", arg, call. = FALSE)
}
if (is.null(predictions)) {
  predictions <- find_latest_prediction_csv(file.path(project_dir, "cfb_v2", "output"))
}
if (is.null(output)) {
  output <- if (basename(predictions) == "predictions.csv") {
    "dashboard.html"
  } else sub("\\.csv$", "_dashboard.html", basename(predictions), ignore.case = TRUE)
}
path <- render_cfb_dashboard(
  predictions, project_dir, dirname(predictions), output
)
cat("Dashboard:", path, "\n")
