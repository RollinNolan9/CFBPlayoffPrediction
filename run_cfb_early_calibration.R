if (length(commandArgs(trailingOnly = TRUE))) stop("This locked experiment takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
  "models.R", "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R"))
  source(file.path(project_dir, "cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "early_calibration.R"))
  source(file.path(project_dir, "cfb_v3", "experiments", file))
result <- run_early_calibration(cfb_v2_config(project_dir, 2026L))
cat("Calibration experiment:", result$output, "\n")
print(result$results$paired_comparisons)
print(result$results$choices)
print(result$results$metrics[result$results$metrics$slice %in% c("all_fbs", "week_0_1", "weeks_2_4"), ])
