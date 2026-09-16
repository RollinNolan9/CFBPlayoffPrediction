if (length(commandArgs(trailingOnly = TRUE))) stop("The locked v1 comparison takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R")) source(file.path(project_dir, "cfb_v2", file))
source(file.path(project_dir, "cfb_v3", "experiments", "controlled_ats.R"))
source(file.path(project_dir, "cfb_v3", "experiments", "v1_comparison.R"))
result <- run_v1_comparison(cfb_v2_config(project_dir, 2026L))
cat("V1 comparison:", result$report, "\n")
print(result$metrics)
print(result$paired)
print(result$coverage)
