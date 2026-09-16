if (length(commandArgs(trailingOnly = TRUE))) stop("This locked replay takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R", "live_data.R")) source(file.path(project_dir, "cfb_v2", file))
source(file.path(project_dir, "cfb_v3", "experiments", "controlled_ats.R"))
source(file.path(project_dir, "cfb_v3", "experiments", "model1_replay.R"))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(zoo))
result <- run_model1_replay(cfb_v2_config(project_dir, 2026L))
cat("Replay report:", result$report, "\n")
print(result$summary)
