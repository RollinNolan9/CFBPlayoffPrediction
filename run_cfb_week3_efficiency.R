if (length(commandArgs(trailingOnly = TRUE))) stop("This locked experiment takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
setwd(project)
Sys.unsetenv("LC_ALL")
source(file.path(project, "cfb_v2/tests/test_week3_efficiency.R"),
  local = new.env(parent = globalenv()))
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
  "models.R", "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R"))
  source(file.path(project, "cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "matchup_foundation_audit.R",
  "external_ratings.R", "week3_efficiency.R")) source(file.path(project, "cfb_v3/experiments", file))
result <- run_week3_efficiency(cfb_v2_config(project, 2026L))
cat("Results:", result$output, "\n")
print(result$metrics[result$metrics$slice == "week_3", ])
print(result$paired[result$paired$slice == "week_3", ])
