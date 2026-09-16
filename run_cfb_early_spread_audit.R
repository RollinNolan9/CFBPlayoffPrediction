if (length(commandArgs(trailingOnly = TRUE))) stop("This read-only audit takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
  "models.R", "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R"))
  source(file.path(project_dir, "cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "early_spread_audit.R"))
  source(file.path(project_dir, "cfb_v3", "experiments", file))
result <- run_early_spread_audit(cfb_v2_config(project_dir, 2026L))
cat("Audit:", result$output, "\n")
print(result$metrics[result$metrics$model %in% c("v3_matched", "v3_control"), ])
print(result$coverage$sources)
cat("Frozen reconstruction differences:", nrow(result$coverage$differences), "\n")
