if (length(commandArgs(trailingOnly = TRUE))) stop("This read-only audit takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
               "models.R", "coach_migration.R", "historical_data.R", "preseason.R",
               "bridge.R", "workflow.R")) source(file.path(project_dir, "cfb_v2", file))
source(file.path(project_dir, "cfb_v3", "experiments", "controlled_ats.R"))
source(file.path(project_dir, "cfb_v3", "experiments", "audit_og_inputs.R"))
result <- run_og_input_audit(cfb_v2_config(project_dir, 2026L))
cat("Audit report:", result$report, "\n")
print(result$coverage)
cat("Scored comparison ready:", result$ready, "\n")
