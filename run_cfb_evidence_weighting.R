if (length(commandArgs(trailingOnly = TRUE))) stop("This locked experiment takes no arguments.")
script <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]),
  winslash = "/", mustWork = TRUE)) else normalizePath(getwd(), winslash = "/")
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R",
  "models.R", "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R"))
  source(file.path(project_dir, "cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "evidence_weighting.R"))
  source(file.path(project_dir, "cfb_v3", "experiments", file))
result <- run_evidence_weighting(cfb_v2_config(project_dir, 2026L))
cat("Evidence-weighting experiment:", result$output, "\n")
print(result$results$paired_comparisons)
keep <- c("all_fbs", "week_0_1", "weeks_2_4", "fcs_only_followup_active", "zero_sample_targeted_active")
print(result$results$metrics[result$results$metrics$slice %in% keep,
  c("model", "slice", "games", "wins", "losses", "pushes", "ats_accuracy", "su_accuracy", "margin_mae")])
