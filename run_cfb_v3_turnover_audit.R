if(length(commandArgs(trailingOnly=TRUE)))stop("This isolated full-v3 impact audit takes no arguments.")
script <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script))dirname(normalizePath(sub("^--file=","",script[1]),winslash="/")) else getwd()
for(file in c("config.R","store.R","features.R","coaches.R","adapters.R","models.R",
  "coach_migration.R","historical_data.R","preseason.R","bridge.R","workflow.R"))
  source(file.path(project,"cfb_v2",file))
for(file in c("controlled_ats.R","external_ratings.R","prediction_tracker.R","tracker_edge.R",
  "matchup_features.R","matchup_research.R","matchup_foundation_audit.R","matchup_v3_turnover.R"))
  source(file.path(project,"cfb_v3/experiments",file))
root <- file.path(project,"cfb_v3/output/experiments")
result <- run_matchup_v3_turnover(project,
  file.path(root,"prediction_tracker/20260912T003231.716Z"),
  file.path(root,"matchup_prepared/20260912T015157.387Z"),
  file.path(root,"matchup_turnover_audit/20260912T022858.354Z"))
cat("Full-v3 turnover audit:",result,"\n")
