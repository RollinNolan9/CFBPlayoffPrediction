args <- commandArgs(trailingOnly=TRUE)
if(length(args)>1 || (length(args) && args[1]!="--turnover-flag"))stop("Only --turnover-flag is supported.")
script <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script))dirname(normalizePath(sub("^--file=","",script[1]),winslash="/")) else getwd()
for(file in c("config.R","store.R","features.R","coaches.R","adapters.R","models.R",
  "coach_migration.R","historical_data.R","preseason.R","bridge.R","workflow.R"))
  source(file.path(project,"cfb_v2",file))
for(file in c("controlled_ats.R","external_ratings.R","matchup_source_probe.R","matchup_play_contract.R","matchup_foundation_audit.R"))
  source(file.path(project,"cfb_v3/experiments",file))
root <- file.path(project,"cfb_v3/output/experiments")
result <- run_matchup_foundation_audit(project,
  file.path(root,"prediction_tracker/20260912T003231.716Z"),
  file.path(root,"matchup_prepared/20260912T015157.387Z"),
  file.path(root,c("matchup_source_probe/20260912T014121.700Z","matchup_source_probe/20260912T014320.238Z")),
  correction=if(length(args))"turnover_flag" else "duplicates")
cat("Football data-quality audit:",result,"\n")
