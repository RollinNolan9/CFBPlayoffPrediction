args <- commandArgs(trailingOnly=TRUE)
if(length(args)>2L || (length(args) && !args[1] %in% c("--audit-only","--compare")) ||
   (length(args) && args[1]=="--compare" && length(args)!=2L) ||
   (length(args) && args[1]=="--audit-only" && length(args)!=1L))
  stop("Usage: Rscript run_cfb_play_cleanup.R [--audit-only | --compare PREPARED_DIRECTORY]")
script <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script))dirname(normalizePath(sub("^--file=","",script[1]),winslash="/")) else getwd()
setwd(project); Sys.unsetenv("LC_ALL")
for(file in c("config.R","store.R","features.R","coaches.R","adapters.R","models.R",
  "coach_migration.R","historical_data.R","preseason.R","bridge.R","workflow.R"))
  source(file.path(project,"cfb_v2",file))
for(file in c("controlled_ats.R","external_ratings.R","prediction_tracker.R","tracker_edge.R",
  "matchup_features.R","matchup_research.R","matchup_foundation_audit.R","matchup_source_probe.R",
  "matchup_drive_audit.R","play_cleanup_classifier.R","play_cleanup_audit.R","play_cleanup_pipeline.R"))
  source(file.path(project,"cfb_v3/experiments",file))
for(file in c("test_play_cleanup_classifier.R","test_play_cleanup_audit.R","test_play_cleanup_pipeline.R"))
  source(file.path(project,"cfb_v2/tests",file),local=new.env(parent=globalenv()))
prepared <- if(length(args) && args[1]=="--compare")normalizePath(args[2],winslash="/") else play_cleanup_prepare(project)
cat("Cleanup audit:",prepared,"\n")
if(!length(args) || args[1]!="--audit-only") {
  result <- play_cleanup_compare(project,prepared)
  cat("Cleanup comparison:",file.path(result,"REPORT.md"),"\n")
}
