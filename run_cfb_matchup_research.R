args <- commandArgs(trailingOnly=TRUE)
if (length(args)>1 || (length(args) && args[1]!="--prepare-only")) stop("Only --prepare-only is supported.")
script <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script))dirname(normalizePath(sub("^--file=","",script[1]),winslash="/")) else getwd()
for (file in c("config.R","store.R","features.R","historical_data.R","models.R","workflow.R"))
  source(file.path(project,"cfb_v2",file))
for (file in c("controlled_ats.R","external_ratings.R","tracker_edge.R","matchup_sources.R",
  "matchup_features.R","matchup_research.R")) source(file.path(project,"cfb_v3/experiments",file))
tracker_dir <- file.path(project,"cfb_v3/output/experiments/prediction_tracker/20260912T003231.716Z")
lines_dir <- file.path(project,"cfb_v3/output/experiments/matchup_sources/20260912T011754.513Z")
prepared <- matchup_prepare(project,tracker_dir,lines_dir)
cat("Prepared matchup inputs:",prepared,"\n")
if (!length(args)) {
  result <- run_matchup_research(project,prepared)
  print(result$summary,row.names=FALSE)
  cat("Matchup experiment:",result$output,"\n")
}
