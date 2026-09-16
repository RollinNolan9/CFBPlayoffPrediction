if(length(commandArgs(trailingOnly=TRUE)))stop("This sealed-artifact report takes no arguments.")
script <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script))dirname(normalizePath(sub("^--file=","",script[1]),winslash="/")) else getwd()
for(file in c("config.R","store.R","features.R","historical_data.R","models.R","workflow.R"))source(file.path(project,"cfb_v2",file))
for(file in c("controlled_ats.R","external_ratings.R","tracker_edge.R","matchup_research.R","matchup_report.R"))
  source(file.path(project,"cfb_v3/experiments",file))
paths <- c(prepared="matchup_prepared/20260912T015157.387Z",
  research="matchup_research/20260912T015409.486Z",
  diagnostics="matchup_diagnostics/20260912T015748.809Z",
  deduplicated_prepared="matchup_repaired/20260912T015412.707Z",
  deduplicated_research="matchup_research/20260912T015450.107Z",
  deduplicated_diagnostics="matchup_diagnostics/20260912T015856.129Z",
  duplicate_sources="matchup_deduplicated/20260912T014915.757Z",
  foundation_duplicates="matchup_foundation_audit/20260912T021612.534Z",
  turnover="matchup_turnover_audit/20260912T022858.354Z",
  v3_turnover="matchup_v3_turnover/20260912T024307.020Z",
  placebo="matchup_placebo/20260912T015740.290Z",
  play_contract="matchup_play_contract/20260912T022410.341Z",
  turnover_checks="matchup_turnover_checks/20260912T023331.623Z",
  box_audit="matchup_box_audit/20260912T032310.128Z",
  synthetic="matchup_synthetic_sensitivity/20260912T030640.332Z",
  current_contract="matchup_current_contract/20260912T031618.681Z",
  event_checks="matchup_event_checks/20260912T031411.869Z",
  drive_audit="matchup_drive_audit/20260912T033657.319Z",
  verification="matchup_verification/20260912T033506.857Z")
artifacts <- as.list(setNames(file.path(project,"cfb_v3/output/experiments",paths),names(paths)))
result <- run_matchup_report(project,artifacts)
cat("Research report:",file.path(result,"REPORT.md"),"\n")
