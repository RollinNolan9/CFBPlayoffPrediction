if(length(commandArgs(trailingOnly=TRUE)))stop("This fixed verification takes no arguments.")
script <- grep("^--file=",commandArgs(),value=TRUE)
project <- if(length(script))dirname(normalizePath(sub("^--file=","",script[1]),winslash="/")) else getwd()
setwd(project)
Sys.unsetenv("LC_ALL")
for(file in c("config.R","store.R","features.R","historical_data.R","models.R","workflow.R"))
  source(file.path("cfb_v2",file))
for(file in c("controlled_ats.R","external_ratings.R","tracker_edge.R","matchup_sources.R","matchup_features.R",
  "matchup_research.R","matchup_foundation_audit.R"))source(file.path("cfb_v3/experiments",file))
config <- cfb_v2_config(project,2026L)
before <- experiment_file_hashes(config)
base <- file.path(project,"cfb_v3/output/experiments")
prepared <- file.path(base,"matchup_prepared/20260912T015157.387Z")
reference <- file.path(base,"matchup_research/20260912T015409.486Z")
external_verify(prepared); external_verify(reference)
original <- read.csv(file.path(prepared,"protected_before.csv"),stringsAsFactors=FALSE)
if(!identical(before,original))stop("Production no longer matches the recorded research baseline.")
output <- external_new_dir(file.path(base,"matchup_verification"),"")
test_files <- c("test_matchup_research.R","test_prediction_tracker.R","test_external_backtest.R",
  "test_external_ratings.R","test_four_way.R","test_v3.R","test_tracker_edge.R",
  "test_v2.R","test_v3_artifacts.R")
tests <- list()
for(test_file in test_files) {
  message("Verifying ",test_file)
  path <- file.path("cfb_v2/tests",test_file)
  source(path,local=new.env(parent=globalenv()))
  tests[[test_file]] <- data.frame(file=test_file,test_blocks=sum(grepl("^test_that\\(",readLines(path,warn=FALSE))),
    status="passed",sha256=digest::digest(file=path,algo="sha256"))
}
fresh_prepared <- matchup_prepare(project,file.path(base,"prediction_tracker/20260912T003231.716Z"),
  file.path(base,"matchup_sources/20260912T011754.513Z"))
raw_checks <- list()
for(file in c("model_inputs.rds","team_game_counts.rds","feature_source_audit.csv",
  "provider_ledger.csv","selected_lines.csv","cohort_coverage.csv","play_source_inventory.csv")) {
  read <- if(grepl("\\.rds$",file))readRDS else function(p)read.csv(p,stringsAsFactors=FALSE)
  equal <- identical(read(file.path(prepared,file)),read(file.path(fresh_prepared,file)))
  if(!equal)stop("Raw-to-feature replay changed: ",file)
  raw_checks[[file]] <- data.frame(file=file,exactly_equal=equal)
}
data <- readRDS(file.path(fresh_prepared,"model_inputs.rds"))
replay <- matchup_walk(data)
checks <- list()
specs <- list(predictions=list(file="predictions.csv",key=c("game_id","model"),fields=c("expected_margin","signal","lambda")),
  candidates=list(file="lambda_candidates.csv",key=c("game_id","model","lambda"),fields=c("expected_margin","margin")),
  choices=list(file="lambda_choices.csv",key=c("model","test_season"),
    fields=c("lambda","train_through","training_rows","test_rows")))
for(name in names(specs)) {
  spec <- specs[[name]]; saved <- read.csv(file.path(reference,spec$file),stringsAsFactors=FALSE)
  if(nrow(saved)!=nrow(replay[[name]]))stop("Replay row count changed: ",name)
  checks[[name]] <- cbind(artifact=name,foundation_compare(saved,replay[[name]],spec$key,spec$fields))
}
checks <- do.call(rbind,checks)
if(any(checks$changed!=0 | checks$missing_mismatch!=0))stop("Fixed research replay changed.")
saved_models <- readRDS(file.path(reference,"models.rds"))
if(!isTRUE(all.equal(saved_models,replay$models,tolerance=1e-10)))stop("Saved model/recipe replay changed.")
after <- experiment_file_hashes(config)
if(!identical(before,after))stop("Production changed during verification.")
write.csv(do.call(rbind,tests),file.path(output,"test_results.csv"),row.names=FALSE)
write.csv(checks,file.path(output,"replay_comparison.csv"),row.names=FALSE)
write.csv(do.call(rbind,raw_checks),file.path(output,"raw_input_comparison.csv"),row.names=FALSE)
write.csv(after,file.path(output,"protected_after.csv"),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(output,"session_info.txt"))
saveRDS(list(created=external_stamp(),prepared=prepared,fresh_prepared=fresh_prepared,reference=reference,
  saved_models_equal=TRUE,protected_files=nrow(after)),file.path(output,"provenance.rds"))
file.copy("run_cfb_matchup_verify.R",file.path(output,"run_cfb_matchup_verify.R"))
external_seal(output)
cat("Verification:",output,"\n")
