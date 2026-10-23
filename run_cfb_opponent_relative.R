# Offline incremental-value test of a pregame opponent-relative margin feature.
args <- commandArgs(trailingOnly=TRUE)
root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
if(!file.exists("cfb_v2/config.R")) stop("Run from repository root.")
for(module in c("config","store","features","coaches","adapters","models",
  "coach_migration","historical_data","preseason","bridge","workflow","live_data"))
  source(file.path("cfb_v2",paste0(module,".R")))
source("cfb_v3/experiments/opponent_relative_features.R")
say <- function(...) {cat(...,"\n");flush.console()}
history_dir <- "cfb_v3/output/experiments/margin_caps/20260927T211431Z"
input <- readRDS(file.path(history_dir,"uncapped_training_inputs.rds"))
hash_table <- read.csv(file.path(history_dir,"input_hashes.csv"),stringsAsFactors=FALSE)
stopifnot(identical(unname(tools::md5sum(hash_table$path)),hash_table$md5))
bundle_path <- "cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z_model.rds"
bundle <- readRDS(bundle_path)
base_features <- bundle$model$foundation$features
choices <- read.csv(file.path(history_dir,"fold_choices.csv"),stringsAsFactors=FALSE)
frozen <- read.csv(file.path(history_dir,"predictions.csv"),stringsAsFactors=FALSE)
frozen <- frozen[frozen$model=="uncapped",]
out <- if(length(args)) args[1] else file.path("cfb_v3/output/experiments/opponent_relative",
  format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"))
if(dir.exists(out)) stop("Refusing to overwrite existing experiment.")
dir.create(out,recursive=TRUE)
out <- normalizePath(out,winslash="/")
say("OUTPUT:",out)
code_files <- c("run_cfb_opponent_relative.R","cfb_v3/experiments/opponent_relative_features.R",
  "cfb_v3/experiments/OPPONENT_RELATIVE_PROTOCOL.md","cfb_v3/experiments/opponent_relative_report.py",
  "cfb_v3/experiments/margin_cap_report.py")
paths <- unique(c(hash_table$path,code_files,bundle_path,
  file.path(history_dir,c("uncapped_training_inputs.rds","predictions.csv","fold_choices.csv"))))
hashes <- tools::md5sum(paths)
stopifnot(!anyNA(hashes))
write.csv(data.frame(path=paths,md5=unname(hashes)),file.path(out,"input_hashes.csv"),row.names=FALSE)
file.copy("cfb_v3/experiments/OPPONENT_RELATIVE_PROTOCOL.md",file.path(out,"PROTOCOL.md"))
write.csv(choices,file.path(out,"fold_choices.csv"),row.names=FALSE)
games <- read_csv_if_present("cfb_v3/data/historical_games.csv",TRUE)
targets <- input$data
say("Building strictly past-only opponent baselines and team summaries")
built <- opponent_relative_build(games,targets)
stopifnot(identical(as.character(built$features$game_id),as.character(targets$game_id)))
data <- targets
data$opponent_relative_margin_diff <- built$features$opponent_relative_margin_diff
stopifnot(all(is.finite(data$opponent_relative_margin_diff)),
  identical(data[names(targets)],targets))
write.csv(built$features,file.path(out,"opponent_relative_features.csv"),row.names=FALSE,na="")
saveRDS(built,file.path(out,"feature_build.rds"))
if(!is.null(built$game_audit)) write.csv(built$game_audit,file.path(out,"game_audit.csv"),row.names=FALSE,na="")
if(!is.null(built$team_audit)) write.csv(built$team_audit,file.path(out,"team_audit.csv"),row.names=FALSE,na="")
if(!is.null(built$qa)) jsonlite::write_json(built$qa,file.path(out,"feature_qa.json"),pretty=TRUE,auto_unbox=TRUE,digits=14,na="null")
variants <- list(uncapped=base_features,
  opponent_relative=c(base_features,"opponent_relative_margin_diff"),
  no_counts=setdiff(base_features,c("source_games_diff","games_played_diff")),
  no_counts_opponent_relative=c(setdiff(base_features,c("source_games_diff","games_played_diff")),"opponent_relative_margin_diff"))
predictions <- all_models <- list()
for(label in names(variants)) {
  say("Refitting chronological folds:",label)
  results <- models <- list()
  for(i in seq_len(nrow(choices))) {
    choice <- choices[i,]; year <- choice$test_season
    train <- which(data$season<year); test <- which(data$season==year)
    stopifnot(length(train)==choice$training_rows,
      max(data$season[train])==choice$train_through_season,
      all(data$season[train]<min(data$season[test])))
    fold <- data
    fold$coach_rating_diff <- fold[[if(choice$coach_recent_share==.70)
      "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
    model <- fit_weighted_ridge(fold[train,],"margin",variants[[label]],
      input$weights[train]/max(input$weights[train]),choice$foundation_lambda)
    predicted <- predict(model,fold[test,])
    stopifnot(all(is.finite(predicted)))
    cols <- c("game_id","season","week","model_week","home","away","home_level",
      "away_level","postseason_type","is_cfp","neutral_site","closing_home_spread")
    results[[as.character(year)]] <- cbind(model=label,fold[test,cols],
      actual_margin=fold$margin[test],expected_margin=predicted)
    models[[as.character(year)]] <- model
  }
  predictions[[label]] <- do.call(rbind,results)
  all_models[[label]] <- models
}
p <- predictions$uncapped
idx <- match(p$game_id,frozen$game_id)
stopifnot(!anyNA(idx),nrow(p)==nrow(frozen),all(p$actual_margin==frozen$actual_margin[idx]))
control_error <- max(abs(p$expected_margin-frozen$expected_margin[idx]))
stopifnot(control_error<1e-8,identical(hashes,tools::md5sum(paths)))
write.csv(do.call(rbind,predictions),file.path(out,"predictions.csv"),row.names=FALSE,na="")
saveRDS(all_models,file.path(out,"fold_models.rds"))
write.csv(do.call(rbind,lapply(names(variants),function(label)
  data.frame(model=label,feature=variants[[label]]))),file.path(out,"feature_manifest.csv"),row.names=FALSE)
qa <- list(status="fitting_complete",as_of=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
  models=names(variants),feature_counts=vapply(variants,length,integer(1)),
  test_seasons=2022:2025,training_rows=nrow(data),predictions_per_variant=nrow(p),
  control_replay_max_error=control_error,source_inputs_unchanged=TRUE,
  targets_unchanged=TRUE,existing_predictors_unchanged=TRUE,weights_unchanged=TRUE,
  primary_slice="week_5_plus",fixed_shrinkage_pseudogames=3,
  production_changed=FALSE,previously_researched_seasons=TRUE)
jsonlite::write_json(qa,file.path(out,"manifest.json"),pretty=TRUE,auto_unbox=TRUE,digits=14)
say("Control replay maximum error:",control_error)
say("Complete:",out)
