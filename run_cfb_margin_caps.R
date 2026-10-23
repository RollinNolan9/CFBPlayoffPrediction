# Offline research only: refit the ranking foundation with capped power inputs.
args <- commandArgs(trailingOnly=TRUE)
script <- grep("^--file=",commandArgs(),value=TRUE)
root <- dirname(normalizePath(sub("^--file=","",script[1]),winslash="/"))
setwd(root)
for (module in c("config","store","features","coaches","adapters","models",
                 "coach_migration","historical_data","preseason","bridge","workflow","live_data")) {
  source(file.path("cfb_v2",paste0(module,".R")))
}
source("cfb_v3/experiments/margin_cap_features.R")
config <- cfb_v2_config(root,2026L)
out <- if(length(args)) args[1] else file.path(config$output_dir,"experiments",
  "margin_caps",format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"))
resume <- length(args) >= 2L && identical(args[2], "--resume")
if(dir.exists(out) && !resume) stop("Refusing to overwrite an experiment directory without --resume.")
if(resume && !file.exists(file.path(out,"manifest.json"))) stop("Resume requires an existing experiment manifest.")
if(!dir.exists(out)) dir.create(out,recursive=TRUE)
out <- normalizePath(out,winslash="/")
say <- function(...) { cat(...,"\n"); flush.console() }
say("OUTPUT:",out)
if(resume) {
  prior_hashes <- read.csv(file.path(out,"input_hashes.csv"),stringsAsFactors=FALSE)
  prior_code <- read.csv(file.path(out,"experiment_code_hashes.csv"),stringsAsFactors=FALSE)
  stable_code <- prior_code[prior_code$path != "run_cfb_margin_caps.R",]
  stopifnot(identical(unname(tools::md5sum(prior_hashes$path)),prior_hashes$md5),
    identical(unname(tools::md5sum(stable_code$path)),stable_code$md5),
    unname(tools::md5sum("cfb_v3/experiments/MARGIN_CAP_PROTOCOL.md")) ==
      unname(tools::md5sum(file.path(out,"PROTOCOL.md"))))
} else file.copy("cfb_v3/experiments/MARGIN_CAP_PROTOCOL.md",file.path(out,"PROTOCOL.md"))
manifest <- list(status="started",started_at=format(Sys.time(),tz="UTC",usetz=TRUE),
  caps=c("uncapped","24","25","35","42"),model="foundation_ridge",
  count_features_retained=TRUE,targets_capped=FALSE,automatic_promotion=FALSE,
  primary_slice="week_5_plus",test_seasons=2022:2025,
  note="Historical research; prior experiments have used these seasons. No production changes.")
if(resume) {
  manifest <- jsonlite::read_json(file.path(out,"manifest.json"),simplifyVector=TRUE)
  manifest$resumed_at <- format(Sys.time(),tz="UTC",usetz=TRUE)
  manifest$resume_policy <- "Validated frozen input hashes; reuse independently rebuilt feature caches, then refit and verify all variants."
}
save_manifest <- function() jsonlite::write_json(manifest,file.path(out,"manifest.json"),
  pretty=TRUE,auto_unbox=TRUE,na="null",digits=14)
save_manifest()
bundle_path <- "cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z_model.rds"
bundle <- readRDS(bundle_path)
features <- bundle$model$foundation$features
stopifnot(is.null(bundle$model$foundation$residual_forest),
          all(c("source_games_diff","games_played_diff","power_rating_diff") %in% features))
choices_path <- "cfb_v3/output/experiments/four_way/20260909T214828Z/fold_choices.csv"
choices <- read_csv_if_present(choices_path,TRUE)
choices <- choices[choices$model=="v3_control",]
stopifnot(identical(as.integer(choices$test_season),2022:2025))
write.csv(choices,file.path(out,"fold_choices.csv"),row.names=FALSE)
write.csv(data.frame(feature=features),file.path(out,"feature_manifest.csv"),row.names=FALSE)
frozen_path <- file.path(config$output_dir,"backtest","rolling_predictions.csv")
frozen <- read_csv_if_present(frozen_path,TRUE)
frozen <- frozen[frozen$model=="preseason_ablation_challenger",]
assert_unique_keys(frozen,"game_id","saved foundation forecasts")
input_files <- c(bundle_path,choices_path,frozen_path,
  list.files(config$data_dir,"\\.csv$",full.names=TRUE),
  list.files(config$inbox_dir,"\\.csv$",full.names=TRUE),
  list.files("cfb_v2","\\.R$",full.names=TRUE))
input_files <- sort(unique(input_files[file.exists(input_files)]))
before <- tools::md5sum(input_files)
write.csv(data.frame(path=input_files,md5=unname(before)),file.path(out,"input_hashes.csv"),row.names=FALSE)
code_files <- c("run_cfb_margin_caps.R","cfb_v3/experiments/margin_cap_worker.R","cfb_v3/experiments/MARGIN_CAP_PROTOCOL.md",
  "cfb_v3/experiments/margin_cap_features.R","cfb_v3/experiments/margin_cap_report.py")
write.csv(data.frame(path=code_files,md5=unname(tools::md5sum(code_files))),
          file.path(out,"experiment_code_hashes.csv"),row.names=FALSE)
cache <- margin_cap_load_inputs(config)
team_games <- read_csv_if_present(file.path(config$data_dir,"historical_team_games.csv"),TRUE)
members <- combine_membership(read_csv_if_present(file.path(config$data_dir,"historical_fbs_membership.csv"),TRUE))
prepare <- function(raw) {
  x <- prepare_training_data(raw[raw$season<=2025,],config)
  x$data <- apply_fbs_bridge_backtest_features(x$data,team_games,members,config)
  x
}
fit_predict <- function(input,label) {
  data <- input$data
  assert_columns(data,features,"foundation cap features")
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
    w <- input$weights[train]/max(input$weights[train])
    model <- fit_weighted_ridge(fold[train,],"margin",features,w,choice$foundation_lambda)
    predicted <- predict(model,fold[test,])
    stopifnot(all(is.finite(predicted)))
    cols <- c("game_id","season","week","model_week","home","away","home_level",
      "away_level","postseason_type","is_cfp","neutral_site","closing_home_spread")
    results[[as.character(year)]] <- cbind(model=label,fold[test,cols],
      actual_margin=fold$margin[test],expected_margin=predicted)
    models[[as.character(year)]] <- model
  }
  saveRDS(models,file.path(out,paste0(label,"_fold_models.rds")))
  do.call(rbind,results)
}
# First establish the cached control before rebuilding any power histories.
cached <- prepare(read_csv_if_present(file.path(config$inbox_dir,"training_games.csv"),TRUE))
cached_predictions <- fit_predict(cached,"cached_control")
i <- match(cached_predictions$game_id,frozen$game_id)
stopifnot(!anyNA(i),nrow(cached_predictions)==nrow(frozen),
  max(abs(cached_predictions$actual_margin-frozen$actual_margin[i]))<1e-10)
cached_error <- max(abs(cached_predictions$expected_margin-frozen$expected_margin[i]))
say("Cached foundation replay max error:",cached_error)
if(cached_error>1e-8) stop("Cached foundation did not reproduce saved forecasts.")
manifest$cached_replay_max_error <- cached_error
save_manifest()
all_predictions <- list()
for(cap in c(Inf,24,25,35,42)) {
  label <- if(is.infinite(cap)) "uncapped" else paste0("cap_",cap)
  rebuilt_path <- file.path(out,paste0(label,"_rebuilt.rds"))
  prepared_path <- file.path(out,"prepared",paste0(label,".rds"))
  if(resume && (file.exists(rebuilt_path) || file.exists(prepared_path))) {
    say("Loading previously rebuilt inputs:",label)
    built <- readRDS(if(file.exists(rebuilt_path)) rebuilt_path else prepared_path)
    stopifnot(identical(built$qa$cap,if(is.infinite(cap)) "uncapped" else as.character(cap)),
      identical(built$qa$source_md5,as.list(cache$source_md5)),
      isTRUE(built$qa$protected_fields_unchanged),isTRUE(built$qa$chronological_pass),
      isTRUE(built$qa$kickoff_cutoff_pass))
  } else {
    say("Building chronological power and coach inputs:",label)
    built <- margin_cap_build(config,cap,cache)
  }
  saveRDS(built,rebuilt_path)
  write.csv(built$snapshot_audit,file.path(out,paste0(label,"_snapshot_audit.csv")),row.names=FALSE,na="")
  jsonlite::write_json(built$qa,file.path(out,paste0(label,"_qa.json")),pretty=TRUE,auto_unbox=TRUE,na="null",digits=14)
  input <- prepare(built$data)
  stopifnot(identical(as.character(input$data$game_id),as.character(cached$data$game_id)),
            isTRUE(all.equal(input$weights,cached$weights)),
            isTRUE(all.equal(input$data$margin,cached$data$margin)))
  for(f in c("source_games_diff","games_played_diff"))
    stopifnot(isTRUE(all.equal(input$data[[f]],cached$data[[f]])))
  saveRDS(input,file.path(out,paste0(label,"_training_inputs.rds")))
  write.csv(input$data[c("game_id","season","model_week","margin",features)],
            file.path(out,paste0(label,"_features.csv")),row.names=FALSE,na="")
  say("Refitting prior-season models:",label)
  p <- fit_predict(input,label)
  if(label=="uncapped") {
    delta <- max(abs(p$expected_margin-cached_predictions$expected_margin))
    say("Rebuilt uncapped replay max error:",delta)
    manifest$rebuilt_replay_max_error <- delta
    if(delta>1e-8) stop("Rebuilt uncapped model changed the saved control.")
  }
  all_predictions[[label]] <- p
  write.csv(do.call(rbind,all_predictions),file.path(out,"predictions.csv"),row.names=FALSE,na="")
  manifest$completed_variants <- names(all_predictions)
  save_manifest()
}
after <- tools::md5sum(input_files)
stopifnot(identical(before,after))
manifest$source_inputs_unchanged <- TRUE
manifest$status <- "fitting_complete"
manifest$completed_at <- format(Sys.time(),tz="UTC",usetz=TRUE)
manifest$predictions_per_variant <- nrow(all_predictions[[1]])
save_manifest()
say("Complete. Run margin_cap_report.py on",out)
