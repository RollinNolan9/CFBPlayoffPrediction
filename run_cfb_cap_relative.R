# Offline factorial test: power cap x direct count predictors x relative feature.
args <- commandArgs(trailingOnly=TRUE)
root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
if(!file.exists("cfb_v2/config.R")) stop("Run from repository root.")
for(module in c("config","store","features","coaches","adapters","models",
  "coach_migration","historical_data","preseason","bridge","workflow","live_data"))
  source(file.path("cfb_v2",paste0(module,".R")))
say <- function(...) {cat(...,"\n");flush.console()}
cap_dir <- "cfb_v3/output/experiments/margin_caps/20260927T211431Z"
relative_dir <- "cfb_v3/output/experiments/opponent_relative/20260927T215407Z"
for(directory in c(cap_dir,relative_dir)) {
  qa <- jsonlite::read_json(file.path(directory,"manifest.json"),simplifyVector=TRUE)
  stopifnot(qa$status=="fitting_complete",isTRUE(qa$source_inputs_unchanged))
  prior_hashes <- read.csv(file.path(directory,"input_hashes.csv"),stringsAsFactors=FALSE)
  stopifnot(identical(unname(tools::md5sum(prior_hashes$path)),prior_hashes$md5))
}
bundle_path <- "cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z_model.rds"
bundle <- readRDS(bundle_path)
base_features <- bundle$model$foundation$features
stopifnot(length(base_features)==20,is.null(bundle$model$foundation$residual_forest))
choices <- read.csv(file.path(cap_dir,"fold_choices.csv"),stringsAsFactors=FALSE)
stopifnot(identical(as.integer(choices$test_season),2022:2025))
caps <- c("uncapped","cap_24","cap_42")
inputs <- setNames(lapply(caps,function(label) readRDS(file.path(cap_dir,paste0(label,"_training_inputs.rds")))),caps)
built <- readRDS(file.path(relative_dir,"feature_build.rds"))
features_csv <- read.csv(file.path(relative_dir,"opponent_relative_features.csv"),stringsAsFactors=FALSE)
stopifnot(isTRUE(all.equal(built$features,features_csv,tolerance=1e-12,check.attributes=FALSE)))
relative <- built$features$opponent_relative_margin_diff
control <- inputs$uncapped
stopifnot(identical(as.character(built$features$game_id),as.character(control$data$game_id)),
  all(is.finite(relative)),built$qa$shrinkage_prior_games==3)
allowed_changes <- c("home_power","away_power","home_power_rating","away_power_rating",
  "power_rating_diff","pregame_expected_margin","coach_rating_65_35_diff",
  "coach_rating_70_30_diff","coach_rating_diff")
for(label in caps) {
  candidate <- inputs[[label]]
  stopifnot(identical(candidate$data$game_id,control$data$game_id),
    identical(candidate$weights,control$weights),identical(candidate$data$margin,control$data$margin))
  fields <- setdiff(names(control$data),allowed_changes)
  stopifnot(isTRUE(all.equal(candidate$data[fields],control$data[fields],tolerance=0,check.attributes=FALSE)))
  inputs[[label]]$data$opponent_relative_margin_diff <- relative
}
variants <- expand.grid(cap=c("uncapped","24","42"),count_predictors=c(TRUE,FALSE),
  relative_feature=c(FALSE,TRUE),stringsAsFactors=FALSE)
variants$model <- paste0(ifelse(variants$cap=="uncapped","uncapped",paste0("cap_",variants$cap)),
  ifelse(variants$count_predictors,"","_no_counts"),ifelse(variants$relative_feature,"_relative",""))
variants$feature_count <- 18L+2L*variants$count_predictors+variants$relative_feature
variants <- variants[c("model","cap","count_predictors","relative_feature","feature_count")]
out <- if(length(args)) args[1] else file.path("cfb_v3/output/experiments/cap_relative",
  format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"))
if(dir.exists(out)) stop("Refusing to overwrite an existing experiment.")
dir.create(out,recursive=TRUE)
out <- normalizePath(out,winslash="/")
say("OUTPUT:",out)
prior_hashes <- read.csv(file.path(relative_dir,"input_hashes.csv"),stringsAsFactors=FALSE)
paths <- unique(c(prior_hashes$path,bundle_path,
  file.path(cap_dir,paste0(caps,"_training_inputs.rds")),
  file.path(cap_dir,c("predictions.csv","fold_choices.csv")),
  file.path(relative_dir,c("feature_build.rds","opponent_relative_features.csv","predictions.csv","independent_qa.json")),
  "run_cfb_cap_relative.R","cfb_v3/experiments/CAP_RELATIVE_PROTOCOL.md",
  "cfb_v3/experiments/cap_relative_report.py"))
hashes <- tools::md5sum(paths)
stopifnot(!anyNA(hashes))
write.csv(data.frame(path=paths,md5=unname(hashes)),file.path(out,"input_hashes.csv"),row.names=FALSE)
invisible(file.copy("cfb_v3/experiments/CAP_RELATIVE_PROTOCOL.md",file.path(out,"PROTOCOL.md")))
write.csv(variants,file.path(out,"variants.csv"),row.names=FALSE)
write.csv(choices,file.path(out,"fold_choices.csv"),row.names=FALSE)
write.csv(built$features,file.path(out,"opponent_relative_features.csv"),row.names=FALSE)
cap_frozen <- read.csv(file.path(cap_dir,"predictions.csv"),stringsAsFactors=FALSE)
relative_frozen <- read.csv(file.path(relative_dir,"predictions.csv"),stringsAsFactors=FALSE)
replay_map <- c(uncapped="uncapped",uncapped_relative="opponent_relative",
  uncapped_no_counts="no_counts",uncapped_no_counts_relative="no_counts_opponent_relative")
predictions <- all_models <- feature_sets <- list()
replay_checks <- list()
for(j in seq_len(nrow(variants))) {
  variant <- variants[j,]; label <- variant$model
  cap_label <- if(variant$cap=="uncapped") "uncapped" else paste0("cap_",variant$cap)
  input <- inputs[[cap_label]]
  data <- input$data
  features <- base_features
  if(!variant$count_predictors) features <- setdiff(features,c("source_games_diff","games_played_diff"))
  if(variant$relative_feature) features <- c(features,"opponent_relative_margin_diff")
  stopifnot(length(features)==variant$feature_count,identical(data$opponent_relative_margin_diff,relative))
  feature_sets[[label]] <- features
  results <- models <- list()
  say("Refitting prior-season models:",label)
  for(i in seq_len(nrow(choices))) {
    choice <- choices[i,]; year <- choice$test_season
    train <- which(data$season<year); test <- which(data$season==year)
    stopifnot(length(train)==choice$training_rows,
      max(data$season[train])==choice$train_through_season,
      all(data$season[train]<min(data$season[test])))
    fold <- data
    fold$coach_rating_diff <- fold[[if(choice$coach_recent_share==.70)
      "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
    model <- fit_weighted_ridge(fold[train,],"margin",features,
      input$weights[train]/max(input$weights[train]),choice$foundation_lambda)
    predicted <- predict(model,fold[test,])
    stopifnot(all(is.finite(predicted)))
    cols <- c("game_id","season","week","model_week","home","away","home_level",
      "away_level","postseason_type","is_cfp","neutral_site","closing_home_spread")
    results[[as.character(year)]] <- cbind(model=label,fold[test,cols],
      actual_margin=fold$margin[test],expected_margin=predicted)
    models[[as.character(year)]] <- model
  }
  p <- do.call(rbind,results)
  old <- NULL
  if(label %in% names(replay_map)) {
    old <- relative_frozen[relative_frozen$model==replay_map[[label]],]
  } else if(label %in% c("cap_24","cap_42")) {
    old <- cap_frozen[cap_frozen$model==label,]
  }
  if(!is.null(old)) {
    idx <- match(p$game_id,old$game_id)
    stopifnot(!anyNA(idx),nrow(p)==nrow(old),all(p$actual_margin==old$actual_margin[idx]))
    error <- max(abs(p$expected_margin-old$expected_margin[idx]))
    stopifnot(error<1e-8)
    replay_checks[[label]] <- data.frame(model=label,rows=nrow(p),max_prediction_error=error)
  }
  predictions[[label]] <- p
  all_models[[label]] <- models
}
replay_checks <- do.call(rbind,replay_checks)
stopifnot(nrow(replay_checks)==6,identical(hashes,tools::md5sum(paths)))
write.csv(do.call(rbind,predictions),file.path(out,"predictions.csv"),row.names=FALSE,na="")
write.csv(replay_checks,file.path(out,"control_replay.csv"),row.names=FALSE)
saveRDS(all_models,file.path(out,"fold_models.rds"))
write.csv(do.call(rbind,lapply(names(feature_sets),function(label)
  data.frame(model=label,feature=feature_sets[[label]]))),file.path(out,"feature_manifest.csv"),row.names=FALSE)
qa <- list(status="fitting_complete",as_of=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
  models=variants$model,test_seasons=2022:2025,training_rows=nrow(control$data),
  predictions_per_variant=nrow(predictions[[1]]),variant_count=nrow(variants),
  control_replay_max_error=max(replay_checks$max_prediction_error),control_replays=6L,
  source_inputs_unchanged=TRUE,targets_unchanged=TRUE,weights_unchanged=TRUE,
  protected_predictors_unchanged=TRUE,relative_feature_identical_across_caps=TRUE,
  relative_feature_uses_raw_margins=TRUE,fixed_shrinkage_pseudogames=3L,
  cap_scope="Opponent-adjusted power and dependent coach expectations only.",
  primary_slice="week_5_plus",primary_comparisons=4L,production_changed=FALSE,
  previously_researched_seasons=TRUE,feature_audit_source=file.path(relative_dir,"independent_qa.json"))
jsonlite::write_json(qa,file.path(out,"manifest.json"),pretty=TRUE,auto_unbox=TRUE,digits=14)
say("All six prior variants replayed; maximum error:",qa$control_replay_max_error)
say("Complete:",out)
