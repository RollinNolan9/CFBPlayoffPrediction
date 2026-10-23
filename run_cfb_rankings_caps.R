# Offline Week 5 ranking comparison. Refit without direct game-count predictors.
# Rscript --vanilla run_cfb_rankings_caps.R [new_output_directory]
args <- commandArgs(trailingOnly=TRUE)
root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
if(!file.exists("cfb_v2/config.R")) stop("Run from the repository root.")
for(module in c("config","store","features","coaches","adapters","models",
  "coach_migration","historical_data","preseason","bridge","workflow","live_data"))
  source(file.path("cfb_v2",paste0(module,".R")))
source("cfb_v3/experiments/margin_cap_features.R")
source("cfb_v3/experiments/margin_cap_current_snapshots.R")
say <- function(...) {cat(...,"\n");flush.console()}
published_dir <- "cfb_v3/output/2026/week_5_rankings/20260927_espn_supplement"
historical_dir <- "cfb_v3/output/experiments/margin_caps/20260927T211431Z"
metadata <- jsonlite::read_json(file.path(published_dir,"qa.json"),simplifyVector=TRUE)
published <- read_csv_if_present(file.path(published_dir,"rankings.csv"),TRUE)
snapshots <- read_csv_if_present(file.path(published_dir,"team_snapshots.csv"),TRUE)
bundle <- readRDS(metadata$model_path)
config <- bundle$config
config$project_dir <- root
config$data_dir <- file.path(root,"cfb_v3/data")
config$inbox_dir <- file.path(root,"cfb_v3/inbox")
reference <- bundle$model$foundation
stopifnot(is.null(reference$residual_forest),metadata$week>=5,
  reference$ridge$lambda==32,all(reference$trained_seasons<metadata$season))
drop_features <- c("source_games_diff","games_played_diff")
features <- setdiff(reference$features,drop_features)
stopifnot(length(features)==18L,all(drop_features %in% reference$features))
hist_manifest <- jsonlite::read_json(file.path(historical_dir,"manifest.json"),simplifyVector=TRUE)
stopifnot(hist_manifest$status=="fitting_complete",isTRUE(hist_manifest$source_inputs_unchanged))
hist_hashes <- read.csv(file.path(historical_dir,"input_hashes.csv"),stringsAsFactors=FALSE)
stopifnot(identical(unname(tools::md5sum(hist_hashes$path)),hist_hashes$md5))
out <- if(length(args)) args[1] else file.path("cfb_v3/output/2026/week_5_rankings_caps",
  format(Sys.time(),"%Y%m%dT%H%M%SZ",tz="UTC"))
if(dir.exists(out)) stop("Refusing to overwrite an existing comparison.")
dir.create(out,recursive=TRUE)
out <- normalizePath(out,winslash="/")
say("OUTPUT:",out)
paths <- unique(c(hist_hashes$path,metadata$model_path,
  list.files(published_dir,pattern="\\.(csv|json)$",full.names=TRUE),
  file.path(historical_dir,paste0(c("uncapped","cap_24","cap_42"),"_training_inputs.rds")),
  file.path(historical_dir,paste0(c("uncapped","cap_24","cap_42"),"_rebuilt.rds")),
  "run_cfb_rankings_caps.R","cfb_v3/experiments/margin_cap_features.R",
  "cfb_v3/experiments/margin_cap_current_snapshots.R"))
hashes <- tools::md5sum(paths)
write.csv(data.frame(path=paths,md5=unname(hashes)),file.path(out,"input_hashes.csv"),row.names=FALSE)
control_input <- readRDS(file.path(historical_dir,"uncapped_training_inputs.rds"))
select_coach <- function(input) {
  input$data$coach_rating_diff <- input$data$coach_rating_65_35_diff
  input
}
control_input <- select_coach(control_input)
control_fit <- fit_weighted_ridge(control_input$data,"margin",reference$features,
  control_input$weights,reference$ridge$lambda)
coef_error <- max(abs(control_fit$coefficients-reference$ridge$coefficients))
train_replay_error <- max(abs(predict(control_fit,control_input$data)-predict(reference$ridge,control_input$data)))
stopifnot(coef_error<1e-8,train_replay_error<1e-8)
say("Published model refit parity:",coef_error,"coefficient error;",train_replay_error,"prediction error")
teams <- sort(published$team)
n <- length(teams)
stopifnot(n==metadata$team_count,nrow(snapshots)==n,setequal(teams,snapshots$team))
pair_index <- expand.grid(home_index=seq_len(n),away_index=seq_len(n))
pair_index <- pair_index[pair_index$home_index!=pair_index$away_index,]
schedule <- data.frame(game_id=paste0("cap_rank_pair_",seq_len(nrow(pair_index))),
  season=metadata$season,week=metadata$week,model_week=metadata$model_week,
  home=teams[pair_index$home_index],away=teams[pair_index$away_index],
  neutral_site=TRUE,postseason_type="regular")
rank_model <- function(model,team_features) {
  x <- make_matchup_features(schedule,team_features,team_features,.65,config)
  stopifnot(all(x$home_field_points==0))
  prediction <- predict(model,x)
  stopifnot(length(prediction)==nrow(pair_index),all(is.finite(prediction)))
  raw_matrix <- matrix(0,n,n,dimnames=list(teams,teams))
  raw_matrix[cbind(pair_index$home_index,pair_index$away_index)] <- prediction
  neutral <- (raw_matrix-t(raw_matrix))/2
  ratings <- rowMeans(neutral)
  stopifnot(max(abs(neutral+t(neutral)))<1e-10,abs(mean(ratings))<1e-10)
  order <- order(-ratings,teams)
  rankings <- published[match(teams[order],published$team),c("team","conference","wins","losses")]
  rankings <- cbind(rank=seq_len(n),rankings,rating=unname(ratings[order]))
  rownames(rankings) <- NULL
  residual <- neutral-outer(ratings,ratings,"-")
  list(rankings=rankings,features=x,raw_prediction=prediction,
    pairs=data.frame(team=schedule$home,opponent=schedule$away,raw_margin=prediction,
      neutral_margin=neutral[cbind(pair_index$home_index,pair_index$away_index)]),
    max_matchup_residual=max(abs(residual)))
}
replay <- rank_model(control_fit,snapshots)
published_error <- max(abs(replay$rankings$rating-published$rating[match(replay$rankings$team,published$team)]))
stopifnot(published_error<1e-8,identical(as.character(replay$rankings$team),as.character(published$team)))
say("Published ranking replay maximum error:",published_error)
cache <- margin_cap_current_load_inputs(config,published_dir,historical_dir)
comparison <- published[,c("team","conference","wins","losses","rank","rating")]
names(comparison)[5:6] <- c("published_rank","published_rating")
audits <- models <- all_rankings <- list()
for(cap in c(Inf,24,42)) {
  label <- if(is.infinite(cap)) "uncapped" else paste0("cap_",cap)
  say("Rebuilding current power and coach inputs:",label)
  current <- margin_cap_current_build(config,cap,cache=cache)
  input <- select_coach(readRDS(file.path(historical_dir,paste0(label,"_training_inputs.rds"))))
  stopifnot(identical(input$data$game_id,control_input$data$game_id),
    identical(input$data$margin,control_input$data$margin),identical(input$weights,control_input$weights))
  fit <- fit_weighted_ridge(input$data,"margin",features,input$weights,reference$ridge$lambda)
  stopifnot(!any(drop_features %in% fit$recipe$features),fit$training_rows==reference$trained_rows)
  result <- rank_model(fit,current$team_features)
  # Meaningful ablation check: changing the direct count columns cannot alter forecasts.
  changed_counts <- result$features
  changed_counts$source_games_diff <- 999
  changed_counts$games_played_diff <- -999
  count_error <- max(abs(predict(fit,changed_counts)-result$raw_prediction))
  stopifnot(count_error==0)
  idx <- match(comparison$team,result$rankings$team)
  comparison[[paste0(label,"_rank")]] <- result$rankings$rank[idx]
  comparison[[paste0(label,"_rating")]] <- result$rankings$rating[idx]
  comparison[[paste0(label,"_rank_change")]] <- comparison$published_rank-result$rankings$rank[idx]
  write.csv(result$rankings,file.path(out,paste0(label,"_rankings.csv")),row.names=FALSE)
  write.csv(head(result$rankings,25),file.path(out,paste0(label,"_top25.csv")),row.names=FALSE)
  write.csv(result$pairs,file.path(out,paste0(label,"_neutral_matchups.csv")),row.names=FALSE)
  write.csv(current$team_features,file.path(out,paste0(label,"_team_snapshots.csv")),row.names=FALSE)
  write.csv(data.frame(feature=names(fit$coefficients),coefficient=unname(fit$coefficients)),
    file.path(out,paste0(label,"_coefficients.csv")),row.names=FALSE)
  current$qa$count_perturbation_prediction_error <- count_error
  current$qa$max_matchup_residual <- result$max_matchup_residual
  audits[[label]] <- current$qa
  jsonlite::write_json(current$qa,file.path(out,paste0(label,"_qa.json")),pretty=TRUE,auto_unbox=TRUE,digits=14)
  models[[label]] <- fit
  all_rankings[[label]] <- result$rankings
  saveRDS(current,file.path(out,paste0(label,"_current_inputs.rds")))
  say("Top five:",paste(head(result$rankings$team,5),collapse=", "))
}
write.csv(comparison,file.path(out,"comparison.csv"),row.names=FALSE)
saveRDS(models,file.path(out,"refitted_models.rds"))
stopifnot(identical(hashes,tools::md5sum(paths)))
qa <- list(season=metadata$season,week=metadata$week,model_week=metadata$model_week,
  as_of=metadata$as_of,generated_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
  team_count=n,pair_count=nrow(pair_index),completed_week=metadata$completed_week,
  model_retrained=TRUE,features=features,removed_features=drop_features,
  training_rows=reference$trained_rows,training_seasons=reference$trained_seasons,
  fixed_lambda=reference$ridge$lambda,coach_recent_share=.65,
  count_policy="Both direct count predictors removed before refitting. Real counts retained in efficiency estimation and records.",
  cap_policy="Symmetric observed-margin cap in power fits before home-field subtraction; dependent coaching expectations rebuilt.",
  actual_targets_capped=FALSE,efficiency_inputs_capped=FALSE,
  source_fit_coefficient_error=coef_error,source_fit_prediction_error=train_replay_error,
  published_rating_replay_error=published_error,source_inputs_unchanged=TRUE,
  preseason_roster_component_used=FALSE,ap_poll_used_as_predictor=FALSE,
  production_model_changed=FALSE,published_site_changed=FALSE,
  reference_rankings=normalizePath(published_dir,winslash="/"),
  historical_cap_experiment=normalizePath(historical_dir,winslash="/"),
  validation_note="Ranking sensitivity comparison only. The earlier cap backtest retained count predictors and does not validate these combined variants.",
  data_note="Uses the same frozen inputs as the published rankings, including the labeled Oklahoma State vs. West Virginia ESPN play-by-play calculated-EPA supplement.",
  variants=audits)
jsonlite::write_json(qa,file.path(out,"qa.json"),pretty=TRUE,auto_unbox=TRUE,digits=14)
say("Ranking comparison complete:",out)
print(comparison[comparison$team %in% c("Southern California","James Madison","South Florida"),],row.names=FALSE)
