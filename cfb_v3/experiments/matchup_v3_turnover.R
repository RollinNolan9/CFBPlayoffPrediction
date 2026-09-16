run_matchup_v3_turnover <- function(project,tracker_dir,prepared_dir,turnover_dir) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  for(directory in c(tracker_dir,prepared_dir,turnover_dir))external_verify(directory)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_v3_turnover"),"")
  message("Full-v3 turnover impact check: ",output)
  joined <- read.csv(file.path(tracker_dir,"joined_inputs.csv"),stringsAsFactors=FALSE)
  prepared <- tracker_prepare(config,joined)
  input <- list(data=prepared$full_data,weights=prepared$full_weights,covered_seasons=prepared$covered_seasons)
  message("Reproducing full frozen v3 and preserving its original fold choices")
  control <- experiment_football(input$data,input$weights,config,input$covered_seasons,"v3_original")
  frozen <- read.csv(file.path(config$output_dir,"backtest/rolling_predictions.csv"),stringsAsFactors=FALSE)
  frozen <- frozen[frozen$model=="ridge_core", ]
  parity <- foundation_compare(frozen,control$predictions,"game_id","expected_margin",1e-8)
  if(any(parity$missing_mismatch+parity$changed>0) || nrow(frozen)!=nrow(control$predictions))
    stop("Full-v3 frozen parity failed.")
  write.csv(parity,file.path(output,"frozen_parity.csv"),row.names=FALSE)
  fields <- control$coach$features
  fixed <- foundation_fixed_forecasts(input,control$choices,fields,config)
  parity <- foundation_compare(frozen,fixed,"game_id","expected_margin",1e-8)
  if(any(parity$missing_mismatch+parity$changed>0))stop("Fixed-choice full-v3 parity failed.")
  write.csv(parity,file.path(output,"fixed_choice_parity.csv"),row.names=FALSE)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  games <- games[games$season<=2025, ]; games$kickoff <- parse_utc_datetime(games$kickoff)
  original <- read_csv_if_present(file.path(config$data_dir,"historical_team_games.csv"),TRUE)
  original <- original[original$season<=2025, ]
  corrected <- readRDS(file.path(turnover_dir,"cleaned_team_games.rds"))
  members <- read.csv(file.path(config$data_dir,"historical_fbs_membership.csv"),stringsAsFactors=FALSE)
  members <- members[members$season<=2025, ]
  power <- unique(rbind(
    data.frame(team=games$home,season=games$season,model_week=games$model_week,power_rating=games$home_power),
    data.frame(team=games$away,season=games$season,model_week=games$model_week,power_rating=games$away_power)))
  assert_unique_keys(power,c("team","season","model_week"),"frozen score-derived power")
  rebuild <- function(team_games) {
    priors <- fbs_bridge_prior_rows(team_games,membership_fbs_flags(members),config)
    built <- build_team_pregame_snapshots(team_games,games,power,config,priors)
    x <- build_historical_matchup_table(built$games,built$snapshots,config)
    apply_fbs_bridge_backtest_features(x,team_games,combine_membership(members),config)
  }
  message("Checking full-population reconstructed feature parity")
  raw_old <- rebuild(original); raw_new <- rebuild(corrected)
  changed_fields <- intersect(fields,names(raw_old))
  parity <- foundation_compare(input$data,raw_old,"game_id",changed_fields)
  write.csv(parity,file.path(output,"feature_parity.csv"),row.names=FALSE)
  if(any(parity$missing_mismatch+parity$changed>0))stop("Full-population feature parity failed.")
  candidate <- input; i <- match(candidate$data$game_id,raw_new$game_id)
  if(anyNA(i))stop("Corrected full-v3 features lost games.")
  candidate$data[changed_fields] <- raw_new[i,changed_fields]
  corrected_predictions <- foundation_fixed_forecasts(candidate,control$choices,fields,config)
  source_quotes <- input$data[input$data$season %in% 2022:2025 & raw_history_eligible(input$data), ]
  source_quotes <- source_quotes[is.finite(source_quotes$closing_home_spread), ]
  source_quotes$market_margin <- -source_quotes$closing_home_spread
  source_quotes$market_residual <- source_quotes$margin-source_quotes$market_margin
  source_quotes$quote_provider <- "original_PBP_unverified"
  source_quotes$quote_support <- 0
  qualified <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  qualified <- qualified[qualified$season %in% 2022:2025, ]
  rows <- list()
  for(lane in c("qualified_quotes","original_pbp_quotes"))for(profile in c("original","turnover_corrected")) {
    x <- if(lane=="qualified_quotes")qualified else source_quotes
    p <- if(profile=="original")fixed else corrected_predictions
    x$expected_margin <- p$expected_margin[match(x$game_id,p$game_id)]
    if(any(!is.finite(x$expected_margin)))stop("Missing full-v3 forecast.")
    x$signal <- x$expected_margin-x$market_margin
    x$model <- paste0("v3_",profile); x$lane <- lane; x$home_cover_probability <- NA_real_
    for(threshold in c(0,3)) {
      x$policy <- paste0(x$model,if(threshold==0)"_all" else "_edge3")
      x$eligible <- abs(x$signal)>=threshold
      rows[[paste(lane,profile,threshold)]] <- edge_grade(x)
    }
  }
  policies <- do.call(bind_rows_fill,rows)
  metrics <- do.call(rbind,lapply(split(policies,policies$lane),function(x)cbind(lane=x$lane[1],matchup_metrics(x))))
  write.csv(policies,file.path(output,"policies.csv"),row.names=FALSE)
  write.csv(metrics,file.path(output,"metrics.csv"),row.names=FALSE)
  write.csv(control$choices,file.path(output,"fixed_fold_choices.csv"),row.names=FALSE)
  saveRDS(candidate,file.path(output,"corrected_training_inputs.rds"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  saveRDS(list(tracker_dir=tracker_dir,prepared_dir=prepared_dir,turnover_dir=turnover_dir,
    created=external_stamp(),correction="Prefer actual turnover flag; no classification changes or duplicate removal",
    production_promotion=FALSE),file.path(output,"provenance.rds"))
  for(file in c("MATCHUP_PROTOCOL.md","matchup_v3_turnover.R","matchup_foundation_audit.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during full-v3 impact audit.")
  external_seal(output)
  output
}
