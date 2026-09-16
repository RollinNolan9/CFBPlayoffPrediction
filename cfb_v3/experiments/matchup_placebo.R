matchup_shuffle <- function(data,seed) {
  set.seed(seed)
  fields <- grep("^mx_",names(data),value=TRUE)
  assert_columns(data,c("season","feature_week_start","is_cfp",fields),"matchup permutation")
  result <- data
  blocks <- split(seq_len(nrow(data)),paste(data$season,data$feature_week_start,data$is_cfp,sep="\r"))
  for (rows in blocks) {
    order <- rows[sample.int(length(rows))]
    result[rows,fields] <- data[order,fields]
  }
  result
}

matchup_placebo_score <- function(predictions) {
  do.call(rbind,lapply(split(predictions,predictions$model),function(x) {
    baseline <- predictions[predictions$model=="linear_components", ]
    reference <- baseline$expected_margin[match(x$game_id,baseline$game_id)]
    side <- sign(x$signal); decisive <- side!=0 & x$market_residual!=0
    strong <- abs(x$signal)>=2; win <- side*x$market_residual>0; loss <- side*x$market_residual<0
    data.frame(model=x$model[1],mae=mean(abs(x$expected_margin-x$margin)),
      gain_over_market=mean(abs(x$market_margin-x$margin)-abs(x$expected_margin-x$margin)),
      gain_over_linear=mean(abs(reference-x$margin)-abs(x$expected_margin-x$margin)),
      ats=mean(win[decisive]),edge2_picks=sum(strong),
      edge2_ats=if(any(strong & decisive))mean(win[strong & decisive]) else NA_real_,
      edge2_units=sum(ifelse(strong & win,10/11,ifelse(strong & loss,-1,0))))
  }))
}

run_matchup_placebo <- function(project,prepared_dir,research_dir,repetitions=499L) {
  if(length(repetitions)!=1L || !repetitions %in% c(2L,499L))stop("Use two smoke draws or the locked 499-draw diagnostic.")
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  external_verify(prepared_dir); external_verify(research_dir)
  data <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  actual <- read.csv(file.path(research_dir,"predictions.csv"),stringsAsFactors=FALSE)
  observed <- matchup_placebo_score(actual)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_placebo"),"")
  draws <- timings <- vector("list",repetitions)
  for (iteration in seq_len(repetitions)) {
    start <- proc.time()[3]
    shuffled <- matchup_shuffle(data,947100L+iteration)
    fit <- matchup_walk(shuffled)
    reference <- actual[actual$model=="external_market", ]
    baseline <- fit$predictions[fit$predictions$model=="external_market", ]
    i <- match(reference$game_id,baseline$game_id)
    if(!isTRUE(all.equal(reference$expected_margin,baseline$expected_margin[i],tolerance=1e-10)))
      stop("Negative control changed the unchanged external baseline.")
    draws[[iteration]] <- cbind(iteration=iteration,matchup_placebo_score(fit$predictions))
    timings[[iteration]] <- data.frame(iteration=iteration,elapsed_seconds=proc.time()[3]-start)
    if(iteration %% 10L == 0L || iteration==repetitions) {
      message("Grouped matchup negative controls: ",iteration,"/",repetitions)
      write.csv(do.call(rbind,draws[seq_len(iteration)]),file.path(output,"draws.csv"),row.names=FALSE)
    }
    rm(fit,shuffled); gc(verbose=FALSE)
  }
  draws <- do.call(rbind,draws); comparison <- list()
  for (name in observed$model) {
    x <- draws[draws$model==name, ]; o <- observed[observed$model==name, ]
    comparison[[name]] <- data.frame(model=name,actual_mae=o$mae,placebo_mean_mae=mean(x$mae),
      placebo_mae_low=quantile(x$mae,.025),placebo_mae_high=quantile(x$mae,.975),
      fraction_placebos_lower_mae=mean(x$mae<o$mae),actual_gain_over_linear=o$gain_over_linear,
      placebo_mean_gain_over_linear=mean(x$gain_over_linear),
      interpretation="negative_control_not_a_calibrated_conditional_p_value")
  }
  write.csv(observed,file.path(output,"observed.csv"),row.names=FALSE)
  write.csv(do.call(rbind,comparison),file.path(output,"comparison.csv"),row.names=FALSE)
  write.csv(do.call(rbind,timings),file.path(output,"timings.csv"),row.names=FALSE)
  saveRDS(list(prepared_dir=prepared_dir,research_dir=research_dir,repetitions=repetitions,
    seed_start=947101L,created=external_stamp(),
    rule="Joint mx feature vectors permuted within season/feature_week_start/CFP blocks; targets, market, external ratings and power held fixed",
    limits="Breaks conditional associations with fixed ratings; reused history; diagnostic not exact inference"),file.path(output,"provenance.rds"))
  for(file in c("matchup_placebo.R","MATCHUP_PROTOCOL.md"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during negative controls.")
  external_seal(output)
  output
}
