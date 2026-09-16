matchup_prepare <- function(project, tracker_dir, lines_dir) {
  config <- cfb_v2_config(project,2026L)
  protected <- experiment_file_hashes(config)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_prepared"),"")
  external_verify(tracker_dir); external_verify(lines_dir)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  games$game_id <- as.character(games$game_id)
  captures <- matchup_qualify_lines(matchup_read_lines(lines_dir,games))
  write.csv(captures$ledger,file.path(output,"provider_ledger.csv"),row.names=FALSE)
  write.csv(captures$selected,file.path(output,"selected_lines.csv"),row.names=FALSE)
  prepared <- readRDS(file.path(tracker_dir,"training_inputs.rds"))
  x <- prepared$data; x$game_id <- as.character(x$game_id)
  x$weight <- prepared$weights
  i <- match(x$game_id,captures$selected$game_id)
  coverage <- x[c("game_id","season","week","home","away","closing_home_spread")]
  coverage$qualified_quote <- !is.na(i)
  coverage$qualified_spread <- captures$selected$spread[i]
  coverage$pbp_difference <- coverage$qualified_spread-coverage$closing_home_spread
  write.csv(coverage,file.path(output,"cohort_coverage.csv"),row.names=FALSE)
  x <- x[!is.na(i), ]; i <- i[!is.na(i)]
  x$original_pbp_spread <- x$closing_home_spread
  x$closing_home_spread <- captures$selected$spread[i]
  x$quote_provider <- captures$selected$provider[i]
  x$quote_support <- captures$selected$supporting_families[i]
  frozen <- read.csv(file.path(tracker_dir,"predictions.csv"),stringsAsFactors=FALSE)
  frozen <- frozen[frozen$model == "v3_frozen", ]; assert_unique_keys(frozen,"game_id","frozen forecast")
  x$v3_margin <- frozen$expected_margin[match(x$game_id,as.character(frozen$game_id))]
  x$market_margin <- -x$closing_home_spread
  x$market_residual <- x$margin-x$market_margin
  x$sagarin_edge <- x$challenger_tracker_sagarin-x$market_margin
  x$fpi_edge <- x$challenger_tracker_fpi-x$market_margin
  x$external_edge <- (x$sagarin_edge+x$fpi_edge)/2
  x$v3_edge <- x$v3_margin-x$market_margin
  if (any(!is.finite(as.matrix(x[c("margin","market_margin","sagarin_edge","fpi_edge","weight")]))))
    stop("Invalid primary inputs.")
  if (any(!is.finite(x$v3_margin[x$season %in% 2022:2025]))) stop("Missing frozen test forecast.")
  if (any(!raw_history_eligible(x))) stop("Non-FBS/non-CFP bowl in primary data.")
  source_inventory <- team_games <- audit <- list()
  for (year in 2020:2025) {
    message("Aggregating causal matchup source counts: ",year)
    path <- file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",year,"_compact.rds"))
    p <- readRDS(path); built <- matchup_aggregate_plays(p,games)
    team_games[[as.character(year)]] <- built$team_games
    audit[[as.character(year)]] <- cbind(season=year,built$audit)
    source_inventory[[as.character(year)]] <- data.frame(file=path,sha256=digest::digest(file=path,algo="sha256"))
    rm(p); gc(verbose=FALSE)
  }
  history <- do.call(rbind,team_games); rownames(history) <- NULL
  message("Constructing strictly preweek matchup snapshots: ",nrow(x)," target games")
  built <- matchup_build_features(history,x)
  i <- match(x$game_id,built$features$game_id)
  x <- cbind(x,built$features[i,setdiff(names(built$features),"game_id"),drop=FALSE])
  assert_unique_keys(x,"game_id","matchup model inputs")
  saveRDS(history,file.path(output,"team_game_counts.rds"))
  saveRDS(x,file.path(output,"model_inputs.rds"))
  write.csv(built$audit,file.path(output,"feature_source_audit.csv"),row.names=FALSE)
  write.csv(do.call(rbind,audit),file.path(output,"play_source_audit.csv"),row.names=FALSE)
  write.csv(do.call(rbind,source_inventory),file.path(output,"play_source_inventory.csv"),row.names=FALSE)
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if (!identical(protected,experiment_file_hashes(config))) stop("Production changed during preparation.")
  saveRDS(list(tracker_dir=tracker_dir,lines_dir=lines_dir,created=external_stamp(),
    production_unchanged=TRUE),file.path(output,"provenance.rds"))
  for (file in c("MATCHUP_PROTOCOL.md","matchup_sources.R","matchup_features.R","matchup_research.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  external_seal(output)
  output
}

matchup_variants <- function() {
  groups <- matchup_feature_groups(); base <- c("sagarin_edge","fpi_edge","market_margin")
  linear <- c(base,groups$linear)
  list(external_market=base,linear_components=linear,style=c(linear,groups$style),
    sacks=c(linear,groups$sacks),high_impact=c(linear,groups$impact),
    form_shift=c(linear,groups$form),all_matchups=c(linear,unlist(groups[-1],use.names=FALSE)))
}

matchup_refresh_features <- function(project,prepared_dir,reason) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  external_verify(prepared_dir)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_repaired"),"")
  x <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  history <- readRDS(file.path(prepared_dir,"team_game_counts.rds"))
  built <- matchup_build_features(history,x); fields <- setdiff(names(built$features),"game_id")
  i <- match(x$game_id,built$features$game_id); x[fields] <- built$features[i,fields]
  saveRDS(x,file.path(output,"model_inputs.rds")); saveRDS(history,file.path(output,"team_game_counts.rds"))
  write.csv(built$audit,file.path(output,"feature_source_audit.csv"),row.names=FALSE)
  for (file in c("play_source_inventory.csv","duplicate_source_audit.csv","verified_removed_rows.csv"))
    if(file.exists(file.path(prepared_dir,file)))file.copy(file.path(prepared_dir,file),file.path(output,file))
  parent <- prepared_dir; seen <- character()
  while(!file.exists(file.path(parent,"provider_ledger.csv"))) {
    if(parent %in% seen)stop("Cyclic prepared provenance.")
    seen <- c(seen,parent)
    parent <- readRDS(file.path(parent,"provenance.rds"))$prepared_dir
    if(is.null(parent))stop("Cannot resolve original quote ledger.")
    external_verify(parent)
  }
  for (file in c("provider_ledger.csv","selected_lines.csv","cohort_coverage.csv"))
    file.copy(file.path(parent,file),file.path(output,file))
  for (file in c("MATCHUP_PROTOCOL.md","matchup_features.R","matchup_research.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  saveRDS(list(prepared_dir=prepared_dir,reason=reason,created=external_stamp()),file.path(output,"provenance.rds"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during feature correction.")
  external_seal(output)
  output
}

matchup_fit <- function(data, features, lambda) {
  allowed <- unique(unlist(matchup_variants(),use.names=FALSE))
  if (length(setdiff(features,allowed)) || anyDuplicated(features)) stop("Prohibited matchup feature schema.")
  assert_columns(data,c(features,"market_residual","weight","season"),"matchup fitting")
  if (any(!is.finite(data$season)) || any(!is.finite(data$weight)) || any(data$weight<=0)) stop("Invalid training season/weight.")
  fit <- fit_weighted_ridge(data,"market_residual",features,data$weight/max(data$weight),lambda)
  list(model=fit,features=features,train_through=max(data$season))
}

matchup_predict <- function(object,data) {
  assert_columns(data,c("season",object$features),"matchup forecast")
  if (any(!is.finite(data$season)) || any(data$season<=object$train_through)) stop("Forecast overlaps training seasons.")
  if (!identical(object$features,object$model$recipe$features)) stop("Model feature schema drift.")
  as.numeric(predict(object$model,data))
}

matchup_choose_lambda <- function(candidate,year) {
  if (is.null(candidate) || !nrow(candidate) || !any(candidate$season<year)) return(128)
  past <- candidate[candidate$season<year, ]; past$error <- abs(past$expected_margin-past$margin)
  mae <- aggregate(error ~ lambda,past,mean)
  mae$lambda[order(mae$error,mae$lambda)][1]
}

matchup_walk <- function(data) {
  forecasts <- candidates <- coefficients <- choices <- models <- list()
  for (name in names(matchup_variants())) {
    features <- matchup_variants()[[name]]; previous <- NULL
    for (year in 2022:2025) {
      train <- data[data$season<year, ]; test <- data[data$season==year, ]
      if (!nrow(test)) stop("Missing matchup evaluation season.")
      chosen <- matchup_choose_lambda(previous,year)
      for (lambda in c(32,128,512)) {
        model <- matchup_fit(train,features,lambda)
        pred <- test; pred$signal <- matchup_predict(model,test)
        pred$expected_margin <- pred$market_margin+pred$signal
        pred$model <- name; pred$lambda <- lambda; pred$home_cover_probability <- NA_real_
        key <- paste(name,year,lambda,sep="_")
        candidate <- pred[c("game_id","season","model","lambda","margin","expected_margin")]
        candidates[[key]] <- candidate
        previous <- rbind(previous,candidate)
        if (lambda == chosen) {
          forecasts[[paste(name,year)]] <- pred; models[[paste(name,year)]] <- model
          choices[[paste(name,year)]] <- data.frame(model=name,test_season=year,lambda=lambda,
            train_through=max(train$season),training_rows=nrow(train),test_rows=nrow(test))
          beta <- model$model$coefficients
          coefficients[[paste(name,year)]] <- data.frame(model=name,test_season=year,term=names(beta),coefficient=unname(beta))
        }
      }
    }
  }
  list(predictions=do.call(rbind,forecasts),candidates=do.call(rbind,candidates),
    choices=do.call(rbind,choices),coefficients=do.call(rbind,coefficients),models=models)
}

matchup_policies <- function(data,forecasts) {
  result <- list()
  add <- function(x,name,threshold) {
    x$policy <- name; x$eligible <- abs(x$signal)>=threshold
    result[[name]] <<- edge_grade(x)
  }
  for (name in unique(forecasts$model)) {
    x <- forecasts[forecasts$model==name, ]; add(x,paste0(name,"_all"),0); add(x,paste0(name,"_edge2"),2)
  }
  for (name in c("v3","external")) {
    x <- data[data$season %in% 2022:2025, ]; x$model <- name; x$lambda <- NA_real_
    x$signal <- x[[paste0(name,"_edge")]]; x$expected_margin <- x$market_margin+x$signal
    x$home_cover_probability <- NA_real_
    add(x,paste0(name,"_all"),0); add(x,paste0(name,"_edge3"),3)
  }
  out <- do.call(rbind,result)
  stopifnot(length(unique(out$policy))==18L)
  out
}

matchup_summary <- function(x) {
  out <- edge_summary(x)
  out$rmse <- sqrt(edge_mean((x$expected_margin-x$margin)^2))
  pick <- abs(x$expected_margin)>1e-8 & x$margin!=0
  out$su_picks <- sum(pick)
  out$su_accuracy <- if(any(pick))mean(sign(x$expected_margin[pick])==sign(x$margin[pick])) else NA_real_
  out
}

matchup_metrics <- function(policies) {
  result <- list()
  for (name in unique(policies$policy)) {
    x <- policies[policies$policy==name, ]; sides <- p4_or_independent_sides(x)
    slices <- list(all=rep(TRUE,nrow(x)),week_0_1=x$week<=1 & !x$is_cfp,
      weeks_2_4=x$week>=2 & x$week<=4 & !x$is_cfp,week_5_plus=x$week>=5 & !x$is_cfp,
      cfp=x$is_cfp,neutral_cfp=x$is_cfp & x$neutral_site,p4_or_independent=sides$home | sides$away,
      g5_involved=!sides$home | !sides$away,spread_0_7=abs(x$market_margin)<=7,
      spread_7_21=abs(x$market_margin)>7 & abs(x$market_margin)<=21,spread_over_21=abs(x$market_margin)>21,
      at_least_two_supporting_families=x$quote_support>=2)
    for (year in sort(unique(x$season))) slices[[paste0("season_",year)]] <- x$season==year
    for (book in unique(x$quote_provider)) slices[[paste0("provider_",book)]] <- x$quote_provider==book
    for (slice in names(slices)) {
      d <- x[which(slices[[slice]]), ]; if (!nrow(d)) next
      result[[paste(name,slice)]] <- cbind(data.frame(policy=name,slice=slice),matchup_summary(d))
    }
  }
  do.call(rbind,result)
}

matchup_uncertainty <- function(policies) {
  result <- omissions <- list()
  for (name in unique(policies$policy)) {
    x <- policies[policies$policy==name, ]; s <- matchup_summary(x); n <- s$wins+s$losses
    ci <- if(n)binom.test(s$wins,n,conf.level=1-.05/18)$conf.int else c(NA_real_,NA_real_)
    roi <- experiment_season_interval(x$profit_minus110[x$selected],x$season[x$selected])
    result[[name]] <- data.frame(policy=name,adjusted_ats_low=ci[1],adjusted_ats_high=ci[2],
      season_roi_low=roi[1],season_roi_high=roi[2])
    for (year in sort(unique(x$season))) omissions[[paste(name,year)]] <- cbind(
      data.frame(policy=name,omitted_season=year),matchup_summary(x[x$season!=year, ]))
  }
  list(intervals=do.call(rbind,result),leave_one_season_out=do.call(rbind,omissions))
}

run_matchup_research <- function(project,prepared_dir) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  external_verify(prepared_dir)
  data <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_research"),"")
  message("Running seven fixed chronological matchup variants.")
  fitted <- matchup_walk(data); policies <- matchup_policies(data,fitted$predictions)
  metrics <- matchup_metrics(policies); uncertainty <- matchup_uncertainty(policies)
  stress <- do.call(rbind,lapply(c(0,.5,1),function(worse) {
    p <- edge_grade(policies,worse)
    cbind(worse_points=worse,matchup_metrics(p))
  }))
  market <- data[data$season %in% 2022:2025, ]; market$expected_margin <- market$market_margin
  market$signal <- 0; market$eligible <- FALSE; market$home_cover_probability <- NA_real_
  market <- edge_grade(market)
  tables <- list(metrics=metrics,policies=policies,predictions=fitted$predictions,
    lambda_candidates=fitted$candidates,lambda_choices=fitted$choices,coefficients=fitted$coefficients,
    uncertainty=uncertainty$intervals,leave_one_season_out=uncertainty$leave_one_season_out,
    line_stress=stress,market_reference=matchup_summary(market))
  for (name in names(tables)) write.csv(tables[[name]],file.path(output,paste0(name,".csv")),row.names=FALSE)
  saveRDS(fitted$models,file.path(output,"models.rds"))
  saveRDS(list(prepared_dir=prepared_dir,created=external_stamp()),file.path(output,"provenance.rds"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if (!identical(protected,experiment_file_hashes(config))) stop("Production changed during research.")
  for (file in c("MATCHUP_PROTOCOL.md","matchup_sources.R","matchup_features.R","matchup_research.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  external_seal(output)
  list(output=output,summary=metrics[metrics$slice=="all", ])
}
