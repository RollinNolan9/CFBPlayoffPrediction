matchup_paired_diagnostics <- function(predictions) {
  result <- annual <- list()
  baselines <- c("market","external_market","linear_components")
  for (name in unique(predictions$model)) for (base in baselines) {
    if (name == base) next
    x <- predictions[predictions$model==name, ]
    if (base == "market") ref <- x$market_margin else {
      b <- predictions[predictions$model==base, ]; assert_unique_keys(b,"game_id","paired baseline")
      i <- match(x$game_id,b$game_id)
      if (anyNA(i) || any(x$margin!=b$margin[i])) stop("Paired metric mismatch.")
      ref <- b$expected_margin[i]
    }
    delta <- abs(x$expected_margin-x$margin)-abs(ref-x$margin)
    ci <- experiment_season_interval(delta,x$season)
    result[[paste(name,base)]] <- data.frame(model=name,baseline=base,games=nrow(x),
      mae_change=mean(delta),season_block_low=ci[1],season_block_high=ci[2])
    a <- aggregate(delta,list(season=x$season),mean); names(a)[2] <- "mae_change"
    annual[[paste(name,base)]] <- cbind(model=name,baseline=base,a)
  }
  list(paired=do.call(rbind,result),annual=do.call(rbind,annual))
}

matchup_quote_sensitivity <- function(policies,ledger) {
  ledger <- ledger[ledger$eligible_record & ledger$supporting_families>=1, ]
  assert_unique_keys(ledger,c("game_id","provider"),"qualified provider alternatives")
  result <- strict <- list()
  for (name in unique(policies$policy)) {
    x <- policies[policies$policy==name, ]
    for (book in unique(ledger$provider)) {
      b <- ledger[ledger$provider==book, ]; i <- match(x$game_id,b$game_id)
      keep <- !is.na(i); if (!any(keep)) next
      original <- x[keep, ]; changed <- original
      changed$market_residual <- changed$margin+b$spread[i[keep]]
      for (source in c("selected_reference","alternative_book")) {
        d <- if(source=="selected_reference")original else changed
        result[[paste(name,book,source)]] <- cbind(data.frame(policy=name,provider=book,
          quote=source,interpretation="fixed_side_counterfactual_not_execution"),matchup_summary(edge_grade(d)))
      }
    }
    supported <- vapply(seq_len(nrow(x)),function(i) {
      b <- ledger[ledger$game_id==x$game_id[i], ]
      family <- matchup_book(x$quote_provider[i])$book_family
      sum(unique(b$book_family[b$book_family!=family & abs(b$spread+x$market_margin[i])<=.5])!="")
    },integer(1))
    for (required in 1:2) {
      d <- x[supported>=required, ]; if(!nrow(d)) next
      strict[[paste(name,required)]] <- cbind(data.frame(policy=name,
        other_families_within_half=required),matchup_summary(d))
    }
  }
  list(provider=do.call(rbind,result),strict_agreement=do.call(rbind,strict))
}

matchup_source_comparison <- function(data) {
  x <- data[data$season %in% 2022:2025 & is.finite(data$original_pbp_spread), ]
  rows <- list()
  for (name in c("v3","external")) for (source in c("original_pbp_spread","closing_home_spread")) {
    d <- x; d$expected_margin <- if(name=="v3")d$v3_margin else
      (d$challenger_tracker_sagarin+d$challenger_tracker_fpi)/2
    d$market_margin <- -d[[source]]; d$signal <- d$expected_margin-d$market_margin
    d$market_residual <- d$margin-d$market_margin; d$eligible <- TRUE; d$home_cover_probability <- NA_real_
    rows[[paste(name,source)]] <- cbind(data.frame(model=name,quote=source),matchup_summary(edge_grade(d)))
  }
  do.call(rbind,rows)
}

matchup_exact_season_signs <- function(predictions) {
  models <- unique(predictions$model); years <- sort(unique(predictions$season))
  contributions <- matrix(0,nrow=length(years),ncol=length(models),dimnames=list(years,models))
  for (name in models) {
    x <- predictions[predictions$model==name, ]
    gain <- abs(x$market_margin-x$margin)-abs(x$expected_margin-x$margin)
    for (year in years) contributions[as.character(year),name] <- sum(gain[x$season==year])/nrow(x)
  }
  signs <- as.matrix(expand.grid(rep(list(c(-1,1)),length(years))))
  null <- signs %*% contributions
  observed <- colSums(contributions)
  maximum <- apply(null,1,max)
  data.frame(model=models,mae_improvement_over_market=unname(observed),
    season_sign_flip_one_sided=vapply(seq_along(models),function(i)mean(null[,i]>=observed[i]-1e-12),0),
    family_max_sign_flip=vapply(observed,function(value)mean(maximum>=value-1e-12),0),
    permutations=nrow(signs),assumption="independent_symmetric_season_effects_only_four_seasons")
}

matchup_feature_audit <- function(data,features) {
  result <- list()
  for (year in sort(unique(data$season))) for (field in features) {
    x <- data[[field]][data$season==year]
    result[[paste(year,field)]] <- data.frame(season=year,feature=field,rows=length(x),
      missing=sum(!is.finite(x)),nonzero=sum(abs(x)>1e-12,na.rm=TRUE),mean=mean(x),sd=sd(x),
      minimum=min(x),maximum=max(x))
  }
  do.call(rbind,result)
}

matchup_leakage_checks <- function(data,history,models) {
  sample <- data[order(data$season,data$week,data$game_id), ]
  phase <- ifelse(sample$is_cfp,"cfp",ifelse(sample$week<=1,"preseason",ifelse(sample$week<=4,"early","in_season")))
  sample <- sample[!duplicated(paste(sample$season,phase)), ]
  checks <- list()
  for (i in seq_len(nrow(sample))) {
    g <- sample[i, ]; old <- matchup_build_features(history,g)
    modified <- history
    future <- modified$kickoff>=old$audit$cutoff
    modified$off_pass_sum[future] <- 1e9; modified$def_pass_sum[future] <- -1e9
    modified$opponent_power[future] <- 1e9
    new <- matchup_build_features(modified,g)
    same <- identical(old,new)
    if (!same) stop("Historical feature future perturbation failed.")
    flipped <- g; flipped$home <- g$away; flipped$away <- g$home
    mirrored <- matchup_build_features(history,flipped)$features
    mirror <- isTRUE(all.equal(as.matrix(old$features[-1]),-as.matrix(mirrored[-1]),tolerance=1e-12))
    if (!mirror) stop("Historical feature mirror failed.")
    checks[[as.character(g$game_id)]] <- data.frame(game_id=g$game_id,season=g$season,week=g$week,
      future_perturbation_unchanged=same,team_swap_antisymmetric=mirror)
  }
  for (key in names(models)) {
    object <- models[[key]]; d <- data[data$season==object$train_through+1, ]
    if (!nrow(d)) stop("Missing model audit season.")
    a <- matchup_predict(object,d); d$margin <- 1e9; d$market_residual <- -1e9
    if (!identical(a,matchup_predict(object,d))) stop("Model reads held-out outcomes.")
  }
  do.call(rbind,checks)
}

run_matchup_diagnostics <- function(project,prepared_dir,research_dir) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  external_verify(prepared_dir); external_verify(research_dir)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_diagnostics"),"")
  data <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  history <- readRDS(file.path(prepared_dir,"team_game_counts.rds"))
  predictions <- read.csv(file.path(research_dir,"predictions.csv"),stringsAsFactors=FALSE)
  policies <- read.csv(file.path(research_dir,"policies.csv"),stringsAsFactors=FALSE)
  ledger_path <- file.path(prepared_dir,"provider_ledger.csv")
  if (!file.exists(ledger_path)) {
    original <- readRDS(file.path(prepared_dir,"provenance.rds"))$prepared_dir
    external_verify(original); ledger_path <- file.path(original,"provider_ledger.csv")
  }
  ledger <- read.csv(ledger_path,stringsAsFactors=FALSE)
  paired <- matchup_paired_diagnostics(predictions)
  quotes <- matchup_quote_sensitivity(policies,ledger)
  models <- readRDS(file.path(research_dir,"models.rds"))
  message("Checking real historical feature cutoffs, reversals and held-out outcome independence.")
  checks <- matchup_leakage_checks(data,history,models)
  tables <- list(paired_margin=paired$paired,paired_by_season=paired$annual,
    provider_quote_sensitivity=quotes$provider,strict_quote_agreement=quotes$strict_agreement,
    same_games_old_vs_provider_quotes=matchup_source_comparison(data),
    exact_season_sign_flip=matchup_exact_season_signs(predictions),
    feature_ranges=matchup_feature_audit(data,unique(unlist(matchup_variants(),use.names=FALSE))),
    historical_causal_checks=checks)
  for (name in names(tables)) write.csv(tables[[name]],file.path(output,paste0(name,".csv")),row.names=FALSE)
  saveRDS(list(prepared_dir=prepared_dir,research_dir=research_dir,created=external_stamp()),file.path(output,"provenance.rds"))
  file.copy(file.path(project,"cfb_v3/experiments/matchup_diagnostics.R"),file.path(output,"matchup_diagnostics.R"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if (!identical(protected,experiment_file_hashes(config))) stop("Production changed during diagnostics.")
  external_seal(output)
  output
}
