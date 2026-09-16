play_cleanup_paths <- function(project) {
  root <- file.path(project,"cfb_v3/output/experiments")
  list(root=root,
    sources=file.path(root,c("matchup_source_probe/20260912T014121.700Z",
                            "matchup_source_probe/20260912T014320.238Z")),
    boxes=file.path(root,"matchup_box_audit/20260912T032310.128Z"),
    tracker=file.path(root,"prediction_tracker/20260912T003231.716Z"),
    prepared=file.path(root,"matchup_prepared/20260912T015157.387Z"),
    v3_reference=file.path(root,"matchup_v3_turnover/20260912T024307.020Z"))
}

play_cleanup_code_inventory <- function(project) {
  files <- c("run_cfb_play_cleanup.R",file.path("cfb_v3/experiments",c(
    "PLAY_CLEANUP_PROTOCOL.md","play_cleanup_classifier.R","play_cleanup_audit.R","play_cleanup_pipeline.R")))
  data.frame(file=files,sha256=vapply(file.path(project,files),function(p)
    digest::digest(file=p,algo="sha256"),character(1)),row.names=NULL)
}

play_cleanup_assert_source <- function(source,classified) {
  metadata <- setdiff(names(attributes(source)),"names")
  if(nrow(classified)!=nrow(source) || !all(names(source) %in% names(classified)) ||
     !all(vapply(names(source),function(field)identical(source[[field]],classified[[field]]),logical(1))) ||
     !identical(attributes(source)[metadata],attributes(classified)[metadata]))
    stop("Classifier modified raw source fields or metadata.")
  invisible(TRUE)
}

play_cleanup_require_parity <- function(reference,candidate,keys,fields,label) {
  if(nrow(reference)!=nrow(candidate))stop(label,": row count changed.")
  check <- foundation_compare(reference,candidate,keys,fields,1e-8)
  if(any(check$missing_mismatch+check$changed>0))stop(label,": parity failed.")
  check
}

play_cleanup_rate_cells <- function(classified) {
  assert_columns(classified,c("clean_giveaway","clean_scrimmage","clean_status"),"classified source")
  if(any(!is.na(classified$clean_giveaway) & !classified$clean_giveaway %in% c(0,1)))
    stop("Invalid cleaned giveaway.")
  compact <- compact_cfb_pbp(as.data.frame(classified))
  rownames(compact) <- paste0("source_",seq_len(nrow(compact)))
  plays <- historical_competitive_plays(compact)
  index <- match(rownames(plays),rownames(compact))
  # Carry stable source positions through the unchanged filter, not colliding play IDs.
  if(anyNA(index))stop("Competitive filter lost source identity.")
  legacy <- plays$rush==1 | plays$pass==1 | plays$sack==1 |
    grepl("pass|rush|run|sack",plays$play_type,ignore.case=TRUE)
  legacy[is.na(legacy)] <- FALSE
  clean_scrimmage <- classified$clean_scrimmage[index]
  finite <- is.finite(as.numeric(plays$EPA)) & !is.na(plays$pos_team) &
    nzchar(plays$pos_team) & !is.na(plays$def_pos_team) & nzchar(plays$def_pos_team)
  eligible <- finite & legacy
  contradiction <- finite & (is.na(clean_scrimmage) | legacy != clean_scrimmage)
  potential <- finite & (legacy | is.na(clean_scrimmage) | clean_scrimmage)
  giveaway <- classified$clean_giveaway[index]
  status <- classified$clean_status[index]
  giveaway[is.na(status) | status!="resolved"] <- NA_real_
  uncertain <- potential & (!is.finite(giveaway) | is.na(status) | status!="resolved")
  havoc <- as.numeric(plays$sack==1 | plays$stuffed_run==1 | giveaway==1)
  summarize <- function(team_field,defense=FALSE) {
    key <- paste(plays$game_id,plays[[team_field]],sep="\r")
    relevant <- which(eligible | contradiction | uncertain)
    if(!length(relevant))return(data.frame(game_id=character(),team=character(),side=character(),
      model_plays=integer(),turnover_unknown_plays=integer(),eligibility_conflicts=integer(),
      turnover_lost_rate=numeric(),havoc_rate=numeric()))
    do.call(rbind,lapply(split(relevant,key[relevant]),function(i) {
      use <- i[eligible[i]]
      bad_type <- any(contradiction[i])
      turnover_unknown <- any(uncertain[i])
      havoc_unknown <- any(uncertain[i] & !is.finite(havoc[i]))
      data.frame(game_id=as.character(plays$game_id[i[1]]),team=plays[[team_field]][i[1]],
        side=if(defense)"defense" else "offense",model_plays=length(use),
        turnover_unknown_plays=sum(uncertain[i]),eligibility_conflicts=sum(contradiction[i]),
        turnover_lost_rate=if(length(use) && !bad_type && !turnover_unknown)
          mean(giveaway[use]) else NA_real_,
        havoc_rate=if(length(use) && !bad_type && !havoc_unknown)
          mean(havoc[use]) else NA_real_)
    }))
  }
  rbind(summarize("pos_team"),summarize("def_pos_team",TRUE))
}

play_cleanup_apply_rates <- function(team_games,cells,prior) {
  assert_unique_keys(team_games,c("game_id","team"),"team-game rates")
  assert_unique_keys(cells,c("game_id","team","side"),"clean rate cells")
  key <- function(x)paste(x$game_id,x$team,sep="\r")
  offense <- cells[cells$side=="offense", ]; defense <- cells[cells$side=="defense", ]
  i <- match(key(team_games),key(offense)); j <- match(key(team_games),key(defense))
  count <- offense$model_plays[i]; known <- is.finite(count) & is.finite(team_games$scrimmage_plays)
  if(any(count[known]!=team_games$scrimmage_plays[known]))stop("Classifier changed model denominator.")
  if(any(is.finite(team_games$scrimmage_plays) & team_games$scrimmage_plays>0 & is.na(i)))
    stop("Classifier lost a model-eligible team-game.")
  team_games$turnover_lost_rate <- offense$turnover_lost_rate[i]
  team_games$havoc_allowed <- offense$havoc_rate[i]
  team_games$havoc_generated <- defense$havoc_rate[j]
  team_games$turnover_rate_regressed <- regress_unstable_rate(team_games$turnover_lost_rate,
    team_games$scrimmage_plays,prior,prior_opportunities=80)
  team_games
}

play_cleanup_prepare <- function(project) {
  paths <- play_cleanup_paths(project); config <- cfb_v2_config(project,2026L)
  protected <- experiment_file_hashes(config)
  code <- play_cleanup_code_inventory(project)
  for(directory in c(paths$sources,paths$boxes,paths$prepared))external_verify(directory)
  output <- external_new_dir(file.path(paths$root,"play_cleanup_prepared"),"")
  message("Preparing isolated play cleanup: ",output)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  games <- games[games$season<=2025, ]; games$kickoff <- parse_utc_datetime(games$kickoff)
  cached_team_games <- read_csv_if_present(file.path(config$data_dir,"historical_team_games.csv"),TRUE)
  cached_team_games <- cached_team_games[cached_team_games$season<=2025, ]
  boxes <- read.csv(file.path(paths$boxes,"reported_stats_ledger.csv"),stringsAsFactors=FALSE)
  boxes <- boxes[boxes$status=="matched" & boxes$duplicate_status=="unique", ]
  gi <- match(boxes$game_id,games$game_id)
  boxes <- boxes[!is.na(gi) & raw_history_eligible(games[gi, ]), ]
  inventory <- removed <- season_counts <- cell_rows <- audits <- list()
  profiles <- list(original=list(),duplicates=list(),classified=list())
  source_cache <- read.csv(file.path(paths$prepared,"play_source_inventory.csv"),stringsAsFactors=FALSE)
  for(year in 2020:2025) {
    message("Source identity, classification and rate audit: ",year)
    candidates <- file.path(paths$sources,paste0("upstream_",year,".rds"))
    full_path <- candidates[file.exists(candidates)]
    if(length(full_path)!=1L)stop("Require one identity-rich source per season.")
    cache_path <- file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",year,"_compact.rds"))
    ci <- match(normalizePath(cache_path,winslash="/"),normalizePath(source_cache$file,winslash="/"))
    if(is.na(ci) || digest::digest(file=cache_path,algo="sha256")!=source_cache$sha256[ci])
      stop("Compact source changed since baseline.")
    full <- as.data.frame(readRDS(full_path)); cached <- as.data.frame(readRDS(cache_path))
    drop <- matchup_verified_duplicate_rows(full,cached)
    inventory[[as.character(year)]] <- data.frame(season=year,file=full_path,
      sha256=digest::digest(file=full_path,algo="sha256"),compact_file=cache_path,
      compact_sha256=digest::digest(file=cache_path,algo="sha256"))
    removed[[as.character(year)]] <- data.frame(season=rep(year,sum(drop)),source_row=which(drop),
      as.data.frame(full[drop,c("game_id","game_play_number","clock_minutes","clock_seconds","play_type","play_text")]))
    clean_source <- as.data.frame(full[!drop, ],stringsAsFactors=FALSE)
    classified <- play_cleanup_classify(clean_source)
    play_cleanup_assert_source(clean_source,classified)
    annotation <- classified[unique(c(intersect(c("game_id","id_play","drive_id","game_play_number",
      "pos_team","def_pos_team","play_type","play_text","EPA"),names(classified)),
      grep("^clean_",names(classified),value=TRUE)))]
    annotation$source_row <- which(!drop)
    saveRDS(annotation,file.path(output,paste0("classifications_",year,".rds")))
    season_counts[[as.character(year)]] <- data.frame(season=year,source_rows=nrow(full),
      duplicates_removed=sum(drop),classified_rows=nrow(classified),
      resolved=sum(classified$clean_status=="resolved",na.rm=TRUE),
      unresolved=sum(is.na(classified$clean_status) | classified$clean_status!="resolved"),
      known_giveaway_values=sum(is.finite(classified$clean_giveaway)),
      confirmed_giveaways=sum(classified$clean_status=="resolved" & classified$clean_giveaway==1,na.rm=TRUE))
    sample <- boxes[boxes$season==year, ]
    if(nrow(sample))audits[[as.character(year)]] <- play_cleanup_reconcile(classified,sample)
    cells <- play_cleanup_rate_cells(classified); cells$season <- year
    cell_rows[[as.character(year)]] <- cells
    g <- games[games$season==year, ]; past <- games[games$season<year, ]
    for(profile in names(profiles)) {
      prior <- turnover_prior_as_of(do.call(rbind,profiles[[profile]]),past,year)
      pbp <- if(profile=="original")cached else cached[!drop, ]
      tg <- build_team_game_efficiencies_v2(pbp,g,prior)
      if(profile=="classified")tg <- play_cleanup_apply_rates(tg,cells,prior)
      profiles[[profile]][[as.character(year)]] <- tg
    }
    rm(full,cached,classified,clean_source,annotation,pbp); gc(verbose=FALSE)
  }
  profiles <- lapply(profiles,function(x)do.call(rbind,x))
  numeric_fields <- names(cached_team_games)[vapply(cached_team_games,is.numeric,logical(1))]
  parity <- play_cleanup_require_parity(cached_team_games,profiles$original,c("game_id","team"),
    numeric_fields,"Original team-game replay")
  unchanged <- setdiff(numeric_fields,c("turnover_lost_rate","turnover_rate_regressed","havoc_allowed","havoc_generated"))
  rate_parity <- play_cleanup_require_parity(profiles$duplicates,profiles$classified,c("game_id","team"),
    unchanged,"Non-rate classifier invariance")
  for(profile in names(profiles))saveRDS(profiles[[profile]],file.path(output,paste0(profile,"_team_games.rds")))
  saveRDS(audits,file.path(output,"reconciliation.rds"))
  if(length(audits))for(name in names(audits[[1]])) {
    tables <- lapply(audits,function(x)x[[name]])
    if(all(vapply(tables,is.data.frame,logical(1))))
      write.csv(do.call(bind_rows_fill,tables),file.path(output,paste0(name,".csv")),row.names=FALSE)
  }
  write.csv(parity,file.path(output,"original_team_game_parity.csv"),row.names=FALSE)
  write.csv(rate_parity,file.path(output,"classifier_unchanged_metrics.csv"),row.names=FALSE)
  write.csv(do.call(rbind,inventory),file.path(output,"source_inventory.csv"),row.names=FALSE)
  write.csv(do.call(rbind,removed),file.path(output,"duplicate_removal_ledger.csv"),row.names=FALSE)
  write.csv(do.call(rbind,season_counts),file.path(output,"classification_summary.csv"),row.names=FALSE)
  write.csv(do.call(rbind,cell_rows),file.path(output,"rate_coverage.csv"),row.names=FALSE)
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  write.csv(code,file.path(output,"code_inventory.csv"),row.names=FALSE)
  saveRDS(list(created=external_stamp(),paths=paths,production_promotion=FALSE,
    limitations="Unresolved events invalidate affected rate cells; unchanged past-only summaries omit missing observations. Individual EPA and original model denominator unchanged."),
    file.path(output,"provenance.rds"))
  for(file in c("PLAY_CLEANUP_PROTOCOL.md","play_cleanup_classifier.R","play_cleanup_audit.R","play_cleanup_pipeline.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during cleanup preparation.")
  if(!identical(code,play_cleanup_code_inventory(project)))stop("Cleanup code changed during preparation; rerun.")
  external_seal(output)
  output
}

play_cleanup_compare <- function(project,prepared_dir) {
  paths <- play_cleanup_paths(project); config <- cfb_v2_config(project,2026L)
  protected <- experiment_file_hashes(config)
  code <- play_cleanup_code_inventory(project)
  for(directory in c(prepared_dir,paths$tracker,paths$prepared,paths$v3_reference))external_verify(directory)
  recorded <- read.csv(file.path(prepared_dir,"protected_before.csv"),stringsAsFactors=FALSE)
  if(!identical(recorded,protected))stop("Production changed since cleanup preparation.")
  recorded_code <- read.csv(file.path(prepared_dir,"code_inventory.csv"),stringsAsFactors=FALSE)
  if(!identical(recorded_code,code))stop("Cleanup code differs from preparation; rerun the full command.")
  output <- external_new_dir(file.path(paths$root,"play_cleanup_comparison"),"")
  message("Comparing fixed cleanup candidates: ",output)
  joined <- read.csv(file.path(paths$tracker,"joined_inputs.csv"),stringsAsFactors=FALSE)
  prepared <- tracker_prepare(config,joined)
  input <- list(data=prepared$full_data,weights=prepared$full_weights,covered_seasons=prepared$covered_seasons)
  schema <- read.csv(file.path(paths$tracker,"feature_manifest.csv"),stringsAsFactors=FALSE)
  fields <- schema$feature[schema$model=="football_matched_control"]
  if(!length(fields) || any(fields %in% c("home","away","margin","season","week")))stop("Invalid feature schema.")
  choices <- read.csv(file.path(paths$v3_reference,"fixed_fold_choices.csv"),stringsAsFactors=FALSE)
  baseline <- foundation_fixed_forecasts(input,choices,fields,config)
  frozen <- read.csv(file.path(config$output_dir,"backtest/rolling_predictions.csv"),stringsAsFactors=FALSE)
  frozen <- frozen[frozen$model=="ridge_core", ]
  parity <- play_cleanup_require_parity(frozen,baseline,"game_id","expected_margin","Frozen v3 forecast")
  write.csv(parity,file.path(output,"frozen_forecast_parity.csv"),row.names=FALSE)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  games <- games[games$season<=2025, ]; games$kickoff <- parse_utc_datetime(games$kickoff)
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
  forecast <- list(original=baseline); missing <- changes <- list()
  training <- list(original=input)
  for(profile in c("original","duplicates","classified")) {
    message("Rebuilding fixed pregame inputs: ",profile)
    tg <- readRDS(file.path(prepared_dir,paste0(profile,"_team_games.rds")))
    raw <- rebuild(tg); changed_fields <- intersect(fields,names(raw))
    if(profile=="original") {
      # Raw reconstruction includes source games outside the training eligibility gate.
      feature_parity <- foundation_compare(input$data,raw,"game_id",changed_fields)
      if(any(feature_parity$missing_mismatch+feature_parity$changed>0))stop("Original feature replay failed.")
      write.csv(feature_parity,file.path(output,"original_feature_parity.csv"),row.names=FALSE)
    } else {
      candidate <- input; index <- match(input$data$game_id,raw$game_id)
      if(anyNA(index))stop("Cleanup lost training games.")
      candidate$data[changed_fields] <- raw[index,changed_fields]
      training[[profile]] <- candidate
      forecast[[profile]] <- foundation_fixed_forecasts(candidate,choices,fields,config)
      changes[[profile]] <- cbind(profile=profile,foundation_compare(input$data,candidate$data,"game_id",fields))
      saveRDS(candidate,file.path(output,paste0(profile,"_training_inputs.rds")))
    }
    data <- training[[profile]]$data
    missing[[profile]] <- data.frame(profile=profile,feature=fields,
      missing_training_cells=vapply(data[fields],function(x)sum(!is.finite(x)),integer(1)))
  }
  removed_features <- fields[grepl("turnover|havoc",fields)]
  if(!length(removed_features))stop("No suspect features in ablation.")
  forecast$duplicates_without_turnover_havoc <- foundation_fixed_forecasts(training$duplicates,
    choices,setdiff(fields,removed_features),config)
  evaluation <- readRDS(file.path(paths$prepared,"model_inputs.rds"))
  evaluation <- evaluation[evaluation$season %in% 2022:2025, ]
  assert_unique_keys(evaluation,"game_id","common evaluation cohort")
  rows <- lapply(names(forecast),function(profile) {
    x <- evaluation; p <- forecast[[profile]]
    assert_unique_keys(p,"game_id","cleanup forecasts")
    x$expected_margin <- p$expected_margin[match(x$game_id,p$game_id)]
    if(any(!is.finite(x$expected_margin)))stop("Missing comparison predictions.")
    x$model <- profile; x$policy <- profile; x$home_cover_probability <- NA_real_
    x$signal <- x$expected_margin-x$market_margin; x$eligible <- TRUE
    edge_grade(x)
  })
  policies <- do.call(bind_rows_fill,rows)
  expected_grade <- sign(policies$margin+policies$closing_home_spread)*sign(policies$signal)
  # Independent grading is checked against the policy engine without treating passes as losses.
  if(any(policies$selected & (policies$win!=(expected_grade>0) | policies$push!=(expected_grade==0))))
    stop("Independent spread grading disagrees.")
  metrics <- matchup_metrics(policies)
  paired <- do.call(rbind,lapply(setdiff(names(forecast),"original"),function(profile) {
    a <- policies[policies$model=="original", ]; b <- policies[policies$model==profile, ]
    b <- b[match(a$game_id,b$game_id), ]
    delta <- abs(b$expected_margin-b$margin)-abs(a$expected_margin-a$margin)
    by_year <- aggregate(delta,list(season=a$season),mean)
    data.frame(profile=profile,season=by_year$season,mae_change=by_year$x,
      interpretation="Negative means lower error than frozen v3; descriptive reused history")
  }))
  write.csv(policies,file.path(output,"predictions.csv"),row.names=FALSE)
  write.csv(metrics,file.path(output,"metrics.csv"),row.names=FALSE)
  write.csv(paired,file.path(output,"paired_season_mae.csv"),row.names=FALSE)
  write.csv(do.call(rbind,missing),file.path(output,"feature_missingness.csv"),row.names=FALSE)
  write.csv(do.call(rbind,changes),file.path(output,"feature_changes.csv"),row.names=FALSE)
  write.csv(choices,file.path(output,"fixed_fold_choices.csv"),row.names=FALSE)
  write.csv(data.frame(feature=removed_features),file.path(output,"ablation_features.csv"),row.names=FALSE)
  saveRDS(forecast,file.path(output,"full_forecasts.rds"))
  saveRDS(list(created=external_stamp(),prepared_dir=prepared_dir,paths=paths,features=fields,
    production_promotion=FALSE,policy="All nonzero edges; no threshold tuning; fixed old fold choices",
    limitations="Four predeclared profiles on reused history; qualification does not establish publication-time prices. Classifier does not repair EPA."),
    file.path(output,"provenance.rds"))
  display <- function(x) {
    keep <- intersect(c("policy","slice","games","wins","losses","pushes","ats","mae","su_accuracy","units_minus110"),names(x))
    y <- x[keep]; for(field in names(y))if(is.numeric(y[[field]]))y[[field]] <- round(y[[field]],5)
    markdown_table(y)
  }
  rate_coverage <- read.csv(file.path(prepared_dir,"rate_coverage.csv"),stringsAsFactors=FALSE)
  coverage <- do.call(rbind,lapply(split(rate_coverage,list(rate_coverage$season,rate_coverage$side),drop=TRUE),function(x)
    data.frame(season=x$season[1],side=x$side[1],cells=nrow(x),
      turnover_rate_missing=sum(!is.finite(x$turnover_lost_rate)),havoc_rate_missing=sum(!is.finite(x$havoc_rate)))))
  write.csv(coverage,file.path(output,"rate_missingness_summary.csv"),row.names=FALSE)
  audit <- read.csv(file.path(prepared_dir,"team_game_reconciliation.csv"),stringsAsFactors=FALSE)
  audit_summary <- do.call(rbind,lapply(split(audit,audit$season),function(x)
    data.frame(season=x$season[1],team_games=nrow(x),
      reported_turnovers=sum(x$reported_total,na.rm=TRUE),
      confirmed_event_lower_bound=sum(x$classified_count_lower_bound,na.rm=TRUE),
      count_matches=sum(x$count_agreement,na.rm=TRUE),
      count_matches_without_audit_blockers=sum(x$count_reconciled,na.rm=TRUE),
      requiring_review=sum(!x$count_reconciled),unknown_observed_rows=sum(x$unknown_event_count,na.rm=TRUE))))
  write.csv(audit_summary,file.path(output,"audit_summary.csv"),row.names=FALSE)
  report <- c("# Bounded Play Cleanup Comparison","",
    "Research candidate only. No production promotion, published-pick edits or individual EPA changes.","",
    "## Identical-Game Results","",display(metrics[metrics$slice=="all", ]),"",
    "Fixed folds, coach shares, phase weights and training population; 2020-2021 warm-up.",
    "The four profiles were fixed before scoring. This history is reused, not untouched validation.",
    "Book corroboration does not prove Friday execution time; prices and returns are hypothetical.","",
    "## Seasonal Results","",display(metrics[grepl("^season_",metrics$slice), ]),"",
    "## CFP And Large Spreads","",display(metrics[metrics$slice %in% c("cfp","neutral_cfp","spread_over_21"), ]),"",
    "Shared CFP coverage is 17 games, not all 28; the external-source cohort excludes missing FPI rows.",
    "These are weekly actual matchups, not a frozen pre-playoff bracket.","",
    "## Bounded Box-Score Audit","",markdown_table(audit_summary),"",
    "Confirmed-event counts are conservative lower bounds, not complete replacements for box totals.",
    "Count matches with unresolved events still require review. A stricter evidence rule can reduce",
    "nominal count agreement without establishing that the source became worse. No source is assumed infallible.","",
    "## Missing Rate Cells","",markdown_table(coverage),"",
    "Unresolved classifications and denominator contradictions invalidate rate cells. No uncertain",
    "event is imputed as zero. Existing historical summaries omit missing cells and may fall back to",
    "prior-season information; final model imputation is fit on training seasons only. See feature_missingness.csv.",
    "Reconciliation/exception records are in the pinned prepared directory. Equal totals alone do not",
    "prove correct event identity. No box-score counts were substituted into model rates.","",
    "## Reproduction","","```powershell","Rscript run_cfb_play_cleanup.R","```","",
    "The default runs focused tests, preparation and comparison offline using retained sealed caches.",
    "A Git clone alone does not include ignored raw/output caches. Production file hashes are checked",
    "at both stage boundaries. Manual review is limited to exceptions; unknown evidence remains unknown.")
  writeLines(report,file.path(output,"REPORT.md"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  write.csv(code,file.path(output,"code_inventory.csv"),row.names=FALSE)
  for(file in c("PLAY_CLEANUP_PROTOCOL.md","play_cleanup_pipeline.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  file.copy(file.path(project,"run_cfb_play_cleanup.R"),file.path(output,"run_cfb_play_cleanup.R"))
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during comparison.")
  if(!identical(code,play_cleanup_code_inventory(project)))stop("Cleanup code changed during comparison; rerun.")
  external_seal(output)
  output
}
