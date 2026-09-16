foundation_compare <- function(reference, candidate, keys, fields, tolerance=1e-10) {
  assert_columns(reference,c(keys,fields),"foundation reference")
  assert_columns(candidate,c(keys,fields),"foundation candidate")
  assert_unique_keys(reference,keys,"foundation reference")
  assert_unique_keys(candidate,keys,"foundation candidate")
  key <- function(x) do.call(paste,c(x[keys],sep="\r"))
  i <- match(key(reference),key(candidate))
  if(anyNA(i)) stop("Foundation reconstruction is missing reference rows.")
  do.call(rbind,lapply(fields,function(field) {
    a <- as.numeric(reference[[field]]); b <- as.numeric(candidate[[field]][i])
    finite <- is.finite(a) & is.finite(b)
    data.frame(feature=field,rows=length(a),missing_mismatch=sum(is.finite(a)!=is.finite(b)),
      changed=sum(abs(a[finite]-b[finite])>tolerance),
      mean_absolute_change=if(any(finite))mean(abs(a[finite]-b[finite])) else 0,
      maximum_absolute_change=if(any(finite))max(abs(a[finite]-b[finite])) else 0)
  }))
}

foundation_fixed_forecasts <- function(input, choices, features, config) {
  data <- input$data; ps <- paste0("ps_",config$preseason$production_features,"_diff")
  gate <- experiment_preseason_gate(input$covered_seasons,ps)
  if(length(setdiff(features,football_feature_names(data)))) stop("Unexpected football schema.")
  assert_unique_keys(choices,"test_season","fixed football choices")
  rows <- lapply(seq_len(nrow(choices)),function(i) {
    choice <- choices[i, ]; year <- choice$test_season
    train <- which(data$season<year); test <- which(data$season==year)
    if(length(train)!=choice$training_rows || max(data$season[train])!=choice$train_through_season)
      stop("Fixed football training population changed.")
    fold <- data
    if(!choice$coach_recent_share %in% c(.65,.70)) stop("Unknown coach split.")
    fold$coach_rating_diff <- fold[[if(choice$coach_recent_share==.70)
      "coach_rating_70_30_diff" else "coach_rating_65_35_diff"]]
    w <- input$weights[train]/max(input$weights[train])
    base <- fit_weighted_ridge(fold[train, ],"margin",setdiff(features,ps),w,choice$foundation_lambda)
    preseason <- fit_weighted_ridge(fold[train, ],"margin",gate(year,features),w,choice$preseason_lambda)
    share <- preseason_blend_share_for_data(fold[test, ],config)
    data.frame(game_id=fold$game_id[test],season=year,
      expected_margin=(1-share)*predict(base,fold[test, ])+share*predict(preseason,fold[test, ]))
  })
  do.call(rbind,rows)
}

matchup_prefer_turnover_flag <- function(pbp) {
  assert_columns(pbp,c("turnover","turnover_indicator"),"turnover source fields")
  known <- is.finite(pbp$turnover)
  if(any(!pbp$turnover[known] %in% c(0,1)))stop("Unexpected actual-turnover flag value.")
  pbp$turnover_indicator[known] <- pbp$turnover[known]
  pbp
}

run_matchup_foundation_audit <- function(project, tracker_dir, prepared_dir, source_dirs,
                                        correction=c("duplicates","turnover_flag")) {
  correction <- match.arg(correction)
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  for(directory in c(tracker_dir,prepared_dir,source_dirs)) external_verify(directory)
  directory <- if(correction=="duplicates")"matchup_foundation_audit" else "matchup_turnover_audit"
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments",directory),"")
  message("Fixed football data-quality audit: ",output)
  input <- readRDS(file.path(tracker_dir,"training_inputs.rds"))
  original_input <- input
  games <- read_csv_if_present(file.path(config$data_dir,"historical_games.csv"),TRUE)
  games <- games[games$season<=2025, ]; games$kickoff <- parse_utc_datetime(games$kickoff)
  cached <- read_csv_if_present(file.path(config$data_dir,"historical_team_games.csv"),TRUE)
  cached <- cached[cached$season<=2025, ]
  removed <- original <- cleaned <- flag_audit <- list()
  cache_inventory <- read.csv(file.path(prepared_dir,"play_source_inventory.csv"),stringsAsFactors=FALSE)
  for(year in 2020:2025) {
    message("Reconstructing unchanged/cleaned football statistics: ",year)
    paths <- file.path(source_dirs,paste0("upstream_",year,".rds")); paths <- paths[file.exists(paths)]
    if(length(paths)!=1L)stop("Require one full source per season.")
    cache_path <- file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",year,"_compact.rds"))
    cache_row <- match(normalizePath(cache_path,winslash="/"),normalizePath(cache_inventory$file,winslash="/"))
    if(is.na(cache_row) || digest::digest(file=cache_path,algo="sha256")!=cache_inventory$sha256[cache_row])
      stop("Cached plays changed since sealed preparation.")
    pbp <- readRDS(cache_path)
    drop <- rep(FALSE,nrow(pbp))
    if(correction=="duplicates") {
      full <- readRDS(paths)
      drop <- matchup_verified_duplicate_rows(full,pbp)
      rm(full)
    }
    removed[[as.character(year)]] <- data.frame(season=year,source_rows=nrow(pbp),removed=sum(drop))
    g <- games[games$season==year, ]; prior_games <- games[games$season<year, ]
    original_prior <- turnover_prior_as_of(do.call(rbind,original),prior_games,year)
    clean_prior <- turnover_prior_as_of(do.call(rbind,cleaned),prior_games,year)
    original[[as.character(year)]] <- build_team_game_efficiencies_v2(pbp,g,original_prior)
    adjusted <- if(correction=="duplicates")pbp[!drop, ] else matchup_prefer_turnover_flag(pbp)
    cleaned[[as.character(year)]] <- build_team_game_efficiencies_v2(adjusted,g,clean_prior)
    if(correction=="turnover_flag") {
      flag_audit[[as.character(year)]] <- rbind(
        cbind(season=year,profile="original",matchup_play_contract(pbp,g)$counts),
        cbind(season=year,profile="actual_turnover_first",matchup_play_contract(adjusted,g)$counts))
    }
    rm(pbp,adjusted); gc(verbose=FALSE)
  }
  original <- do.call(rbind,original); cleaned <- do.call(rbind,cleaned)
  fields <- intersect(names(cached)[vapply(cached,is.numeric,logical(1))],names(original))
  fields <- setdiff(fields,c("game_id","season","week","model_week","home_score","away_score","margin"))
  parity <- foundation_compare(cached,original,c("game_id","team"),fields)
  write.csv(parity,file.path(output,"team_game_parity.csv"),row.names=FALSE)
  write.csv(foundation_compare(original,cleaned,c("game_id","team"),fields),
    file.path(output,"team_game_changes.csv"),row.names=FALSE)
  write.csv(do.call(rbind,removed),file.path(output,"removed_counts.csv"),row.names=FALSE)
  if(length(flag_audit))write.csv(do.call(rbind,flag_audit),file.path(output,"flag_source_audit.csv"),row.names=FALSE)
  saveRDS(cleaned,file.path(output,"cleaned_team_games.rds"))
  if(any(parity$missing_mismatch+parity$changed>0)) stop("Unchanged team-game reconstruction failed; inspect parity audit.")
  members <- read_csv_if_present(file.path(config$data_dir,"historical_fbs_membership.csv"),TRUE)
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
  message("Reconstructing pregame snapshots with frozen score-derived power")
  raw_original <- rebuild(original); raw_clean <- rebuild(cleaned)
  schema <- read.csv(file.path(tracker_dir,"feature_manifest.csv"),stringsAsFactors=FALSE)
  features <- schema$feature[schema$model=="football_matched_control"]
  changed_fields <- intersect(features,names(raw_original))
  feature_parity <- foundation_compare(input$data,raw_original,"game_id",changed_fields)
  write.csv(feature_parity,file.path(output,"feature_parity.csv"),row.names=FALSE)
  if(any(feature_parity$missing_mismatch+feature_parity$changed>0))
    stop("Unchanged feature reconstruction failed; inspect parity audit.")
  index <- match(input$data$game_id,raw_clean$game_id)
  if(anyNA(index))stop("Cleaned features lost games.")
  input$data[changed_fields] <- raw_clean[index,changed_fields]
  changes <- foundation_compare(original_input$data,input$data,"game_id",features)
  write.csv(changes,file.path(output,"feature_changes.csv"),row.names=FALSE)
  saveRDS(input,file.path(output,"cleaned_training_inputs.rds"))
  choices <- read.csv(file.path(tracker_dir,"fold_choices.csv"),stringsAsFactors=FALSE)
  choices <- choices[choices$model=="football_matched_control", ]
  baseline <- foundation_fixed_forecasts(original_input,choices,features,config)
  frozen <- read.csv(file.path(tracker_dir,"predictions.csv"),stringsAsFactors=FALSE)
  frozen <- frozen[frozen$model=="football_matched_control", ]
  forecast_parity <- foundation_compare(frozen,baseline,"game_id","expected_margin",1e-8)
  write.csv(forecast_parity,file.path(output,"forecast_parity.csv"),row.names=FALSE)
  if(any(forecast_parity$missing_mismatch+forecast_parity$changed>0))
    stop("Unchanged forecast reconstruction failed.")
  corrected <- foundation_fixed_forecasts(input,choices,features,config)
  evaluation <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  evaluation <- evaluation[evaluation$season %in% 2022:2025, ]
  predictions <- do.call(rbind,lapply(c("original","cleaned"),function(profile) {
    p <- if(profile=="original")baseline else corrected
    x <- evaluation; x$model <- paste0("football_",profile)
    x$expected_margin <- p$expected_margin[match(x$game_id,p$game_id)]
    if(any(!is.finite(x$expected_margin)))stop("Missing fixed football forecast.")
    x
  }))
  policies <- do.call(rbind,lapply(split(predictions,predictions$model),function(x) {
    do.call(rbind,lapply(c(0,3),function(edge) {
      p <- x; p$policy <- paste0(x$model[1],if(edge==0)"_all" else "_edge3")
      p$pick_side <- sign(p$expected_margin-p$market_margin)
      p$pick_side[abs(p$expected_margin-p$market_margin)<edge] <- 0
      p
    }))
  }))
  write.csv(predictions,file.path(output,"predictions.csv"),row.names=FALSE)
  write.csv(policies,file.path(output,"policies.csv"),row.names=FALSE)
  write.csv(choices,file.path(output,"fixed_fold_choices.csv"),row.names=FALSE)
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  saveRDS(list(tracker_dir=tracker_dir,prepared_dir=prepared_dir,source_dirs=source_dirs,
    schema=features,created=external_stamp(),correction=correction,
    scope="Single-defect fixed football refit; no production promotion"),
    file.path(output,"provenance.rds"))
  for(file in c("MATCHUP_PROTOCOL.md","matchup_foundation_audit.R","matchup_source_probe.R","matchup_play_contract.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  if(!identical(protected,experiment_file_hashes(config))) stop("Protected production changed.")
  external_seal(output)
  output
}
