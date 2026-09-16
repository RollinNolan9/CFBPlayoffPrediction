matchup_play_contract <- function(pbp,games) {
  eligible <- games$game_id[raw_history_eligible(games) & games$completed]
  p <- historical_competitive_plays(pbp[as.character(pbp$game_id) %in% eligible, ])
  p <- p[is.finite(p$EPA), ]
  flag <- function(x) !is.na(x) & x==1
  scrimmage <- flag(p$rush) | flag(p$pass) | flag(p$sack) |
    grepl("pass|rush|run|sack",p$play_type,ignore.case=TRUE)
  pass <- flag(p$pass) | flag(p$sack) | grepl("pass|sack",p$play_type,ignore.case=TRUE)
  rush <- flag(p$rush) | grepl("rush|run",p$play_type,ignore.case=TRUE)
  turnover <- ifelse(is.finite(p$turnover_indicator),p$turnover_indicator,
    ifelse(is.finite(p$turnover),p$turnover,0))
  giveaway_description <- paste(p$play_type,p$play_text)
  giveaway_type <- grepl("intercept",giveaway_description,ignore.case=TRUE) |
    grepl("fumble recovery \\(opponent\\)|fumble return touchdown|sack touchdown",p$play_type,ignore.case=TRUE)
  giveaway <- turnover==1 & giveaway_type
  havoc <- flag(p$sack) | turnover==1 | flag(p$stuffed_run)
  cases <- list(excluded_giveaway=giveaway & !scrimmage,
    overlapping_pass_rush=scrimmage & pass & rush,
    broad_turnover_only_havoc=scrimmage & havoc & !flag(p$sack) & !flag(p$stuffed_run) & !giveaway,
    extreme_epa=abs(p$EPA)>20)
  counts <- data.frame(competitive_finite=nrow(p),scrimmage=sum(scrimmage),
    giveaways=sum(giveaway),included_giveaways=sum(giveaway & scrimmage),
    excluded_giveaways=sum(cases$excluded_giveaway),
    overlapping_pass_rush=sum(cases$overlapping_pass_rush),havoc=sum(havoc & scrimmage),
    broad_turnover_only_havoc=sum(cases$broad_turnover_only_havoc),
    success_vs_positive_epa_disagreements=sum(p$success!=(p$EPA>0) & scrimmage,na.rm=TRUE),
    source_missing_success=sum(!is.finite(p$success) & scrimmage),
    epa_min=min(p$EPA),epa_max=max(p$EPA),extreme_epa=sum(cases$extreme_epa))
  types <- examples <- list()
  for(name in names(cases)) {
    x <- p[which(cases[[name]]), ]
    if(!nrow(x))next
    types[[name]] <- cbind(case=name,as.data.frame(table(x$play_type),stringsAsFactors=FALSE))
    # Stable source-order witnesses, not outcome-selected games or repairs.
    examples[[name]] <- cbind(case=name,head(x[c("game_id","id_play","pos_team","def_pos_team",
      "play_type","play_text","EPA","pass","rush","sack","turnover_indicator","turnover","stuffed_run")],12))
  }
  list(counts=counts,types=do.call(rbind,types),examples=do.call(rbind,examples))
}

run_matchup_play_contract <- function(project) {
  config <- cfb_v2_config(project,2026L); before <- experiment_file_hashes(config)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_play_contract"),"")
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  counts <- types <- examples <- inventory <- list()
  for(year in 2020:2025) {
    message("Auditing existing play-field semantics: ",year)
    path <- file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",year,"_compact.rds"))
    result <- matchup_play_contract(readRDS(path),games)
    counts[[as.character(year)]] <- cbind(season=year,result$counts)
    if(!is.null(result$types))types[[as.character(year)]] <- cbind(season=year,result$types)
    if(!is.null(result$examples))examples[[as.character(year)]] <- cbind(season=year,result$examples)
    inventory[[as.character(year)]] <- data.frame(file=path,sha256=digest::digest(file=path,algo="sha256"))
  }
  for(name in c("counts","types","examples","inventory"))
    write.csv(do.call(rbind,get(name)),file.path(output,paste0(name,".csv")),row.names=FALSE)
  file.copy(file.path(project,"cfb_v3/experiments/matchup_play_contract.R"),file.path(output,"matchup_play_contract.R"))
  write.csv(before,file.path(output,"protected_before.csv"),row.names=FALSE)
  saveRDS(list(created=external_stamp(),scope="Read-only field-contract audit; no feature or forecast changes",
    success_definition="50/70/100 down-distance success; intentionally different from positive EPA",
    havoc_definition="Existing broad possession-change flag plus sacks and stuffed runs; not a standard third-party havoc rating"),
    file.path(output,"provenance.rds"))
  if(!identical(before,experiment_file_hashes(config)))stop("Production changed during play-field audit.")
  external_seal(output)
  output
}

run_matchup_current_contract <- function(project) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  paths <- file.path(project,c("cfb_v2/cache/pbp/pbp_2026_compact.rds",
    "cfb_v2/cache/schedules/cfbd_schedule_2026.rds","cfb_v2/cache/teams/cfbd_fbs_teams_2026.rds"))
  p <- compact_cfb_pbp(readRDS(paths[1]))
  games <- derive_games_from_pbp(p,schedule=readRDS(paths[2]),fbs_membership=readRDS(paths[3]))
  cutoff <- parse_utc_datetime("2026-09-07T00:00:00Z")
  games <- games[!is.na(games$kickoff) & games$kickoff<cutoff & games$completed, ]
  original <- matchup_play_contract(p,games)
  corrected <- matchup_play_contract(matchup_prefer_turnover_flag(p),games)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_current_contract"),"")
  write.csv(rbind(cbind(profile="original",original$counts),
    cbind(profile="flag_only_candidate",corrected$counts)),file.path(output,"counts.csv"),row.names=FALSE)
  write.csv(games,file.path(output,"source_games.csv"),row.names=FALSE)
  if(!is.null(corrected$examples))write.csv(corrected$examples,file.path(output,"candidate_examples.csv"),row.names=FALSE)
  write.csv(data.frame(file=paths,sha256=vapply(paths,function(f)digest::digest(file=f,algo="sha256"),"")),
    file.path(output,"source_inventory.csv"),row.names=FALSE)
  saveRDS(list(created=external_stamp(),kickoff_bucket_cutoff=as.character(cutoff),
    scope="Read-only 2026 source-field check, using games kicked off before the existing Monday bucket. This is not an exact information-arrival cutoff, live feature rebuild or new pick card."),
    file.path(output,"provenance.rds"))
  file.copy(file.path(project,"cfb_v3/experiments/matchup_play_contract.R"),file.path(output,"matchup_play_contract.R"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during current-source check.")
  external_seal(output)
  output
}
