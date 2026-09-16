matchup_source_probe <- function(project, years=c(2021,2023)) {
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_source_probe"),"")
  ledger <- list()
  for (year in years) {
    url <- paste0("https://github.com/sportsdataverse/sportsdataverse-data/releases/download/cfbfastR_cfb_pbp/play_by_play_",year,".rds")
    file <- paste0("upstream_",year,".rds"); path <- file.path(output,file)
    message("Checking upstream full PBP schema: ",year)
    stamp <- external_fetch(url,path)
    ledger[[file]] <- data.frame(file=file,url=url,captured_at=stamp,sha256=digest::digest(file=path,algo="sha256"))
    write.csv(do.call(rbind,ledger),file.path(output,"source_inventory.csv"),row.names=FALSE)
  }
  external_seal(output)
  output
}

matchup_verified_duplicate_rows <- function(full, cached) {
  required <- c("game_id","game_play_number","clock_minutes","clock_seconds","down","distance","play_text","EPA")
  assert_columns(full,required,"full source identity")
  compact <- compact_cfb_pbp(full); reference <- compact_cfb_pbp(cached)
  same <- all.equal(compact,reference,check.attributes=FALSE,tolerance=0)
  if (!isTRUE(same)) stop("Upstream is not identical to normalized cached inputs: ",paste(same,collapse="; "))
  # Compare the full identity-rich rows only where compact rows already coincide.
  candidates <- which(duplicated(compact) | duplicated(compact,fromLast=TRUE))
  remove <- rep(FALSE,nrow(full))
  if (length(candidates)) remove[candidates] <- duplicated(as.data.frame(full[candidates, ,drop=FALSE]))
  remove
}

matchup_duplicate_control <- function(project,prepared_dir,source_dirs) {
  config <- cfb_v2_config(project,2026L); before <- experiment_file_hashes(config)
  external_verify(prepared_dir)
  for (directory in source_dirs) external_verify(directory)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_deduplicated"),"")
  x <- readRDS(file.path(prepared_dir,"model_inputs.rds"))
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  history <- audit <- removed_rows <- inventory <- list()
  for (year in 2020:2025) {
    filename <- paste0("upstream_",year,".rds")
    paths <- file.path(source_dirs,filename); paths <- paths[file.exists(paths)]
    if (length(paths)!=1L) stop("Require exactly one verified full source per season.")
    message("Auditing exact upstream duplicate plays: ",year)
    full <- readRDS(paths); cached <- readRDS(file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",year,"_compact.rds")))
    remove <- matchup_verified_duplicate_rows(full,cached)
    removed_rows[[as.character(year)]] <- data.frame(season=rep(year,sum(remove)),source_row=which(remove),
      as.data.frame(full[remove,c("game_id","game_play_number","clock_minutes","clock_seconds","down","distance","play_type","play_text","EPA")]))
    built <- matchup_aggregate_plays(cached[!remove, ],games)
    history[[as.character(year)]] <- built$team_games
    audit[[as.character(year)]] <- cbind(data.frame(season=year,full_rows=nrow(full),
      exact_full_duplicates=sum(remove),normalized_cache_identical=TRUE),built$audit)
    inventory[[as.character(year)]] <- data.frame(season=year,file=paths,sha256=digest::digest(file=paths,algo="sha256"))
    rm(full,cached); gc(verbose=FALSE)
  }
  h <- do.call(rbind,history); built <- matchup_build_features(h,x)
  i <- match(x$game_id,built$features$game_id)
  fields <- setdiff(names(built$features),"game_id")
  original <- as.matrix(x[fields]); x[fields] <- built$features[i,fields]
  changed <- data.frame(game_id=x$game_id,season=x$season,week=x$week,
    changed_features=rowSums(abs(original-as.matrix(x[fields]))>1e-12),
    maximum_absolute_change=apply(abs(original-as.matrix(x[fields])),1,max))
  saveRDS(x,file.path(output,"model_inputs.rds")); saveRDS(h,file.path(output,"team_game_counts.rds"))
  write.csv(built$audit,file.path(output,"feature_source_audit.csv"),row.names=FALSE)
  write.csv(do.call(rbind,audit),file.path(output,"duplicate_source_audit.csv"),row.names=FALSE)
  write.csv(do.call(rbind,removed_rows),file.path(output,"verified_removed_rows.csv"),row.names=FALSE)
  write.csv(do.call(rbind,inventory),file.path(output,"play_source_inventory.csv"),row.names=FALSE)
  write.csv(changed,file.path(output,"feature_changes.csv"),row.names=FALSE)
  saveRDS(list(prepared_dir=prepared_dir,source_dirs=source_dirs,
    scope="Exact full-source duplicates only; original power/external forecasts and production unchanged",
    created=external_stamp()),file.path(output,"provenance.rds"))
  for (file in c("MATCHUP_PROTOCOL.md","matchup_source_probe.R","matchup_features.R","matchup_research.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  write.csv(before,file.path(output,"protected_before.csv"),row.names=FALSE)
  if (!identical(before,experiment_file_hashes(config))) stop("Production changed during duplicate audit.")
  external_seal(output)
  output
}
