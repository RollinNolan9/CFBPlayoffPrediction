# Merge only the validated game into the isolated Week 6 snapshot.
root <- normalizePath(getwd(),winslash='/')
out <- file.path(root,'cfb_v3/output/2026/week_6_rankings_relative/20261004T163129Z')
supp <- file.path(out,'espn_supplement')
for(m in c('config','store','features','adapters','historical_data'))source(file.path('cfb_v2',paste0(m,'.R')))
qa <- jsonlite::read_json(file.path(supp,'school_epa_qa.json'),simplifyVector=TRUE)
independent <- jsonlite::read_json(file.path(supp,'independent_adapter_qa.json'),simplifyVector=TRUE)
stopifnot(independent$status=='passed',independent$processed_rds_sha256==qa$processed_rds_sha256)
stopifnot(qa$status=='validated_calculated_school_supplement',qa$model_hashes_match_manifest,
  qa$finite_epa_all_counted_offensive_plays,qa$school_boxscore_exact_match)
source_path <- file.path(supp,'school_401860899_processed_full.rds')
stopifnot(digest::digest(source_path,algo='sha256',file=TRUE)==qa$processed_rds_sha256)
full <- as.data.frame(readRDS(source_path)); game_id <- '401860899'
raw <- readRDS(file.path(out,'cfb_v2/cache/schedules/cfbd_schedule_2026.rds'))
schedule <- raw[as.character(raw$game_id)==game_id,]
stopifnot(nrow(schedule)==1L,schedule$completed,schedule$home_points==26,schedule$away_points==56,
  schedule$home_team=='Colorado State',schedule$away_team=='Oregon State',
  all(as.character(full$game_id)==game_id),!anyDuplicated(full$id_play))
full$season <- full$year <- as.integer(schedule$season)
full$week <- as.integer(schedule$week)
full$home <- schedule$home_team;full$away <- schedule$away_team
for(n in c('neutral_site','conference_game','season_type','start_date','completed'))full[[n]]<-schedule[[n]]
for(side in c('home','away'))for(field in c('division','conference','pregame_elo'))
  full[[paste0(side,'_team_',field)]]<-schedule[[paste0(side,'_',field)]]
recovered <- compact_cfb_pbp(full)
cache_path <- file.path(out,'cfb_v2/cache/pbp/pbp_2026_compact.rds')
base <- readRDS(cache_path)
backup <- file.path(supp,'official_pbp_before_supplement.rds')
if (file.exists(backup)) {
  official <- readRDS(backup)
  existing_official <- base[as.character(base$game_id)!=game_id,]
  stopifnot(isTRUE(all.equal(official,existing_official,check.attributes=FALSE)))
  base <- official
}
stopifnot(!game_id %in% base$game_id,identical(names(base),names(recovered)))
if(is.numeric(base$id_play)) {
  ids <- as.numeric(recovered$id_play)
  stopifnot(all(is.finite(ids)),identical(sprintf('%.0f',ids),as.character(recovered$id_play)))
  recovered$id_play <- ids
}
updated <- rbind(base,recovered)
stopifnot(isTRUE(all.equal(base,updated[seq_len(nrow(base)),names(base)],check.attributes=FALSE)))
if (!file.exists(backup)) stopifnot(file.copy(cache_path,backup))
saveRDS(recovered,file.path(supp,'verified_school_game_compact.rds'),compress='xz')
saveRDS(updated,cache_path,compress='xz')
roundtrip <- readRDS(cache_path)
stopifnot(isTRUE(all.equal(readRDS(backup),roundtrip[seq_len(nrow(base)),names(base)],check.attributes=FALSE)))
as_of <- Sys.time()
stamp <- format(as_of,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
info <- jsonlite::read_json(file.path(out,'input_manifest.json'),simplifyVector=TRUE)
if (is.null(info$original_official_snapshot_as_of)) info$original_official_snapshot_as_of <- info$as_of
info$as_of <- stamp
info$supplement_added_at <- stamp
info$supplement_game_id <- game_id
info$official_required_games <- info$required_fbs_games-1L
info$calculated_supplement_games <- 1L
info$official_rows_unchanged <- TRUE
info$supplement_source <- qa$source_url
membership <- readRDS(file.path(out,'cfb_v2/cache/teams/cfbd_fbs_teams_2026.rds'))
universe <- derive_games_from_pbp(updated,schedule=raw,fbs_membership=membership)
completed <- filter_completed_historical_games(universe)
completed <- completed[completed$model_week<info$model_week & completed$kickoff<as_of,]
stopifnot(nrow(completed)==info$completed_prior_games)
coverage <- read.csv(file.path(out,'input_coverage.csv'),stringsAsFactors=FALSE)
target <- which(as.character(coverage$game_id)==game_id)
stopifnot(length(target)==1L)
competitive <- historical_competitive_plays(recovered)
coverage$pbp_available[target] <- TRUE
coverage$pbp_rows[target] <- nrow(recovered)
coverage$finite_epa_rows[target] <- sum(is.finite(recovered$EPA))
coverage$competitive_rows[target] <- nrow(competitive)
coverage$competitive_finite_epa_rows[target] <- sum(is.finite(competitive$EPA))
coverage$source[target] <- 'Official CSU canonical play-by-play reconstruction; pinned v2 calculated EPA; not official published EPA'
missing <- coverage$required_fbs_history & (coverage$pbp_rows==0L | coverage$competitive_finite_epa_rows==0L)
stopifnot(!any(missing),coverage$competitive_finite_epa_rows[target]>0L)
info$missing_required_games <- 0L;info$status <- 'inputs_ready'
write.csv(coverage,file.path(out,'input_coverage.csv'),row.names=FALSE,na='')
write.csv(coverage[missing,],file.path(out,'missing_required_pbp.csv'),row.names=FALSE,na='')
write.csv(completed,file.path(out,'source_games.csv'),row.names=FALSE,na='')
saveRDS(universe,file.path(out,'schedule_universe.rds'))
jsonlite::write_json(info,file.path(out,'input_manifest.json'),pretty=TRUE,auto_unbox=TRUE)
supplement <- list(game_id=game_id,away='Oregon State',home='Colorado State',
  epa_status='calculated_supplement',source_url=qa$source_url,source_name=qa$source_name,
  added_at=stamp,processed_plays=nrow(recovered),method=qa$method,
  source_path=source_path,source_sha256=qa$processed_rds_sha256,
  original_official_pbp_backup=backup,official_rows_unchanged=TRUE,
  clock_policy=qa$clock_policy,school_boxscore_exact_match=TRUE,
  authorization='User authorized ESPN calculated-EPA supplement; official school PBP reconciles corrupted ESPN records.',
  limitation='Locally calculated EPA is not equivalent to official published cfbfastR EPA; all other games retain the official feed.')
jsonlite::write_json(supplement,file.path(out,'epa_supplement.json'),pretty=TRUE,auto_unbox=TRUE)
protected <- read.csv(file.path(out,'protected_input_hashes.csv'),stringsAsFactors=FALSE)
stopifnot(identical(unname(tools::md5sum(protected$path)),protected$md5))
cat('Isolated input coverage complete:',sum(coverage$required_fbs_history),
    'FBS games; official rows unchanged; supplement',nrow(recovered),'rows.\n')
