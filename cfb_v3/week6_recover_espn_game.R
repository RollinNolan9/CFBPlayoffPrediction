options(timeout=180)
root <- normalizePath('cfb_v3/output/2026/week_5_recovery',winslash='/')
.libPaths(c(file.path(root,'r-library'),.libPaths()))
Sys.setenv(R_USER_CACHE_DIR=file.path(root,'r-user-cache'))
options(cfbfastR.pbp_engine='v2')
suppressPackageStartupMessages(library(cfbfastR))
suppressPackageStartupMessages(library(dplyr))
stopifnot(inherits(get('ep_model',asNamespace('cfbfastR')),'xgb.Booster'))
manifest_path <- file.path(root,'cfb_model_artifacts_MANIFEST_20260927.json')
manifest <- jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
assign('manifest',manifest,envir=get('.cfb_model_env',asNamespace('cfbfastR')))
game_id <- '401860899'
stamp <- format(Sys.time(),'%Y%m%dT%H%M%SZ',tz='UTC')
out <- normalizePath('cfb_v3/output/2026/week_6_rankings_relative/20261004T163129Z',winslash='/')
out <- file.path(out,'espn_supplement')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
url <- paste0('https://site.api.espn.com/apis/site/v2/sports/football/college-football/summary?event=',game_id)
warnings_seen <- character()
tryCatch({
  frozen_json <- commandArgs(trailingOnly=TRUE)
  if(length(frozen_json)) {
    raw_text <- paste(readLines(frozen_json[[1]],warn=FALSE,encoding='UTF-8'),collapse='\n')
  } else {
    response <- httr2::request(url) |> httr2::req_perform()
    raw_text <- httr2::resp_body_string(response)
  }
  raw_path <- file.path(out,'espn_summary_401860899.json')
  if(length(frozen_json))stopifnot(file.copy(frozen_json[[1]],raw_path,overwrite=FALSE)) else writeLines(raw_text,raw_path,useBytes=TRUE)
  raw <- jsonlite::fromJSON(raw_path,simplifyVector=FALSE)
  competition <- raw$header$competitions[[1]]
  stopifnot(identical(as.character(raw$header$id),game_id),isTRUE(competition$status$type$completed))
  home <- Filter(function(x)x$homeAway=='home',competition$competitors)[[1]]
  away <- Filter(function(x)x$homeAway=='away',competition$competitors)[[1]]
  stopifnot(home$team$location=='Colorado State',away$team$location=='Oregon State',
            as.numeric(home$score)==26,as.numeric(away$score)==56)
  all_drives <- raw$drives$previous
  if(!is.null(raw$drives$current))all_drives <- c(all_drives,list(raw$drives$current))
  raw_plays <- unlist(lapply(all_drives,function(d)d$plays),recursive=FALSE)
  raw_ids <- vapply(raw_plays,function(x)as.character(x$id),'')
  raw_periods <- sort(unique(vapply(raw_plays,function(x)as.integer(x$period$number),1L)))
  stopifnot(length(raw_ids)>=100L,!anyDuplicated(raw_ids),all(1:4 %in% raw_periods))
  processed <- data.frame() # Process exactly the preserved summary below; no second feed.
  processing_route <- 'espn_cfb_pbp(engine=v2,output=full)'
  if(!is.data.frame(processed)||nrow(processed)<100L) {
    # The package documents .espn_to_epa_input as shared by site-v2 summary and core-v2.
    # Map preserved site-v2 JSON onto that exact plays_*/drive_* contract.
    `%||%` <- function(a,b)if(is.null(a))b else a
    mapped <- dplyr::bind_rows(lapply(all_drives,function(d)dplyr::bind_rows(lapply(d$plays,function(p) {
      data.frame(plays_text=p$text,plays_type_text=p$type$text,
        plays_start_down=as.numeric(p$start$down %||% NA_real_),
        plays_start_distance=as.numeric(p$start$distance %||% NA_real_),
        plays_period_number=p$period$number,plays_id=as.character(p$id),
        plays_start_yards_to_endzone=as.numeric(p$start$yardsToEndzone %||% NA_real_),
        plays_end_yards_to_endzone=as.numeric(p$end$yardsToEndzone %||% NA_real_),
        plays_stat_yardage=as.numeric(p$statYardage %||% 0),
        plays_start_yard_line=as.numeric(p$start$yardLine %||% NA_real_),
        plays_clock_display_value=p$clock$displayValue,
        plays_start_team_id=as.character(p$start$team$id %||% NA_character_),
        plays_home_score=p$homeScore,plays_away_score=p$awayScore,
        drive_id=as.character(d$id),drive_start_text=d$start$text %||% NA_character_,
        drive_start_yard_line=as.numeric(d$start$yardLine %||% NA_real_),
        drive_end_text=d$end$text %||% NA_character_,
        drive_end_yard_line=as.numeric(d$end$yardLine %||% NA_real_),drive_yards=d$yards %||% NA_real_,
        home_team=home$team$location,away_team=away$team$location,
        home_team_id=as.character(home$team$id),away_team_id=as.character(away$team$id),
        home_team_abbreviation=home$team$abbreviation,away_team_abbreviation=away$team$abbreviation,
        season=raw$header$season$year,wk=raw$header$week,scoring=isTRUE(p$scoringPlay),
        stringsAsFactors=FALSE)
    }))))
    mapped <- mapped[order(match(mapped$plays_id,raw_ids)),]
    adapter <- cfbfastR:::.espn_to_epa_input(mapped,game_id=game_id)
    saveRDS(adapter,file.path(out,'espn_model_input.rds'))
    processed <- cfbfastR:::.run_epa_wpa(adapter,
      ep_model=get('ep_model',asNamespace('cfbfastR')),
      fg_model=get('fg_model',asNamespace('cfbfastR')),
      wp_model=get('wp_model',asNamespace('cfbfastR')),season=2026)
    processed <- cfbfastR:::.pbp_apply_output_schema(processed,output='full')
    processing_route <- 'Saved ESPN site-v2 summary -> .espn_to_epa_input -> .run_epa_wpa -> full output schema'
  }
  stopifnot(is.data.frame(processed),nrow(processed)>=100L,
    identical(unique(as.character(processed$game_id)),game_id),
    !anyDuplicated(paste(processed$game_id,processed$id_play)),
    all(1:4 %in% as.integer(as.character(processed$period))),
    !any(is.infinite(processed$EPA)),sum(is.finite(processed$EPA))>100L)
  processed$pbp_source <- 'ESPN live play-by-play'
  processed$epa_source <- 'Locally calculated cfbfastR 3.0.0.9000 v2; not official published cfbfastR EPA'
  processed$epa_estimated_supplement <- TRUE
  processed$source_retrieved_at_utc <- stamp
  processed_path <- file.path(out,'espn_401860899_processed_full.rds')
  saveRDS(processed,processed_path)
  ordinary <- ((!is.na(processed$rush)&processed$rush==1)|(!is.na(processed$pass)&processed$pass==1)) &
    !grepl('Kneel|Spike',processed$play_text,ignore.case=TRUE)
  ids <- as.character(processed$id_play)
  stopifnot(setequal(raw_ids,ids), all(is.finite(processed$EPA[ordinary & !is.na(ordinary)])),
    any(grepl('End of (Game|4th quarter)',tail(processed$play_text,3),ignore.case=TRUE)))
  scores <- lapply(c('Colorado State','Oregon State'),function(team)list(team=team,
    maximum_score=max(c(processed$pos_team_score[processed$pos_team==team],
      processed$def_pos_team_score[processed$def_pos_team==team]),na.rm=TRUE)))
  model_dir <- file.path(tools::R_user_dir('cfbfastR',which='cache'),'models')
  model_paths <- list.files(model_dir,pattern='^(ep_model|fg_model|wp_naive)\\.ubj$',full.names=TRUE)
  models <- lapply(model_paths,function(path)list(file=basename(path),path=path,
    sha256=digest::digest(path,algo='sha256',file=TRUE)))
  stopifnot(length(models)==3L)
  for(m in models)stopifnot(identical(m$sha256,manifest$assets[[m$file]]$sha256))
  stopifnot(file.copy(manifest_path,file.path(out,'model_manifest.json'),overwrite=FALSE))
  qa <- list(status='processed_for_user_authorized_estimated_supplement',created_at_utc=stamp,
    game_id=game_id,home='Colorado State',away='Oregon State',final_home_score=26,final_away_score=56,
    source_url=url,processing_route=processing_route,raw_summary_plays=length(raw_ids),raw_periods=raw_periods,
    processed_plays=nrow(processed),processed_columns=ncol(processed),
    processed_period_counts=as.list(table(processed$period)),
    processed_teams=sort(unique(as.character(processed$pos_team))),
    raw_only_play_ids=setdiff(raw_ids,ids),processed_only_play_ids=setdiff(ids,raw_ids),
    duplicate_processed_ids=sum(duplicated(ids)),
    same_offense_defense=sum(processed$pos_team==processed$def_pos_team,na.rm=TRUE),
    finite_epa=sum(is.finite(processed$EPA)),missing_epa=sum(is.na(processed$EPA)),
    missing_epa_types=as.list(table(processed$play_type[!is.finite(processed$EPA)])),
    ordinary_rush_pass_plays_excluding_kneels_spikes=sum(ordinary,na.rm=TRUE),
    finite_epa_on_ordinary_plays=sum(ordinary&is.finite(processed$EPA),na.rm=TRUE),
    observed_maximum_team_scores=scores,
    tail_plays=as.data.frame(tail(processed[,intersect(c('id_play','period','play_type','play_text',
      'pos_team','def_pos_team','pos_team_score','def_pos_team_score','EPA'),names(processed))],3)),
    package_version=as.character(packageVersion('cfbfastR')),
    package_source_commit=trimws(system2('git',c('-C',shQuote(file.path(root,'cfbfastR-v3-source')),
      'rev-parse','HEAD'),stdout=TRUE)),engine='v2',output='full',ep_model_class=class(get('ep_model',asNamespace('cfbfastR'))),
    model_artifacts=models,model_hashes_match_manifest=TRUE,
    model_manifest_sha256=digest::digest(manifest_path,algo='sha256',file=TRUE),
    raw_retrieved_at_utc=format(file.info(if(length(frozen_json))frozen_json[[1]]else raw_path)$mtime,
      '%Y-%m-%dT%H:%M:%SZ',tz='UTC'),warnings=warnings_seen,
    limitation='This game uses ESPN plays and local model-estimated EPA as explicitly requested. EPA is not identical to the published classic dataset. Existing games must remain unchanged; supplement this game only.',
    raw_json_path=raw_path,raw_json_sha256=digest::digest(raw_path,algo='sha256',file=TRUE),
    processed_rds_path=processed_path,processed_rds_sha256=digest::digest(processed_path,algo='sha256',file=TRUE))
  stopifnot(qa$same_offense_defense==0L,qa$duplicate_processed_ids==0L,
            setequal(qa$processed_teams,c('Colorado State','Oregon State')),
            scores[[1]]$maximum_score==26,scores[[2]]$maximum_score==56)
  jsonlite::write_json(qa,file.path(out,'qa.json'),auto_unbox=TRUE,pretty=TRUE,na='null')
  file.copy('cfb_v3/week6_recover_espn_game.R',file.path(out,'recover_espn_game.R'),overwrite=FALSE)
  cat('ESPN_RECOVERY_DIRECTORY:',out,'\n')
  print(qa[c('processed_plays','processed_columns','finite_epa','missing_epa','missing_epa_types',
    'ordinary_rush_pass_plays_excluding_kneels_spikes','finite_epa_on_ordinary_plays',
    'raw_only_play_ids','processed_only_play_ids','tail_plays')])
},error=function(e){
  jsonlite::write_json(list(status='failed',error=conditionMessage(e),warnings=warnings_seen),
    file.path(out,'failure.json'),auto_unbox=TRUE,pretty=TRUE)
  cat('ESPN_RECOVERY_FAILED:',conditionMessage(e),'\nDIRECTORY:',out,'\n')
  quit(status=1)
})
