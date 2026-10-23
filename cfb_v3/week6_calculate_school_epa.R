# Offline, source-validated one-game supplement using the existing pinned runtime.
recovery <- normalizePath('cfb_v3/output/2026/week_5_recovery', winslash='/')
out <- normalizePath('cfb_v3/output/2026/week_6_rankings_relative/20261004T163129Z/espn_supplement', winslash='/')
.libPaths(c(file.path(recovery,'r-library'),.libPaths()))
Sys.setenv(R_USER_CACHE_DIR=file.path(recovery,'r-user-cache'))
options(cfbfastR.pbp_engine='v2')
suppressPackageStartupMessages(library(cfbfastR))
suppressPackageStartupMessages(library(dplyr))
manifest_path <- file.path(recovery,'cfb_model_artifacts_MANIFEST_20260927.json')
manifest <- jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
assign('manifest',manifest,envir=get('.cfb_model_env',asNamespace('cfbfastR')))
source_qa <- jsonlite::read_json(file.path(out,'school_reconstruction_qa.json'),simplifyVector=TRUE)
stopifnot(source_qa$status == 'canonical_school_states_validated',
  source_qa$boxscore_counts_and_yards_match, source_qa$counted_offensive_plays == 127L)
x <- read.csv(file.path(out,'school_model_input.csv'),stringsAsFactors=FALSE,
  colClasses=c(plays_id='character',plays_start_team_id='character',
    home_team_id='character',away_team_id='character'))
x$scoring <- tolower(as.character(x$scoring))=='true'
stopifnot(!anyDuplicated(x$plays_id),identical(order(x$plays_id),seq_len(nrow(x))))
adapter <- cfbfastR:::.espn_to_epa_input(x,game_id='401860899')
# Count only actual team timeouts from the authoritative narration. The generic
# ESPN adapter's abbreviated-team parser cannot parse full school team names.
for (half in unique(adapter$half)) {
  jj <- which(adapter$half == half)
  home_remaining <- 3-cumsum(grepl('^Timeout Colorado State,',x$school_text[jj]))
  away_remaining <- 3-cumsum(grepl('^Timeout Oregon State,',x$school_text[jj]))
  adapter$offense_timeouts[jj] <- ifelse(adapter$offense_play[jj]=='Colorado State',home_remaining,away_remaining)
  adapter$defense_timeouts[jj] <- ifelse(adapter$offense_play[jj]=='Colorado State',away_remaining,home_remaining)
}
# The source is school state data mapped to the existing ESPN adapter contract.
# Supply exact drive boundary states rather than its abbreviation-based fallback.
for (drive in unique(adapter$drive_id)) {
  ii <- which(adapter$drive_id==drive)
  adapter$drive_start_yards_to_goal[ii] <- adapter$yards_to_goal[ii[1L]]
  adapter$drive_end_yards_to_goal[ii] <- adapter$end_yards_to_endzone[tail(ii,1L)]
}
normal <- x$plays_type_text %in% c('Rush','Rushing Touchdown','Pass Reception',
  'Passing Touchdown','Pass Incompletion','Pass Interception Return','Sack','Fumble Recovery (Opponent)')
stopifnot(sum(normal)==127L,all(is.finite(adapter$yards_to_goal[normal])),
  all(adapter$yards_to_goal[normal]>0 & adapter$yards_to_goal[normal]<=100),
  all(adapter$down[normal] %in% 1:4),all(adapter$distance[normal]>0),
  all(is.finite(adapter$clock_minutes[normal])),all(is.finite(adapter$clock_seconds[normal])))
saveRDS(adapter,file.path(out,'school_model_input.rds'))
warnings_seen <- character()
# Keep the pinned engine unchanged on disk. A local wrapper supplies canonical
# afterstates for accepted penalties after its generic yardage-based inference.
engine <- cfbfastR:::.run_epa_wpa
engine_environment <- new.env(parent=environment(engine))
engine_environment$.pbp_prep_epa_df_after <- function(dat) {
  dat <- cfbfastR:::.pbp_prep_epa_df_after(dat)
  idx <- match(dat$id_play,x$plays_id)
  eligible <- grepl('PENALTY',x$school_text[idx],fixed=TRUE) & !x$scoring[idx] &
    !grepl('declined',x$school_text[idx],ignore.case=TRUE)
  changes <- list()
  for (i in which(eligible)) {
    parts <- strsplit(x$school_new_context[idx[i]],',',fixed=TRUE)[[1L]]
    side <- parts[1L]; spot <- parts[4L]
    next_down <- as.numeric(parts[2L]); next_distance <- as.numeric(parts[3L])
    yard <- as.numeric(substring(spot,2L))
    next_yardline <- if (substring(spot,1L,1L)==side) 100-yard else yard
    changes[[length(changes)+1L]] <- data.frame(school_play_id=x$school_play_id[idx[i]],
      model_id=dat$id_play[i],old_down=dat$new_down[i],school_down=next_down,
      old_distance=dat$new_distance[i],school_distance=next_distance,
      old_yards_to_goal=dat$new_yardline[i],school_yards_to_goal=next_yardline)
    dat$new_down[i] <- next_down;dat$new_distance[i] <- next_distance
    dat$new_yardline[i] <- next_yardline;dat$new_log_ydstogo[i] <- log(next_distance)
    dat$new_Goal_To_Go[i] <- next_yardline<=next_distance
  }
  stopifnot(length(changes)==6L)
  write.csv(do.call(rbind,changes),file.path(out,'canonical_penalty_afterstate_audit.csv'),row.names=FALSE)
  saveRDS(dat,file.path(out,'school_pre_epa_states.rds'))
  dat
}
environment(engine) <- engine_environment
processed <- withCallingHandlers(engine(adapter,
  ep_model=get('ep_model',asNamespace('cfbfastR')),
  fg_model=get('fg_model',asNamespace('cfbfastR')),
  wp_model=get('wp_model',asNamespace('cfbfastR')),season=2026L),
  warning=function(w){warnings_seen <<- c(warnings_seen,conditionMessage(w));invokeRestart('muffleWarning')})
processed <- cfbfastR:::.pbp_apply_output_schema(processed,output='full')
ii <- match(x$plays_id,processed$id_play)
stopifnot(nrow(processed)==nrow(x),!anyNA(ii),!anyDuplicated(processed$id_play),
  setequal(processed$pos_team,c('Oregon State','Colorado State')),
  all(processed$pos_team!=processed$def_pos_team),!any(is.infinite(processed$EPA)),
  all(is.finite(processed$EPA[ii[normal]])),all(1:4 %in% processed$period),
  max(processed$pos_team_score[processed$pos_team=='Oregon State'])==56,
  max(processed$pos_team_score[processed$pos_team=='Colorado State'])==26)
processed$pbp_source <- 'Official Colorado State canonical play-by-play; ESPN inconsistencies rejected'
processed$epa_source <- 'Pinned cfbfastR 3.0.0.9000 v2 calculated EPA; not official published EPA'
processed$epa_estimated_supplement <- TRUE
processed$school_play_id <- x$school_play_id[match(processed$id_play,x$plays_id)]
processed_path <- file.path(out,'school_401860899_processed_full.rds')
saveRDS(processed,processed_path)
model_dir <- file.path(tools::R_user_dir('cfbfastR',which='cache'),'models')
model_paths <- list.files(model_dir,pattern='^(ep_model|fg_model|wp_naive)\\.ubj$',full.names=TRUE)
models <- lapply(model_paths,function(path)list(file=basename(path),path=path,
  sha256=digest::digest(path,algo='sha256',file=TRUE)))
stopifnot(length(models)==3L)
for(m in models)stopifnot(identical(m$sha256,manifest$assets[[m$file]]$sha256))
jsonlite::write_json(list(status='validated_calculated_school_supplement',
  created_at_utc=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),game_id='401860899',
  away='Oregon State',home='Colorado State',final_away_score=56,final_home_score=26,
  source_url=source_qa$source_url,source_name=source_qa$source_name,
  method='Official school canonical states mapped to pinned cfbfastR v2 model; corrupted ESPN records rejected.',
  processed_plays=nrow(processed),counted_offensive_plays=sum(normal),
  finite_epa=sum(is.finite(processed$EPA)),finite_epa_all_counted_offensive_plays=TRUE,
  missing_epa_types=as.list(table(processed$play_type[!is.finite(processed$EPA)])),
  no_duplicate_ids=TRUE,synthetic_id_policy=source_qa$id_policy,
  clock_policy=source_qa$clock_policy,score_policy=source_qa$score_policy,
  school_boxscore_exact_match=TRUE,model_artifacts=models,model_hashes_match_manifest=TRUE,
  accepted_penalty_afterstates='Six non-scoring accepted-penalty plays use official next down/distance/field state before EP evaluation; touchdown states retain model scoring conventions.',
  afterstate_override_stage='Inside local .pbp_prep_epa_df_after wrapper, before unchanged .pbp_create_epa calls pinned EP prediction on before/after states. Full pre-EPA frame retained in school_pre_epa_states.rds.',
  narration_names='Exact official roster name replacements only; no greedy text regex.',
  model_manifest_sha256=digest::digest(manifest_path,algo='sha256',file=TRUE),
  processed_rds_sha256=digest::digest(processed_path,algo='sha256',file=TRUE),
  package_version=as.character(packageVersion('cfbfastR')),
  warnings=warnings_seen,epa_status='calculated_supplement'),
  file.path(out,'school_epa_qa.json'),auto_unbox=TRUE,pretty=TRUE,na='null')
cat('School EPA validated:',nrow(processed),'rows;',sum(normal),'counted offensive plays all finite EPA.\n')
