matchup_capture_boxscores <- function(project,years=2022:2025) {
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_box_sources"),"")
  inventory <- list()
  slices <- data.frame(type=c("regular","regular","postseason"),week=c(1,8,1))
  for(year in years)for(k in seq_len(nrow(slices))) {
    type <- slices$type[k]; week <- slices$week[k]
    message("Capturing reported team box scores: ",year," ",type," week ",week)
    file <- paste0("box_",year,"_",type,"_",week,".json"); path <- file.path(output,file)
    url <- "https://api.collegefootballdata.com/games/teams"
    stamp <- external_fetch(url,path,list(year=year,seasonType=type,week=week),authenticated=TRUE)
    inventory[[file]] <- data.frame(season=year,season_type=type,week=week,file=file,
      url=paste0(url,"?year=",year,"&seasonType=",type,"&week=",week),captured_at=stamp,
      sha256=digest::digest(file=path,algo="sha256"))
    write.csv(do.call(rbind,inventory),file.path(output,"source_inventory.csv"),row.names=FALSE)
  }
  writeLines("Fixed audit slices: 2022-2025 regular Weeks 1 and 8, plus postseason Week 1. Not full-season coverage.",file.path(output,"SCOPE.txt"))
  external_seal(output)
  output
}

matchup_read_boxscores <- function(directory,games) {
  external_verify(directory); assert_unique_keys(games,"game_id","box-score schedule")
  inventory <- read.csv(file.path(directory,"source_inventory.csv"),stringsAsFactors=FALSE)
  scalar <- function(x,name) {
    v <- x[[name]]
    if(is.null(v) || !length(v))return(NA_character_)
    if(length(v)!=1)stop("Non-scalar box-score field: ",name)
    as.character(v)
  }
  rows <- list()
  for(k in seq_len(nrow(inventory))) {
    raw <- jsonlite::fromJSON(file.path(directory,inventory$file[k]),simplifyVector=FALSE)
    for(game in raw) {
      id <- scalar(game,"id"); i <- match(id,as.character(games$game_id))
      if(is.na(i))next
      g <- games[i, ]
      for(team in game$teams) {
        school <- canonical_team(scalar(team,"team")); location <- scalar(team,"homeAway")
        side <- if(isTRUE(school==g$home))"home" else if(isTRUE(school==g$away))"away" else NA_character_
        reason <- if(is.na(side))"team_mismatch" else "matched"
        if(g$season!=inventory$season[k])reason <- "season_mismatch"
        if(!is.na(side)) {
          points <- suppressWarnings(as.numeric(scalar(team,"points")))
          if(!is.finite(points) || points!=g[[paste0(side,"_score")]])reason <- "score_mismatch"
          if(!identical(side,location) && !isTRUE(g$neutral_site))reason <- "side_mismatch"
        }
        value <- function(category) {
          hit <- Filter(function(s)identical(s$category,category),team$stats)
          if(length(hit)!=1)return(NA_real_)
          n <- suppressWarnings(as.numeric(scalar(hit[[1]],"stat")))
          if(!is.finite(n) || n<0 || n!=floor(n))NA_real_ else n
        }
        totals <- c(turnovers=value("turnovers"),interceptions=value("interceptions"),fumbles_lost=value("fumblesLost"))
        if(reason=="matched" && any(!is.finite(totals)))reason <- "missing_turnover_stats"
        if(reason=="matched" && totals[1]!=sum(totals[-1]))reason <- "unreconciled_reported_totals"
        rows[[length(rows)+1L]] <- data.frame(game_id=id,team=school,season=g$season,
          home=g$home,away=g$away,status=reason,as.list(totals),source_file=inventory$file[k])
      }
    }
  }
  if(!length(rows))stop("No box-score rows matched the foundation.")
  x <- do.call(rbind,rows)
  keys <- paste(x$game_id,x$team)
  x$duplicate_status <- "unique"
  for(i in split(seq_len(nrow(x)),keys))if(length(i)>1) {
    if(nrow(unique(x[i,c("turnovers","interceptions","fumbles_lost","status")]))==1)
      x$duplicate_status[i[-1]] <- "identical_removed" else x$duplicate_status[i] <- "conflicting_quarantined"
  }
  x
}

run_matchup_box_audit <- function(project,directory) {
  config <- cfb_v2_config(project,2026L); before <- experiment_file_hashes(config)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  games <- games[games$season<=2025, ]
  boxes <- matchup_read_boxscores(directory,games)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_box_audit"),"")
  write.csv(boxes,file.path(output,"reported_stats_ledger.csv"),row.names=FALSE)
  usable <- boxes[boxes$status=="matched" & boxes$duplicate_status=="unique", ]
  assert_unique_keys(usable,c("game_id","team"),"qualified box-score totals")
  counts <- details <- review <- list()
  for(year in sort(unique(usable$season))) {
    message("Reconciling full-game source turnover counts: ",year)
    pbp <- compact_cfb_pbp(readRDS(file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",year,"_compact.rds"))))
    fixed <- matchup_prefer_turnover_flag(pbp)
    x <- usable[usable$season==year, ]; index <- match(x$game_id,games$game_id)
    x <- x[raw_history_eligible(games[index, ]), ]
    legacy <- corrected <- rep(NA_real_,nrow(x))
    all_types_finite <- all_types_any_epa <- rep(NA_real_,nrow(x))
    x$pbp_status <- "available"
    for(i in seq_len(nrow(x))) {
      rows <- !is.na(pbp$game_id) & pbp$game_id==x$game_id[i]
      if(!any(rows) || !any(pbp$pos_team[rows]==x$team[i],na.rm=TRUE)) {
        x$pbp_status[i] <- "missing_team_plays"
        next
      }
      legacy[i] <- matchup_giveaway_count(pbp[rows, ],x$team[i])
      corrected[i] <- matchup_giveaway_count(fixed[rows, ],x$team[i])
      all_types_finite[i] <- matchup_giveaway_count(fixed[rows, ],x$team[i],include_all_play_types=TRUE)
      all_types_any_epa[i] <- matchup_giveaway_count(fixed[rows, ],x$team[i],include_all_play_types=TRUE,require_epa=FALSE)
      if(corrected[i]!=x$turnovers[i]) {
        p <- pbp[rows, ]; team_rows <- !is.na(p$pos_team) & p$pos_team==x$team[i]
        event <- (!is.na(p$turnover) & p$turnover==1) |
          grepl("fumble|intercept",paste(p$play_type,p$play_text),ignore.case=TRUE)
        p <- p[team_rows & event,c("game_id","id_play","pos_team","play_type","play_text",
          "EPA","pass","rush","sack","turnover","turnover_indicator")]
        if(nrow(p)) {
          p$reported_team_total <- x$turnovers[i]
          p$corrected_team_count <- corrected[i]
          p$counted_by_flag_correction <- vapply(seq_len(nrow(p)),function(j)
            matchup_giveaway_count(matchup_prefer_turnover_flag(p[j, ]),x$team[i])==1,logical(1))
          review[[paste(x$game_id[i],x$team[i])]] <- p
        }
      }
    }
    x$legacy_scrimmage_finite_epa <- legacy
    x$corrected_scrimmage_finite_epa <- corrected
    x$candidate_all_types_finite_epa <- all_types_finite
    x$candidate_all_types_any_epa <- all_types_any_epa
    x$legacy_shortfall <- x$turnovers-legacy; x$corrected_shortfall <- x$turnovers-corrected
    details[[as.character(year)]] <- x
    available <- x$pbp_status=="available"
    counts[[as.character(year)]] <- data.frame(season=year,team_games=nrow(x),missing_pbp=sum(!available),
      reported_total_turnovers=sum(x$turnovers[available]),legacy_scrimmage_count=sum(legacy,na.rm=TRUE),
      corrected_scrimmage_count=sum(corrected,na.rm=TRUE),
      legacy_exact=sum(legacy==x$turnovers,na.rm=TRUE),corrected_exact=sum(corrected==x$turnovers,na.rm=TRUE),
      corrected_below_total=sum(corrected<x$turnovers,na.rm=TRUE),corrected_above_total=sum(corrected>x$turnovers,na.rm=TRUE))
    counts[[as.character(year)]]$additional_events_outside_scrimmage_gate <- sum(all_types_finite-corrected,na.rm=TRUE)
    counts[[as.character(year)]]$additional_events_without_finite_epa <- sum(all_types_any_epa-all_types_finite,na.rm=TRUE)
    counts[[as.character(year)]]$all_event_count_exact <- sum(all_types_any_epa==x$turnovers,na.rm=TRUE)
  }
  write.csv(do.call(rbind,counts),file.path(output,"season_reconciliation.csv"),row.names=FALSE)
  write.csv(do.call(rbind,details),file.path(output,"team_game_reconciliation.csv"),row.names=FALSE)
  if(length(review))write.csv(do.call(rbind,review),file.path(output,"discrepant_game_play_queue.csv"),row.names=FALSE)
  write.csv(before,file.path(output,"protected_before.csv"),row.names=FALSE)
  saveRDS(list(source_dir=directory,created=external_stamp(),
    limits="Reported box totals include all plays; tested feature counts require scrimmage flags and finite EPA. Differences are review queues, not automatic corrections. CFBD and ESPN may share underlying providers.",
    scope="Data-quality audit only; no prediction or source-count replacement"),file.path(output,"provenance.rds"))
  for(file in c("matchup_box_audit.R","matchup_turnover_witness.R","matchup_foundation_audit.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  if(!identical(before,experiment_file_hashes(config)))stop("Production changed during box-score audit.")
  external_seal(output)
  output
}
