matchup_giveaway_count <- function(p,team,include_all_play_types=FALSE,require_epa=TRUE) {
  flag <- function(x) !is.na(x) & x==1
  scrimmage <- flag(p$rush) | flag(p$pass) | flag(p$sack) |
    grepl("pass|rush|run|sack",p$play_type,ignore.case=TRUE)
  resolved <- ifelse(is.finite(p$turnover_indicator),p$turnover_indicator,
    ifelse(is.finite(p$turnover),p$turnover,0))
  giveaway <- grepl("intercept",paste(p$play_type,p$play_text),ignore.case=TRUE) |
    grepl("fumble recovery \\(opponent\\)|fumble return touchdown|sack touchdown",p$play_type,ignore.case=TRUE)
  eligible_type <- if(include_all_play_types)rep(TRUE,nrow(p)) else scrimmage
  eligible_epa <- if(require_epa)is.finite(p$EPA) else rep(TRUE,nrow(p))
  sum(p$pos_team==team & eligible_type & eligible_epa & resolved==1 & giveaway,na.rm=TRUE)
}

run_matchup_turnover_witness <- function(project,source_dir) {
  config <- cfb_v2_config(project,2026L); before <- experiment_file_hashes(config)
  external_verify(source_dir)
  inventory <- read.csv(file.path(source_dir,"source_inventory.csv"),stringsAsFactors=FALSE)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_turnover_checks"),"")
  checks <- examples <- list()
  for(i in seq_len(nrow(inventory))) {
    source <- jsonlite::fromJSON(file.path(source_dir,inventory$file[i]),simplifyVector=FALSE)
    id <- as.character(source$header$id); g <- games[as.character(games$game_id)==id, ]
    if(nrow(g)!=1 || id!=inventory$game_id[i] || g$season!=source$header$season$year)
      stop("ESPN witness identity/season mismatch.")
    competitors <- source$header$competitions[[1]]$competitors
    for(team in competitors) {
      side <- team$homeAway
      if(!side %in% c("home","away"))stop("Unknown ESPN side.")
      if(canonical_team(team$team$location)!=g[[side]] || as.numeric(team$score)!=g[[paste0(side,"_score")]])
        stop("ESPN witness team or score mismatch.")
    }
    p <- readRDS(file.path(project,"cfb_v2/cache/pbp",paste0("pbp_",g$season,"_compact.rds")))
    p <- compact_cfb_pbp(p[as.character(p$game_id)==id, ])
    competitive <- historical_competitive_plays(p)
    fixed <- matchup_prefer_turnover_flag(p)
    fixed_competitive <- matchup_prefer_turnover_flag(competitive)
    for(team in source$boxscore$teams) {
      name <- canonical_team(team$team$location)
      if(!name %in% c(g$home,g$away))stop("Unmapped box-score team.")
      value <- function(field) {
        rows <- Filter(function(x)identical(x$name,field),team$statistics)
        if(length(rows)!=1L)stop("Missing/ambiguous box-score stat: ",field)
        result <- as.numeric(rows[[1]]$displayValue)
        if(!is.finite(result))stop("Invalid box-score statistic.")
        result
      }
      totals <- c(turnovers=value("turnovers"),interceptions=value("interceptions"),fumbles_lost=value("fumblesLost"))
      if(totals[1]!=sum(totals[-1]))stop("Box-score turnovers do not reconcile.")
      checks[[paste(id,name)]] <- data.frame(game_id=id,season=g$season,team=name,
        official_turnovers=totals[1],official_interceptions=totals[2],official_fumbles_lost=totals[3],
        legacy_all_game_scrimmage=matchup_giveaway_count(p,name),
        corrected_all_game_scrimmage=matchup_giveaway_count(fixed,name),
        legacy_competitive=matchup_giveaway_count(competitive,name),
        corrected_competitive=matchup_giveaway_count(fixed_competitive,name),
        source_url=inventory$url[i],captured_at=inventory$captured_at[i],
        limits="Box totals may include special teams/OT/garbage time excluded from feature rates")
    }
    changed <- is.finite(p$turnover) & p$turnover!=p$turnover_indicator
    changed[is.na(changed)] <- FALSE
    examples[[id]] <- p[changed,c("game_id","id_play","pos_team","def_pos_team","play_type","play_text",
      "EPA","pass","rush","sack","turnover_indicator","turnover")]
  }
  write.csv(do.call(rbind,checks),file.path(output,"box_score_checks.csv"),row.names=FALSE)
  write.csv(do.call(rbind,examples),file.path(output,"flag_disagreement_plays.csv"),row.names=FALSE)
  write.csv(before,file.path(output,"protected_before.csv"),row.names=FALSE)
  saveRDS(list(source_dir=source_dir,created=external_stamp(),scope="Four convenience witnesses, not a representative validation sample"),
    file.path(output,"provenance.rds"))
  file.copy(file.path(project,"cfb_v3/experiments/matchup_turnover_witness.R"),file.path(output,"matchup_turnover_witness.R"))
  if(!identical(before,experiment_file_hashes(config)))stop("Production changed during box-score verification.")
  external_seal(output)
  output
}
