matchup_capture_event_sources <- function(project) {
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_event_sources"),"")
  inventory <- list()
  requests <- list(stat_types=list(endpoint="/plays/stats/types",query=NULL),
    michigan_2023=list(endpoint="/plays/stats",query=list(gameId=401520162)),
    lsu_2025=list(endpoint="/plays/stats",query=list(gameId=401752671)),
    miami_2025=list(endpoint="/plays/stats",query=list(gameId=401754522)),
    missouri_2023=list(endpoint="/plays/stats",query=list(gameId=401520341)))
  for(name in names(requests)) {
    message("Probing structured event statistics: ",name)
    request <- requests[[name]]; url <- paste0("https://api.collegefootballdata.com",request$endpoint)
    file <- paste0(name,".json"); path <- file.path(output,file)
    captured <- external_fetch(url,path,request$query,authenticated=TRUE)
    inventory[[name]] <- data.frame(file=file,url=url,
      query=as.character(jsonlite::toJSON(request$query,auto_unbox=TRUE,null="null")),captured_at=captured,
      sha256=digest::digest(file=path,algo="sha256"))
    write.csv(do.call(rbind,inventory),file.path(output,"source_inventory.csv"),row.names=FALSE)
  }
  writeLines("Four deliberately selected contradictory-play witnesses. These structured stats may share the same provider/parser defects. No model or source rows changed.",
    file.path(output,"SCOPE.txt"))
  file.copy(file.path(project,"cfb_v3/experiments/matchup_event_sources.R"),file.path(output,"matchup_event_sources.R"))
  external_seal(output)
  output
}

matchup_check_event_sources <- function(project,directory) {
  external_verify(directory)
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  games <- read.csv(file.path(config$data_dir,"historical_games.csv"),stringsAsFactors=FALSE)
  inventory <- read.csv(file.path(directory,"source_inventory.csv"),stringsAsFactors=FALSE)
  inventory <- inventory[inventory$file!="stat_types.json", ]
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_event_checks"),"")
  counts <- events <- list()
  for(i in seq_len(nrow(inventory))) {
    query <- jsonlite::fromJSON(inventory$query[i]); id <- as.character(query$gameId)
    g <- games[as.character(games$game_id)==id, ]
    if(nrow(g)!=1)stop("Structured-event game identity is ambiguous.")
    x <- jsonlite::fromJSON(file.path(directory,inventory$file[i]))
    assert_columns(x,c("gameId","season","team","opponent","playId","statType","athleteId"),"event associations")
    valid <- as.character(x$gameId)==id & x$season==g$season &
      ((canonical_team(x$team)==g$home & canonical_team(x$opponent)==g$away) |
       (canonical_team(x$team)==g$away & canonical_team(x$opponent)==g$home))
    if(anyNA(valid) || !all(valid))stop("Structured-event identity mismatch.")
    if(!is.character(x$playId) || anyNA(x$playId))stop("Event play IDs lost string precision.")
    counts[[id]] <- data.frame(game_id=id,season=g$season,home=g$home,away=g$away,
      association_rows=nrow(x),potential_2000_row_cap=nrow(x)>=2000,
      fumble_rows=sum(x$statType=="Fumble"),recovery_rows=sum(x$statType=="Fumble Recovered"),
      interception_thrown_rows=sum(x$statType=="Interception Thrown"),
      limits="Associations are incomplete and do not establish zero events when absent")
    hit <- x$statType %in% c("Fumble","Fumble Recovered","Interception Thrown","Interception")
    events[[id]] <- x[hit,c("gameId","season","team","opponent","playId","athleteId","athleteName","statType","stat")]
  }
  write.csv(do.call(rbind,counts),file.path(output,"association_coverage.csv"),row.names=FALSE)
  write.csv(do.call(rbind,events),file.path(output,"event_witnesses.csv"),row.names=FALSE)
  saveRDS(list(source_dir=directory,created=external_stamp(),scope="Four selected witnesses; not a replacement event ledger"),
    file.path(output,"provenance.rds"))
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  file.copy(file.path(project,"cfb_v3/experiments/matchup_event_sources.R"),file.path(output,"matchup_event_sources.R"))
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during event-source check.")
  external_seal(output)
  output
}
