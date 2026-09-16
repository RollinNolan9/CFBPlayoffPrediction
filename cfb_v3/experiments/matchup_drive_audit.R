matchup_drive_ledger <- function(pbp) {
  fields <- c("game_id","drive_id","pos_team","drive_result")
  assert_columns(pbp,fields,"identity-rich drive source")
  x <- unique(pbp[fields]); x$team <- canonical_team(x$pos_team)
  key <- paste(x$game_id,x$drive_id,sep="\r")
  conflict <- duplicated(key) | duplicated(key,fromLast=TRUE)
  x$status <- ifelse(conflict,"conflicting_drive_quarantined","qualified")
  missing <- is.na(x$game_id) | is.na(x$drive_id) | is.na(x$team) | is.na(x$drive_result)
  x$status[missing] <- "missing_drive_identity"
  x$plain_giveaway <- x$status=="qualified" & x$drive_result %in% c("FUMBLE","INT")
  x$explicit_return_giveaway <- x$status=="qualified" & x$drive_result %in% c("FUMBLE RETURN TD","INT TD")
  x$ambiguous_fumble_td <- x$drive_result=="FUMBLE TD"
  x
}

run_matchup_drive_audit <- function(project,box_dir) {
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  external_verify(box_dir)
  boxes <- read.csv(file.path(box_dir,"team_game_reconciliation.csv"),stringsAsFactors=FALSE)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_drive_audit"),"")
  paths <- c(`2022`="matchup_source_probe/20260912T014320.238Z/upstream_2022.rds",
    `2023`="matchup_source_probe/20260912T014121.700Z/upstream_2023.rds",
    `2024`="matchup_source_probe/20260912T014320.238Z/upstream_2024.rds",
    `2025`="matchup_source_probe/20260912T014320.238Z/upstream_2025.rds")
  rows <- ledgers <- inventory <- list()
  for(year in names(paths)) {
    path <- file.path(project,"cfb_v3/output/experiments",paths[year])
    external_verify(dirname(path))
    message("Checking drive-level turnover evidence: ",year)
    p <- readRDS(path); x <- boxes[boxes$season==as.integer(year), ]
    p <- p[as.character(p$game_id) %in% as.character(x$game_id), ]
    ledger <- matchup_drive_ledger(p)
    count <- function(team,id,field)sum(ledger$team==team & as.character(ledger$game_id)==as.character(id) & ledger[[field]],na.rm=TRUE)
    x$plain_turnover_drives <- mapply(count,x$team,x$game_id,MoreArgs=list(field="plain_giveaway"))
    x$explicit_return_drives <- mapply(count,x$team,x$game_id,MoreArgs=list(field="explicit_return_giveaway"))
    x$drive_evidence_count <- x$plain_turnover_drives+x$explicit_return_drives
    x$ambiguous_fumble_td_drives <- mapply(count,x$team,x$game_id,MoreArgs=list(field="ambiguous_fumble_td"))
    rows[[year]] <- x; ledgers[[year]] <- ledger
    inventory[[year]] <- data.frame(file=path,sha256=digest::digest(file=path,algo="sha256"))
  }
  result <- do.call(rbind,rows)
  summary <- do.call(rbind,lapply(split(result,result$season),function(x)data.frame(season=x$season[1],
    team_games=nrow(x),reported=sum(x$turnovers),drive_evidence=sum(x$drive_evidence_count),
    exactly_equal=sum(x$turnovers==x$drive_evidence_count),below=sum(x$drive_evidence_count<x$turnovers),
    above=sum(x$drive_evidence_count>x$turnovers),ambiguous_fumble_td=sum(x$ambiguous_fumble_td_drives))))
  write.csv(result,file.path(output,"team_game_checks.csv"),row.names=FALSE)
  write.csv(summary,file.path(output,"season_checks.csv"),row.names=FALSE)
  write.csv(do.call(rbind,ledgers),file.path(output,"drive_ledger.csv"),row.names=FALSE)
  write.csv(do.call(rbind,inventory),file.path(output,"source_inventory.csv"),row.names=FALSE)
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  saveRDS(list(created=external_stamp(),box_dir=box_dir,
    scope="Source-repair feasibility only. No drive event assigned to a model play, EPA replaced, or prediction generated. Ambiguous labels and conflicts remain unresolved."),file.path(output,"provenance.rds"))
  file.copy(file.path(project,"cfb_v3/experiments/matchup_drive_audit.R"),file.path(output,"matchup_drive_audit.R"))
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during drive audit.")
  external_seal(output)
  output
}
