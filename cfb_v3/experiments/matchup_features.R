matchup_count_fields <- function() {
  c("plays", "pass_n", "pass_sum", "rush_n", "rush_sum", "sack_n", "high_n", "negative_n")
}

matchup_aggregate_plays <- function(pbp, games) {
  assert_unique_keys(games, "game_id", "matchup source schedule")
  games$game_id <- as.character(games$game_id)
  valid <- raw_history_eligible(games) & games$completed &
    is.finite(games$home_score) & is.finite(games$away_score)
  valid[is.na(valid)] <- FALSE
  g <- games[valid, ]
  p <- pbp[as.character(pbp$game_id) %in% g$game_id, ]
  n_raw <- nrow(p)
  if (!n_raw) stop("No eligible source plays.")
  assert_columns(p, c("game_id","id_play","pos_team","def_pos_team","EPA",
    "pass","rush","sack","play_type"), "matchup plays")
  p$game_id <- as.character(p$game_id)
  p$pos_team <- canonical_team(p$pos_team); p$def_pos_team <- canonical_team(p$def_pos_team)
  id <- paste(p$game_id,p$id_play,sep="\r")
  missing_id <- is.na(p$id_play) | !nzchar(as.character(p$id_play))
  # Numeric cached IDs collide across distinct plays. Preserve source rows; the
  # compact schema cannot establish whether fully identical rows are duplicates.
  id_collisions <- sum(duplicated(id) & !missing_id)
  identical_rows <- sum(duplicated(p))
  n_unique <- nrow(p)
  p <- historical_competitive_plays(p)
  n_competitive <- nrow(p)
  flag <- function(x) !is.na(x) & x == 1
  sack <- flag(p$sack) | grepl("sack",p$play_type,ignore.case=TRUE)
  pass <- flag(p$pass) | sack | grepl("pass|intercept",p$play_type,ignore.case=TRUE)
  rush <- flag(p$rush) | grepl("rush|run",p$play_type,ignore.case=TRUE)
  pass[is.na(pass)] <- FALSE; rush[is.na(rush)] <- FALSE; sack[is.na(sack)] <- FALSE
  special <- grepl("punt|kickoff|field goal|extra point",p$play_type,ignore.case=TRUE)
  special[is.na(special)] <- FALSE
  ambiguous <- pass & rush & !sack
  i <- match(p$game_id,g$game_id)
  identity <- (p$pos_team == g$home[i] & p$def_pos_team == g$away[i]) |
    (p$pos_team == g$away[i] & p$def_pos_team == g$home[i])
  identity[is.na(identity)] <- FALSE
  keep <- (pass | rush) & !ambiguous & !special & is.finite(p$EPA) & identity
  q <- p[keep, ]; pass <- pass[keep]; sack <- sack[keep]
  if (!nrow(q)) stop("No classified competitive plays.")
  x <- data.frame(game_id=q$game_id,team=q$pos_team,opponent=q$def_pos_team,
    plays=1,pass_n=as.numeric(pass),pass_sum=ifelse(pass,q$EPA,0),
    rush_n=as.numeric(!pass),rush_sum=ifelse(!pass,q$EPA,0),
    sack_n=as.numeric(sack),high_n=as.numeric(q$EPA >= 2),negative_n=as.numeric(q$EPA <= -2))
  off <- stats::aggregate(x[matchup_count_fields()],x[c("game_id","team","opponent")],sum)
  def <- off; def$team <- off$opponent; def$opponent <- off$team
  names(off)[match(matchup_count_fields(),names(off))] <- paste0("off_",matchup_count_fields())
  names(def)[match(matchup_count_fields(),names(def))] <- paste0("def_",matchup_count_fields())
  out <- merge(off,def,by=c("game_id","team","opponent"),all=TRUE,sort=FALSE)
  counts <- c(paste0("off_",matchup_count_fields()),paste0("def_",matchup_count_fields()))
  for (field in counts) out[[field]][is.na(out[[field]])] <- 0
  i <- match(out$game_id,g$game_id)
  out$season <- g$season[i]; out$kickoff <- as.numeric(parse_utc_datetime(g$kickoff[i]))
  out$opponent_power <- ifelse(out$team == g$home[i],g$away_power[i],g$home_power[i])
  assert_unique_keys(out,c("game_id","team"),"matchup team-game counts")
  if (any(!is.finite(out$kickoff))) stop("Source kickoff missing.")
  if (any(out$off_pass_n+out$off_rush_n != out$off_plays) ||
      any(out$off_sack_n > out$off_pass_n)) stop("Exclusive play classification failed.")
  list(team_games=out,audit=data.frame(raw=n_raw,retained=n_unique,id_collisions=id_collisions,
    missing_ids=sum(missing_id),identical_rows=identical_rows,competitive=n_competitive,
    ambiguous=sum(ambiguous),invalid_team=sum(!identity),classified=nrow(q),team_games=nrow(out)))
}

matchup_league_prior <- function(history, season, cutoff) {
  prior <- history[history$kickoff < cutoff & history$season == season, ]
  source <- "current_season"
  if (!nrow(prior)) {
    years <- history$season[history$season < season & history$kickoff < cutoff]
    prior <- if (length(years)) history[history$season == max(years) & history$kickoff < cutoff, ] else history[FALSE, ]
    source <- if (nrow(prior)) "previous_league_season" else "fixed_neutral"
  }
  total <- function(field) sum(prior[[paste0("off_",field)]])
  rate <- function(num,den,default) if (total(den)>0) total(num)/total(den) else default
  list(pass_epa=rate("pass_sum","pass_n",0),rush_epa=rate("rush_sum","rush_n",0),
    pass_fraction=rate("pass_n","plays",.5),sack=rate("sack_n","pass_n",.06),
    high=rate("high_n","plays",.1),negative=rate("negative_n","plays",.1),
    source=source,max_kickoff=if(nrow(prior))max(prior$kickoff) else NA_real_)
}

matchup_team_rates <- function(history, prior) {
  output <- c()
  for (side in c("off","def")) {
    total <- function(field) sum(history[[paste0(side,"_",field)]])
    shrink <- function(num,den,base,n) (total(num)-base*total(den))/(total(den)+n)
    value <- c(pass_epa=shrink("pass_sum","pass_n",prior$pass_epa,50),
      rush_epa=shrink("rush_sum","rush_n",prior$rush_epa,50),
      pass_fraction=shrink("pass_n","plays",prior$pass_fraction,100),
      sack=shrink("sack_n","pass_n",prior$sack,50),
      high=shrink("high_n","plays",prior$high,100),
      negative=shrink("negative_n","plays",prior$negative,100))
    names(value) <- paste0(side,"_",names(value)); output <- c(output,value)
  }
  output
}

matchup_team_snapshot <- function(history, team, season, cutoff, week, prior) {
  h <- history[history$team == team & history$season == season & history$kickoff < cutoff, ]
  if (week <= 1) h <- h[FALSE, ]
  h <- h[order(h$kickoff,h$game_id), ]
  rates <- matchup_team_rates(h,prior)
  value <- c(rates,source_games=nrow(h),source_opponent_power=if(any(is.finite(h$opponent_power)))
    mean(h$opponent_power[is.finite(h$opponent_power)]) else 0,
    form_pass=0,form_rush=0,form_sack=0,form_available=as.numeric(nrow(h)>=4))
  if (nrow(h)>=4) {
    recent <- matchup_team_rates(tail(h,2),prior)
    earlier <- matchup_team_rates(head(h,-2),prior)
    value[c("form_pass","form_rush","form_sack")] <- (recent-earlier)[c("off_pass_epa","off_rush_epa","off_sack")]
  }
  list(values=value,max_kickoff=if(nrow(h))max(h$kickoff) else NA_real_,
    source_ids=paste(h$game_id,collapse=";"))
}

matchup_combine <- function(home, away, prior) {
  main <- home-away
  names(main) <- paste0("mx_",names(main),"_diff")
  style <- function(a,b) c(preference=a["off_pass_fraction"]*(b["def_pass_epa"]-b["def_rush_epa"]),
    efficiency=(a["off_pass_epa"]-a["off_rush_epa"])*(b["def_pass_epa"]-b["def_rush_epa"]))
  sacks <- function(a,b) -a["off_sack"]*b["def_sack"]*(a["off_pass_fraction"]+prior$pass_fraction)
  impact <- function(a,b) c(high=a["off_high"]*b["def_high"],negative=-a["off_negative"]*b["def_negative"])
  interaction <- c(style(home,away)-style(away,home),sacks(home,away)-sacks(away,home),
    impact(home,away)-impact(away,home))
  names(interaction) <- c("mx_style_preference","mx_style_efficiency","mx_sack_pressure",
    "mx_high_impact","mx_negative_impact")
  c(main,interaction)
}

matchup_build_features <- function(history, games) {
  assert_unique_keys(history,c("game_id","team"),"matchup history")
  assert_unique_keys(games,"game_id","matchup targets")
  assert_columns(games,c("is_cfp","week"),"matchup target phases")
  if (anyNA(games$is_cfp)) stop("Unknown CFP phase.")
  phase_week <- ifelse(games$is_cfp,99,games$week)
  result <- audit <- list(); priors <- new.env(parent=emptyenv())
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]; kickoff <- as.numeric(parse_utc_datetime(g$kickoff))
    week_cutoff <- as.numeric(parse_utc_datetime(g$feature_week_start))
    if (!all(is.finite(c(kickoff,week_cutoff,g$week,g$season)))) stop("Missing target cutoff.")
    cutoff <- min(kickoff,week_cutoff)
    key <- paste(g$season,cutoff)
    if (!exists(key,priors,inherits=FALSE)) assign(key,matchup_league_prior(history,g$season,cutoff),priors)
    prior <- get(key,priors,inherits=FALSE)
    home <- matchup_team_snapshot(history,g$home,g$season,cutoff,phase_week[i],prior)
    away <- matchup_team_snapshot(history,g$away,g$season,cutoff,phase_week[i],prior)
    value <- matchup_combine(home$values,away$values,prior)
    result[[i]] <- cbind(data.frame(game_id=as.character(g$game_id)),as.data.frame(as.list(value)))
    audit[[i]] <- data.frame(game_id=as.character(g$game_id),season=g$season,week=g$week,phase_week=phase_week[i],
      cutoff=cutoff,kickoff=kickoff,home_sources=home$values["source_games"],away_sources=away$values["source_games"],
      home_max_source=home$max_kickoff,away_max_source=away$max_kickoff,
      home_source_ids=home$source_ids,away_source_ids=away$source_ids,
      league_prior_source=prior$source,league_max_source=prior$max_kickoff)
  }
  x <- do.call(rbind,result); a <- do.call(rbind,audit); rownames(x) <- rownames(a) <- NULL
  if (any(!is.finite(as.matrix(x[setdiff(names(x),"game_id")])))) stop("Nonfinite matchup feature.")
  for (field in c("home_max_source","away_max_source","league_max_source"))
    if (any(a[[field]] >= a$cutoff,na.rm=TRUE)) stop("Future matchup source detected.")
  if (any(as.matrix(x[x$game_id %in% as.character(games$game_id[phase_week<=1]),-1,drop=FALSE]) != 0))
    stop("Week 0/1 matchup evidence is not neutral.")
  list(features=x,audit=a)
}

matchup_feature_groups <- function() {
  rates <- c("pass_epa","rush_epa","pass_fraction","sack","high","negative")
  linear <- paste0("mx_",c(paste0("off_",rates),paste0("def_",rates),"source_games","source_opponent_power"),"_diff")
  list(linear=c(linear,"power_rating_diff"),style=c("mx_style_preference","mx_style_efficiency"),
    sacks="mx_sack_pressure",impact=c("mx_high_impact","mx_negative_impact"),
    form=paste0("mx_",c("form_pass","form_rush","form_sack","form_available"),"_diff"))
}
