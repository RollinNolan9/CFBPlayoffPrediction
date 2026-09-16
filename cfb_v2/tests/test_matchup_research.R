library(testthat)
for (file in c("config.R","store.R","features.R","historical_data.R","models.R","workflow.R"))
  source(file.path("cfb_v2",file))
for (file in c("controlled_ats.R","external_ratings.R","tracker_edge.R","matchup_sources.R",
  "matchup_features.R","matchup_research.R","matchup_source_probe.R","matchup_diagnostics.R",
  "matchup_placebo.R","matchup_play_contract.R","matchup_foundation_audit.R",
  "matchup_turnover_witness.R","matchup_box_audit.R","matchup_drive_audit.R")) source(file.path("cfb_v3/experiments",file))

matchup_test_history <- function() {
  h <- expand.grid(game_id=as.character(1:8),team=c("A","B"),stringsAsFactors=FALSE)
  h$opponent <- ifelse(h$team=="A","B","A"); h$season <- 2023
  h$kickoff <- as.numeric(as.POSIXct("2023-09-02",tz="UTC"))+7*86400*(as.numeric(h$game_id)-1)
  h$opponent_power <- ifelse(h$team=="A",3,-2)
  for (side in c("off","def")) {
    h[[paste0(side,"_plays")]] <- 60
    h[[paste0(side,"_pass_n")]] <- ifelse(h$team=="A",35,25)
    h[[paste0(side,"_rush_n")]] <- 60-h[[paste0(side,"_pass_n")]]
    h[[paste0(side,"_pass_sum")]] <- as.numeric(h$game_id)*ifelse(h$team=="A",2,-1)
    h[[paste0(side,"_rush_sum")]] <- ifelse(h$team=="A",1,3)
    h[[paste0(side,"_sack_n")]] <- ifelse(h$team=="A",3,1)
    h[[paste0(side,"_high_n")]] <- ifelse(h$team=="A",7,3)
    h[[paste0(side,"_negative_n")]] <- ifelse(h$team=="A",2,5)
  }
  h
}

matchup_test_target <- function() data.frame(game_id="target",season=2023,week=5,home="A",away="B",
  kickoff="2023-09-30T19:00:00Z",feature_week_start="2023-09-25",is_cfp=FALSE)

matchup_test_lines <- function() {
  data.frame(game_id=c("1","1","1","2","2","3","3"),
    provider=c("DraftKings","Bovada","Caesars","DraftKings","Bovada","Caesars","William Hill (New Jersey)"),
    book_family=c("DraftKings","Bovada","Caesars","DraftKings","Bovada","Caesars","Caesars"),
    spread=c(-17,-24,-23,7,7.5,-3,-3),identity_status="matched",duplicate_status="unique",formatted_verified=TRUE)
}

test_that("source spreads are side-aware and unrelated providers do not corroborate stale quotes", {
  expect_true(matchup_formatted_spread("North Carolina -2.5","North Carolina","South Carolina",-2.5))
  expect_true(matchup_formatted_spread("South Carolina +2.5","North Carolina","South Carolina",-2.5))
  expect_false(matchup_formatted_spread("North Carolina -2.5","North Carolina","South Carolina",2.5))
  expect_true(matchup_formatted_spread("PK","A","B",0))
  expect_false(matchup_formatted_spread(NA_character_,"A","B",0))
  x <- matchup_test_lines(); q <- matchup_qualify_lines(x)
  expect_equal(q$selected$provider,c("Bovada","DraftKings"))
  expect_equal(q$selected$spread,c(-24,7))
  expect_false(any(q$ledger$chosen[q$ledger$game_id=="3"]))
  x$margin <- c(50,-50,0,8,-8,3,-3)
  expect_equal(matchup_qualify_lines(x)$selected$spread,q$selected$spread)
  expect_equal(matchup_book("Draft Kings")$provider,"DraftKings")
})

test_that("quarantined, unverifiable and same-family source records cannot supply support", {
  x <- matchup_test_lines(); x$duplicate_status[2] <- "conflicting_quarantined"
  x$formatted_verified[5] <- NA
  q <- matchup_qualify_lines(x)
  expect_equal(nrow(q$selected),0)
  expect_false(anyNA(q$ledger$eligible_record))
})

matchup_test_source_directory <- function(payload) {
  directory <- tempfile("matchup_lines_"); dir.create(directory)
  file <- "source.json"; path <- file.path(directory,file)
  jsonlite::write_json(payload,path,auto_unbox=TRUE,null="null",digits=NA)
  write.csv(data.frame(file=file,sha256=digest::digest(file=path,algo="sha256"),
    retrieved_at="2026-09-12T00:00:00Z",evidence="test_fixture"),file.path(directory,"source_inventory.csv"),row.names=FALSE)
  external_seal(directory)
  directory
}

test_that("neutral line reversals flip spreads and moneylines with score validation", {
  g <- data.frame(game_id="1",season=2023,week=1,home="B",away="A",home_score=17,away_score=21,neutral_site=TRUE)
  payload <- list(list(id="1",season=2023,homeTeam="A",awayTeam="B",homeScore=21,awayScore=17,
    lines=list(list(provider="DraftKings",spread=-3.5,spreadOpen=-2.5,formattedSpread="A -3.5",homeMoneyline=-150,awayMoneyline=125))))
  directory <- matchup_test_source_directory(payload)
  x <- matchup_read_lines(directory,g)
  expect_equal(x$identity_status,"neutral_reversal")
  expect_equal(x$spread,3.5); expect_equal(x$spread_open,2.5)
  expect_equal(x$home_moneyline,125); expect_equal(x$away_moneyline,-150)
  g$neutral_site <- FALSE
  x <- matchup_read_lines(directory,g)
  expect_equal(x$identity_status,"side_mismatch"); expect_true(is.na(x$spread))
})

test_that("source missing seasons, bad scores and conflicting normalized providers are quarantined", {
  g <- data.frame(game_id="1",season=2023,week=1,home="A",away="B",home_score=21,away_score=17,neutral_site=FALSE)
  line <- list(provider="DraftKings",spread=-3,formattedSpread="A -3")
  payload <- list(list(id="1",homeTeam="A",awayTeam="B",homeScore=21,awayScore=17,lines=list(line)))
  expect_equal(matchup_read_lines(matchup_test_source_directory(payload),g)$identity_status,"season_mismatch")
  payload[[1]]$season <- 2023; payload[[1]]$homeScore <- 0
  expect_equal(matchup_read_lines(matchup_test_source_directory(payload),g)$identity_status,"score_mismatch")
  payload[[1]]$homeScore <- 21
  different <- line; different$provider <- "Draft Kings"; different$spread <- -4; different$formattedSpread <- "A -4"
  payload[[1]]$lines <- list(line,different)
  x <- matchup_read_lines(matchup_test_source_directory(payload),g)
  expect_true(all(x$duplicate_status=="conflicting_quarantined"))
  payload[[1]]$lines <- list(line,line)
  x <- matchup_read_lines(matchup_test_source_directory(payload),g)
  expect_equal(x$duplicate_status,c("unique","identical_removed"))
})

test_that("matchup inputs exclude current/future games and first-week source stats", {
  h <- matchup_test_history(); g <- matchup_test_target(); before <- matchup_build_features(h,g)
  expect_equal(before$audit$home_sources,4)
  expect_true(before$audit$home_max_source<before$audit$cutoff)
  h$off_pass_sum[h$kickoff>=before$audit$cutoff] <- 99999
  h$opponent_power[h$kickoff>=before$audit$cutoff] <- -99999
  expect_equal(matchup_build_features(h,g),before)
  g$week <- 1
  one <- matchup_build_features(h,g)
  expect_true(all(as.matrix(one$features[-1])==0))
  expect_equal(one$audit$home_sources,0)
})

test_that("league priors do not ingest future seasons and team priors do not carry history", {
  h <- matchup_test_history(); g <- matchup_test_target(); g$season <- 2024
  g$kickoff <- "2024-09-07T18:00:00Z"; g$feature_week_start <- "2024-09-02"; g$week <- 2
  prior <- matchup_league_prior(h,2024,as.numeric(parse_utc_datetime(g$feature_week_start)))
  expect_equal(prior$source,"previous_league_season")
  built <- matchup_build_features(h,g)
  expect_equal(built$audit$home_sources,0)
  expect_true(all(as.matrix(built$features[-1])==0))
  future <- h; future$season <- 2025; future$kickoff <- future$kickoff+2*366*86400
  future$game_id <- paste0("future_",future$game_id)
  future$off_pass_sum <- 999999
  expect_equal(matchup_build_features(rbind(h,future),g),built)
})

test_that("interactions and component differentials reverse cleanly with team orientation", {
  h <- matchup_test_history(); g <- matchup_test_target()
  a <- matchup_build_features(h,g)$features
  g$home <- "B"; g$away <- "A"
  b <- matchup_build_features(h,g)$features
  expect_equal(as.matrix(a[-1]),-as.matrix(b[-1]))
  expect_true(any(abs(as.matrix(a[-1]))>0))
})

test_that("CFP week-one source labels are postseason, not preseason", {
  h <- matchup_test_history(); g <- matchup_test_target(); g$week <- 1; g$is_cfp <- TRUE
  built <- matchup_build_features(h,g)
  expect_equal(built$audit$phase_week,99)
  expect_equal(built$audit$home_sources,4)
  expect_true(any(abs(as.matrix(built$features[-1]))>0))
  g$is_cfp <- FALSE
  expect_true(all(as.matrix(matchup_build_features(h,g)$features[-1])==0))
})

test_that("shrinkage remains finite with missing samples and form shifts need four games", {
  h <- matchup_test_history(); prior <- matchup_league_prior(h,2023,max(h$kickoff))
  z <- matchup_team_rates(h[FALSE, ],prior)
  expect_true(all(z==0))
  early <- matchup_team_snapshot(h,"A",2023,sort(unique(h$kickoff))[4],4,prior)
  expect_equal(early$values[c("form_pass","form_rush","form_sack","form_available")],
    c(form_pass=0,form_rush=0,form_sack=0,form_available=0))
  late <- matchup_team_snapshot(h,"A",2023,sort(unique(h$kickoff))[5],5,prior)
  expect_equal(unname(late$values["form_available"]),1)
})

test_that("classification excludes kneels and specials, counts sacks once and preserves colliding IDs", {
  g <- data.frame(game_id="1",home="A",away="B",home_level="fbs",away_level="fbs",
    postseason_type="regular",is_cfp=FALSE,completed=TRUE,home_score=7,away_score=0,
    season=2023,kickoff="2023-09-02T18:00:00Z",home_power=3,away_power=-2)
  p <- data.frame(game_id="1",id_play=rep(4e17,6),pos_team="A",def_pos_team="B",home="A",away="B",
    pos_team_score=0,def_pos_team_score=0,period=1,play_type=c("Pass Reception","Rush","Sack","QB Kneel","Punt","Rush"),
    play_text="",EPA=c(2,-2,-3,-1,4,1),pass=c(1,0,1,0,0,1),rush=c(0,1,1,1,0,1),sack=c(0,0,1,0,0,0))
  built <- matchup_aggregate_plays(p,g)
  a <- built$team_games[built$team_games$team=="A", ]
  expect_equal(a$off_plays,3)
  expect_equal(a$off_pass_n,2)
  expect_equal(a$off_rush_n,1)
  expect_equal(a$off_sack_n,1)
  expect_equal(a$off_high_n,1)
  expect_equal(a$off_negative_n,2)
  expect_equal(built$audit$id_collisions,5)
  expect_equal(built$audit$ambiguous,1)
})

matchup_test_fit_data <- function() {
  set.seed(828)
  d <- data.frame(game_id=as.character(1:720),season=rep(2020:2025,each=120),
    week=rep(1:12,60),margin=rnorm(720)*14,weight=rep(.82^(5:0),each=120))
  d$weight[d$season==2020] <- d$weight[d$season==2020]/2
  for (feature in unique(unlist(matchup_variants(),use.names=FALSE))) d[[feature]] <- rnorm(720)
  d$market_residual <- d$margin-d$market_margin
  d
}

test_that("model allowlists, train-only recipes and held-out chronology are enforced", {
  d <- matchup_test_fit_data(); train <- d[d$season<2022, ]; test <- d[d$season==2022, ]
  fit <- matchup_fit(train,matchup_variants()$style,128)
  expect_error(matchup_fit(train,c("home","margin"),128),"Prohibited")
  expect_error(matchup_predict(fit,train),"overlaps")
  baseline <- matchup_predict(fit,test)
  test$margin <- 10000; test$market_residual <- -10000
  expect_equal(matchup_predict(fit,test),baseline)
  test$mx_style_efficiency <- NULL
  expect_error(matchup_predict(fit,test),"mx_style_efficiency")
})

test_that("penalties and predictions ignore later outcomes", {
  d <- matchup_test_fit_data(); first <- matchup_walk(d)
  expect_true(all(first$choices$lambda[first$choices$test_season==2022]==128))
  d$margin[d$season>=2024] <- 99999; d$market_residual[d$season>=2024] <- 99999
  later <- matchup_walk(d)
  keep <- first$predictions$season<=2024
  expect_equal(first$predictions$expected_margin[keep],later$predictions$expected_margin[keep])
  expect_equal(first$choices[first$choices$test_season<=2024, ],later$choices[later$choices$test_season<=2024, ])
})

test_that("only identity-rich exact duplicates can be removed from matched upstream data", {
  full <- data.frame(game_id="1",id_play=4e17,game_play_number=c(1,2,1),clock_minutes=c(15,14,15),
    clock_seconds=0,down=1,distance=10,play_text="Pass complete",EPA=1,home="A",away="B",pos_team="A",def_pos_team="B")
  cached <- compact_cfb_pbp(full)
  expect_equal(matchup_verified_duplicate_rows(full,cached),c(FALSE,FALSE,TRUE))
  full$game_play_number[3] <- 3; full$clock_minutes[3] <- 13
  expect_equal(matchup_verified_duplicate_rows(full,cached),c(FALSE,FALSE,FALSE))
  full$EPA[1] <- full$EPA[1]+1e-10
  expect_error(matchup_verified_duplicate_rows(full,cached),"not identical")
  full$game_play_number <- NULL
  expect_error(matchup_verified_duplicate_rows(full,cached),"game_play_number")
})

test_that("negative controls preserve targets and joint vectors within chronological blocks", {
  d <- matchup_test_fit_data(); d$feature_week_start <- paste(d$season,d$week); d$is_cfp <- FALSE
  a <- matchup_shuffle(d,17); fields <- grep("^mx_",names(d),value=TRUE)
  expect_identical(a,matchup_shuffle(d,17))
  expect_identical(a[setdiff(names(d),fields)],d[setdiff(names(d),fields)])
  for (i in split(seq_len(nrow(d)),d$feature_week_start)) {
    expect_equal(unname(sort(apply(a[i,fields],1,paste,collapse=";"))),unname(sort(apply(d[i,fields],1,paste,collapse=";"))))
  }
  expect_false(identical(a[fields],d[fields]))
})

test_that("chronological learners recover an injected persistent matchup signal", {
  d <- matchup_test_fit_data(); set.seed(54)
  d$market_residual <- 12*d$mx_style_preference+rnorm(nrow(d),sd=.3)
  d$margin <- d$market_margin+d$market_residual
  f <- matchup_walk(d)$predictions
  base <- f[f$model=="external_market", ]; style <- f[f$model=="style", ]
  expect_gt(mean(abs(base$expected_margin-base$margin))-mean(abs(style$expected_margin-style$margin)),4)
})

test_that("fixed-side quote stress preserves picks and cannot improve payout", {
  x <- data.frame(signal=c(3,-3,0),market_residual=c(1,-.5,0),eligible=TRUE)
  base <- edge_grade(x); stress <- edge_grade(x,1)
  expect_identical(base$selected,stress$selected)
  expect_identical(base$side,stress$side)
  expect_true(all(stress$profit_minus110<=base$profit_minus110))
})

test_that("foundation parity is keyed, checks missingness and rejects lost or duplicated games", {
  a <- data.frame(game_id=c("a","b","c"),value=c(1,NA,3))
  expect_equal(foundation_compare(a,a[3:1, ],"game_id","value")$changed,0)
  b <- a; b$value[1] <- 2; b$value[2] <- 0
  x <- foundation_compare(a,b,"game_id","value")
  expect_equal(x$changed,1); expect_equal(x$missing_mismatch,1)
  expect_error(foundation_compare(a,b[-1, ],"game_id","value"),"missing reference")
  expect_error(foundation_compare(a,rbind(b,b[1, ]),"game_id","value"),"duplicate")
  expect_error(foundation_compare(a,b["game_id"],"game_id","value"),"value")
})

test_that("ordinary giveaways are not masked by zero composite indicators", {
  p <- data.frame(game_id="1",id_play=1:6,pos_team="A",def_pos_team="B",home="A",away="B",
    pos_team_score=0,def_pos_team_score=0,period=1,
    play_type=c("Interception Return","Fumble Recovery (Opponent)","Pass Incompletion",
      "Fumble Recovery (Own)","Rush","Punt"),play_text="",
    EPA=c(-3,-4,-2,-1,1,-.5),pass=c(1,0,1,0,0,0),rush=c(0,1,0,1,1,0),sack=0,
    turnover_indicator=c(0,0,1,0,0,1),turnover=c(1,1,1,0,0,1),stuffed_run=0,success=0)
  g <- data.frame(game_id="1",season=2023,week=1,model_week=1,kickoff="2023-09-02T18:00:00Z",
    home="A",away="B",home_score=7,away_score=0,neutral_site=FALSE,
    source_season_type="regular",postseason_type="regular",home_level="fbs",away_level="fbs",
    completed=TRUE,is_cfp=FALSE)
  before <- p; fixed <- matchup_prefer_turnover_flag(p)
  expect_identical(p,before)
  expect_identical(fixed[setdiff(names(p),"turnover_indicator")],p[setdiff(names(p),"turnover_indicator")])
  old <- build_team_game_efficiencies_v2(p,g,.02)
  new <- build_team_game_efficiencies_v2(fixed,g,.02)
  expect_equal(old$turnover_lost_rate[old$team=="A"],0)
  expect_equal(new$turnover_lost_rate[new$team=="A"],2/5)
  expect_equal(new$havoc_allowed[new$team=="A"],3/5)
  expect_equal(matchup_play_contract(fixed,g)$counts$included_giveaways,2)
  p$turnover[1] <- NA; p$turnover_indicator[1] <- 1
  expect_equal(matchup_prefer_turnover_flag(p)$turnover_indicator[1],1)
  p$turnover[1] <- 2
  expect_error(matchup_prefer_turnover_flag(p),"Unexpected")
})

matchup_test_box_directory <- function(payload,season=2023) {
  directory <- tempfile("matchup_boxes_"); dir.create(directory)
  path <- file.path(directory,"source.json")
  jsonlite::write_json(payload,path,auto_unbox=TRUE,null="null",digits=NA)
  write.csv(data.frame(file="source.json",season=season,
    sha256=digest::digest(file=path,algo="sha256")),file.path(directory,"source_inventory.csv"),row.names=FALSE)
  external_seal(directory)
  directory
}

matchup_test_box <- function()list(id="1",teams=list(list(team="A",homeAway="home",points=21,
  stats=list(list(category="turnovers",stat="2"),list(category="interceptions",stat="1"),
    list(category="fumblesLost",stat="1")))))

test_that("box-score audit validates the real team field and reconciles reported totals", {
  g <- data.frame(game_id="1",season=2023,home="A",away="B",home_score=21,away_score=17,neutral_site=FALSE)
  payload <- matchup_test_box()
  x <- matchup_read_boxscores(matchup_test_box_directory(list(payload)),g)
  expect_equal(x$status,"matched"); expect_equal(x$team,"A"); expect_equal(x$turnovers,2)
  payload$teams[[1]]$stats[[3]]$stat <- "0"
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(payload)),g)$status,"unreconciled_reported_totals")
  payload$teams[[1]]$stats <- payload$teams[[1]]$stats[-3]
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(payload)),g)$status,"missing_turnover_stats")
  payload$teams[[1]]$points <- 20
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(payload)),g)$status,"score_mismatch")
})

test_that("box-score identity and duplicate conflicts cannot silently enter audit counts", {
  g <- data.frame(game_id="1",season=2023,home="A",away="B",home_score=21,away_score=17,neutral_site=FALSE)
  a <- matchup_test_box(); b <- a
  x <- matchup_read_boxscores(matchup_test_box_directory(list(a,b)),g)
  expect_equal(x$duplicate_status,c("unique","identical_removed"))
  b$teams[[1]]$stats[[1]]$stat <- "1"; b$teams[[1]]$stats[[3]]$stat <- "0"
  x <- matchup_read_boxscores(matchup_test_box_directory(list(a,b)),g)
  expect_true(all(x$duplicate_status=="conflicting_quarantined"))
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(a),2022),g)$status,"season_mismatch")
  a$teams[[1]]$homeAway <- "away"
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(a)),g)$status,"side_mismatch")
  g$neutral_site <- TRUE
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(a)),g)$status,"matched")
  a$teams[[1]]$team <- "C"
  expect_equal(matchup_read_boxscores(matchup_test_box_directory(list(a)),g)$status,"team_mismatch")
})

test_that("the flag-only candidate does not pretend to repair contradictory source play types", {
  p <- data.frame(pos_team="Michigan",play_type="Fumble Recovery (Opponent)",
    play_text="J.J. McCarthy fumbled, recovered by J.J. Mccarthy",EPA=-2.1089806,
    pass=0,rush=1,sack=0,turnover=1,turnover_indicator=0)
  expect_equal(matchup_giveaway_count(p,"Michigan"),0)
  expect_equal(matchup_giveaway_count(matchup_prefer_turnover_flag(p),"Michigan"),1)
  p$play_type <- "Pass Reception"; p$pass <- 1; p$rush <- 0
  p$pos_team <- "LSU"; p$play_text <- "Chris Hilton Jr. fumbled, recovered by CLEM"
  expect_equal(matchup_giveaway_count(matchup_prefer_turnover_flag(p),"LSU"),0)
  p$play_type <- "Fumble Recovery (Opponent)"; p$pass <- 0
  fixed <- matchup_prefer_turnover_flag(p)
  expect_equal(matchup_giveaway_count(fixed,"LSU"),0)
  expect_equal(matchup_giveaway_count(fixed,"LSU",include_all_play_types=TRUE),1)
  fixed$EPA <- NA_real_
  expect_equal(matchup_giveaway_count(fixed,"LSU",include_all_play_types=TRUE),0)
  expect_equal(matchup_giveaway_count(fixed,"LSU",include_all_play_types=TRUE,require_epa=FALSE),1)
})

test_that("drive evidence counts outcomes once and quarantines conflicting or ambiguous labels", {
  p <- data.frame(game_id="1",drive_id=c("a","a","b","c","d","d","e"),
    pos_team="A",offense_play=c("B","A","B","A","A","A","A"),
    drive_result=c("FUMBLE","FUMBLE","PUNT","FUMBLE TD","INT","TD",NA))
  d <- matchup_drive_ledger(p)
  expect_equal(sum(d$plain_giveaway),1)
  expect_equal(sum(d$explicit_return_giveaway),0)
  expect_equal(d$status[d$drive_id=="d"],rep("conflicting_drive_quarantined",2))
  expect_equal(d$status[d$drive_id=="e"],"missing_drive_identity")
  expect_false(d$plain_giveaway[d$drive_id=="b"])
  expect_true(d$ambiguous_fumble_td[d$drive_id=="c"])
  expect_false(d$plain_giveaway[d$drive_id=="c"])
})
