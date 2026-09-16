library(testthat)
for(file in c("config.R","store.R","features.R","historical_data.R","models.R","workflow.R"))
  source(file.path("cfb_v2",file))
for(file in c("matchup_foundation_audit.R","play_cleanup_pipeline.R"))
  source(file.path("cfb_v3/experiments",file))

cleanup_pipeline_fixture <- function() {
  data.frame(game_id=c("1","1","1"),id_play=c(9,9,10),home="A",away="B",
    pos_team="A",def_pos_team="B",pos_team_score=0,def_pos_team_score=0,
    period=1,play_type="Rush",play_text="Runner rushes for 3 yards",garbage_time=FALSE,
    rush=1,pass=0,sack=0,stuffed_run=0,EPA=c(.1,.2,.3),
    clean_giveaway=c(0,1,0),clean_scrimmage=TRUE,clean_status="resolved",stringsAsFactors=FALSE)
}

test_that("rate adapter preserves colliding IDs and its denominator", {
  x <- cleanup_pipeline_fixture(); before <- x
  cells <- play_cleanup_rate_cells(x)
  expect_equal(cells$model_plays,c(3L,3L))
  expect_equal(cells$havoc_rate,c(1/3,1/3))
  expect_equal(cells$turnover_lost_rate,c(1/3,1/3))
  expect_identical(x,before)
})

test_that("unknown giveaways invalidate rates rather than becoming zero", {
  x <- cleanup_pipeline_fixture(); x$clean_giveaway[2] <- NA_real_
  cells <- play_cleanup_rate_cells(x)
  expect_true(all(is.na(cells$turnover_lost_rate)))
  expect_true(all(is.na(cells$havoc_rate)))
  expect_equal(cells$model_plays,c(3L,3L))
  x$sack[2] <- 1
  cells <- play_cleanup_rate_cells(x)
  expect_true(all(is.na(cells$turnover_lost_rate)))
  expect_equal(cells$havoc_rate,c(1/3,1/3))
})

test_that("eligibility disagreement is quarantined without assigning new EPA plays", {
  x <- cleanup_pipeline_fixture(); x$rush[2] <- 0; x$play_type[2] <- "Fumble Recovery (Opponent)"
  cells <- play_cleanup_rate_cells(x)
  expect_equal(cells$model_plays,c(2L,2L))
  expect_equal(cells$eligibility_conflicts,c(1L,1L))
  expect_true(all(is.na(cells$turnover_lost_rate)))
  expect_true(all(is.na(cells$havoc_rate)))
})

test_that("intentional garbage and kneel filtering does not poison rate cells", {
  x <- cleanup_pipeline_fixture(); x$clean_giveaway[2:3] <- NA_real_
  x$garbage_time[2] <- TRUE; x$play_text[3] <- "Quarterback kneels"
  cells <- play_cleanup_rate_cells(x)
  expect_equal(cells$model_plays,c(1L,1L))
  expect_equal(cells$turnover_lost_rate,c(0,0))
})

test_that("applying rates changes no EPA or opportunity columns", {
  x <- cleanup_pipeline_fixture(); cells <- play_cleanup_rate_cells(x)
  tg <- data.frame(game_id="1",team=c("A","B"),scrimmage_plays=c(3,NA),
    offense_epa=c(.2,NA),defense_epa=c(NA,-.2),turnover_lost_rate=c(0,NA),
    havoc_allowed=c(0,NA),havoc_generated=c(NA,0),turnover_rate_regressed=c(.02,NA))
  y <- play_cleanup_apply_rates(tg,cells,.02)
  expect_identical(y[c("offense_epa","defense_epa","scrimmage_plays")],
    tg[c("offense_epa","defense_epa","scrimmage_plays")])
  expect_equal(y$turnover_lost_rate[1],1/3)
  expect_equal(y$havoc_generated[2],1/3)
  expect_true(is.na(y$turnover_lost_rate[2]))
  tg$scrimmage_plays[1] <- 2
  expect_error(play_cleanup_apply_rates(tg,cells,.02),"denominator")
  expect_error(play_cleanup_apply_rates(tg,rbind(cells,cells),.02),"duplicate",ignore.case=TRUE)
})

test_that("cleanup parity rejects changed populations and missingness", {
  x <- data.frame(game_id=c("1","2"),value=c(1,NA_real_))
  expect_error(play_cleanup_require_parity(x,x[1, ],"game_id","value","fixture"),"row count")
  y <- x; y$value[2] <- 0
  expect_error(play_cleanup_require_parity(x,y,"game_id","value","fixture"),"parity")
  expect_equal(play_cleanup_require_parity(x,x[2:1, ],"game_id","value","fixture")$changed,0)
})

test_that("no eligible plays preserve an empty typed rate schema", {
  x <- cleanup_pipeline_fixture(); x$garbage_time <- TRUE
  cells <- play_cleanup_rate_cells(x)
  expect_equal(nrow(cells),0L)
  expect_true(all(c("game_id","side","turnover_lost_rate","havoc_rate") %in% names(cells)))
})

test_that("data-table source classes do not change adapter column selection", {
  skip_if_not_installed("data.table")
  x <- cleanup_pipeline_fixture(); dt <- data.table::as.data.table(x)
  expect_equal(play_cleanup_rate_cells(dt),play_cleanup_rate_cells(x))
  expect_equal(as.data.frame(dt),x)
})

test_that("source parity preserves loader metadata without relying on lossy data-frame subsetting", {
  x <- data.frame(game_id="1",EPA=.2)
  attr(x,"cfbfastR_timestamp") <- as.POSIXct("2020-01-01",tz="UTC")
  attr(x,"sportsdataverse_type") <- "classic"
  y <- x; y$clean_giveaway <- 0L
  expect_silent(play_cleanup_assert_source(x,y))
  y$EPA <- .3
  expect_error(play_cleanup_assert_source(x,y),"raw source")
  y <- x; attr(y,"sportsdataverse_type") <- "other"
  expect_error(play_cleanup_assert_source(x,y),"metadata")
})

test_that("known own recovery with unknown scrimmage eligibility cannot disappear", {
  x <- cleanup_pipeline_fixture()
  x$rush[2] <- 0; x$play_type[2] <- "Fumble Recovery (Own)"
  x$clean_giveaway[2] <- 0; x$clean_scrimmage[2] <- NA; x$clean_status[2] <- "unresolved"
  cells <- play_cleanup_rate_cells(x)
  expect_equal(cells$model_plays,c(2L,2L))
  expect_equal(cells$eligibility_conflicts,c(1L,1L))
  expect_true(all(is.na(cells$turnover_lost_rate)))
  expect_true(all(is.na(cells$havoc_rate)))
})

test_that("unresolved status cannot be overridden by a finite giveaway flag", {
  x <- cleanup_pipeline_fixture(); x$clean_status[1] <- "unresolved"
  cells <- play_cleanup_rate_cells(x)
  expect_true(all(is.na(cells$turnover_lost_rate)))
  expect_true(all(is.na(cells$havoc_rate)))
  expect_equal(cells$turnover_unknown_plays,c(1L,1L))
})
