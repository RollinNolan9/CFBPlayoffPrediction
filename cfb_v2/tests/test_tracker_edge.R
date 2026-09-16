library(testthat)
for (file in c("config.R", "store.R", "features.R", "models.R", "workflow.R")) source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "external_ratings.R", "tracker_edge.R")) source(file.path("cfb_v3/experiments", file))

edge_fixture <- function() {
  set.seed(4901)
  x <- data.frame(game_id = as.character(1:900), season = rep(2020:2025, each = 150),
    week = rep(1:15, 60), home = "A", away = "B", neutral_site = FALSE, is_cfp = FALSE,
    postseason_type = "regular", market_margin = rnorm(900)*14,
    sagarin_edge = rnorm(900)*5, fpi_edge = rnorm(900)*5, v3_edge = rnorm(900)*7,
    source_agrees = TRUE, source_difference = 0, weight = rep(.82^(5:0), each = 150))
  x$weight[x$season == 2020] <- x$weight[x$season == 2020]/2
  x$external_edge <- (x$sagarin_edge+x$fpi_edge)/2
  x$market_residual <- .2*x$sagarin_edge+.1*x$fpi_edge+rnorm(900)*12
  x$market_residual[seq(1,900,20)] <- 0
  x$margin <- x$market_margin+x$market_residual
  x$v3_margin <- x$market_margin+x$v3_edge
  x$v3_edge[x$season < 2022] <- NA_real_
  for (name in c("opening", "midweek", "updated")) x[[paste0("tracker_",name,"_margin")]] <- x$market_margin
  x
}

test_that("late-line learners use only explicit residual predictors", {
  x <- edge_fixture(); d <- x[x$season < 2022, ]; test <- x[x$season == 2022, ]
  a <- edge_fit(d,"ridge")
  b <- fit_weighted_ridge(d,"market_residual",edge_features(), d$weight/max(d$weight),8)
  expect_equal(edge_predict(a,test), predict(b,test))
  expect_error(edge_fit(d,"ridge","margin"),"Prohibited")
  expect_error(edge_predict(a,d),"training seasons")
  test$market_margin[1] <- NA
  expect_error(edge_predict(a,test),"Finite")
})

test_that("cover GLM excludes pushes and predicts probabilities without fake margins", {
  x <- edge_fixture(); d <- x[x$season < 2022, ]; test <- x[x$season == 2022, ]
  fit <- edge_fit(d,"cover")
  expect_equal(fit$training_rows,sum(d$market_residual != 0))
  p <- edge_predict(fit,test)
  expect_true(all(is.finite(p) & p > 0 & p < 1))
  d$market_residual <- 1
  expect_error(edge_fit(d,"cover"),"both outcomes")
})

test_that("final-season outcomes cannot change any held-out forecast or coefficient", {
  x <- edge_fixture(); a <- edge_walk(x)
  x$market_residual[x$season == 2025] <- -999
  x$margin[x$season == 2025] <- 999
  b <- edge_walk(x)
  expect_equal(a$predictions$signal,b$predictions$signal)
  expect_equal(a$coefficients,b$coefficients)
  expect_true(all(a$coefficients$train_through < a$coefficients$test_season))
  expect_true(all(is.na(subset(a$predictions,model=="cover_glm")$expected_margin)))
})

test_that("v3 stack only trains on previous OOF seasons and matched rows", {
  x <- edge_fixture(); x$v3_edge[x$season < 2022] <- 999
  a <- edge_walk(x)
  expect_equal(a$models$stack_external_2023$training_rows,150L)
  expect_equal(a$models$stack_plus_v3_2023$training_rows,150L)
  for (year in 2023:2025) {
    expect_equal(a$models[[paste0("stack_external_",year)]]$training_rows,
      a$models[[paste0("stack_plus_v3_",year)]]$training_rows)
  }
})

test_that("stress keeps selections fixed and never improves a result", {
  x <- data.frame(signal=c(1,-1,1,0,1), eligible=c(TRUE,TRUE,TRUE,TRUE,FALSE), market_residual=c(0,-.5,-1,7,8))
  a <- edge_grade(x); b <- edge_grade(x,.5); c <- edge_grade(x,1)
  expect_equal(c(a$win[2],a$push[1],a$loss[3]),rep(TRUE,3))
  expect_equal(a$selected,b$selected)
  expect_equal(b$selected,c$selected)
  expect_true(all(b$profit_minus110 <= a$profit_minus110))
  expect_true(all(c$profit_minus110 <= b$profit_minus110))
  expect_true(b$push[2]); expect_true(b$loss[1])
  expect_error(edge_grade(x,-.5),"Invalid")
})

test_that("all twelve policies retain their cohorts and explicit abstentions", {
  x <- edge_fixture(); a <- edge_walk(x); p <- edge_policies(x,a$predictions)
  expect_equal(length(unique(p$policy)),12L)
  expect_false(anyDuplicated(p[c("game_id","policy")]) > 0)
  expect_equal(nrow(subset(p,policy=="v3_all")),600L)
  expect_equal(nrow(subset(p,policy=="stack_plus_v3_all")),450L)
  expect_true(all(subset(p,policy=="cover_glm_p55" & selected)$home_cover_probability >= .55 |
    subset(p,policy=="cover_glm_p55" & selected)$home_cover_probability <= .45))
  d <- subset(p,policy=="unanimous_edge2" & selected)
  expect_true(all(pmin(abs(d$v3_edge),abs(d$sagarin_edge),abs(d$fpi_edge)) >= 2))
  s <- edge_metrics(p)
  expect_true(all(s$wins+s$losses+s$pushes+s$passes==s$games))
  u <- edge_uncertainty(p)
  expect_equal(nrow(u$uncertainty),12L)
})

test_that("metrics report probability accuracy and zero selections honestly", {
  x <- data.frame(signal=c(.05,-.05,0),eligible=c(FALSE,FALSE,FALSE),market_residual=c(3,-3,0),
    expected_margin=NA_real_, margin=c(3,-3,0),home_cover_probability=c(.55,.45,.5))
  s <- edge_summary(edge_grade(x))
  expect_equal(s$picks,0L); expect_true(is.na(s$ats)); expect_true(is.na(s$roi_minus110))
  expect_true(is.na(s$mae)); expect_equal(s$brier,.45^2); expect_equal(s$log_loss,-log(.55))
  stressed <- edge_summary(edge_grade(x,.5))
  expect_true(is.na(stressed$brier)); expect_true(is.na(stressed$log_loss))
})

test_that("PBP provenance tracing preserves disagreements instead of fixing them", {
  root <- tempfile(); path <- file.path(root,"cfb_v2/cache/pbp")
  dir.create(path,recursive=TRUE); on.exit(unlink(root,recursive=TRUE))
  p <- data.frame(game_id=c("1","2","2"), home=c("A","B","B"), away=c("B","A","A"), spread=c(-3,4,5))
  saveRDS(p,file.path(path,"pbp_2022_compact.rds"))
  d <- data.frame(game_id=c("1","2","3"), season=c(2022,2022,2023), home="A",away="B",closing_home_spread=c(-3,-4,-2))
  x <- edge_pbp_trace(root,d)
  expect_equal(x$trace_status,c("matches_cached_pbp_quote","reversed_source_sides","cache_unavailable"))
  expect_equal(x$distinct_quotes[2],2L)
  expect_equal(x$closing_home_spread,d$closing_home_spread)
})

test_that("quote counterfactuals exclude learned market-aware models", {
  x <- edge_fixture(); q <- edge_quote_audit(x,NULL)
  expect_equal(sort(unique(q$raw_quote_counterfactual$model)),c("external","sagarin","v3"))
  expect_true(all(q$market_summary$within_half==150L))
  expect_equal(length(unique(q$raw_quote_counterfactual$games)),1L)
  expect_equal(nrow(q$raw_quote_counterfactual),12L)
})
