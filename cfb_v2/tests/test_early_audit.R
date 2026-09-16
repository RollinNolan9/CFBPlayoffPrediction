library(testthat)
for (file in c("config.R", "store.R", "features.R", "coaches.R", "adapters.R", "models.R",
  "coach_migration.R", "historical_data.R", "preseason.R", "bridge.R", "workflow.R")) source(file.path("cfb_v2", file))
for (file in c("controlled_ats.R", "four_way.R", "early_spread_audit.R", "early_calibration.R"))
  source(file.path("cfb_v3", "experiments", file))

calibration_fixture <- function() {
  x <- expand.grid(season = 2022:2024, week = c(1L, 3L, 8L), game = seq_len(110))
  x$game_id <- as.character(seq_len(nrow(x)))
  x$postseason_type <- "regular"
  x$expected_margin <- (x$game-55)/2
  x$actual_margin <- 2+0.8*x$expected_margin
  x$closing_home_spread <- -x$expected_margin+1
  x
}

test_that("audit compares lined margins on exactly the market's rows", {
  x <- data.frame(expected_margin = c(10, 50, -6), actual_margin = c(7, 0, -10),
    closing_home_spread = c(-7, NA, 4))
  s <- audit_margin_summary(x)
  expect_equal(s$games, 3L)
  expect_equal(s$lined, 2L)
  expect_equal(s$lined_model_mae, 3.5)
  expect_equal(s$market_mae, 3)
  expect_equal(s$pushes, 1L)
  expect_equal(s$wins, 1L)
  expect_equal(s$max_absolute_prediction, 50)
})

test_that("early calibration phase always excludes postseason", {
  x <- data.frame(week = c(0, 1, 2, 4, 5, 1, 1, NA),
    postseason_type = c(rep("regular", 5), "cfp", "conference_championship", "regular"))
  expect_equal(early_calibration_phase(x), c("week_0_1", "week_0_1", "weeks_2_4",
    "weeks_2_4", rep("inactive", 4)))
})

test_that("calibration requires 100 past out-of-sample games per phase", {
  x <- calibration_fixture()
  fit <- fit_early_calibration(x, 2022L)
  expect_false(any(fit$choices$applied))
  target <- x[x$season == 2022, ]
  expect_identical(predict_early_calibration(fit, target)$expected_margin, target$expected_margin)
  history <- x[x$season == 2022 & x$week == 1, ][1:99, ]
  expect_false(any(fit_early_calibration(history, 2023L)$choices$applied))
  history <- x[x$season == 2022 & x$week == 1, ][1:100, ]
  expect_true(fit_early_calibration(history, 2023L)$choices$applied[1])
  expect_false(fit_early_calibration(history, 2023L)$choices$applied[2])
})

test_that("current and future outcomes or market lines cannot affect calibration", {
  x <- calibration_fixture()
  before <- fit_early_calibration(x, 2024L)
  changed <- x
  changed$actual_margin[changed$season >= 2024] <- 9999
  changed$expected_margin[changed$season >= 2024] <- -9999
  changed$closing_home_spread <- 9999
  after <- fit_early_calibration(changed, 2024L)
  expect_equal(before, after)
  expect_true(all(before$choices$last_training_season < 2024))
  expect_equal(before$models$week_0_1$recipe$features, "expected_margin")
})

test_that("calibration only changes active early-season rows and reconciles coefficients", {
  x <- calibration_fixture()
  fit <- fit_early_calibration(x, 2024L)
  target <- x[x$season == 2024, ]
  target$postseason_type[1] <- "cfp"
  p <- predict_early_calibration(fit, target)
  inactive <- early_calibration_phase(target) == "inactive"
  expect_identical(p$expected_margin[inactive], target$expected_margin[inactive])
  expect_true(all(p$correction[inactive] == 0))
  expect_true(all(!p$calibration_applied[inactive]))
  active <- early_calibration_phase(target) == "week_0_1"
  choice <- fit$choices[fit$choices$phase == "week_0_1", ]
  expect_equal(p$expected_margin[active], choice$intercept+choice$slope*target$expected_margin[active])
  expect_lt(mean(abs(p$expected_margin[active]-target$actual_margin[active])),
    mean(abs(target$expected_margin[active]-target$actual_margin[active])))
})

test_that("calibration rejects duplicate history or a wrong target year", {
  x <- calibration_fixture()
  expect_error(fit_early_calibration(rbind(x, x[1, ]), 2024L), "duplicate")
  fit <- fit_early_calibration(x, 2024L)
  expect_error(predict_early_calibration(fit, x[x$season == 2023, ]), "season")
  target <- x[x$season == 2024, ]; target$expected_margin[1] <- NA
  expect_error(predict_early_calibration(fit, target), "invalid")
})
