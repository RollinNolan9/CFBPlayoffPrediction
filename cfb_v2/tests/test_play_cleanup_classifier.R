library(testthat)
source("cfb_v2/features.R")
source("cfb_v3/experiments/play_cleanup_classifier.R")

cleanup_fixture <- function(type = "Rush", text = "John Runner run for 3 yds") {
  n <- max(length(type), length(text))
  data.frame(game_id = rep("g1", n), game_play_number = seq_len(n),
    drive_id = rep("d1", n), pos_team = rep("LSU", n), def_pos_team = rep("Clemson", n),
    play_type = rep(type, length.out = n), play_text = rep(text, length.out = n),
    turnover = rep(0L, n), turnover_indicator = rep(0L, n),
    change_of_pos_team = rep(0L, n), drive_result = rep(NA_character_, n),
    EPA = rep(0.25, n), stringsAsFactors = FALSE)
}

test_that("classification appends only the public fields without changing inputs", {
  p <- cleanup_fixture(c("Rush", "Pass Incompletion"),
    c("John Runner run for 3 yds", "John Passer pass incomplete"))
  p$unrelated <- I(list(c("a", "b"), NULL))
  p$date <- as.Date(c("2025-01-01", "2025-01-02"))
  rownames(p) <- c("source_9", "source_1")
  before <- serialize(p, NULL); x <- play_cleanup_classify(p)
  expect_identical(serialize(p, NULL), before)
  expect_identical(x[names(p)], p)
  expect_identical(tail(names(x), 5), c("clean_giveaway", "clean_scrimmage",
    "clean_status", "clean_reason", "clean_evidence"))
  expect_identical(x$clean_giveaway, c(0L, 0L))
  expect_identical(x$clean_scrimmage, c(TRUE, TRUE))
  expect_identical(x$EPA, p$EPA)
  expect_error(play_cleanup_classify(x), "no classifier output")
})

test_that("empty and missing records remain explicit rather than becoming zero", {
  expect_equal(nrow(play_cleanup_classify(cleanup_fixture()[FALSE, ])), 0)
  p <- data.frame(game_id = "missing")
  x <- play_cleanup_classify(p)
  expect_true(is.na(x$clean_giveaway)); expect_true(is.na(x$clean_scrimmage))
  expect_equal(x$clean_status, "unresolved")
  p <- cleanup_fixture(c("placeholder", "Pass Reception"), c(NA, NA))
  expect_true(all(is.na(play_cleanup_classify(p)$clean_giveaway)))
  expect_error(play_cleanup_classify(list()), "data frame")
})

test_that("explicit interceptions override a false zero composite flag", {
  p <- cleanup_fixture(c("Interception Return", "Pass"),
    c("John Passer pass intercepted Alan Defender return for 4 yds",
      "John Passer pass intercepted by Alan Defender"))
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(1L, 1L))
  expect_true(all(x$clean_scrimmage))
  expect_true(all(x$clean_reason == "interception"))
  p$pos_team[1] <- NA
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway[1]))
})

test_that("interception near misses and source contradictions do not create giveaways", {
  p <- cleanup_fixture(c("Pass Incompletion", "Pass Incompletion", "Interception Return"),
    c("John Passer pass nearly intercepted but incomplete",
      "John Passer pass incomplete, dropped interception",
      "John Passer pass incomplete, not intercepted"))
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(0L, 0L, NA_integer_))
  p <- cleanup_fixture("Pass Incompletion", "John Passer pass intercepted by Alan Defender")
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
  p <- cleanup_fixture("Interception Return", NA_character_)
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
})

test_that("lost fumbles need explicit recovering-team and initial-play evidence", {
  p <- cleanup_fixture(c("Pass Reception", "Fumble Recovery (Opponent)", "Fumble Recovery (Opponent)"),
    c("John Passer pass complete to John Receiver who fumbled, recovered by CLEM",
      "John Runner run for 3 yds John Runner fumbled, recovered by Clemson Alan Defender",
      "John Runner run for 3 yds John Runner fumbled, recovered by Alan Defender"))
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(1L, 1L, NA_integer_))
  expect_true(all(x$clean_scrimmage))
  expect_match(x$clean_evidence[1], "explicit_recovery_team=def")
  p <- cleanup_fixture("Fumble Recovery (Opponent)", "John Runner fumbled, recovered by CLEM")
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
})

test_that("unknown team abbreviations are not inferred from prefixes or surnames", {
  p <- cleanup_fixture("Rush", c(
    "John Runner run for 3 yds and fumbled, recovered by Jones at the Clemson 20",
    "John Runner run for 3 yds and fumbled, recovered by CLE Alan Defender"))
  expect_true(all(is.na(play_cleanup_classify(p)$clean_giveaway)))
  p$def_pos_team_abbreviation <- "CLE"
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(NA_integer_, 1L))
  p$pos_team_abbreviation <- "CLE"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway[2]))
})

test_that("Michigan-style own recoveries use complete identity not a turnover flag", {
  p <- cleanup_fixture("Fumble Recovery (Opponent)",
    "J.J. McCarthy run for 2 yds to the ECU 3 J.J. McCarthy fumbled, recovered by J.J. Mccarthy")
  p$pos_team <- "Michigan"; p$def_pos_team <- "East Carolina"
  p$fumble_player_name <- "J.J. McCarthy"
  p$fumble_recovered_player_name <- "J.J. Mccarthy"
  p$turnover <- 1L; p$change_of_pos_team <- 1L
  p$drive_result <- "DOWNS"
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, 0L); expect_true(x$clean_scrimmage)
  expect_equal(x$clean_reason, "own_recovery")
  p$fumble_player_name <- "McCarthy"; p$fumble_recovered_player_name <- "McCarthy"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
  p$fumble_player_id <- "4433970"; p$fumble_recovered_player_id <- "4433970"
  expect_identical(play_cleanup_classify(p)$clean_giveaway, 0L)
})

test_that("contradictory recovered-team and player evidence stays unresolved", {
  p <- cleanup_fixture("Rush", "John Runner run for 3 yds and fumbled, recovered by CLEM")
  p$fumble_player_name <- "John Runner"; p$fumble_recovered_player_name <- "John Runner"
  expect_equal(play_cleanup_classify(p)$clean_reason, "conflicting_recovery_evidence")
  p$fumble_recovered_player_name <- NA; p$recovery_team <- "LSU"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
  p$recovery_team <- "Unknown Team"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
  p$recovery_team <- "Clemson"
  expect_identical(play_cleanup_classify(p)$clean_giveaway, 1L)
})

test_that("own recovery types and explicit own wording do not count losses", {
  p <- cleanup_fixture(c("Fumble Recovery (Own)", "Rush", "Rush"),
    c("John Runner run for 1 yd and fumbled, recovered by John Teammate",
      "John Runner run for 1 yd and fumbled, recovered by LSU John Teammate",
      "John Runner run for 1 yd and recovered his own fumble"))
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(0L, 0L, 0L))
  p <- cleanup_fixture("Fumble Recovery (Own)", "John Runner run for 1 yd and fumbled, recovered by CLEM")
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
})

test_that("nullified events are zero and replay reversals require their final event", {
  p <- cleanup_fixture(rep("Interception Return", 5), c(
    "John Passer pass intercepted by Alan Defender, NO PLAY",
    "John Passer pass intercepted by Alan Defender, play was nullified",
    "John Passer pass intercepted by Alan Defender; ruling overturned",
    "John Passer pass intercepted by Alan Defender; replay confirms the ruling",
    "John Passer pass intercepted by Alan Defender"))
  p$penalty_no_play <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(0L, 0L, NA_integer_, NA_integer_, 0L))
  expect_identical(x$clean_scrimmage, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_true(all(x$clean_status[c(3, 4)] == "unresolved"))
})

test_that("explicit nullification variants do not escape the no-play guard", {
  p <- cleanup_fixture("Interception Return", paste(
    "John Passer pass intercepted by Alan Defender;",
    c("the play is nullified", "NO-PLAY", "NO - PLAY", "the play has been negated",
      "the play is not nullified", "no player was penalized")))
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(0L, 0L, 0L, 0L, 1L, 1L))
  expect_identical(x$clean_scrimmage, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_true(all(x$clean_reason[1:4] == "nullified_play"))
})

test_that("canonical-equivalent opponents cannot establish giveaway ownership", {
  p <- cleanup_fixture("Interception Return", "John Passer pass intercepted by Alan Defender")
  p$pos_team <- "USC"; p$def_pos_team <- "Southern California"
  before <- p; x <- play_cleanup_classify(p)
  expect_true(is.na(x$clean_giveaway)); expect_equal(x$clean_status, "unresolved")
  expect_identical(x[names(p)], before)
  p$play_type <- "Rush"
  p$play_text <- "John Runner run for 3 yards and fumbled, recovered by Southern California"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
})

test_that("explicit possessing-team interceptions are contradictions without surname guessing", {
  p <- cleanup_fixture("Interception Return", c(
    "John Passer pass intercepted by LSU Alan Defender",
    "John Passer pass intercepted by CLEM Alan Defender",
    "John Passer pass intercepted by Williams",
    "John Passer pass intercepted by Alan Defender return for 3 yds to the LSU 20"))
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(NA_integer_, 1L, 1L, 1L))
  expect_equal(x$clean_reason[1], "conflicting_interception_ownership")
  expect_equal(x$clean_evidence[3], "explicit_intercepted_pass")
  p <- cleanup_fixture("Interception Return", "John Passer pass intercepted by USC Alan Defender")
  p$pos_team <- "Southern California"; p$def_pos_team <- "UCLA"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
  p$pos_team <- "USC"; p$def_pos_team <- "USC Upstate"
  p$play_text <- "John Passer pass intercepted by USC Upstate Alan Defender"
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
})

test_that("declined penalties differ from penalties with unclear event standing", {
  p <- cleanup_fixture("Interception Return", c(
    "John Passer pass intercepted by Alan Defender. PENALTY offside declined",
    "John Passer pass intercepted by Alan Defender. PENALTY holding",
    "John Passer pass intercepted by Alan Defender. PENALTY offside declined. PENALTY holding accepted"))
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(1L, NA_integer_, NA_integer_))
  p <- cleanup_fixture("Penalty", "PENALTY false start 5 yards")
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, 0L); expect_false(x$clean_scrimmage)
})

test_that("kick and multi-possession turnovers need separate ownership review", {
  p <- cleanup_fixture(c("Kickoff Team Fumble Recovery", "Fumble Recovery (Opponent)",
    "Interception Return", "Rush"), c(
    "Kickoff return for 12 yds, fumbled, recovered by CLEM",
    "John Returner punt return for 2 yds and fumbled, recovered by CLEM",
    "John Passer pass intercepted by Alan Defender who fumbled, recovered by LSU",
    "John Runner run for 3 yds, John Runner fumbled, recovered by CLEM, Alan Defender fumbled, recovered by LSU"))
  x <- play_cleanup_classify(p)
  expect_true(all(is.na(x$clean_giveaway)))
  expect_identical(x$clean_scrimmage, c(FALSE, FALSE, TRUE, TRUE))
  expect_match(x$clean_reason[3], "multiple_possession")
})

test_that("natural possession changes are not giveaways and unexplained ones are not zero", {
  p <- cleanup_fixture(c("Field Goal Missed", "Punt", "Passing Touchdown", "Rush", "Sack"),
    c("John Kicker 44 yd field goal missed", "John Punter punt for 41 yards",
      "John Passer pass complete for a touchdown", "John Runner run for 2 yds", "John Passer sacked for a loss of 3 yards"))
  p$turnover <- 1L; p$change_of_pos_team <- 1L
  p$down <- c(4L, 4L, 1L, 4L, 3L)
  p$distance <- 5L; p$yards_gained <- c(0, 41, 30, 2, -3)
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(0L, 0L, 0L, 0L, NA_integer_))
  p <- cleanup_fixture("Pass Incompletion", "John Passer pass incomplete")
  p$change_of_pos_team <- 1L; p$end_of_half <- 1L
  expect_identical(play_cleanup_classify(p)$clean_giveaway, 0L)
})

test_that("kneels, spikes, administrative rows and score transitions retain distinct semantics", {
  p <- cleanup_fixture(c("Rush", "Pass Incompletion", "End Period", "Timeout"),
    c("John Passer kneels for a loss of 2 yards", "John Passer spikes the ball",
      "End of 1st quarter", "Timeout LSU"))
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, rep(0L, 4)); expect_false(any(x$clean_scrimmage))
  p <- cleanup_fixture("Timeout", "John Passer pass intercepted by Alan Defender")
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway))
})

test_that("drive outcome only flags its unresolved terminal event and ignores next-drive labels", {
  p <- cleanup_fixture(c("Rush", "Sack"), c("John Runner run for 3 yds", "John Passer sacked for a loss of 8 yards"))
  p$drive_result <- "FUMBLE"; p$drive_result_detailed <- "Offense Touchdown"
  x <- play_cleanup_classify(p)
  expect_identical(x$clean_giveaway, c(0L, NA_integer_))
  expect_equal(x$clean_reason[2], "drive_giveaway_missing_event")
  shuffled <- play_cleanup_classify(p[2:1, ])
  expect_identical(shuffled$clean_giveaway, rev(x$clean_giveaway))
  p$drive_result <- "PUNT"; p$drive_result_detailed <- "FUMBLE"
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(0L, 0L))
})

test_that("drive identity conflicts and incomplete sequences cannot assign a turnover", {
  p <- cleanup_fixture(c("Rush", "Sack"), c("John Runner run for 3 yds", "John Passer sacked for a loss of 8 yards"))
  p$drive_result <- c("DOWNS", "FUMBLE")
  expect_equal(play_cleanup_classify(p)$clean_reason[2], "conflicting_drive_metadata")
  p$game_play_number <- NA
  expect_true(all(is.na(play_cleanup_classify(p)$clean_giveaway)))
  p$drive_id <- NA
  expect_true(is.na(play_cleanup_classify(p)$clean_giveaway[2]))
  p <- cleanup_fixture(c("Rush", "Sack"), c("John Runner run for 3 yds", "John Passer sacked for a loss of 8 yards"))
  p$game_id <- c("game_a", "game_b"); p$drive_result <- c("PUNT", "FUMBLE")
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(0L, NA_integer_))
})

test_that("an already identified drive event is not copied to a later administrative row", {
  p <- cleanup_fixture(c("Pass", "Timeout"),
    c("John Passer pass intercepted by Alan Defender", "Timeout LSU"))
  p$drive_result <- "INT"
  expect_identical(play_cleanup_classify(p)$clean_giveaway, c(1L, 0L))
})

test_that("confirmed events do not conceal contradictory drive outcomes or ownership", {
  p <- cleanup_fixture(c("Rush", "Pass"),
    c("John Runner run for 3 yds", "John Passer pass intercepted by Alan Defender"))
  p$drive_result <- c("FUMBLE", "INT")
  x <- play_cleanup_classify(p)
  expect_true(all(is.na(x$clean_giveaway)))
  expect_true(all(x$clean_reason == "conflicting_drive_metadata"))
  p$drive_result <- "FUMBLE"
  expect_true(all(is.na(play_cleanup_classify(p)$clean_giveaway)))
  p$drive_result <- "INT"; p$pos_team[1] <- "Clemson"
  expect_true(all(is.na(play_cleanup_classify(p)$clean_giveaway)))
})

test_that("numeric ID collisions and exact duplicates never remove or merge rows", {
  p <- cleanup_fixture(c("Rush", "Pass"),
    c("John Runner run for 3 yds", "John Passer pass intercepted by Alan Defender"))
  p$id_play <- 4.017527e17
  x <- play_cleanup_classify(p)
  expect_equal(nrow(x), 2); expect_identical(x$clean_giveaway, c(0L, 1L))
  p <- rbind(p, p[2, ]); x <- play_cleanup_classify(p)
  expect_equal(nrow(x), 3); expect_identical(x[names(p)], p)
  expect_identical(x$clean_giveaway, c(0L, 1L, 1L))
})

test_that("actual retained Michigan LSU and Miami witnesses stay regression fixtures", {
  paths <- c(
    `2023` = "cfb_v3/output/experiments/matchup_source_probe/20260912T014121.700Z/upstream_2023.rds",
    `2025` = "cfb_v3/output/experiments/matchup_source_probe/20260912T014320.238Z/upstream_2025.rds",
    `2022` = "cfb_v3/output/experiments/matchup_source_probe/20260912T014320.238Z/upstream_2022.rds")
  if (!all(file.exists(paths))) skip("Optional full upstream witness archives are absent.")
  witnesses <- list()
  for (year in names(paths)) {
    p <- readRDS(paths[[year]])
    game <- switch(year, `2023` = "401520162", `2025` = "401752671", `2022` = "401411144")
    p <- p[as.character(p$game_id) == game, , drop = FALSE]
    before <- serialize(p, NULL); x <- play_cleanup_classify(p)
    expect_identical(serialize(p, NULL), before)
    expect_true(all(vapply(names(p), function(field)
      identical(x[[field]], p[[field]]), logical(1))))
    expect_identical(class(x), class(p))
    witnesses[[year]] <- x
  }
  michigan <- witnesses[["2023"]]
  michigan <- michigan[michigan$game_play_number == 99 &
    michigan$play_type == "Fumble Recovery (Opponent)", ]
  expect_equal(nrow(michigan), 1)
  expect_identical(michigan$clean_giveaway, 0L)
  expect_equal(michigan$clean_reason, "own_recovery")
  lsu <- witnesses[["2025"]]
  lsu <- lsu[lsu$game_play_number %in% c(14, 69, 90), ]
  expect_equal(nrow(lsu), 3)
  expect_identical(lsu$clean_giveaway, rep(1L, 3))
  miami <- witnesses[["2022"]]
  miami <- miami[miami$game_play_number %in% c(60, 128, 150), ]
  expect_equal(nrow(miami), 3)
  expect_true(all(is.na(miami$clean_giveaway)))
  expect_true(all(miami$clean_scrimmage))
  expect_true(all(miami$clean_status == "unresolved"))
  expect_true(all(miami$turnover == 0 & miami$turnover_indicator == 0))
})
