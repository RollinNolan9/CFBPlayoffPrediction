library(testthat)
for (file in c("store.R", "features.R", "historical_data.R")) source(file.path("cfb_v2", file))
for (file in c("matchup_drive_audit.R", "play_cleanup_audit.R"))
  source(file.path("cfb_v3/experiments", file))

cleanup_audit_fixture <- function(n = 2L, team = "A", game = "g1", season = 2025L) {
  data.frame(game_id = rep(game, n), season = rep(season, n), id_play = seq_len(n),
    game_play_number = seq_len(n), drive_id = paste0("d", seq_len(n)), drive_result = "PUNT",
    period = 1L, clock_minutes = 12L, clock_seconds = 0L, down = 1L, distance = 10L,
    pos_team = team, def_pos_team = ifelse(team == "A", "B", "A"), home = "A", away = "B",
    pos_team_score = 0L, def_pos_team_score = 0L, play_type = "Rush", play_text = "A player rushes for five yards",
    EPA = 0.5, rush = 1L, pass = 0L, sack = 0L, turnover = 0L, turnover_indicator = 0L,
    garbage_time = FALSE, clean_giveaway = 0, clean_scrimmage = TRUE, clean_status = "resolved",
    clean_reason = "fixture_classification", clean_evidence = "fixture_evidence", stringsAsFactors = FALSE)
}

cleanup_box_fixture <- function(total = 0, game = "g1", team = "A", season = 2025L) {
  data.frame(game_id = game, season = season, team = team, home = "A", away = "B",
    turnovers = total, interceptions = total, fumbles_lost = 0,
    status = "matched", duplicate_status = "unique", source_file = "fixture.json", stringsAsFactors = FALSE)
}

test_that("full-game events are separated from the unchanged model filters", {
  p <- cleanup_audit_fixture(4)
  p$clean_giveaway <- 1
  p$play_type[2] <- "Kickoff Return"; p$rush[2] <- 0; p$clean_scrimmage[2] <- FALSE
  p$garbage_time[3] <- TRUE; p$EPA[4] <- NA_real_
  p$drive_result <- "FUMBLE"
  out <- play_cleanup_reconcile(p, cleanup_box_fixture(4))
  x <- out$team_game_reconciliation
  expect_equal(x$classified_count_lower_bound, 4)
  expect_equal(x$legacy_model_count_lower_bound, 1)
  expect_equal(x$resolved_giveaways_outside_legacy_model, 3)
  expect_equal(x$unknown_event_count, 0)
  expect_true(x$count_reconciled)
  expect_false(x$event_verification_complete)
  expect_equal(sum(out$exception_queue$exception_type == "resolved_event_model_excluded"), 3)
  expect_setequal(out$play_ledger$audit_model_exclusion,
    c("included", "outside_legacy_scrimmage_gate", "competitive_or_clock_filter", "missing_finite_epa"))
})

test_that("observed count equality cannot resolve unknown events", {
  p <- cleanup_audit_fixture()
  p$clean_giveaway <- c(1, NA_real_); p$clean_status[2] <- "unresolved"
  out <- play_cleanup_reconcile(p, cleanup_box_fixture(1)); x <- out$team_game_reconciliation
  expect_true(x$count_agreement)
  expect_false(x$count_reconciled)
  expect_equal(x$classified_count_lower_bound, 1)
  expect_equal(x$unknown_event_count, 1)
  expect_equal(x$legacy_model_unknown_event_count, 1)
  expect_match(x$blockers, "unresolved_events")
  expect_equal(out$play_ledger$clean_giveaway, c(1, NA_real_))
  expect_true(any(out$exception_queue$exception_type == "team_game_review"))
})

test_that("missing classifier status or evidence remains unknown even on numeric zero", {
  p <- cleanup_audit_fixture(3)
  p$clean_status[1] <- NA_character_; p$clean_reason[2] <- NA_character_; p$clean_evidence[3] <- ""
  x <- play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation
  expect_equal(x$unknown_event_count, 3)
  expect_true(x$count_agreement); expect_false(x$count_reconciled)
})

test_that("absent games and absent team plays produce missing counts, not zero", {
  p <- cleanup_audit_fixture(team = "B")
  boxes <- rbind(cleanup_box_fixture(), cleanup_box_fixture(game = "absent"))
  out <- play_cleanup_reconcile(p, boxes); x <- out$team_game_reconciliation
  expect_equal(x$source_status, c("missing_team_pbp", "missing_game_pbp"))
  expect_true(all(is.na(x$classified_count_lower_bound)))
  expect_true(all(is.na(x$unknown_event_count)))
  expect_true(all(!x$count_reconciled))
  expect_equal(sum(out$exception_queue$exception_type == "team_game_review"), 2)
  empty <- p[FALSE, ]
  expect_equal(play_cleanup_reconcile(empty, cleanup_box_fixture())$team_game_reconciliation$source_status,
    "missing_game_pbp")
})

test_that("duplicate box keys never multiply plays or silently become qualified", {
  p <- cleanup_audit_fixture()
  for (total in c(0, 1)) {
    b <- rbind(cleanup_box_fixture(), cleanup_box_fixture(total))
    out <- play_cleanup_reconcile(p, b)
    expect_equal(nrow(out$team_game_reconciliation), 1)
    expect_equal(out$team_game_reconciliation$source_rows, 2)
    expect_equal(out$team_game_reconciliation$box_status, "duplicate_box_key_quarantined")
    expect_true(is.na(out$team_game_reconciliation$reported_total))
    expect_equal(nrow(out$box_ledger), 2)
  }
})

test_that("bad box totals and missing qualification statuses cannot reconcile", {
  p <- cleanup_audit_fixture()
  variants <- list(cleanup_box_fixture(), cleanup_box_fixture(), cleanup_box_fixture(),
    cleanup_box_fixture(), cleanup_box_fixture())
  variants[[1]]$status <- NA_character_
  variants[[2]]$duplicate_status <- NA_character_
  variants[[3]]$turnovers <- NA_real_
  variants[[4]]$turnovers <- 1
  variants[[5]]$turnovers <- "not reported"
  for (b in variants) {
    x <- play_cleanup_reconcile(p, b)$team_game_reconciliation
    expect_false(x$count_reconciled); expect_true(is.na(x$reported_total))
    expect_true(nzchar(x$blockers))
  }
})

test_that("only full-source duplicates are flagged and no source rows are deleted", {
  p <- cleanup_audit_fixture()
  p$id_play <- 1L
  expect_equal(play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation$exact_duplicate_rows, 0)
  duplicate <- rbind(p[1, ], p[1, ])
  original <- duplicate
  out <- play_cleanup_reconcile(duplicate, cleanup_box_fixture())
  expect_identical(duplicate, original)
  expect_equal(nrow(out$play_ledger), 2)
  expect_equal(out$team_game_reconciliation$exact_duplicate_rows, 2)
  expect_equal(out$team_game_reconciliation$unknown_event_count, 2)
  expect_false(out$team_game_reconciliation$count_reconciled)
})

test_that("conflicting detailed event identities stay quarantined", {
  p <- cleanup_audit_fixture()
  p$game_play_number <- 1L; p$play_text[2] <- "Different event text"
  x <- play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation
  expect_equal(x$conflicting_identity_rows, 2)
  expect_equal(x$unknown_event_count, 2)
  expect_match(x$blockers, "conflicting_source_identity")
})

test_that("unattributed source plays block count agreement and retain an exception", {
  p <- cleanup_audit_fixture()
  p$pos_team[2] <- NA_character_
  out <- play_cleanup_reconcile(p, cleanup_box_fixture())
  expect_equal(out$team_game_reconciliation$unassigned_game_rows, 1)
  expect_false(out$team_game_reconciliation$count_reconciled)
  expect_true(any(is.na(out$exception_queue$pos_team)))
  p$pos_team[2] <- "Wrong team"
  expect_equal(play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation$unassigned_game_rows, 1)
  p$game_id[2] <- NA_character_
  expect_match(play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation$blockers,
    "unkeyed_source_rows_in_batch")
})

test_that("legacy filter membership is preserved including fallback score differential", {
  p <- cleanup_audit_fixture(4)
  p$garbage_time <- NA
  p$period <- c(1L, 4L, 2L, 1L); p$pos_team_score <- c(0L, 30L, 40L, 0L)
  p$play_text[4] <- "Quarterback kneels to end half"
  mask <- play_cleanup_model_masks(p)
  expect_equal(mask$audit_competitive, c(TRUE, FALSE, FALSE, FALSE))
  tagged <- p; tagged$id_play <- seq_len(nrow(p))
  expected <- historical_competitive_plays(tagged)$id_play
  expect_equal(which(mask$audit_competitive), expected)
  p$down[1] <- NA_integer_
  expect_equal(play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation$unknown_event_count, 0)
})

test_that("clean scrimmage contradictions are diagnostics, not changed legacy denominators", {
  p <- cleanup_audit_fixture(1)
  p$clean_giveaway <- 1; p$rush <- 0; p$play_type <- "Fumble Recovery (Opponent)"
  x <- play_cleanup_reconcile(p, cleanup_box_fixture(1))$team_game_reconciliation
  expect_equal(x$legacy_model_count_lower_bound, 0)
  expect_equal(x$clean_model_count_lower_bound, 1)
  expect_equal(x$model_eligibility_contradiction_count, 1)
  expect_false(x$count_reconciled)
})

test_that("drive conflicts are visible without converting drives to invented events", {
  p <- cleanup_audit_fixture()
  p$drive_id <- "one_drive"; p$drive_result <- c("FUMBLE", "PUNT")
  out <- play_cleanup_reconcile(p, cleanup_box_fixture())
  expect_equal(out$team_game_reconciliation$classified_count_lower_bound, 0)
  expect_equal(out$team_game_reconciliation$conflicting_drive_rows, 2)
  expect_false(out$team_game_reconciliation$count_reconciled)
  expect_equal(nrow(out$drive_ledger), 2)
})

test_that("exception context preserves evidence and neighboring plays from the same game", {
  p <- cleanup_audit_fixture(3)
  p$play_text <- c("Before", "Own recovery with conflicting fields", "After")
  p$clean_giveaway[2] <- NA_real_; p$clean_status[2] <- "unresolved"
  p$clean_reason[2] <- "own_recovery_type_conflict"; p$clean_evidence[2] <- "original witness evidence"
  out <- play_cleanup_reconcile(p, cleanup_box_fixture())
  x <- out$exception_queue[out$exception_queue$audit_row == 2 & !is.na(out$exception_queue$audit_row), ]
  expect_equal(x$audit_previous_text, "Before"); expect_equal(x$audit_next_text, "After")
  expect_equal(x$clean_reason, p$clean_reason[2]); expect_equal(x$clean_evidence, p$clean_evidence[2])
  expect_equal(x$EPA, p$EPA[2]); expect_equal(x$game_play_number, 2)
})

test_that("combined seasons and aliases join once while unrelated games are scoped out", {
  p <- rbind(cleanup_audit_fixture(1, "UConn", "old", 2024),
    cleanup_audit_fixture(1, "UConn", "new", 2025), cleanup_audit_fixture(1, game = "unrelated"))
  b <- rbind(cleanup_box_fixture(game = "old", team = "Connecticut", season = 2024),
    cleanup_box_fixture(game = "new", team = "Connecticut", season = 2025))
  b$home <- "Connecticut"
  out <- play_cleanup_reconcile(p, b)
  expect_equal(nrow(out$team_game_reconciliation), 2)
  expect_equal(out$team_game_reconciliation$source_rows, c(1, 1))
  expect_equal(nrow(out$play_ledger), 2)
  expect_equal(out$play_ledger$audit_row, c(1, 2))
  expect_true(all(out$team_game_reconciliation$count_reconciled))
})

test_that("schema failures stop loudly and partial identity cannot certify agreement", {
  p <- cleanup_audit_fixture()
  bad <- p; bad$clean_giveaway <- c(0, 2)
  expect_error(play_cleanup_reconcile(bad, cleanup_box_fixture()), "0/1/NA")
  bad <- p; bad$clean_scrimmage <- c(0, 1)
  expect_error(play_cleanup_reconcile(bad, cleanup_box_fixture()), "logical")
  bad <- p; bad$clean_status <- "probably"
  expect_error(play_cleanup_reconcile(bad, cleanup_box_fixture()), "resolved/unresolved")
  p$game_play_number <- NULL
  out <- play_cleanup_reconcile(p, cleanup_box_fixture())
  expect_false(out$team_game_reconciliation$count_reconciled)
  expect_match(out$team_game_reconciliation$blockers, "full_source_identity_unavailable")
})

test_that("empty box scopes return empty tables without placeholder exceptions", {
  out <- play_cleanup_reconcile(cleanup_audit_fixture(), cleanup_box_fixture()[FALSE, ])
  expect_equal(nrow(out$team_game_reconciliation), 0)
  expect_equal(nrow(out$exception_queue), 0)
  expect_equal(nrow(out$play_ledger), 0)
})

test_that("the audit accepts supplied Michigan and LSU witness decisions without reclassification", {
  michigan <- cleanup_audit_fixture(1, "Michigan", "401520162", 2023)
  michigan$play_type <- "Fumble Recovery (Opponent)"
  michigan$play_text <- "J.J. McCarthy recovers his own fumble"
  michigan$turnover <- 1L; michigan$clean_giveaway <- 0L
  michigan$clean_evidence <- "classifier own-recovery evidence"
  lsu <- cleanup_audit_fixture(1, "LSU", "401752671", 2025)
  lsu$rush <- 0L; lsu$pass <- 1L; lsu$play_type <- "Pass Reception"
  lsu$play_text <- "Chris Hilton Jr. fumbles; recovered by Clemson"
  lsu$clean_giveaway <- 1L; lsu$turnover_indicator <- 0L
  lsu$clean_evidence <- "classifier explicit recovery evidence"
  p <- rbind(michigan, lsu)
  boxes <- rbind(cleanup_box_fixture(game = "401520162", team = "Michigan", season = 2023),
    cleanup_box_fixture(1, game = "401752671", team = "LSU", season = 2025))
  boxes$home <- boxes$team
  out <- play_cleanup_reconcile(p, boxes)
  expect_equal(out$team_game_reconciliation$classified_count_lower_bound, c(0, 1))
  expect_equal(out$play_ledger$turnover, p$turnover)
  expect_equal(out$play_ledger$play_type, p$play_type)
  expect_equal(out$play_ledger$EPA, p$EPA)
  expect_equal(out$drive_ledger$season, c(2023, 2025))
})

cleanup_admin_fixture <- function() {
  p <- cleanup_audit_fixture(3)
  p$play_type <- c("Timeout", "End Period", "Penalty")
  p$play_text <- c("Timeout A", "End of first quarter", "False start, NO PLAY")
  p$clean_scrimmage <- FALSE
  p$clean_reason <- c("administrative", "administrative", "nullified_play")
  for (field in c("game_play_number", "down", "distance", "clock_minutes", "clock_seconds",
      "rush", "pass", "sack", "garbage_time", "pos_team_score", "def_pos_team_score"))
    p[[field]] <- NA
  p$drive_id <- "same_drive"
  p
}

test_that("explicit resolved non-events do not need irrelevant sequence or model-gate metadata", {
  p <- cleanup_admin_fixture()
  out <- play_cleanup_reconcile(p, cleanup_box_fixture())
  x <- out$team_game_reconciliation
  expect_equal(x$unknown_event_count, 0)
  expect_equal(x$model_eligibility_unknown_count, 0)
  expect_equal(x$conflicting_identity_rows, 0)
  expect_true(x$count_reconciled)
  expect_equal(nrow(out$exception_queue), 0)
  expect_true(all(out$play_ledger$audit_resolved_non_event))
  expect_true(all(is.na(out$play_ledger$audit_legacy_scrimmage)))
})

test_that("the non-event exemption cannot hide missing identity, evidence, or actual turnovers", {
  p <- cleanup_admin_fixture()[1, ]
  variants <- list(p, p, p, p, p, p)
  variants[[1]]$clean_status <- "unresolved"
  variants[[2]]$clean_evidence <- NA_character_
  variants[[3]]$pos_team <- NA_character_
  variants[[4]]$game_id <- NA_character_
  variants[[5]]$season <- NA_integer_
  variants[[6]]$clean_giveaway <- 1
  for (v in variants) expect_false(play_cleanup_reconcile(v, cleanup_box_fixture())$
    team_game_reconciliation$count_reconciled)
  blank <- p; blank$play_type <- NA_character_; blank$play_text <- NA_character_
  blank$clean_reason <- "missing_event_semantics"; blank$clean_status <- "unresolved"
  blank$clean_giveaway <- NA_real_
  x <- play_cleanup_reconcile(blank, cleanup_box_fixture())$team_game_reconciliation
  expect_equal(x$unknown_event_count, 1)
  expect_equal(x$model_eligibility_unknown_count, 1)
  expect_false(x$count_reconciled)
})

test_that("non-event scope does not suppress real legacy denominator contradictions", {
  p <- cleanup_admin_fixture()[3, ]
  p$play_type <- "Pass"; p$pass <- 1L; p$garbage_time <- FALSE
  x <- play_cleanup_reconcile(p, cleanup_box_fixture())$team_game_reconciliation
  expect_equal(x$model_eligibility_contradiction_count, 1)
  expect_false(x$count_reconciled)
  p <- cleanup_admin_fixture()[1, ]; p <- rbind(p, p)
  expect_equal(play_cleanup_reconcile(p, cleanup_box_fixture())$
    team_game_reconciliation$exact_duplicate_rows, 2)
})
