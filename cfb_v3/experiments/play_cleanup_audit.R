play_cleanup_audit_key <- function(game_id, season, team) {
  paste(trimws(as.character(game_id)), as.character(season), canonical_team(team), sep = "\r")
}

play_cleanup_audit_blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))

play_cleanup_model_masks <- function(pbp) {
  if (!nrow(pbp)) return(data.frame(audit_competitive = logical(), audit_filter_unknown = logical(),
    audit_legacy_scrimmage = logical(), audit_finite_epa = logical(),
    audit_legacy_model_eligible = logical(), audit_clean_model_eligible = logical(),
    audit_model_exclusion = character()))
  # Reuse the production competitive filter, tagging a copy to retain row identity.
  tagged <- compact_cfb_pbp(pbp)
  tagged$id_play <- seq_len(nrow(tagged))
  competitive <- historical_competitive_plays(tagged)
  kept <- seq_len(nrow(tagged)) %in% competitive$id_play[!is.na(competitive$id_play)]
  old_scrimmage <- as.logical(tagged$rush == 1 | tagged$pass == 1 | tagged$sack == 1 |
    grepl("pass|rush|run|sack", tagged$play_type, ignore.case = TRUE))
  team_known <- !play_cleanup_audit_blank(tagged$pos_team) &
    !play_cleanup_audit_blank(tagged$def_pos_team)
  finite_epa <- is.finite(suppressWarnings(as.numeric(tagged$EPA)))
  if (all(is.na(tagged$garbage_time))) {
    home_score <- ifelse(tagged$pos_team == tagged$home, tagged$pos_team_score,
      ifelse(tagged$def_pos_team == tagged$home, tagged$def_pos_team_score, NA))
    away_score <- ifelse(tagged$pos_team == tagged$away, tagged$pos_team_score,
      ifelse(tagged$def_pos_team == tagged$away, tagged$def_pos_team_score, NA))
    score_known <- is.finite(suppressWarnings(as.numeric(home_score))) &
      is.finite(suppressWarnings(as.numeric(away_score))) &
      is.finite(suppressWarnings(as.numeric(tagged$period)))
    filter_unknown <- !score_known
  } else {
    filter_unknown <- is.na(tagged$garbage_time)
  }
  old_eligible <- kept & !is.na(old_scrimmage) & old_scrimmage & finite_epa & team_known
  clean_eligible <- kept & !is.na(pbp$clean_scrimmage) & pbp$clean_scrimmage &
    finite_epa & team_known
  exclusion <- rep("included", nrow(pbp))
  exclusion[!team_known] <- "missing_team_identity"
  exclusion[team_known & !finite_epa] <- "missing_finite_epa"
  exclusion[team_known & finite_epa & is.na(old_scrimmage)] <- "unknown_legacy_scrimmage_gate"
  exclusion[team_known & finite_epa & !is.na(old_scrimmage) & !old_scrimmage] <-
    "outside_legacy_scrimmage_gate"
  exclusion[!kept] <- "competitive_or_clock_filter"
  data.frame(audit_competitive = kept, audit_filter_unknown = filter_unknown,
    audit_legacy_scrimmage = old_scrimmage, audit_finite_epa = finite_epa,
    audit_legacy_model_eligible = old_eligible, audit_clean_model_eligible = clean_eligible,
    audit_model_exclusion = exclusion, stringsAsFactors = FALSE)
}

play_cleanup_read_audit_boxes <- function(project,
    box_dir = file.path(project, "cfb_v3/output/experiments/matchup_box_audit/20260912T032310.128Z")) {
  external_verify(box_dir)
  boxes <- read.csv(file.path(box_dir, "reported_stats_ledger.csv"), stringsAsFactors = FALSE)
  cohort <- read.csv(file.path(box_dir, "team_game_reconciliation.csv"), stringsAsFactors = FALSE)
  assert_unique_keys(cohort, c("season", "game_id", "team"), "fixed audit cohort")
  key <- play_cleanup_audit_key(cohort$game_id, cohort$season, cohort$team)
  boxes[play_cleanup_audit_key(boxes$game_id, boxes$season, boxes$team) %in% key, , drop = FALSE]
}

play_cleanup_reconcile <- function(classified_pbp, boxes) {
  assert_columns(classified_pbp, c("game_id", "pos_team", "clean_giveaway", "clean_scrimmage",
    "clean_status", "clean_reason", "clean_evidence"), "classified PBP")
  assert_columns(boxes, c("game_id", "season", "team", "turnovers", "interceptions",
    "fumbles_lost", "status", "duplicate_status"), "reported box ledger")
  if (!"season" %in% names(classified_pbp) && !"year" %in% names(classified_pbp))
    stop("Classified PBP requires season or year.")
  if (any(grepl("^audit_", names(classified_pbp)))) stop("Reserved audit_ fields already present.")
  if (!is.numeric(classified_pbp$clean_giveaway) ||
      any(!is.na(classified_pbp$clean_giveaway) & !classified_pbp$clean_giveaway %in% c(0, 1)))
    stop("clean_giveaway must be numeric 0/1/NA.")
  if (!is.logical(classified_pbp$clean_scrimmage)) stop("clean_scrimmage must be logical.")
  if (any(!is.na(classified_pbp$clean_status) &
          !classified_pbp$clean_status %in% c("resolved", "unresolved")))
    stop("clean_status must be resolved/unresolved/NA.")

  p <- as.data.frame(classified_pbp, stringsAsFactors = FALSE)
  scoped <- as.character(p$game_id) %in% as.character(boxes$game_id) | play_cleanup_audit_blank(p$game_id)
  p <- p[scoped, , drop = FALSE]
  n <- nrow(p)
  p$audit_row <- which(scoped)
  p$audit_game_id <- trimws(as.character(p$game_id))
  p$audit_team <- canonical_team(p$pos_team)
  p$audit_season <- if ("season" %in% names(p)) p$season else p$year
  if ("year" %in% names(p)) p$audit_season[is.na(p$audit_season)] <- p$year[is.na(p$audit_season)]
  p$audit_key <- play_cleanup_audit_key(p$audit_game_id, p$audit_season, p$audit_team)
  p <- cbind(p, play_cleanup_model_masks(p))
  # Explicit classifier non-events need no down/clock/sequence to establish zero.
  p$audit_resolved_non_event <- !is.na(p$clean_status) & p$clean_status == "resolved" &
    !is.na(p$clean_giveaway) & p$clean_giveaway == 0 &
    !is.na(p$clean_scrimmage) & !p$clean_scrimmage &
    p$clean_reason %in% c("administrative", "nullified_play", "penalty_only", "kneel_or_spike") &
    !play_cleanup_audit_blank(p$clean_evidence)
  p$audit_identity_missing <- play_cleanup_audit_blank(p$audit_game_id) |
    is.na(p$audit_season) | play_cleanup_audit_blank(p$audit_team)
  p$audit_exact_duplicate <- rep(FALSE, n)
  identity <- c("game_id", "game_play_number", "clock_minutes", "clock_seconds", "down", "distance")
  identity_available <- all(identity %in% names(p))
  p$audit_identity_conflict <- rep(FALSE, n)
  if (identity_available && n) {
    source_fields <- names(classified_pbp)[!grepl("^clean_", names(classified_pbp))]
    exact <- as.data.frame(p[source_fields])
    p$audit_exact_duplicate <- duplicated(exact) | duplicated(exact, fromLast = TRUE)
    detailed_identity <- p[c(identity, "pos_team")]
    identity_missing <- play_cleanup_audit_blank(p$game_play_number) & !p$audit_resolved_non_event
    p$audit_identity_missing <- p$audit_identity_missing | identity_missing
    keyed <- which(!play_cleanup_audit_blank(p$game_play_number))
    groups <- split(keyed, do.call(paste, c(detailed_identity, sep = "\r"))[keyed])
    for (i in groups) if (length(i) > 1L && nrow(unique(exact[i, , drop = FALSE])) > 1L)
      p$audit_identity_conflict[i] <- TRUE
  }
  p$audit_event_unknown <- is.na(p$clean_giveaway) | is.na(p$clean_status) |
    p$clean_status != "resolved" | play_cleanup_audit_blank(p$clean_reason) |
    play_cleanup_audit_blank(p$clean_evidence) | p$audit_identity_missing |
    p$audit_exact_duplicate | p$audit_identity_conflict
  p$audit_gate_unknown <- !p$audit_resolved_non_event &
    (is.na(p$clean_scrimmage) | p$audit_filter_unknown | is.na(p$audit_legacy_scrimmage))
  p$audit_eligibility_contradiction <- !is.na(p$clean_scrimmage) &
    !is.na(p$audit_legacy_scrimmage) & p$clean_scrimmage != p$audit_legacy_scrimmage
  p$audit_resolved_giveaway <- !p$audit_event_unknown & !is.na(p$clean_giveaway) & p$clean_giveaway == 1

  # This is corroborating drive metadata, not a second event classifier.
  drive_fields <- c("game_id", "drive_id", "pos_team", "drive_result")
  drives <- if (all(drive_fields %in% names(p)) && n) matchup_drive_ledger(p) else data.frame()
  p$audit_drive_conflict <- rep(FALSE, n)
  if (nrow(drives)) {
    seasons <- split(p$audit_season, p$audit_game_id)
    drives$season <- vapply(as.character(drives$game_id), function(id) {
      values <- unique(seasons[[id]])
      if (length(values) == 1L && !is.na(values)) as.numeric(values) else NA_real_
    }, numeric(1))
    drive_key <- paste(drives$game_id, drives$drive_id, sep = "\r")
    bad <- drives$status == "conflicting_drive_quarantined" |
      (!is.na(drives$ambiguous_fumble_td) & drives$ambiguous_fumble_td)
    p$audit_drive_conflict <- paste(p$game_id, p$drive_id, sep = "\r") %in% drive_key[bad]
  }
  b <- as.data.frame(boxes, stringsAsFactors = FALSE)
  b$audit_box_row <- seq_len(nrow(b))
  b$audit_key <- play_cleanup_audit_key(b$game_id, b$season, b$team)
  missing_box_identity <- play_cleanup_audit_blank(b$game_id) | is.na(b$season) |
    play_cleanup_audit_blank(b$team)
  # Unkeyed rows each remain visible; no NA-to-NA or many-to-many join.
  b$audit_key[missing_box_identity] <- paste0("unkeyed_box_", which(missing_box_identity))
  groups <- split(seq_len(nrow(b)), factor(b$audit_key, levels = unique(b$audit_key)))
  p_groups <- split(seq_len(n), p$audit_key)
  game_groups <- split(seq_len(n), p$audit_game_id)
  unkeyed_source <- sum(play_cleanup_audit_blank(p$audit_game_id))
  summaries <- list()
  for (k in seq_along(groups)) {
    bi <- groups[[k]]; first <- b[bi[1], , drop = FALSE]
    key <- first$audit_key
    i <- p_groups[[key]]; if (is.null(i)) i <- integer()
    gi <- game_groups[[as.character(first$game_id)]]; if (is.null(gi)) gi <- integer()
    values <- lapply(b[bi, c("turnovers", "interceptions", "fumbles_lost"), drop = FALSE],
      function(v) suppressWarnings(as.numeric(as.character(v))))
    valid_stats <- vapply(values,
      function(v) all(is.finite(v) & v >= 0 & v == floor(v)), logical(1))
    box_status <- if (any(missing_box_identity[bi])) "missing_box_identity" else
      if (length(bi) != 1L) "duplicate_box_key_quarantined" else
      if (is.na(first$status) || first$status != "matched") "unqualified_box_status" else
      if (is.na(first$duplicate_status) || first$duplicate_status != "unique") "unqualified_box_duplicate_status" else
      if (!all(valid_stats)) "missing_or_invalid_box_totals" else
      if (values$turnovers[1] != values$interceptions[1] + values$fumbles_lost[1]) "conflicting_box_totals" else "qualified"
    reported <- if (box_status == "qualified") values$turnovers[1] else NA_real_
    roster <- canonical_team(b$team[as.character(b$game_id) == as.character(first$game_id)])
    for (field in intersect(c("home", "away"), names(b))) roster <- c(roster, canonical_team(first[[field]]))
    roster <- unique(roster[!play_cleanup_audit_blank(roster)])
    identity_problem <- gi[is.na(p$audit_season[gi]) | p$audit_season[gi] != first$season |
      play_cleanup_audit_blank(p$audit_team[gi]) | (length(roster) >= 2L & !p$audit_team[gi] %in% roster)]
    available <- length(i) > 0L
    source_status <- if (available) "team_plays_present_completeness_unverified" else
      if (!length(gi)) "missing_game_pbp" else "missing_team_pbp"
    lower <- if (available) sum(p$audit_resolved_giveaway[i]) else NA_integer_
    unknown <- if (available) sum(p$audit_event_unknown[i]) else NA_integer_
    gate_unknown <- if (available) sum(p$audit_gate_unknown[i]) else NA_integer_
    legacy <- if (available) sum(p$audit_resolved_giveaway[i] & p$audit_legacy_model_eligible[i]) else NA_integer_
    clean <- if (available) sum(p$audit_resolved_giveaway[i] & p$audit_clean_model_eligible[i]) else NA_integer_
    blockers <- character()
    if (box_status != "qualified") blockers <- c(blockers, box_status)
    if (!available) blockers <- c(blockers, source_status)
    if (length(identity_problem)) blockers <- c(blockers, "unattributed_or_wrong_season_source")
    if (unkeyed_source) blockers <- c(blockers, "unkeyed_source_rows_in_batch")
    if (available && unknown > 0L) blockers <- c(blockers, "unresolved_events")
    if (available && gate_unknown > 0L) blockers <- c(blockers, "unknown_model_eligibility")
    if (available && any(p$audit_eligibility_contradiction[i])) blockers <- c(blockers, "scrimmage_eligibility_contradiction")
    if (available && any(p$audit_exact_duplicate[i])) blockers <- c(blockers, "exact_source_duplicates_not_removed")
    if (available && any(p$audit_identity_conflict[i])) blockers <- c(blockers, "conflicting_source_identity")
    if (available && any(p$audit_drive_conflict[i])) blockers <- c(blockers, "conflicting_or_ambiguous_drive")
    if (!identity_available) blockers <- c(blockers, "full_source_identity_unavailable")
    agreement <- if (available && is.finite(reported)) lower == reported else NA
    if (isFALSE(agreement)) blockers <- c(blockers, "count_disagreement")
    reconciled <- !length(blockers) && isTRUE(agreement)
    summaries[[k]] <- data.frame(game_id = as.character(first$game_id), season = first$season,
      team = canonical_team(first$team), audit_key = key, box_input_rows = length(bi),
      box_status = box_status, reported_total = reported,
      reported_values = paste(unique(b$turnovers[bi]), collapse = " | "),
      source_status = source_status, source_rows = length(i),
      classified_count_lower_bound = lower, unknown_event_count = unknown,
      unassigned_game_rows = length(identity_problem), unkeyed_source_rows_in_batch = unkeyed_source,
      legacy_model_count_lower_bound = legacy, clean_model_count_lower_bound = clean,
      legacy_model_unknown_event_count = if (available) sum(p$audit_event_unknown[i] & p$audit_legacy_model_eligible[i]) else NA_integer_,
      model_eligibility_unknown_count = gate_unknown,
      model_eligibility_contradiction_count = sum(p$audit_eligibility_contradiction[i]),
      resolved_giveaways_outside_legacy_model = if (available) lower - legacy else NA_integer_,
      resolved_giveaways_outside_clean_model = if (available) lower - clean else NA_integer_,
      exact_duplicate_rows = sum(p$audit_exact_duplicate[i]),
      conflicting_identity_rows = sum(p$audit_identity_conflict[i]),
      conflicting_drive_rows = sum(p$audit_drive_conflict[i]),
      count_difference_lower_bound_minus_reported = lower - reported,
      count_agreement = agreement, count_reconciled = reconciled,
      reconciliation_status = if (reconciled) "count_agreement_not_event_verification" else "review_required",
      blockers = paste(unique(blockers), collapse = ";"), event_verification_complete = FALSE,
      source_independence = "CFBD/ESPN may share a provider; box totals are checks, not independent truth",
      stringsAsFactors = FALSE)
  }
  team_games <- if (length(summaries)) do.call(rbind, summaries) else data.frame()
  if (nrow(team_games)) assert_unique_keys(team_games, "audit_key", "reconciliation output")

  context <- intersect(c("game_id", "season", "year", "id_play", "game_play_number", "drive_id",
    "drive_play_number", "period", "clock_minutes", "clock_seconds", "down", "distance",
    "yards_to_goal", "yard_line", "pos_team", "def_pos_team", "home", "away",
    "pos_team_score", "def_pos_team_score", "pos_score_diff", "play_type", "play_text",
    "EPA", "rush", "pass", "sack", "turnover", "turnover_indicator", "drive_result",
    "drive_result_detailed", "lead_pos_team", "change_of_pos_team", "garbage_time",
    names(p)[grepl("^(clean_|audit_)", names(p))]), names(p))
  ledger <- p[context]
  ledger$audit_previous_text <- ledger$audit_next_text <- rep(NA_character_, n)
  if ("play_text" %in% names(p)) for (i in game_groups) {
    if ("game_play_number" %in% names(p)) i <- i[order(p$game_play_number[i], p$audit_row[i], na.last = TRUE)]
    if (length(i) > 1L) {
      ledger$audit_previous_text[i[-1]] <- as.character(p$play_text[i[-length(i)]])
      ledger$audit_next_text[i[-length(i)]] <- as.character(p$play_text[i[-1]])
    }
  }
  idx <- if (nrow(team_games)) match(p$audit_key, team_games$audit_key) else rep(NA_integer_, n)
  review_team <- rep(FALSE, n)
  if (nrow(team_games)) review_team[!is.na(idx)] <- !team_games$count_reconciled[idx[!is.na(idx)]]
  problem <- p$audit_event_unknown | p$audit_gate_unknown | p$audit_drive_conflict |
    p$audit_eligibility_contradiction
  excluded <- p$audit_resolved_giveaway & !p$audit_legacy_model_eligible
  select <- problem | excluded | (review_team & p$audit_resolved_giveaway)
  queue <- ledger[select, , drop = FALSE]
  queue$exception_type <- ifelse(problem[select], "unresolved_source_or_event",
    ifelse(excluded[select], "resolved_event_model_excluded", "resolved_event_in_review_game"))
  qi <- idx[select]
  queue$reported_total <- if (nrow(team_games)) team_games$reported_total[qi] else rep(NA_real_, nrow(queue))
  queue$team_blockers <- if (nrow(team_games)) team_games$blockers[qi] else rep(NA_character_, nrow(queue))
  if (nrow(team_games)) for (j in which(!team_games$count_reconciled)) {
    # Always emit a team-level exception, including missing events with no suspect play.
    row <- ledger[NA_integer_, , drop = FALSE]
    row$game_id <- team_games$game_id[j]
    if ("season" %in% names(row)) row$season <- team_games$season[j]
    row$pos_team <- team_games$team[j]
    row$audit_game_id <- team_games$game_id[j]; row$audit_team <- team_games$team[j]
    row$audit_season <- team_games$season[j]; row$audit_key <- team_games$audit_key[j]
    row$exception_type <- "team_game_review"
    row$reported_total <- team_games$reported_total[j]; row$team_blockers <- team_games$blockers[j]
    queue <- rbind(queue, row)
  }
  rownames(queue) <- rownames(ledger) <- rownames(team_games) <- NULL
  list(team_game_reconciliation = team_games, exception_queue = queue,
    play_ledger = ledger, box_ledger = b, drive_ledger = drives,
    limits = c("No classification, duplicate removal, source mutation, EPA repair, or model fitting occurs here.",
      "Lower bounds exclude unresolved and duplicate/conflicting rows; they are not zero-filled complete counts.",
      "Unknown counts concern observed rows only; missing source events have no defensible finite upper bound.",
      "Matching full-game totals does not independently verify individual events or source completeness.",
      "Rows outside requested game IDs are omitted; unkeyed source rows remain visible and block batch agreement.",
      "Filtered model counts intentionally differ from full-game box totals."))
}
