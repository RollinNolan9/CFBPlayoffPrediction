# Event audit only: no filtering, deduplication, EPA adjustment, or model flags.
play_cleanup_classify <- function(pbp) {
  if (!is.data.frame(pbp)) stop("pbp must be a data frame.")
  if (!exists("canonical_team", mode = "function", inherits = TRUE))
    stop("Source cfb_v2/features.R before using the play classifier.")
  clean_names <- c("clean_giveaway", "clean_scrimmage", "clean_status",
                   "clean_reason", "clean_evidence")
  if (anyDuplicated(names(pbp)) || any(clean_names %in% names(pbp)))
    stop("Input requires unique original columns and no classifier output columns.")
  n <- nrow(pbp)
  field_cache <- new.env(parent = emptyenv())
  missing_field <- rep(NA_character_, n)
  get <- function(name) {
    if (exists(name, envir = field_cache, inherits = FALSE))
      return(field_cache[[name]])
    if (!name %in% names(pbp)) return(missing_field)
    if (!is.atomic(pbp[[name]])) stop("Non-atomic event field: ", name)
    field_cache[[name]] <- as.character(pbp[[name]])
    field_cache[[name]]
  }
  norm <- function(x) {
    x <- tolower(trimws(x)); x[is.na(x)] <- ""
    gsub("[[:space:]]+", " ", x)
  }
  key <- function(x) gsub("[^a-z0-9]", "", norm(x))
  flag <- function(name) norm(get(name)) %in% c("1", "true")
  has <- function(pattern, value) grepl(pattern, value, perl = TRUE)
  type <- norm(get("play_type")); txt <- norm(get("play_text"))
  raw_pos <- norm(get("pos_team")); raw_def <- norm(get("def_pos_team"))
  pos <- norm(canonical_team(get("pos_team")))
  def <- norm(canonical_team(get("def_pos_team")))
  giveaway <- rep(NA_integer_, n); scrimmage <- rep(NA, n)
  reason <- rep("missing_event_semantics", n)
  evidence <- rep("", n)
  assign_result <- function(i, value, why, proof = "") {
    giveaway[i] <<- as.integer(value); reason[i] <<- why
    evidence[i] <<- proof
  }

  ordinary_types <- c("rush", "pass", "pass reception", "pass incompletion",
    "passing touchdown", "rushing touchdown", "sack")
  int_types <- c("interception return", "interception return touchdown")
  lost_types <- c("fumble recovery (opponent)",
    "fumble recovery (opponent) touchdown", "fumble return touchdown")
  own_type <- type == "fumble recovery (own)"
  administrative <- type %in% c("timeout", "end period", "end of half",
    "end of game", "end of regulation")
  special <- has("^(kickoff|punt|blocked punt|field goal|blocked field goal|missed field goal|extra point|two point|defensive 2pt)", type) |
    has("\\b(punts? for|punt return|kickoff|kick return|field goal|extra point|two.point conversion)\\b", txt)
  kneel <- has("\\b(kneel(?:s|ed|ing)?|spik(?:e|es|ed|ing))\\b", txt)
  action <- has("\\b(pass (?:complete|incomplete|intercepted)|sacked|(?:run|rush) for|rushes for|rushed for)\\b", txt)
  scrimmage[type %in% c(ordinary_types, int_types) | action |
    flag("rush") | flag("pass") | flag("sack")] <- TRUE
  scrimmage[special | administrative | kneel] <- FALSE

  fumble <- has("\\bfumbl(?:e|es|ed|ing)\\b", txt) |
    type %in% c(lost_types, "fumble recovery (own)")
  negated_int <- has("\\b(nearly|almost|not) intercepted\\b|\\b(dropped|potential|would.be) interception\\b", txt)
  intercepted <- has("\\b(pass intercepted|intercepted by|pass.*was intercepted)\\b", txt) & !negated_int
  int_event <- type %in% int_types | intercepted
  no_play <- has("\\bno(?:\\s+|\\s*-\\s*)play\\b|\\bplay (?:(?:is|was|has been) )?(?:nullified|negated)\\b", txt) |
    flag("penalty_no_play")
  replay <- has("\\b(overturned|reversed|replay|under review)\\b", txt)
  penalty <- has("\\bpenalt(?:y|ies)\\b", txt) | flag("penalty_flag")
  penalty_declined <- flag("penalty_declined") |
    has("\\bpenalt(?:y|ies)[^.]*\\bdeclined\\b", txt)
  penalty_conflict <- flag("penalty_offset") |
    has("\\b(accepted|offsetting)\\b", txt) |
    lengths(regmatches(txt, gregexpr("\\bpenalty\\b", txt, perl = TRUE))) > 1L

  # Only explicit team tokens, not surname or abbreviation-prefix guesses.
  # The small legacy alias set is limited to witnessed source spellings.
  legacy_aliases <- list(clemson = "clem", michigan = "mich",
    `east carolina` = "ecu", miami = "miafl")
  aliases <- function(i, side) {
    team <- if (side == "pos") pos[i] else def[i]
    raw_team <- if (side == "pos") raw_pos[i] else raw_def[i]
    values <- c(team, raw_team, legacy_aliases[[team]])
    for (field in paste0(if (side == "pos") "pos_team" else "def_pos_team",
                         c("_abbreviation", "_abbrev")))
      values <- c(values, norm(get(field)[i]))
    values <- unique(values[nzchar(values)])
    values
  }
  starts_team <- function(value, candidates) any(vapply(candidates, function(a) {
    startsWith(value, a) && (nchar(value) == nchar(a) ||
      substr(value, nchar(a) + 1L, nchar(a) + 1L) %in% c(" ", ",", ".", ")", ":"))
  }, logical(1)))
  interception_sides <- function(i) {
    text <- get("play_text")[i]
    if (is.na(text) || !nzchar(trimws(text))) return(character())
    matches <- gregexpr("intercepted(?:\\s+by)?\\s+", text,
                        ignore.case = TRUE, perl = TRUE)[[1]]
    sides <- character()
    if (matches[1] > 0L) for (j in seq_along(matches)) {
      value <- substring(text, matches[j] + attr(matches, "match.length")[j])
      words <- strsplit(trimws(value), "[[:space:]]+")[[1]]
      prefixes <- vapply(seq_along(words), function(k)
        sub("[,;:.]+$", "", paste(words[seq_len(k)], collapse = " ")), character(1))
      teams <- norm(canonical_team(prefixes))
      sides <- c(sides,
        if (starts_team(norm(value), aliases(i, "pos")) || pos[i] %in% teams) "pos",
        if (starts_team(norm(value), aliases(i, "def")) || def[i] %in% teams) "def")
    }
    unique(sides)
  }
  recovery_side <- function(i) {
    sides <- character(); proofs <- character()
    for (field in c("fumble_recovered_team", "fumble_recovery_team", "recovery_team")) {
      value <- norm(get(field)[i])
      if (!nzchar(value)) next
      side <- c(if (value %in% aliases(i, "pos")) "pos",
                if (value %in% aliases(i, "def")) "def")
      sides <- c(sides, if (length(side) == 1L) side else "unknown")
      proofs <- c(proofs, paste0(field, "=", value))
    }
    matches <- gregexpr("recovered by\\s+", txt[i], perl = TRUE)[[1]]
    if (matches[1] > 0L) for (j in seq_along(matches)) {
      value <- substring(txt[i], matches[j] + attr(matches, "match.length")[j])
      side <- c(if (starts_team(value, aliases(i, "pos"))) "pos",
                if (starts_team(value, aliases(i, "def"))) "def")
      if (length(side) == 1L) {
        sides <- c(sides, side)
        proofs <- c(proofs, paste0("explicit_recovery_team=", side))
      }
    }
    list(sides = unique(sides), proof = paste(unique(proofs), collapse = ";"))
  }
  same_player <- function(i) {
    id1 <- get("fumble_player_id")[i]; id2 <- get("fumble_recovered_player_id")[i]
    valid_id <- function(x) !is.na(x) && has("^[0-9]+$", x) && !has("^0+$", x)
    if (valid_id(id1) && valid_id(id2)) return(identical(id1, id2))
    a <- norm(get("fumble_player_name")[i]); b <- norm(get("fumble_recovered_player_name")[i])
    # Full-name equality can establish the same person; a lone surname cannot.
    nzchar(a) && nzchar(b) && has("[a-z].* [a-z]", a) &&
      has("[a-z].* [a-z]", b) && key(a) == key(b)
  }
  identity_ok <- nzchar(pos) & nzchar(def) & key(pos) != key(def)
  for (i in seq_len(n)) {
    if (no_play[i]) {
      scrimmage[i] <- FALSE
      assign_result(i, 0L, "nullified_play", "explicit_no_play_or_penalty_no_play")
      next
    }
    if (replay[i]) {
      assign_result(i, NA, "replay_requires_final_event", "replay_or_reversal_text")
      next
    }
    if (administrative[i]) {
      if (fumble[i] || int_event[i])
        assign_result(i, NA, "administrative_event_conflict", "event_text_on_administrative_row")
      else assign_result(i, 0L, "administrative", paste0("play_type=", type[i]))
      next
    }
    if (type[i] == "penalty" && !fumble[i] && !int_event[i]) {
      scrimmage[i] <- FALSE
      assign_result(i, 0L, "penalty_only", "penalty_without_ball_event")
      next
    }
    if ((fumble[i] || int_event[i]) && penalty[i] &&
        (!penalty_declined[i] || penalty_conflict[i])) {
      assign_result(i, NA, "penalty_event_standing_unknown", "penalty_with_possible_giveaway")
      next
    }
    if ((fumble[i] && int_event[i]) ||
        lengths(regmatches(txt[i], gregexpr("\\bfumbled\\b", txt[i], perl = TRUE))) > 1L) {
      assign_result(i, NA, "multiple_possession_events", "cannot_encode_multiple_events_in_one_binary_value")
      next
    }
    if (special[i] && (fumble[i] || int_event[i])) {
      assign_result(i, NA, "special_teams_event_ownership", "return_or_kick_requires_event_team_attribution")
      next
    }
    if (fumble[i]) {
      if (!identity_ok[i]) {
        assign_result(i, NA, "missing_team_identity", "fumble_requires_distinct_pos_team_and_def_pos_team")
        next
      }
      rec <- recovery_side(i)
      own <- same_player(i) || has("\\brecover(?:s|ed|ing) (?:his|their|its) own fumble\\b", txt[i])
      if (length(rec$sides) > 1L || "unknown" %in% rec$sides ||
          (own && "def" %in% rec$sides) || (own_type[i] && "def" %in% rec$sides)) {
        assign_result(i, NA, "conflicting_recovery_evidence", rec$proof)
      } else if (own || "pos" %in% rec$sides) {
        assign_result(i, 0L, "own_recovery", if (own) "same_full_player_or_explicit_own_recovery" else rec$proof)
      } else if ("def" %in% rec$sides && isTRUE(scrimmage[i])) {
        assign_result(i, 1L, "explicit_opponent_recovery", rec$proof)
      } else if (own_type[i] && nzchar(txt[i]) &&
                 has("\\b(recovered by|out of bounds)\\b", txt[i])) {
        assign_result(i, 0L, "own_recovery_type", "own_recovery_type_and_recovery_text")
      } else {
        assign_result(i, NA, "fumble_recovery_unverified",
          paste0("play_type=", type[i], ";recovery_team_or_initial_play_unverified"))
      }
      next
    }
    if (int_event[i]) {
      sides <- interception_sides(i)
      if (negated_int[i] || type[i] == "pass incompletion") {
        assign_result(i, NA, "conflicting_interception_evidence", "interception_type_or_text_conflicts_with_incompletion")
      } else if ("pos" %in% sides) {
        assign_result(i, NA, "conflicting_interception_ownership",
                      paste0("explicit_intercepting_team=", paste(sides, collapse = "|")))
      } else if (identity_ok[i] && intercepted[i] && isTRUE(scrimmage[i])) {
        assign_result(i, 1L, "interception", "explicit_intercepted_pass")
      } else assign_result(i, NA, "interception_unverified", "missing_explicit_pass_or_team_identity")
      next
    }
    if (kneel[i]) {
      assign_result(i, 0L, "kneel_or_spike", "explicit_kneel_or_spike")
      next
    }
    if (special[i] && nzchar(txt[i])) {
      assign_result(i, 0L, "ordinary_special_teams", "kick_or_return_without_giveaway_semantics")
      next
    }
    if (type[i] %in% ordinary_types && nzchar(txt[i])) {
      assign_result(i, 0L, "ordinary_scrimmage", paste0("play_type=", type[i]))
    }
  }

  # Possession changes are not turnovers by definition. Unexplained changes or
  # turnover flags on ordinary plays are review signals, never positive labels.
  suspicious <- flag("turnover") | flag("turnover_indicator") | flag("change_of_pos_team")
  down <- suppressWarnings(as.numeric(get("down")))
  yards <- suppressWarnings(as.numeric(get("yards_gained")))
  distance <- suppressWarnings(as.numeric(get("distance")))
  failed_fourth <- !is.na(down) & down == 4 &
    (flag("downs_turnover") | (is.finite(yards) & is.finite(distance) & yards < distance))
  natural_change <- failed_fourth | flag("end_of_half") |
    type %in% c("passing touchdown", "rushing touchdown") |
    has("\\btouchdown\\b", txt)
  conflict <- which(reason == "ordinary_scrimmage" & suspicious & !natural_change)
  assign_result(conflict, NA, "unexplained_possession_change",
                "source_turnover_or_possession_flag_without_event_semantics")

  # Drive outcomes locate review candidates only. The last eligible row is not
  # relabeled as a fumble, and earlier plays never inherit the drive's giveaway.
  game <- get("game_id"); drive <- get("drive_id")
  seqno <- suppressWarnings(as.numeric(get("game_play_number")))
  result <- toupper(trimws(get("drive_result")))
  valid <- !is.na(game) & nzchar(game) & !is.na(drive) & nzchar(drive)
  relevant <- result %in% c("FUMBLE", "INT", "FUMBLE RETURN TD", "INT TD", "FUMBLE TD")
  keys <- paste(game, drive, sep = "\r")
  review_keys <- unique(keys[valid & relevant])
  if (length(review_keys)) {
    groups <- split(which(valid & keys %in% review_keys), keys[valid & keys %in% review_keys])
    for (idx in groups) {
      eligible <- idx[!administrative[idx] & !no_play[idx] & type[idx] != "penalty"]
      if (!length(eligible)) next
      outcomes <- unique(result[idx][!is.na(result[idx])])
      owners <- unique(pos[idx][nzchar(pos[idx])])
      if (length(outcomes) != 1L || length(owners) != 1L) {
        assign_result(eligible, NA, "conflicting_drive_metadata",
          paste0("drive_result=", paste(outcomes, collapse = "|"),
            ";drive_owners=", paste(owners, collapse = "|")))
        next
      }
      confirmed <- idx[!is.na(giveaway[idx]) & giveaway[idx] == 1L]
      expected_reason <- if (outcomes %in% c("INT", "INT TD"))
        "interception" else "explicit_opponent_recovery"
      if (length(confirmed)) {
        if (any(reason[confirmed] != expected_reason))
          assign_result(eligible, NA, "drive_event_type_conflict",
                        paste0("drive_result=", outcomes, ";confirmed_event_type_disagrees"))
        next
      }
      trustworthy <- all(is.finite(seqno[eligible])) && !anyDuplicated(seqno[eligible])
      candidate <- if (trustworthy) eligible[which.max(seqno[eligible])] else eligible
      candidate <- candidate[is.na(giveaway[candidate]) | giveaway[candidate] == 0L]
      why <- if (!trustworthy) "drive_sequence_unverified" else if (length(outcomes) != 1L)
        "conflicting_drive_metadata" else "drive_giveaway_missing_event"
      assign_result(candidate, NA, why,
        paste0("drive_result=", paste(outcomes, collapse = "|"), ";review_only_not_event_assignment"))
    }
  }
  lone_hint <- which(relevant & !valid & reason == "ordinary_scrimmage")
  assign_result(lone_hint, NA, "drive_identity_unverified", "drive_giveaway_hint_without_game_drive_identity")

  # Source columns and their values are retained; only these five columns append.
  out <- if (inherits(pbp, "data.table")) data.table::copy(pbp) else pbp
  out$clean_giveaway <- giveaway
  out$clean_scrimmage <- scrimmage
  out$clean_status <- ifelse(is.na(giveaway) | is.na(scrimmage), "unresolved", "resolved")
  out$clean_reason <- reason
  out$clean_evidence <- evidence
  out
}
