# Point-forecast injury scenarios. Source features.R and dashboard.R first.
# This module consumes explicit per-game estimates; it never estimates player value.
injury_overlay_time <- function(x, label) {
  value <- parse_utc_datetime(x)
  if (!length(value) || anyNA(value)) stop(label, " requires valid UTC timestamps.", call. = FALSE)
  value
}

injury_overlay_status <- function(x) {
  value <- tolower(trimws(as.character(x)))
  value[value %in% c("out for season", "out_for_season", "out-for-season", "season out", "season-ending")] <- "out"
  value[value %in% c("available", "cleared")] <- "active"
  if (anyNA(value) || any(!value %in% c("out", "active")))
    stop("This overlay supports confirmed out or active statuses only.", call. = FALSE)
  value
}

injury_overlay_card <- function(card, injury_as_of) {
  assert_columns(card, c("game_id", "season", "week", "home", "away", "as_of", "expected_margin"), "injury card")
  assert_unique_keys(card, "game_id", "injury card")
  cutoff <- injury_overlay_time(injury_as_of, "injury_as_of")
  if (length(cutoff) != 1L) stop("injury_as_of must be one timestamp.")
  baseline_time <- injury_overlay_time(card$as_of, "Card as_of")
  if (any(baseline_time > cutoff)) stop("Injury cutoff predates the baseline card.")
  if (any(!is.finite(card$expected_margin))) stop("Baseline margins must be finite.")
  if ("kickoff" %in% names(card) && any(injury_overlay_time(card$kickoff, "Kickoff") <= cutoff))
    stop("Injury overlay requires upcoming games.")
  cutoff
}

resolve_season_availability <- function(registry, card, injury_as_of) {
  cutoff <- injury_overlay_card(card, injury_as_of)
  empty <- data.frame(game_id = character(), season = integer(), team = character(),
    player_id = character(), player_name = character(), position = character(), starter = logical(),
    status = character(), source_type = character(), source_url = character(), source_timestamp = character(),
    through_week = numeric(), registry_status = character(), stringsAsFactors = FALSE)
  if (is.null(registry) || !nrow(registry)) return(empty)
  assert_columns(registry, c("season", "team", "status", "source_type", "source_url", "source_timestamp"), "availability registry")
  registry$source_timestamp <- injury_overlay_time(registry$source_timestamp, "Registry source_timestamp")
  # Historical resolution ignores evidence that was not yet available.
  registry <- registry[registry$source_timestamp <= cutoff, , drop = FALSE]
  if (!nrow(registry)) return(empty)
  registry$registry_status <- as.character(registry$status)
  registry$status <- injury_overlay_status(registry$status)
  registry$team <- canonical_team(registry$team)
  if (!"player_id" %in% names(registry)) registry$player_id <- NA_character_
  if (!"player_name" %in% names(registry)) registry$player_name <- NA_character_
  if (!"position" %in% names(registry)) registry$position <- ""
  if (!"starter" %in% names(registry)) registry$starter <- FALSE
  if (!"through_week" %in% names(registry)) registry$through_week <- NA_real_
  supplied_week <- !is.na(registry$through_week) & nzchar(trimws(as.character(registry$through_week)))
  registry$through_week <- suppressWarnings(as.numeric(registry$through_week))
  invalid_week <- !is.na(registry$through_week) & (!is.finite(registry$through_week) |
    registry$through_week < 0 | registry$through_week != floor(registry$through_week))
  if (any(invalid_week | (supplied_week & is.na(registry$through_week))))
    stop("Registry through_week must be a nonnegative whole number.")
  rows <- lapply(seq_len(nrow(card)), function(i) {
    take <- registry$season == card$season[i] & registry$team %in% canonical_team(c(card$home[i], card$away[i]))
    report <- registry[which(take), , drop = FALSE]
    if (!nrow(report)) return(NULL)
    report$game_id <- as.character(card$game_id[i])
    # Whitelisting prevents any old game-specific point estimate carrying forward.
    report <- report[intersect(names(empty), names(report))]
    report <- select_latest_injury_reports(report, cutoff)
    # Resolve latest status before expiration, so an old indefinite Out cannot
    # resurface when a newer, limited-duration report expires.
    report <- report[is.na(report$through_week) | report$through_week >= card$week[i], , drop = FALSE]
    report
  })
  rows <- Filter(function(x) !is.null(x) && nrow(x), rows)
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out$source_timestamp <- format(injury_overlay_time(out$source_timestamp, "Source timestamp"),
    "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rownames(out) <- NULL
  out
}

apply_injury_overlay <- function(card, reports, injury_as_of, config) {
  cutoff <- injury_overlay_card(card, injury_as_of)
  assert_columns(card, "market_home_spread", "point card")
  if (("injury_overlay_applied" %in% names(card) && any(card$injury_overlay_applied %in% TRUE)) ||
      ("injuries_adjusted" %in% names(card) && any(card$injuries_adjusted %in% TRUE)) ||
      ("injury_adjustment" %in% names(card) && any(is.finite(card$injury_adjustment) & card$injury_adjustment != 0)))
    stop("Refusing a second injury overlay; use the frozen baseline card.")
  probability_fields <- intersect(c("margin_sd", "home_win_probability", "home_cover_probability", "pick_cover_probability"), names(card))
  if (any(vapply(card[probability_fields], function(x) any(is.finite(suppressWarnings(as.numeric(x)))), logical(1))))
    stop("This overlay accepts point-only cards without probabilities or uncertainty estimates.")
  original_names <- names(card)
  baseline <- card$expected_margin
  output <- card
  output$baseline_expected_margin <- baseline
  output$injury_adjustment <- 0
  output$injuries_adjusted <- FALSE
  output$injury_overlay_applied <- TRUE
  output$injury_estimate_status <- "not_assessed"
  output$injury_as_of <- NA_character_
  if (!"foundation_expected_margin" %in% names(output)) output$foundation_expected_margin <- baseline
  defaults <- list(injury_scenario = "most_likely", pick_status = "article_pick",
    availability_note = "", availability_source = "", confidence_tier = NA_character_,
    reported_confidence = NA_character_, fair_spread = NA_real_, ats_edge_home = NA_real_,
    ats_pick = NA_character_, straight_up_pick = NA_character_)
  for (name in setdiff(names(defaults), names(output))) output[[name]] <- defaults[[name]]
  audit <- list()
  if (is.null(reports) || !nrow(reports)) return(list(predictions = output, reports = reports, audit = data.frame()))
  assert_columns(reports, c("game_id", "team", "status", "source_timestamp", "source_type", "source_url"), "injury reports")
  reports$game_id <- as.character(reports$game_id)
  report_time <- injury_overlay_time(reports$source_timestamp, "Report source_timestamp")
  if (any(report_time > cutoff)) stop("Future injury report supplied to the overlay.")
  if (any(!reports$game_id %in% as.character(card$game_id))) stop("Injury report game is not in this card.")
  reports$status <- injury_overlay_status(reports$status)
  reports$team <- canonical_team(reports$team)
  index <- match(reports$game_id, as.character(card$game_id))
  if (any(!reports$team %in% unique(canonical_team(c(card$home, card$away)))) ||
      any(!(reports$team == canonical_team(card$home[index]) | reports$team == canonical_team(card$away[index]))))
    stop("Injury report team does not match its game.")
  if (anyNA(reports$source_type) || any(!tolower(reports$source_type) %in% config$injuries$trusted_source_types))
    stop("Injury report source is outside the approved hierarchy.")
  resolved <- select_latest_injury_reports(reports, cutoff)
  if (any(table(resolved$game_id[resolved$status == "out"]) > 1L))
    stop("Multiple unavailable players in one game need an explicit joint estimate; overlapping effects cannot be summed.")
  if (!"impact_points" %in% names(resolved)) resolved$impact_points <- NA_real_
  resolved$impact_points <- suppressWarnings(as.numeric(resolved$impact_points))
  for (name in c("estimate_game_id", "baseline_as_of", "estimate_method", "estimate_source", "estimate_note"))
    if (!name %in% names(resolved)) resolved[[name]] <- NA_character_
  if (!"input_verified" %in% names(resolved)) resolved$input_verified <- FALSE
  for (j in seq_len(nrow(resolved))) {
    report <- resolved[j, , drop = FALSE]
    i <- match(report$game_id, as.character(card$game_id))
    status <- report$status
    player <- if (!is.na(report$player_name) && nzchar(report$player_name)) report$player_name else report$player_id
    delta <- 0; estimate_status <- "active_no_adjustment"; reason <- "Latest report is active; no absence adjustment."
    if (status == "out") {
      known <- is.finite(report$impact_points) && isTRUE(as.logical(report$input_verified))
      if (known) {
        if (is.na(report$estimate_game_id) || as.character(report$estimate_game_id) != report$game_id)
          stop("Estimate game ID is missing or does not match the injury game.")
        estimate_baseline <- injury_overlay_time(report$baseline_as_of, "Estimate baseline_as_of")
        if (estimate_baseline != injury_overlay_time(card$as_of[i], "Baseline as_of"))
          stop("Estimate baseline_as_of does not match the frozen card.")
        if ("conflict_flag" %in% names(report) && isTRUE(as.logical(report$conflict_flag))) known <- FALSE
      }
      if (known) {
        scenario_input <- report
        scenario_input$confidence <- 1 # Arithmetic multiplier, never a betting classification.
        scenario <- build_injury_scenarios(scenario_input, baseline[i], card$home[i], card$away[i], config)
        delta <- scenario$margin_adjustment[scenario$scenario == "most_likely"]
        if (length(delta) != 1L || !is.finite(delta)) stop("Invalid replacement scenario adjustment.")
        estimate_status <- "replacement_scenario"
        reason <- "Full signed player-versus-replacement estimate; supplemental scenario, not a learned injury coefficient."
        output$expected_margin[i] <- baseline[i] + delta
        output$injuries_adjusted[i] <- TRUE
        output$injury_scenario[i] <- "replacement_scenario"
        note <- sprintf("%s is out. Replacement scenario: %+.2f points to the home margin; model baseline unchanged in Components. %s", player, delta, reason)
      } else {
        delta <- NA_real_
        estimate_status <- "unestimated_confirmed_absence"
        reason <- "Confirmed absence; no usable fresh point estimate. Numerical baseline retained for review, not an injury-adjusted pick."
        output$injury_scenario[i] <- "confirmed_absence_unestimated"
        note <- paste0(player, " is out. ", reason)
      }
      if (!is.na(report$estimate_note) && nzchar(report$estimate_note)) note <- paste(note, report$estimate_note)
      output$pick_status[i] <- "injury_conflict_review"
      if (!"availability_note" %in% names(output)) output$availability_note <- ""
      previous_note <- ifelse(is.na(output$availability_note[i]), "", output$availability_note[i])
      output$availability_note[i] <- trimws(paste(previous_note, note))
      if (!"availability_source" %in% names(output)) output$availability_source <- ""
      output$availability_source[i] <- as.character(report$source_url)
      output$injury_adjustment[i] <- delta
    }
    # An active report for another player must not erase this game's Out state.
    if (status == "out" || output$injury_estimate_status[i] == "not_assessed") {
      output$injury_estimate_status[i] <- estimate_status
      output$injury_as_of[i] <- format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    audit[[j]] <- data.frame(game_id = report$game_id, team = report$team,
      player_id = report$player_id, player_name = player, status = status,
      source_timestamp = format(injury_overlay_time(report$source_timestamp, "Source timestamp"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      injury_as_of = format(cutoff, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      baseline_as_of = as.character(card$as_of[i]), estimate_game_id = as.character(report$estimate_game_id),
      estimate_method = as.character(report$estimate_method), estimate_source = as.character(report$estimate_source),
      input_verified = isTRUE(as.logical(report$input_verified)), impact_points = report$impact_points,
      baseline_expected_margin = baseline[i], injury_adjustment = delta,
      expected_margin = if (status == "out" && is.finite(delta)) baseline[i] + delta else baseline[i],
      estimate_status = estimate_status, reason = reason, stringsAsFactors = FALSE)
  }
  affected <- which(output$injury_estimate_status %in% c("replacement_scenario", "unestimated_confirmed_absence"))
  if (length(affected)) {
    output$fair_spread[affected] <- -output$expected_margin[affected]
    if ("fair_margin" %in% names(output)) output$fair_margin[affected] <- output$expected_margin[affected]
    output$ats_edge_home[affected] <- output$expected_margin[affected] + output$market_home_spread[affected]
    output$ats_pick[affected] <- ifelse(output$ats_edge_home[affected] >= 0, output$home[affected], output$away[affected])
    output$straight_up_pick[affected] <- ifelse(output$expected_margin[affected] >= 0, output$home[affected], output$away[affected])
    decorated <- prepare_dashboard_predictions(output[affected, , drop = FALSE])
    decorated$confidence_tier <- NA_character_; decorated$reported_confidence <- NA_character_
    for (name in names(decorated)) {
      if (!name %in% names(output)) output[[name]] <- decorated[[name]][rep(NA_integer_, nrow(output))]
      output[[name]][affected] <- decorated[[name]]
    }
  }
  unchanged <- setdiff(seq_len(nrow(card)), affected)
  if (length(unchanged)) {
    # CSV all-empty columns can be logical NA; one injury note promotes the
    # column to character. Preserve exact cell values and missingness, not that
    # incidental storage type. Numeric values still compare exactly.
    for (name in setdiff(original_names, c("injuries_adjusted")))
      if (!all((is.na(output[[name]][unchanged]) & is.na(card[[name]][unchanged])) |
          (!is.na(output[[name]][unchanged]) & !is.na(card[[name]][unchanged]) &
            output[[name]][unchanged] == card[[name]][unchanged])))
        stop("Overlay changed an unrelated card field: ", name)
  }
  if (!identical(output$foundation_expected_margin, if ("foundation_expected_margin" %in% names(card)) card$foundation_expected_margin else baseline))
    stop("Overlay changed the model baseline component.")
  list(predictions = output, reports = resolved, audit = do.call(rbind, audit))
}
