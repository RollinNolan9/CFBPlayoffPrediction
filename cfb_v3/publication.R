# Reprice a frozen card without refitting or changing its margin projections.
reprice_publication_card <- function(card, lines, bundle) {
  stopifnot(!anyDuplicated(card$game_id), !anyDuplicated(lines$game_id))
  hit <- match(card$game_id, lines$game_id)
  if (anyNA(hit)) stop("Fresh DraftKings quotes are missing for part of the card.")
  quote <- lines[hit, , drop = FALSE]
  if (any(!is.finite(quote$home_spread)) || any(!nzchar(quote$captured_at))) {
    stop("Every publication quote needs a finite spread and a capture timestamp.")
  }
  out <- card
  new <- make_game_picks(
    card, card, quote$home_spread, force_pick = card$forced_pick,
    ats_threshold = bundle$ats_threshold, ats_model = bundle$ats_model,
    config = bundle$config
  )
  market_fields <- c("market_home_spread", "ats_edge_home", "home_cover_probability",
                     "ats_pick", "pick_status", "confidence_tier")
  for (field in market_fields) out[[field]] <- new[[field]]
  # Repricing must not remove an existing availability or transition restriction.
  protected <- card$pick_status %in% c("injury_conflict_review", "transition_review")
  out$pick_status[protected] <- card$pick_status[protected]
  out$reported_confidence <- ifelse(card$reported_confidence == "provisional",
                                    "provisional", out$confidence_tier)
  out$market_provider <- "DraftKings via CFBD"
  out$market_captured_at <- quote$captured_at
  out$market_total <- quote$total
  out$opening_home_spread <- quote$opening_home_spread
  out$draftkings_home_spread <- quote$home_spread
  out$fanduel_home_spread <- NA_real_
  out$market_source <- "https://api.collegefootballdata.com/lines"
  out$ats_calibrated <- !is.null(bundle$ats_model)
  out$previous_market_home_spread <- card$market_home_spread
  out$previous_ats_pick <- card$ats_pick
  out$previous_market_captured_at <- card$market_captured_at
  out$market_change_note <- paste0(
    "Previous: ", dashboard_pick_text(card$ats_pick, card$home, card$market_home_spread),
    ". Refreshed: ", dashboard_pick_text(out$ats_pick, out$home, out$market_home_spread), "."
  )
  stopifnot(identical(out$expected_margin, card$expected_margin),
            identical(out$margin_sd, card$margin_sd),
            identical(out$straight_up_pick, card$straight_up_pick))
  prepare_dashboard_predictions(out)
}
