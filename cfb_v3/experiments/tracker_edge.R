edge_features <- function(with_v3 = FALSE) {
  c("sagarin_edge", "fpi_edge", "market_margin", if (with_v3) "v3_edge")
}

edge_inputs <- function(directory) {
  external_verify(directory)
  prepared <- readRDS(file.path(directory, "training_inputs.rds"))
  joined <- read.csv(file.path(directory, "joined_inputs.csv"), stringsAsFactors = FALSE)
  frozen <- read.csv(file.path(directory, "predictions.csv"), stringsAsFactors = FALSE)
  frozen <- frozen[frozen$model == "v3_frozen", ]
  fields <- c("game_id", "season", "week", "kickoff", "home", "away", "neutral_site",
    "postseason_type", "is_cfp", "home_conference", "away_conference", "margin",
    "closing_home_spread", "challenger_tracker_sagarin", "challenger_tracker_fpi")
  x <- prepared$data[fields]; x$game_id <- as.character(x$game_id)
  x$weight <- prepared$weights
  assert_unique_keys(x, "game_id", "edge training inputs")
  assert_unique_keys(joined, "game_id", "edge source inputs")
  assert_unique_keys(frozen, "game_id", "edge frozen v3")
  i <- match(x$game_id, as.character(joined$game_id))
  if (anyNA(i) || any(x$margin != joined$margin[i])) stop("Edge input identity mismatch.")
  for (field in c("tracker_opening_margin", "tracker_midweek_margin", "tracker_updated_margin"))
    x[[field]] <- joined[[field]][i]
  f <- match(x$game_id, as.character(frozen$game_id))
  x$v3_margin <- frozen$expected_margin[f]
  x <- x[is.finite(x$closing_home_spread) & is.finite(x$margin) & x$weight > 0, ]
  x$market_margin <- -x$closing_home_spread
  x$market_residual <- x$margin-x$market_margin
  x$sagarin_edge <- x$challenger_tracker_sagarin-x$market_margin
  x$fpi_edge <- x$challenger_tracker_fpi-x$market_margin
  x$v3_edge <- x$v3_margin-x$market_margin
  x$external_edge <- (x$sagarin_edge+x$fpi_edge)/2
  x$source_difference <- x$tracker_updated_margin-x$market_margin
  x$source_agrees <- is.finite(x$source_difference) & abs(x$source_difference) <= .5
  if (any(!is.finite(as.matrix(x[edge_features()])))) stop("Incomplete external inputs.")
  if (any(x$postseason_type == "non_cfp_bowl")) stop("Bowl in edge training.")
  test <- x$season %in% 2022:2025
  if (sum(test) != 3006 || any(!is.finite(x$v3_margin[test]))) stop("Primary cohort changed.")
  x
}

edge_fit <- function(data, kind, features = edge_features()) {
  if (!kind %in% c("ridge", "cover")) stop("Unknown edge model.")
  if (length(setdiff(features, edge_features(TRUE)))) stop("Prohibited edge feature.")
  assert_columns(data, c(features, "market_residual", "weight", "season"), "edge fit")
  keep <- is.finite(data$market_residual) & is.finite(data$weight) & data$weight > 0
  if (kind == "cover") keep <- keep & data$market_residual != 0
  d <- data[keep, ]; w <- d$weight/max(d$weight)
  if (nrow(d) < 100 || any(!is.finite(as.matrix(d[features])))) stop("Insufficient finite edge training data.")
  if (kind == "ridge") {
    model <- fit_weighted_ridge(d, "market_residual", features, w, lambda = 8)
    recipe <- model$recipe
  } else {
    recipe <- fit_numeric_recipe(d, features)
    z <- as.data.frame(bake_numeric_recipe(recipe, d))
    z$cover <- as.numeric(d$market_residual > 0)
    if (length(unique(z$cover)) < 2L) stop("Cover training needs both outcomes.")
    model <- stats::glm(stats::reformulate(features, "cover"), data = z, weights = w,
      family = stats::quasibinomial(), control = stats::glm.control(maxit = 100))
    if (!isTRUE(model$converged) || any(!is.finite(stats::coef(model)))) stop("Cover GLM failed to converge.")
  }
  list(kind = kind, features = features, recipe = recipe, model = model,
    train_through = max(d$season), training_rows = nrow(d))
}

edge_predict <- function(object, new_data) {
  assert_columns(new_data, c("season", object$features), "edge prediction")
  if (any(!is.finite(new_data$season)) || any(new_data$season <= object$train_through))
    stop("Edge prediction must follow the training seasons.")
  if (any(!is.finite(as.matrix(new_data[object$features])))) stop("Finite quote and external inputs required.")
  if (object$kind == "ridge") return(as.numeric(predict(object$model, new_data)))
  z <- as.data.frame(bake_numeric_recipe(object$recipe, new_data))
  as.numeric(stats::predict(object$model, z, type = "response"))
}

edge_walk <- function(data) {
  forecasts <- models <- coefficients <- list()
  variants <- list(residual_ridge = list(kind = "ridge", v3 = FALSE, years = 2022:2025),
    cover_glm = list(kind = "cover", v3 = FALSE, years = 2022:2025),
    stack_external = list(kind = "ridge", v3 = FALSE, years = 2023:2025),
    stack_plus_v3 = list(kind = "ridge", v3 = TRUE, years = 2023:2025))
  for (name in names(variants)) {
    v <- variants[[name]]; stack <- grepl("^stack_", name)
    eligible <- if (stack) data$season >= 2022 & is.finite(data$v3_edge) else rep(TRUE, nrow(data))
    for (year in v$years) {
      train <- data[eligible & data$season < year, ]
      test <- data[eligible & data$season == year, ]
      if (!nrow(test)) stop("Missing edge test season.")
      model <- edge_fit(train, v$kind, edge_features(v$v3))
      value <- edge_predict(model, test)
      test$model <- name
      test$signal <- if (v$kind == "cover") value-.5 else value
      test$expected_margin <- if (v$kind == "cover") NA_real_ else test$market_margin+value
      test$home_cover_probability <- if (v$kind == "cover") value else NA_real_
      test$cohort <- if (stack) "oof_v3_2023_2025" else "primary_2022_2025"
      key <- paste(name, year, sep = "_")
      forecasts[[key]] <- test; models[[key]] <- model
      beta <- if (v$kind == "cover") stats::coef(model$model) else model$model$coefficients
      coefficients[[key]] <- data.frame(model = name, test_season = year,
        train_through = model$train_through, training_rows = model$training_rows,
        term = names(beta), standardized_coefficient = unname(beta))
    }
  }
  list(predictions = do.call(rbind, forecasts), models = models, coefficients = do.call(rbind, coefficients))
}

edge_grade <- function(x, worse_points = 0) {
  if (length(worse_points) != 1L || !is.finite(worse_points) || worse_points < 0 || anyNA(x$eligible))
    stop("Invalid stress or eligibility.")
  x$side <- ifelse(abs(x$signal) < 1e-8, 0, sign(x$signal))
  x$selected <- x$eligible & is.finite(x$signal) & x$side != 0
  buffer <- x$side*x$market_residual-worse_points
  x$push <- x$selected & abs(buffer) < 1e-8
  x$win <- x$selected & !x$push & buffer > 0
  x$loss <- x$selected & !x$push & buffer < 0
  x$profit_minus110 <- ifelse(x$win, 10/11, ifelse(x$loss, -1, 0))
  x$worse_points <- worse_points
  x
}

edge_policies <- function(data, forecasts) {
  test <- data[data$season %in% 2022:2025, ]; policies <- list()
  add <- function(x, name, eligible = rep(TRUE, nrow(x))) {
    x$policy <- name; x$eligible <- eligible
    policies[[name]] <<- edge_grade(x)
  }
  for (name in c("v3", "sagarin", "external")) {
    x <- test; x$cohort <- "primary_2022_2025"; x$model <- name
    x$signal <- x[[paste0(name, "_edge")]]
    x$expected_margin <- x$market_margin+x$signal; x$home_cover_probability <- NA_real_
    add(x, paste0(name, "_all"))
    if (name != "sagarin") add(x, paste0(name, "_edge3"), abs(x$signal) >= 3)
    if (name == "external") {
      same <- sign(x$v3_edge) == sign(x$sagarin_edge) & sign(x$v3_edge) == sign(x$fpi_edge)
      strong <- pmin(abs(x$v3_edge), abs(x$sagarin_edge), abs(x$fpi_edge)) >= 2
      add(x, "unanimous_edge2", same & strong)
    }
  }
  for (name in unique(forecasts$model)) {
    x <- forecasts[forecasts$model == name, ]; add(x, paste0(name, "_all"))
    if (name == "residual_ridge") add(x, "residual_ridge_edge2", abs(x$signal) >= 2)
    if (name == "cover_glm") add(x, "cover_glm_p55", abs(x$signal) >= .05)
  }
  do.call(rbind, policies)
}

edge_mean <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_

edge_summary <- function(x) {
  wins <- sum(x$win); losses <- sum(x$loss); picks <- sum(x$selected)
  decisive <- wins+losses; nonpush <- x$market_residual != 0
  prob <- x$home_cover_probability; y <- as.numeric(x$market_residual > 0)
  if (any(x$worse_points > 0)) prob[] <- NA_real_
  p <- pmax(1e-8, pmin(1-1e-8, prob))
  data.frame(games = nrow(x), picks = picks, wins = wins, losses = losses, pushes = sum(x$push),
    passes = nrow(x)-picks, ats = if (decisive) wins/decisive else NA_real_,
    units_minus110 = sum(x$profit_minus110),
    roi_minus110 = if (picks) sum(x$profit_minus110)/picks else NA_real_,
    mae = edge_mean(abs(x$expected_margin-x$margin)),
    brier = edge_mean((prob[nonpush]-y[nonpush])^2),
    log_loss = edge_mean(-y[nonpush]*log(p[nonpush])-(1-y[nonpush])*log1p(-p[nonpush])))
}

edge_metrics <- function(policies) {
  result <- list()
  for (name in unique(policies$policy)) {
    x <- policies[policies$policy == name, ]
    slices <- list(all = rep(TRUE, nrow(x)), source_agrees = x$source_agrees,
      week_0_1 = x$week <= 1 & !x$is_cfp, weeks_2_4 = x$week >= 2 & x$week <= 4 & !x$is_cfp,
      week_5_plus = x$week >= 5 & !x$is_cfp, cfp = x$is_cfp,
      spread_0_7 = abs(x$market_margin) <= 7,
      spread_7_21 = abs(x$market_margin) > 7 & abs(x$market_margin) <= 21,
      spread_over_21 = abs(x$market_margin) > 21,
      favorite = x$side*sign(x$market_margin) > 0,
      underdog = x$side*sign(x$market_margin) < 0, pickem = x$market_margin == 0)
    for (year in sort(unique(x$season))) slices[[paste0("season_", year)]] <- x$season == year
    for (slice in names(slices)) {
      d <- x[which(slices[[slice]]), ]; if (!nrow(d)) next
      result[[length(result)+1L]] <- cbind(data.frame(cohort = x$cohort[1], policy = name, slice = slice), edge_summary(d))
    }
  }
  do.call(rbind, result)
}

edge_uncertainty <- function(policies) {
  result <- omissions <- list()
  family <- 12L
  stopifnot(length(unique(policies$policy)) == family)
  for (name in unique(policies$policy)) {
    x <- policies[policies$policy == name, ]; s <- edge_summary(x); n <- s$wins+s$losses
    ci <- if (n) stats::binom.test(s$wins, n, conf.level = 1-.05/family)$conf.int else c(NA_real_, NA_real_)
    roi <- experiment_season_interval(x$profit_minus110[x$selected], x$season[x$selected])
    result[[name]] <- data.frame(cohort = x$cohort[1], policy = name,
      adjusted_ats_low = ci[1], adjusted_ats_high = ci[2], season_roi_low = roi[1], season_roi_high = roi[2],
      adjusted_lower_above_minus110 = is.finite(ci[1]) && ci[1] > 11/21)
    for (year in sort(unique(x$season))) omissions[[paste(name, year)]] <- cbind(
      data.frame(cohort = x$cohort[1], policy = name, omitted_season = year), edge_summary(x[x$season != year, ]))
  }
  list(uncertainty = do.call(rbind, result), leave_one_season_out = do.call(rbind, omissions))
}

edge_diagnostics <- function(data, forecasts) {
  test <- data[data$season %in% 2022:2025, ]; curves <- slopes <- list()
  for (name in c("v3", "sagarin", "fpi", "external")) {
    signal <- test[[paste0(name, "_edge")]]
    bin <- cut(abs(signal), c(-Inf, 1, 3, 7, Inf), labels = c("0_1", "1_3", "3_7", "over_7"))
    for (group in levels(bin)) {
      i <- which(bin == group); side <- sign(signal[i]); decisive <- test$market_residual[i] != 0 & side != 0
      curves[[paste(name, group)]] <- data.frame(model = name, edge_bin = group, games = length(i),
        mean_claimed_edge = edge_mean(abs(signal[i])),
        mean_realized_cushion = edge_mean(side*test$market_residual[i]),
        decisions = sum(decisive), ats = edge_mean(as.numeric(side[decisive]*test$market_residual[i][decisive] > 0)))
    }
    for (year in c(0L, 2022:2025)) {
      i <- if (year == 0L) seq_len(nrow(test)) else which(test$season == year)
      fit <- stats::lm(y ~ e, data = data.frame(y = test$market_residual[i], e = signal[i]))
      slopes[[paste(name, year)]] <- data.frame(model = name, season = if (year == 0) "all" else as.character(year),
        games = length(i), correlation = stats::cor(signal[i], test$market_residual[i]),
        descriptive_edge_slope = unname(stats::coef(fit)[2]))
    }
  }
  x <- forecasts[forecasts$model == "cover_glm" & forecasts$market_residual != 0, ]
  x$bin <- cut(x$home_cover_probability, c(-Inf, .4, .45, .5, .55, .6, Inf))
  calibration <- do.call(rbind, lapply(split(x, x$bin), function(d) data.frame(
    bin = as.character(d$bin[1]), games = nrow(d), mean_probability = edge_mean(d$home_cover_probability),
    observed_home_cover = edge_mean(as.numeric(d$market_residual > 0)))))
  list(edge_calibration = do.call(rbind, curves), residual_association = do.call(rbind, slopes),
    probability_calibration = calibration)
}

edge_quote_audit <- function(data, policies) {
  d <- data[data$season %in% 2022:2025, ]; audit <- d[c("game_id", "season", "home", "away", "neutral_site",
    "market_margin", "tracker_opening_margin", "tracker_midweek_margin", "tracker_updated_margin", "source_difference", "source_agrees")]
  counts <- do.call(rbind, lapply(sort(unique(d$season)), function(year) {
    x <- d[d$season == year, ]; gap <- abs(x$source_difference)
    data.frame(season = year, games = nrow(x), updated_present = sum(is.finite(gap)),
      identical = sum(gap < 1e-8, na.rm = TRUE), within_half = sum(gap <= .5, na.rm = TRUE),
      over_three = sum(gap > 3, na.rm = TRUE), median_absolute_gap = stats::median(gap, na.rm = TRUE))
  }))
  # Counterfactual quote comparison applies only to raw margin systems, never late-line learners.
  quotes <- list(canonical = "market_margin", tracker_opening = "tracker_opening_margin",
    tracker_midweek = "tracker_midweek_margin", tracker_updated = "tracker_updated_margin")
  comparable <- Reduce(`&`, lapply(d[unlist(quotes)], is.finite))
  sensitivity <- list()
  for (quote in names(quotes)) for (name in c("v3", "sagarin", "external")) {
    x <- d[comparable, ]; old_margin <- x$market_margin+x[[paste0(name, "_edge")]]
    x$market_margin <- x[[quotes[[quote]]]]; x$market_residual <- x$margin-x$market_margin
    x$signal <- old_margin-x$market_margin; x$expected_margin <- old_margin
    x$home_cover_probability <- NA_real_; x$eligible <- TRUE
    sensitivity[[paste(quote, name)]] <- cbind(data.frame(quote = quote, model = name), edge_summary(edge_grade(x)))
  }
  list(market_audit = audit, market_summary = counts, raw_quote_counterfactual = do.call(rbind, sensitivity))
}

edge_pbp_trace <- function(project, data) {
  result <- list()
  for (year in 2022:2025) {
    x <- data[data$season == year, c("game_id", "season", "home", "away", "closing_home_spread")]
    if (!nrow(x)) next
    path <- file.path(project, "cfb_v2/cache/pbp", paste0("pbp_", year, "_compact.rds"))
    x$pbp_file <- path; x$pbp_sha256 <- NA_character_
    x$pbp_home <- x$pbp_away <- NA_character_
    x$pbp_spread <- NA_real_; x$distinct_quotes <- NA_integer_
    x$trace_status <- "cache_unavailable"
    if (file.exists(path)) {
      p <- readRDS(path)
      assert_columns(p, c("game_id", "home", "away", "spread"), "PBP quote trace")
      q <- unique(p[c("game_id", "home", "away", "spread")]); rm(p)
      q$game_id <- as.character(q$game_id)
      q <- q[is.finite(q$spread), ]; i <- match(x$game_id, q$game_id)
      x$pbp_sha256 <- digest::digest(file = path, algo = "sha256")
      x$pbp_home <- canonical_team(q$home[i]); x$pbp_away <- canonical_team(q$away[i])
      x$pbp_spread <- q$spread[i]
      n <- table(q$game_id); x$distinct_quotes <- as.integer(n[match(x$game_id, names(n))])
      same <- x$home == x$pbp_home & x$away == x$pbp_away
      reverse <- x$home == x$pbp_away & x$away == x$pbp_home
      x$trace_status <- ifelse(is.na(i), "no_finite_pbp_quote", ifelse(reverse, "reversed_source_sides",
        ifelse(!same, "team_mismatch", ifelse(abs(x$closing_home_spread-x$pbp_spread) < 1e-8,
          "matches_cached_pbp_quote", "differs_from_cached_pbp_quote"))))
    }
    result[[as.character(year)]] <- x
  }
  do.call(rbind, result)
}

edge_report <- function(output, metrics, uncertainty, stress, diagnostics, quote) {
  display <- function(x) {
    for (name in intersect(c("ats", "roi_minus110"), names(x))) x[[name]] <- round(100*x[[name]], 2)
    for (name in intersect(c("mae", "brier", "log_loss"), names(x))) x[[name]] <- round(x[[name]], 4)
    markdown_table(x)
  }
  writeLines(c("# Market-edge investigation", "", "## Status", "",
    "Exploratory only. All twelve declared policies reported; no production promotion.",
    "The primary historical quote and tracker projections lack verified bookmaker/publication times.",
    "Market-aware fits are late-line diagnostics, NOT executable Friday bets or opening-line strategies.", "",
    "## All policies", "", display(metrics[metrics$slice == "all", ]), "",
    "ATS and hypothetical ROI columns are percentages. ROI assumes flat risk at -110; actual prices are unavailable.",
    "The OOF-v3 stack has a different 2023-2025 cohort: compare its two rows to each other, not the primary denominator.",
    "MAE and probability losses score all available forecasts in a slice, not only selected wagers. A cover GLM has no margin/SU prediction.", "",
    "## Seasons", "", display(metrics[grepl("^season_", metrics$slice), ]), "",
    "## Uncertainty", "", markdown_table(uncertainty), "",
    "Intervals above are proportions, not percentages. Binomial intervals adjust across twelve policies but assume independent games.",
    "ROI intervals resample whole seasons. Four or fewer seasons and historical reuse limit inference.", "",
    "## Worse-line stress", "", display(stress), "",
    "Same picks, no new selection. These are hypothetical 0.5/1-point worse handicaps at unchanged -110 prices, not quoted markets.", "",
    "## Source agreement", "", display(metrics[metrics$slice == "source_agrees", ]), "",
    markdown_table(quote$market_summary), "",
    "The canonical column is named closing_home_spread, but source identity and exact closing availability are not independently verified.",
    "No quote was replaced to improve results. See market_audit.csv for individual discrepancies.", "",
    "## Raw-model quote counterfactual", "", display(quote$raw_quote_counterfactual), "",
    "Identical four-quote cohort. Ratings may have appeared AFTER opening/midweek prices disappeared; this is not achievable ROI or verified CLV.", "",
    "## Edge calibration", "", markdown_table(diagnostics$edge_calibration), "",
    "A large claimed edge need not produce a large realized cushion. Bins are descriptive, not newly selected betting rules.", "",
    markdown_table(diagnostics$residual_association), "",
    "Slopes/correlations are retrospective diagnostics, not coefficients fitted for historical picks.", "",
    "## Cover-probability calibration", "", markdown_table(diagnostics$probability_calibration), "",
    "Probabilities condition on a non-push; evaluate Brier/log loss against the 0.5 baseline (0.25/log(2)).", "",
    "## Reproduction", "", "Run Rscript run_cfb_tracker_edge.R with the sealed source caches present.",
    "See PROTOCOL.md, per-game policies/predictions, fold_coefficients.csv, fold_models.rds, leave_one_season_out.csv and checksums.csv.",
    "Production/input hashes are checked before/after; no actual-v3 forecast, coach mapping, roster fade or card is changed."),
    file.path(output, "REPORT.md"))
}

run_tracker_edge <- function(project) {
  config <- cfb_v2_config(project, 2026L); before <- experiment_file_hashes(config)
  root <- file.path(project, "cfb_v3/experiments")
  code <- c(file.path(root, c("tracker_edge.R", "TRACKER_EDGE_PROTOCOL.md", "controlled_ats.R", "external_ratings.R")),
    file.path(project, "run_cfb_tracker_edge.R"))
  hashes <- vapply(code, function(path) digest::digest(file = path, algo = "sha256"), "")
  source_dir <- file.path(config$output_dir, "experiments/prediction_tracker/20260912T003231.716Z")
  output <- external_new_dir(file.path(config$output_dir, "experiments/tracker_edge"), "")
  message("Edge investigation: ", output)
  jsonlite::write_json(list(status = "started", automatic_promotion = FALSE), file.path(output, "manifest.json"), auto_unbox = TRUE)
  file.copy(file.path(root, "TRACKER_EDGE_PROTOCOL.md"), file.path(output, "PROTOCOL.md"))
  write.csv(before, file.path(output, "protected_before.csv"), row.names = FALSE)
  write.csv(data.frame(path = code, sha256 = hashes), file.path(output, "code_hashes.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(output, "session_info.txt"))
  data <- edge_inputs(source_dir)
  message("Tracing historical quotes to the retained PBP source")
  pbp_trace <- edge_pbp_trace(project, data)
  message("Fitting past-only market residual and cover models")
  fit <- edge_walk(data); policies <- edge_policies(data, fit$predictions)
  stopifnot(!anyDuplicated(policies[c("game_id", "policy")]), all(fit$coefficients$train_through < fit$coefficients$test_season))
  metrics <- edge_metrics(policies); uncertainty <- edge_uncertainty(policies)
  stress <- do.call(rbind, lapply(c(.5, 1), function(points) {
    x <- edge_metrics(edge_grade(policies, points)); cbind(worse_points = points, x[x$slice == "all", ])
  }))
  diagnostics <- edge_diagnostics(data, fit$predictions); quote <- edge_quote_audit(data, policies)
  outputs <- c(list(inputs = data, pbp_quote_trace = pbp_trace, predictions = fit$predictions, policies = policies,
    metrics = metrics, fold_coefficients = fit$coefficients, line_stress = stress), uncertainty, diagnostics, quote)
  for (name in names(outputs)) write.csv(outputs[[name]], file.path(output, paste0(name, ".csv")), row.names = FALSE)
  saveRDS(fit$models, file.path(output, "fold_models.rds"))
  edge_report(output, metrics, uncertainty$uncertainty, stress, diagnostics, quote)
  after <- experiment_file_hashes(config)
  write.csv(after, file.path(output, "protected_after.csv"), row.names = FALSE)
  if (!identical(before, after)) stop("Protected production files changed.")
  if (!identical(hashes, vapply(code, function(path) digest::digest(file = path, algo = "sha256"), ""))) stop("Code changed during run.")
  jsonlite::write_json(list(status = "complete", automatic_promotion = FALSE, production_unchanged = TRUE,
    source_directory = source_dir, source_seal_sha256 = digest::digest(file = file.path(source_dir, "checksums.csv"), algo = "sha256"),
    primary_games = sum(data$season %in% 2022:2025), policies = length(unique(policies$policy)),
    evidence = "exploratory_late_line_publisher_archive"), file.path(output, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
  external_seal(output)
  list(output = output, summary = metrics[metrics$slice == "all", ])
}
