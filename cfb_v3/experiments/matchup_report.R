matchup_report_table <- function(x) {
  for(field in names(x))if(is.numeric(x[[field]]))x[[field]] <- round(x[[field]],4)
  markdown_table(x)
}

matchup_foundation_policies <- function(directory,profile) {
  x <- read.csv(file.path(directory,"policies.csv"),stringsAsFactors=FALSE)
  x$signal <- x$expected_margin-x$market_margin
  x$eligible <- x$pick_side!=0; x$home_cover_probability <- NA_real_
  x$profile <- profile
  edge_grade(x)
}

run_matchup_report <- function(project,artifacts) {
  required <- c("prepared","research","diagnostics","deduplicated_prepared","deduplicated_research",
    "deduplicated_diagnostics","duplicate_sources","foundation_duplicates","turnover","v3_turnover",
    "placebo","play_contract","turnover_checks","box_audit","synthetic","current_contract",
    "event_checks","drive_audit","verification")
  if(!setequal(names(artifacts),required))stop("Incomplete research report provenance.")
  for(directory in artifacts)external_verify(directory)
  config <- cfb_v2_config(project,2026L); protected <- experiment_file_hashes(config)
  output <- external_new_dir(file.path(project,"cfb_v3/output/experiments/matchup_report"),"")
  read <- function(name,file)read.csv(file.path(artifacts[[name]],file),stringsAsFactors=FALSE)
  policy_profiles <- list(original=read("research","policies.csv"),
    duplicates_removed=read("deduplicated_research","policies.csv"),
    fixed_football_duplicates=matchup_foundation_policies(artifacts$foundation_duplicates,"fixed_football_duplicates"),
    fixed_football_turnover=matchup_foundation_policies(artifacts$turnover,"fixed_football_turnover"))
  v3 <- read("v3_turnover","policies.csv")
  policy_profiles$full_v3_turnover <- v3[v3$lane=="qualified_quotes", ]
  all <- do.call(bind_rows_fill,lapply(names(policy_profiles),function(name) {
    x <- policy_profiles[[name]]; x$profile <- name; x
  }))
  signal <- all$expected_margin+all$closing_home_spread
  side <- ifelse(abs(signal)<1e-8,0,sign(signal))
  selected <- all$eligible & is.finite(signal) & side!=0
  cover <- side*(all$margin+all$closing_home_spread)
  push <- selected & abs(cover)<1e-8
  win <- selected & !push & cover>0
  loss <- selected & !push & cover<0
  if(anyNA(c(win,loss,push,selected)) || !identical(win,all$win) || !identical(loss,all$loss) ||
    !identical(push,all$push) || !identical(selected,all$selected))stop("Independent score/line regrade disagrees.")
  write.csv(data.frame(policy_rows=nrow(all),score_line_regrade_equal=TRUE),
    file.path(output,"independent_regrade.csv"),row.names=FALSE)
  family <- length(unique(paste(all$profile,all$policy)))
  joint <- do.call(rbind,lapply(split(all,paste(all$profile,all$policy)),function(x) {
    s <- matchup_summary(x); n <- s$wins+s$losses
    ci <- if(n)binom.test(s$wins,n,conf.level=1-.05/family)$conf.int else c(NA_real_,NA_real_)
    cbind(profile=x$profile[1],policy=x$policy[1],s,
      family_size=family,nominal_family_ats_low=ci[1],nominal_family_ats_high=ci[2])
  }))
  write.csv(joint,file.path(output,"all_policy_profiles.csv"),row.names=FALSE)
  roots <- joint[!joint$profile %in% c("original","duplicates_removed"), ]
  annual <- do.call(rbind,lapply(names(policy_profiles),function(name)
    cbind(profile=name,matchup_metrics(policy_profiles[[name]]))))
  write.csv(annual,file.path(output,"all_profile_slices.csv"),row.names=FALSE)
  changes <- list()
  for(name in c("fixed_football_duplicates","fixed_football_turnover","full_v3_turnover")) {
    x <- policy_profiles[[name]]
    old <- x[grepl("original_all$",x$policy), ]
    new <- x[grepl("cleaned_all$|corrected_all$",x$policy), ]
    if(!nrow(old) || nrow(old)!=nrow(new))stop("Root-cause paired population mismatch.")
    i <- match(old$game_id,new$game_id)
    if(anyNA(i) || any(old$margin!=new$margin[i]))stop("Root-cause paired scores mismatch.")
    delta <- abs(new$expected_margin[i]-old$margin)-abs(old$expected_margin-old$margin)
    ci <- experiment_season_interval(delta,old$season)
    changes[[name]] <- data.frame(profile=name,games=nrow(old),mae_change=mean(delta),
      season_block_low=ci[1],season_block_high=ci[2],
      average_forecast_change=mean(abs(new$expected_margin[i]-old$expected_margin)),
      largest_forecast_change=max(abs(new$expected_margin[i]-old$expected_margin)),
      changed_ats_sides=sum(old$side!=new$side[i]))
  }
  root_changes <- do.call(rbind,changes)
  write.csv(root_changes,file.path(output,"root_cause_paired.csv"),row.names=FALSE)
  primary <- joint[joint$profile=="original", ]
  market <- read("research","market_reference.csv")
  placebo <- read("placebo","comparison.csv")
  duplicate <- read("duplicate_sources","duplicate_source_audit.csv")
  flag <- read("turnover","flag_source_audit.csv")
  witness <- read("turnover_checks","box_score_checks.csv")
  boxes <- read("box_audit","season_reconciliation.csv")
  synthetic <- read("synthetic","synthetic_comparison.csv")
  current <- read("current_contract","counts.csv")
  associations <- read("event_checks","association_coverage.csv")
  verification <- read("verification","test_results.csv")
  raw_replay <- read("verification","raw_input_comparison.csv")
  if(any(verification$status!="passed") || !all(raw_replay$exactly_equal))stop("Verification did not pass.")
  quotes <- read.csv(file.path(project,"cfb_v3/experiments/matchup_public_quote_checks.csv"),stringsAsFactors=FALSE)
  models <- primary[grepl("_all$",primary$policy), ]
  display <- function(x) {
    x$ats_pct <- 100*x$ats; x$su_pct <- 100*x$su_accuracy
    matchup_report_table(x[c("policy","games","picks","wins","losses","pushes","ats_pct","mae","su_pct","units_minus110")])
  }
  root_display <- roots; root_display$policy <- paste(root_display$profile,root_display$policy,sep=": ")
  cfp <- annual[annual$profile=="original" & annual$slice=="cfp" &
    annual$policy %in% c("v3_all","external_all","all_matchups_all"), ]
  v3metrics <- read("v3_turnover","metrics.csv")
  linecheck <- read("diagnostics","same_games_old_vs_provider_quotes.csv")
  source_matches <- sum(is.finite(quotes$published_home_spread) & quotes$published_home_spread==quotes$selected_home_spread)
  lines <- c("# Matchup Research And Data-Quality Audit", "",
    "## Decision", "",
    "No model promotion. These reused historical samples do not establish a betting advantage.",
    "The matchup additions fail the margin-error and betting checks. Two independently verified",
    "data defects justify cleanup work, but correctness is not evidence of profitable ATS forecasting.", "",
    "Production engine, cached production datasets, fitted backtest files and published cards remain unchanged.", "",
    "## Common Historical Games", "",
    "2,955 FBS-vs-FBS games in 2022-2025; 2020-2021 warm-up. Non-CFP bowls excluded.",
    "Named-book quotes pass side/score/format checks and have another bookmaker family within one point.",
    "This is provider corroboration, NOT proof of independently timestamped Friday execution or true closing prices.",
    sprintf("The market reference itself has margin MAE %.5f points and no ATS selections.",market$mae), "",
    display(models), "",
    "MAE and SU use every game in a policy's evaluation cohort, not only its selected bets.",
    "ATS excludes pushes and zero-edge passes. Units assume risking one unit per selection at uniform -110;",
    "actual spread prices are unavailable. These are illustrations, not realized betting returns.", "",
    "## Every Fixed Primary Policy", "", display(primary), "",
    "No post-result threshold optimization or cherry-picked subgroup promotion. The seven models",
    "use the same rows, training-only standardization, prior-season-only training, and fixed penalty grid.",
    "First outer fold uses 128; later folds choose among 32/128/512 on earlier outer-fold MAE only.", "",
    "## Negative Controls", "",
    matchup_report_table(placebo[c("model","actual_mae","placebo_mean_mae","fraction_placebos_lower_mae")]), "",
    "499 seeded joint feature-vector shuffles within season/week/CFP blocks. Every iteration refits",
    "the same chronological models and repeats the same past-only penalty choice. The external baseline",
    "is invariant. These are descriptive negative controls, not calibrated conditional p-values:",
    "permutation breaks associations with fixed team strength. Synthetic positive-control tests",
    "confirm the fitting pipeline can recover a deliberately injected persistent matchup effect.", "",
    "### Synthetic Method Sensitivity", "",
    matchup_report_table(synthetic), "",
    "These are SIMULATED outcomes, not additional historical bets. Sixteen fixed residual permutations",
    "are reused at each injected effect size; feature scale comes only from the 2020-2021 warm-up.",
    "A two-point injected effect improves style versus its linear control in all 16 draws. A one-point",
    "effect is less consistently useful, especially against the simpler external/market control.",
    "This supports method sensitivity to an injected effect, not the existence of a real football edge",
    "or a calibrated power estimate. The real-data matchup results remain negative.", "",
    "## Proven Data Defects", "",
    "### Exact Upstream Duplicates", "",
    matchup_report_table(duplicate[c("season","full_rows","exact_full_duplicates","normalized_cache_identical")]), "",
    "Full identity-rich upstream rows reproduce the normalized cache exactly before any removal.",
    "5,177 full-row duplicates are verified across all six seasons, mostly 2021. Numeric play IDs",
    "also collide on distinct plays, so deduplicating on IDs or compact rows alone is unsafe.",
    "The duplicate-only football audit reproduces the original features and forecasts before refitting.",
    "Opponent-adjusted power is game-score-derived and remains identical; source efficiency rates change.", "",
    "### Turnover-Field Precedence", "",
    "The original code prefers finite turnover_indicator over turnover. In these source files the",
    "former frequently remains zero for ordinary interceptions and lost fumbles. A play-type gate",
    "cannot repair that zero. The isolated correction prefers turnover, retaining the existing",
    "scrimmage/type filter and fallback only for unavailable turnover values. Havoc uses that same",
    "resolved flag, so both turnover and havoc features are rebuilt, including prior-only smoothing.", "",
    matchup_report_table(flag[c("season","profile","scrimmage","included_giveaways","havoc")]), "",
    matchup_report_table(witness[c("season","team","official_turnovers","legacy_all_game_scrimmage","corrected_all_game_scrimmage")]), "",
    "Four convenience box-score witnesses are not a representative validation sample. Some corrected",
    "counts still do not equal official totals. Do NOT assume every discrepancy is garbage time:",
    "LSU-Clemson 2025 includes a Chris Hilton Jr. lost fumble labeled Pass Reception, which the",
    "existing giveaway-type gate still excludes. Separately, 181 eligible finite-EPA 2025 opponent",
    "fumble-recovery rows have neither a pass nor rush flag and remain outside the scrimmage gate.",
    "These classification defects are documented separately; no silent text-based repair was folded",
    "into the tested turnover-precedence candidate. Source completeness and return-only rows need review.", "",
    "### Broader Box-Score Check", "",
    matchup_report_table(boxes), "",
    "Fixed data-quality slices: 2022-2025 regular Weeks 1 and 8, plus postseason Week 1.",
    "The eligible FBS/non-bowl filter leaves 888 team-games. This is not full-season coverage.",
    "Reported turnovers total 1,226; legacy finite-EPA scrimmage counts capture 71 and the",
    "flag-only candidate captures 1,100. Exact team-game agreement rises from 264 to 721.",
    "133 corrected counts remain below the box total and 34 exceed it; no sampled team has missing PBP.",
    "Box totals include special teams, while these counts retain the scrimmage/finite-EPA gate.",
    "The overcounts show why the source field named turnover is not an independently verified truth.",
    "For Michigan-East Carolina 2023, the source labels McCarthy's own fumble recovery as",
    "Fumble Recovery (Opponent) and sets turnover=1, despite Michigan's reported zero turnovers.",
    "No text-based relabeling or replacement with box totals was installed. The sealed audit",
    "retains source identity/score/stat validation, duplicate quarantine, and a discrepant-play review queue.",
    "CFBD and ESPN may share underlying providers; matching them is not independent play-by-play corroboration.", "",
    "The additional gate counts are diagnostic only. Allowing non-scrimmage events can include special",
    "teams, and accepting missing EPA does not make a play model-eligible. Neither relaxation was used",
    "in any fitted candidate, and total-count agreement can conceal compensating classification errors.", "",
    "A separate official-gamebook witness exposes information missing from both cached flags and text:",
    "three Miami sack-fumbles against Duke in 2022 are cached as ordinary sacks with both flags zero.",
    "See the copied matchup_gamebook_witnesses.csv and",
    "[Duke's official play-by-play](https://goduke.com/sports/football/stats/2022/miami/boxscore/20473).",
    "Associated EPA needs an independent source/model-vintage check; no corrected EPA was calculated",
    "or tested here. Do not assume the flag-only results quantify the benefit of a complete repair.", "",
    "### Drive Evidence Feasibility", "",
    matchup_report_table(read("drive_audit","season_checks.csv")), "",
    "Raw drive outcomes retain FUMBLE for the three official sack-fumble witnesses, even where",
    "play flags and descriptions omit it. However, drive evidence alone exactly matches only",
    "709 of the 888 sampled team-game box totals, with 155 below and 24 above. It is repair",
    "evidence, not a replacement classifier or a measured betting improvement. Contradictory",
    "drive metadata is quarantined; ambiguous FUMBLE TD labels are not counted as giveaways.",
    "This corrected audit uses pos_team, the possessing team; the earlier offense_play probe",
    "incorrectly mixed kickoff kicking units into drive ownership and is superseded.",
    "No drive-based relabeling, EPA recalculation or production change was made.", "",
    "### Current Cache And Structured Event Sources", "",
    matchup_report_table(current[c("profile","scrimmage","included_giveaways","excluded_giveaways","havoc")]), "",
    "The 2026 cached source check uses games kicked off before the existing September 7 Monday bucket.",
    "It demonstrates that the flag issue persists now; it is not a reconstructed information-arrival",
    "cutoff, a new live card, or verification of 75 official giveaways. Production picks are unchanged.", "",
    matchup_report_table(associations[c("season","home","away","association_rows","fumble_rows","recovery_rows","interception_thrown_rows")]), "",
    "The structured player/play association endpoint does not resolve the four selected witnesses:",
    "no recovery associations were returned, and both known LSU fumble events are absent. Responses",
    "were below its documented 2,000-record limit. Missing associations cannot mean zero events.",
    "See [current CFBD play API](https://github.com/CFBD/cfbd-python/blob/main/docs/PlaysApi.md).", "",
    "## Root-Cause Controls", "", display(root_display), "",
    matchup_report_table(root_changes), "",
    "Fixed-football controls retain the 4,246-row tracker-matched training population and original",
    "fold settings. The full-v3 control instead retains the complete original v3 training population.",
    "All rows above score the same 2,955 qualified-quote games. They are not interchangeable fits.",
    "Corrected candidates are isolated, not installed into production or the published article.", "",
    "Broader full-v3 impact check, using original PBP quotes rather than qualified bookmaker quotes:", "",
    display(v3metrics[v3metrics$lane=="original_pbp_quotes" & v3metrics$slice=="all", ]), "",
    "## Playoff Scope", "", display(cfp), "",
    "The shared external-input cohort has only 17 CFP games: all 2025 CFP FPI forecasts are absent",
    "from the tracker archive. These are actual round matchups with weekly inputs, NOT an entire",
    "bracket frozen before round one. The broader full-v3-only PBP lane retains its separate CFP coverage:", "",
    display(v3metrics[v3metrics$lane=="original_pbp_quotes" & v3metrics$slice=="cfp", ]), "",
    "Small playoff samples are descriptive. They do not validate a special playoff betting policy.", "",
    "## Quote Checks", "", matchup_report_table(linecheck), "",
    sprintf("Fixed 12-game public-source spot check: 11 quotes recovered, %d exact matches, two differing quotes, one unresolved.",source_matches),
    "Some pages contradict themselves or show different books/times; none establishes a complete",
    "Friday-to-close executable record. See MATCHUP_QUOTE_AUDIT.md and matchup_public_quote_checks.csv.",
    "Alternative-book and half/one-point-worse diagnostics hold the original selected side fixed.", "",
    "## Statistical Limits", "",
    sprintf("all_policy_profiles.csv reports all %d policy/profile combinations, including repeated controls,",family),
    "with conservative within-report Bonferroni-adjusted binomial intervals. Those nominal intervals",
    "do not solve game dependence, prior historical experimentation, source timing, or data reuse.",
    "Paired season-block intervals use only four seasons; exact sign-flip diagnostics have 16 sign patterns.",
    "All 2022-2025 seasons were examined in earlier work. No result here is untouched confirmation.", "",
    "## Verification And Reproduction", "",
    "All source/result directories are checksum-verified. Outputs preserve every policy and exclusion.",
    "Regression tests cover source identity/signs/families, train/future exclusion, Week 0/1 neutrality,",
    "CFP phase, side-reversing features, exact duplicate proof, train-only recipes and turnover masking.",
    "The first matchup result 20260912T013933.970Z is superseded: its new feature builder treated",
    "CFBD postseason week 1 as preseason. The corrected builder uses CFP phase 99; production already did.",
    "No initial or failed output was silently overwritten. See copied protocol for the correction chronology.", "",
    sprintf("The fixed suite passes %d test blocks; the raw-to-feature reconstruction matches exactly.",sum(verification$test_blocks)),
    "Selected/candidate forecasts and saved model recipes reproduce to floating-point precision.",
    "An independent margin-plus-home-spread calculation reproduces every reported policy row's grade.",
    "The saved Week 2 card and read-only ledger replay pass. Package source-failure tests emit",
    "expected blocked-network cfbfastR model-load warnings; no replacement source was downloaded.", "",
    "Commands from repository root (retained ignored caches required; no automatic downloads):", "",
    "```powershell", "Rscript run_cfb_matchup_research.R",
    "Rscript run_cfb_matchup_foundation_audit.R",
    "Rscript run_cfb_matchup_foundation_audit.R --turnover-flag",
    "Rscript run_cfb_v3_turnover_audit.R", "Rscript run_cfb_matchup_verify.R",
    "Rscript run_cfb_matchup_report.R", "```", "",
    "The full source bundles and RDS/CSV research outputs are local ignored artifacts; a Git clone alone",
    "does not contain them. Copy the retained research caches to reproduce this exact audit.", "",
    "## Source Semantics", "",
    "The installed cfbfastR is 2.0.0 and these caches use the classic release family. The current",
    "upstream has a distinct ESPN-derived family and newer model artifacts; do not mix EPA vintages",
    "or replace the source silently. The classic success field uses 50/70/100 down-distance thresholds,",
    "not positive-EPA success. Our havoc label is a custom disruption measure, not a standard provider rating.",
    "References: [classic loader](https://github.com/sportsdataverse/cfbfastR/blob/main/R/load_cfb_pbp.R),",
    "[field definitions](https://github.com/sportsdataverse/cfbfastR/blob/main/R/cfbd_pbp_data.R),",
    "[model/source release notes](https://github.com/sportsdataverse/cfbfastR/blob/main/NEWS.md).", "",
    "## Next Decision", "",
    "Prioritize the turnover/classification data contract, with box-score reconciliation and regression",
    "fixtures, before another model version. Keep v3 and the published card frozen during that work.",
    "Continue same-cutoff, actual-price prospective benchmark snapshots. Do not market these historical",
    "tests as a proven betting edge or promote the weakest-looking subgroup's opposite side.")
  writeLines(lines,file.path(output,"REPORT.md"))
  write.csv(data.frame(artifact=names(artifacts),directory=unlist(artifacts,use.names=FALSE)),
    file.path(output,"artifact_inventory.csv"),row.names=FALSE)
  write.csv(protected,file.path(output,"protected_before.csv"),row.names=FALSE)
  for(file in c("MATCHUP_PROTOCOL.md","MATCHUP_QUOTE_AUDIT.md","TURNOVER_DATA_CONTRACT.md",
    "matchup_public_quote_checks.csv","matchup_gamebook_witnesses.csv","matchup_report.R"))
    file.copy(file.path(project,"cfb_v3/experiments",file),file.path(output,file))
  writeLines(capture.output(sessionInfo()),file.path(output,"session_info.txt"))
  if(!identical(protected,experiment_file_hashes(config)))stop("Production changed during reporting.")
  external_seal(output)
  output
}
