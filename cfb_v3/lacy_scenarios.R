# Offline supplemental analysis; never changes the fitted model or production inbox.
# Rscript cfb_v3/lacy_scenarios.R --predictions=PATH.csv --player-inputs=PATH.rds
args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = NULL) {
  hit <- args[startsWith(args, paste0("--", name, "="))]
  if (length(hit) > 1L) stop("Repeated argument: ", name)
  if (length(hit)) substring(hit, nchar(name) + 4L) else default
}
if (any(!grepl("^--(predictions|player-inputs)=", args))) stop("Unknown argument.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
modules <- file.path(root, "cfb_v2", c("config.R", "store.R", "features.R", "models.R"))
for (module in modules) source(module)
for (package in c("digest", "jsonlite")) require_v2_package(package)
default_csv <- file.path(root, "cfb_v3/output/2026/week_4/live_live_2026_w04_20260924T173139Z.csv")
csv <- normalizePath(option("predictions", default_csv), winslash = "/", mustWork = TRUE)
model_path <- sub("\\.csv$", "_model.rds", csv)
features_path <- sub("\\.csv$", "_features.csv", csv)
player_path <- normalizePath(option("player-inputs", file.path(dirname(csv),
  "lacy_scenarios/player_inputs.rds")), winslash = "/", mustWork = TRUE)
bundle <- readRDS(model_path)
baseline_as_of_utc <- format(parse_utc_datetime(bundle$as_of), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
card <- read.csv(csv, stringsAsFactors = FALSE)
features <- read.csv(features_path, stringsAsFactors = FALSE)
raw <- readRDS(player_path)
injury_path <- file.path(bundle$config$inbox_dir, "injuries.csv")
sources <- c(csv, model_path, features_path, player_path, injury_path, modules, script)
sources <- normalizePath(sources, winslash = "/", mustWork = TRUE)
hashes <- vapply(sources, function(path) digest::digest(file = path, algo = "sha256"), "")
target_id <- "401856699"
target <- card[as.character(card$game_id) == target_id, , drop = FALSE]
stopifnot(nrow(target) == 1L, target$season == 2026L, target$week == 4L,
  canonical_team(target$home) == "Florida", canonical_team(target$away) == "Mississippi")
matchup <- features[as.character(features$game_id) == target_id, , drop = FALSE]
stopifnot(nrow(matchup) == 1L, matchup$season == 2026L, matchup$week == 4L,
  canonical_team(matchup$home) == "Florida", canonical_team(matchup$away) == "Mississippi")
schedule <- target[c("game_id", "season", "week", "home", "away")]
schedule$game_id <- as.character(schedule$game_id)
matchup$game_id <- as.character(matchup$game_id)
market <- as.numeric(target$market_home_spread)
stopifnot(is.finite(market), raw$season == 2026L, raw$through_week == 3L)

usage <- as.data.frame(raw$usage)
ppa <- as.data.frame(raw$ppa)
usage <- usage[canonical_team(usage$team) == "Mississippi" & usage$position == "RB", ]
ppa <- ppa[canonical_team(ppa$team) == "Mississippi" & ppa$position == "RB", ]
assert_unique_keys(usage, "athlete_id", "RB usage")
assert_unique_keys(ppa, "athlete_id", "RB PPA")
players <- merge(usage[c("athlete_id", "name", "usg_overall")],
  ppa[c("athlete_id", "avg_PPA_all", "total_PPA_all")], by = "athlete_id")
ids <- c("5086388", "5209688", "5084286")
players <- players[match(ids, players$athlete_id), ]
stopifnot(identical(as.character(players$athlete_id), ids),
  identical(players$name, c("Kewan Lacy", "JT Lindsey", "Makhi Frazier")),
  all(is.finite(as.matrix(players[c("usg_overall", "avg_PPA_all", "total_PPA_all")]))))
games <- as.data.frame(raw$games)
if (any(games$season == 2026L & games$completed %in% TRUE & games$week > 3L, na.rm = TRUE)) {
  stop("Player snapshot contains completed Week 4+ games; cannot use it for this Week 4 scenario.")
}
completed <- games$season == 2026L & games$week <= 3L & games$completed %in% TRUE &
  (canonical_team(games$home_team) == "Mississippi" |
     canonical_team(games$away_team) == "Mississippi") &
  parse_utc_datetime(games$start_date) < parse_utc_datetime(bundle$as_of)
completed_games <- sum(completed, na.rm = TRUE)
stopifnot(completed_games == 3L, as.numeric(raw$team_stats$games[1]) == completed_games,
  players$avg_PPA_all[1] != 0, players$usg_overall[1] > 0,
  all(players$usg_overall[-1] > 0))
opportunities <- players$total_PPA_all[1] / players$avg_PPA_all[1] / completed_games
backup_weights <- players$usg_overall[-1] / sum(players$usg_overall[-1])
backup_ppa <- sum(backup_weights * players$avg_PPA_all[-1])
loss <- opportunities * (players$avg_PPA_all[1] -
  c(backup_ppa, players$avg_PPA_all[2], players$avg_PPA_all[3]))
stopifnot(is.finite(opportunities), opportunities > 0, all(is.finite(loss)))
plans <- data.frame(
  scenario_id = c("available", "out_usage_weighted", "out_all_jt", "out_all_frazier"),
  scenario = c("Lacy available / no new injury adjustment", "Lacy out / usage-PPA estimate",
    "Lacy out / all replacement work to JT Lindsey", "Lacy out / all replacement work to Makhi Frazier"),
  status = c("active", rep("out", 3)),
  replacement_ppa = c(NA_real_, backup_ppa, players$avg_PPA_all[2:3]),
  impact_points = c(0, loss), stringsAsFactors = FALSE)

injuries <- select_latest_injury_reports(read.csv(injury_path, stringsAsFactors = FALSE), bundle$as_of)
if (nrow(injuries)) {
  same_player <- (!is.na(injuries$player_id) & injuries$player_id == ids[1]) |
    (!is.na(injuries$player_name) & tolower(trimws(injuries$player_name)) == "kewan lacy")
  remove <- injuries$game_id == target_id & canonical_team(injuries$team) == "Mississippi" & same_player
  injuries <- injuries[!remove, , drop = FALSE]
}
rows <- data.frame(game_id = target_id, team = "Mississippi", player_id = ids[1],
  player_name = "Kewan Lacy", position = "RB", status = plans$status,
  usage_share = players$usg_overall[1], starter = TRUE, impact_points = plans$impact_points,
  availability_probability = ifelse(plans$status == "active", 1, 0),
  source_type = "manual_exception", source_url = NA_character_,
  source_timestamp = raw$fetched_at, confidence = 1, conflict_flag = FALSE,
  stringsAsFactors = FALSE)
bind_injuries <- function(extra) {
  existing <- injuries
  for (name in setdiff(names(existing), names(extra))) extra[[name]] <- NA
  for (name in setdiff(names(extra), names(existing))) existing[[name]] <- rep(NA, nrow(existing))
  rbind(existing[names(extra)], extra)
}
results <- lapply(seq_len(nrow(plans)), function(i) predict_week(
  bundle$model, schedule, matchup, market_home_spread = market, force_game_ids = target_id,
  injuries = bind_injuries(rows[i, , drop = FALSE]), ats_threshold = bundle$ats_threshold,
  ats_model = bundle$ats_model, config = bundle$config))
baseline <- results[[1]]$expected_margin
stopifnot(abs(baseline - target$expected_margin) < 1e-8,
  abs(results[[1]]$margin_sd - target$margin_sd) < 1e-8)
for (i in seq_along(results)) stopifnot(
  abs(results[[i]]$expected_margin - baseline - plans$impact_points[i]) < 1e-8,
  abs(results[[i]]$margin_sd - results[[1]]$margin_sd) < 1e-8,
  identical(as.numeric(results[[i]]$market_home_spread), market),
  abs(results[[i]]$ats_edge_home - results[[i]]$expected_margin - market) < 1e-8)
out <- plans
out$game_id <- target_id
out$home <- "Florida"
out$away <- "Mississippi"
out$expected_home_margin <- vapply(results, function(x) x$expected_margin, 0)
out$market_home_spread <- market
out$ole_miss_market_edge <- -(out$expected_home_margin + market)
out$ats_side_by_margin <- ifelse(out$ole_miss_market_edge > 0, "Ole Miss",
  ifelse(out$ole_miss_market_edge < 0, "Florida", "No margin edge"))
out$margin_sd <- vapply(results, function(x) x$margin_sd, 0)
out$market_captured_at <- target$market_captured_at
out$baseline_as_of <- baseline_as_of_utc
out$player_inputs_fetched_at <- raw$fetched_at
flip_impact <- -(baseline + market)
reference_flip <- -(baseline - 3)
margin_label <- function(x) ifelse(abs(x) < 1e-8, "Even", sprintf("%s by %.1f",
  ifelse(x > 0, "Florida", "Ole Miss"), abs(x)))
out$projected_margin <- margin_label(out$expected_home_margin)
market_label <- sprintf("Ole Miss %+.1f / Florida %+.1f", -market, market)
flip_text <- if (flip_impact > 0) sprintf(
  "A %.2f-point loss of Ole Miss margin reaches the current spread; a larger loss changes the margin-based ATS side to Florida.", flip_impact) else
  "The available baseline already favors Florida against the current spread; losing Ole Miss production does not reverse that side."
assumptions <- c(
  "Supplemental, unvalidated usage/PPA replacement calculation; this is not a learned player-specific injury coefficient.",
  "Implied Lacy PPA opportunities per game = total PPA / average PPA / 3 completed games. Published rounded inputs make this approximate; deprecated countable_plays is unused.",
  "Main replacement PPA is the current-usage-weighted mean for JT Lindsey and Makhi Frazier. The alternatives allocate all replacement work to one backup; they are not confidence bounds.",
  "Hold expected opportunities and backup efficiency fixed. PPA reflects opponent, play, teammate and game context; backup samples are small and may not transfer to a starting role.",
  "The frozen team baseline embeds actual prior availability. Available means no new injury adjustment, not a refitted fully healthy team model.",
  "Only Lacy's supplemental adjustment varies. The fitted model, matchup features, market and other resolved injuries stay fixed. Signed impacts are preserved.",
  "In the injury API, confidence = 1 is an arithmetic multiplier, not betting confidence. No ATS probability is supplied in this report.",
  "Player statistics were retrieved after the baseline market snapshot, through Week 3 only. Genuine injury reports use the baseline cutoff; hypothetical scenario rows retain the later player-data retrieval timestamp.")
stopifnot(identical(hashes, vapply(sources, function(path) digest::digest(file = path, algo = "sha256"), "")))
stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
directory <- file.path(dirname(csv), "lacy_scenarios", paste0("usage_ppa_", stamp, "_", Sys.getpid()))
if (dir.exists(directory)) stop("Scenario output already exists: ", directory)
dir.create(directory, recursive = TRUE)
write.csv(out, file.path(directory, "scenarios.csv"), row.names = FALSE, na = "")
write.csv(cbind(scenario_id = plans$scenario_id, rows), file.path(directory, "hypothetical_injury_rows.csv"), row.names = FALSE, na = "")
write.csv(injuries, file.path(directory, "other_resolved_injuries.csv"), row.names = FALSE, na = "")
write.csv(players, file.path(directory, "player_inputs.csv"), row.names = FALSE, na = "")
manifest <- list(created_at = stamp, target_game_id = target_id, baseline_as_of = baseline_as_of_utc,
  market_captured_at = target$market_captured_at, market_home_spread = market,
  player_inputs_fetched_at = raw$fetched_at, statistics_through_week = raw$through_week,
  completed_games = completed_games, implied_lacy_opportunities_per_game = opportunities,
  backup_weights = setNames(backup_weights, players$name[-1]), replacement_ppa = backup_ppa,
  current_market_flip_loss_points = flip_impact,
  original_discussion_reference = list(ole_miss_spread = 3, fresh_quote = FALSE,
    flip_loss_points = reference_flip), source_hashes = data.frame(path = sources, sha256 = hashes),
  assumptions = assumptions, verification = list(available_reproduces_saved_margin = TRUE,
    deterministic_absence_preserves_margin_sd = TRUE, exact_signed_margin_shift = TRUE,
    frozen_market = TRUE, source_files_unchanged = TRUE, refitted_model = FALSE))
jsonlite::write_json(manifest, file.path(directory, "manifest.json"), auto_unbox = TRUE,
  pretty = TRUE, digits = 16, na = "null")
table_lines <- c("| Scenario | Ole Miss loss (pts) | Projection | Ole Miss edge at current line |",
  "|---|---:|---|---:|", sprintf("| %s | %+.2f | %s | %+.2f |", out$scenario,
    out$impact_points, out$projected_margin, out$ole_miss_market_edge))
method <- sprintf("Lacy: %.1f%% usage, %.3f average PPA, %.3f total PPA across %d team games. Implied workload: %.2f PPA opportunities/game. Replacement mix: JT Lindsey %.1f%%, Makhi Frazier %.1f%%; weighted PPA %.4f. Main margin shift: %+.2f points toward Florida.",
  100 * players$usg_overall[1], players$avg_PPA_all[1], players$total_PPA_all[1], completed_games,
  opportunities, 100 * backup_weights[1], 100 * backup_weights[2], backup_ppa, loss[1])
reference <- sprintf("Original discussion reference: Ole Miss +3 (not a refreshed quote); %.2f points of lost margin reaches that spread.", reference_flip)
writeLines(c("# Ole Miss at Florida: Kewan Lacy scenarios", "", "Week 4, 2026 · Supplemental usage/PPA sensitivity", "",
  paste("Frozen market:", market_label, "— captured", target$market_captured_at), "", table_lines,
  "", method, "", flip_text, "", reference, "", "## Assumptions", "", paste0("- ", assumptions),
  "", paste0("[Frozen baseline dashboard](../../", sub("\\.csv$", "_dashboard.html", basename(csv)), ")"),
  "", paste0("[Frozen baseline CSV](../../", basename(csv), ")"),
  "", "[Availability reporting sources](../availability_sources.md)"), file.path(directory, "comparison.md"))
escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}
html_rows <- sprintf("<tr><td>%s</td><td>%+.2f</td><td>%s</td><td>%+.2f</td></tr>",
  escape(out$scenario), out$impact_points, escape(out$projected_margin), out$ole_miss_market_edge)
html <- c('<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>Kewan Lacy · Ole Miss at Florida</title><style>body{margin:0;background:#f3f5f8;color:#172033;font:16px/1.6 system-ui,sans-serif}main{max-width:1040px;margin:auto;padding:36px 24px}h1{font-size:34px;line-height:1.15;margin:8px 0 18px}h2{font-size:21px}.eyebrow{color:#556173;letter-spacing:.08em;font-size:12px;font-weight:750;text-transform:uppercase}.cards{display:grid;grid-template-columns:1fr 1fr;gap:18px;margin:26px 0}.card{background:white;border-radius:16px;padding:24px;border-top:5px solid #cf243b}.card:last-child{border-color:#315b92}.value{font-size:32px;font-weight:750;margin:10px 0}.note{color:#556173;font-size:14px}.table{overflow:auto}table{border-collapse:collapse;width:100%;background:white}th,td{text-align:left;padding:13px;border-bottom:1px solid #dbe0e8}th{font-size:13px;color:#556173}a{color:#2457a5}li{margin:8px 0}@media(max-width:650px){.cards{grid-template-columns:1fr}h1{font-size:28px}}</style><main>',
  '<div class="eyebrow">Week 4 · September 26, 2026 · Supplemental analysis</div><h1>Ole Miss at Florida<br>Kewan Lacy scenarios</h1>',
  '<p>Usage/PPA replacement sensitivity. These results reuse the fitted Week 4 model; the player adjustment is an unvalidated supplemental estimate.</p>',
  sprintf('<p class="note">Frozen market: <strong>%s</strong><br>Captured %s · Player statistics retrieved %s</p>', escape(market_label), escape(target$market_captured_at), escape(raw$fetched_at)),
  '<div class="cards">',
  sprintf('<section class="card"><div>%s</div><div class="value">%s</div><div>Ole Miss margin edge at the current line: %+.2f pts</div></section>', escape(out$scenario[1]), escape(out$projected_margin[1]), out$ole_miss_market_edge[1]),
  sprintf('<section class="card"><div>%s</div><div class="value">%s</div><div>Ole Miss margin edge at the current line: %+.2f pts</div></section>', escape(out$scenario[2]), escape(out$projected_margin[2]), out$ole_miss_market_edge[2]), '</div>',
  paste0('<h2>How the adjustment is calculated</h2><p>', escape(method), '</p><p><strong>', escape(flip_text), '</strong></p>'),
  '<h2>Replacement allocation sensitivities</h2><p class="note">The alternatives show different workload allocations. They are not confidence bounds.</p><div class="table"><table><thead><tr><th>Scenario</th><th>Ole Miss loss (pts)</th><th>Projection</th><th>Ole Miss edge (pts)</th></tr></thead><tbody>',
  html_rows, '</tbody></table></div>', paste0('<p class="note">', escape(reference), '</p>'),
  '<h2>Assumptions and limits</h2><ul>', paste0('<li>', escape(assumptions), '</li>'), '</ul>',
  sprintf('<p><a href="../../%s">Frozen baseline dashboard</a> · <a href="../../%s">Baseline CSV</a> · <a href="scenarios.csv">Scenario CSV</a> · <a href="manifest.json">Sources and verification</a> · <a href="../availability_sources.md">Availability reporting sources</a></p>',
    escape(sub("\\.csv$", "_dashboard.html", basename(csv))), escape(basename(csv))), '</main></html>')
writeLines(html, file.path(directory, "comparison.html"), useBytes = TRUE)
print(out[c("scenario", "impact_points", "projected_margin", "ole_miss_market_edge")], row.names = FALSE)
cat("\n", flip_text, "\nOutput: ", normalizePath(directory, winslash = "/"), "\n", sep = "")
