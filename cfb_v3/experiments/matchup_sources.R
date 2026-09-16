matchup_capture_lines <- function(project, years = 2020:2025) {
  root <- file.path(project, "cfb_v3/output/experiments/matchup_sources")
  output <- external_new_dir(root, "")
  inventory <- list()
  for (year in years) for (type in c("regular", "postseason")) {
    filename <- paste0("cfbd_lines_", year, "_", type, ".json")
    path <- file.path(output, filename)
    message("Capturing provider lines: ", year, " ", type)
    captured <- external_fetch("https://api.collegefootballdata.com/lines", path,
      query = list(year = year, seasonType = type), authenticated = TRUE)
    x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!is.list(x)) stop("Unexpected line response.")
    inventory[[length(inventory)+1L]] <- data.frame(season = year, season_type = type,
      source_url = paste0("https://api.collegefootballdata.com/lines?year=",year,"&seasonType=",type),
      retrieved_at = captured, file = filename, game_rows = length(x),
      sha256 = digest::digest(file = path, algo = "sha256"),
      evidence = "provider_reported_historical_line_no_verified_observation_timestamp")
    write.csv(do.call(rbind, inventory), file.path(output,"source_inventory.csv"), row.names = FALSE)
  }
  external_seal(output)
  output
}

matchup_book <- function(provider) {
  provider <- trimws(provider)
  provider[provider == "Draft Kings"] <- "DraftKings"
  family <- c("DraftKings" = "DraftKings", "FanDuel" = "FanDuel", "Bovada" = "Bovada",
    "Caesars" = "Caesars", "Caesars (Pennsylvania)" = "Caesars",
    "Caesars Sportsbook (Colorado)" = "Caesars", "William Hill (New Jersey)" = "Caesars",
    "ESPN Bet" = "ESPN Bet", "SugarHouse" = "SugarHouse")
  data.frame(provider = provider, book_family = unname(family[provider]), stringsAsFactors = FALSE)
}

matchup_formatted_spread <- function(text, home, away, spread) {
  text <- trimws(as.character(text))
  if (is.na(text) || !nzchar(text) || !is.finite(spread)) return(FALSE)
  if (spread == 0 && grepl("^(pk|pick|pick.?em|even|0)$",text,ignore.case=TRUE)) return(TRUE)
  pattern <- "\\s+([+-]?[0-9]+(?:\\.[0-9]+)?)$"
  if (!grepl(pattern,text,perl=TRUE)) return(FALSE)
  number <- as.numeric(sub(paste0(".*",pattern),"\\1",text,perl=TRUE))
  label <- canonical_team(sub(pattern,"",text,perl=TRUE))
  if (anyNA(c(label,home,away))) return(FALSE)
  value <- if (label == canonical_team(home)) number else if (label == canonical_team(away)) -number else NA_real_
  is.finite(value) && abs(value-spread) <= .001
}

matchup_read_lines <- function(directory, games) {
  external_verify(directory)
  inventory <- read.csv(file.path(directory,"source_inventory.csv"),stringsAsFactors=FALSE)
  assert_unique_keys(games,"game_id","line schedule")
  scalar <- function(x, name, numeric = FALSE) {
    v <- x[[name]]
    if (is.null(v) || !length(v)) return(if (numeric) NA_real_ else NA_character_)
    if (length(v) != 1L) stop("Non-scalar line field: ",name)
    if (numeric) as.numeric(v) else as.character(v)
  }
  rows <- list()
  for (file in seq_len(nrow(inventory))) {
    path <- file.path(directory,inventory$file[file])
    if (!identical(digest::digest(file=path,algo="sha256"),inventory$sha256[file])) stop("Raw line source changed.")
    source <- jsonlite::fromJSON(path,simplifyVector=FALSE)
    for (g in source) {
      id <- scalar(g,"id"); i <- match(id,as.character(games$game_id))
      if (is.na(i)) next
      home <- canonical_team(scalar(g,"homeTeam")); away <- canonical_team(scalar(g,"awayTeam"))
      same <- isTRUE(home == canonical_team(games$home[i]) && away == canonical_team(games$away[i]))
      reverse <- isTRUE(home == canonical_team(games$away[i]) && away == canonical_team(games$home[i]))
      orientation <- if (same) 1 else if (reverse && isTRUE(games$neutral_site[i])) -1 else NA_real_
      reason <- if (same) "matched" else if (reverse && isTRUE(games$neutral_site[i])) "neutral_reversal" else "side_mismatch"
      source_season <- scalar(g,"season",TRUE)
      if (!is.finite(source_season) || source_season != games$season[i]) reason <- "season_mismatch"
      if (is.finite(orientation)) {
        hs <- scalar(g,if(orientation==1)"homeScore" else "awayScore",TRUE)
        as <- scalar(g,if(orientation==1)"awayScore" else "homeScore",TRUE)
        if (!all(is.finite(c(hs,as,games$home_score[i],games$away_score[i]))) ||
            hs != games$home_score[i] || as != games$away_score[i]) reason <- "score_mismatch"
      }
      for (line in g$lines) {
        spread <- scalar(line,"spread",TRUE)
        book <- matchup_book(scalar(line,"provider"))
        rows[[length(rows)+1L]] <- data.frame(game_id=id, season=games$season[i], week=games$week[i],
          home=games$home[i],away=games$away[i],neutral_site=games$neutral_site[i],
          source_home=home,source_away=away,orientation=orientation,identity_status=reason,
          provider=book$provider,book_family=book$book_family,
          spread=spread*orientation,spread_open=scalar(line,"spreadOpen",TRUE)*orientation,
          total=scalar(line,"overUnder",TRUE),
          home_moneyline=scalar(line,if(is.finite(orientation) && orientation == -1)"awayMoneyline" else "homeMoneyline",TRUE),
          away_moneyline=scalar(line,if(is.finite(orientation) && orientation == -1)"homeMoneyline" else "awayMoneyline",TRUE),
          formatted_spread=scalar(line,"formattedSpread"),
          formatted_verified=matchup_formatted_spread(scalar(line,"formattedSpread"),home,away,spread),
          source_file=inventory$file[file],source_sha256=inventory$sha256[file],retrieved_at=inventory$retrieved_at[file],
          evidence=inventory$evidence[file],stringsAsFactors=FALSE)
      }
    }
  }
  raw <- do.call(rbind,rows)
  if (is.null(raw) || !nrow(raw)) stop("No historical lines matched the schedule.")
  keys <- paste(raw$game_id,raw$provider,sep="\r")
  raw$duplicate_status <- "unique"
  for (i in split(seq_len(nrow(raw)),keys)) {
    if (length(i) < 2L) next
    values <- raw[i,c("spread","spread_open","home_moneyline","away_moneyline","total","identity_status","formatted_verified")]
    if (nrow(unique(values)) == 1L) raw$duplicate_status[i[-1]] <- "identical_removed" else raw$duplicate_status[i] <- "conflicting_quarantined"
  }
  raw
}

matchup_qualify_lines <- function(raw) {
  raw$eligible_record <- raw$identity_status %in% c("matched","neutral_reversal") &
    raw$duplicate_status == "unique" & !is.na(raw$book_family) &
    is.finite(raw$spread) & raw$formatted_verified
  raw$eligible_record[is.na(raw$eligible_record)] <- FALSE
  raw$supporting_families <- 0L
  raw$chosen <- FALSE
  priority <- c("DraftKings","FanDuel","Bovada","Caesars Sportsbook (Colorado)",
    "Caesars","William Hill (New Jersey)","Caesars (Pennsylvania)","ESPN Bet","SugarHouse")
  for (rows in split(which(raw$eligible_record),raw$game_id[raw$eligible_record])) {
    for (i in rows) {
      support <- rows[raw$book_family[rows] != raw$book_family[i] & abs(raw$spread[rows]-raw$spread[i]) <= 1]
      raw$supporting_families[i] <- length(unique(raw$book_family[support]))
    }
    eligible <- rows[raw$supporting_families[rows] >= 1L]
    if (length(eligible)) {
      selected <- eligible[order(match(raw$provider[eligible],priority),raw$provider[eligible])][1]
      raw$chosen[selected] <- TRUE
    }
  }
  selected <- raw[raw$chosen, ]
  raw$qualification <- ifelse(raw$chosen, "chosen", ifelse(raw$eligible_record &
    raw$supporting_families >= 1L, "supported_alternative", ifelse(raw$eligible_record,
    "no_other_family_within_one", "record_failed_validation")))
  assert_unique_keys(selected,"game_id","qualified historical reference")
  list(ledger=raw,selected=selected)
}
