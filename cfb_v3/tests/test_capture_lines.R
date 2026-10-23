library(testthat)
source("cfb_v3/capture_lines.R")

as_body <- function(x) list(body = charToRaw(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")),
                            headers = list(`x-requests-remaining` = "497", `x-requests-used` = "3"))

cfbd_fixture <- function(endpoint, query) {
  switch(endpoint,
    calendar = as_body(list(
      list(season = 2026, week = 8, seasonType = "regular",
           startDate = "2026-10-20T04:00:00.000Z", endDate = "2026-10-26T03:30:00.000Z"),
      list(season = 2026, week = 9, seasonType = "regular",
           startDate = "2026-10-27T04:00:00.000Z", endDate = "2026-11-02T03:30:00.000Z"))),
    games = as_body(list(
      list(id = 401000001, homeTeam = "Ohio State", awayTeam = "Penn State",
           startDate = "2026-10-24T16:00:00.000Z", startTimeTBD = FALSE, neutralSite = FALSE),
      list(id = 401000002, homeTeam = "Miami", awayTeam = "Miami (OH)",
           startDate = "2026-10-24T23:30:00.000Z", startTimeTBD = FALSE, neutralSite = FALSE),
      list(id = 401000003, homeTeam = "Texas", awayTeam = "Oklahoma",
           startDate = "2026-10-24T19:30:00.000Z", startTimeTBD = FALSE, neutralSite = TRUE))),
    lines = as_body(list(
      list(id = 401000001, season = 2026, seasonType = "regular", week = 8,
           homeTeam = "Ohio State", awayTeam = "Penn State", startDate = "2026-10-24T16:00:00.000Z",
           lines = list(
             list(provider = "DraftKings", spread = -6.5, spreadOpen = -5.5, overUnder = 47.5,
                  overUnderOpen = 48, homeMoneyline = -250, awayMoneyline = 205),
             list(provider = "ESPN Bet", spread = -7, spreadOpen = NULL, overUnder = 47,
                  overUnderOpen = NULL, homeMoneyline = NULL, awayMoneyline = NULL))))))
}

odds_fixture <- function(regions, markets) {
  book <- function(home_name, away_name, home_point, home_price, away_price) list(
    key = "fanduel", title = "FanDuel", last_update = "2026-10-23T05:58:12Z",
    markets = list(
      list(key = "h2h", last_update = "2026-10-23T05:58:12Z", outcomes = list(
        list(name = home_name, price = -240), list(name = away_name, price = 198))),
      list(key = "spreads", last_update = "2026-10-23T05:57:00Z", outcomes = list(
        list(name = home_name, price = home_price, point = home_point),
        list(name = away_name, price = away_price, point = -home_point))),
      list(key = "totals", last_update = "2026-10-23T05:50:00Z", outcomes = list(
        list(name = "Over", price = -112, point = 47.5), list(name = "Under", price = -108, point = 47.5)))))
  as_body(list(
    list(id = "evt-osu", commence_time = "2026-10-24T16:00:00Z",
         home_team = "Ohio State Buckeyes", away_team = "Penn State Nittany Lions",
         bookmakers = list(book("Ohio State Buckeyes", "Penn State Nittany Lions", -6.5, -105, -115))),
    list(id = "evt-miami", commence_time = "2026-10-24T23:30:00Z",
         home_team = "Miami Hurricanes", away_team = "Miami (OH) RedHawks",
         bookmakers = list(book("Miami Hurricanes", "Miami (OH) RedHawks", -24.5, -110, -110))),
    # Neutral site listed with the opposite orientation from CFBD.
    list(id = "evt-rrs", commence_time = "2026-10-24T19:30:00Z",
         home_team = "Oklahoma Sooners", away_team = "Texas Longhorns",
         bookmakers = list(book("Oklahoma Sooners", "Texas Longhorns", 3.5, -102, -118))),
    list(id = "evt-later", commence_time = "2026-11-14T17:00:00Z",
         home_team = "Iowa Hawkeyes", away_team = "Nebraska Cornhuskers", bookmakers = list())))
}

fixed_clock <- function() as.POSIXct("2026-10-23T06:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

run_fixture <- function(store, label = "friday", ...) {
  capture_lines(store, label, season = 2026L, sources = "cfbd,odds", clock = fixed_clock,
                fetch_cfbd = cfbd_fixture, fetch_odds = odds_fixture, ...)
}

test_that("a capture writes book, prices, timestamps, and CFBD game IDs", {
  store <- tempfile("lines")
  result <- run_fixture(store)
  q <- utils::read.csv(file.path(store, "line_quotes.csv"), stringsAsFactors = FALSE,
                       colClasses = c(game_id = "character"))
  expect_identical(names(q), lc_store_columns)
  expect_equal(nrow(q), 5L)
  expect_true(all(q$week == 8L & q$season_type == "regular"))
  expect_true(all(q$captured_at == "2026-10-23T06:00:00Z"))
  cfbd <- q[q$source == "cfbd", ]
  expect_setequal(cfbd$book, c("draftkings", "espnbet"))
  expect_equal(cfbd$spread_open[cfbd$book == "draftkings"], -5.5)
  expect_true(all(is.na(cfbd$home_spread_price)))
  odds <- q[q$source == "odds_api", ]
  expect_setequal(odds$game_id, c("401000001", "401000002", "401000003"))
  osu <- odds[odds$game_id == "401000001", ]
  expect_equal(c(osu$home_spread, osu$home_spread_price, osu$away_spread_price), c(-6.5, -105, -115))
  expect_equal(c(osu$total, osu$over_price, osu$under_price), c(47.5, -112, -108))
  expect_equal(osu$spread_updated_at, "2026-10-23T05:57:00Z")
  expect_equal(osu$minutes_to_kickoff, 34 * 60)
  expect_true(osu$pregame)
  miami <- odds[odds$game_id == "401000002", ]
  expect_equal(c(miami$home, miami$away), c("Miami", "Miami (OH)"))
  log <- utils::read.csv(file.path(store, "captures.csv"), stringsAsFactors = FALSE)
  expect_equal(log$status, c("ok", "ok"))
  expect_equal(log$requests_remaining[log$source == "odds_api"], 497)
  expect_equal(log$games[log$source == "odds_api"], 3L)
  expect_true(all(file.exists(file.path(store, "raw", result$capture_id, log$raw_file))))
})

test_that("swapped neutral-site orientation is re-expressed for the CFBD home team", {
  q <- run_fixture(tempfile("lines"))$quotes
  rrs <- q[q$source == "odds_api" & q$game_id %in% "401000003", ]
  expect_equal(c(rrs$home, rrs$source_home), c("Texas", "Oklahoma Sooners"))
  expect_equal(rrs$home_spread, -3.5)
  expect_equal(c(rrs$home_spread_price, rrs$away_spread_price), c(-118, -102))
  expect_equal(c(rrs$home_moneyline, rrs$away_moneyline), c(198, -240))
})

test_that("captures append and never rewrite earlier rows", {
  store <- tempfile("lines")
  run_fixture(store, "opener")
  first <- readLines(file.path(store, "line_quotes.csv"))
  later <- function() fixed_clock() + 3600
  capture_lines(store, "friday", season = 2026L, sources = "cfbd,odds", clock = later,
                fetch_cfbd = cfbd_fixture, fetch_odds = odds_fixture)
  both <- readLines(file.path(store, "line_quotes.csv"))
  expect_identical(both[seq_along(first)], first)
  expect_equal(length(both), 2L * length(first) - 1L)
  expect_error(run_fixture(store, "opener"), "already exists")
  writeLines(sub("capture_id", "renamed", both[1]), file.path(store, "line_quotes.csv"))
  expect_error(capture_lines(store, "close", season = 2026L, sources = "cfbd", clock = function() later() + 60,
                             fetch_cfbd = cfbd_fixture), "header differs")
})

test_that("a failing source is logged without leaking the key", {
  store <- tempfile("lines")
  withr_key <- Sys.getenv("ODDS_API_KEY")
  Sys.setenv(ODDS_API_KEY = "super-secret-key")
  on.exit(Sys.setenv(ODDS_API_KEY = withr_key))
  leaky <- function(regions, markets) {
    key <- Sys.getenv("ODDS_API_KEY")
    lc_http_json("http://127.0.0.1:9/odds", list(apiKey = key), secret = key)
  }
  result <- capture_lines(store, "adhoc", season = 2026L, sources = "cfbd,odds", clock = fixed_clock,
                          fetch_cfbd = cfbd_fixture, fetch_odds = leaky)
  expect_equal(result$log$status, c("ok", "error"))
  stored <- paste(readLines(file.path(store, "captures.csv")), collapse = "\n")
  expect_false(grepl("super-secret-key", stored, fixed = TRUE))
  expect_equal(sum(result$quotes$source == "cfbd"), 2L)
})

test_that("auto sources skip the Odds API when no key is configured", {
  old <- Sys.getenv("ODDS_API_KEY")
  Sys.unsetenv("ODDS_API_KEY")
  on.exit(if (nzchar(old)) Sys.setenv(ODDS_API_KEY = old))
  result <- capture_lines(tempfile("lines"), "adhoc", season = 2026L, clock = fixed_clock,
                          fetch_cfbd = cfbd_fixture, fetch_odds = function(...) stop("should not run"))
  expect_equal(result$log$source, c("cfbd", "odds_api"))
  expect_equal(result$log$status, c("ok", "skipped"))
})

test_that("the default week moves on once the last kickoff has passed", {
  cal <- list(
    list(week = 8, seasonType = "regular", startDate = "2026-10-20T04:00:00.000Z",
         endDate = "2026-10-27T03:59:00.000Z", lastGameStart = "2026-10-25T03:30:00.000Z"),
    list(week = 9, seasonType = "regular", startDate = "2026-10-27T04:00:00.000Z",
         endDate = "2026-11-03T03:59:00.000Z", lastGameStart = "2026-11-01T03:30:00.000Z"))
  sunday <- lc_parse_time("2026-10-25T17:00:00Z")
  expect_equal(lc_current_week(cal, sunday)$week, 9L)
  expect_equal(lc_current_week(cal, sunday - 86400)$week, 8L)
})

test_that("aliases and one-team fallback match unusual Odds API names", {
  games <- lc_parse_cfbd_games(list(
    list(id = 1, homeTeam = "App State", awayTeam = "UL Monroe", startDate = "2026-10-24T20:00:00.000Z")))
  t <- lc_parse_time("2026-10-24T20:00:00Z")
  m <- lc_match_event("Appalachian State Mountaineers", "UL Monroe Warhawks", t, games)
  expect_equal(c(m$game_id, m$method), c("1", "one_team_and_kickoff"))
  alias <- stats::setNames("App State", lc_team_key("Appalachian State Mountaineers"))
  m <- lc_match_event("Appalachian State Mountaineers", "UL Monroe Warhawks", t, games, alias)
  expect_equal(c(m$game_id, m$method), c("1", "team_names"))
  expect_equal(lc_match_event("Boise State Broncos", "Utah State Aggies", t, games)$method, "unmatched")
})

test_that("as-of selection returns the latest pregame quote per book", {
  store <- tempfile("lines")
  run_fixture(store, "opener")
  capture_lines(store, "close", season = 2026L, sources = "cfbd,odds",
                clock = function() fixed_clock() + 40 * 3600,
                fetch_cfbd = cfbd_fixture, fetch_odds = odds_fixture)
  q <- utils::read.csv(file.path(store, "line_quotes.csv"), stringsAsFactors = FALSE,
                       colClasses = c(game_id = "character"))
  friday_noon_pt <- as.POSIXct("2026-10-23 19:00:00", tz = "UTC")
  a <- line_quotes_as_of(q, friday_noon_pt)
  expect_true(all(a$label == "opener"))
  latest <- line_quotes_as_of(q, fixed_clock() + 41 * 3600, books = "FanDuel")
  # The 16:00Z game kicked off before the 22:00Z close capture, so its opener quote is kept.
  expect_equal(latest$label[latest$game_id == "401000001"], "opener")
  expect_equal(latest$label[latest$game_id == "401000002"], "close")
})
