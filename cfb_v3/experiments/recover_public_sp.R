# Read-only source recovery. Download time is not historical publication time.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[1])) stop("Supply an existing experiment output directory.")
directory <- file.path(normalizePath(args[1], winslash = "/"), "public_sources")
if (dir.exists(directory)) stop("Refusing to overwrite a source capture.")
dir.create(directory)
sources <- data.frame(
  name = c("connelly_2022", "connelly_2023", "connelly_2024", "connelly_2025", "connelly_2026", "cfbtxt_2026"),
  url = c(
    "https://docs.google.com/spreadsheets/d/1llrN8luL0XWuP8Y-Pb1NXKU84JhXLeUPafy1RfITEDw/export?format=xlsx",
    "https://docs.google.com/spreadsheets/d/e/2PACX-1vRh9Slymcisd5-uEIvAD4zjGkJ7aeARseChhne-HdpyQeQiTSJeZD0WfyuG40O5S7Z20wz1XLYSUDUj/pubhtml",
    "https://docs.google.com/spreadsheets/d/1CJImfkg0ouHIIIGOWRfbvwC0TNWh76n47xkz8nqrVBc/export?format=xlsx",
    "https://docs.google.com/spreadsheets/d/1a6hboWNnPeUzx5oUEjwJwf9vw4DuAV7lTaW4Q92Zpls/export?format=xlsx",
    "https://docs.google.com/spreadsheets/d/1vwoVl-Dxy0es87Z9I1RTvFzr72Lb1fAkREfbLxbK-eg/export?format=xlsx",
    "https://cfbtxt.com/data/ratings_snapshots/2026/historical_ratings.csv"),
  evidence_url = c(
    "https://t.co/ld8Z9RqL9c",
    "https://t.co/fjsWyQPXfP",
    "https://bsky.app/profile/espnbillc.bsky.social/post/3lb6kz6kfw22w",
    "https://bsky.app/profile/espnbillc.bsky.social/post/3mcwuxsfgfs2q",
    "https://cfbtxt.com/ratings/", "https://cfbtxt.com/data/"))
inventory <- list()
for (i in seq_len(nrow(sources))) {
  item <- as.list(sources[i, ])
  extension <- if (grepl("format=xlsx", item$url, fixed = TRUE)) ".xlsx" else
    if (grepl("/pubhtml$", item$url)) ".html" else ".csv"
  path <- file.path(directory, paste0(item$name, extension))
  item$result <- tryCatch({
    response <- httr::GET(item$url, httr::timeout(60))
    httr::stop_for_status(response)
    writeBin(httr::content(response, as = "raw"), path)
    item$retrieved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    item$sha256 <- digest::digest(file = path, algo = "sha256")
    item$bytes <- file.info(path)$size
    if (extension == ".xlsx") {
      sheets <- readxl::excel_sheets(path)
      item$sheets <- sheets
      # Inspect headers in every tab, not just the final rankings tab.
      item$headers <- lapply(stats::setNames(sheets, sheets), function(sheet) {
        x <- suppressMessages(readxl::read_excel(path, sheet = sheet, n_max = 2,
          col_names = FALSE, .name_repair = "minimal"))
        unname(as.list(as.data.frame(x)))
      })
      cat(item$name, ":", length(sheets), "tabs\n")
      inspect <- grep("FBS|TOP", sheets, value = TRUE)
      print(head(inspect, 8))
      print(item$headers[head(inspect, 2)])
    } else if (extension == ".html") {
      html <- paste(readLines(path, warn = FALSE), collapse = "\n")
      pattern <- 'items\\.push\\(\\{name: "([^"]+)", pageUrl: "[^"]+", gid: "(-?[0-9]+)"'
      tabs <- stringr::str_match_all(html, pattern)[[1]]
      if (!nrow(tabs)) stop("Published-sheet tab metadata changed; review the HTML.")
      item$sheets <- tabs[, 2]
      item$headers <- list()
      for (j in which(grepl("^FBS", tabs[, 2]))) {
        url <- paste0(item$url, "/sheet?headers=false&gid=", tabs[j, 3])
        tab_path <- file.path(directory, paste0(item$name, "_", tabs[j, 3], ".html"))
        response <- httr::GET(url, httr::timeout(60))
        httr::stop_for_status(response)
        writeBin(httr::content(response, as = "raw"), tab_path)
        doc <- xml2::read_html(tab_path)
        first_rows <- head(xml2::xml_find_all(doc, ".//table//tr"), 3)
        item$headers[[tabs[j, 2]]] <- lapply(first_rows, function(row)
          xml2::xml_text(xml2::xml_find_all(row, "./td|./th")))
        item$tab_sources[[tabs[j, 2]]] <- list(url = url,
          retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          sha256 = digest::digest(file = tab_path, algo = "sha256"))
      }
      cat(item$name, ":", length(item$sheets), "tabs; FBS headers captured\n")
      print(names(item$headers))
    } else {
      x <- read.csv(path)
      item$columns <- names(x)
      item$rows <- nrow(x)
      cat(item$name, ":", nrow(x), "rows\n")
      print(names(x))
      print(utils::head(x, 2))
    }
    "downloaded_for_review_not_accepted_as_pregame"
  }, error = function(e) paste("error:", conditionMessage(e)))
  inventory[[item$name]] <- item
}
jsonlite::write_json(inventory, file.path(directory, "inventory.json"),
  pretty = TRUE, auto_unbox = TRUE, na = "null")
cat("Source inventory:", file.path(directory, "inventory.json"), "\n")
