args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply the captured public_sources directory.")
directory <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
output <- file.path(directory, "inspection")
if (dir.exists(output)) stop("Refusing to overwrite a completed inspection.")
source("cfb_v2/store.R")
source("cfb_v2/features.R")
source("cfb_v3/experiments/audit_og_inputs.R")
inventory <- jsonlite::fromJSON(file.path(directory, "inventory.json"), simplifyVector = FALSE)
dir.create(output)
rows <- list(); summary <- list()
required <- c("Team", "SP+", "Off. SP+", "Def. SP+")
for (name in grep("^connelly_", names(inventory), value = TRUE)) {
  item <- inventory[[name]]
  if (!identical(item$result, "downloaded_for_review_not_accepted_as_pregame"))
    stop("Incomplete source capture: ", name)
  season <- as.integer(sub("connelly_", "", name))
  sheets <- unlist(item$sheets)
  for (sheet in sheets[grepl("FBS", sheets, fixed = TRUE)]) {
    headers <- unlist(item$headers[[sheet]])
    has_components <- all(required %in% headers)
    key <- paste(name, sheet)
    summary[[key]] <- data.frame(season = season, sheet = sheet,
      has_components = has_components, teams = 0L, complete_numeric = 0L)
    if (!has_components) next
    source_url <- item$url
    retrieved_at <- item$retrieved_at
    sha256 <- item$sha256
    if (season == 2023L) {
      tab <- item$tab_sources[[sheet]]
      gid <- sub(".*gid=", "", tab$url)
      path <- file.path(directory, paste0(name, "_", gid, ".html"))
      stopifnot(identical(digest::digest(file = path, algo = "sha256"), tab$sha256))
      tables <- rvest::html_table(xml2::read_html(path), header = FALSE, fill = TRUE, convert = FALSE)
      stopifnot(length(tables) == 1L)
      raw <- as.data.frame(tables[[1]])
      header <- which(apply(raw, 1, function(row) all(required %in% row)))
      stopifnot(length(header) == 1L)
      data <- raw[seq.int(header + 1L, nrow(raw)), , drop = FALSE]
      names(data) <- unlist(raw[header, ], use.names = FALSE)
      source_url <- tab$url; retrieved_at <- tab$retrieved_at; sha256 <- tab$sha256
    } else {
      path <- file.path(directory, paste0(name, ".xlsx"))
      stopifnot(identical(digest::digest(file = path, algo = "sha256"), item$sha256))
      data <- suppressMessages(readxl::read_excel(path, sheet = sheet,
        col_types = "text", .name_repair = "minimal"))
    }
    sp <- og_extract_public_sp(data)
    sp$season <- season; sp$snapshot_label <- sheet
    sp$source_url <- source_url; sp$source_sha256 <- sha256
    sp$retrieved_at <- retrieved_at
    sp$available_at <- NA_character_; sp$through_at <- NA_character_
    sp$evidence_reviewed <- FALSE
    sp$gate_status <- if (grepl("FINAL", sheet, fixed = TRUE)) "rejected_final_snapshot" else "review_required"
    rows[[key]] <- sp
    summary[[key]]$teams <- nrow(sp)
    summary[[key]]$complete_numeric <- sum(is.finite(sp$rating) &
      is.finite(sp$offense_rating) & is.finite(sp$defense_rating))
  }
}
summary <- do.call(rbind, summary)
sp <- do.call(rbind, rows)
write.csv(summary, file.path(output, "table_coverage.csv"), row.names = FALSE)
write.csv(sp, file.path(output, "recovered_sp_candidates.csv"), row.names = FALSE)
old <- readRDS("cfb_v3/output/experiments/model1_inputs/original_workflow_inputs.rds")$sp
old$team <- canonical_team(old$team)
old <- old[old$year == 2024L, ]
final <- sp[sp$season == 2024L & sp$snapshot_label == "FBS FINAL", ]
parity <- merge(final, old, by = "team", suffixes = c("_public_final", "_cached"))
for (column in c("rating", "offense_rating", "defense_rating"))
  parity[[paste0(column, "_equal")]] <- abs(parity[[paste0(column, "_public_final")]] -
    parity[[paste0(column, "_cached")]]) < 1e-8
write.csv(parity, file.path(output, "2024_final_sp_parity.csv"), row.names = FALSE)
cat("Recovered numeric table coverage (NOT certified pregame):\n")
print(summary[summary$has_components, ], row.names = FALSE)
cat("2024 final SP+ parity:", nrow(parity), "matched teams; exact equality counts:\n")
print(colSums(parity[paste0(c("rating", "offense_rating", "defense_rating"), "_equal")]))
cat("Inspection:", output, "\n")
