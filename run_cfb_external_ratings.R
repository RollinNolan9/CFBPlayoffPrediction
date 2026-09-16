script <- grep("^--file=", commandArgs(), value = TRUE)
project <- if (length(script)) dirname(normalizePath(sub("^--file=", "", script[1]), winslash = "/")) else getwd()
for (file in c("store.R", "features.R")) source(file.path(project, "cfb_v2", file))
for (file in c("audit_og_inputs.R", "controlled_ats.R", "external_ratings.R"))
  source(file.path(project, "cfb_v3/experiments", file))
args <- commandArgs(trailingOnly = TRUE)
usage <- paste("Usage:\n  Rscript run_cfb_external_ratings.R snapshot --card PATH [--sources PATH]\n",
  " Rscript run_cfb_external_ratings.R score --snapshot DIRECTORY [--snapshot DIRECTORY ...] [--results CSV]\n",
  "Snapshot downloads current ratings; score fetches CFBD finals unless a results CSV is supplied.")
if (!length(args) || args[1] %in% c("help", "--help")) { cat(usage, "\n"); quit(status = 0) }
mode <- args[1]; args <- args[-1]
if (!mode %in% c("snapshot", "score") || !length(args) || length(args) %% 2L) stop(usage)
keys <- args[seq.int(1L, length(args), by = 2L)]
values <- args[seq.int(2L, length(args), by = 2L)]
allowed <- if (mode == "snapshot") c("--card", "--sources") else c("--snapshot", "--results")
if (any(!keys %in% allowed) || any(duplicated(keys[keys != "--snapshot"]))) stop(usage)
option <- function(key, default = NULL) if (key %in% keys) values[keys == key] else default
if (mode == "snapshot") {
  card <- option("--card"); if (is.null(card)) stop(usage)
  result <- external_snapshot(project, card, option("--sources", file.path(project, "cfb_v3/experiments/external_sources.json")))
} else {
  snapshots <- option("--snapshot"); if (is.null(snapshots)) stop(usage)
  result <- external_score(project, snapshots, option("--results"))
}
cat("Benchmark output:", result, "\n")
cat(paste(readLines(file.path(result, "report.md")), collapse = "\n"), "\n")
