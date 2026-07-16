Sys.setlocale("LC_ALL", "C.UTF-8")
res <- testthat::test_file("cfb_v2/tests/test_v2.R", reporter = "silent")
df <- as.data.frame(res)
bad <- df[df$failed > 0 | df$error, c("test", "failed", "error")]
if (nrow(bad)) print(bad)
cat("TOTAL PASSED:", sum(df$passed), " FAILED:", sum(df$failed),
    " ERRORS:", sum(df$error), "\n")
