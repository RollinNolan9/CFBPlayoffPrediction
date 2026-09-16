# The engine retains its existing module paths; v3 data and outputs are isolated.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
root <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
} else getwd()
source(file.path(root, "run_cfb_v2.R"), chdir = FALSE)
