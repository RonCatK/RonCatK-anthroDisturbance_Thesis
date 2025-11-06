#!/usr/bin/env Rscript

# Thin wrapper so the central validation runner can launch UA replicates.
# Delegates to validation/ua/run_ua_replicates.R while keeping the original CLI options.

args_full <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
script_dir <- if (length(script_path)) dirname(script_path) else getwd()
project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

args <- commandArgs(trailingOnly = TRUE)

if (any(args %in% c("--help", "-h"))) {
  cat(paste(
    "Usage: Rscript validation/runner.R --suite=ua [options]\n",
    "Delegates to validation/ua/run_ua_replicates.R; pass --help there for full details.\n",
    sep = "\n"
  ))
  quit(save = "no", status = 0, runLast = FALSE)
}

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) {
  stop("Unable to locate Rscript in PATH.", call. = FALSE)
}

ua_script <- normalizePath(file.path("validation", "ua", "run_ua_replicates.R"),
                           winslash = "/", mustWork = TRUE)
status <- system2(rscript, c(ua_script, args))
quit(save = "no", status = status, runLast = FALSE)
