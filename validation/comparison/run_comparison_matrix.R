#!/usr/bin/env Rscript

args_full <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
script_dir <- if (length(script_path)) dirname(script_path) else getwd()
project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(project_root)

comparison_root <- file.path(project_root, "validation", "comparison")
old_opts <- options(
  validation.suite_id = "comparison",
  validation.suite_label = "comparison",
  validation.suite_root = comparison_root,
  validation.suite_entrypoint = file.path("validation", "comparison", "run_comparison_suite.R"),
  validation.matrix_entrypoint = file.path("validation", "comparison", "run_comparison_matrix.R"),
  validation.default_csv = file.path(comparison_root, "testing_runs.csv"),
  validation.scratch_root = file.path(project_root, "scratch", "validation", "comparison"),
  validation.cache_root = file.path(project_root, "cache", "validation", "comparison"),
  validation.outputs_root = file.path(project_root, "outputs", "comparison")
)
on.exit(do.call(options, old_opts), add = TRUE)

source(file.path("validation", "system", "run_system_matrix.R"), chdir = FALSE)
