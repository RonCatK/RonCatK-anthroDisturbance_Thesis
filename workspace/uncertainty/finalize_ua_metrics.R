#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

parse_run_names <- function(value) {
  if (is.null(value) || !nzchar(value)) return(character())
  parts <- unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

run_rscript <- function(script, args) {
  script_path <- file.path(project_root, script)
  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path, call. = FALSE)
  }
  cmd <- c(script_path, args)
  status <- system2("Rscript", cmd)
  if (!identical(status, 0L)) {
    stop("Command failed (", status, "): Rscript ", paste(cmd, collapse = " "), call. = FALSE)
  }
}

normalize_path <- function(pathValue) {
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) return(NA_character_)
  expanded <- path.expand(pathValue)
  candidates <- unique(c(expanded, file.path(project_root, expanded)))
  for (cand in candidates) {
    norm <- tryCatch(normalizePath(cand, winslash = "/", mustWork = FALSE), error = function(...) NULL)
    if (!is.null(norm)) return(norm)
  }
  expanded
}

default_random <- paste0("ua_random_", sprintf("%03d", 1:8), collapse = ",")
default_sitesel <- paste0("ua_sitesel_", sprintf("%03d", 1:4), collapse = ",")

option_list <- list(
  optparse::make_option(
    "--suite",
    type = "character",
    default = "uncertainty",
    help = "Suite label passed to collect_ua_metrics (default uncertainty)."
  ),
  optparse::make_option(
    "--base-run",
    type = "character",
    default = "ua_base",
    help = "Run name for the base UA scenario (default ua_base)."
  ),
  optparse::make_option(
    "--random-runs",
    type = "character",
    default = default_random,
    help = "Comma-separated random-design run_name values (default ua_random_001..008)."
  ),
  optparse::make_option(
    "--sitesel-runs",
    type = "character",
    default = default_sitesel,
    help = "Comma-separated site-selection run_name values (default ua_sitesel_001..004)."
  ),
  optparse::make_option(
    "--design-file",
    type = "character",
    default = file.path("workspace", "uncertainty", "config", "ua_design_points.csv"),
    help = "Design CSV for random UA runs (default workspace/uncertainty/config/ua_design_points.csv).",
    metavar = "FILE"
  ),
  optparse::make_option(
    "--sitesel-design-file",
    type = "character",
    default = file.path("workspace", "uncertainty", "config", "ua_site_selection_design_points.csv"),
    help = "Design CSV for site-selection UA runs (default workspace/uncertainty/config/ua_site_selection_design_points.csv).",
    metavar = "FILE"
  ),
  optparse::make_option(
    "--base-config",
    type = "character",
    default = file.path("workspace", "uncertainty", "config", "ua_base.yaml"),
    help = "Base YAML used for traceability in replicates mode (default workspace/uncertainty/config/ua_base.yaml).",
    metavar = "FILE"
  ),
  optparse::make_option(
    "--allow-incomplete-replicates",
    action = "store_true",
    default = FALSE,
    help = "Pass through to collect_ua_metrics (allow incomplete replicates)."
  ),
  optparse::make_option(
    "--allow-duplicate-run-rows",
    action = "store_true",
    default = FALSE,
    help = "Pass through to collect_ua_metrics (dedupe runs.csv rows instead of erroring)."
  ),
  optparse::make_option(
    "--skip-summary",
    action = "store_true",
    default = FALSE,
    help = "Collect metrics only; do not run summarize_ua_results.R."
  )
)

opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

random_runs <- parse_run_names(opts$`random-runs`)
sitesel_runs <- parse_run_names(opts$`sitesel-runs`)
if (!length(random_runs)) stop("At least one --random-runs value is required.", call. = FALSE)
if (!length(sitesel_runs)) stop("At least one --sitesel-runs value is required.", call. = FALSE)

runs_csv <- file.path(project_root, "workspace", "uncertainty", "runs.csv")
if (!file.exists(runs_csv)) stop("Run registry not found: ", runs_csv, call. = FALSE)

design_file <- normalize_path(opts$`design-file`)
if (!file.exists(design_file)) stop("Design CSV not found: ", opts$`design-file`, call. = FALSE)
sitesel_design <- normalize_path(opts$`sitesel-design-file`)
if (!file.exists(sitesel_design)) stop("Site-selection design CSV not found: ", opts$`sitesel-design-file`, call. = FALSE)
base_cfg <- normalize_path(opts$`base-config`)
if (!file.exists(base_cfg)) stop("Base config not found: ", opts$`base-config`, call. = FALSE)

results_dir <- file.path(project_root, "outputs", "uncertainty", "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

passthrough_flags <- character()
if (isTRUE(opts$`allow-incomplete-replicates`)) passthrough_flags <- c(passthrough_flags, "--allow-incomplete-replicates")
if (isTRUE(opts$`allow-duplicate-run-rows`)) passthrough_flags <- c(passthrough_flags, "--allow-duplicate-run-rows")

message("Collecting UA base metrics for run ", opts$`base-run`)
run_rscript(
  "workspace/uncertainty/collect_ua_metrics.R",
  c(
    "--mode=replicates",
    paste0("--suite=", opts$suite),
    paste0("--run-name=", opts$`base-run`),
    paste0("--base-config=", base_cfg),
    passthrough_flags
  )
)

message("Collecting UA random-design metrics for runs: ", paste(random_runs, collapse = ", "))
run_rscript(
  "workspace/uncertainty/collect_ua_metrics.R",
  c(
    "--mode=design",
    paste0("--suite=", opts$suite),
    paste0("--run-name=", paste(random_runs, collapse = ",")),
    paste0("--design-file=", design_file),
    passthrough_flags
  )
)

message("Collecting UA site-selection metrics for runs: ", paste(sitesel_runs, collapse = ", "))
run_rscript(
  "workspace/uncertainty/collect_ua_metrics.R",
  c(
    "--mode=design",
    paste0("--suite=", opts$suite),
    paste0("--run-name=", paste(sitesel_runs, collapse = ",")),
    paste0("--design-file=", sitesel_design),
    passthrough_flags
  )
)

if (!isTRUE(opts$`skip-summary`)) {
  message("Summarizing UA key metrics into ", results_dir)
  run_rscript(
    "workspace/uncertainty/summarize_ua_results.R",
    c(
      paste0("--base-run=", opts$`base-run`),
      paste0("--random-runs=", paste(random_runs, collapse = ",")),
      paste0("--sitesel-runs=", paste(sitesel_runs, collapse = ",")),
      paste0("--results-dir=", results_dir)
    )
  )
} else {
  message("Skipping UA summary step (--skip-summary).")
}

message("UA metric pipeline complete. Results under: ", results_dir)
