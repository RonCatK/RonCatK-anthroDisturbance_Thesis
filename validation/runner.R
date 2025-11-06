#!/usr/bin/env Rscript

# Central validation runner. Delegates to system, rates, or UA suites while keeping the
# original per-suite scripts intact. Use --suite=<system|rates|ua> to select a suite.

print_main_help <- function() {
  cat(paste(
    "Usage: Rscript validation/runner.R [--suite=S] [suite-specific options]\n",
    "  --suite=system   Execute verification system tests (default).\n",
    "  --suite=rates    Execute disturbance rate verification scenarios.\n",
    "  --suite=ua       Execute uncertainty analysis runs.\n",
    "\n",
    "Additional options are delegated to each suite:\n",
    "  system suite  -> validation/system/run_system_suite.R --help\n",
    "  rates suite   -> validation/rates/scenarios/run_rates_suite.R --help\n",
    "  ua suite      -> validation/ua/run_ua_suite.R --help\n",
    sep = "\n"
  ))
}

args_full <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
script_dir <- if (length(script_path)) dirname(script_path) else getwd()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
setwd(project_root)

args <- commandArgs(trailingOnly = TRUE)
suite <- "system"
suite_args <- character(0)

if (length(args)) {
  for (arg in args) {
    if (grepl("^--suite=", arg, ignore.case = TRUE)) {
      suite <- tolower(sub("^--suite=", "", arg, ignore.case = TRUE))
    } else {
      suite_args <- c(suite_args, arg)
    }
  }
}

if (!length(args) && interactive()) {
  print_main_help()
  quit(save = "no", status = 0, runLast = FALSE)
}

if (!nzchar(suite) || !suite %in% c("system", "rates", "ua")) {
  stop("Unknown --suite value: ", suite, call. = FALSE)
}

if (!length(args) && !interactive()) {
  # No args given; fall back to default suite with defaults (system).
  suite_args <- character(0)
}

run_system_suite <- function(pass_args) {
  sys_runner <- file.path(project_root, "validation", "system", "run_system_suite.R")
  if (!file.exists(sys_runner)) {
    stop("System suite runner not found at ", sys_runner, call. = FALSE)
  }
  source(sys_runner, local = TRUE)
  opts <- parse_cli_args(pass_args)
  if (opts$show_help) {
    cat(paste(
      "Usage: Rscript validation/runner.R --suite=system [--csv=PATH] [--scenario=id1,id2] [--force] [--dry-run]\n",
      "  --csv=PATH        Path to CSV file (default: validation/system/testing_runs.csv)\n",
      "  --scenario=IDS    Comma-separated scenario_id list to run (defaults to all active pending)\n",
      "  --force           Run even if status already SUCCESS or SKIP\n",
      "  --mode=NAME       Mode selector: default|respect|all (default: default)\n",
      "  --dry-run         Show which scenarios would run without executing\n",
      sep = ""
    ))
    quit(save = "no", status = 0, runLast = FALSE)
  }
  csvPath <- normalizePath(opts$csv, winslash = "/", mustWork = FALSE)
  res <- execute_scenarios_from_csv(
    csv_path = csvPath,
    scenario_ids = opts$scenario_ids,
    force = opts$force,
    dry_run = opts$dry_run,
    mode = opts$mode
  )
  if (!opts$dry_run) {
    failCount <- sum(vapply(res$results, function(x) identical(x$status, "FAIL"), logical(1)))
    return(if (failCount > 0) 1L else 0L)
  }
  0L
}

run_external_suite <- function(script_rel, pass_args) {
  rscript <- Sys.which("Rscript")
  if (!nzchar(rscript)) {
    stop("Unable to locate Rscript in PATH.", call. = FALSE)
  }
  target <- normalizePath(file.path(project_root, script_rel), winslash = "/", mustWork = TRUE)
  cmd_args <- c(target, pass_args)
  status <- system2(rscript, cmd_args)
  status
}

rc <- switch(
  suite,
  system = run_system_suite(suite_args),
  rates = run_external_suite(file.path("validation", "rates", "scenarios", "run_rates_suite.R"), suite_args),
  ua    = run_external_suite(file.path("validation", "ua", "run_ua_suite.R"), suite_args)
)

quit(save = "no", status = as.integer(rc), runLast = FALSE)
