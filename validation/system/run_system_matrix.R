source("validation/system/run_system_suite.R")
library(data.table)

matrix_entrypoint <- getOption(
  "validation.matrix_entrypoint",
  file.path("validation", suite_id, paste0("run_", suite_id, "_matrix.R"))
)

parse_matrix_args <- function(args) {
  opts <- list(
    csv = suite_default_csv,
    scenario_ids = character(0),
    force = FALSE,
    dry_run = FALSE,
    mode = "default",
    show_help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$show_help <- TRUE
    } else if (identical(arg, "--force")) {
      opts$force <- TRUE
    } else if (identical(arg, "--dry-run")) {
      opts$dry_run <- TRUE
    } else if (grepl("^--csv=", arg, ignore.case = TRUE)) {
      opts$csv <- sub("^--csv=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--scenario=", arg, ignore.case = TRUE)) {
      vals <- sub("^--scenario=", "", arg, ignore.case = TRUE)
      vals <- unlist(strsplit(vals, "[,;]"))
      vals <- trimws(vals[nzchar(vals)])
      opts$scenario_ids <- unique(c(opts$scenario_ids, vals))
    } else if (grepl("^--mode=", arg, ignore.case = TRUE)) {
      opts$mode <- tolower(sub("^--mode=", "", arg, ignore.case = TRUE))
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

maybe_print_help_and_quit <- function() {
  entry_abs <- tryCatch({
    if (!nzchar(matrix_entrypoint)) stop("no entry")
    if (!grepl("^[A-Za-z]:|^/", matrix_entrypoint)) {
      file.path(project_root, matrix_entrypoint)
    } else {
      matrix_entrypoint
    }
  }, error = function(...) file.path("validation", suite_id, paste0("run_", suite_id, "_matrix.R")))
  entry_disp <- relative_to_root(entry_abs)
  if (is.na(entry_disp)) entry_disp <- entry_abs
  csv_disp <- relative_to_root(suite_default_csv)
  if (is.na(csv_disp)) csv_disp <- suite_default_csv
  cat(paste0(
    sprintf("Usage: Rscript %s [options]\n", entry_disp),
    sprintf("  --csv=PATH        Override the testing_runs.csv path (default: %s).\n", csv_disp),
    "  --scenario=IDS    Comma-separated list of scenario_ids to run.\n",
    "  --force           Run even if status already SUCCESS or SKIP.\n",
    "  --dry-run         Show which scenarios would run without executing.\n",
    "  --mode=NAME       Pass-through mode for execute_scenarios_from_csv (default: default).\n",
    "  --help            Show this message.\n"
  ))
  quit(save = "no", status = 0, runLast = FALSE)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- parse_matrix_args(args)
  if (opts$show_help) maybe_print_help_and_quit()

  csv_path <- normalizePath(opts$csv, winslash = "/", mustWork = TRUE)
  dt <- fread(csv_path, fill = TRUE)
  if (!"scenario_id" %in% names(dt)) {
    stop("testing_runs.csv is missing the 'scenario_id' column.", call. = FALSE)
  }

  scenario_ids <- opts$scenario_ids
  if (!length(scenario_ids)) {
    if (!"active" %in% names(dt)) {
      dt[, active := TRUE]
    }
    scenario_ids <- dt[active == TRUE, unique(scenario_id)]
  }

  if (!length(scenario_ids)) {
    message("No scenarios selected (active rows not found). Nothing to do.")
    quit(save = "no", status = 0, runLast = FALSE)
  }

  message("Running scenarios: ", paste(scenario_ids, collapse = ", "))
  res <- execute_scenarios_from_csv(
    csv_path = csv_path,
    scenario_ids = scenario_ids,
    force = opts$force,
    dry_run = opts$dry_run,
    mode = opts$mode
  )

  if (opts$dry_run) {
    message("Dry-run complete. testing_runs.csv unchanged.")
    invisible(res)
  } else {
    failCount <- sum(vapply(res$results, function(x) identical(x$status, "FAIL"), logical(1)))
    if (failCount > 0) {
      quit(save = "no", status = 1, runLast = FALSE)
    }
  }
}

main()
