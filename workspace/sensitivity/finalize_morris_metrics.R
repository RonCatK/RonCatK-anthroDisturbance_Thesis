#!/usr/bin/env Rscript
# Finalize Morris sensitivity analysis without salvage/rerun steps.
# Runs metric collection + effects analysis, then writes a compact QC summary.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

default_opts <- list(
  runs = file.path(project_root, "outputs", "traceability", "suite_runs", "sensitivity_runs.csv"),
  results_dir = file.path(project_root, "outputs", "sensitivity", "results"),
  help = FALSE
)

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) return(opts)
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
      i <- i + 1
      next
    }
    if (grepl("^--runs=", arg, ignore.case = TRUE)) {
      opts$runs <- sub("^--runs=", "", arg, ignore.case = TRUE)
      i <- i + 1
      next
    }
    if (arg %in% c("--runs")) {
      opts$runs <- args[[i + 1]]
      i <- i + 2
      next
    }
    if (grepl("^--results-dir=", arg, ignore.case = TRUE) || grepl("^--results_dir=", arg, ignore.case = TRUE)) {
      opts$results_dir <- sub("^--results[-_]dir=", "", arg, ignore.case = TRUE)
      i <- i + 1
      next
    }
    if (arg %in% c("--results-dir", "--results_dir")) {
      opts$results_dir <- args[[i + 1]]
      i <- i + 2
      next
    }
    warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    i <- i + 1
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/sensitivity/finalize_morris_metrics.R [options]\n",
    "  --runs=PATH         runs.csv path (default outputs/traceability/suite_runs/sensitivity_runs.csv)\n",
    "  --results-dir=PATH  results directory (default outputs/sensitivity/results)\n",
    "  --help              Show this message\n"
  ))
}

run_rscript <- function(script, args = character()) {
  script_path <- file.path(project_root, script)
  if (!file.exists(script_path)) {
    stop("Required script not found: ", script_path, call. = FALSE)
  }
  cmd <- c(script_path, args)
  message("Running: Rscript ", paste(cmd, collapse = " "))
  status <- system2("Rscript", cmd)
  if (!identical(status, 0L)) {
    stop("Command failed (", status, "): Rscript ", paste(cmd, collapse = " "), call. = FALSE)
  }
}

build_qc_summary <- function(step_qc_path, out_csv, out_md) {
  if (!file.exists(step_qc_path)) {
    stop("Missing step QC file: ", step_qc_path, call. = FALSE)
  }
  step_qc <- suppressMessages(readr::read_csv(step_qc_path, show_col_types = FALSE))
  reason_col <- if ("reason_skipped" %in% names(step_qc)) {
    "reason_skipped"
  } else if ("reason" %in% names(step_qc)) {
    "reason"
  } else {
    stop("Step QC is missing reason_skipped/reason column: ", step_qc_path, call. = FALSE)
  }

  step_qc <- step_qc %>%
    mutate(reason = if_else(is.na(.data[[reason_col]]) | .data[[reason_col]] == "", "kept", .data[[reason_col]]))

  counts <- step_qc %>%
    count(reason, name = "n") %>%
    mutate(
      table = "counts_by_reason",
      n = as.integer(.data$n)
    ) %>%
    select(table, reason, n)

  kept_pairs <- step_qc %>%
    filter(.data$reason == "kept") %>%
    mutate(changed_factors = trimws(dplyr::coalesce(.data$changed_factors, ""))) %>%
    tidyr::separate_rows(.data$changed_factors, sep = "\\s*,\\s*") %>%
    filter(nzchar(.data$changed_factors)) %>%
    count(.data$changed_factors, name = "kept_pairs") %>%
    mutate(
      table = "kept_pairs_by_factor",
      changed_param = .data$changed_factors,
      reason = NA_character_,
      n = NA_integer_,
      kept_pairs = as.integer(.data$kept_pairs)
    ) %>%
    select(table, reason, n, changed_param, kept_pairs)

  pick_steps <- function(reason_label, table_name) {
    step_qc %>%
      filter(.data$reason == reason_label) %>%
      transmute(
        table = table_name,
        reason = NA_character_,
        n = NA_integer_,
        changed_param = NA_character_,
        kept_pairs = NA_integer_,
        trajectory_id = .data$trajectory_id,
        from_point = .data$from_point,
        to_point = .data$to_point,
        changed_factors = .data$changed_factors,
        from_run_name = .data$from_run_name,
        to_run_name = .data$to_run_name
      )
  }

  confounded_rows <- pick_steps("confounded", "confounded_steps")
  noop_rows <- pick_steps("no-op", "no_op_steps")
  missing_rows <- pick_steps("missing_data", "missing_steps")

  out <- bind_rows(
    counts,
    kept_pairs,
    confounded_rows,
    noop_rows,
    missing_rows
  )

  required_cols <- c(
    "table", "reason", "n", "changed_param", "kept_pairs",
    "trajectory_id", "from_point", "to_point", "changed_factors",
    "from_run_name", "to_run_name"
  )
  for (col in required_cols) {
    if (!col %in% names(out)) out[[col]] <- NA
  }
  out <- out[, required_cols]

  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, out_csv)

  counts_fmt <- counts %>%
    mutate(reason = as.character(.data$reason), n = as.integer(.data$n)) %>%
    arrange(.data$reason)
  get_n <- function(label) {
    val <- counts_fmt$n[counts_fmt$reason == label][1]
    if (is.na(val)) 0L else val
  }
  md_lines <- c(
    "# Morris step QC summary",
    "",
    sprintf("- kept: %d", get_n("kept")),
    sprintf("- no-op: %d", get_n("no-op")),
    sprintf("- confounded: %d", get_n("confounded")),
    sprintf("- missing: %d", get_n("missing_data"))
  )
  writeLines(md_lines, con = out_md, useBytes = TRUE)
}

opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
if (opts$help) {
  print_usage()
  quit(save = "no", status = 0, runLast = FALSE)
}

run_rscript(
  "workspace/sensitivity/collect_morris_metrics.R",
  c(
    paste0("--runs=", opts$runs),
    paste0("--results-dir=", opts$results_dir)
  )
)

run_rscript("workspace/sensitivity/analyse_morris_effects.R")

step_qc_path <- file.path(opts$results_dir, "morris_step_qc.csv")
qc_csv <- file.path(opts$results_dir, "morris_qc_summary.csv")
qc_md <- file.path(opts$results_dir, "morris_qc_summary.md")

build_qc_summary(step_qc_path, qc_csv, qc_md)

message("Morris metrics finalized. Results under: ", opts$results_dir)
