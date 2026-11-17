#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

option_list <- list(
  optparse::make_option(
    c("--input"),
    type = "character",
    default = file.path("outputs", "uncertainty", "results", "ua_run_metrics_ua_base.csv"),
    help = "UA run metrics CSV to validate.",
    metavar = "FILE"
  ),
  optparse::make_option(
    c("--run-name"),
    type = "character",
    default = NA_character_,
    help = "Optional run_name to filter before QA checks.",
    metavar = "RUN"
  )
)

resolve_path <- function(path) {
  candidates <- c(path, file.path(project_root, path))
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) return(path)
  normalizePath(hit, winslash = "/", mustWork = TRUE)
}

expected_years_by_run <- function(rep_years) {
  rep_years <- rep_years %>%
    mutate(year_key = vapply(years, function(x) paste(x, collapse = ","), character(1)))
  rep_years %>%
    group_by(run_name, year_key) %>%
    summarise(
      n_reps = n(),
      years = list(first(years)),
      years_len = lengths(years),
      .groups = "drop"
    ) %>%
    arrange(run_name, desc(n_reps), desc(years_len), year_key) %>%
    group_by(run_name) %>%
    slice(1) %>%
    ungroup() %>%
    select(run_name, expected_years = years)
}

main <- function() {
  opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
  input_path <- resolve_path(opts$input)
  if (!file.exists(input_path)) {
    stop("Input CSV not found: ", opts$input, call. = FALSE)
  }
  message("QA input: ", input_path)
  df <- suppressMessages(readr::read_csv(input_path, show_col_types = FALSE))
  if (!nrow(df)) stop("Input CSV has no rows.", call. = FALSE)

  if (!is.na(opts$`run-name`)) {
    df <- df %>% filter(tolower(run_name) == tolower(opts$`run-name`))
  }
  if (!nrow(df)) stop("No rows after run_name filter.", call. = FALSE)

  duplicate_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
  if (nrow(duplicate_rows)) {
    stop(sprintf("Found %d duplicate rows in UA metrics output.", nrow(duplicate_rows)), call. = FALSE)
  }

  required_trace <- c("clusterDistance", "useClusterMethod", "siteSelectionAsDistributing")
  missing_cols <- setdiff(required_trace, names(df))
  if (length(missing_cols)) {
    stop("Missing required traceability columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  trace_na <- df %>%
    summarise(across(all_of(required_trace), ~ all(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "param", values_to = "all_na") %>%
    filter(all_na)
  if (nrow(trace_na)) {
    stop("Traceability columns are all NA: ", paste(trace_na$param, collapse = ", "), call. = FALSE)
  }

  if (!all(c("run_name", "replicate", "year") %in% names(df))) {
    stop("Missing required columns for replicate completeness checks.", call. = FALSE)
  }
  if (!"seed" %in% names(df)) df$seed <- NA_real_
  rep_years <- df %>%
    filter(!is.na(year)) %>%
    distinct(run_name, replicate, seed, year) %>%
    group_by(run_name, replicate, seed) %>%
    summarise(years = list(sort(unique(year))), .groups = "drop")
  if (nrow(rep_years)) {
    expected <- expected_years_by_run(rep_years)
    rep_status <- rep_years %>%
      left_join(expected, by = "run_name") %>%
      mutate(
        missing_years = map2(years, expected_years, function(actual, expected) {
          if (is.null(actual) || all(is.na(actual))) actual <- numeric()
          if (is.null(expected) || all(is.na(expected))) expected <- numeric()
          setdiff(expected, actual)
        })
      )
    incomplete <- rep_status %>% filter(lengths(missing_years) > 0)
    if (nrow(incomplete)) {
      stop(sprintf("Found %d incomplete replicates missing expected years.", nrow(incomplete)), call. = FALSE)
    }
  }

  message("QA checks passed.")
}

tryCatch(main(), error = function(e) {
  message("qc_ua_metrics.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
