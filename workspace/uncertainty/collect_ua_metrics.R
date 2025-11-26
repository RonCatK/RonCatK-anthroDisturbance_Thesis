#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(optparse)
})

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

option_list <- list(
  optparse::make_option(
    c("--mode"),
    type = "character",
    help = "Mode: replicates | design.",
    metavar = "MODE"
  ),
  optparse::make_option(
    c("--suite"),
    type = "character",
    default = "uncertainty",
    help = "Suite label used in run registry (default: uncertainty).",
    metavar = "SUITE"
  ),
  optparse::make_option(
    c("--run-name"),
    type = "character",
    default = NA_character_,
    help = "Run name to analyse (default ua_base for replicates mode).",
    metavar = "RUN"
  ),
  optparse::make_option(
    c("--design-file"),
    type = "character",
    default = file.path("workspace", "uncertainty", "ua_design_points.csv"),
    help = "Design CSV (required for design mode).",
    metavar = "FILE"
  )
)

parse_run_names <- function(value) {
  if (is.null(value) || !nzchar(value)) return(character())
  parts <- unlist(strsplit(value, ","), use.names = FALSE)
  trimmed <- trimws(parts)
  trimmed[nzchar(trimmed)]
}

make_run_label <- function(run_names) {
  if (!length(run_names)) return("ua_runs")
  if (length(run_names) == 1L) return(run_names[[1]])
  sanitized <- gsub("[^A-Za-z0-9_-]+", "_", run_names)
  paste0("batch_", paste0(sanitized, collapse = "_"))
}

parse_args <- function() {
  opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
  run_names <- parse_run_names(opts[["run-name"]])
  if (is.null(opts$mode) || !nzchar(opts$mode)) {
    stop("Argument --mode is required (replicates | design).", call. = FALSE)
  }
  opts$mode <- match.arg(tolower(opts$mode), c("replicates", "design"))
  if (identical(opts$mode, "replicates")) {
    if (!length(run_names)) run_names <- "ua_base"
  } else if (!length(run_names)) {
    stop("In design mode, --run-name must be supplied (comma-separated allowed).", call. = FALSE)
  }
  opts$run_names <- run_names
  opts$run_name <- if (length(run_names)) run_names[[1]] else NA_character_
  opts
}

normalize_path <- function(pathValue) {
  if (length(pathValue) > 1) {
    return(vapply(pathValue, normalize_path, character(1), USE.NAMES = FALSE))
  }
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) return(NA_character_)
  expanded <- path.expand(pathValue)
  candidates <- unique(c(expanded, file.path(project_root, expanded)))
  for (cand in candidates) {
    norm <- tryCatch(normalizePath(cand, winslash = "/", mustWork = FALSE), error = function(...) NULL)
    if (!is.null(norm)) return(norm)
  }
  expanded
}

find_run_registry <- function() {
  candidates <- c(
    file.path(project_root, "workspace", "uncertainty", "runs.csv")
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop("No run registry found (checked workspace/uncertainty/runs.csv).", call. = FALSE)
  }
  hit
}

is_main_call <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  if (identical(expr[[1]], as.symbol("main"))) return(TRUE)
  parts <- as.list(expr)
  any(vapply(parts, function(p) {
    is.call(p) && identical(p[[1]], as.symbol("main"))
  }, logical(1)))
}

load_helper_env <- function(script_path) {
  if (!file.exists(script_path)) return(NULL)
  env <- new.env()
  exprs <- parse(script_path)
  exprs <- exprs[!vapply(exprs, is_main_call, logical(1))]
  for (ex in exprs) {
    eval(ex, envir = env)
  }
  env
}

metric_group_from_id <- function(metric_id) {
  case_when(
    str_detect(metric_id, "disagreement|agreement") ~ "agreement",
    str_detect(metric_id, "patch|segment|nn_distance") ~ "config",
    str_detect(metric_id, "length") ~ "footprint",
    str_detect(metric_id, "area") ~ "area",
    TRUE ~ "other"
  )
}

standardize_runs <- function(df, suite_default) {
  nm <- names(df)
  nml <- tolower(nm)
  rename_first <- function(data, candidates, to) {
    cand <- candidates[candidates %in% names(data)][1]
    if (!is.na(cand)) dplyr::rename(data, !!to := !!sym(cand)) else data
  }
  df <- df %>%
    rename_first(nm[nml == "suite"], "suite") %>%
    rename_first(nm[nml %in% c("run_name", "run", "scenario_id", "scenario")], "run_name") %>%
    rename_first(nm[nml %in% c("replicate", "rep", "rep_id")], "replicate") %>%
    rename_first(nm[nml == "seed"], "seed") %>%
    rename_first(nm[nml %in% c("output_path", "output_dir", "output_folder")], "output_path") %>%
    rename_first(nm[nml == "status"], "status") %>%
    rename_first(nm[nml %in% c("design_id", "designid", "sample_id", "point_id")], "design_id")
  if (!"suite" %in% names(df)) df$suite <- suite_default
  if (!"status" %in% names(df)) df$status <- NA_character_
  df
}

expand_output_rows <- function(row) {
  paths <- str_split(row$output_path %||% "", ";")[[1]]
  paths <- trimws(paths)
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(tibble())
  }
  log_paths <- if ("log_file" %in% names(row)) str_split(row$log_file %||% "", ";")[[1]] else character(0)
  log_paths <- trimws(log_paths)
  tibble(
    suite = row$suite %||% NA_character_,
    run_name = row$run_name %||% NA_character_,
    replicate = if (!is.null(row$replicate) && !is.na(row$replicate)) row$replicate else seq_along(paths),
    seed = row$seed %||% NA_real_,
    output_path = paths,
    log_file = if (length(log_paths)) log_paths[seq_along(paths)] else NA_character_,
    status = row$status %||% NA_character_,
    design_id = row$design_id %||% NA_real_
  )
}

read_runs_registry <- function(registry_path, suite_val, run_name_val) {
  message(sprintf("Reading run registry: %s", registry_path))
  raw <- suppressMessages(readr::read_csv(registry_path, show_col_types = FALSE))
  std <- standardize_runs(raw, suite_default = suite_val)
  filtered <- std %>%
    filter(tolower(.data$suite) == tolower(suite_val))
  if (length(run_name_val)) {
    filtered <- filtered %>% filter(tolower(.data$run_name) %in% tolower(run_name_val))
  }
  expanded <- filtered %>%
    split(seq_len(nrow(.))) %>%
    map_dfr(expand_output_rows)
  if (!nrow(expanded)) return(tibble())
  expanded <- expanded %>%
    mutate(
      output_path = normalize_path(output_path),
      output_exists = dir.exists(output_path),
      status_flag = tolower(status),
      is_success = output_exists | status_flag %in% c("success", "ok", "passed", "done")
    ) %>%
    filter(is_success)
  expanded %>%
    mutate(
      replicate = as.integer(replicate),
      seed = suppressWarnings(as.numeric(seed)),
      run_id = ifelse(
        is.na(replicate),
        run_name,
        paste0(run_name, "_rep_", sprintf("%03d", replicate))
      )
    )
}

read_design_file <- function(path) {
  candidates <- c(path, file.path(project_root, path), file.path(project_root, "workspace", "uncertainty", "config", basename(path)))
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop("Design file not found: ", path, call. = FALSE)
  }
  message(sprintf("Reading UA design metadata from %s", hit))
  design <- suppressMessages(readr::read_csv(hit, show_col_types = FALSE))
  rename_first <- function(data, candidates, to) {
    cand <- candidates[candidates %in% names(data)][1]
    if (!is.na(cand)) dplyr::rename(data, !!to := !!sym(cand)) else data
  }
  design <- design %>%
    rename_first(c("design_id", "sample_id", "point_id"), "design_id") %>%
    mutate(
      design_id = if ("design_id" %in% names(design)) design_id else row_number(),
      totalDisturbanceRate = if ("totalDisturbanceRate" %in% names(design)) totalDisturbanceRate else NA_real_,
      clusterDistance = if ("clusterDistance" %in% names(design)) clusterDistance else NA_real_,
      useClusterMethod = if ("useClusterMethod" %in% names(design)) useClusterMethod else NA,
      siteSelectionAsDistributing = if ("siteSelectionAsDistributing" %in% names(design)) siteSelectionAsDistributing else NA_character_
    )
  design
}

compute_ua_metrics_for_run <- function(run_row, ua_env) {
  out_dir <- run_row$output_path
  rep_id <- run_row$replicate %||% 1L
  run_nm <- run_row$run_name %||% "ua_run"
  ua_tbl <- tryCatch({
    if (is.null(ua_env)) tibble() else {
      dt <- ua_env$metrics_from_output_dir(out_dir, scenario_id = run_nm, rep_id = rep_id)
      as_tibble(dt)
    }
  }, error = function(e) {
    message(sprintf("UA metric extraction failed for %s: %s", out_dir, conditionMessage(e)))
    tibble()
  })
  if (nrow(ua_tbl)) {
    ua_tbl <- ua_tbl %>%
      rename(metric_id = metric) %>%
      mutate(
        metric_group = metric_group_from_id(metric_id),
        sector = if ("sector" %in% names(.)) sector else NA_character_
      ) %>%
      select(metric_group, metric_id, year, sector, value)
  }
  ua_tbl
}

summarise_replicates <- function(df) {
  df %>%
    group_by(metric_group, metric_id, sector, year) %>%
    summarise(
      n_runs = n(),
      value_mean = mean(value, na.rm = TRUE),
      value_median = median(value, na.rm = TRUE),
      value_sd = sd(value, na.rm = TRUE),
      value_p05 = quantile(value, 0.05, na.rm = TRUE, names = FALSE),
      value_p95 = quantile(value, 0.95, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

main <- function() {
  opts <- parse_args()
  registry_path <- find_run_registry()
  design_path <- opts[["design-file"]]
  run_name <- opts$run_name
  run_names <- opts$run_names
  run_label <- make_run_label(run_names)
  suite <- opts$suite
  run_display <- if (length(run_names)) paste(run_names, collapse = ",") else run_name
  if (identical(opts$mode, "design") && (is.null(run_name) || is.na(run_name) || !nzchar(run_name))) {
    stop("In design mode, --run-name must be supplied to match the UA run folder.", call. = FALSE)
  }
  runs_df <- read_runs_registry(registry_path, suite_val = suite, run_name_val = run_names)
  if (!nrow(runs_df)) stop("No successful runs found for suite=", suite, " run_name=", run_display, call. = FALSE)
  if (!"design_id" %in% names(runs_df)) runs_df$design_id <- NA_integer_
  if (opts$mode == "replicates") {
    runs_df <- runs_df %>% mutate(design_id = 1L)
  } else {
    design_df <- read_design_file(design_path)
    join_by <- if ("run_name" %in% names(design_df)) "run_name" else "design_id"
    runs_df <- runs_df %>%
      left_join(design_df, by = join_by)
    if ("design_id.x" %in% names(runs_df) || "design_id.y" %in% names(runs_df)) {
      runs_df <- runs_df %>%
        mutate(design_id = dplyr::coalesce(
          if ("design_id.x" %in% names(.)) .data$design_id.x else NA_integer_,
          if ("design_id.y" %in% names(.)) .data$design_id.y else NA_integer_
        )) %>%
        select(-any_of(c("design_id.x", "design_id.y")))
    }
    if (!nrow(runs_df)) stop("Design join produced no rows; check design/run_name alignment.", call. = FALSE)
    dropped <- sum(is.na(runs_df$design_id))
    if (dropped > 0) {
      message(sprintf("Dropping %d runs without design metadata.", dropped))
      runs_df <- runs_df %>% filter(!is.na(design_id))
    }
  }
  ua_env <- load_helper_env(file.path(project_root, "workspace", "uncertainty", "ua_metrics.R"))
  if (is.null(ua_env)) stop("Failed to load workspace/uncertainty/ua_metrics.R helpers.", call. = FALSE)

  message(sprintf(
    "Computing metrics for %d runs (mode=%s, run_name(s)=[%s]).",
    nrow(runs_df),
    opts$mode,
    run_display
  ))
  run_metrics <- runs_df %>%
    mutate(.row = row_number()) %>%
    split(.$.row) %>%
    map_dfr(function(row) {
      row <- row %>% select(-.row)
      metrics <- compute_ua_metrics_for_run(row, ua_env)
      if (!nrow(metrics)) return(tibble())
      metrics %>%
        mutate(
          mode = opts$mode,
          suite = suite,
          run_name = row$run_name,
          run_id = row$run_id,
          seed = row$seed,
          replicate = row$replicate,
          design_id = row$design_id %||% NA_integer_,
          totalDisturbanceRate = row$totalDisturbanceRate %||% NA_real_,
          clusterDistance = row$clusterDistance %||% NA_real_,
          useClusterMethod = row$useClusterMethod %||% NA,
          siteSelectionAsDistributing = row$siteSelectionAsDistributing %||% NA_character_
        )
    })

  if (!nrow(run_metrics)) stop("No metrics could be computed from available outputs.", call. = FALSE)

  results_dir <- file.path(project_root, "workspace", "uncertainty", "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  run_out_csv <- file.path(results_dir, sprintf("ua_run_metrics_%s.csv", run_label))
  readr::write_csv(run_metrics, run_out_csv)

  if (opts$mode == "replicates") {
    rep_summary <- summarise_replicates(run_metrics)
    rep_out_csv <- file.path(results_dir, sprintf("ua_replicates_summary_%s.csv", run_label))
    readr::write_csv(rep_summary, rep_out_csv)
    message(sprintf("Replicates summary written: %s", rep_out_csv))
  } else {
    design_summary <- run_metrics %>%
      group_by(design_id, metric_group, metric_id, sector, year) %>%
      summarise(
        n_runs = n(),
        value_mean = mean(value, na.rm = TRUE),
        value_median = median(value, na.rm = TRUE),
        value_sd = sd(value, na.rm = TRUE),
        value_p05 = quantile(value, 0.05, na.rm = TRUE, names = FALSE),
        value_p95 = quantile(value, 0.95, na.rm = TRUE, names = FALSE),
        .groups = "drop"
      ) %>%
      left_join(
        run_metrics %>%
          distinct(design_id, totalDisturbanceRate, clusterDistance, useClusterMethod, siteSelectionAsDistributing),
        by = "design_id"
      )
    global_summary <- design_summary %>%
      group_by(metric_group, metric_id, sector, year) %>%
      summarise(
        n_designs = n(),
        median_of_medians = median(value_median, na.rm = TRUE),
        p05_median = quantile(value_median, 0.05, na.rm = TRUE, names = FALSE),
        p95_median = quantile(value_median, 0.95, na.rm = TRUE, names = FALSE),
        mean_median = mean(value_median, na.rm = TRUE),
        sd_median = sd(value_median, na.rm = TRUE),
        .groups = "drop"
      )
    design_out_csv <- file.path(results_dir, sprintf("ua_design_summary_%s.csv", run_label))
    global_out_csv <- file.path(results_dir, sprintf("ua_global_summary_%s.csv", run_label))
    readr::write_csv(design_summary, design_out_csv)
    readr::write_csv(global_summary, global_out_csv)
    message(sprintf("Design summary written: %s", design_out_csv))
    message(sprintf("Global UA summary written: %s", global_out_csv))
  }

  per_design_counts <- run_metrics %>% count(design_id, name = "n_runs")
  message(sprintf(
    "Computed metrics for %d runs (unique design ids: %d). Runs per design (summary): %s",
    n_distinct(run_metrics$run_id),
    n_distinct(run_metrics$design_id),
    paste(capture.output(summary(per_design_counts$n_runs)), collapse = "; ")
  ))
  message(sprintf("Run-level metrics: %s", run_out_csv))
}

tryCatch(main(), error = function(e) {
  message("collect_ua_metrics.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
