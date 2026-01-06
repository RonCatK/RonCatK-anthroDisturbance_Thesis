#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(optparse)
  library(yaml)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L) return(b)
  if (length(a) == 1L && is.na(a)) return(b)
  a
}

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
    default = file.path("workspace", "uncertainty", "config", "ua_design_points.csv"),
    help = "Design CSV (required for design mode).",
    metavar = "FILE"
  ),
  optparse::make_option(
    c("--base-config"),
    type = "character",
    default = file.path("workspace", "uncertainty", "config", "ua_base.yaml"),
    help = "Base YAML config for traceability metadata in replicates mode.",
    metavar = "FILE"
  ),
  optparse::make_option(
    c("--allow-duplicate-run-rows"),
    action = "store_true",
    default = FALSE,
    help = "Allow duplicate run registry rows (dedupe keep-first with warning)."
  ),
  optparse::make_option(
    c("--allow-incomplete-replicates"),
    action = "store_true",
    default = FALSE,
    help = "Allow incomplete replicates (flag and exclude from summaries)."
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

ensure_qa_dir <- function() {
  qa_dir <- file.path(project_root, "outputs", "uncertainty", "results", "qa")
  dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
  qa_dir
}

write_qa_csv <- function(df, filename) {
  qa_dir <- ensure_qa_dir()
  qa_path <- file.path(qa_dir, filename)
  readr::write_csv(df, qa_path)
  qa_path
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
    rename_first(nm[nml %in% c("config_file", "config", "cfg", "config_path")], "config_file") %>%
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
  if (!is.null(row$replicate) && !is.na(row$replicate) && length(paths) > 1) {
    stop(
      sprintf(
        "Malformed runs registry: replicate %s has multiple output_path entries. ",
        row$replicate
      ),
      "Replicate rows must include exactly one output_path; omit replicate to enumerate paths.",
      call. = FALSE
    )
  }
  original_paths <- paths
  paths <- unique(paths)
  log_paths <- if ("log_file" %in% names(row)) str_split(row$log_file %||% "", ";")[[1]] else character(0)
  log_paths <- trimws(log_paths)
  if (length(log_paths)) {
    log_paths <- log_paths[seq_along(original_paths)]
    log_paths <- log_paths[match(paths, original_paths)]
  }
  tibble(
    suite = row$suite %||% NA_character_,
    run_name = row$run_name %||% NA_character_,
    replicate = if (!is.null(row$replicate) && !is.na(row$replicate)) row$replicate else seq_along(paths),
    seed = row$seed %||% NA_real_,
    config_file = row$config_file %||% NA_character_,
    output_path = paths,
    log_file = if (length(log_paths)) log_paths[seq_along(paths)] else NA_character_,
    status = row$status %||% NA_character_,
    design_id = row$design_id %||% NA_real_
  )
}

check_duplicate_run_rows <- function(df, run_label, allow_duplicates) {
  key_cols <- c("suite", "run_name", "replicate", "seed", "output_path")
  if (!all(key_cols %in% names(df))) return(df)
  dupes <- df %>%
    group_by(across(all_of(key_cols))) %>%
    mutate(duplicate_count = n()) %>%
    filter(duplicate_count > 1) %>%
    ungroup()
  if (nrow(dupes)) {
    qa_path <- write_qa_csv(dupes, sprintf("ua_qc_duplicate_run_rows_%s.csv", run_label))
    if (!isTRUE(allow_duplicates)) {
      stop(
        sprintf("Duplicate run registry rows detected (%d). See %s.", nrow(dupes), qa_path),
        "Use --allow-duplicate-run-rows to dedupe with warning.",
        call. = FALSE
      )
    }
    warning(
      sprintf("Duplicate run registry rows detected (%d). Dedupe keep-first (see %s).", nrow(dupes), qa_path),
      call. = FALSE
    )
    df <- df %>%
      mutate(.row = row_number()) %>%
      distinct(across(all_of(key_cols)), .keep_all = TRUE) %>%
      arrange(.row) %>%
      select(-.row)
  }
  df
}

read_runs_registry <- function(registry_path, suite_val, run_name_val, run_label, allow_duplicates) {
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
      config_file = normalize_path(config_file),
      output_exists = dir.exists(output_path),
      status_flag = tolower(status),
      is_success = output_exists | status_flag %in% c("success", "ok", "passed", "done")
    ) %>%
    filter(is_success) %>%
    mutate(
      replicate = as.integer(replicate),
      seed = suppressWarnings(as.numeric(seed))
    )
  expanded <- check_duplicate_run_rows(expanded, run_label = run_label, allow_duplicates = allow_duplicates)
  expanded %>%
    mutate(
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
  design_raw <- suppressMessages(readr::read_csv(hit, show_col_types = FALSE))
  missing_cols <- setdiff(
    c("totalDisturbanceRate", "clusterDistance", "useClusterMethod", "siteSelectionAsDistributing"),
    names(design_raw)
  )
  if (length(missing_cols)) {
    warning(
      sprintf("Design metadata missing columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }
  rename_first <- function(data, candidates, to) {
    cand <- candidates[candidates %in% names(data)][1]
    if (!is.na(cand)) dplyr::rename(data, !!to := !!sym(cand)) else data
  }
  has_design_id <- any(c("design_id", "sample_id", "point_id") %in% names(design_raw))
  design <- design_raw %>%
    rename_first(c("design_id", "sample_id", "point_id"), "design_id") %>%
    mutate(
      design_id = if (has_design_id) design_id else row_number(),
      totalDisturbanceRate = if ("totalDisturbanceRate" %in% names(design_raw)) totalDisturbanceRate else NA_real_,
      clusterDistance = if ("clusterDistance" %in% names(design_raw)) clusterDistance else NA_real_,
      useClusterMethod = if ("useClusterMethod" %in% names(design_raw)) useClusterMethod else NA,
      siteSelectionAsDistributing = if ("siteSelectionAsDistributing" %in% names(design_raw)) siteSelectionAsDistributing else NA_character_
    )
  design
}

read_base_config <- function(path) {
  candidates <- c(path, file.path(project_root, path), file.path(project_root, "workspace", "uncertainty", "config", basename(path)))
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop("Base config not found: ", path, call. = FALSE)
  }
  message(sprintf("Reading UA base config from %s", hit))
  cfg <- yaml::read_yaml(hit)
  params <- cfg$params$anthroDisturbance_Generator %||% list()
  list(
    totalDisturbanceRate = params$totalDisturbanceRate %||% NA_real_,
    clusterDistance = params$clusterDistance %||% NA_real_,
    useClusterMethod = params$useClusterMethod %||% NA,
    siteSelectionAsDistributing = params$siteSelectionAsDistributing %||% NA_character_
  )
}

read_config_traceability <- function(config_path) {
  if (is.null(config_path) || is.na(config_path) || !nzchar(config_path)) {
    return(list(
      totalDisturbanceRate = NA_real_,
      clusterDistance = NA_real_,
      useClusterMethod = NA,
      siteSelectionAsDistributing = NA_character_
    ))
  }
  hit <- normalize_path(config_path)
  if (is.null(hit) || is.na(hit) || !file.exists(hit)) {
    warning("Config file not found: ", config_path, call. = FALSE)
    return(list(
      totalDisturbanceRate = NA_real_,
      clusterDistance = NA_real_,
      useClusterMethod = NA,
      siteSelectionAsDistributing = NA_character_
    ))
  }
  cfg <- yaml::read_yaml(hit)
  params <- cfg$params$anthroDisturbance_Generator %||% list()
  list(
    totalDisturbanceRate = params$totalDisturbanceRate %||% NA_real_,
    clusterDistance = params$clusterDistance %||% NA_real_,
    useClusterMethod = params$useClusterMethod %||% NA,
    siteSelectionAsDistributing = params$siteSelectionAsDistributing %||% NA_character_
  )
}

populate_traceability_from_configs <- function(df) {
  if (!"config_file" %in% names(df)) return(df)
  if (!"totalDisturbanceRate" %in% names(df)) df$totalDisturbanceRate <- NA_real_
  if (!"clusterDistance" %in% names(df)) df$clusterDistance <- NA_real_
  if (!"useClusterMethod" %in% names(df)) df$useClusterMethod <- NA
  if (!"siteSelectionAsDistributing" %in% names(df)) df$siteSelectionAsDistributing <- NA_character_

  config_tbl <- df %>%
    distinct(config_file) %>%
    mutate(trace = map(config_file, read_config_traceability)) %>%
    transmute(
      config_file,
      cfg_totalDisturbanceRate = vapply(trace, function(x) x$totalDisturbanceRate %||% NA_real_, numeric(1)),
      cfg_clusterDistance = vapply(trace, function(x) x$clusterDistance %||% NA_real_, numeric(1)),
      cfg_useClusterMethod = vapply(trace, function(x) x$useClusterMethod %||% NA, logical(1)),
      cfg_siteSelectionAsDistributing = vapply(trace, function(x) x$siteSelectionAsDistributing %||% NA_character_, character(1))
    )

  df %>%
    left_join(config_tbl, by = "config_file") %>%
    mutate(
      totalDisturbanceRate = coalesce(as.numeric(totalDisturbanceRate), cfg_totalDisturbanceRate),
      clusterDistance = coalesce(as.numeric(clusterDistance), cfg_clusterDistance),
      useClusterMethod = coalesce(as.logical(useClusterMethod), cfg_useClusterMethod),
      siteSelectionAsDistributing = coalesce(as.character(siteSelectionAsDistributing), cfg_siteSelectionAsDistributing)
    ) %>%
    select(-starts_with("cfg_"))
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

expected_years_by_run <- function(rep_years) {
  if (!nrow(rep_years)) return(tibble())
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

check_incomplete_replicates <- function(run_metrics, runs_df, run_label, allow_incomplete) {
  rep_years <- run_metrics %>%
    filter(!is.na(year)) %>%
    distinct(run_name, replicate, seed, year) %>%
    group_by(run_name, replicate, seed) %>%
    summarise(years = list(sort(unique(year))), .groups = "drop")
  if (!nrow(rep_years)) {
    return(list(
      run_metrics = run_metrics %>% mutate(replicate_status = "complete"),
      complete_metrics = run_metrics
    ))
  }
  expected <- expected_years_by_run(rep_years)
  rep_status <- rep_years %>%
    left_join(expected, by = "run_name") %>%
    mutate(
      missing_years = map2(years, expected_years, function(actual, expected) {
        if (is.null(actual) || all(is.na(actual))) actual <- numeric()
        if (is.null(expected) || all(is.na(expected))) expected <- numeric()
        setdiff(expected, actual)
      }),
      replicate_status = if_else(lengths(missing_years) > 0, "incomplete", "complete")
    )
  incomplete <- rep_status %>%
    filter(.data$replicate_status == "incomplete") %>%
    mutate(
      missing_years = vapply(missing_years, function(x) paste(x, collapse = ","), character(1))
    )
  if (nrow(incomplete)) {
    run_lookup <- runs_df %>%
      select(run_name, replicate, seed, output_path)
    incomplete <- incomplete %>%
      left_join(run_lookup, by = c("run_name", "replicate", "seed"))
    qa_path <- write_qa_csv(
      incomplete %>% select(run_name, replicate, seed, missing_years, output_path),
      sprintf("ua_qc_incomplete_replicates_%s.csv", run_label)
    )
    if (!isTRUE(allow_incomplete)) {
      stop(
        sprintf("Incomplete replicates detected (%d). See %s.", nrow(incomplete), qa_path),
        "Use --allow-incomplete-replicates to proceed (incomplete replicates flagged).",
        call. = FALSE
      )
    }
    warning(
      sprintf("Incomplete replicates detected (%d). Proceeding with complete-only summaries (see %s).", nrow(incomplete), qa_path),
      call. = FALSE
    )
  }
  run_metrics <- run_metrics %>%
    left_join(
      rep_status %>% select(run_name, replicate, seed, replicate_status),
      by = c("run_name", "replicate", "seed")
    ) %>%
    mutate(replicate_status = coalesce(replicate_status, "complete"))
  complete_metrics <- run_metrics %>% filter(.data$replicate_status == "complete")
  list(run_metrics = run_metrics, complete_metrics = complete_metrics)
}

main <- function() {
  opts <- parse_args()
  registry_path <- find_run_registry()
  design_path <- opts[["design-file"]]
  base_config_path <- opts[["base-config"]]
  run_name <- opts$run_name
  run_names <- opts$run_names
  run_label <- make_run_label(run_names)
  suite <- opts$suite
  run_display <- if (length(run_names)) paste(run_names, collapse = ",") else run_name
  if (identical(opts$mode, "design") && (is.null(run_name) || is.na(run_name) || !nzchar(run_name))) {
    stop("In design mode, --run-name must be supplied to match the UA run folder.", call. = FALSE)
  }
  runs_df <- read_runs_registry(
    registry_path,
    suite_val = suite,
    run_name_val = run_names,
    run_label = run_label,
    allow_duplicates = opts$`allow-duplicate-run-rows`
  )
  if (!nrow(runs_df)) stop("No successful runs found for suite=", suite, " run_name=", run_display, call. = FALSE)
  if (!"design_id" %in% names(runs_df)) runs_df$design_id <- NA_integer_
  if (opts$mode == "replicates") {
    base_params <- read_base_config(base_config_path)
    runs_df <- runs_df %>%
      mutate(
        design_id = 1L,
        totalDisturbanceRate = base_params$totalDisturbanceRate,
        clusterDistance = base_params$clusterDistance,
        useClusterMethod = base_params$useClusterMethod,
        siteSelectionAsDistributing = base_params$siteSelectionAsDistributing
      )
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
  runs_df <- populate_traceability_from_configs(runs_df)
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

  if (opts$mode == "replicates") {
    qc <- check_incomplete_replicates(
      run_metrics,
      runs_df,
      run_label = run_label,
      allow_incomplete = opts$`allow-incomplete-replicates`
    )
    run_metrics <- qc$run_metrics
    run_metrics_complete <- qc$complete_metrics
  } else {
    run_metrics_complete <- run_metrics
  }

  before_distinct <- nrow(run_metrics)
  run_metrics <- run_metrics %>% distinct()
  if (nrow(run_metrics) < before_distinct) {
    warning(
      sprintf("Removed %d exact duplicate metric rows as a final safeguard.", before_distinct - nrow(run_metrics)),
      call. = FALSE
    )
  }

  results_dir <- file.path(project_root, "outputs", "uncertainty", "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  run_out_csv <- file.path(results_dir, sprintf("ua_run_metrics_%s.csv", run_label))
  readr::write_csv(run_metrics, run_out_csv)

  if (opts$mode == "replicates") {
    rep_summary <- summarise_replicates(run_metrics_complete)
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
