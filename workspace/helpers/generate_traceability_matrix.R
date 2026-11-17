#!/usr/bin/env Rscript

# Generate a CI-updated traceability matrix from a lightweight requirements spec
# plus whatever test/system artifacts exist in the workspace.
#
# Inputs:
#   - docs/traceability/traceability_requirements.csv  (authoritative spec)
#   - outputs/traceability/*                           (evidence produced by CI/local runs)
#
# Outputs:
#   - docs/traceability/traceability_matrix.csv        (CSV table for thesis/analysis)
#   - docs/traceability/traceability_matrix.md         (human-readable table)

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1 && isTRUE(is.na(a))) return(b)
  if (is.character(a) && length(a) >= 1 && !isTRUE(is.na(a[1])) && !nzchar(a[1])) return(b)
  a
}

args <- commandArgs(trailingOnly = TRUE)
opt <- list(
  spec = "docs/traceability/traceability_requirements.csv",
  out_csv = "docs/traceability/traceability_matrix.csv",
  out_md = "docs/traceability/traceability_matrix.md"
)

for (arg in args) {
  if (grepl("^--spec=", arg)) opt$spec <- sub("^--spec=", "", arg)
  if (grepl("^--out-csv=", arg)) opt$out_csv <- sub("^--out-csv=", "", arg)
  if (grepl("^--out-md=", arg)) opt$out_md <- sub("^--out-md=", "", arg)
}

spec_path <- normalizePath(opt$spec, winslash = "/", mustWork = TRUE)
spec <- utils::read.csv(spec_path, stringsAsFactors = FALSE, check.names = FALSE)

matrix_as_of <- tryCatch(
  {
    out <- suppressWarnings(system2("git", c("log", "-1", "--format=%cs"), stdout = TRUE, stderr = TRUE))
    out <- trimws(out)
    if (length(out) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", out[[1]])) out[[1]] else format(Sys.Date(), "%Y-%m-%d")
  },
  error = function(e) format(Sys.Date(), "%Y-%m-%d")
)

required_cols <- c(
  "id",
  "tier",
  "requirement",
  "acceptance",
  "evidence_ref",
  "metric_ref",
  "cadence"
)
missing_cols <- setdiff(required_cols, names(spec))
if (length(missing_cols)) {
  stop("Spec file missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

split_refs <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(character())
  refs <- unlist(strsplit(x, ";", fixed = TRUE))
  refs <- trimws(refs)
  refs[nzchar(refs)]
}

parse_ref <- function(ref) {
  ref <- trimws(ref)
  if (!nzchar(ref) || !grepl(":", ref, fixed = TRUE)) {
    return(list(type = "unknown", path = ref))
  }
  parts <- strsplit(ref, ":", fixed = TRUE)[[1]]
  list(type = tolower(parts[[1]]), path = paste(parts[-1], collapse = ":"))
}

safe_exists <- function(path) file.exists(path %||% "")

summ_file <- function(path) {
  if (!safe_exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  list(evidence = paste0(path, " (present)"), metric = "present", status = "met")
}

summ_coverage_txt <- function(path) {
  pq <- split_path_query(path)
  file_path <- pq$path
  query <- pq$query
  min_pct <- suppressWarnings(as.numeric(query[["min"]] %||% NA))

  if (!safe_exists(file_path)) {
    return(list(evidence = paste0(file_path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  lines <- tryCatch(readLines(file_path, warn = FALSE), error = function(e) character())
  hit <- regmatches(lines, regexpr("[0-9]+(\\.[0-9]+)?", lines, perl = TRUE))
  hit <- hit[nzchar(hit)][1]
  pct <- suppressWarnings(as.numeric(hit))
  if (is.na(pct)) {
    return(list(evidence = paste0(file_path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }
  pct_fmt <- sprintf("%.2f", pct)
  status <- "not_evaluated"
  if (!is.na(min_pct)) status <- if (pct >= min_pct) "met" else "not_met"

  list(
    evidence = paste0(file_path, " (coverage ", pct_fmt, "%)"),
    metric = paste0("coverage ", pct_fmt, "%"),
    status = status
  )
}

summ_coverage_bundle <- function(path) {
  pq <- split_path_query(path)
  raw <- pq$path
  query <- pq$query
  min_pct <- suppressWarnings(as.numeric(query[["min"]] %||% NA))

  if (is.null(raw) || !nzchar(raw)) {
    return(list(evidence = " (missing)", metric = "missing", status = "not_evaluated"))
  }

  paths <- trimws(unlist(strsplit(raw, ",", fixed = TRUE)))
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(list(evidence = paste0(raw, " (missing)"), metric = "missing", status = "not_evaluated"))
  }

  pct_vals <- c()
  for (p in paths) {
    if (!file.exists(p)) next
    lines <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
    hit <- regmatches(lines, regexpr("[0-9]+(\\.[0-9]+)?", lines, perl = TRUE))
    hit <- hit[nzchar(hit)][1]
    pct <- suppressWarnings(as.numeric(hit))
    if (!is.na(pct)) pct_vals <- c(pct_vals, pct)
  }

  if (!length(pct_vals)) {
    return(list(evidence = paste0(raw, " (missing)"), metric = "missing", status = "not_evaluated"))
  }

  min_cov <- min(pct_vals, na.rm = TRUE)
  pct_fmt <- sprintf("%.2f", min_cov)
  status <- "not_evaluated"
  if (!is.na(min_pct)) status <- if (min_cov >= min_pct) "met" else "not_met"

  list(
    evidence = paste0(raw, " (min coverage ", pct_fmt, "%)"),
    metric = paste0("min_coverage ", pct_fmt, "%"),
    status = status
  )
}

extract_attr_int <- function(line, attr) {
  pat <- paste0(attr, "=\"([0-9]+)\"")
  m <- regexec(pat, line, perl = TRUE)
  hit <- regmatches(line, m)[[1]]
  if (length(hit) >= 2) suppressWarnings(as.integer(hit[[2]])) else NA_integer_
}

parse_junit_files <- function(files) {
  if (!length(files)) return(list(ok = FALSE, reason = "empty"))
  parts <- lapply(files, function(p) {
    lines <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
    suite_lines <- grep("<testsuite\\b", lines, value = TRUE)
    if (!length(suite_lines)) {
      return(list(ok = FALSE, file = p))
    }
    tests <- sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "tests"), na.rm = TRUE)
    failures <- sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "failures"), na.rm = TRUE)
    errors <- sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "errors"), na.rm = TRUE)
    skipped <- sum(vapply(suite_lines, extract_attr_int, integer(1), attr = "skipped"), na.rm = TRUE)
    list(ok = TRUE, tests = tests, failures = failures, errors = errors, skipped = skipped)
  })

  ok <- vapply(parts, function(x) isTRUE(x$ok), logical(1))
  if (!all(ok)) {
    bad_file <- parts[which(!ok)[1]][[1]]$file %||% files[[which(!ok)[1]]]
    return(list(ok = FALSE, reason = "unparseable", file = bad_file))
  }

  tests <- sum(vapply(parts, function(x) x$tests, integer(1)), na.rm = TRUE)
  failures <- sum(vapply(parts, function(x) x$failures, integer(1)), na.rm = TRUE)
  errors <- sum(vapply(parts, function(x) x$errors, integer(1)), na.rm = TRUE)
  skipped <- sum(vapply(parts, function(x) x$skipped, integer(1)), na.rm = TRUE)
  passed <- tests - failures - errors - skipped
  pass_rate <- if (!is.na(tests) && tests > 0) (passed / tests) * 100 else NA_real_
  status <- if (!is.na(failures) && !is.na(errors) && (failures + errors) == 0L) "pass" else "fail"

  list(
    ok = TRUE,
    tests = tests,
    failures = failures,
    errors = errors,
    skipped = skipped,
    passed = passed,
    pass_rate = pass_rate,
    status = status,
    n_files = length(files)
  )
}

summ_junit <- function(path) {
  if (!safe_exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  parsed <- parse_junit_files(path)
  if (!isTRUE(parsed$ok)) {
    return(list(evidence = paste0(csv_path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  metric <- sprintf("tests: %s/%s passed (skipped %s)", parsed$passed, parsed$tests, parsed$skipped)
  if (!is.na(parsed$pass_rate)) metric <- paste0(metric, sprintf("; pass_rate %.1f%%", parsed$pass_rate))
  if (!is.na(parsed$failures) && parsed$failures > 0) metric <- paste0(metric, sprintf("; failures %s", parsed$failures))
  if (!is.na(parsed$errors) && parsed$errors > 0) metric <- paste0(metric, sprintf("; errors %s", parsed$errors))

  list(
    evidence = paste0(path, " (", parsed$status, ")"),
    metric = metric,
    status = if (parsed$status == "pass") "met" else "not_met"
  )
}

summ_junit_dir <- function(path) {
  pq <- split_path_query(path)
  dir_path <- pq$path
  query <- pq$query

  if (is.null(dir_path) || !nzchar(dir_path) || !dir.exists(dir_path)) {
    return(list(evidence = paste0(dir_path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }

  junit_files <- list.files(dir_path, pattern = "\\.xml$", full.names = TRUE)
  if (!length(junit_files)) {
    return(list(evidence = paste0(dir_path, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  include_re <- query[["include_regex"]] %||% ""
  exclude_re <- query[["exclude_regex"]] %||% ""
  if (nzchar(include_re)) {
    keep <- grepl(include_re, basename(junit_files), perl = TRUE)
    junit_files <- junit_files[keep]
  }
  if (nzchar(exclude_re) && length(junit_files)) {
    drop <- grepl(exclude_re, basename(junit_files), perl = TRUE)
    junit_files <- junit_files[!drop]
  }

  if (!length(junit_files)) {
    return(list(evidence = paste0(dir_path, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  parsed <- parse_junit_files(junit_files)
  if (!isTRUE(parsed$ok)) {
    bad_file <- parsed$file %||% dir_path
    return(list(
      evidence = paste0(dir_path, " (unparseable: ", bad_file, ")"),
      metric = "unparseable",
      status = "not_evaluated"
    ))
  }

  metric <- sprintf(
    "tests: %s/%s passed (skipped %s); files %s",
    parsed$passed,
    parsed$tests,
    parsed$skipped,
    parsed$n_files
  )
  if (!is.na(parsed$pass_rate)) metric <- paste0(metric, sprintf("; pass_rate %.1f%%", parsed$pass_rate))
  if (parsed$failures > 0) metric <- paste0(metric, sprintf("; failures %s", parsed$failures))
  if (parsed$errors > 0) metric <- paste0(metric, sprintf("; errors %s", parsed$errors))

  list(
    evidence = paste0(dir_path, " (", parsed$status, ")"),
    metric = metric,
    status = if (parsed$status == "pass") "met" else "not_met"
  )
}

summ_junit_bundle <- function(path) {
  pq <- split_path_query(path)
  raw <- pq$path
  query <- pq$query

  if (is.null(raw) || !nzchar(raw)) {
    return(list(evidence = " (missing)", metric = "missing", status = "not_evaluated"))
  }

  paths <- trimws(unlist(strsplit(raw, ",", fixed = TRUE)))
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(list(evidence = paste0(raw, " (missing)"), metric = "missing", status = "not_evaluated"))
  }

  junit_files <- character()
  for (p in paths) {
    if (dir.exists(p)) {
      junit_files <- c(junit_files, list.files(p, pattern = "\\.xml$", full.names = TRUE))
    } else if (file.exists(p)) {
      junit_files <- c(junit_files, p)
    }
  }
  junit_files <- unique(junit_files)

  if (!length(junit_files)) {
    return(list(evidence = paste0(raw, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  include_re <- query[["include_regex"]] %||% ""
  exclude_re <- query[["exclude_regex"]] %||% ""
  if (nzchar(include_re)) {
    keep <- grepl(include_re, basename(junit_files), perl = TRUE)
    junit_files <- junit_files[keep]
  }
  if (nzchar(exclude_re) && length(junit_files)) {
    drop <- grepl(exclude_re, basename(junit_files), perl = TRUE)
    junit_files <- junit_files[!drop]
  }

  if (!length(junit_files)) {
    return(list(evidence = paste0(raw, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  parsed <- parse_junit_files(junit_files)
  if (!isTRUE(parsed$ok)) {
    bad_file <- parsed$file %||% raw
    return(list(
      evidence = paste0(raw, " (unparseable: ", bad_file, ")"),
      metric = "unparseable",
      status = "not_evaluated"
    ))
  }

  metric <- sprintf(
    "tests: %s/%s passed (skipped %s); files %s",
    parsed$passed,
    parsed$tests,
    parsed$skipped,
    parsed$n_files
  )
  if (!is.na(parsed$pass_rate)) metric <- paste0(metric, sprintf("; pass_rate %.1f%%", parsed$pass_rate))
  if (parsed$failures > 0) metric <- paste0(metric, sprintf("; failures %s", parsed$failures))
  if (parsed$errors > 0) metric <- paste0(metric, sprintf("; errors %s", parsed$errors))

  list(
    evidence = paste0(raw, " (", parsed$status, ")"),
    metric = metric,
    status = if (parsed$status == "pass") "met" else "not_met"
  )
}

summ_config_validation <- function(path) {
  if (!safe_exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(dt) || !all(c("file", "valid") %in% names(dt))) {
    return(list(evidence = paste0(path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }
  total <- nrow(dt)
  has_checked <- "checked" %in% names(dt)
  if (has_checked) {
    checked_total <- sum(dt$checked %in% TRUE, na.rm = TRUE)
    valid_n <- sum(dt$checked %in% TRUE & dt$valid %in% TRUE, na.rm = TRUE)
    invalid_n <- checked_total - valid_n
    skipped_n <- total - checked_total
    status <- if (invalid_n == 0) "pass" else "fail"
    metric <- sprintf("runner configs: %s/%s valid; skipped %s", valid_n, checked_total, skipped_n)
    return(list(
      evidence = paste0(path, " (", status, ")"),
      metric = metric,
      status = if (invalid_n == 0) "met" else "not_met"
    ))
  }

  valid_n <- sum(dt$valid %in% TRUE, na.rm = TRUE)
  invalid_n <- total - valid_n
  status <- if (invalid_n == 0) "pass" else "fail"
  list(
    evidence = paste0(path, " (", status, ")"),
    metric = sprintf("configs: %s/%s valid", valid_n, total),
    status = if (invalid_n == 0) "met" else "not_met"
  )
}

parse_query_string <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(list())
  parts <- unlist(strsplit(x, "&", fixed = TRUE))
  parts <- parts[nzchar(parts)]
  out <- list()
  for (p in parts) {
    if (!grepl("=", p, fixed = TRUE)) next
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    key <- tolower(trimws(kv[[1]]))
    val <- trimws(paste(kv[-1], collapse = "="))
    if (nzchar(key) && nzchar(val)) out[[key]] <- val
  }
  out
}

split_path_query <- function(path) {
  path <- as.character(path %||% "")
  if (!nzchar(path) || !grepl("\\?", path, perl = TRUE)) return(list(path = path, query = list()))
  file_path <- sub("\\?.*$", "", path, perl = TRUE)
  query_str <- sub("^[^?]*\\?", "", path, perl = TRUE)
  list(path = file_path, query = parse_query_string(query_str))
}

summ_runs_csv <- function(path) {
  pq <- split_path_query(path)
  csv_path <- pq$path
  query <- pq$query
  min_success_rate <- suppressWarnings(as.numeric(query[["min_success_rate"]] %||% 100))

  if (!safe_exists(csv_path)) {
    return(list(evidence = paste0(csv_path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  required <- c("timestamp", "suite", "run_name", "replicate", "status")
  if (is.null(dt) || !all(required %in% names(dt))) {
    return(list(evidence = paste0(csv_path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  include_re <- query[["include_run_name_regex"]] %||% ""
  exclude_re <- query[["exclude_run_name_regex"]] %||% ""
  if (nzchar(include_re)) {
    hit <- grepl(include_re, dt$run_name %||% "", perl = TRUE)
    hit[is.na(hit)] <- FALSE
    dt <- dt[hit, , drop = FALSE]
  }
  if (nzchar(exclude_re)) {
    hit <- grepl(exclude_re, dt$run_name %||% "", perl = TRUE)
    hit[is.na(hit)] <- FALSE
    dt <- dt[!hit, , drop = FALSE]
  }

  if (!nrow(dt)) {
    return(list(evidence = paste0(csv_path, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  # If a run_name has any replicate-specific entries, ignore replicate==NA rows for that run_name.
  # replicate==NA indicates the run failed before a replicate was assigned.
  has_rep <- tapply(!is.na(dt$replicate), dt$run_name, any)
  keep <- !is.na(dt$replicate) | !has_rep[as.character(dt$run_name)]
  keep[is.na(keep)] <- FALSE
  dt <- dt[keep, , drop = FALSE]
  if (!nrow(dt)) {
    return(list(evidence = paste0(csv_path, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  ts <- suppressWarnings(as.POSIXct(dt$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  # Fall back to lexicographic ordering (ISO timestamps sort) if parsing fails.
  if (all(is.na(ts))) {
    ts <- dt$timestamp
  }
  dt$.ts <- ts
  dt$.row <- seq_len(nrow(dt))
  dt$.rep <- ifelse(is.na(dt$replicate), "NA", as.character(dt$replicate))
  dt$.key <- paste0(dt$run_name, "#", dt$.rep)

  dt <- dt[order(dt$.ts, dt$.row), , drop = FALSE]
  latest <- dt[!duplicated(dt$.key, fromLast = TRUE), , drop = FALSE]

  total <- nrow(latest)
  success_n <- sum(latest$status %in% "success", na.rm = TRUE)
  fail_n <- total - success_n
  status <- if (fail_n == 0) "pass" else "fail"
  success_rate <- if (total > 0) (success_n / total) * 100 else NA_real_

  run_names_n <- length(unique(latest$run_name))
  failing <- latest[!latest$status %in% "success", , drop = FALSE]
  failing_items <- character()
  if (nrow(failing)) {
    items <- paste0(failing$run_name, "[rep ", failing$.rep, "]=", failing$status)
    failing_items <- head(items, 8)
  }

  metric <- sprintf("runs: %s/%s success; run_names %s", success_n, total, run_names_n)
  if (!is.na(success_rate)) metric <- paste0(metric, sprintf("; success_rate %.1f%%", success_rate))
  if (nrow(failing)) metric <- paste0(metric, "; failing: ", paste(failing_items, collapse = ", "))

  eval_status <- "not_evaluated"
  if (!is.na(success_rate)) {
    eval_status <- if (success_rate >= min_success_rate) "met" else "not_met"
  }

  list(
    evidence = paste0(csv_path, " (", status, ")"),
    metric = metric,
    status = eval_status
  )
}

adqd_metric_summary <- function(dt, path_label) {
  if (is.null(dt) || !nrow(dt)) {
    return(list(evidence = paste0(path_label, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  key_cols <- c(
    "prevalence_ref",
    "prevalence_sim",
    "binary_f1",
    "binary_iou",
    "change_mask_quantity_disagreement",
    "change_mask_allocation_disagreement",
    "grid_spearman_10km",
    "linear_p90_distance_m"
  )
  if (!any(key_cols %in% names(dt))) {
    return(list(evidence = paste0(path_label, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  mean_or_na <- function(col) {
    if (!col %in% names(dt)) return(NA_real_)
    suppressWarnings(mean(as.numeric(dt[[col]]), na.rm = TRUE))
  }

  fmt <- function(x, digits = 4) {
    if (is.na(x) || is.nan(x)) return("NA")
    sprintf(paste0("%.", digits, "f"), x)
  }

  metrics <- c(
    change_prevalence_obs = mean_or_na("prevalence_ref"),
    change_prevalence_sim = mean_or_na("prevalence_sim"),
    change_mask_f1 = mean_or_na("binary_f1"),
    change_mask_iou = mean_or_na("binary_iou"),
    QD_change = mean_or_na("change_mask_quantity_disagreement"),
    AD_change = mean_or_na("change_mask_allocation_disagreement"),
    spearman_rho_10km = mean_or_na("grid_spearman_10km"),
    linear_p90_m = mean_or_na("linear_p90_distance_m")
  )

  if (all(is.na(metrics))) {
    return(list(evidence = paste0(path_label, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  metric <- paste0(
    "rows ", nrow(dt),
    "; change_prevalence_obs ", fmt(metrics[["change_prevalence_obs"]], 5),
    "; change_prevalence_sim ", fmt(metrics[["change_prevalence_sim"]], 5),
    "; change_mask_f1 ", fmt(metrics[["change_mask_f1"]], 4),
    "; change_mask_iou ", fmt(metrics[["change_mask_iou"]], 4),
    "; QD_change ", fmt(metrics[["QD_change"]], 4),
    "; AD_change ", fmt(metrics[["AD_change"]], 4),
    "; spearman_rho_10km ", fmt(metrics[["spearman_rho_10km"]], 4),
    "; linear_p90_m ", fmt(metrics[["linear_p90_m"]], 1)
  )

  list(evidence = paste0(path_label, " (pass)"), metric = metric, status = "met")
}

summ_adqd_summary_dir <- function(path) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  files <- list.files(path, pattern = "__adqd_summary\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) {
    return(list(evidence = paste0(path, " (empty)"), metric = "empty", status = "not_evaluated"))
  }

  fi <- file.info(files)
  latest <- files[which.max(fi$mtime)]
  dt <- tryCatch(utils::read.csv(latest, stringsAsFactors = FALSE), error = function(e) NULL)
  adqd_metric_summary(dt, latest)
}

summ_adqd_summary_csv <- function(path) {
  pq <- split_path_query(path)
  csv_path <- pq$path
  if (!safe_exists(csv_path)) {
    return(list(evidence = paste0(csv_path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  adqd_metric_summary(dt, csv_path)
}

summ_ua_key_metrics_csv <- function(path) {
  pq <- split_path_query(path)
  csv_path <- pq$path
  if (!safe_exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(dt) || !nrow(dt) || !"metric_id" %in% names(dt)) {
    return(list(evidence = paste0(path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  total_rows <- nrow(dt)
  target <- dt[dt$metric_id %in% "total_interval_new_area_km2", , drop = FALSE]
  if (!nrow(target)) {
    return(list(evidence = paste0(path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }

  year_num <- suppressWarnings(as.integer(target$year))
  pick <- target[which.max(year_num), , drop = FALSE][1, , drop = FALSE]

  fmt <- function(x, digits = 3) {
    x <- suppressWarnings(as.numeric(x))
    if (is.na(x)) return("NA")
    sprintf(paste0("%.", digits, "f"), x)
  }

  unit <- pick$unit %||% "km^2"
  metric <- paste0(
    "rows ", total_rows,
    "; year ", pick$year %||% "",
    "; base_median ", fmt(pick$base_median, 3),
    "; random_median ", fmt(pick$random_median, 3),
    "; sitesel_median ", fmt(pick$sitesel_median, 3),
    " ", unit
  )

  append_metric <- function(label, value, digits = 3, suffix = "") {
    if (is.null(value) || isTRUE(is.na(value))) return(metric)
    val <- suppressWarnings(as.numeric(value))
    if (is.na(val)) return(metric)
    paste0(metric, "; ", label, " ", sprintf(paste0("%.", digits, "f"), val), suffix)
  }

  metric <- append_metric("base_cv", pick$base_cv, 3)
  metric <- append_metric("base_iqr", pick$base_iqr, 3, paste0(" ", unit))
  metric <- append_metric("random_shift_pct", pick$random_shift_pct, 1, "%")
  metric <- append_metric("sitesel_shift_pct", pick$sitesel_shift_pct, 1, "%")

  list(evidence = paste0(csv_path, " (pass)"), metric = metric, status = "met")
}

summ_pct_disturbance_summary_csv <- function(path) {
  pq <- split_path_query(path)
  csv_path <- pq$path
  query <- pq$query
  median_max <- suppressWarnings(as.numeric(query[["median_abs_pct_diff_max"]] %||% NA))
  p95_max <- suppressWarnings(as.numeric(query[["p95_abs_pct_diff_max"]] %||% NA))

  if (!safe_exists(csv_path)) {
    return(list(evidence = paste0(csv_path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  required <- c("n_files", "n_entries", "median_abs_pct_diff", "p95_abs_pct_diff")
  if (is.null(dt) || !nrow(dt) || !all(required %in% names(dt))) {
    return(list(evidence = paste0(csv_path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }
  row <- dt[1, , drop = FALSE]
  fmt <- function(x, digits = 2) {
    x <- suppressWarnings(as.numeric(x))
    if (is.na(x)) return("NA")
    sprintf(paste0("%.", digits, "f"), x)
  }
  metric <- paste0(
    "files ", row$n_files,
    "; entries ", row$n_entries,
    "; median_abs_pct_diff ", fmt(row$median_abs_pct_diff, 2),
    "; p95_abs_pct_diff ", fmt(row$p95_abs_pct_diff, 2)
  )

  status <- "not_evaluated"
  if (!is.na(median_max) || !is.na(p95_max)) {
    median_ok <- is.na(median_max) || suppressWarnings(as.numeric(row$median_abs_pct_diff)) <= median_max
    p95_ok <- is.na(p95_max) || suppressWarnings(as.numeric(row$p95_abs_pct_diff)) <= p95_max
    status <- if (median_ok && p95_ok) "met" else "not_met"
  }

  list(evidence = paste0(csv_path, " (pass)"), metric = metric, status = status)
}

summ_rates_verification_csv <- function(path) {
  if (!safe_exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(dt) || !nrow(dt) || !"rate_gap_pct" %in% names(dt)) {
    return(list(evidence = paste0(path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }
  gaps <- suppressWarnings(as.numeric(dt$rate_gap_pct))
  abs_gaps <- abs(gaps)
  max_abs <- suppressWarnings(max(abs_gaps, na.rm = TRUE))
  mean_abs <- suppressWarnings(mean(abs_gaps, na.rm = TRUE))
  cap_true <- if ("capacity_flag" %in% names(dt)) sum(dt$capacity_flag %in% TRUE, na.rm = TRUE) else NA_integer_
  parts <- c(
    paste0("rows ", nrow(dt)),
    sprintf("max_abs_rate_gap_pct %.3f", max_abs),
    sprintf("mean_abs_rate_gap_pct %.3f", mean_abs)
  )
  if (!is.na(cap_true)) parts <- c(parts, paste0("capacity_flag_true ", cap_true))
  list(evidence = paste0(path, " (pass)"), metric = paste(parts, collapse = "; "), status = "met")
}

summ_morris_qc_csv <- function(path) {
  if (!safe_exists(path)) {
    return(list(evidence = paste0(path, " (missing)"), metric = "missing", status = "not_evaluated"))
  }
  dt <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(dt) || !nrow(dt) || !all(c("table", "reason", "n") %in% names(dt))) {
    return(list(evidence = paste0(path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }
  counts <- dt[dt$table %in% "counts_by_reason", c("reason", "n"), drop = FALSE]
  if (!nrow(counts)) {
    return(list(evidence = paste0(path, " (unparseable)"), metric = "unparseable", status = "not_evaluated"))
  }
  counts$n <- suppressWarnings(as.integer(counts$n))
  get_n <- function(reason) {
    v <- counts$n[counts$reason %in% reason][1]
    if (is.na(v)) 0L else v
  }
  kept <- get_n("kept")
  noop <- get_n("no-op")
  conf <- get_n("confounded")
  metric <- sprintf("kept %s; no-op %s; confounded %s", kept, noop, conf)
  list(evidence = paste0(path, " (pass)"), metric = metric, status = "met")
}

summ_ref <- function(ref) {
  parsed <- parse_ref(ref)
  path <- parsed$path
  switch(
    parsed$type,
    file = summ_file(path),
    junit = summ_junit(path),
    junit_dir = summ_junit_dir(path),
    junit_bundle = summ_junit_bundle(path),
    coverage_txt = summ_coverage_txt(path),
    coverage_bundle = summ_coverage_bundle(path),
    config_validation = summ_config_validation(path),
    runs_csv = summ_runs_csv(path),
    adqd_summary_dir = summ_adqd_summary_dir(path),
    adqd_summary_csv = summ_adqd_summary_csv(path),
    ua_key_metrics_csv = summ_ua_key_metrics_csv(path),
    pct_disturbance_summary_csv = summ_pct_disturbance_summary_csv(path),
    rates_verification_csv = summ_rates_verification_csv(path),
    morris_qc_csv = summ_morris_qc_csv(path),
    summ_file(path)
  )
}

combine_parts <- function(parts) {
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  paste(parts, collapse = "; ")
}

normalize_status <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(x)) return("not_evaluated")
  if (x %in% c("met", "not_met", "not_evaluated")) return(x)
  "not_evaluated"
}

combine_status <- function(statuses) {
  statuses <- vapply(statuses, normalize_status, character(1))
  if (!length(statuses)) return("Not evaluated")
  if (any(statuses %in% "not_met")) return("Not met")
  if (all(statuses %in% "met")) return("Met")
  "Not evaluated"
}

out_rows <- lapply(seq_len(nrow(spec)), function(i) {
  row <- spec[i, ]
  evidence_refs <- split_refs(row$evidence_ref)
  metric_refs <- split_refs(row$metric_ref)
  if (!length(metric_refs)) metric_refs <- evidence_refs

  evidence_summaries <- lapply(evidence_refs, summ_ref)
  metric_summaries <- lapply(metric_refs, summ_ref)

  evidence_parts <- vapply(evidence_summaries, function(x) x$evidence %||% "", character(1), USE.NAMES = FALSE)
  metric_parts <- vapply(metric_summaries, function(x) x$metric %||% "", character(1), USE.NAMES = FALSE)
  status_parts <- vapply(metric_summaries, function(x) x$status %||% "not_evaluated", character(1), USE.NAMES = FALSE)
  status_val <- combine_status(status_parts)

	  data.frame(
	    id = row$id,
	    tier = row$tier,
	    requirement = row$requirement,
	    evidence = combine_parts(evidence_parts),
	    acceptance = row$acceptance,
	    metric = combine_parts(metric_parts),
	    status = status_val,
	    cadence = row$cadence,
	    as_of = matrix_as_of,
	    stringsAsFactors = FALSE
	  )
	})

out_dt <- do.call(rbind, out_rows)

out_csv <- opt$out_csv
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out_dt, file = out_csv, row.names = FALSE)

escape_md <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("[\r\n]+", " ", x)
  x
}

md_lines <- c(
  "# Traceability Matrix",
  "",
  "<!-- Generated by workspace/helpers/generate_traceability_matrix.R; do not edit manually. -->",
  ""
)

table_lines <- function(df) {
  lines <- c(
    "| ID | Requirement | Evidence | Acceptance | Metric | Status | Cadence | As of |",
    "|---|---|---|---|---|---|---|---|"
  )
  for (i in seq_len(nrow(df))) {
    lines <- c(
      lines,
      paste0(
        "| ",
        escape_md(df$id[[i]]), " | ",
        escape_md(df$requirement[[i]]), " | ",
        escape_md(df$evidence[[i]]), " | ",
        escape_md(df$acceptance[[i]]), " | ",
        escape_md(df$metric[[i]]), " | ",
        escape_md(df$status[[i]]), " | ",
        escape_md(df$cadence[[i]]), " | ",
        escape_md(df$as_of[[i]]),
        " |"
      )
    )
  }
  lines
}

gate_rows <- out_dt[out_dt$tier %in% "Gate checks", , drop = FALSE]
bench_rows <- out_dt[out_dt$tier %in% "Benchmark evidence", , drop = FALSE]

if (nrow(gate_rows)) {
  md_lines <- c(md_lines, "## Gate checks", "", table_lines(gate_rows), "")
}
if (nrow(bench_rows)) {
  md_lines <- c(md_lines, "## Benchmark evidence", "", table_lines(bench_rows), "")
}

other_rows <- out_dt[!out_dt$tier %in% c("Gate checks", "Benchmark evidence"), , drop = FALSE]
if (nrow(other_rows)) {
  md_lines <- c(md_lines, "## Other", "", table_lines(other_rows), "")
}

out_md <- opt$out_md
dir.create(dirname(out_md), recursive = TRUE, showWarnings = FALSE)
writeLines(md_lines, con = out_md)

message("Wrote: ", out_csv)
message("Wrote: ", out_md)
