#!/usr/bin/env Rscript

# Snapshot suite-level evidence (runs + key metric summaries) into a tracked folder.
# This lets the CI traceability matrix include results from long-running suites
# without re-running them on GitHub Actions.

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1 && isTRUE(is.na(a))) return(b)
  if (is.character(a) && length(a) >= 1 && !isTRUE(is.na(a[1])) && !nzchar(a[1])) return(b)
  a
}

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_root <- file.path("docs", "traceability", "evidence")
out_runs <- file.path(out_root, "suite_runs")
out_metrics <- file.path(out_root, "metrics")

dir.create(out_runs, recursive = TRUE, showWarnings = FALSE)
dir.create(out_metrics, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
rows <- list()

relativize_path <- function(path) {
  path <- path %||% ""
  if (!nzchar(path)) return("")
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(project_root, "/")
  if (startsWith(normalized, prefix)) {
    return(sub(prefix, "", normalized, fixed = TRUE))
  }
  normalized
}

record_copy <- function(src, dst, ok, message = "") {
  rows[[length(rows) + 1]] <<- data.frame(
    timestamp = stamp,
    source = relativize_path(src),
    dest = relativize_path(dst),
    copied = isTRUE(ok),
    message = substr(gsub("[\r\n]+", " ", message %||% ""), 1, 500),
    stringsAsFactors = FALSE
  )
}

copy_if_exists <- function(src, dst) {
  src_norm <- normalizePath(src, winslash = "/", mustWork = FALSE)
  dst_norm <- normalizePath(dst, winslash = "/", mustWork = FALSE)
  if (!file.exists(src_norm)) {
    record_copy(src_norm, dst_norm, FALSE, "missing")
    return(invisible(FALSE))
  }
  dir.create(dirname(dst_norm), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch(file.copy(src_norm, dst_norm, overwrite = TRUE), error = function(e) FALSE)
  record_copy(src_norm, dst_norm, ok, if (ok) "" else "copy failed")
  invisible(ok)
}

# ---- Snapshot workspace/*/runs.csv (suite execution log) ----
runs_files <- Sys.glob(file.path("workspace", "*", "runs.csv"))
if (!length(runs_files)) {
  record_copy("", file.path(out_runs, "<suite>_runs.csv"), FALSE, "no workspace/*/runs.csv found")
} else {
  for (src in sort(runs_files)) {
    suite <- basename(dirname(src))
    if (suite %in% c("rates")) next
    dst <- file.path(out_runs, paste0(suite, "_runs.csv"))
    copy_if_exists(src, dst)
  }
}

# ---- Snapshot ADQD metric summaries (latest per scenario) ----
pick_latest_file <- function(dir, pattern) {
  if (is.null(dir) || !nzchar(dir) || !dir.exists(dir)) return(NA_character_)
  files <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(NA_character_)
  fi <- file.info(files)
  files[which.max(fi$mtime)]
}

adqd_scenarios <- c("VERIFICATION", "HOLDOUT", "VERIFICATION_CARIBOU", "HOLDOUT_CARIBOU")
for (sc in adqd_scenarios) {
  src_dir <- file.path("outputs", "adqd_validation", "results", sc)
  src <- pick_latest_file(src_dir, "adqd_summary\\.csv$")
  dst <- file.path(out_metrics, paste0("adqd_", sc, "__adqd_summary.csv"))
  if (is.na(src)) {
    record_copy(src_dir, dst, FALSE, "missing")
  } else {
    copy_if_exists(src, dst)
  }
}

# ---- Snapshot Morris QC summary ----
copy_if_exists(
  file.path("outputs", "sensitivity", "results", "morris_qc_summary.csv"),
  file.path(out_metrics, "morris_qc_summary.csv")
)

# ---- Snapshot uncertainty key-metrics summary ----
copy_if_exists(
  file.path("outputs", "uncertainty", "results", "ua_key_metrics_summary.csv"),
  file.path(out_metrics, "ua_key_metrics_summary.csv")
)

# ---- Snapshot uncertainty site-selection benchmark overview ----
copy_if_exists(
  file.path("outputs", "uncertainty", "results", "ua_sitesel_benchmark_overview_2031.csv"),
  file.path(out_metrics, "ua_sitesel_benchmark_overview_2031.csv")
)
copy_if_exists(
  file.path("outputs", "uncertainty", "results", "ua_sitesel_benchmark_ranges_2031.csv"),
  file.path(out_metrics, "ua_sitesel_benchmark_ranges_2031.csv")
)

# ---- Build system PercentageDisturbances summary ----
system_root <- file.path(project_root, "outputs", "system")
summary_path <- file.path(out_metrics, "system_pct_disturbance_summary.csv")
if (!dir.exists(system_root)) {
  record_copy(system_root, summary_path, FALSE, "missing")
} else {
  pct_files <- list.files(system_root, pattern = "^PercentageDisturbances_.*\\.txt$", recursive = TRUE, full.names = TRUE)
  pct_files <- sort(unique(pct_files))
  if (!length(pct_files)) {
    record_copy(system_root, summary_path, FALSE, "missing")
  } else {
    entries <- list()
    min_ref_pct <- 0.1
    for (p in pct_files) {
      lines <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
      if (!length(lines)) next
      for (ln in lines) {
        ln <- trimws(ln)
        if (!nzchar(ln)) next
        parts <- strsplit(ln, "\\s+")[[1]]
        if (length(parts) < 4) next
        pct <- suppressWarnings(as.numeric(parts[[4]]))
        if (is.na(pct)) next
        ref_pct <- if (length(parts) >= 5) suppressWarnings(as.numeric(parts[[5]])) else NA_real_
        entries[[length(entries) + 1]] <- data.frame(
          file = sub(paste0("^", project_root, "/"), "", normalizePath(p, winslash = "/", mustWork = FALSE)),
          sector = parts[[1]],
          origin = parts[[2]],
          year = suppressWarnings(as.integer(parts[[3]])),
          pct_diff = pct,
          abs_pct_diff = abs(pct),
          ref_pct = ref_pct,
          stringsAsFactors = FALSE
        )
      }
    }
    if (!length(entries)) {
      record_copy(system_root, summary_path, FALSE, "unparseable")
    } else {
      dt <- do.call(rbind, entries)
      keep <- is.na(dt$ref_pct) | abs(dt$ref_pct) >= min_ref_pct
      dt_used <- dt[keep, , drop = FALSE]
      if (!nrow(dt_used)) {
        record_copy(system_root, summary_path, FALSE, "unparseable")
      } else {
      out <- data.frame(
        n_files = length(pct_files),
        n_entries = nrow(dt),
        n_entries_used = nrow(dt_used),
        epsilon_ref_pct = min_ref_pct,
        median_abs_pct_diff = stats::median(dt_used$abs_pct_diff, na.rm = TRUE),
        p95_abs_pct_diff = stats::quantile(dt_used$abs_pct_diff, probs = 0.95, na.rm = TRUE, names = FALSE),
        stringsAsFactors = FALSE
      )
      dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(out, file = summary_path, row.names = FALSE)
      record_copy(system_root, summary_path, TRUE, "")
      }
    }
  }
}

# ---- Remove rates evidence (no longer tracked in traceability) ----
rates_paths <- c(
  file.path(out_runs, "rates_runs.csv"),
  file.path(out_metrics, "rates_base_verification.csv"),
  file.path(out_metrics, "rates_base_verification_500.csv"),
  file.path(out_metrics, "rates_high_verification.csv"),
  file.path(out_metrics, "rates_high_verification_500.csv"),
  file.path(out_metrics, "rates_low_verification.csv"),
  file.path(out_metrics, "rates_low_verification_500.csv")
)
for (p in rates_paths) {
  if (file.exists(p)) unlink(p, recursive = FALSE, force = TRUE)
}

report_path <- file.path(out_root, "snapshot_summary.csv")
report <- if (length(rows)) do.call(rbind, rows) else data.frame(
  timestamp = character(),
  source = character(),
  dest = character(),
  copied = logical(),
  message = character(),
  stringsAsFactors = FALSE
)
utils::write.csv(report, file = report_path, row.names = FALSE)

copied_n <- sum(report$copied %in% TRUE, na.rm = TRUE)
missing_n <- sum(report$message %in% "missing", na.rm = TRUE)
failed_n <- nrow(report) - copied_n - missing_n

message("Snapshot written: ", report_path)
message("copied=", copied_n, " missing=", missing_n, " failed=", failed_n)
