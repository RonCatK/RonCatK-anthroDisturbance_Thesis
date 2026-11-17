#!/usr/bin/env Rscript

# Validate workspace runner configs (YAML) without requiring input data.
# Writes a machine-readable report under outputs/traceability/system_tests/ so CI can
# feed a traceability matrix.

invisible(suppressPackageStartupMessages({
  ok <- requireNamespace("yaml", quietly = TRUE)
  if (!isTRUE(ok)) {
    stop("Missing required package: yaml", call. = FALSE)
  }
}))

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 1 && isTRUE(is.na(a))) return(b)
  if (is.character(a) && length(a) >= 1 && !isTRUE(is.na(a[1])) && !nzchar(a[1])) return(b)
  a
}

args <- commandArgs(trailingOnly = TRUE)
workspace_root <- sub("^--workspace=", "", args[grepl("^--workspace=", args)][1]) %||% "workspace"
out_dir <- sub("^--out-dir=", "", args[grepl("^--out-dir=", args)][1]) %||% file.path("outputs", "traceability", "system_tests")

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
workspace_root <- normalizePath(workspace_root, winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

is_config_path <- function(path) {
  # Restrict to workspace/**/config/*.ya?ml
  if (!grepl("(^|/)workspace/[^/]+/config/.*\\.(ya?ml)$", path, perl = TRUE)) return(FALSE)
  # Exclude suite parameter templates (not runnable configs)
  !grepl("_parameters\\.ya?ml$", basename(path), ignore.case = TRUE)
}

list_config_files <- function() {
  git <- Sys.which("git")
  if (nzchar(git) && dir.exists(file.path(project_root, ".git"))) {
    tracked <- tryCatch(
      system2(git, c("ls-files", "--", "workspace"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    tracked <- tracked[nzchar(tracked)]
    tracked <- tracked[vapply(tracked, is_config_path, logical(1))]
    tracked <- sort(unique(tracked))
    if (length(tracked)) {
      return(normalizePath(file.path(project_root, tracked), winslash = "/", mustWork = TRUE))
    }
  }

  config_files <- list.files(
    path = workspace_root,
    pattern = "\\.(ya?ml)$",
    recursive = TRUE,
    full.names = TRUE
  )
  config_files <- normalizePath(config_files, winslash = "/", mustWork = FALSE)
  config_files <- config_files[vapply(config_files, is_config_path, logical(1))]
  sort(unique(config_files))
}

config_files <- list_config_files()

required_top_level <- c("suite", "run_name", "modules", "times", "data_profile", "n_reps")
runner_key_candidates <- required_top_level

validate_times <- function(times) {
  if (!is.list(times)) return("times must be a map/object")
  needed <- c("start", "end", "timeunit")
  miss <- setdiff(needed, names(times))
  if (length(miss)) return(paste0("times missing keys: ", paste(miss, collapse = ", ")))
  if (is.null(times$start) || is.null(times$end)) return("times start/end must be present")
  if (is.null(times$timeunit) || !nzchar(as.character(times$timeunit)[1])) return("times timeunit must be a non-empty string")
  NULL
}

validate_modules <- function(mods) {
  if (is.null(mods)) return("modules must be present")
  if (!is.character(mods) || !length(mods)) return("modules must be a non-empty list of strings")
  if (any(!nzchar(mods))) return("modules contains empty entries")
  NULL
}

validate_paths <- function(paths) {
  # paths is optional in runner schema (defaults exist); only validate if present
  if (is.null(paths)) return(NULL)
  if (!is.list(paths)) return("paths must be a map/object")
  NULL
}

validate_config <- function(cfg) {
  if (!is.list(cfg)) return("config must parse to a map/object")
  missing <- setdiff(required_top_level, names(cfg))
  if (length(missing)) return(paste0("missing keys: ", paste(missing, collapse = ", ")))

  err <- validate_modules(cfg$modules)
  if (!is.null(err)) return(err)
  err <- validate_times(cfg$times)
  if (!is.null(err)) return(err)
  err <- validate_paths(cfg$paths)
  if (!is.null(err)) return(err)

  # suite + run_name as non-empty strings
  if (!nzchar(as.character(cfg$suite)[1])) return("suite must be a non-empty string")
  if (!nzchar(as.character(cfg$run_name)[1])) return("run_name must be a non-empty string")

  # n_reps as positive integer-ish
  reps <- suppressWarnings(as.integer(cfg$n_reps))
  if (is.na(reps) || reps < 1) return("n_reps must be a positive integer")

  NULL
}

rows <- lapply(config_files, function(path) {
  rel <- sub(paste0("^", project_root, "/"), "", path)
  parsed <- tryCatch(yaml::read_yaml(path), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(data.frame(
      file = rel,
      checked = TRUE,
      valid = FALSE,
      message = substr(gsub("[\r\n]+", " ", conditionMessage(parsed)), 1, 500),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.list(parsed)) {
    return(data.frame(
      file = rel,
      checked = TRUE,
      valid = FALSE,
      message = "config must parse to a map/object",
      stringsAsFactors = FALSE
    ))
  }

  # Some YAML files under workspace/**/config are module parameter inputs rather than runner configs.
  # Only validate the runner schema when the YAML looks like a runner config (has runner keys).
  is_runner_cfg <- any(runner_key_candidates %in% names(parsed))
  if (!is_runner_cfg) {
    return(data.frame(
      file = rel,
      checked = FALSE,
      valid = TRUE,
      message = "skipped (non-runner YAML)",
      stringsAsFactors = FALSE
    ))
  }

  err <- validate_config(parsed)
  data.frame(
    file = rel,
    checked = TRUE,
    valid = is.null(err),
    message = if (is.null(err)) "" else substr(gsub("[\r\n]+", " ", err), 1, 500),
    stringsAsFactors = FALSE
  )
})

rows <- rows[!vapply(rows, is.null, logical(1))]
report <- if (length(rows)) do.call(rbind, rows) else data.frame(
  file = character(),
  checked = logical(),
  valid = logical(),
  message = character(),
  stringsAsFactors = FALSE
)

report_path <- file.path(out_dir, "config_validation.csv")
utils::write.csv(report, file = report_path, row.names = FALSE)

total <- nrow(report)
checked_total <- sum(report$checked %in% TRUE, na.rm = TRUE)
valid_n <- sum(report$checked %in% TRUE & report$valid %in% TRUE, na.rm = TRUE)
invalid_n <- checked_total - valid_n
skipped_n <- total - checked_total
stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

summary_lines <- c(
  paste0("timestamp=", stamp),
  paste0("workspace_root=", workspace_root),
  paste0("total=", total),
  paste0("checked=", checked_total),
  paste0("valid=", valid_n),
  paste0("invalid=", invalid_n),
  paste0("skipped=", skipped_n),
  paste0("report=", report_path)
)
writeLines(summary_lines, con = file.path(out_dir, "config_validation_summary.txt"))

if (invalid_n > 0) {
  message("Config validation failed for ", invalid_n, " checked file(s). See ", report_path)
  quit(save = "no", status = 1, runLast = FALSE)
}

message(
  "Config validation passed (", valid_n, "/", checked_total,
  " checked; skipped ", skipped_n, "). Report: ", report_path
)
