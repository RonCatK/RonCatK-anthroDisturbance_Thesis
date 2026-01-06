#!/usr/bin/env Rscript

# Generic SpaDES runner for thesis workflows.
# Usage: Rscript workspace/runner.R /path/to/config.yaml
# Config format (YAML/JSON/R list):
#   suite: system|verification|sensitivity|uncertainty|adqd_validation|rates|e2e_dummy
#   run_name: short identifier for the run
#   description: optional text
#   config_version: optional, defaults to "1"
#   modules: [anthroDisturbance_DataPrep, anthroDisturbance_Generator, ...]
#   times: { start: 0, end: 10, timeunit: "year" }
#   paths:
#     input_root: path to inputs (e.g., data/preprocessed)
#     output_root: path for outputs (default: outputs)
#     scratch_root: path for logs/scratch (default: scratch)
#     module_path: optional, defaults to modules
#   data_profile: default/preprocessed/synthetic/etc (written to runs.csv)
#   params: named list of module parameter overrides
#   n_reps: integer replicate count
#   seed_base: optional integer base seed (rep seeds = seed_base + rep_id - 1)
#   seeds: optional explicit vector of seeds (length == n_reps)
#   input_behaviour: { allow_download_if_missing: false }
#
# Behaviour:
# - Validates config and builds deterministic seeds.
# - Ensures output/log/scratch folders exist under the chosen roots.
# - Verifies input presence using CHECKSUMS.txt when available; errors if
#   missing or checksum mismatch unless allow_download_if_missing is TRUE.
# - Supplies disturbanceDT with local file:// URLs so prep modules do not
#   re-download data already present.
# - Captures console + message output to per-replicate log files under scratch.
# - Appends one row per replicate to workspace/runs.csv with status & paths.

suppressPackageStartupMessages({
  required <- c(
    "data.table", "yaml", "jsonlite",
    "SpaDES.core", "SpaDES.tools", "SpaDES.project", "reproducible", "terra",
    "digest", "futile.logger"
  )
  ok <- vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  if (!all(ok)) {
    stop(
      sprintf(
        "Missing required packages: %s",
        paste(required[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  library(data.table)
  library(SpaDES.core)
  library(SpaDES.tools)
})

# Optional verbose tracing for terra::writeRaster to debug filename mismatches.
# Enable by running with environment variable DEBUG_WRITE_RASTER=true.
if (isTRUE(grepl("^1|^true|^yes$", Sys.getenv("DEBUG_WRITE_RASTER"), ignore.case = TRUE))) {
  try({
    trace(
      what = "writeRaster",
      where = asNamespace("terra"),
      tracer = quote({
        fname_val <- if (exists("filename", inherits = FALSE)) filename else NULL
        if (!is.null(fname_val) && length(fname_val) != 1L) {
          caller <- tryCatch(sys.call(-1), error = function(...) NULL)
          msg <- sprintf("[debug writeRaster] filename length=%s; caller=%s",
                         length(fname_val),
                         paste(deparse(caller), collapse = " "))
          message(msg)
          message(sprintf("[debug writeRaster] filenames: %s",
                          paste(fname_val, collapse = " | ")))
        }
      }),
      print = FALSE
    )
  }, silent = TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a

coerce_positive_int <- function(value, default) {
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 1) return(default)
  parsed
}

detect_physical_cores <- function() {
  cores <- tryCatch(parallel::detectCores(logical = FALSE), error = function(...) NA_integer_)
  if (is.na(cores) || cores < 1L) cores <- 1L
  cores
}

determine_runner_core_budget <- function() {
  concurrent_env <- Sys.getenv("RUNNER_CONCURRENT_JOBS", unset = "4")
  concurrent_runs <- coerce_positive_int(concurrent_env, 4L)
  detected <- detect_physical_cores()
  override_env <- Sys.getenv("RUNNER_CORES_PER_JOB", unset = "")
  override <- coerce_positive_int(override_env, NA_integer_)
  per_job <- if (!is.na(override)) override else max(1L, floor(detected / concurrent_runs))
  per_job <- min(per_job, detected)
  list(
    detected_cores = detected,
    concurrent_runs = concurrent_runs,
    per_job = per_job,
    override = !is.na(override)
  )
}

apply_thread_limits <- function(workers) {
  thread_envs <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "NUMEXPR_MAX_THREADS", "VECLIB_MAXIMUM_THREADS"
  )
  for (env_var in thread_envs) {
    env_pair <- as.list(setNames(as.character(workers), env_var))
    do.call(Sys.setenv, env_pair)
  }
  data.table::setDTthreads(workers)
  options(runner.core_budget = workers)
}

runner_core_budget <- determine_runner_core_budget()
apply_thread_limits(runner_core_budget$per_job)
message(
  sprintf(
    "Runner core budget: %s cores per job (detected %s physical cores, planning %s concurrent jobs%s).",
    runner_core_budget$per_job,
    runner_core_budget$detected_cores,
    runner_core_budget$concurrent_runs,
    if (runner_core_budget$override) " with manual override" else ""
  )
)

deauth_googledrive <- function() {
  if (requireNamespace("googledrive", quietly = TRUE)) {
    tryCatch(
      googledrive::drive_deauth(),
      error = function(...) NULL,
      warning = function(...) NULL
    )
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  alt <- getOption("runner.config_path", default = "")
  if (!nzchar(alt)) alt <- Sys.getenv("RUNNER_CONFIG_PATH", unset = "")
  if (nzchar(alt)) args <- alt
}
if (length(args) != 1) {
  stop("Usage: Rscript workspace/runner.R /path/to/config.[yaml|json|R]", call. = FALSE)
}

args_full <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
script_dir <- if (length(script_path)) dirname(script_path) else getwd()
project_root_opt <- getOption("runner.project_root", default = NULL)
project_root <- if (!is.null(project_root_opt)) {
  normalizePath(project_root_opt, winslash = "/", mustWork = TRUE)
} else {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
}
setwd(project_root)
deauth_googledrive()

timestamp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
timestamp_tag <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

relative_to_root <- function(pathValue) {
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) return(NA_character_)
  normalized <- normalizePath(pathValue, winslash = "/", mustWork = FALSE)
  prefix <- paste0(normalizePath(project_root, winslash = "/", mustWork = TRUE), "/")
  if (startsWith(normalized, prefix)) sub(prefix, "", normalized, fixed = TRUE) else normalized
}

format_error_message <- function(condition) {
  msg <- if (inherits(condition, "condition")) conditionMessage(condition) else as.character(condition)
  if (!length(msg)) msg <- ""
  msg <- gsub("[\r\n]+", " ", msg)
  substr(msg, 1, 500)
}

log_config_failure <- function(cfg, err_msg) {
  if (is.null(cfg)) return(invisible())
  # Build canonical paths for output/log even if the run never created them.
  output_dir_path <- file.path(cfg$paths$output_root %||% file.path(project_root, "outputs"), cfg$suite, cfg$run_name, "rep_001")
  log_file_path <- file.path(cfg$paths$scratch_root %||% file.path(project_root, "scratch"), cfg$suite, cfg$run_name, "rep_001.log")
  row <- list(
    timestamp = timestamp_now(),
    suite = cfg$suite,
    run_name = cfg$run_name,
    replicate = NA_integer_,
    seed = NA_integer_,
    config_file = relative_to_root(cfg$config_file),
    modules = paste(cfg$modules, collapse = ","),
    data_profile = cfg$data_profile,
    input_root = relative_to_root(cfg$paths$input_root),
    output_dir = relative_to_root(output_dir_path),
    log_file = relative_to_root(log_file_path),
    status = "error",
    error_message = err_msg %||% ""
  )
  tryCatch(
    append_run_log(row = row, suite = cfg$suite),
    error = function(e) warning("log_config_failure: ", conditionMessage(e), call. = FALSE)
  )
}

load_config <- function(path) {
  if (!file.exists(path)) stop("Config file not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  cfg <- switch(
    ext,
    yml = yaml::read_yaml(path),
    yaml = yaml::read_yaml(path),
    json = jsonlite::fromJSON(path, simplifyVector = FALSE),
    r = {
      env <- new.env(parent = baseenv())
      sys.source(path, envir = env, keep.source = FALSE)
      if (exists("config", envir = env, inherits = FALSE)) get("config", envir = env) else if (exists("cfg", envir = env, inherits = FALSE)) get("cfg", envir = env) else stop("R config must assign `config` or `cfg` list.", call. = FALSE)
    },
    stop("Unsupported config extension: ", ext, call. = FALSE)
  )
  cfg
}

assert_scalar_character <- function(x, field) {
  if (is.null(x) || is.na(x) || !nzchar(x)) stop(field, " must be a non-empty string.", call. = FALSE)
}

validate_config <- function(cfg) {
  if (!is.list(cfg)) stop("Config must be a list-like object.", call. = FALSE)
  assert_scalar_character(cfg$suite, "suite")
  suite <- tolower(cfg$suite)
  allowed_suites <- c("system", "verification", "sensitivity", "uncertainty", "adqd_validation", "rates", "e2e_dummy")
  if (!suite %in% allowed_suites) stop("suite must be one of: ", paste(allowed_suites, collapse = ", "), call. = FALSE)

  assert_scalar_character(cfg$run_name, "run_name")
  run_name <- cfg$run_name

  modules <- cfg$modules
  if (is.null(modules) || !length(modules)) stop("modules must be a non-empty list/vector.", call. = FALSE)
  modules <- as.character(unlist(modules))

  times <- cfg$times
  if (is.null(times) || !is.list(times)) stop("times must be a list with start/end/timeunit.", call. = FALSE)
  if (is.null(times$start) || is.null(times$end)) stop("times must include start and end.", call. = FALSE)
  times$timeunit <- times$timeunit %||% "year"

  metadata <- cfg$metadata %||% list()
  baseline_mode <- metadata$baseline_mode %||% cfg$baseline_mode %||% NULL

  paths <- cfg$paths %||% list()
  default_base_input <- file.path(project_root, "data", "preprocessed")
  default_input_root <- if (suite == "adqd_validation") file.path(default_base_input, "comparison") else default_base_input

  provided_input_root <- paths$input_root %||% NULL
  if (is.null(provided_input_root) || !nzchar(provided_input_root)) {
    paths$input_root <- default_input_root
  } else {
    paths$input_root <- provided_input_root
  }

  paths$output_root <- paths$output_root %||% file.path(project_root, "outputs")
  paths$scratch_root <- paths$scratch_root %||% file.path(project_root, "scratch")
  paths$module_path <- paths$module_path %||% file.path(project_root, "modules")

  n_reps <- as.integer(cfg$n_reps %||% 1L)
  if (is.na(n_reps) || n_reps < 1) stop("n_reps must be a positive integer.", call. = FALSE)

  seed_base <- cfg$seed_base
  seeds <- cfg$seeds
  input_behaviour <- cfg$input_behaviour %||% list()
  input_behaviour$allow_download_if_missing <- isTRUE(input_behaviour$allow_download_if_missing)
  default_sa <- file.path(project_root, "data", "study_area", "aoi_southwest_NWT.shp")
  study_area <- cfg$study_area %||% default_sa
  if (!nzchar(study_area)) study_area <- NULL
  default_rtm <- file.path(project_root, "data", "study_area", "aoi_southwest_NWT_RTM_250m.tif")
  raster_to_match <- cfg$raster_to_match %||% cfg$rasterToMatch %||% default_rtm
  if (!nzchar(raster_to_match)) raster_to_match <- NULL
  features_to_avoid <- cfg$features_to_avoid %||% cfg$featuresToAvoid %||% NULL
  if (!is.null(features_to_avoid) && !nzchar(features_to_avoid)) features_to_avoid <- NULL
  if (!is.null(features_to_avoid)) {
    if (!grepl("^(/|[A-Za-z]:)", features_to_avoid)) {
      features_to_avoid <- file.path(project_root, features_to_avoid)
    }
    features_to_avoid <- normalizePath(features_to_avoid, winslash = "/", mustWork = FALSE)
  }
  geodata_path <- cfg$geodata_path %||% cfg$paths$geodata_path %||% NULL
  if (!is.null(geodata_path) && !nzchar(geodata_path)) geodata_path <- NULL
  if (!is.null(geodata_path)) {
    if (!grepl("^(/|[A-Za-z]:)", geodata_path)) {
      geodata_path <- file.path(project_root, geodata_path)
    }
    geodata_path <- normalizePath(geodata_path, winslash = "/", mustWork = FALSE)
  }
  dem_path <- cfg$dem %||% cfg$DEM %||% NULL
  if (!is.null(dem_path) && !nzchar(dem_path)) dem_path <- NULL
  if (!is.null(dem_path)) {
    if (!grepl("^(/|[A-Za-z]:)", dem_path)) {
      dem_path <- file.path(project_root, dem_path)
    }
    dem_path <- normalizePath(dem_path, winslash = "/", mustWork = FALSE)
  }

  disturbance_rate_file <- cfg$disturbance_rate_file %||% cfg$disturbance_rates %||% cfg$paths$disturbance_rate_file %||% cfg$paths$disturbance_rates
  if (!is.null(disturbance_rate_file) && !nzchar(disturbance_rate_file)) disturbance_rate_file <- NULL
  if (!is.null(disturbance_rate_file)) {
    if (!grepl("^(/|[A-Za-z]:)", disturbance_rate_file)) {
      disturbance_rate_file <- file.path(project_root, disturbance_rate_file)
    }
    disturbance_rate_file <- normalizePath(disturbance_rate_file, winslash = "/", mustWork = FALSE)
  }

  live_maps <- cfg$live_maps %||% list()
  if (is.logical(live_maps) && length(live_maps) == 1L) {
    live_maps <- list(enabled = isTRUE(live_maps))
  } else if (is.character(live_maps) && length(live_maps) == 1L) {
    live_maps <- list(enabled = tolower(live_maps) %in% c("true", "t", "yes", "y", "1"))
  } else if (!is.list(live_maps)) {
    live_maps <- list(enabled = FALSE)
  }
  live_maps$enabled <- isTRUE(live_maps$enabled)
  live_maps$title <- live_maps$title %||% ""
  live_maps$debug <- isTRUE(live_maps$debug)
  live_maps$filesOnly <- isTRUE(live_maps$filesOnly)

  list(
    config_version = cfg$config_version %||% "1",
    description = cfg$description %||% "",
    suite = suite,
    run_name = run_name,
    modules = modules,
    times = times,
    paths = paths,
    data_profile = cfg$data_profile %||% "default",
    params = cfg$params %||% list(),
    metadata = cfg$metadata %||% list(),
    n_reps = n_reps,
    seed_base = seed_base,
    seeds = seeds,
    input_behaviour = input_behaviour,
    study_area = study_area,
    raster_to_match = raster_to_match,
    features_to_avoid = features_to_avoid,
    geodata_path = geodata_path,
    dem = dem_path,
    disturbance_rate_file = disturbance_rate_file,
    live_maps = live_maps,
    config_file = normalizePath(cfg$config_file, winslash = "/", mustWork = FALSE)
  )
}

derive_seeds <- function(cfg) {
  if (!is.null(cfg$seeds)) {
    seeds <- as.integer(unlist(cfg$seeds))
    if (length(seeds) != cfg$n_reps) {
      stop("Length of seeds must equal n_reps (", cfg$n_reps, ").", call. = FALSE)
    }
    return(seeds)
  }
  base <- as.integer(cfg$seed_base %||% 12345L)
  base + seq_len(cfg$n_reps) - 1L
}

link_geodata_cache <- function(input_root, geodata_root, log_line = NULL) {
  if (is.null(geodata_root) || !nzchar(geodata_root)) return(invisible(FALSE))
  log_fn <- if (is.null(log_line)) message else log_line
  src_root <- normalizePath(geodata_root, winslash = "/", mustWork = FALSE)
  dest_root <- normalizePath(input_root, winslash = "/", mustWork = FALSE)
  for (dir_name in c("elevation", "landuse")) {
    src <- file.path(src_root, dir_name)
    dest <- file.path(dest_root, dir_name)
    if (dir.exists(dest)) {
      dest_files <- list.files(dest, all.files = FALSE, no.. = TRUE)
      if (length(dest_files)) {
        log_fn(paste0("geodata cache already present: ", dest))
        next
      }
      # empty destination; attempt to replace with a symlink or copy
      ok_rm <- tryCatch(unlink(dest, recursive = TRUE), error = function(...) FALSE)
      if (!isTRUE(ok_rm)) {
        log_fn(paste0("geodata cache destination not removable: ", dest))
      }
    }
    if (!dir.exists(src)) {
      log_fn(paste0("geodata cache source missing: ", src))
      next
    }
    ok <- tryCatch(file.symlink(src, dest), error = function(...) FALSE)
    if (isTRUE(ok)) {
      log_fn(paste0("Linked geodata cache ", dir_name, ": ", src, " -> ", dest))
    } else {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      files <- list.files(src, full.names = TRUE)
      if (length(files)) {
        file.copy(files, dest, overwrite = FALSE)
        log_fn(paste0("Copied geodata cache ", dir_name, ": ", src, " -> ", dest))
      } else {
        log_fn(paste0("geodata cache source empty: ", src))
      }
    }
  }
  invisible(TRUE)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

load_checksums <- function(input_root) {
  candidates <- c(
    file.path(input_root, "CHECKSUMS.txt"),
    file.path(input_root, "checksums.txt"),
    file.path(project_root, "data", "CHECKSUMS.txt"),
    file.path(project_root, "data", "preprocessed", "CHECKSUMS.txt"),
    file.path(project_root, "data", "raw", "CHECKSUMS.txt")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) return(NULL)
  dt <- tryCatch(data.table::fread(candidates[[1]]), error = function(e) NULL)
  if (is.null(dt) || !"file" %in% names(dt) || !nrow(dt)) return(NULL)
  dt[, path := {
    f <- as.character(file)
    is_abs <- grepl("^(/|[A-Za-z]:)", f)
    target <- ifelse(is_abs, f, file.path(input_root, f))
    normalizePath(target, winslash = "/", mustWork = FALSE)
  }]
  dt
}

verify_inputs <- function(checksums, allow_download) {
  if (is.null(checksums) || !nrow(checksums)) return(invisible(TRUE))
  missing <- checksums[!file.exists(path)]
  if (nrow(missing)) {
    msg <- paste("Missing expected input files:", paste(missing$file, collapse = ", "))
    if (allow_download) {
      warning(msg, immediate. = TRUE)
    } else {
      stop(msg, call. = FALSE)
    }
  }

  bad <- list()
  for (i in seq_len(nrow(checksums))) {
    row <- checksums[i]
    if (!file.exists(row$path)) next
    algo <- if (is.null(row$algorithm)) "xxhash64" else as.character(row$algorithm)
    digest_val <- tryCatch(digest::digest(row$path, algo, file = TRUE), error = function(...) NA_character_)
    if (!is.na(row$checksum) && !is.na(digest_val) && !identical(row$checksum, digest_val)) {
      bad[[length(bad) + 1]] <- row$file
    }
  }
  if (length(bad)) {
    msg <- paste("Checksum mismatch for:", paste(bad, collapse = ", "))
    if (allow_download) {
      warning(msg, immediate. = TRUE)
    } else {
      stop(msg, call. = FALSE)
    }
  }
  invisible(TRUE)
}

load_disturbance_dt <- function(input_root, cfg = NULL) {
  default_cand <- c(
    file.path(input_root, "disturbanceDT.csv"),
    file.path(input_root, "DisturbanceDT.csv"),
    file.path(project_root, "modules", "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
  )

  nested <- character(0)
  if (dir.exists(input_root)) {
    nested <- list.files(input_root, pattern = "disturbanceDT\\.csv$|DisturbanceDT\\.csv$", recursive = TRUE, full.names = TRUE)
  }
  cand <- if (length(nested)) nested else default_cand

  cand <- unique(cand[file.exists(cand)])
  if (!length(cand)) return(NULL)

  tabs <- lapply(cand, function(p) tryCatch(data.table::fread(p), error = function(...) NULL))
  tabs <- tabs[!vapply(tabs, is.null, logical(1))]
  if (!length(tabs)) return(NULL)

  dt <- data.table::rbindlist(tabs, use.names = TRUE, fill = TRUE)

  if (!"URL" %in% names(dt)) dt[, URL := NA_character_]
  dt[, URL := {
    u <- as.character(URL)
    u[!nzchar(u)] <- NA_character_
    u
  }]
  dt[, local_path := {
    fn <- as.character(fileName)
    out <- rep(NA_character_, length(fn))
    parent_input_root <- tryCatch(normalizePath(file.path(input_root, ".."), winslash = "/", mustWork = FALSE), error = function(...) NULL)
    for (i in seq_along(fn)) {
      f <- fn[[i]]
      candidates <- c(
        if (grepl("^(/|[A-Za-z]:)", f)) f else NULL,
        file.path(project_root, f),
        file.path(project_root, "data", f),
        if (!is.null(parent_input_root)) file.path(parent_input_root, f) else NULL,
        file.path(input_root, f)
      )
      candidates <- candidates[nzchar(candidates)]
      hit <- NA_character_
      for (cand in candidates) {
        cand_norm <- normalizePath(cand, winslash = "/", mustWork = FALSE)
        if (file.exists(cand_norm) || dir.exists(cand_norm)) {
          hit <- cand_norm
          break
        }
      }
      out[[i]] <- hit
    }
    out
  }]
  dt[is.na(URL), URL := local_path]
  dt[, URL := ifelse(
    grepl("^file://", URL) | dir.exists(URL),
    URL,
    ifelse(!is.na(URL), paste0("file://", utils::URLencode(URL, reserved = FALSE)), NA_character_)
  )]
  dt[, local_path := NULL]
  dt
}

load_disturbance_rates <- function(input_root, cfg = NULL) {
  explicit <- NULL
  if (is.list(cfg) && length(cfg)) {
    explicit <- cfg$disturbance_rate_file %||% cfg$paths$disturbance_rate_file %||% cfg$paths$disturbance_rates
  }

  if (is.null(explicit) || !nzchar(explicit)) return(NULL)
  candidate <- tryCatch(normalizePath(explicit, winslash = "/", mustWork = FALSE), error = function(...) explicit)
  if (!file.exists(candidate)) {
    warning("disturbance_rate_file not found; continuing with module defaults: ", candidate, immediate. = TRUE)
    return(NULL)
  }

  dt <- tryCatch(data.table::fread(candidate), error = function(...) NULL)
  if (is.null(dt) || !nrow(dt)) return(NULL)
  dt
}

should_use_wind_data <- function(cfg) {
  gen <- cfg$params$anthroDisturbance_Generator
  if (is.null(gen)) return(TRUE)
  val <- gen$use_wind_data
  if (is.null(val)) return(TRUE)
  if (is.logical(val)) return(isTRUE(val))
  if (is.character(val)) return(tolower(val) %in% c("true", "t", "yes", "y", "1"))
  if (is.numeric(val)) return(val != 0)
  TRUE
}

drop_wind_from_disturbance <- function(dt) {
  if (is.null(dt)) return(NULL)
  cols <- intersect(c("dataName", "classToSearch", "dataClass", "fileName", "dataType"), names(dt))
  if (!length(cols)) return(dt)
  combined <- apply(dt[, cols, with = FALSE], 1, paste, collapse = "|")
  mask <- grepl("wind", combined, ignore.case = TRUE)
  if (!any(mask)) return(dt)
  dt <- dt[!mask]
  dt
}

drop_wind_from_params <- function(dt) {
  if (is.null(dt)) return(NULL)
  cols <- intersect(c("dataName", "dataClass", "disturbanceOrigin", "disturbanceEnd", "disturbanceType"), names(dt))
  if (!length(cols)) return(dt)
  combined <- apply(dt[, cols, with = FALSE], 1, paste, collapse = "|")
  mask <- grepl("wind", combined, ignore.case = TRUE)
  if (!any(mask)) return(dt)
  dt <- dt[!mask]
  dt
}

normalize_generator_params <- function(cfg) {
  if (is.null(cfg$params$anthroDisturbance_Generator)) return(cfg)
  gen <- cfg$params$anthroDisturbance_Generator

  # Split semicolon-delimited siteSelection values into a character vector
  if (!is.null(gen$siteSelectionAsDistributing) && is.character(gen$siteSelectionAsDistributing)) {
    val <- gen$siteSelectionAsDistributing
    if (length(val) == 1L) {
      sval <- trimws(val)
      if (identical(sval, "") || tolower(sval) %in% c("na", "null")) {
        gen$siteSelectionAsDistributing <- character(0)
      } else if (grepl(";", sval, fixed = TRUE)) {
        parts <- trimws(strsplit(sval, ";", fixed = TRUE)[[1]])
        gen$siteSelectionAsDistributing <- parts[nzchar(parts)]
      }
    }
  }

  cfg$params$anthroDisturbance_Generator <- gen
  cfg
}

start_logging <- function(log_file) {
  if (identical(Sys.getenv("RUNNER_NO_SINK"), "1")) {
    return(list(con = NULL, disabled = TRUE))
  }
  ensure_dir(dirname(log_file))
  con <- file(log_file, open = "wt")
  sink(con, split = TRUE)
  tryCatch(
    sink(con, type = "message", split = TRUE),
    error = function(...) sink(con, type = "message", split = FALSE)
  )
  try(futile.logger::flog.appender(futile.logger::appender.file(log_file)), silent = TRUE)
  list(con = con, disabled = FALSE)
}

stop_logging <- function(handle) {
  if (is.null(handle) || isTRUE(handle$disabled)) return(invisible())
  try(sink(type = "message"), silent = TRUE)
  try(sink(), silent = TRUE)
  try(close(handle$con), silent = TRUE)
}

append_run_log <- function(row, suite, runs_csv_path = NULL) {
  cols <- c(
    "timestamp", "suite", "run_name", "replicate", "seed",
    "config_file", "modules", "data_profile", "input_root",
    "output_dir", "log_file", "status", "error_message"
  )
  missing_cols <- setdiff(cols, names(row))
  if (length(missing_cols)) {
    stop("append_run_log: missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  runs_csv <- runs_csv_path %||% file.path(project_root, "workspace", suite, "runs.csv")
  ensure_dir(dirname(runs_csv))
  df <- as.data.frame(row[cols], stringsAsFactors = FALSE)
  write.table(
    df,
    file = runs_csv,
    sep = ",",
    qmethod = "double",
    row.names = FALSE,
    col.names = !file.exists(runs_csv),
    append = file.exists(runs_csv),
    quote = TRUE
  )
}

run_replicate <- function(cfg, rep_id, seed, disturbance_dt) {
  rep_tag <- sprintf("rep_%03d", rep_id)
  output_dir <- ensure_dir(file.path(cfg$paths$output_root, cfg$suite, cfg$run_name, rep_tag))
  scratch_dir <- ensure_dir(file.path(cfg$paths$scratch_root, cfg$suite, cfg$run_name, rep_tag))
  log_file <- normalizePath(
    file.path(cfg$paths$scratch_root, cfg$suite, cfg$run_name, paste0(rep_tag, ".log")),
    winslash = "/",
    mustWork = FALSE
  )
  ensure_dir(dirname(log_file))

  log_handle <- start_logging(log_file)
  on.exit(stop_logging(log_handle), add = TRUE)

  status <- "success"
  err_msg <- ""

  opts_list <- list(
    spades.inputPath = cfg$paths$input_root,
    spades.dataPath = cfg$paths$input_root,
    spades.outputPath = output_dir,
    spades.scratchPath = scratch_dir,
    spades.seed = seed,
    spades.useRequire = if (identical(cfg$suite, "e2e_dummy")) FALSE else getOption("spades.useRequire", TRUE),
    spades.allowInitDuringSimInit = TRUE,
    spades.recoveryMode = 0,
    spades.DTthreads = runner_core_budget$per_job,
    reproducible.inputPaths = cfg$paths$input_root,
    reproducible.destinationPath = cfg$paths$input_root,
    reproducible.cachePath = file.path(project_root, "cache", "workspace"),
    reproducible.overwrite = TRUE,
    reproducible.checkHash = FALSE,
    reproducible.useCloud = FALSE,
    reproducible.gdalwarp = TRUE,
    runner.dataPath = cfg$paths$input_root
  )
  old_opts <- options(opts_list)
  on.exit(do.call(options, old_opts), add = TRUE)

  set.seed(seed)
  try(futile.logger::flog.info(paste0("[", rep_tag, "] Starting replicate at ", timestamp_now())), silent = TRUE)

  log_line <- function(txt) {
    msg <- paste0("[", rep_tag, "] ", txt)
    message(msg)
    try(cat(msg, file = log_file, append = TRUE, sep = "\n"), silent = TRUE)
    try(futile.logger::flog.info(msg), silent = TRUE)
  }

  log_line(paste0("Options: reproducible.destinationPath=", getOption("reproducible.destinationPath"),
                  "; spades.inputPath=", getOption("spades.inputPath"),
                  "; spades.outputPath=", getOption("spades.outputPath")))

  link_geodata_cache(cfg$paths$input_root, cfg$geodata_path, log_line)

  objects <- list()
  is_valid_spat <- function(obj) {
    tryCatch({
      if (inherits(obj, "SpatRaster")) {
        terra::nlyr(obj)
      } else if (inherits(obj, "SpatVector")) {
        terra::nrow(obj)
      }
      TRUE
    }, error = function(...) FALSE)
  }
  
  log_line(paste0("Modules being run: ", paste(cfg$modules, collapse = ",")))

  if (!is.null(disturbance_dt)) objects$disturbanceDT <- disturbance_dt
  # Only load DisturbanceRate when explicitly supplied in config (paths$disturbance_rate_file or disturbance_rate_file).
  drates <- NULL
  explicit_rate <- cfg$disturbance_rate_file %||% cfg$paths$disturbance_rate_file %||% cfg$paths$disturbance_rates
  if (!is.null(explicit_rate) && nzchar(explicit_rate)) {
    drates <- load_disturbance_rates(cfg$paths$input_root, cfg)
  }
  if (!is.null(drates)) {
    objects$DisturbanceRate <- drates
    log_line(paste0("DisturbanceRate loaded (rows=", nrow(drates), " cols=", ncol(drates), ")"))
    # Provide a custom disturbanceParameters table so scheduling matches the short AD/QD windows.
    params_path <- file.path(cfg$paths$module_path, "anthroDisturbance_Generator", "data", "paramsGeneral.txt")
    if (file.exists(params_path)) {
      base_params <- data.table::data.table(dget(params_path))
      # Merge rates into the base parameters.
      override_cols <- c("disturbanceRate", "disturbanceSize", "disturbanceInterval", "resolutionVector")
      merge_cols <- setdiff(intersect(names(base_params), names(drates)), override_cols)
      if (!all(c("dataName", "dataClass") %in% merge_cols)) merge_cols <- c("dataName", "dataClass")
      custom_params <- merge(
        base_params,
        drates,
        by = merge_cols,
        all.x = TRUE,
        suffixes = c("", ".rate")
      )
      # Use supplied rates when available.
      if ("disturbanceRate.rate" %in% names(custom_params)) {
        custom_params[, disturbanceRate :=
                        ifelse(!is.na(`disturbanceRate.rate`),
                               `disturbanceRate.rate`,
                               disturbanceRate)]
        custom_params[, `disturbanceRate.rate` := NULL]
        custom_params[, disturbanceRate := suppressWarnings(as.numeric(disturbanceRate))]
      }
      if ("disturbanceSize.rate" %in% names(custom_params)) {
        custom_params[, disturbanceSize :=
                        fifelse(!is.na(`disturbanceSize.rate`) & nzchar(`disturbanceSize.rate`),
                                `disturbanceSize.rate`,
                                disturbanceSize)]
        custom_params[, `disturbanceSize.rate` := NULL]
      }
      if ("disturbanceInterval.rate" %in% names(custom_params)) {
        custom_params[, disturbanceInterval :=
                        fifelse(!is.na(`disturbanceInterval.rate`),
                                as.integer(`disturbanceInterval.rate`),
                                disturbanceInterval)]
        custom_params[, `disturbanceInterval.rate` := NULL]
      } else {
        custom_params[, disturbanceInterval := suppressWarnings(as.integer(disturbanceInterval))]
      }
      if ("resolutionVector.rate" %in% names(custom_params)) {
        custom_params[, resolutionVector :=
                        fifelse(!is.na(`resolutionVector.rate`),
                                as.integer(`resolutionVector.rate`),
                                resolutionVector)]
        custom_params[, `resolutionVector.rate` := NULL]
      }
      # Tighten interval to annual for anything with a finite rate so events are scheduled inside 2010–2015.
      custom_params[is.finite(disturbanceRate), disturbanceInterval := 1L]
      objects$disturbanceParameters <- custom_params
      log_line("Custom disturbanceParameters built from paramsGeneral + disturbanceRates (interval=1 for supplied rates).")
    } else {
      log_line("paramsGeneral.txt not found; using module defaults for disturbanceParameters.")
    }
  } else {
    log_line("DisturbanceRate not supplied in config; relying on module defaults (.inputObjects).")
    if (identical(cfg$suite, "adqd_validation") && is.null(objects$disturbanceParameters)) {
      params_path <- file.path(cfg$paths$module_path, "anthroDisturbance_Generator", "data", "paramsGeneral.txt")
      if (file.exists(params_path)) {
        adqd_params <- data.table::data.table(dget(params_path))
        adqd_params[, disturbanceInterval := 1L]
        objects$disturbanceParameters <- adqd_params
        log_line("ADQD suite: disturbanceInterval forced to 1 year for ECCC-derived runs.")
      } else {
        log_line("ADQD suite requested interval override but paramsGeneral.txt was not found.")
      }
    }
  }
  if (!should_use_wind_data(cfg)) {
    if (!is.null(objects$disturbanceParameters)) {
      before_n <- nrow(objects$disturbanceParameters)
      objects$disturbanceParameters <- drop_wind_from_params(objects$disturbanceParameters)
      after_n <- if (is.null(objects$disturbanceParameters)) 0L else nrow(objects$disturbanceParameters)
      log_line(sprintf(
        "use_wind_data is FALSE; removed %s wind-related entries from disturbanceParameters",
        before_n - after_n
      ))
    }
    if (!is.null(objects$DisturbanceRate)) {
      before_n <- nrow(objects$DisturbanceRate)
      objects$DisturbanceRate <- drop_wind_from_params(objects$DisturbanceRate)
      after_n <- if (is.null(objects$DisturbanceRate)) 0L else nrow(objects$DisturbanceRate)
      log_line(sprintf(
        "use_wind_data is FALSE; removed %s wind-related entries from DisturbanceRate",
        before_n - after_n
      ))
    }
  }
  if (!is.null(cfg$study_area)) {
    sa_path <- cfg$study_area
    if (!grepl("^(/|[A-Za-z]:)", sa_path)) {
      sa_path <- file.path(project_root, sa_path)
    }
    sa_path <- tryCatch(normalizePath(sa_path, winslash = "/", mustWork = FALSE), error = function(...) sa_path)
    if (file.exists(sa_path)) {
      sa_obj <- tryCatch(terra::vect(sa_path), error = function(...) NULL)
      if (!is.null(sa_obj)) objects$studyArea <- sa_obj
    }
  }
  if (!is.null(cfg$raster_to_match)) {
    rtm_path <- cfg$raster_to_match
    if (!grepl("^(/|[A-Za-z]:)", rtm_path)) {
      rtm_path <- file.path(project_root, rtm_path)
    }
    rtm_path <- tryCatch(normalizePath(rtm_path, winslash = "/", mustWork = FALSE), error = function(...) rtm_path)
    if (file.exists(rtm_path)) {
      rtm_obj <- tryCatch(terra::rast(rtm_path), error = function(...) NULL)
      if (!is.null(rtm_obj)) objects$rasterToMatch <- rtm_obj
    } else {
      log_line(paste0("raster_to_match path not found: ", rtm_path))
    }
  }
  if (is.null(objects$rasterToMatch)) {
    fallback_rtm <- file.path(project_root, "data", "study_area", "aoi_southwest_NWT_RTM_250m.tif")
    if (!file.exists(fallback_rtm)) {
      fallback_rtm <- file.path(project_root, "data", "raw", "RTM.tif")
    }
    if (file.exists(fallback_rtm)) {
      rtm_obj <- tryCatch(terra::rast(fallback_rtm), error = function(...) NULL)
      if (!is.null(rtm_obj)) {
        objects$rasterToMatch <- rtm_obj
        log_line(paste0("Loaded fallback rasterToMatch from ", fallback_rtm))
      }
    }
  }
  if (!is.null(objects$studyArea) && !is.null(objects$rasterToMatch)) {
    sa_crs <- tryCatch(terra::crs(objects$studyArea), error = function(...) "")
    rtm_crs <- tryCatch(terra::crs(objects$rasterToMatch), error = function(...) "")
    if (!nzchar(sa_crs) && nzchar(rtm_crs)) {
      terra::crs(objects$studyArea) <- rtm_crs
      log_line("Assigned rasterToMatch CRS to studyArea (studyArea CRS missing).")
    } else if (nzchar(sa_crs) && nzchar(rtm_crs) &&
               !terra::same.crs(objects$studyArea, objects$rasterToMatch)) {
      objects$studyArea <- tryCatch(
        terra::project(objects$studyArea, objects$rasterToMatch),
        error = function(e) {
          log_line(paste0("Failed to project studyArea to rasterToMatch CRS: ", conditionMessage(e)))
          objects$studyArea
        }
      )
      log_line("Projected studyArea to match rasterToMatch CRS.")
    }
  }
  if (!is.null(cfg$features_to_avoid)) {
    fta_path <- cfg$features_to_avoid
    if (!grepl("^(/|[A-Za-z]:)", fta_path)) {
      fta_path <- file.path(project_root, fta_path)
    }
    fta_path <- tryCatch(normalizePath(fta_path, winslash = "/", mustWork = FALSE), error = function(...) fta_path)
    if (file.exists(fta_path)) {
      fta_obj <- tryCatch(terra::rast(fta_path), error = function(...) NULL)
      if (is.null(fta_obj)) {
        fta_obj <- tryCatch(terra::vect(fta_path), error = function(...) NULL)
      }
      if (!is.null(fta_obj) && is_valid_spat(fta_obj)) {
        objects$featuresToAvoid <- fta_obj
        log_line(paste0("Loaded featuresToAvoid from ", fta_path))
      } else {
        log_line(paste0("Failed to load featuresToAvoid from ", fta_path))
      }
    } else {
      log_line(paste0("featuresToAvoid path not found: ", fta_path))
    }
  }
  if (!is.null(cfg$dem)) {
    dem_path <- cfg$dem
    if (!grepl("^(/|[A-Za-z]:)", dem_path)) {
      dem_path <- file.path(project_root, dem_path)
    }
    dem_path <- tryCatch(normalizePath(dem_path, winslash = "/", mustWork = FALSE), error = function(...) dem_path)
    if (file.exists(dem_path)) {
      dem_obj <- tryCatch(terra::rast(dem_path), error = function(...) NULL)
      if (!is.null(dem_obj) && is_valid_spat(dem_obj)) {
        objects$DEM <- dem_obj
        log_line(paste0("Loaded DEM from ", dem_path))
      } else {
        log_line(paste0("Failed to load DEM from ", dem_path))
      }
    } else {
      log_line(paste0("DEM path not found: ", dem_path))
    }
  }
  log_line(
    paste0("Objects injected: ",
           paste(names(objects), collapse = ","), "; disturbanceDT rows: ",
           if (is.null(disturbance_dt)) 0L else nrow(disturbance_dt))
  )

  sim <- NULL
  res <- NULL
  tryCatch({
    obj_list <- objects
    inputs_list <- NULL
    if (!is.null(disturbance_dt)) obj_list$disturbanceDT <- disturbance_dt

    sim <- SpaDES.core::simInit(
      times   = cfg$times,
      params  = cfg$params,
      modules = cfg$modules,
      paths   = list(
        modulePath  = cfg$paths$module_path,
        inputPath   = cfg$paths$input_root,
        outputPath  = output_dir,
        scratchPath = scratch_dir,
        cachePath   = file.path(project_root, "cache", "workspace")
      ),
      objects = obj_list
    )
    sim_str <- tryCatch(capture.output(str(sim, max.level = 0))[1],
                        error = function(...) NA_character_)
    log_line(paste0("simInit completed; sim class: ", paste(class(sim), collapse = "/"),
                    "; str: ", sim_str))
    if (is.null(sim)) {
      try(writeLines("simInit returned NULL", file.path(scratch_dir, "simInit_null.txt")), silent = TRUE)
      stop("simInit returned NULL")
    } else {
      try(saveRDS(sim, file.path(scratch_dir, "sim_after_init.rds")), silent = TRUE)
    }
    res <- SpaDES.core::spades(sim)
  }, error = function(e) {
    status <<- "error"
    err_msg <<- format_error_message(e)
    inert <- capture.output(traceback())
    calls <- capture.output(sys.calls())
    err_call <- capture.output(conditionCall(e))
    if (requireNamespace("rlang", quietly = TRUE)) {
      rb <- tryCatch(rlang::trace_back(error = e), error = function(...) NULL)
      rb_lines <- if (is.null(rb)) character(0) else capture.output(rb)
    } else {
      rb_lines <- character(0)
    }
    try(futile.logger::flog.error(paste0("[", rep_tag, "] ERROR during run: ", err_msg)), silent = TRUE)
    if (length(inert)) try(cat(paste(inert, collapse = "\n"), file = log_file, append = TRUE), silent = TRUE)
    if (length(calls)) try(cat(paste(calls, collapse = "\n"), file = log_file, append = TRUE), silent = TRUE)
    if (length(err_call)) try(cat(paste(err_call, collapse = "\n"), file = log_file, append = TRUE), silent = TRUE)
    if (length(rb_lines)) try(cat(paste(rb_lines, collapse = "\n"), file = log_file, append = TRUE), silent = TRUE)
    res <<- NULL
  })

  if (is.null(sim)) {
    err_msg <- err_msg %||% "simInit returned NULL"
    status <- "error"
  }

  end_time <- timestamp_now()

  append_run_log(
    row = list(
      timestamp = end_time,
      suite = cfg$suite,
      run_name = cfg$run_name,
      replicate = rep_id,
      seed = seed,
      config_file = relative_to_root(cfg$config_file),
      modules = paste(cfg$modules, collapse = ","),
      data_profile = cfg$data_profile,
      input_root = relative_to_root(cfg$paths$input_root),
      output_dir = relative_to_root(output_dir),
      log_file = relative_to_root(log_file),
      status = status,
      error_message = err_msg %||% ""
    ),
    suite = cfg$suite
  )

  list(
    status = status,
    error_message = err_msg,
    output_dir = output_dir,
    log_file = log_file,
    sim = sim,
    result = res
  )
}

should_launch_live_maps <- function(cfg) {
  live_cfg <- cfg$live_maps %||% list()
  if (!isTRUE(live_cfg$enabled)) return(FALSE)
  if (!interactive()) {
    message("Live maps were requested but the session is not interactive; skipping SpaDES.shiny::shine().")
    return(FALSE)
  }
  TRUE
}

launch_live_maps <- function(sim, cfg, replicate_id = NULL) {
  if (is.null(sim)) return(invisible())
  if (!requireNamespace("SpaDES.shiny", quietly = TRUE)) {
    stop(
      "Live maps require the SpaDES.shiny package; install it with install.packages('SpaDES.shiny').",
      call. = FALSE
    )
  }
  title <- cfg$live_maps$title
  if (!nzchar(title)) {
    title <- paste(toupper(cfg$suite), cfg$run_name)
  }
  if (!is.null(replicate_id)) {
    rep_str <- tryCatch(as.integer(replicate_id), warning = function(...) NA_integer_)
    if (!is.na(rep_str)) {
      title <- sprintf("%s (replicate %03d)", title, rep_str)
    }
  }
  message("Launching SpaDES.shiny::shine() for live maps of ", cfg$run_name, ".")
  SpaDES.shiny::shine(
    sim,
    title = title,
    debug = cfg$live_maps$debug,
    filesOnly = cfg$live_maps$filesOnly
  )
  invisible(sim)
}

# --- main --------------------------------------------------------------------

config_path <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
config_raw <- load_config(config_path)
config_raw$config_file <- config_path
cfg <- validate_config(config_raw)
cfg <- normalize_generator_params(cfg)
tryCatch({
  # Reduce terra threading to improve stability on some platforms
  try(terra::terraOptions(numThreads = 1L), silent = TRUE)

  seeds <- derive_seeds(cfg)

  cfg$paths$input_root <- normalizePath(cfg$paths$input_root, winslash = "/", mustWork = TRUE)
  cfg$paths$output_root <- normalizePath(cfg$paths$output_root, winslash = "/", mustWork = FALSE)
  cfg$paths$scratch_root <- normalizePath(cfg$paths$scratch_root, winslash = "/", mustWork = FALSE)
  cfg$paths$module_path <- normalizePath(cfg$paths$module_path, winslash = "/", mustWork = TRUE)

  ensure_dir(cfg$paths$output_root)
  ensure_dir(cfg$paths$scratch_root)
  cache_root <- ensure_dir(file.path(project_root, "cache", "workspace"))

  checksums <- load_checksums(cfg$paths$input_root)
  verify_inputs(checksums, cfg$input_behaviour$allow_download_if_missing)
  disturbance_dt <- load_disturbance_dt(cfg$paths$input_root, cfg)
  use_wind <- should_use_wind_data(cfg)
  if (!use_wind && !is.null(disturbance_dt)) {
    before_n <- nrow(disturbance_dt)
    disturbance_dt <- drop_wind_from_disturbance(disturbance_dt)
    after_n <- if (is.null(disturbance_dt)) 0L else nrow(disturbance_dt)
    message(
      sprintf(
        "use_wind_data is FALSE; removed %s wind-related entries from disturbanceDT",
        before_n - after_n
      )
    )
  }

  run_rows <- list()
  overall_status <- 0L
  last_successful_sim <- NULL
  last_successful_rep <- NULL

  for (rep_id in seq_len(cfg$n_reps)) {
    seed <- seeds[[rep_id]]
    res <- run_replicate(cfg, rep_id, seed, disturbance_dt)
    if (!identical(res$status, "success")) overall_status <- 1L
    run_rows[[length(run_rows) + 1]] <- res
    if (identical(res$status, "success") && !is.null(res$sim)) {
      last_successful_sim <- res$sim
      last_successful_rep <- rep_id
    }
  }

  if (!is.null(last_successful_sim) && should_launch_live_maps(cfg)) {
    launch_live_maps(last_successful_sim, cfg, last_successful_rep)
  }

  if (overall_status != 0L) {
    quit(save = "no", status = overall_status, runLast = FALSE)
  } else {
    quit(save = "no", status = 0, runLast = FALSE)
  }
}, error = function(e) {
  log_config_failure(cfg, format_error_message(e))
  stop(e)
})
