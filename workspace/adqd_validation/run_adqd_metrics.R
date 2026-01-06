#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

parse_args <- function(args) {
  opts <- list(
    config_dir = file.path(project_root, "workspace", "adqd_validation", "config"),
    bead_root = file.path(project_root, "data", "raw", "ECCC"),
    mode = "default",
    dry_run = FALSE,
    help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
    } else if (arg == "--dry-run") {
      opts$dry_run <- TRUE
    } else if (grepl("^--config-dir=", arg, ignore.case = TRUE)) {
      opts$config_dir <- sub("^--config-dir=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--bead-root=", arg, ignore.case = TRUE)) {
      opts$bead_root <- sub("^--bead-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--mode=", arg, ignore.case = TRUE)) {
      opts$mode <- tolower(sub("^--mode=", "", arg, ignore.case = TRUE))
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/adqd_validation/run_adqd_metrics.R [options]\n",
    "  --config-dir=DIR   Config directory (default workspace/adqd_validation/config)\n",
    "  --bead-root=DIR    BEAD archive root (default data/raw/ECCC)\n",
    "  --mode=default|all  default = verification/holdout (+ caribou); all = include map helper configs\n",
    "  --dry-run          Print planned compute_map_metrics calls only\n",
    "  --help             Show this message\n"
  ))
}

normalize_existing <- function(path_value) {
  normalizePath(path_value, winslash = "/", mustWork = TRUE)
}

intervals_from_config <- function(cfg) {
  start_year <- as.integer(cfg$times$start %||% NA)
  end_year <- as.integer(cfg$times$end %||% NA)
  if (is.na(start_year) || is.na(end_year)) return(list())

  run_interval <- cfg$params$anthroDisturbance_Generator$runInterval %||% (end_year - start_year)
  run_interval <- suppressWarnings(as.integer(run_interval))
  if (is.na(run_interval) || run_interval <= 0) run_interval <- end_year - start_year

  if ((end_year - start_year) > run_interval && run_interval > 0) {
    breaks <- seq(start_year, end_year, by = run_interval)
    if (tail(breaks, 1) != end_year) breaks <- c(breaks, end_year)
    intervals <- lapply(seq_len(length(breaks) - 1), function(i) c(breaks[i], breaks[i + 1]))
  } else {
    intervals <- list(c(start_year, end_year))
  }
  intervals
}

intervals_to_flag <- function(intervals) {
  parts <- vapply(intervals, function(x) paste0(x[[1]], ":", x[[2]]), character(1))
  paste(parts, collapse = ",")
}

replicates_from_config <- function(cfg) {
  n_reps <- cfg$n_reps %||% 1L
  n_reps <- suppressWarnings(as.integer(n_reps))
  if (is.na(n_reps) || n_reps < 1) n_reps <- 1L
  seq_len(n_reps)
}

bead_year_available <- function(bead_root, year) {
  if (year == 2010L) {
    file.exists(file.path(bead_root, "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip"))
  } else if (year == 2015L) {
    file.exists(file.path(bead_root, "ECCC_2015_anthro_dist_corrected_to_NT1_2016_final.zip"))
  } else if (year == 2020L) {
    file.exists(file.path(bead_root, "NorthwestTerritories2020.gdb.zip")) ||
      file.exists(file.path(bead_root, "NWT2020_Disturb_Perturb_Line_valid.gpkg")) ||
      file.exists(file.path(bead_root, "NWT2020_Disturb_Perturb_Poly_valid.gpkg"))
  } else {
    TRUE
  }
}

opts <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(opts$help)) {
  print_usage()
  quit(save = "no", status = 0, runLast = FALSE)
}

config_dir <- normalize_existing(opts$config_dir)
bead_root <- normalize_existing(opts$bead_root)

all_configs <- list.files(config_dir, pattern = "^adqd_.*\\.yaml$", full.names = TRUE)
if (!length(all_configs)) stop("No adqd_*.yaml configs found under ", config_dir, call. = FALSE)

default_configs <- c(
  "adqd_verification.yaml",
  "adqd_holdout.yaml",
  "adqd_verification_caribou.yaml",
  "adqd_holdout_caribou.yaml"
)

if (identical(opts$mode, "default")) {
  configs <- all_configs[basename(all_configs) %in% default_configs]
} else if (identical(opts$mode, "all")) {
  configs <- all_configs
} else {
  stop("Invalid --mode: ", opts$mode, call. = FALSE)
}

if (!length(configs)) stop("No configs selected for mode ", opts$mode, call. = FALSE)

overall_status <- 0L
for (cfg_path in configs) {
  cfg <- yaml::read_yaml(cfg_path)
  run_name <- cfg$run_name %||% basename(cfg_path)
  analysis_mode <- toupper(gsub("^ADQD_", "", run_name))
  simulation_root <- file.path(project_root, "outputs", "adqd_validation", run_name)
  output_root <- file.path(project_root, "outputs", "adqd_validation", "results", analysis_mode)
  study_area <- cfg$study_area %||% file.path(project_root, "data", "study_area", "NWT_boundary.shp")

  if (!dir.exists(simulation_root)) {
    warning("Missing simulation outputs: ", simulation_root, immediate. = TRUE)
    overall_status <- 1L
    next
  }

  intervals <- intervals_from_config(cfg)
  if (!length(intervals)) {
    warning("Unable to derive intervals for ", cfg_path, immediate. = TRUE)
    overall_status <- 1L
    next
  }

  years_needed <- unique(unlist(intervals))
  missing_years <- years_needed[!vapply(years_needed, bead_year_available, logical(1), bead_root = bead_root)]
  if (length(missing_years)) {
    warning("Skipping metrics for ", run_name, ": missing BEAD data for ", paste(missing_years, collapse = ", "), immediate. = TRUE)
    overall_status <- 1L
    next
  }

  rep_ids <- replicates_from_config(cfg)
  interval_flag <- intervals_to_flag(intervals)
  rep_flag <- paste(rep_ids, collapse = ",")

  cmd <- c(
    file.path(project_root, "workspace", "adqd_validation", "compute_map_metrics.R"),
    paste0("--simulation-root=", simulation_root),
    paste0("--output-root=", output_root),
    paste0("--bead-root=", bead_root),
    paste0("--study-area=", study_area),
    paste0("--intervals=", interval_flag),
    paste0("--replicates=", rep_flag),
    paste0("--analysis-mode=", analysis_mode)
  )

  if (grepl("CARIBOU", analysis_mode)) {
    cmd <- c(cmd, "--caribou-buffer")
  }

  message("Running ADQD metrics for ", run_name)
  if (isTRUE(opts$dry_run)) {
    message("Dry run: Rscript ", paste(cmd, collapse = " "))
    next
  }

  status <- system2("Rscript", cmd)
  if (!identical(status, 0L)) {
    warning("compute_map_metrics failed for ", run_name, " (status ", status, ")", immediate. = TRUE)
    overall_status <- 1L
  }
}

if (!identical(overall_status, 0L)) {
  quit(save = "no", status = overall_status, runLast = FALSE)
}

message("ADQD metrics complete.")
