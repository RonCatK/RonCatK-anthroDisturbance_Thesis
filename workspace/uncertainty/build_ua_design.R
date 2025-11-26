#!/usr/bin/env Rscript
# Build a lightweight UA design by sampling parameter ranges and emitting per-run configs.
# Keeps the number of runs modest by default (10 samples × n_reps from base config).

suppressPackageStartupMessages({
  library(yaml)
  library(data.table)
})

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

default_opts <- list(
  parameters_file = file.path(project_root, "workspace", "uncertainty", "config", "ua_parameters.yaml"),
  base_config = file.path(project_root, "workspace", "uncertainty", "config", "ua_base.yaml"),
  probability_file = file.path(project_root, "workspace", "uncertainty", "config", "probabilityDisturbance.yaml"),
  samples = 10L,          # keep small to avoid explosion
  replicates = NA_integer_, # if NA, use n_reps from base config
  scenario_prefix = "ua_random",
  design_output = file.path(project_root, "workspace", "uncertainty", "config", "ua_design_points.csv"),
  runs_csv = file.path(project_root, "workspace", "uncertainty", "config", "ua_runs.csv"),
  config_dir = file.path(project_root, "workspace", "uncertainty", "config", "generated"),
  seed = 12345L,
  runs_mode = "replace",
  help = FALSE
)

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) return(opts)
  for (arg in args) {
    if (arg %in% c("--help", "-h")) opts$help <- TRUE
    else if (grepl("^--parameters=", arg, ignore.case = TRUE)) opts$parameters_file <- sub("^--parameters=", "", arg, ignore.case = TRUE)
    else if (grepl("^--base-config=", arg, ignore.case = TRUE)) opts$base_config <- sub("^--base-config=", "", arg, ignore.case = TRUE)
    else if (grepl("^--probability-file=", arg, ignore.case = TRUE)) opts$probability_file <- sub("^--probability-file=", "", arg, ignore.case = TRUE)
    else if (grepl("^--samples=", arg, ignore.case = TRUE)) opts$samples <- as.integer(sub("^--samples=", "", arg, ignore.case = TRUE))
    else if (grepl("^--replicates=", arg, ignore.case = TRUE)) opts$replicates <- as.integer(sub("^--replicates=", "", arg, ignore.case = TRUE))
    else if (grepl("^--scenario-prefix=", arg, ignore.case = TRUE)) opts$scenario_prefix <- sub("^--scenario-prefix=", "", arg, ignore.case = TRUE)
    else if (grepl("^--design-output=", arg, ignore.case = TRUE)) opts$design_output <- sub("^--design-output=", "", arg, ignore.case = TRUE)
    else if (grepl("^--runs-csv=", arg, ignore.case = TRUE)) opts$runs_csv <- sub("^--runs-csv=", "", arg, ignore.case = TRUE)
    else if (grepl("^--config-dir=", arg, ignore.case = TRUE)) opts$config_dir <- sub("^--config-dir=", "", arg, ignore.case = TRUE)
    else if (grepl("^--seed=", arg, ignore.case = TRUE)) {
      seed_val <- sub("^--seed=", "", arg, ignore.case = TRUE)
      if (!nzchar(seed_val) || tolower(seed_val) %in% c("na", "null")) opts$seed <- NA_integer_
      else opts$seed <- as.integer(seed_val)
    }
    else if (grepl("^--runs-mode=", arg, ignore.case = TRUE)) {
      mode_val <- tolower(sub("^--runs-mode=", "", arg, ignore.case = TRUE))
      opts$runs_mode <- mode_val
    }
    else warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/uncertainty/build_ua_design.R [options]\n",
    "  --parameters=PATH     UA parameter definition YAML (default workspace/uncertainty/config/ua_parameters.yaml)\n",
    "  --base-config=PATH    Base config to clone (default workspace/uncertainty/config/ua_base.yaml)\n",
    "  --probability-file=PATH Optional probabilityDisturbance YAML to inject (default workspace/uncertainty/config/probabilityDisturbance.yaml if present)\n",
    "  --samples=N           Number of parameter samples (default 10)\n",
    "  --replicates=N        Replicates per sample (default = n_reps from base config)\n",
    "  --scenario-prefix=STR Prefix for run names (default ua_random)\n",
    "  --design-output=PATH  Where to write sampled parameter table (default .../ua_design_points.csv)\n",
    "  --runs-csv=PATH       UA runs index to update (default .../config/ua_runs.csv)\n",
    "  --config-dir=DIR      Where to write generated configs (default .../config/generated)\n",
    "  --seed=N              RNG seed for reproducible sampling (default 12345; set to NA to disable)\n",
    "  --runs-mode=replace|append  Replace (default) or append to the UA runs index\n",
    "  --help                Show this message\n"
  ))
}

read_parameter_definitions <- function(path) {
  if (!file.exists(path)) stop("Parameter definition file not found: ", path, call. = FALSE)
  cfg <- yaml::read_yaml(path)
  params <- cfg$parameters
  if (is.null(params) || !length(params)) stop("Parameter definition file contains no entries.", call. = FALSE)
  params
}

sample_param <- function(entry) {
  t <- tolower(entry$type)
  if (t == "continuous") {
    lo <- as.numeric(entry$min); hi <- as.numeric(entry$max)
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi) stop("Invalid bounds for ", entry$name)
    runif(1, lo, hi)
  } else if (t == "integer") {
    lo <- as.integer(entry$min); hi <- as.integer(entry$max)
    if (!is.finite(lo) || !is.finite(hi) || lo > hi) stop("Invalid integer bounds for ", entry$name)
    sample(seq(lo, hi), 1)
  } else if (t %in% c("discrete", "categorical")) {
    vals <- entry$values %||% entry$categories
    if (is.null(vals) || !length(vals)) stop("No values provided for ", entry$name)
    sample(vals, 1)
  } else {
    stop("Unsupported type for ", entry$name, ": ", entry$type)
  }
}

sanitize_anthro_params <- function(params) {
  if (is.null(params)) return(params)
  genRaster <- isTRUE(params$generatedDisturbanceAsRaster)
  useCluster <- isTRUE(params$useClusterMethod)

  if (isFALSE(params$maskWaterAndMountainsFromLines)) {
    params$altitudeCut <- NULL
  }

  if (genRaster) {
    drop <- c("useClusterMethod", "seismicLineGrids", "clusterDistance",
              "distanceNewLinesFactor", "runClusteringInParallel",
              "refinedStructure", "siteSelectionAsDistributing",
              "probabilityDisturbance", "maskWaterAndMountainsFromLines",
              "altitudeCut", "useRoadsPackage")
    params[drop] <- NULL
    return(params)
  }

  if (useCluster) {
    params$seismicLineGrids <- NULL
  } else {
    params$distanceNewLinesFactor <- NULL
    params$refinedStructure <- NULL
  }
  params
}

materialize_config <- function(base_cfg, insert_vals, n_reps, run_name, prob_override = NULL) {
  cfg <- base_cfg
  if (is.null(cfg$params)) cfg$params <- list()
  if (is.null(cfg$params$anthroDisturbance_Generator)) cfg$params$anthroDisturbance_Generator <- list()
  for (nm in names(insert_vals)) {
    cfg$params$anthroDisturbance_Generator[[nm]] <- insert_vals[[nm]]
  }
  cfg$params$anthroDisturbance_Generator <- sanitize_anthro_params(cfg$params$anthroDisturbance_Generator)
  if (!is.null(prob_override) && is.null(cfg$params$anthroDisturbance_Generator$probabilityDisturbance)) {
    cfg$params$anthroDisturbance_Generator$probabilityDisturbance <- prob_override
  }
  cfg$run_name <- run_name
  cfg$n_reps <- n_reps
  cfg
}

write_config <- function(cfg, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(cfg, path)
}

write_runs_csv <- function(runs_csv, rows_dt, mode = c("replace", "append")) {
  dir.create(dirname(runs_csv), recursive = TRUE, showWarnings = FALSE)
  mode <- match.arg(mode)
  if (!file.exists(runs_csv)) {
    fwrite(rows_dt, runs_csv)
    return(invisible(NULL))
  }
  existing <- tryCatch(fread(runs_csv, fill = TRUE), error = function(...) data.table())
  if (!nrow(existing)) {
    fwrite(rows_dt, runs_csv)
    return(invisible(NULL))
  }
  if (identical(mode, "append")) {
    combined <- rbindlist(list(existing, rows_dt), use.names = TRUE, fill = TRUE)
  } else {
    keep <- existing[!run_name %in% rows_dt$run_name]
    combined <- rbindlist(list(keep, rows_dt), use.names = TRUE, fill = TRUE)
  }
  fwrite(combined, runs_csv)
}

main <- function() {
  opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    print_usage()
    quit(save = "no", status = 0, runLast = FALSE)
  }

  params <- read_parameter_definitions(opts$parameters_file)
  base_cfg <- yaml::read_yaml(opts$base_config)
  if (is.null(base_cfg$paths$input_root)) stop("Base config missing paths/input_root.", call. = FALSE)
  prob_override <- NULL
  if (!is.null(opts$probability_file) && file.exists(opts$probability_file)) {
    prob_cfg <- yaml::read_yaml(opts$probability_file)
    prob_override <- prob_cfg$anthroDisturbance_Generator$probabilityDisturbance %||% prob_cfg$probabilityDisturbance %||% NULL
  }

  n_reps <- if (is.na(opts$replicates)) base_cfg$n_reps %||% 1L else opts$replicates
  samples <- max(1L, as.integer(opts$samples))
  opts$runs_mode <- ifelse(tolower(opts$runs_mode) %in% c("replace", "append"),
                           tolower(opts$runs_mode), "replace")
  seed_int <- if (!is.null(opts$seed) && !is.na(opts$seed)) as.integer(opts$seed) else NA_integer_
  if (!is.na(seed_int)) set.seed(seed_int)

  # Sample parameter table
  draws <- vector("list", samples)
  for (i in seq_len(samples)) {
    vals <- lapply(params, sample_param)
    names(vals) <- vapply(params, function(p) p$column, character(1))
    draws[[i]] <- vals
  }
  design_dt <- rbindlist(lapply(seq_along(draws), function(i) {
    as.data.table(c(list(sample_id = i), draws[[i]]))
  }), fill = TRUE)

  # Write configs and runs
  run_rows <- vector("list", samples)
  for (i in seq_len(samples)) {
    run_name <- sprintf("%s_%03d", opts$scenario_prefix, i)
    cfg_path <- file.path(opts$config_dir, paste0(run_name, ".yaml"))
    cfg <- materialize_config(base_cfg, draws[[i]], n_reps, run_name, prob_override = prob_override)
    write_config(cfg, cfg_path)
    desc <- paste0("UA sample ", i, " | ", paste(names(draws[[i]]), draws[[i]], sep = "=", collapse = "; "))
    vals_dt <- as.data.table(as.list(draws[[i]]))
    meta_dt <- data.table(
      run_name = run_name,
      scenario_id = run_name,
      cfg = cfg_path,
      desc = desc,
      design_sample = i,
      n_reps = n_reps,
      rng_seed = seed_int
    )
    run_rows[[i]] <- cbind(meta_dt, vals_dt)
  }
  design_dt[, run_name := sprintf("%s_%03d", opts$scenario_prefix, seq_len(samples))]
  design_dt[, rng_seed := seed_int]
  fwrite(design_dt, opts$design_output)

  write_runs_csv(opts$runs_csv, rbindlist(run_rows, use.names = TRUE, fill = TRUE), mode = opts$runs_mode)
  message(sprintf("Generated %d configs in %s", samples, opts$config_dir))
  message(sprintf("Design table: %s", opts$design_output))
  message(sprintf("Runs index updated: %s", opts$runs_csv))
}

tryCatch(main(), error = function(e) {
  message("build_ua_design failed: ", conditionMessage(e))
  quit(status = 1L)
})
