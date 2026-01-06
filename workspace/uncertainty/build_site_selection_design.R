#!/usr/bin/env Rscript
# Build a deterministic UA design that only varies siteSelectionAsDistributing.

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
})

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

relative_to_root <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NA_character_)
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(project_root, "/")
  if (startsWith(normalized, prefix)) sub(prefix, "", normalized, fixed = TRUE) else normalized
}

default_opts <- list(
  base_config = file.path(project_root, "workspace", "uncertainty", "config", "ua_base.yaml"),
  config_dir = file.path(project_root, "workspace", "uncertainty", "config", "generated_site_selection"),
  design_output = file.path(project_root, "workspace", "uncertainty", "config", "ua_site_selection_design_points.csv"),
  runs_csv = file.path(project_root, "workspace", "uncertainty", "config", "ua_site_selection_runs.csv"),
  scenario_prefix = "ua_sitesel",
  replicates = NA_integer_,
  total_rate = 1.5,
  runs_mode = "replace",
  help = FALSE
)

site_levels <- c(
  "seismicLines",
  "oilGas",
  "oilGas;cutblocks;mining",
  "oilGas;cutblocks;mining;seismicLines"
)

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) {
    return(opts)
  }
  for (arg in args) {
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
    } else if (grepl("^--base-config=", arg, ignore.case = TRUE)) {
      opts$base_config <- sub("^--base-config=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--config-dir=", arg, ignore.case = TRUE)) {
      opts$config_dir <- sub("^--config-dir=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--design-output=", arg, ignore.case = TRUE)) {
      opts$design_output <- sub("^--design-output=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--runs-csv=", arg, ignore.case = TRUE)) {
      opts$runs_csv <- sub("^--runs-csv=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--scenario-prefix=", arg, ignore.case = TRUE)) {
      opts$scenario_prefix <- sub("^--scenario-prefix=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--replicates=", arg, ignore.case = TRUE)) {
      opts$replicates <- as.integer(sub("^--replicates=", "", arg, ignore.case = TRUE))
    } else if (grepl("^--total-rate=", arg, ignore.case = TRUE)) {
      rate_val <- sub("^--total-rate=", "", arg, ignore.case = TRUE)
      opts$total_rate <- suppressWarnings(as.numeric(rate_val))
    } else if (grepl("^--runs-mode=", arg, ignore.case = TRUE)) {
      opts$runs_mode <- tolower(sub("^--runs-mode=", "", arg, ignore.case = TRUE))
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/uncertainty/build_site_selection_design.R [options]\n",
    "  --base-config=PATH      Base config to clone (default workspace/uncertainty/config/ua_base.yaml)\n",
    "  --config-dir=DIR        Output directory for generated configs\n",
    "  --design-output=PATH    Where to write the design table\n",
    "  --runs-csv=PATH         Where to write the runs index\n",
    "  --scenario-prefix=STR   Prefix for run names (default ua_sitesel)\n",
    "  --replicates=N          Replicates per run (default = base config n_reps)\n",
    "  --total-rate=NUM        Fixed totalDisturbanceRate to inject (default 1.5)\n",
    "  --runs-mode=replace|append  Replace (default) or append to the runs index\n",
    "  --help                  Show this message\n"
  ))
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

  if (!file.exists(opts$base_config)) {
    stop("Base config not found: ", opts$base_config, call. = FALSE)
  }

  base_cfg <- yaml::read_yaml(opts$base_config)
  if (is.null(base_cfg$params)) base_cfg$params <- list()
  if (is.null(base_cfg$params$anthroDisturbance_Generator)) {
    base_cfg$params$anthroDisturbance_Generator <- list()
  }

  n_reps <- if (is.na(opts$replicates)) base_cfg$n_reps %||% 1L else opts$replicates
  runs_mode <- if (opts$runs_mode %in% c("replace", "append")) opts$runs_mode else "replace"

  run_rows <- vector("list", length(site_levels))
  for (i in seq_along(site_levels)) {
    site_val <- site_levels[[i]]
    run_name <- sprintf("%s_%03d", opts$scenario_prefix, i)
    cfg <- base_cfg
    cfg$run_name <- run_name
    cfg$n_reps <- n_reps
    cfg$description <- sprintf("UA site selection: %s", site_val)
    cfg$params$anthroDisturbance_Generator$siteSelectionAsDistributing <- site_val
    if (!is.na(opts$total_rate)) {
      cfg$params$anthroDisturbance_Generator$totalDisturbanceRate <- opts$total_rate
    }

    cfg_path <- file.path(opts$config_dir, paste0(run_name, ".yaml"))
    dir.create(dirname(cfg_path), recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(cfg, cfg_path)

    run_rows[[i]] <- data.table(
      run_name = run_name,
      scenario_id = run_name,
      cfg = relative_to_root(cfg_path),
      desc = sprintf("UA siteSelection=%s", site_val),
      design_sample = i,
      n_reps = n_reps,
      rng_seed = NA_integer_,
      siteSelectionAsDistributing = site_val,
      totalDisturbanceRate = opts$total_rate,
      clusterDistance = base_cfg$params$anthroDisturbance_Generator$clusterDistance %||% NA_real_,
      useClusterMethod = base_cfg$params$anthroDisturbance_Generator$useClusterMethod %||% NA
    )
  }

  design_dt <- data.table(
    sample_id = seq_along(site_levels),
    run_name = sprintf("%s_%03d", opts$scenario_prefix, seq_along(site_levels)),
    siteSelectionAsDistributing = site_levels,
    totalDisturbanceRate = opts$total_rate,
    clusterDistance = base_cfg$params$anthroDisturbance_Generator$clusterDistance %||% NA_real_,
    useClusterMethod = base_cfg$params$anthroDisturbance_Generator$useClusterMethod %||% NA,
    rng_seed = NA_integer_
  )
  dir.create(dirname(opts$design_output), recursive = TRUE, showWarnings = FALSE)
  fwrite(design_dt, opts$design_output)

  write_runs_csv(opts$runs_csv, rbindlist(run_rows, use.names = TRUE, fill = TRUE), mode = runs_mode)

  message(sprintf("Generated %d configs in %s", length(site_levels), opts$config_dir))
  message(sprintf("Design table: %s", opts$design_output))
  message(sprintf("Runs index updated: %s", opts$runs_csv))
}

tryCatch(main(), error = function(e) {
  message("build_site_selection_design.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
