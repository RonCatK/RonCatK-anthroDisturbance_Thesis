#!/usr/bin/env Rscript
# Build Morris design points and emit per-run configs for workspace/runner.R
#
# Fix (2025-12): export `norm_*` as the snapped norm returned by `map_norm_to_value()`
# for discrete/boolean/integer factors (so `norm_*` and `value_*` are consistent and
# no-op discrete steps are not mis-labelled as changes downstream).

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
  library(glue)
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
  parameters_file = file.path(project_root, "workspace", "sensitivity", "config", "morris_parameters.yaml"),
  base_config = file.path(project_root, "workspace", "sensitivity", "config", "sa_base.yaml"),
  trajectories = 10L,
  levels = 6L,
  grid_jump = 1L,
  replicates = 3L,
  scenario_prefix = "sa_morris",
  start_index = 1L,
  design_output = file.path(project_root, "workspace", "sensitivity", "config", "morris_design_points.csv"),
  runs_csv = file.path(project_root, "workspace", "sensitivity", "config", "sa_runs.csv"),
  config_dir = file.path(project_root, "workspace", "sensitivity", "config", "generated"),
  help = FALSE
)

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) return(opts)
  for (arg in args) {
    if (arg %in% c("--help", "-h")) opts$help <- TRUE
    else if (grepl("^--parameters=", arg, ignore.case = TRUE)) opts$parameters_file <- sub("^--parameters=", "", arg, ignore.case = TRUE)
    else if (grepl("^--base-config=", arg, ignore.case = TRUE)) opts$base_config <- sub("^--base-config=", "", arg, ignore.case = TRUE)
    else if (grepl("^--trajectories=", arg, ignore.case = TRUE)) opts$trajectories <- as.integer(sub("^--trajectories=", "", arg, ignore.case = TRUE))
    else if (grepl("^--levels=", arg, ignore.case = TRUE)) opts$levels <- as.integer(sub("^--levels=", "", arg, ignore.case = TRUE))
    else if (grepl("^--grid-jump=", arg, ignore.case = TRUE)) opts$grid_jump <- as.integer(sub("^--grid-jump=", "", arg, ignore.case = TRUE))
    else if (grepl("^--replicates=", arg, ignore.case = TRUE)) opts$replicates <- as.integer(sub("^--replicates=", "", arg, ignore.case = TRUE))
    else if (grepl("^--scenario-prefix=", arg, ignore.case = TRUE)) opts$scenario_prefix <- sub("^--scenario-prefix=", "", arg, ignore.case = TRUE)
    else if (grepl("^--start-index=", arg, ignore.case = TRUE)) opts$start_index <- as.integer(sub("^--start-index=", "", arg, ignore.case = TRUE))
    else if (grepl("^--design-output=", arg, ignore.case = TRUE)) opts$design_output <- sub("^--design-output=", "", arg, ignore.case = TRUE)
    else if (grepl("^--runs-csv=", arg, ignore.case = TRUE)) opts$runs_csv <- sub("^--runs-csv=", "", arg, ignore.case = TRUE)
    else if (grepl("^--config-dir=", arg, ignore.case = TRUE)) opts$config_dir <- sub("^--config-dir=", "", arg, ignore.case = TRUE)
    else warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/sensitivity/build_morris_design.R [options]\n",
    "  --parameters=PATH      YAML file describing Morris factors (default workspace/sensitivity/config/morris_parameters.yaml)\n",
    "  --base-config=PATH     Base runner config to clone (default workspace/sensitivity/config/sa_base.yaml)\n",
    "  --trajectories=N       Number of random Morris trajectories (default 10)\n",
    "  --levels=N             Grid levels per factor (default 6)\n",
    "  --grid-jump=N          Morris grid jump (default 1)\n",
    "  --replicates=N         Replicates per design point (sets n_reps; default 3)\n",
    "  --scenario-prefix=NAME Prefix for generated run_names (default sa_morris)\n",
    "  --start-index=N        Starting trajectory index (default 1)\n",
    "  --design-output=PATH   Where to write design metadata CSV\n",
    "  --runs-csv=PATH        Where to write run index CSV (config path + metadata)\n",
    "  --config-dir=PATH      Directory to write generated configs into\n",
    "  --help                 Show this message\n"
  ))
}

read_parameter_definitions <- function(path) {
  if (!file.exists(path)) stop("Parameter definition file not found: ", path, call. = FALSE)
  cfg <- yaml::read_yaml(path)
  params <- cfg$parameters
  if (is.null(params) || !length(params)) stop("Parameter definition file contains no entries.", call. = FALSE)
  params
}

validate_parameter <- function(entry) {
  required <- c("name", "column", "type")
  missing <- required[!required %in% names(entry)]
  if (length(missing)) stop(glue("Parameter entry missing fields: {paste(missing, collapse = ', ')}"), call. = FALSE)
  entry$type <- tolower(entry$type)
  if (!entry$type %in% c("continuous", "discrete", "categorical", "integer")) {
    stop(glue("Unsupported parameter type '{entry$type}' for {entry$name}"), call. = FALSE)
  }
  if (entry$type == "continuous") {
    entry$min <- as.numeric(entry$min)
    entry$max <- as.numeric(entry$max)
    if (!is.finite(entry$min) || !is.finite(entry$max) || entry$min >= entry$max) {
      stop(glue("Invalid bounds for {entry$name}: min={entry$min}, max={entry$max}"), call. = FALSE)
    }
  } else if (entry$type == "discrete") {
    vals <- entry$values
    if (is.null(vals) || !length(vals)) {
      stop(glue("Discrete parameter {entry$name} has no values"), call. = FALSE)
    }
    # Preserve logical/character values as-is; coerce to numeric only when appropriate
    if (is.logical(vals) || is.character(vals)) {
      entry$values <- vals
    } else {
      entry$values <- as.numeric(vals)
    }
  } else if (entry$type == "integer") {
    entry$min <- as.integer(entry$min)
    entry$max <- as.integer(entry$max)
    entry$levels <- as.integer(entry$levels %||% 6L)
    if (entry$min >= entry$max) stop(glue("Integer parameter {entry$name} has min >= max"), call. = FALSE)
  } else if (entry$type == "categorical") {
    entry$categories <- as.character(entry$categories)
  }
  entry
}

prepare_parameters <- function(entries) {
  params <- lapply(entries, validate_parameter)
  params <- Filter(function(p) {
    include <- if ("include_in_morris" %in% names(p)) isTRUE(p$include_in_morris) else TRUE
    include && !identical(p$type, "categorical")
  }, params)
  params
}

map_norm_to_value <- function(param, norm) {
  norm <- min(max(norm, 0), 1)
  if (param$type == "continuous") {
    val <- param$min + norm * (param$max - param$min)
    list(value = val, norm = norm)
  } else if (param$type == "discrete") {
    n_vals <- length(param$values)
    idx <- round(norm * (n_vals - 1))
    idx <- min(max(idx, 0), n_vals - 1)
    list(value = param$values[[idx + 1]], norm = idx / (n_vals - 1))
  } else if (param$type == "integer") {
    levels <- param$levels %||% 6L
    grid <- seq(0, 1, length.out = levels)
    idx <- which.min(abs(grid - norm))
    value <- round(param$min + (idx - 1) * ((param$max - param$min) / (levels - 1)))
    value <- min(max(value, param$min), param$max)
    list(value = value, norm = (idx - 1) / (levels - 1))
  } else {
    idx <- round(norm * (length(param$categories) - 1))
    idx <- min(max(idx, 0), length(param$categories) - 1)
    list(value = param$categories[idx + 1], norm = idx / (length(param$categories) - 1))
  }
}

format_samples <- function(norm_matrix, params) {
  n <- nrow(norm_matrix)
  actual_list <- vector("list", n)
  meta_list <- vector("list", n)
  for (i in seq_len(n)) {
    actual_vals <- list()
    meta_vals <- list()
    for (j in seq_along(params)) {
      param <- params[[j]]
      mapped <- map_norm_to_value(param, norm_matrix[i, j])
      actual_vals[[param$column]] <- mapped$value
      meta_vals[[paste0("value_", param$name)]] <- mapped$value
      meta_vals[[paste0("norm_", param$name)]] <- mapped$norm
    }
    actual_list[[i]] <- actual_vals
    meta_list[[i]] <- meta_vals
  }
  list(actual = actual_list, meta = data.table::rbindlist(meta_list, fill = TRUE))
}

sample_morris_trajectory <- function(p, levels, grid_jump) {
  grid_step <- 1 / (levels - 1)
  delta <- grid_jump * grid_step
  base_choices <- seq(0, 1 - delta, by = grid_step)
  base_point <- vapply(seq_len(p), function(...) sample(base_choices, 1), numeric(1))
  order <- sample(seq_len(p))
  points <- matrix(NA_real_, nrow = p + 1, ncol = p)
  points[1, ] <- base_point
  current <- base_point
  for (i in seq_len(p)) {
    idx <- order[i]
    directions <- c()
    if (current[idx] + delta <= 1 + 1e-9) directions <- c(directions, delta)
    if (current[idx] - delta >= -1e-9) directions <- c(directions, -delta)
    if (!length(directions)) directions <- 0
    step <- sample(directions, 1)
    current[idx] <- current[idx] + step
    points[i + 1, ] <- current
  }
  list(points = points, order = order, delta = delta)
}

generate_design_matrix <- function(params, opts) {
  p <- length(params)
  pts_per_traj <- p + 1
  norm_matrix <- matrix(NA_real_, nrow = opts$trajectories * pts_per_traj, ncol = p)
  info <- vector("list", opts$trajectories)
  row_idx <- 1L
  for (t in seq_len(opts$trajectories)) {
    traj <- sample_morris_trajectory(p, opts$levels, opts$grid_jump)
    norm_matrix[row_idx:(row_idx + pts_per_traj - 1), ] <- traj$points
    info[[t]] <- list(order = traj$order, delta = traj$delta)
    row_idx <- row_idx + pts_per_traj
  }
  colnames(norm_matrix) <- vapply(params, function(p) p$name, character(1))
  list(norm_matrix = norm_matrix, info = info)
}

annotate_design_steps <- function(design_dt, params, opts) {
  norm_cols <- paste0("norm_", vapply(params, function(p) p$name, character(1)))
  setorder(design_dt, trajectory_id, point_index)
  traj_ids <- unique(design_dt$trajectory_id)
  for (t in traj_ids) {
    idxs <- which(design_dt$trajectory_id == t)
    if (length(idxs) < 2) next
    for (k in 2:length(idxs)) {
      cur_idx <- idxs[k]
      prev_idx <- idxs[k - 1]
      cur_vals <- as.numeric(design_dt[cur_idx, ..norm_cols])
      prev_vals <- as.numeric(design_dt[prev_idx, ..norm_cols])
      deltas <- cur_vals - prev_vals
      changed <- which(abs(deltas) > 1e-6)
      if (length(changed) == 1) {
        param_name <- params[[changed]]$name
        design_dt[cur_idx, `:=`(
          changed_parameter = param_name,
          delta_norm = deltas[changed],
          prev_scenario_id = design_dt[prev_idx, scenario_id]
        )]
      }
    }
  }
  design_dt[]
}

sanitize_anthro_params <- function(params) {
  if (is.null(params)) return(params)
  genRaster <- isTRUE(params$generatedDisturbanceAsRaster)
  useCluster <- isTRUE(params$useClusterMethod)

  # Altitude cut only matters when masking mountains/water.
  if (isFALSE(params$maskWaterAndMountainsFromLines)) {
    params$altitudeCut <- NULL
  }

  if (genRaster) {
    # Raster path ignores vector/line-only controls.
    drop <- c("useClusterMethod", "seismicLineGrids", "clusterDistance",
              "distanceNewLinesFactor", "runClusteringInParallel",
              "refinedStructure", "siteSelectionAsDistributing",
              "probabilityDisturbance", "maskWaterAndMountainsFromLines",
              "altitudeCut", "useRoadsPackage")
    params[drop] <- NULL
    return(params)
  }

  # Vector path: swap which knobs apply based on clustering strategy.
  if (useCluster) {
    params$seismicLineGrids <- NULL
  } else {
    params$distanceNewLinesFactor <- NULL
    params$refinedStructure <- NULL
  }
  params
}

merge_config_params <- function(base_cfg, values) {
  cfg <- base_cfg
  if (is.null(cfg$params)) cfg$params <- list()
  if (is.null(cfg$params$anthroDisturbance_Generator)) cfg$params$anthroDisturbance_Generator <- list()
  for (nm in names(values)) {
    cfg$params$anthroDisturbance_Generator[[nm]] <- values[[nm]]
  }
  cfg$params$anthroDisturbance_Generator <- sanitize_anthro_params(cfg$params$anthroDisturbance_Generator)
  cfg
}

write_config <- function(cfg, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(cfg, path)
}

main <- function() {
  opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    print_usage()
    quit(save = "no", status = 0, runLast = FALSE)
  }

  params <- prepare_parameters(read_parameter_definitions(opts$parameters_file))
  if (!length(params)) stop("No eligible parameters found for Morris design.", call. = FALSE)
  base_cfg <- yaml::read_yaml(opts$base_config)
  if (is.null(base_cfg$paths$input_root)) stop("Base config missing paths/input_root.", call. = FALSE)
  if (!identical(base_cfg$suite, "sensitivity")) base_cfg$suite <- "sensitivity"

  design <- generate_design_matrix(params, opts)
  samples <- format_samples(design$norm_matrix, params)
  p <- length(params)
  pts_per_traj <- p + 1L
  n_pts <- nrow(samples$meta)
  if (n_pts != opts$trajectories * pts_per_traj) stop("Unexpected design shape.", call. = FALSE)

  design_rows <- vector("list", n_pts)
  run_rows <- vector("list", n_pts)
  seed_base_global <- base_cfg$seed_base %||% 12345L

  for (i in seq_len(n_pts)) {
    traj_idx <- opts$start_index - 1L + ceiling(i / pts_per_traj)
    point_idx <- (i - 1L) %% pts_per_traj
    run_name <- sprintf("%s_t%02d_p%02d", opts$scenario_prefix, traj_idx, point_idx)
    cfg <- merge_config_params(base_cfg, samples$actual[[i]])
    cfg$run_name <- run_name
    cfg$n_reps <- opts$replicates
    seed_offset <- (i - 1L) * cfg$n_reps
    cfg$seed_base <- seed_base_global + seed_offset
    cfg$seeds <- as.list(seed_base_global + seed_offset + seq_len(cfg$n_reps) - 1L)
    cfg$suite <- "sensitivity"
    cfg$description <- paste0("Morris trajectory ", traj_idx, " point ", point_idx)
    cfg$paths$output_root <- cfg$paths$output_root %||% file.path(project_root, "outputs")
    cfg$paths$scratch_root <- cfg$paths$scratch_root %||% file.path(project_root, "scratch")
    cfg$paths$module_path <- cfg$paths$module_path %||% file.path(project_root, "modules")

    cfg_path <- file.path(opts$config_dir, paste0(run_name, ".yaml"))
    write_config(cfg, cfg_path)

    meta_row <- as.list(samples$meta[i])
    meta_row$scenario_id <- run_name
    meta_row$trajectory_id <- traj_idx
    meta_row$point_index <- point_idx
    meta_row$grid_levels <- opts$levels
    meta_row$grid_jump <- opts$grid_jump
    meta_row$replicates <- opts$replicates
    meta_row$config_file <- relative_to_root(cfg_path)
    design_rows[[i]] <- meta_row

    run_rows[[i]] <- list(
      run_name = run_name,
      trajectory_id = traj_idx,
      point_index = point_idx,
      config_file = relative_to_root(cfg_path)
    )
  }

  design_dt <- data.table::rbindlist(design_rows, fill = TRUE)
  design_dt <- annotate_design_steps(design_dt, params, opts)
  runs_dt <- data.table::rbindlist(run_rows, fill = TRUE)

  dir.create(dirname(opts$design_output), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$runs_csv), recursive = TRUE, showWarnings = FALSE)
  fwrite(design_dt, file = opts$design_output)
  fwrite(runs_dt, file = opts$runs_csv)

  message(glue("Generated {n_pts} design points across {opts$trajectories} trajectories."))
  message(glue("Configs written to {opts$config_dir}"))
  message(glue("Run index: {opts$runs_csv}"))
  message(glue("Design metadata: {opts$design_output}"))
}

main()
