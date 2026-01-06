#!/usr/bin/env Rscript
# Compute Morris elementary effects (mu, mu*, sigma) from design-level metrics.
#
# Fix (2025-12):
# - Compute elementary effects only for step pairs where exactly one factor
#   actually changes (based on snapped/coded values), skipping "no-op" and
#   "confounded" steps instead of trusting `changed_parameter` from design export.
# - Compute `dx` from the same snapped norm used to detect the change.
# - Infer the active factor set from the configs used in `runs.csv` so analysis
#   remains correct even if the saved design CSV is missing/out-of-date.
# - Emit per-step QC (`outputs/sensitivity/results/morris_step_qc.csv`) and stop
#   with clear errors if any EE violates the invariants (n_changed != 1, dx_used
#   missing/0, or duplicated `useClusterMethod_num` factor leakage).

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(yaml)
})

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a
as_logical_flag <- function(x) {
  if (is.null(x)) return(NA)
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(tolower(x) %in% c("true", "t", "yes", "y", "1"))
  as.logical(x)
}

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

design_candidates <- c(
  file.path(project_root, "workspace", "sensitivity", "morris_design_points.csv"),
  file.path(project_root, "workspace", "sensitivity", "config", "morris_design_points.csv")
)
runs_path <- file.path(project_root, "outputs", "traceability", "suite_runs", "sensitivity_runs.csv")
config_dir <- file.path(project_root, "workspace", "sensitivity", "config", "generated")
results_dir <- file.path(project_root, "outputs", "sensitivity", "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
metrics_rds <- file.path(results_dir, "morris_design_metrics_long.rds")
metrics_csv <- file.path(results_dir, "morris_design_metrics_long.csv")
parameters_file <- file.path(project_root, "workspace", "sensitivity", "config", "morris_parameters.yaml")
fig_dir <- file.path(project_root, "outputs", "sensitivity", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

site_levels <- c(
  "",
  "oilGas",
  "oilGas;cutblocks",
  "oilGas;cutblocks;mining",
  "oilGas;cutblocks;mining;seismicLines"
)

param_bounds <- list(
  totalDisturbanceRate = c(min = 0.5, max = 3.0),
  clusterDistance = c(min = 500, max = 2000)
)

log_info <- function(...) message(sprintf(...))

read_design_csv <- function() {
  path <- design_candidates[file.exists(design_candidates)][1]
  if (is.na(path)) return(tibble())
  suppressMessages(readr::read_csv(path, show_col_types = FALSE))
}

parse_run_name <- function(run_name) {
  tibble(
    trajectory_id = suppressWarnings(as.integer(str_match(run_name, "t(\\d+)")[, 2])),
    point_index = suppressWarnings(as.integer(str_match(run_name, "p(\\d+)")[, 2]))
  )
}

design_from_config <- function(cfg_path) {
  if (!file.exists(cfg_path)) return(NULL)
  cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
  if (is.null(cfg)) return(NULL)
  run_name <- cfg$run_name %||% cfg$scenario_id %||% basename(cfg_path)
  params <- cfg$params$anthroDisturbance_Generator %||% list()
  parsed <- parse_run_name(run_name)
  tibble(
    scenario_id = run_name,
    trajectory_id = parsed$trajectory_id,
    point_index = parsed$point_index,
    description = cfg$description %||% NA_character_,
    totalDisturbanceRate = params$totalDisturbanceRate %||% NA_real_,
    clusterDistance = params$clusterDistance %||% NA_real_,
    useClusterMethod = params$useClusterMethod %||% NA,
    siteSelectionAsDistributing = params$siteSelectionAsDistributing %||% NA_character_,
    config_file = cfg_path
  )
}

load_runs_registry <- function() {
  if (!file.exists(runs_path)) return(tibble())
  suppressMessages(readr::read_csv(runs_path, show_col_types = FALSE)) %>%
    filter(suite == "sensitivity")
}

load_design_metadata <- function() {
  design_csv <- read_design_csv()
  runs_df <- load_runs_registry()
  cfg_paths <- unique(runs_df$config_file)
  cfg_paths <- cfg_paths[file.exists(cfg_paths)]
  design_cfg <- map_dfr(cfg_paths, design_from_config)

  if (nrow(design_csv)) {
    design_df <- design_csv %>%
      mutate(
        scenario_id = dplyr::coalesce(
          if ("scenario_id" %in% names(design_csv)) scenario_id else NA_character_,
          if ("run_name" %in% names(design_csv)) run_name else NA_character_
        ),
        totalDisturbanceRate = dplyr::coalesce(
          if ("totalDisturbanceRate" %in% names(design_csv)) totalDisturbanceRate else NA_real_,
          if ("value_totalDisturbanceRate" %in% names(design_csv)) value_totalDisturbanceRate else NA_real_
        ),
        clusterDistance = dplyr::coalesce(
          if ("clusterDistance" %in% names(design_csv)) clusterDistance else NA_real_,
          if ("value_cluster_distance" %in% names(design_csv)) value_cluster_distance else NA_real_
        ),
        useClusterMethod = dplyr::coalesce(
          if ("useClusterMethod" %in% names(design_csv)) useClusterMethod else NA,
          if ("value_use_cluster_method" %in% names(design_csv)) value_use_cluster_method else NA
        ),
        siteSelectionAsDistributing = dplyr::coalesce(
          if ("siteSelectionAsDistributing" %in% names(design_csv)) siteSelectionAsDistributing else NA_character_,
          if ("value_site_selection" %in% names(design_csv)) value_site_selection else NA_character_
        ),
        trajectory_id = if ("trajectory_id" %in% names(design_csv)) trajectory_id else NA_integer_,
        point_index = if ("point_index" %in% names(design_csv)) point_index else NA_integer_,
        norm_totalDisturbanceRate = if ("norm_totalDisturbanceRate" %in% names(design_csv)) norm_totalDisturbanceRate else NA_real_,
        norm_clusterDistance = if ("norm_cluster_distance" %in% names(design_csv)) norm_cluster_distance else {
          if ("norm_clusterDistance" %in% names(design_csv)) norm_clusterDistance else NA_real_
        },
        norm_useClusterMethod = if ("norm_use_cluster_method" %in% names(design_csv)) norm_use_cluster_method else NA_real_,
        norm_siteSelection = if ("norm_site_selection" %in% names(design_csv)) norm_site_selection else NA_real_
      )
  } else {
    design_df <- tibble()
  }

  if (nrow(design_cfg)) {
    design_cfg <- design_cfg %>%
      mutate(
        norm_totalDisturbanceRate = (totalDisturbanceRate - param_bounds$totalDisturbanceRate[["min"]]) /
          diff(param_bounds$totalDisturbanceRate),
        norm_clusterDistance = (clusterDistance - param_bounds$clusterDistance[["min"]]) /
          diff(param_bounds$clusterDistance),
        norm_useClusterMethod = ifelse(is.na(useClusterMethod), NA_real_, as.numeric(as_logical_flag(useClusterMethod))),
        norm_siteSelection = {
          idx <- match(siteSelectionAsDistributing, site_levels) - 1
          ifelse(is.na(idx), NA_real_, idx / (length(site_levels) - 1))
        }
      )
  }

  merged <- bind_rows(
    design_df,
    design_cfg %>% filter(!scenario_id %in% design_df$scenario_id)
  )

  has_design_id <- "design_id" %in% names(merged)
  merged <- merged %>%
    mutate(
      design_id = if (has_design_id) design_id else row_number(),
      useClusterMethod = as_logical_flag(useClusterMethod),
      norm_totalDisturbanceRate = ifelse(
        is.na(norm_totalDisturbanceRate) & !is.na(totalDisturbanceRate),
        (totalDisturbanceRate - param_bounds$totalDisturbanceRate[["min"]]) /
          diff(param_bounds$totalDisturbanceRate),
        norm_totalDisturbanceRate
      ),
      norm_clusterDistance = ifelse(
        is.na(norm_clusterDistance) & !is.na(clusterDistance),
        (clusterDistance - param_bounds$clusterDistance[["min"]]) /
          diff(param_bounds$clusterDistance),
        norm_clusterDistance
      ),
      norm_useClusterMethod = ifelse(
        is.na(norm_useClusterMethod) & !is.na(useClusterMethod),
        as.numeric(as_logical_flag(useClusterMethod)),
        norm_useClusterMethod
      ),
      norm_siteSelection = ifelse(
        is.na(norm_siteSelection) & !is.na(siteSelectionAsDistributing),
        {
          idx <- match(siteSelectionAsDistributing, site_levels) - 1
          ifelse(is.na(idx), NA_real_, idx / (length(site_levels) - 1))
        },
        norm_siteSelection
      )
    ) %>%
    arrange(trajectory_id, point_index, design_id) %>%
    mutate(design_id = row_number())
  merged
}

load_morris_parameter_map <- function(path) {
  if (!file.exists(path)) {
    log_info("Parameter definition file not found (%s); using hard-coded factor definitions.", path)
    return(list(
      totalDisturbanceRate = list(type = "continuous", min = param_bounds$totalDisturbanceRate[["min"]], max = param_bounds$totalDisturbanceRate[["max"]]),
      clusterDistance = list(type = "continuous", min = param_bounds$clusterDistance[["min"]], max = param_bounds$clusterDistance[["max"]]),
      useClusterMethod = list(type = "discrete", values = c(TRUE, FALSE)),
      siteSelectionAsDistributing = list(type = "discrete", values = site_levels)
    ))
  }

  cfg <- yaml::read_yaml(path)
  params <- cfg$parameters %||% list()
  if (!length(params)) stop("No parameters found in: ", path, call. = FALSE)

  out <- list()
  for (p in params) {
    type <- tolower(p$type %||% "")
    if (type %in% c("", "categorical")) next

    factor_id <- p$column %||% p$name
    if (is.null(factor_id) || !nzchar(factor_id)) next

    if (type == "continuous") {
      out[[factor_id]] <- list(type = type, min = as.numeric(p$min), max = as.numeric(p$max))
    } else if (type == "discrete") {
      vals <- p$values
      if (is.null(vals)) next
      vals <- unlist(vals, recursive = FALSE, use.names = FALSE)
      out[[factor_id]] <- list(type = type, values = vals)
    } else if (type == "integer") {
      out[[factor_id]] <- list(
        type = type,
        min = as.integer(p$min),
        max = as.integer(p$max),
        levels = as.integer(p$levels %||% 6L)
      )
    }
  }

  out
}

load_params_from_configs <- function(param_cols) {
  runs_df <- load_runs_registry()
  if (!nrow(runs_df)) return(tibble())

  cfg_paths <- unique(runs_df$config_file)
  cfg_paths <- cfg_paths[file.exists(cfg_paths)]
  if (!length(cfg_paths)) return(tibble())

  purrr::map_dfr(cfg_paths, function(cfg_path) {
    cfg <- tryCatch(yaml::read_yaml(cfg_path), error = function(e) NULL)
    if (is.null(cfg)) return(NULL)
    run_name <- cfg$run_name %||% cfg$scenario_id %||% basename(cfg_path)
    params <- cfg$params$anthroDisturbance_Generator %||% list()

    row <- list(scenario_id = run_name)
    for (col in param_cols) {
      row[[col]] <- params[[col]] %||% NA
    }
    tibble::as_tibble(row)
  })
}

snapped_norm_for_value <- function(value, param) {
  if (is.null(value)) return(NA_real_)
  if (length(value) != 1) value <- value[[1]]
  if (is.atomic(value) && isTRUE(is.na(value))) return(NA_real_)

  type <- param$type %||% NA_character_
  if (identical(type, "continuous")) {
    rng <- param$max - param$min
    if (!is.finite(rng) || isTRUE(all.equal(rng, 0))) return(NA_real_)
    out <- (as.numeric(value) - param$min) / rng
    return(min(max(out, 0), 1))
  }

  if (identical(type, "discrete")) {
    vals <- param$values
    if (is.null(vals) || length(vals) < 2) return(NA_real_)

    if (is.logical(vals)) {
      v <- as_logical_flag(value)
    } else if (is.numeric(vals)) {
      v <- suppressWarnings(as.numeric(value))
    } else {
      v <- as.character(value)
      vals <- as.character(vals)
    }

    idx <- match(v, vals)
    if (is.na(idx)) return(NA_real_)
    return((idx - 1) / (length(vals) - 1))
  }

  if (identical(type, "integer")) {
    levels <- as.integer(param$levels %||% 6L)
    if (levels < 2) return(NA_real_)
    v <- suppressWarnings(as.integer(value))
    if (is.na(v)) return(NA_real_)
    grid_vals <- round(param$min + (0:(levels - 1)) * ((param$max - param$min) / (levels - 1)))
    idx <- which.min(abs(grid_vals - v))
    return((idx - 1) / (levels - 1))
  }

  NA_real_
}

add_snapped_norms <- function(design_points, param_map) {
  factors <- names(param_map)
  for (f in factors) {
    param <- param_map[[f]]
    norm_col <- paste0("snorm_", f)
    if (!f %in% names(design_points)) {
      design_points[[norm_col]] <- NA_real_
      next
    }
    design_points[[norm_col]] <- vapply(design_points[[f]], snapped_norm_for_value, numeric(1), param = param)
  }
  design_points
}

build_step_pairs <- function(design_points) {
  design_points %>%
    distinct(design_id, trajectory_id, step_index, .keep_all = TRUE) %>%
    filter(!is.na(trajectory_id), !is.na(step_index), !is.na(design_id)) %>%
    arrange(trajectory_id, step_index, design_id) %>%
    group_by(trajectory_id) %>%
    mutate(
      from_design_id = design_id,
      to_design_id = lead(design_id),
      from_step = step_index,
      to_step = lead(step_index)
    ) %>%
    filter(!is.na(to_design_id)) %>%
    ungroup() %>%
    select(trajectory_id, from_step, to_step, from_design_id, to_design_id)
}

compute_step_qc <- function(step_pairs, design_points, factors, tiny = 1e-9) {
  norm_cols <- paste0("snorm_", factors)
  design_norm <- design_points %>% select(design_id, all_of(norm_cols))

  pairs <- step_pairs %>%
    left_join(design_norm, by = c("from_design_id" = "design_id")) %>%
    rename_with(~ paste0("from_", .x), all_of(norm_cols)) %>%
    left_join(design_norm, by = c("to_design_id" = "design_id")) %>%
    rename_with(~ paste0("to_", .x), all_of(norm_cols))

  from_mat <- as.matrix(pairs[paste0("from_", norm_cols)])
  to_mat <- as.matrix(pairs[paste0("to_", norm_cols)])
  colnames(from_mat) <- factors
  colnames(to_mat) <- factors

  na_from <- is.na(from_mat)
  na_to <- is.na(to_mat)
  one_na <- xor(na_from, na_to)
  both_na <- na_from & na_to
  missing_dx <- apply(one_na, 1, any)
  delta_mat <- to_mat - from_mat
  delta_mat[both_na] <- 0
  colnames(delta_mat) <- factors

  changed_list <- lapply(seq_len(nrow(delta_mat)), function(i) {
    row <- delta_mat[i, ]
    changed <- rep(FALSE, length(factors))
    changed[which(abs(row) > tiny & !is.na(row))] <- TRUE
    changed[which(one_na[i, ])] <- TRUE
    factors[changed]
  })

  n_changed <- lengths(changed_list)
  changed_factor_single <- vapply(changed_list, function(x) if (length(x) == 1) x[[1]] else NA_character_, character(1))
  changed_factors_str <- vapply(changed_list, function(x) paste(x, collapse = ","), character(1))
  dx_used <- vapply(seq_len(nrow(delta_mat)), function(i) {
    if (n_changed[[i]] == 0) return(0)
    if (n_changed[[i]] > 1) return(NA_real_)
    factor <- changed_factor_single[[i]]
    out <- unname(delta_mat[i, factor])
    if (is.na(out) || abs(out) < 1e-12) return(NA_real_)
    out
  }, numeric(1))

  reason_skipped <- ifelse(n_changed == 0, "no-op", ifelse(n_changed > 1, "confounded", ""))
  reason_skipped <- ifelse(reason_skipped == "" & (missing_dx | is.na(dx_used)), "missing_data", reason_skipped)

  tibble(
    trajectory_id = pairs$trajectory_id,
    from_step = pairs$from_step,
    to_step = pairs$to_step,
    from_design_id = pairs$from_design_id,
    to_design_id = pairs$to_design_id,
    changed_factors = changed_factors_str,
    n_changed = as.integer(n_changed),
    reason_skipped = reason_skipped,
    dx_used = dx_used,
    changed_param = changed_factor_single
  )
}

compute_elementary_effects <- function(metrics_df, step_qc, response_col) {
  keys <- c("trajectory_id", "from_step", "to_step", "from_design_id", "to_design_id")

  candidate <- step_qc %>% filter(reason_skipped == "")
  if (!nrow(candidate)) {
    out_qc <- step_qc
    out_qc$n_ee_rows <- 0L
    return(list(ee_long = tibble(), step_qc = out_qc))
  }

  metrics_vals <- metrics_df %>%
    select(design_id, metric_id, year, value = all_of(response_col))

  ee_long <- candidate %>%
    select(all_of(keys), changed_param, n_changed, dx_used) %>%
    inner_join(metrics_vals, by = c("from_design_id" = "design_id")) %>%
    rename(value_from = value) %>%
    inner_join(metrics_vals, by = c("to_design_id" = "design_id", "metric_id", "year")) %>%
    rename(value_to = value) %>%
    filter(!is.na(value_from), !is.na(value_to)) %>%
    mutate(ee = (value_to - value_from) / dx_used) %>%
    select(changed_param, metric_id, year, all_of(keys), n_changed, dx_used, ee)

  ee_counts <- ee_long %>%
    group_by(across(all_of(keys))) %>%
    summarise(n_ee_rows = sum(!is.na(ee)), .groups = "drop")

  step_qc2 <- step_qc %>%
    left_join(ee_counts, by = keys) %>%
    mutate(
      n_ee_rows = dplyr::coalesce(n_ee_rows, 0L),
      reason_skipped = ifelse(reason_skipped == "" & n_ee_rows == 0, "missing_data", reason_skipped)
    )

  kept_keys <- step_qc2 %>%
    filter(reason_skipped == "") %>%
    select(all_of(keys))

  ee_long2 <- ee_long %>% inner_join(kept_keys, by = keys)

  list(ee_long = ee_long2, step_qc = step_qc2)
}

compute_effects_summary <- function(ee_long) {
  if (!nrow(ee_long)) return(tibble())
  ee_long %>%
    group_by(changed_param, metric_id, year) %>%
    summarise(
      mu = mean(ee, na.rm = TRUE),
      mu_star = mean(abs(ee), na.rm = TRUE),
      sigma = sd(ee, na.rm = TRUE),
      n_ee = sum(!is.na(ee)),
      .groups = "drop"
    ) %>%
    mutate(rank_mu_star = rank(-mu_star, ties.method = "average"))
}

write_plots <- function(effects) {
  if (!nrow(effects)) return(invisible(NULL))
  key_metrics <- effects %>%
    distinct(metric_id, year) %>%
    head(4)
  purrr::pwalk(key_metrics, function(metric_id, year) {
    df <- effects %>% filter(metric_id == !!metric_id, year == !!year)
    if (!nrow(df)) return(invisible(NULL))
    p <- ggplot(df, aes(x = mu_star, y = sigma, color = changed_param, label = changed_param)) +
      geom_point(size = 3, alpha = 0.8) +
      ggrepel::geom_text_repel(size = 3, max.overlaps = 10, show.legend = FALSE) +
      labs(
        title = paste0("Morris effects: ", metric_id, " (", year, ")"),
        x = expression(mu["*"]),
        y = expression(sigma),
        color = "Parameter"
      ) +
      theme_minimal()
    out <- file.path(fig_dir, sprintf("morris_%s_%s.png", metric_id, year))
    ggsave(out, plot = p, width = 7, height = 5, dpi = 150)
  })
}

main <- function() {
  metrics_df <- if (file.exists(metrics_rds)) {
    readRDS(metrics_rds)
  } else if (file.exists(metrics_csv)) {
    suppressMessages(readr::read_csv(metrics_csv, show_col_types = FALSE))
  } else {
    stop("Unable to locate design-level metrics (expected RDS or CSV).", call. = FALSE)
  }
  if (!nrow(metrics_df)) stop("Design-level metrics table is empty.", call. = FALSE)

  param_map <- load_morris_parameter_map(parameters_file)
  param_cols <- names(param_map)

  design_df <- load_design_metadata()
  if (!nrow(design_df)) stop("Design metadata could not be loaded.", call. = FALSE)

  design_df <- design_df %>%
    mutate(step_index = if ("point_index" %in% names(design_df)) point_index else step_index)

  response_col <- if ("value_median" %in% names(metrics_df)) "value_median" else "value_mean"

  cfg_params <- load_params_from_configs(param_cols)
  if (nrow(cfg_params)) {
    existing <- intersect(param_cols, names(design_df))
    cfg_params <- cfg_params %>% rename_with(~ paste0(.x, "_cfg"), all_of(existing))
    design_df <- design_df %>% left_join(cfg_params, by = "scenario_id")
    for (col in existing) {
      cfg_col <- paste0(col, "_cfg")
      if (cfg_col %in% names(design_df)) {
        design_df[[col]] <- dplyr::coalesce(design_df[[col]], design_df[[cfg_col]])
      }
    }
    design_df <- design_df %>% select(-all_of(paste0(existing, "_cfg")))
  }

  factor_candidates <- param_cols[param_cols %in% names(design_df)]
  factors <- factor_candidates[vapply(factor_candidates, function(col) {
    x <- design_df[[col]]
    x <- x[!is.na(x)]
    length(unique(as.character(x))) > 1
  }, logical(1))]
  if (!length(factors)) stop("No Morris factors could be resolved from parameter definitions + design metadata.", call. = FALSE)
  if (anyDuplicated(factors)) stop("Duplicate factor names detected in parameter map.", call. = FALSE)

  design_points <- design_df %>%
    select(design_id, trajectory_id, step_index, scenario_id, all_of(factors)) %>%
    distinct(design_id, .keep_all = TRUE) %>%
    mutate(
      across(any_of("useClusterMethod"), as_logical_flag),
      across(any_of("siteSelectionAsDistributing"), as.character)
    ) %>%
    add_snapped_norms(param_map = param_map)

  step_pairs <- build_step_pairs(design_points)
  step_qc <- compute_step_qc(step_pairs, design_points, factors = factors)

  ee_res <- compute_elementary_effects(metrics_df, step_qc, response_col = response_col)
  ee_long <- ee_res$ee_long
  step_qc_final <- ee_res$step_qc
  effects <- compute_effects_summary(ee_long)

  if (nrow(ee_long)) {
    if (any(is.na(ee_long$n_changed) | ee_long$n_changed != 1L)) {
      stop("Internal error: computed an EE where n_changed != 1.", call. = FALSE)
    }
    if (any(is.na(ee_long$dx_used) | abs(ee_long$dx_used) < 1e-12)) {
      stop("Internal error: computed an EE with missing/zero dx_used.", call. = FALSE)
    }
  }
  if (nrow(effects) && all(c("useClusterMethod", "useClusterMethod_num") %in% effects$changed_param)) {
    stop("Factor canonicalization failed: both useClusterMethod and useClusterMethod_num appear in effects output.", call. = FALSE)
  }

  effects_csv <- file.path(results_dir, "morris_effects_long.csv")
  effects_rds <- file.path(results_dir, "morris_effects_long.rds")
  if (nrow(effects)) {
    write_csv(effects, effects_csv)
    saveRDS(effects, effects_rds)
  }
  if (nrow(ee_long)) {
    write_csv(ee_long, file.path(results_dir, "morris_elementary_effects_long.csv"))
    saveRDS(ee_long, file.path(results_dir, "morris_elementary_effects_long.rds"))
  }
  write_csv(
    step_qc_final %>%
      select(
        trajectory_id, from_step, to_step, from_design_id, to_design_id,
        changed_factors, n_changed, reason_skipped, dx_used
      ),
    file.path(results_dir, "morris_step_qc.csv")
  )
  write_plots(effects)

  ranked <- effects %>%
    group_by(changed_param) %>%
    summarise(median_mu_star = median(mu_star, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(median_mu_star))

  qc_summary <- step_qc_final %>%
    summarise(
      total_pairs = n(),
      kept_pairs = sum(reason_skipped == ""),
      no_op_pairs = sum(reason_skipped == "no-op"),
      confounded_pairs = sum(reason_skipped == "confounded"),
      missing_data_pairs = sum(reason_skipped == "missing_data")
    )
  kept_per_factor <- step_qc_final %>%
    filter(reason_skipped == "") %>%
    count(changed_param, name = "kept_pairs") %>%
    arrange(desc(kept_pairs))

  log_info(
    "Step QC: total=%d kept=%d no-op=%d confounded=%d missing=%d",
    qc_summary$total_pairs, qc_summary$kept_pairs, qc_summary$no_op_pairs,
    qc_summary$confounded_pairs, qc_summary$missing_data_pairs
  )
  if (nrow(kept_per_factor)) {
    log_info(
      "Kept pairs per factor: %s",
      paste(sprintf("%s=%d", kept_per_factor$changed_param, kept_per_factor$kept_pairs), collapse = "; ")
    )
  }

  log_info("Analysed %d metrics (%d EE rows). Top parameters by median mu*: %s",
           effects %>% distinct(metric_id, year) %>% nrow(),
           nrow(ee_long),
           paste(ranked$changed_param, collapse = ", "))
  log_info("Effects saved to %s", effects_csv)
}

tryCatch(main(), error = function(e) {
  message("analyse_morris_effects.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
