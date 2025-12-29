#!/usr/bin/env Rscript
# Collate Morris run outputs, compute per-run disturbance metrics, and
# summarise across replicates for each design point.
#
# Note (2025-12): `polygon_patch_density_per_100km2` is computed per 100 km² of
# disturbed polygon area (not total study area). The metric_id is renamed to
# `polygon_patch_density_per_100km2_disturbed` to avoid misinterpretation.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(sf)
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

default_opts <- list(
  runs_path = file.path(project_root, "workspace", "sensitivity", "runs.csv"),
  results_dir = file.path(project_root, "outputs", "sensitivity", "results"),
  help = FALSE
)

normalize_path <- function(path) {
  if (is.null(path) || isTRUE(is.na(path)) || !nzchar(path)) return(NA_character_)
  if (file.exists(path) || dir.exists(path)) return(path)
  alt <- file.path(project_root, path)
  if (file.exists(alt) || dir.exists(alt)) return(alt)
  path
}

parse_cli_args <- function(args) {
  opts <- default_opts
  if (!length(args)) return(opts)

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
      i <- i + 1
      next
    }

    if (grepl("^--runs=", arg, ignore.case = TRUE)) {
      opts$runs_path <- sub("^--runs=", "", arg, ignore.case = TRUE)
      i <- i + 1
      next
    }
    if (arg %in% c("--runs")) {
      opts$runs_path <- args[[i + 1]]
      i <- i + 2
      next
    }

    if (grepl("^--results-dir=", arg, ignore.case = TRUE) || grepl("^--results_dir=", arg, ignore.case = TRUE)) {
      opts$results_dir <- sub("^--results[-_]dir=", "", arg, ignore.case = TRUE)
      i <- i + 1
      next
    }
    if (arg %in% c("--results-dir", "--results_dir")) {
      opts$results_dir <- args[[i + 1]]
      i <- i + 2
      next
    }

    warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    i <- i + 1
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/sensitivity/collect_morris_metrics.R [options]\n",
    "  --runs=PATH         runs.csv path (default workspace/sensitivity/runs.csv)\n",
    "  --results-dir=PATH  output directory for results (default outputs/sensitivity/results)\n",
    "  --help              Show this message\n"
  ))
}

opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
if (opts$help) {
  print_usage()
  quit(save = "no", status = 0, runLast = FALSE)
}

design_candidates <- c(
  file.path(project_root, "workspace", "sensitivity", "morris_design_points.csv"),
  file.path(project_root, "workspace", "sensitivity", "config", "morris_design_points.csv")
)
runs_path <- normalize_path(opts$runs_path)
config_dir <- file.path(project_root, "workspace", "sensitivity", "config", "generated")
results_dir <- normalize_path(opts$results_dir)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

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
  log_info("Reading Morris design metadata from %s", path)
  out <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  out
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

load_design_metadata <- function(runs_df) {
  design_csv <- read_design_csv()
  if (nrow(design_csv)) {
    log_info("Design columns: %s", paste(names(design_csv), collapse = ", "))
    log_info("Design preview:\n%s", paste(capture.output(print(head(design_csv))), collapse = "\n"))
  } else {
    log_info("No design CSV found; will rebuild from generated configs.")
  }

  cfg_rows <- unique(runs_df$config_file)
  cfg_rows <- cfg_rows[file.exists(cfg_rows)]
  design_cfg <- map_dfr(cfg_rows, design_from_config)

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
      useClusterMethod = as_logical_flag(useClusterMethod),
      design_id = if (has_design_id) design_id else row_number(),
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
    )

  merged <- merged %>%
    arrange(trajectory_id, point_index, design_id) %>%
    mutate(design_id = row_number())

  merged
}

read_runs_registry <- function() {
  if (!file.exists(runs_path)) {
    stop("Run registry not found: ", runs_path, call. = FALSE)
  }
  suppressMessages(readr::read_csv(runs_path, show_col_types = FALSE)) %>%
    filter(suite == "sensitivity")
}

is_diff <- function(a, b, tol = 1e-9) {
  if (all(is.na(c(a, b)))) return(FALSE)
  if (is.numeric(a) && is.numeric(b)) {
    !isTRUE(all.equal(a, b, tolerance = tol))
  } else {
    !identical(as.character(a), as.character(b))
  }
}

infer_changed_parameter <- function(df, param_cols) {
  df <- df %>% arrange(trajectory_id, point_index)
  out <- vector("character", nrow(df))
  for (i in seq_len(nrow(df))) {
    if (i == 1) {
      out[[i]] <- NA_character_
      next
    }
    prev <- df[i - 1, param_cols, drop = FALSE]
    cur <- df[i, param_cols, drop = FALSE]
    diffs <- map_lgl(param_cols, ~ is_diff(cur[[.x]], prev[[.x]]))
    if (sum(diffs, na.rm = TRUE) == 1) {
      out[[i]] <- param_cols[which(diffs)[1]]
    } else {
      out[[i]] <- NA_character_
    }
  }
  out
}

is_seismic_sector <- function(sector) {
  is.character(sector) && grepl("seismicLines", sector, fixed = TRUE)
}

estimate_line_length_from_buffered_polygons <- function(geom, buffer_width_m = 3) {
  # Approximate centerline length from buffered line polygons (buffer width in meters).
  areas_m2 <- as.numeric(sf::st_area(geom))
  length_m <- (areas_m2 - pi * buffer_width_m^2) / (2 * buffer_width_m)
  ifelse(is.na(length_m) | length_m < 0, NA_real_, length_m)
}

read_sector_stats <- function(output_dir) {
  shp_files <- list.files(output_dir, pattern = "^disturbances_.*\\.shp$", full.names = TRUE)
  if (!length(shp_files)) return(tibble())
  sf::sf_use_s2(FALSE)
  map_dfr(shp_files, function(shp) {
    bn <- basename(shp)
    year <- suppressWarnings(as.integer(str_match(bn, "_(\\d{4})_")[, 2]))
    sector <- sub("^disturbances_", "", bn)
    sector <- sub("_[0-9]{4}_.*$", "", sector)
    if (is.na(year) || !nzchar(sector)) return(NULL)
    geom <- tryCatch(sf::read_sf(shp, quiet = TRUE), error = function(e) NULL)
    if (is.null(geom) || !nrow(geom)) {
      return(tibble(sector = sector, year = year, geom_kind = NA_character_))
    }
    gtype <- unique(as.character(sf::st_geometry_type(geom)))
    kind <- if (any(grepl("POLYGON", gtype))) "polygons" else if (any(grepl("LINE", gtype))) "lines" else "other"
    is_seismic <- is_seismic_sector(sector)
    if (is_seismic && identical(kind, "polygons")) {
      lengths_m <- estimate_line_length_from_buffered_polygons(geom, buffer_width_m = 3)
      segment_count <- sum(!is.na(lengths_m))
      cumulative_length_km <- sum(lengths_m, na.rm = TRUE) / 1000
      tibble(
        sector = sector,
        year = year,
        geom_kind = "lines",
        cumulative_area_km2 = NA_real_,
        cumulative_length_km = cumulative_length_km,
        patch_count = NA_real_,
        mean_patch_area_km2 = NA_real_,
        median_patch_area_km2 = NA_real_,
        patch_density_per_100km2 = NA_real_,
        segment_count = segment_count,
        mean_segment_length_km = if (segment_count > 0) mean(lengths_m, na.rm = TRUE) / 1000 else NA_real_,
        median_segment_length_km = if (segment_count > 0) median(lengths_m, na.rm = TRUE) / 1000 else NA_real_
      )
    } else if (identical(kind, "polygons")) {
      areas <- as.numeric(sf::st_area(geom)) / 1e6
      patch_count <- sum(!is.na(areas))
      cumulative_area_km2 <- sum(areas, na.rm = TRUE)
      tibble(
        sector = sector,
        year = year,
        geom_kind = kind,
        cumulative_area_km2 = cumulative_area_km2,
        cumulative_length_km = NA_real_,
        patch_count = patch_count,
        mean_patch_area_km2 = if (patch_count > 0) mean(areas, na.rm = TRUE) else NA_real_,
        median_patch_area_km2 = if (patch_count > 0) median(areas, na.rm = TRUE) else NA_real_,
        patch_density_per_100km2 = if (!is.na(cumulative_area_km2) && cumulative_area_km2 > 0 && patch_count > 0) {
          patch_count / (cumulative_area_km2 / 100)
        } else {
          NA_real_
        },
        segment_count = NA_real_,
        mean_segment_length_km = NA_real_,
        median_segment_length_km = NA_real_
      )
    } else if (identical(kind, "lines")) {
      lengths <- as.numeric(sf::st_length(geom)) / 1000
      segment_count <- sum(!is.na(lengths))
      cumulative_length_km <- sum(lengths, na.rm = TRUE)
      tibble(
        sector = sector,
        year = year,
        geom_kind = kind,
        cumulative_area_km2 = NA_real_,
        cumulative_length_km = cumulative_length_km,
        patch_count = NA_real_,
        mean_patch_area_km2 = NA_real_,
        median_patch_area_km2 = NA_real_,
        patch_density_per_100km2 = NA_real_,
        segment_count = segment_count,
        mean_segment_length_km = if (segment_count > 0) mean(lengths, na.rm = TRUE) else NA_real_,
        median_segment_length_km = if (segment_count > 0) median(lengths, na.rm = TRUE) else NA_real_
      )
    } else {
      NULL
    }
  })
}

aggregate_metrics <- function(sector_stats, years_of_interest = NULL) {
  if (!nrow(sector_stats)) return(tibble())
  if (is.null(years_of_interest)) years_of_interest <- sort(unique(sector_stats$year))
  map_dfr(years_of_interest, function(yr) {
    year_dt <- sector_stats %>% filter(year == yr)
    line_dt <- year_dt %>% filter(geom_kind == "lines")
    poly_dt <- year_dt %>% filter(geom_kind == "polygons")

    total_line_length <- sum(line_dt$cumulative_length_km, na.rm = TRUE)
    total_segments <- sum(line_dt$segment_count, na.rm = TRUE)
    total_poly_area <- sum(poly_dt$cumulative_area_km2, na.rm = TRUE)
    total_patches <- sum(poly_dt$patch_count, na.rm = TRUE)

    mean_seg_length <- if (total_segments > 0) total_line_length / total_segments else NA_real_
    mean_patch_area <- if (total_patches > 0) total_poly_area / total_patches else NA_real_
    patch_density <- if (!is.na(total_poly_area) && total_poly_area > 0 && total_patches > 0) {
      total_patches / (total_poly_area / 100)
    } else {
      NA_real_
    }

    tibble(
      year = yr,
      metric_id = c(
        "total_polygon_area_km2",
        "total_linear_length_km",
        "polygon_patch_density_per_100km2_disturbed",
        "polygon_mean_patch_area_km2",
        "polygon_patch_count",
        "line_segment_count",
        "line_mean_segment_length_km"
      ),
      metric_group = c("polygon", "linear", "polygon", "polygon", "polygon", "linear", "linear"),
      value = c(
        total_poly_area,
        total_line_length,
        patch_density,
        mean_patch_area,
        total_patches,
        total_segments,
        mean_seg_length
      )
    )
  })
}

compute_run_metrics <- function(run_row) {
  out_dir <- run_row$output_path
  sector_stats <- read_sector_stats(out_dir)
  years <- sort(unique(sector_stats$year))
  if (!length(years)) return(tibble())
  metrics <- aggregate_metrics(sector_stats, years_of_interest = years)
  if (!nrow(metrics)) return(tibble())
  metrics %>%
    mutate(
      run_name = run_row$run_name,
      design_id = run_row$design_id,
      run_id = run_row$run_id,
      seed = run_row$seed,
      trajectory_id = run_row$trajectory_id,
      step_index = run_row$point_index,
      totalDisturbanceRate = run_row$totalDisturbanceRate,
      clusterDistance = run_row$clusterDistance,
      useClusterMethod = run_row$useClusterMethod,
      siteSelectionAsDistributing = run_row$siteSelectionAsDistributing
    )
}

main <- function() {
  runs_df <- read_runs_registry()
  if (!nrow(runs_df)) stop("Run registry is empty after filtering suite == 'sensitivity'.", call. = FALSE)

  runs_df <- runs_df %>%
    mutate(
      run_id = paste0(run_name, "_rep_", sprintf("%03d", replicate %||% row_number())),
      output_path = file.path(project_root, output_dir),
      log_path = file.path(project_root, log_file)
    )

  design_df <- load_design_metadata(runs_df)
  if (!nrow(design_df)) stop("No design metadata could be assembled from CSV or generated configs.", call. = FALSE)
  param_cols <- c("totalDisturbanceRate", "clusterDistance", "useClusterMethod", "siteSelectionAsDistributing")
  if (!"changed_parameter" %in% names(design_df)) {
    design_df <- design_df %>%
      group_by(trajectory_id) %>%
      mutate(changed_parameter = infer_changed_parameter(cur_data_all(), param_cols)) %>%
      ungroup()
  }

  runs_joined <- runs_df %>%
    left_join(design_df %>% rename(run_name = scenario_id), by = "run_name") %>%
    mutate(output_exists = dir.exists(output_path)) %>%
    filter(status == "success")

  if (any(is.na(runs_joined$design_id))) {
    dropped <- sum(is.na(runs_joined$design_id))
    log_info("Dropping %d runs without design metadata.", dropped)
    runs_joined <- runs_joined %>% filter(!is.na(design_id))
  }

  if (!nrow(runs_joined)) stop("No successful runs found after joining with design metadata.", call. = FALSE)

  log_info("Joined runs: %d rows (%d unique design points)", nrow(runs_joined), n_distinct(runs_joined$design_id))

  run_metrics <- runs_joined %>%
    split(seq_len(nrow(.))) %>%
    map_dfr(~ compute_run_metrics(.x))

  if (!nrow(run_metrics)) stop("No metrics could be computed from available outputs.", call. = FALSE)

  design_metrics <- run_metrics %>%
    group_by(design_id, metric_id, metric_group, year) %>%
    summarise(
      n_runs = n(),
      value_median = median(value, na.rm = TRUE),
      value_mean = mean(value, na.rm = TRUE),
      value_sd = sd(value, na.rm = TRUE),
      value_p05 = quantile(value, 0.05, na.rm = TRUE),
      value_p95 = quantile(value, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(design_df %>% select(design_id, trajectory_id, point_index, all_of(param_cols)), by = "design_id") %>%
    rename(step_index = point_index)

  run_out_csv <- file.path(results_dir, "morris_run_metrics_long.csv")
  design_out_csv <- file.path(results_dir, "morris_design_metrics_long.csv")
  run_out_rds <- file.path(results_dir, "morris_run_metrics_long.rds")
  design_out_rds <- file.path(results_dir, "morris_design_metrics_long.rds")

  write_csv(run_metrics, run_out_csv)
  write_csv(design_metrics, design_out_csv)
  saveRDS(run_metrics, run_out_rds)
  saveRDS(design_metrics, design_out_rds)

  per_design_counts <- run_metrics %>% count(design_id, name = "n_runs")
  log_info(
    "Computed metrics for %d runs across %d design points. Successful runs per design (summary): %s",
    nrow(runs_joined),
    n_distinct(run_metrics$design_id),
    paste(capture.output(summary(per_design_counts$n_runs)), collapse = "; ")
  )
  log_info("Run-level metrics: %s", run_out_csv)
  log_info("Design-level metrics: %s", design_out_csv)
}

tryCatch(main(), error = function(e) {
  message("collect_morris_metrics.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
