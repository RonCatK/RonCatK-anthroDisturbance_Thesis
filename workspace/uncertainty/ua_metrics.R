#!/usr/bin/env Rscript
# UA metrics collector (workspace version)
# Reads outputs/traceability suite runs registry, computes per-replicate disturbance metrics,
# summarises across replicates, and writes run-level outputs under outputs/uncertainty/results.

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
  library(optparse)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

option_list <- list(
  optparse::make_option(
    "--runs-csv",
    type = "character",
    default = file.path(project_root, "outputs", "traceability", "suite_runs", "uncertainty_runs.csv"),
    help = "Run registry CSV (default outputs/traceability/suite_runs/uncertainty_runs.csv)."
  ),
  optparse::make_option(
    "--suite",
    type = "character",
    default = "uncertainty",
    help = "Suite filter (default uncertainty)."
  ),
  optparse::make_option(
    "--run-name",
    type = "character",
    default = NA_character_,
    help = "Comma-separated run_name(s) to process (default: all matching suite)."
  ),
  optparse::make_option(
    "--results-root",
    type = "character",
    default = file.path(project_root, "outputs", "uncertainty", "results"),
    help = "Output folder for per-run metric tables."
  ),
  optparse::make_option(
    "--aggregate-from-year",
    type = "integer",
    default = NA_integer_,
    help = "Optional start year for cumulative/end metrics."
  ),
  optparse::make_option(
    "--aggregate-to-year",
    type = "integer",
    default = NA_integer_,
    help = "Optional end year for cumulative/end metrics."
  ),
  optparse::make_option(
    "--derive-shape-metrics",
    action = "store_true",
    default = FALSE,
    help = "Derive patch_count per km2 using yearly new area."
  ),
  optparse::make_option(
    c("-h", "--help"),
    action = "store_true",
    default = FALSE,
    help = "Show help and exit."
  )
)

parse_args <- function() {
  opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
  if (opts$help) {
    optparse::print_help(optparse::OptionParser(option_list = option_list))
    quit(status = 0, runLast = FALSE)
  }
  opts
}

split_paths <- function(x) {
  if (is.null(x) || all(is.na(x))) return(character(0))
  vals <- trimws(unlist(strsplit(x, "[;]")))
  vals[nzchar(vals)]
}

normalize_to_root <- function(pathValue) {
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) return(NA_character_)
  candidates <- unique(c(pathValue, file.path(project_root, pathValue)))
  for (cand in candidates) {
    expanded <- tryCatch(normalizePath(cand, winslash = "/", mustWork = FALSE), error = function(...) NULL)
    if (!is.null(expanded) && (file.exists(expanded) || dir.exists(expanded))) return(expanded)
  }
  tryCatch(normalizePath(candidates[length(candidates)], winslash = "/", mustWork = FALSE), error = function(...) candidates[length(candidates)])
}

relative_to_root <- function(pathValue) {
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) return(NA_character_)
  normalized <- tryCatch(normalizePath(pathValue, winslash = "/", mustWork = FALSE), error = function(...) pathValue)
  prefix <- paste0(project_root, "/")
  if (startsWith(normalized, prefix)) sub(prefix, "", normalized, fixed = TRUE) else normalized
}

geom_kind <- function(x) {
  if (!inherits(x, "SpatVector")) return(NA_character_)
  sfx <- try(suppressWarnings(sf::st_as_sf(x)), silent = TRUE)
  if (inherits(sfx, "try-error")) return(NA_character_)
  gtypes <- unique(as.character(sf::st_geometry_type(sfx, by_geometry = TRUE)))
  if (length(gtypes) == 0) return(NA_character_)
  if (all(grepl("LINE", gtypes))) return("lines")
  if (any(grepl("POLYGON", gtypes))) return("polygons")
  "other"
}

geom_area_km2 <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (inherits(x, "SpatRaster")) {
    cs <- terra::cellSize(x, unit = "km")
    m <- terra::mask(cs, x)
    vals <- terra::values(m, mat = FALSE)
    return(sum(vals, na.rm = TRUE))
  }
  if (inherits(x, "SpatVector")) {
    kind <- geom_kind(x)
    if (!identical(kind, "polygons")) return(NA_real_)
    a <- try(terra::expanse(x, unit = "km"), silent = TRUE)
    if (inherits(a, "try-error")) return(NA_real_)
    return(sum(a, na.rm = TRUE))
  }
  NA_real_
}

geom_length_km <- function(x) {
  if (!inherits(x, "SpatVector")) return(NA_real_)
  kind <- geom_kind(x)
  if (!identical(kind, "lines")) return(NA_real_)
  sfx <- suppressWarnings(sf::st_as_sf(x))
  sum(as.numeric(sf::st_length(sfx)), na.rm = TRUE) / 1000
}

feature_areas_km2 <- function(x) {
  vals <- try(terra::expanse(x, unit = "km"), silent = TRUE)
  if (inherits(vals, "try-error")) return(numeric())
  as.numeric(vals)
}

feature_lengths_km <- function(x) {
  sfx <- try(suppressWarnings(sf::st_as_sf(x)), silent = TRUE)
  if (inherits(sfx, "try-error")) return(numeric())
  vals <- try(as.numeric(sf::st_length(sfx)), silent = TRUE)
  if (inherits(vals, "try-error")) return(numeric())
  vals / 1000
}

nearest_neighbor_stats <- function(x) {
  out <- list(mean = NA_real_, median = NA_real_)
  sfx <- try(suppressWarnings(sf::st_as_sf(x)), silent = TRUE)
  if (inherits(sfx, "try-error") || nrow(sfx) < 2) return(out)
  cent <- try(suppressWarnings(sf::st_centroid(sf::st_geometry(sfx))), silent = TRUE)
  if (inherits(cent, "try-error")) return(out)
  dist_mat <- try(suppressWarnings(sf::st_distance(cent)), silent = TRUE)
  if (inherits(dist_mat, "try-error")) return(out)
  dist_mat <- suppressWarnings(as.matrix(dist_mat))
  diag(dist_mat) <- NA_real_
  mins <- apply(dist_mat, 1, function(row) {
    row <- row[!is.na(row)]
    if (!length(row)) return(NA_real_)
    min(row[row > 0], na.rm = TRUE)
  })
  mins <- as.numeric(mins) / 1000
  mins <- mins[is.finite(mins)]
  if (!length(mins)) return(out)
  list(mean = mean(mins, na.rm = TRUE), median = stats::median(mins, na.rm = TRUE))
}

first_non_na <- function(vals, default = NA_real_) {
  vals <- vals[!is.na(vals)]
  if (length(vals)) vals[[1]] else default
}

# NOTE: "yearly_new" historically represents interval increments between snapshots,
# not per-year rates. Prefer interval/annualized metrics below; keep yearly_new as
# a compatibility alias for interval increments.
compute_interval_new <- function(vals) {
  if (!length(vals)) return(vals)
  out <- rep(NA_real_, length(vals))
  prev_obs <- NA_real_
  for (i in seq_along(vals)) {
    cur <- vals[i]
    if (is.na(cur)) {
      out[i] <- NA_real_
    } else if (is.na(prev_obs)) {
      out[i] <- cur
      prev_obs <- cur
    } else {
      out[i] <- cur - prev_obs
      prev_obs <- cur
    }
  }
  out
}

compute_yearly_new <- function(vals) {
  compute_interval_new(vals)
}

compute_annualized_new <- function(vals, years) {
  if (!length(vals)) return(vals)
  if (length(vals) != length(years)) {
    stop("Annualized computation requires matching values and years.", call. = FALSE)
  }
  out <- rep(NA_real_, length(vals))
  prev_obs <- NA_real_
  prev_year <- NA_real_
  for (i in seq_along(vals)) {
    cur <- vals[i]
    yr <- years[i]
    if (is.na(cur) || is.na(yr)) {
      out[i] <- NA_real_
    } else if (is.na(prev_obs) || is.na(prev_year)) {
      out[i] <- NA_real_
      prev_obs <- cur
      prev_year <- yr
    } else {
      delta_years <- yr - prev_year
      out[i] <- if (!is.na(delta_years) && delta_years != 0) (cur - prev_obs) / delta_years else NA_real_
      prev_obs <- cur
      prev_year <- yr
    }
  }
  out
}

read_sector_stats <- function(output_dir) {
  shp_files <- list.files(output_dir, pattern = "^disturbances_.*\\.shp$", full.names = TRUE)
  if (!length(shp_files)) return(data.table())
  sf::sf_use_s2(FALSE)
  rows <- lapply(shp_files, function(shp) {
    bn <- basename(shp)
    if (!grepl("^disturbances_", bn)) return(NULL)
    year <- suppressWarnings(as.integer(sub("^disturbances_.*_(\\d{4})_.*$", "\\1", bn)))
    if (is.na(year)) return(NULL)
    sector <- sub("^disturbances_(.+)_\\d{4}_.*$", "\\1", bn)
    geom <- tryCatch(suppressWarnings(terra::vect(shp)), error = function(...) NULL)
    if (is.null(geom)) {
      warning(sprintf("Unable to read %s; skipping.", shp), call. = FALSE)
      return(NULL)
    }
    kind <- geom_kind(geom)
    cumulative_area_km2 <- geom_area_km2(geom)
    cumulative_length_km <- geom_length_km(geom)
    patch_metrics <- list(
      patch_count = NA_real_,
      mean_patch_area_km2 = NA_real_,
      median_patch_area_km2 = NA_real_,
      patch_density_per_100km2 = NA_real_,
      mean_nn_distance_km = NA_real_,
      median_nn_distance_km = NA_real_
    )
    line_metrics <- list(
      segment_count = NA_real_,
      mean_segment_length_km = NA_real_,
      median_segment_length_km = NA_real_
    )
    if (identical(kind, "polygons")) {
      areas <- feature_areas_km2(geom)
      if (length(areas)) {
        patch_metrics$patch_count <- length(areas)
        patch_metrics$mean_patch_area_km2 <- mean(areas, na.rm = TRUE)
        patch_metrics$median_patch_area_km2 <- stats::median(areas, na.rm = TRUE)
      }
      if (!is.na(cumulative_area_km2) && !is.na(patch_metrics$patch_count) &&
          is.finite(cumulative_area_km2) && cumulative_area_km2 > 0) {
        patch_metrics$patch_density_per_100km2 <- patch_metrics$patch_count / (cumulative_area_km2 / 100)
      }
      nn <- nearest_neighbor_stats(geom)
      patch_metrics$mean_nn_distance_km <- nn$mean
      patch_metrics$median_nn_distance_km <- nn$median
    } else if (identical(kind, "lines")) {
      lengths <- feature_lengths_km(geom)
      if (length(lengths)) {
        line_metrics$segment_count <- length(lengths)
        line_metrics$mean_segment_length_km <- mean(lengths, na.rm = TRUE)
        line_metrics$median_segment_length_km <- stats::median(lengths, na.rm = TRUE)
      }
    }
    data.table(
      sector = sector,
      year = year,
      geom_kind = kind,
      cumulative_area_km2 = cumulative_area_km2,
      cumulative_length_km = cumulative_length_km,
      patch_count = patch_metrics$patch_count,
      mean_patch_area_km2 = patch_metrics$mean_patch_area_km2,
      median_patch_area_km2 = patch_metrics$median_patch_area_km2,
      patch_density_per_100km2 = patch_metrics$patch_density_per_100km2,
      mean_nn_distance_km = patch_metrics$mean_nn_distance_km,
      median_nn_distance_km = patch_metrics$median_nn_distance_km,
      segment_count = line_metrics$segment_count,
      mean_segment_length_km = line_metrics$mean_segment_length_km,
      median_segment_length_km = line_metrics$median_segment_length_km
    )
  })
  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(dt)
  dt[, `:=`(
    cumulative_area_km2 = ifelse(is.finite(cumulative_area_km2), cumulative_area_km2, NA_real_),
    cumulative_length_km = ifelse(is.finite(cumulative_length_km), cumulative_length_km, NA_real_)
  )]
  dt <- dt[, .(
    geom_kind = {
      kinds <- unique(na.omit(geom_kind))
      if (length(kinds)) kinds[1] else NA_character_
    },
    cumulative_area_km2 = if (all(is.na(cumulative_area_km2))) NA_real_ else sum(cumulative_area_km2, na.rm = TRUE),
    cumulative_length_km = if (all(is.na(cumulative_length_km))) NA_real_ else sum(cumulative_length_km, na.rm = TRUE),
    patch_count = if (all(is.na(patch_count))) NA_real_ else sum(patch_count, na.rm = TRUE),
    mean_patch_area_km2 = first_non_na(mean_patch_area_km2),
    median_patch_area_km2 = first_non_na(median_patch_area_km2),
    patch_density_per_100km2 = if (!is.na(cumulative_area_km2) && !is.na(patch_count) && cumulative_area_km2 > 0) {
      patch_count / (cumulative_area_km2 / 100)
    } else {
      NA_real_
    },
    mean_nn_distance_km = if (all(is.na(mean_nn_distance_km))) NA_real_ else mean(mean_nn_distance_km, na.rm = TRUE),
    median_nn_distance_km = first_non_na(median_nn_distance_km),
    segment_count = if (all(is.na(segment_count))) NA_real_ else sum(segment_count, na.rm = TRUE),
    mean_segment_length_km = first_non_na(mean_segment_length_km),
    median_segment_length_km = first_non_na(median_segment_length_km)
  ), by = .(sector, year)]
  dt[]
}

metrics_from_output_dir <- function(output_dir, scenario_id, rep_id) {
  sector_dt <- read_sector_stats(output_dir)
  if (!nrow(sector_dt)) return(data.table())
  out <- list()
  poly_dt <- sector_dt[geom_kind == "polygons"]
  if (nrow(poly_dt)) {
    data.table::setorder(poly_dt, sector, year)
    poly_dt[, sector_interval_new_area_km2 := compute_interval_new(cumulative_area_km2), by = sector]
    poly_dt[, sector_yearly_new_area_km2 := sector_interval_new_area_km2]
    poly_dt[, sector_annualized_new_area_km2 := compute_annualized_new(cumulative_area_km2, year), by = sector]
    out[["sector_yearly"]] <- poly_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_yearly_new_area_km2",
      sector = sector,
      value = sector_yearly_new_area_km2
    )]
    out[["sector_interval"]] <- poly_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_interval_new_area_km2",
      sector = sector,
      value = sector_interval_new_area_km2
    )]
    out[["sector_annualized"]] <- poly_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_annualized_new_area_km2",
      sector = sector,
      value = sector_annualized_new_area_km2
    )]
    out[["sector_current_area"]] <- poly_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_current_total_area_km2",
      sector = sector,
      value = cumulative_area_km2
    )]
    totals <- poly_dt[, .(value = sum(sector_yearly_new_area_km2, na.rm = TRUE)), by = year]
    totals_interval <- poly_dt[, .(value = sum(sector_interval_new_area_km2, na.rm = TRUE)), by = year]
    totals_annualized <- poly_dt[, .(
      value = if (all(is.na(sector_annualized_new_area_km2))) {
        NA_real_
      } else {
        sum(sector_annualized_new_area_km2, na.rm = TRUE)
      }
    ), by = year]
    out[["total_yearly"]] <- totals[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "total_yearly_new_area_km2",
      sector = NA_character_,
      value = value
    )]
    out[["total_interval"]] <- totals_interval[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "total_interval_new_area_km2",
      sector = NA_character_,
      value = value
    )]
    out[["total_annualized"]] <- totals_annualized[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "total_annualized_new_area_km2",
      sector = NA_character_,
      value = value
    )]
    if ("patch_count" %in% names(poly_dt)) {
      out[["sector_patch_count"]] <- poly_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_patch_count",
        sector = sector,
        value = patch_count
      )]
    }
    if ("mean_patch_area_km2" %in% names(poly_dt)) {
      out[["sector_mean_patch_area_km2"]] <- poly_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_mean_patch_area_km2",
        sector = sector,
        value = mean_patch_area_km2
      )]
    }
    if ("median_patch_area_km2" %in% names(poly_dt)) {
      out[["sector_median_patch_area_km2"]] <- poly_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_median_patch_area_km2",
        sector = sector,
        value = median_patch_area_km2
      )]
    }
    if ("patch_density_per_100km2" %in% names(poly_dt)) {
      out[["sector_patch_density_per_100km2"]] <- poly_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_patch_density_per_100km2",
        sector = sector,
        value = patch_density_per_100km2
      )]
    }
    if ("mean_nn_distance_km" %in% names(poly_dt)) {
      out[["sector_mean_nn_distance_km"]] <- poly_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_mean_nn_distance_km",
        sector = sector,
        value = mean_nn_distance_km
      )]
    }
    if ("median_nn_distance_km" %in% names(poly_dt)) {
      out[["sector_median_nn_distance_km"]] <- poly_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_median_nn_distance_km",
        sector = sector,
        value = median_nn_distance_km
      )]
    }
  }
  line_dt <- sector_dt[geom_kind == "lines"]
  if (nrow(line_dt)) {
    data.table::setorder(line_dt, sector, year)
    out[["sector_current_length"]] <- line_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_current_total_length_km",
      sector = sector,
      value = cumulative_length_km
    )]
    if ("segment_count" %in% names(line_dt)) {
      out[["sector_segment_count"]] <- line_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_segment_count",
        sector = sector,
        value = segment_count
      )]
    }
    if ("mean_segment_length_km" %in% names(line_dt)) {
      out[["sector_mean_segment_length_km"]] <- line_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_mean_segment_length_km",
        sector = sector,
        value = mean_segment_length_km
      )]
    }
    if ("median_segment_length_km" %in% names(line_dt)) {
      out[["sector_median_segment_length_km"]] <- line_dt[, .(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = year,
        metric = "sector_median_segment_length_km",
        sector = sector,
        value = median_segment_length_km
      )]
    }
  }
  if (!length(out)) return(data.table())
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

write_replicate_index <- function(path, info_dt) {
  if (!nrow(info_dt)) {
    data.table::fwrite(data.table(), file = path)
  } else {
    data.table::fwrite(info_dt, file = path)
  }
}

summarize_metrics <- function(dt) {
  if (!nrow(dt)) return(data.table())
  sumDT <- dt[, {
    valid_vals <- value[!is.na(value)]
    n_obs <- length(valid_vals)
    mean_val <- if (n_obs) mean(valid_vals, na.rm = TRUE) else NA_real_
    sd_val <- if (n_obs > 1) stats::sd(valid_vals, na.rm = TRUE) else NA_real_
    se_val <- if (!is.na(sd_val) && n_obs > 0) sd_val / sqrt(n_obs) else NA_real_
    quantiles <- if (n_obs) stats::quantile(valid_vals, probs = c(0.05, 0.5, 0.95), na.rm = TRUE, type = 7, names = FALSE) else rep(NA_real_, 3)
    list(
      n = n_obs,
      mean = mean_val,
      sd = sd_val,
      se = se_val,
      median = quantiles[2],
      q05 = quantiles[1],
      q95 = quantiles[3],
      lwr = quantiles[1],
      upr = quantiles[3],
      ci_lwr = if (!is.na(se_val)) mean_val - 1.96 * se_val else NA_real_,
      ci_upr = if (!is.na(se_val)) mean_val + 1.96 * se_val else NA_real_
    )
  }, by = .(scenario_id, label, year, metric, sector)]
  sumDT
}

derive_shape_metrics <- function(dt, enable_shape) {
  if (!isTRUE(enable_shape) || !nrow(dt)) return(dt)
  metric_set <- unique(dt$metric)
  area_metric <- if ("sector_interval_new_area_km2" %in% metric_set) {
    "sector_interval_new_area_km2"
  } else if ("sector_yearly_new_area_km2" %in% metric_set) {
    "sector_yearly_new_area_km2"
  } else {
    return(dt)
  }
  needed <- c("sector_patch_count", area_metric)
  if (!all(needed %in% metric_set)) return(dt)
  total_area <- dt[metric == area_metric, .(area = value), by = .(scenario_id, rep_id, year, sector)]
  patch_counts <- dt[metric == "sector_patch_count"]
  merged <- merge(patch_counts, total_area, by = c("scenario_id", "rep_id", "year", "sector"), all.x = TRUE)
  merged[, value := ifelse(!is.na(area) & area > 0, value / area, NA_real_)]
  merged[, metric := "sector_patch_count_per_km2_new"]
  merged[, area := NULL]
  data.table::rbindlist(list(dt, merged), use.names = TRUE, fill = TRUE)
}

aggregate_metrics <- function(dt, from_year, to_year) {
  if (!nrow(dt) || is.na(from_year) || is.na(to_year)) return(dt)
  if (!any(dt$year >= from_year & dt$year <= to_year, na.rm = TRUE)) return(dt)
  sum_patterns <- c("yearly_new", "interval_new", "total_yearly", "total_interval", "current_total", "total_current")
  end_patterns <- c("patch_density", "nn_distance", "segment_length", "patch_area", "patch_count_per_km2_new", "segment_count")
  sum_dt <- dt[
    year >= from_year & year <= to_year & sapply(metric, function(m) any(grepl(sum_patterns, m))),
    .(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE)),
    by = .(scenario_id, rep_id, label, metric, sector)
  ]
  if (nrow(sum_dt)) {
    sum_dt[, `:=`(
      year = to_year,
      metric = paste0(metric, "_cum", from_year, "_", to_year)
    )]
  }
  end_dt <- dt[
    year >= from_year & year <= to_year & sapply(metric, function(m) any(grepl(end_patterns, m))),
  ][
    order(year),
    .(value = tail(value[!is.na(value)], 1)),
    by = .(scenario_id, rep_id, label, metric, sector)
  ]
  if (nrow(end_dt)) {
    end_dt[, `:=`(
      year = to_year,
      metric = paste0(metric, "_end", to_year)
    )]
  }
  data.table::rbindlist(list(dt, sum_dt, end_dt), use.names = TRUE, fill = TRUE)
}

process_run <- function(run_name, run_rows, opts) {
  label <- run_name
  metrics_list <- list()
  info_rows <- list()
  for (i in seq_len(nrow(run_rows))) {
    out_dir <- normalize_to_root(run_rows$output_dir[i])
    if (!dir.exists(out_dir)) {
      warning(sprintf("Output directory missing for run %s replicate %s: %s", run_name, run_rows$replicate[i], out_dir), call. = FALSE)
      next
    }
    rep_id <- run_rows$replicate[i]
    metrics <- metrics_from_output_dir(out_dir, scenario_id = run_name, rep_id = rep_id)
    if (nrow(metrics)) {
      metrics[, label := label]
      metrics_list[[length(metrics_list) + 1L]] <- metrics
    }
    info_rows[[length(info_rows) + 1L]] <- data.table(
      scenario_id = run_name,
      label = label,
      rep_id = rep_id,
      output_path = relative_to_root(out_dir),
      log_path = if (!is.null(run_rows$log_file)) relative_to_root(run_rows$log_file[i]) else NA_character_
    )
  }
  metrics_raw <- if (length(metrics_list)) data.table::rbindlist(metrics_list, use.names = TRUE, fill = TRUE) else data.table()
  metrics_raw <- derive_shape_metrics(metrics_raw, opts$derive_shape_metrics)
  metrics_raw <- aggregate_metrics(metrics_raw, opts$aggregate_from_year, opts$aggregate_to_year)
  list(metrics_raw = metrics_raw, info = if (length(info_rows)) data.table::rbindlist(info_rows, use.names = TRUE, fill = TRUE) else data.table())
}

main <- function() {
  opts <- parse_args()
  runs_csv <- normalize_to_root(opts$runs_csv)
  if (is.na(runs_csv) || !file.exists(runs_csv)) stop("Run registry not found: ", opts$runs_csv, call. = FALSE)
  runs_df <- suppressMessages(data.table::fread(runs_csv, fill = TRUE))
  if (!"run_name" %in% names(runs_df)) stop("runs CSV missing run_name column.", call. = FALSE)
  runs_df[, suite := if ("suite" %in% names(runs_df)) suite else NA_character_]
  runs_df <- runs_df[tolower(suite) == tolower(opts$suite) | is.na(suite)]
  if (!is.na(opts$run_name)) {
    wanted <- trimws(unlist(strsplit(opts$run_name, "[,;]")))
    wanted <- wanted[nzchar(wanted)]
    if (length(wanted)) runs_df <- runs_df[tolower(run_name) %in% tolower(wanted)]
  }
  if (!nrow(runs_df)) stop("No runs found after filtering suite/run_name.", call. = FALSE)
  runs_df[, replicate := if ("replicate" %in% names(runs_df)) replicate else NA_integer_]
  runs_df[, output_dir := if ("output_dir" %in% names(runs_df)) output_dir else NA_character_]
  runs_df[, log_file := if ("log_file" %in% names(runs_df)) log_file else NA_character_]
  runs_df[, status := if ("status" %in% names(runs_df)) status else NA_character_]
  runs_df[, output_exists := dir.exists(normalize_to_root(output_dir))]
  runs_df <- runs_df[output_exists | tolower(status) %in% c("success", "ok", "passed", "done")]
  if (!nrow(runs_df)) stop("No successful runs with existing outputs.", call. = FALSE)
  opts$aggregate_from_year <- opts$aggregate_from_year
  opts$aggregate_to_year <- opts$aggregate_to_year

  results_root <- normalize_to_root(opts$results_root)
  dir.create(results_root, recursive = TRUE, showWarnings = FALSE)

  run_names <- unique(runs_df$run_name)
  for (rn in run_names) {
    run_rows <- runs_df[run_name == rn]
    message(sprintf("Processing run %s (%d replicates)...", rn, nrow(run_rows)))
    proc <- process_run(rn, run_rows, opts)
    metrics_raw <- proc$metrics_raw
    results_dir <- file.path(results_root, rn, "results")
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(metrics_raw, file = file.path(results_dir, "metrics_raw.csv"))
    summary_dt <- summarize_metrics(metrics_raw)
    data.table::fwrite(summary_dt, file = file.path(results_dir, "metrics_summary.csv"))
    write_replicate_index(file.path(results_dir, "replicate_index.csv"), proc$info)
    message(sprintf("Run %s: metrics written to %s", rn, results_dir))
  }
}

tryCatch(main(), error = function(e) {
  message("ua_metrics.R failed: ", conditionMessage(e))
  quit(status = 1L)
})
