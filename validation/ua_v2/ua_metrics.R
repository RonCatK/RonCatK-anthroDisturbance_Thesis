#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
  library(glue)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

parse_cli_args <- function(args) {
  opts <- list(
    testing_csv = file.path(project_root, "validation", "system", "testing_runs.csv"),
    scenario_ids = character(0),
    results_root = file.path(project_root, "outputs", "validation", "ua_v2"),
    make_figures = TRUE,
    help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$help <- TRUE
    } else if (grepl("^--testing-csv=", arg, ignore.case = TRUE)) {
      opts$testing_csv <- sub("^--testing-csv=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--scenario=", arg, ignore.case = TRUE)) {
      vals <- sub("^--scenario=", "", arg, ignore.case = TRUE)
      vals <- trimws(unlist(strsplit(vals, "[,;]")))
      vals <- vals[nzchar(vals)]
      opts$scenario_ids <- unique(c(opts$scenario_ids, vals))
    } else if (grepl("^--results-root=", arg, ignore.case = TRUE)) {
      opts$results_root <- sub("^--results-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--figures=", arg, ignore.case = TRUE)) {
      flag <- tolower(sub("^--figures=", "", arg, ignore.case = TRUE))
      opts$make_figures <- flag %in% c("1", "true", "t", "yes", "y", "on")
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript validation/ua_v2/ua_metrics.R [options]\n",
    "  --testing-csv=PATH   Path to system testing_runs.csv (default validation/system/testing_runs.csv)\n",
    "  --scenario=IDS       Comma-separated UA scenario_ids (default = all rows beginning with 'ua_').\n",
    "  --results-root=DIR   Directory to store UA metric outputs (default outputs/validation/ua_v2).\n",
    "  --figures=true|false Generate summary figures after metrics (default true).\n",
    "  --help               Show this message and exit.\n"
  ))
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
    if (!is.null(expanded) && (file.exists(expanded) || dir.exists(expanded))) {
      return(expanded)
    }
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

compute_yearly_new <- function(vals) {
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

read_sector_stats <- function(output_dir) {
  shp_files <- list.files(output_dir, pattern = "^disturbances_.*\\.shp$", full.names = TRUE)
  if (!length(shp_files)) return(data.table())
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
    data.table(
      sector = sector,
      year = year,
      geom_kind = kind,
      cumulative_area_km2 = geom_area_km2(geom),
      cumulative_length_km = geom_length_km(geom)
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
    cumulative_length_km = if (all(is.na(cumulative_length_km))) NA_real_ else sum(cumulative_length_km, na.rm = TRUE)
  ), by = .(sector, year)]
  dt[]
}

metrics_from_output_dir <- function(output_dir, scenario_id, rep_id) {
  sector_dt <- read_sector_stats(output_dir)
  if (!nrow(sector_dt)) return(data.table())
  out <- list()
  poly_dt <- sector_dt[geom_kind == "polygons"]
  if (nrow(poly_dt)) {
    setorder(poly_dt, sector, year)
    poly_dt[, sector_yearly_new_area_km2 := compute_yearly_new(cumulative_area_km2), by = sector]
    out[["sector_yearly"]] <- poly_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_yearly_new_area_km2",
      sector = sector,
      value = sector_yearly_new_area_km2
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
    out[["total_yearly"]] <- totals[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "total_yearly_new_area_km2",
      sector = NA_character_,
      value = value
    )]
  }
  line_dt <- sector_dt[geom_kind == "lines"]
  if (nrow(line_dt)) {
    setorder(line_dt, sector, year)
    out[["sector_current_length"]] <- line_dt[, .(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = year,
      metric = "sector_current_total_length_km",
      sector = sector,
      value = cumulative_length_km
    )]
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
  sumDT <- dt[, .(
    n = .N,
    mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
    sd = if (all(is.na(value))) NA_real_ else stats::sd(value, na.rm = TRUE)
  ), by = .(scenario_id, label, year, metric, sector)]
  sumDT[is.nan(mean), mean := NA_real_]
  sumDT[is.nan(sd), sd := NA_real_]
  sumDT[, se := ifelse(n > 0, sd / sqrt(pmax(n, 1)), NA_real_)]
  sumDT[, `:=`(
    lwr = mean - 1.96 * se,
    upr = mean + 1.96 * se
  )]
  sumDT
}

generate_figures <- function(results_dir, make_figures) {
  if (!isTRUE(make_figures)) return(invisible(NULL))
  metrics_path <- file.path(results_dir, "metrics_summary.csv")
  if (!file.exists(metrics_path)) return(invisible(NULL))
  fig_script <- file.path(project_root, "validation", "ua_v2", "ua_figures.R")
  if (!file.exists(fig_script)) {
    warning("ua_figures.R not found; skipping figures.", call. = FALSE)
    return(invisible(NULL))
  }
  fig_dir <- file.path(dirname(results_dir), "figures")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  status <- system2(
    command = Sys.which("Rscript"),
    args = c(fig_script,
             paste0("--metrics=", metrics_path),
             paste0("--output-dir=", fig_dir))
  )
  if (!identical(status, 0L)) {
    warning("ua_figures.R reported a non-zero exit status.", call. = FALSE)
  }
}

process_scenario <- function(row, results_root, make_figures) {
  scenario_id <- row$scenario_id
  label <- if (nzchar(row$description)) row$description else scenario_id
  output_paths <- split_paths(row$output_path)
  if (!length(output_paths)) {
    message(sprintf("Scenario %s has no recorded output paths; skipping.", scenario_id))
    return(invisible(NULL))
  }
  log_paths <- split_paths(row$log_path)
  norm_outputs <- vapply(output_paths, normalize_to_root, character(1))
  norm_logs <- if (length(log_paths)) vapply(log_paths, normalize_to_root, character(1)) else character(0)
  rep_ids <- vapply(seq_along(norm_outputs), function(i) {
    base <- basename(norm_outputs[[i]])
    rid <- suppressWarnings(as.integer(sub(".*_run_(\\d+).*", "\\1", base)))
    if (is.na(rid)) i else rid
  }, integer(1))

  metrics_list <- list()
  info_rows <- list()
  for (i in seq_along(norm_outputs)) {
    outDir <- norm_outputs[[i]]
    if (!dir.exists(outDir)) {
      warning(sprintf("Output directory missing for scenario %s replicate %d: %s", scenario_id, rep_ids[[i]], outDir), call. = FALSE)
      next
    }
    metrics <- metrics_from_output_dir(outDir, scenario_id, rep_ids[[i]])
    if (nrow(metrics)) {
      metrics[, label := label]
      metrics_list[[length(metrics_list) + 1L]] <- metrics
    }
    info_rows[[length(info_rows) + 1L]] <- data.table(
      scenario_id = scenario_id,
      label = label,
      rep_id = rep_ids[[i]],
      output_path = relative_to_root(outDir),
      log_path = if (length(norm_logs) >= i) relative_to_root(norm_logs[[i]]) else NA_character_
    )
  }
  metrics_raw <- if (length(metrics_list)) {
    data.table::rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table()
  }
  if (!nrow(metrics_raw)) {
    message(sprintf("No metrics extracted for scenario %s.", scenario_id))
  }
  scenario_root <- file.path(results_root, scenario_id)
  results_dir <- file.path(scenario_root, "results")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  metrics_raw_path <- file.path(results_dir, "metrics_raw.csv")
  data.table::fwrite(metrics_raw, file = metrics_raw_path)
  summary_dt <- summarize_metrics(metrics_raw)
  data.table::fwrite(summary_dt, file = file.path(results_dir, "metrics_summary.csv"))
  replicate_idx <- if (length(info_rows)) data.table::rbindlist(info_rows, use.names = TRUE, fill = TRUE) else data.table()
  write_replicate_index(file.path(results_dir, "replicate_index.csv"), replicate_idx)
  generate_figures(results_dir, make_figures)
  message(glue("Scenario {scenario_id}: wrote metrics to {results_dir}"))
}

main <- function() {
  opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    print_usage()
    quit(save = "no", status = 0, runLast = FALSE)
  }
  testing_csv <- normalizePath(opts$testing_csv, winslash = "/", mustWork = TRUE)
  table <- data.table::fread(testing_csv, fill = TRUE)
  if (!"scenario_id" %in% names(table)) {
    stop("testing CSV missing scenario_id column.", call. = FALSE)
  }
  scenario_ids <- opts$scenario_ids
  if (!length(scenario_ids)) {
    scenario_ids <- table[startsWith(scenario_id, "ua_"), unique(scenario_id)]
  }
  if (!length(scenario_ids)) {
    message("No UA scenarios detected; nothing to summarize.")
    quit(save = "no", status = 0, runLast = FALSE)
  }
  selected <- table[scenario_id %in% scenario_ids]
  if (!nrow(selected)) {
    message("Requested UA scenarios not found in testing CSV.")
    quit(save = "no", status = 1, runLast = FALSE)
  }
  results_root <- normalizePath(opts$results_root, winslash = "/", mustWork = FALSE)
  dir.create(results_root, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(selected))) {
    process_scenario(selected[i], results_root = results_root, make_figures = opts$make_figures)
  }
}

main()
