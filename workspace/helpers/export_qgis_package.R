#!/usr/bin/env Rscript
# Builds a QGIS-ready package from the latest successful adqd_validation run.


args_full <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
script_dir <- if (length(script_path)) dirname(script_path) else getwd()
project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tools)
})
sf::sf_use_s2(FALSE)
options(warn = 1)

snap_tolerance <- 0.25
line_diff_buffer <- 1
area_sliver_tolerance <- 50    # square metres
length_sliver_tolerance <- 20  # metres
scratch_root <- file.path(project_root, "scratch")
packages_root <- file.path(scratch_root, "qgis_packages")
dir.create(packages_root, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (is.na(a) || (is.character(a) && !nzchar(a[1]))) return(b)
  a
}

sanitize_layer <- function(...) {
  nm <- paste(..., sep = "_")
  nm <- gsub("[^A-Za-z0-9_]+", "_", nm)
  nm <- gsub("_+", "_", nm)
  nm <- sub("^_", "", nm)
  nm <- substr(nm, 1, 60)
  if (!nzchar(nm)) nm <- paste0("layer_", format(Sys.time(), "%H%M%S"))
  nm
}

unique_layer <- function(base, env) {
  count <- env[[base]]
  if (is.null(count)) {
    env[[base]] <- 1L
    base
  } else {
    count <- count + 1L
    env[[base]] <- count
    sanitize_layer(base, count)
  }
}

normalize_class_name <- function(value) {
  if (is.null(value) || !nzchar(value)) return("unknown")
  gsub("[^a-z0-9]+", "", tolower(value))
}

add_to_collector <- function(collector, class_label, path) {
  if (is.null(collector) || is.null(path) || !nzchar(path)) return(collector)
  label <- if (is.null(class_label) || !nzchar(class_label)) "unknown" else class_label
  key <- normalize_class_name(label)
  entry <- collector[[key]]
  if (is.null(entry)) {
    entry <- list(paths = character(), label = label)
  }
  if (!(path %in% entry$paths)) {
    entry$paths <- c(entry$paths, path)
  }
  if (!nzchar(entry$label)) entry$label <- label
  collector[[key]] <- entry
  collector
}

resolve_shape_path <- function(base_root, file_name = NULL, url_val = NULL) {
  candidates <- character()
  if (!is.null(file_name) && nzchar(file_name)) {
    fn <- utils::URLdecode(file_name)
    if (grepl("^file://", fn)) fn <- sub("^file://", "", fn)
    if (grepl("^(/|[A-Za-z]:)", fn)) {
      candidates <- c(candidates, fn)
    } else {
      candidates <- c(candidates, file.path(base_root, fn))
    }
  }
  if (!is.null(url_val) && nzchar(url_val)) {
    url_val <- utils::URLdecode(url_val)
    if (grepl("^file://", url_val)) url_val <- sub("^file://", "", url_val)
    candidates <- c(candidates, url_val)
  }
  candidates <- unique(candidates)
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  if (length(candidates)) return(candidates[[1]])
  NULL
}

snap_geometries <- function(obj, tolerance = snap_tolerance) {
  if (is.null(obj) || !nrow(obj) || tolerance <= 0) return(obj)
  tryCatch(sf::st_snap_to_grid(obj, tolerance), error = function(e) obj)
}

drop_sliver_geometries <- function(obj, min_area = area_sliver_tolerance, min_length = length_sliver_tolerance) {
  if (is.null(obj) || !nrow(obj)) return(NULL)
  geom_types <- unique(as.character(sf::st_geometry_type(obj)))
  keep <- rep(TRUE, nrow(obj))
  if (any(grepl("POLYGON|MULTIPOLYGON", geom_types, ignore.case = TRUE))) {
    areas <- suppressWarnings(as.numeric(sf::st_area(obj)))
    keep <- areas >= min_area
  } else if (any(grepl("LINESTRING|MULTILINESTRING", geom_types, ignore.case = TRUE))) {
    lengths <- suppressWarnings(as.numeric(sf::st_length(obj)))
    keep <- lengths >= min_length
  }
  obj <- obj[keep, , drop = FALSE]
  if (!nrow(obj)) return(NULL)
  obj
}

buffer_lines_for_diff <- function(obj, width = line_diff_buffer) {
  if (is.null(obj) || width <= 0) return(NULL)
  geom <- if (inherits(obj, "sf")) obj else sf::st_sf(geometry = obj)
  if (!nrow(geom)) return(NULL)
  tryCatch(sf::st_buffer(geom, dist = width, endCapStyle = "FLAT"), error = function(e) NULL)
}

crs_defined <- function(crs_obj) {
  if (is.null(crs_obj)) return(FALSE)
  epsg_ok <- !is.null(crs_obj$epsg) && !is.na(crs_obj$epsg)
  wkt_ok <- !is.null(crs_obj$wkt) && nzchar(crs_obj$wkt)
  epsg_ok || wkt_ok
}

add_layer <- function(path, layer_name, gpkg_path, env) {
  if (!file.exists(path)) {
    warning("Skipping missing shapefile: ", path)
    return()
  }
  message("Adding layer ", layer_name, " from ", path)
  lyr <- tryCatch(st_read(path, quiet = TRUE), error = function(e) {
    warning("Failed to read ", path, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(lyr)) return()
  final_name <- unique_layer(layer_name, env)
  st_write(lyr, gpkg_path, layer = final_name, append = file.exists(gpkg_path), quiet = TRUE)
  invisible(NULL)
}

process_disturbance_csv <- function(base_root, csv_relpath, origin_tag, gpkg_path, env, collector = NULL) {
  csv_path <- if (grepl("^(/|[A-Za-z]:)", csv_relpath)) csv_relpath else file.path(base_root, csv_relpath)
  if (!file.exists(csv_path)) {
    warning("CSV not found: ", csv_path)
    return(collector)
  }
  dt <- fread(csv_path)
  if (!NROW(dt)) return(collector)
  cols <- c("dataName", "dataClass", "classToSearch", "fileName")
  if ("URL" %in% names(dt)) cols <- c(cols, "URL")
  if ("url" %in% names(dt)) cols <- c(cols, "url")
  dt <- unique(dt[, ..cols])
  for (i in seq_len(nrow(dt))) {
    row <- dt[i]
    shp_path <- resolve_shape_path(base_root, row$fileName, row$URL %||% row$url)
    purpose <- if (grepl("^potential", row$dataClass, ignore.case = TRUE)) "potential" else "baseline"
    layer_name <- sanitize_layer("input", origin_tag, purpose, row$dataName, row$dataClass, row$classToSearch %||% "")
    add_layer(shp_path, layer_name, gpkg_path, env)
    if (!grepl("^potential", row$dataClass, ignore.case = TRUE)) {
      collector <- add_to_collector(collector, row$dataClass, shp_path)
    }
  }
  collector
}

process_outputs_for_run <- function(run_dir, run_name, gpkg_path, env,
                                    replicate_tag = NULL, collector = NULL) {
  target_dir <- run_dir
  label <- run_name
  if (!is.null(replicate_tag) && nzchar(replicate_tag)) {
    candidate <- file.path(run_dir, replicate_tag)
    if (dir.exists(candidate)) {
      target_dir <- candidate
      label <- paste(run_name, replicate_tag, sep = "_")
    } else {
      warning("Replicate ", replicate_tag, " not found under ", run_dir, "; using base directory.")
    }
  }
  shp_files <- list.files(target_dir, pattern = "^disturbances_.*\\.shp$", full.names = TRUE, recursive = TRUE)
  if (!length(shp_files)) {
    stop("No disturbance shapefiles found in ", target_dir, "; cannot package outputs.")
  }
  meta <- lapply(shp_files, function(shp) {
    fname <- basename(shp)
    parts <- strsplit(file_path_sans_ext(sub("^disturbances_", "", fname)), "_")[[1]]
    if (length(parts) < 2) return(NULL)
    year_guess <- suppressWarnings(as.integer(tail(parts, 1)))
    list(
      file = shp,
      dataName = parts[1],
      origin = parts[2],
      year = year_guess,
      year_tag = if (!is.na(year_guess)) year_guess else tail(parts, 1)
    )
  })
  meta <- Filter(Negate(is.null), meta)
  if (!length(meta)) {
    warning("Could not parse disturbance filenames under ", run_dir)
    warning("Could not parse disturbance filenames under ", run_dir)
    return(collector)
  }
  years <- vapply(meta, `[[`, numeric(1), "year")
  if (all(is.na(years))) {
    chosen <- seq_along(meta)
  } else {
    max_year <- max(years, na.rm = TRUE)
    chosen <- which(years == max_year)
  }
  for (idx in chosen) {
    info <- meta[[idx]]
    layer_name <- sanitize_layer("output", label, info$year_tag, info$dataName, info$origin)
    add_layer(info$file, layer_name, gpkg_path, env)
    collector <- add_to_collector(collector, info$origin, info$file)
  }
  collector
}

collect_sf_from_paths <- function(paths, target_crs = NULL, line_tol = length_sliver_tolerance) {
  if (is.null(paths) || !length(paths)) return(NULL)
  objs <- list()
  current_crs <- if (crs_defined(target_crs)) target_crs else NULL
  for (path in unique(paths)) {
    obj <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) NULL)
    if (is.null(obj) || !nrow(obj)) next
    obj <- tryCatch(sf::st_make_valid(obj), error = function(e) obj)
    obj <- obj[, 0, drop = FALSE]
    obj <- snap_geometries(obj)
    obj_crs <- sf::st_crs(obj)
    if (!crs_defined(current_crs) && crs_defined(obj_crs)) {
      current_crs <- obj_crs
    }
    if (crs_defined(current_crs) && crs_defined(obj_crs) && !identical(obj_crs, current_crs)) {
      obj <- tryCatch(sf::st_transform(obj, current_crs), error = function(e) obj)
    }
    objs[[length(objs) + 1L]] <- obj
  }
  if (!length(objs)) return(NULL)
  out <- do.call(rbind, objs)
  if (crs_defined(current_crs)) sf::st_crs(out) <- current_crs
  drop_sliver_geometries(out, min_length = line_tol)
}

compute_difference_geometry <- function(primary, secondary) {
  if (is.null(primary) || !nrow(primary)) return(NULL)
  primary <- snap_geometries(primary)
  primary <- drop_sliver_geometries(primary, min_length = 0)
  if (is.null(primary) || !nrow(primary)) return(NULL)
  if (is.null(secondary) || !nrow(secondary)) {
    return(primary)
  }
  target_crs <- sf::st_crs(primary)
  secondary_crs <- sf::st_crs(secondary)
  if (crs_defined(target_crs) && crs_defined(secondary_crs) &&
      !identical(secondary_crs, target_crs)) {
    secondary <- tryCatch(sf::st_transform(secondary, target_crs), error = function(e) secondary)
  }
  secondary <- snap_geometries(secondary)
  secondary <- drop_sliver_geometries(secondary, min_length = 0)
  if (is.null(secondary) || !nrow(secondary)) return(primary)
  geom_types <- unique(as.character(sf::st_geometry_type(primary)))
  if (any(grepl("LINESTRING", geom_types, ignore.case = TRUE))) {
    prim_union <- tryCatch(sf::st_union(primary), error = function(e) primary)
    prim_buf <- buffer_lines_for_diff(prim_union)
    if (is.null(prim_buf)) return(NULL)
    sec_union <- tryCatch(sf::st_union(secondary), error = function(e) NULL)
    sec_buf <- buffer_lines_for_diff(sec_union)
    diff_poly <- if (is.null(sec_buf)) {
      prim_buf
    } else {
      tryCatch(sf::st_difference(prim_buf, sec_buf), error = function(e) NULL)
    }
    if (is.null(diff_poly)) return(NULL)
    diff_poly <- snap_geometries(diff_poly)
    diff_poly <- diff_poly[!sf::st_is_empty(diff_poly), , drop = FALSE]
    if (!nrow(diff_poly)) return(NULL)
    mask <- tryCatch(sf::st_union(diff_poly), error = function(e) NULL)
    if (is.null(mask) || sf::st_is_empty(mask)) return(NULL)
    diff_lines <- tryCatch(sf::st_intersection(primary, mask), error = function(e) NULL)
    if (is.null(diff_lines) || !nrow(diff_lines)) return(NULL)
    diff_lines <- snap_geometries(diff_lines)
    diff_lines <- drop_sliver_geometries(diff_lines, min_length = 0)
    if (is.null(diff_lines) || !nrow(diff_lines)) return(NULL)
    return(diff_lines)
  }
  union_geom <- tryCatch(sf::st_union(secondary), error = function(e) NULL)
  if (is.null(union_geom)) return(primary)
  diff_geom <- tryCatch(sf::st_difference(primary, union_geom), error = function(e) NULL)
  if (is.null(diff_geom)) return(NULL)
  diff_geom <- snap_geometries(diff_geom)
  diff_geom <- diff_geom[!sf::st_is_empty(diff_geom), , drop = FALSE]
  if (!nrow(diff_geom)) return(NULL)
  drop_sliver_geometries(diff_geom, min_length = 0)
}

write_difference_layers <- function(run_id, gpkg_path, env, input_coll, output_coll) {
  class_keys <- sort(unique(c(names(input_coll), names(output_coll))))
  if (!length(class_keys)) return()
  for (key in class_keys) {
    input_info <- input_coll[[key]]
    output_info <- output_coll[[key]]
    input_sf <- collect_sf_from_paths(if (!is.null(input_info)) input_info$paths else NULL,
                                      line_tol = 0)
    output_sf <- collect_sf_from_paths(if (!is.null(output_info)) output_info$paths else NULL,
                                       target_crs = if (!is.null(input_sf)) sf::st_crs(input_sf) else NULL,
                                       line_tol = 0)
    label <- output_info$label %||% input_info$label %||% key
    diff_sim <- compute_difference_geometry(output_sf, input_sf)
    if (!is.null(diff_sim) && nrow(diff_sim)) {
      layer_name <- sanitize_layer("difference", run_id, label, "sim_minus_input")
      final_name <- unique_layer(layer_name, env)
      sf::st_write(diff_sim, gpkg_path, layer = final_name,
                   append = file.exists(gpkg_path), quiet = TRUE)
    }
  }
}

# --- choose latest successful adqd_validation run ----------------------------
runs_path <- file.path(project_root, "workspace", "adqd_validation", "runs.csv")
if (!file.exists(runs_path)) stop("runs.csv not found: ", runs_path)
runs_dt <- fread(runs_path)
if (!nrow(runs_dt)) stop("runs.csv is empty: ", runs_path)
ok <- runs_dt[status == "success" & suite == "adqd_validation"]
if (!nrow(ok)) stop("No successful adqd_validation runs found in runs.csv")
setorder(ok, timestamp)
latest <- ok[.N]

input_root <- latest$input_root
if (!grepl("^(/|[A-Za-z]:)", input_root)) input_root <- file.path(project_root, input_root)
input_root <- normalizePath(input_root, winslash = "/", mustWork = TRUE)

output_dir <- latest$output_dir
if (!grepl("^(/|[A-Za-z]:)", output_dir)) output_dir <- file.path(project_root, output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
run_base <- basename(output_dir)            # e.g., rep_005
run_parent <- dirname(output_dir)           # e.g., .../ADQD_VERIFICATION
run_name <- latest$run_name %||% basename(run_parent)

pkg_id <- paste0("adqd_", run_name, "_", run_base)
gpkg_path <- file.path(packages_root, paste0(pkg_id, ".gpkg"))
zip_path <- file.path(packages_root, paste0(pkg_id, ".zip"))
if (file.exists(gpkg_path)) unlink(gpkg_path)
if (file.exists(zip_path)) unlink(zip_path)
layer_env <- new.env(parent = emptyenv())
input_collector <- list()
output_collector <- list()

# Add the disturbanceDT used for the run (inputs)
input_collector <- process_disturbance_csv(
  base_root = input_root,
  csv_relpath = "disturbanceDT.csv",
  origin_tag = "adqd_input",
  gpkg_path = gpkg_path,
  env = layer_env,
  collector = input_collector
)

# Add sim outputs if any shapefiles exist
output_collector <- process_outputs_for_run(
  run_dir = run_parent,
  run_name = run_name,
  gpkg_path = gpkg_path,
  env = layer_env,
  replicate_tag = run_base,
  collector = output_collector
)

# Optional: diff layers when both input and output exist
if (length(output_collector)) {
  message("Computing per-class differences for ", pkg_id, " ...")
  write_difference_layers(pkg_id, gpkg_path, layer_env, input_collector, output_collector)
}

if (!file.exists(gpkg_path)) stop("No layers written; nothing to package.")

utils::zip(zipfile = zip_path, files = gpkg_path, flags = "-j")

message("Packaged latest run: ", pkg_id)
message("  GeoPackage: ", gpkg_path)
message("  Zip archive: ", zip_path)
