#!/usr/bin/env Rscript
# Computes BEAD vs simulation confusion, quantity, and disagreement metrics for AD/QD runs.

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
default_analysis_mode <- "adqd_holdout"

parse_cli_args <- function(args) {
  opts <- list(
    simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_HOLDOUT"),
    output_root = file.path(project_root, "scratch", "adqd_validation", "metrics", "ADQD_HOLDOUT"),
    bead_root = file.path(project_root, "data", "raw", "ECCC"),
    study_area = file.path(project_root, "data", "study_area", "aoi_southwest_NWT.shp"),
    intervals = list(c(2015L, 2020L)),
    replicates = 1:5,
    line_buffer = 30,
    polygon_buffer = 0,
    raster_resolution = 15,
    analysis_mode = default_analysis_mode,
    caribou_buffer = FALSE,
    help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$help <- TRUE
    } else if (identical(arg, "--caribou-buffer")) {
      opts$caribou_buffer <- TRUE
    } else if (grepl("^--simulation-root=", arg, ignore.case = TRUE)) {
      opts$simulation_root <- sub("^--simulation-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--output-root=", arg, ignore.case = TRUE)) {
      opts$output_root <- sub("^--output-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--bead-root=", arg, ignore.case = TRUE)) {
      opts$bead_root <- sub("^--bead-root=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--study-area=", arg, ignore.case = TRUE)) {
      opts$study_area <- sub("^--study-area=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--intervals=", arg, ignore.case = TRUE)) {
      raw <- sub("^--intervals=", "", arg, ignore.case = TRUE)
      segs <- trimws(unlist(strsplit(raw, "[,;]")))
      segs <- segs[nzchar(segs)]
      parsed <- lapply(segs, function(seg) {
        yrs <- as.integer(strsplit(seg, "[:|-]")[[1]])
        if (length(yrs) != 2 || any(is.na(yrs))) {
          stop("Invalid interval specification: ", seg, call. = FALSE)
        }
        yrs
      })
      opts$intervals <- parsed
    } else if (grepl("^--replicates=", arg, ignore.case = TRUE)) {
      raw <- sub("^--replicates=", "", arg, ignore.case = TRUE)
      vals <- unique(as.integer(trimws(unlist(strsplit(raw, "[,;]")))))
      vals <- vals[!is.na(vals)]
      if (length(vals)) opts$replicates <- vals
    } else if (grepl("^--line-buffer=", arg, ignore.case = TRUE)) {
      val <- suppressWarnings(as.numeric(sub("^--line-buffer=", "", arg, ignore.case = TRUE)))
      if (!is.na(val)) opts$line_buffer <- val
    } else if (grepl("^--polygon-buffer=", arg, ignore.case = TRUE)) {
      val <- suppressWarnings(as.numeric(sub("^--polygon-buffer=", "", arg, ignore.case = TRUE)))
      if (!is.na(val)) opts$polygon_buffer <- val
    } else if (grepl("^--resolution=", arg, ignore.case = TRUE)) {
      val <- suppressWarnings(as.numeric(sub("^--resolution=", "", arg, ignore.case = TRUE)))
      if (!is.na(val) && val > 0) opts$raster_resolution <- val
    } else if (grepl("^--analysis-mode=", arg, ignore.case = TRUE)) {
      val <- trimws(sub("^--analysis-mode=", "", arg, ignore.case = TRUE))
      if (nzchar(val)) opts$analysis_mode <- val
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/adqd_validation/compute_map_metrics.R [options]\n",
    "  --simulation-root=DIR   Path to AD/QD run outputs (default outputs/adqd_validation/ADQD_HOLDOUT).\n",
    "  --output-root=DIR       Directory to store metric tables (default scratch/adqd_validation/metrics/ADQD_HOLDOUT).\n",
    "  --bead-root=DIR         Folder with BEAD data archives (default data/raw/ECCC).\n",
    "  --study-area=FILE       Study area polygon used for clipping (default data/study_area/aoi_southwest_NWT.shp).\n",
    "  --intervals=a:b,c:d     Comma-separated baseline:comparison year pairs (default 2015:2020).\n",
    "  --replicates=list       Replicate IDs to analyse (default 1:5).\n",
    "  --line-buffer=VALUE     Buffer (m) applied to line features before calculating areas (default 30).\n",
    "  --polygon-buffer=VALUE  Optional buffer (m) applied to polygons (default 0).\n",
    "  --resolution=VALUE      Raster resolution in metres for map comparison (default 15).\n",
    "  --caribou-buffer        Use 500 m buffers for all disturbance geometries (caribou mode).\n",
    "  --analysis-mode=LABEL   Tag written to outputs (default adqd_holdout).\n",
    "  --help                  Show this message and exit.\n"
  ))
}

normalize_existing_path <- function(pathValue, mustExist = TRUE) {
  if (is.null(pathValue) || !nzchar(pathValue)) return(NA_character_)
  expanded <- path.expand(pathValue)
  if (mustExist && !file.exists(expanded)) {
    stop("Path not found: ", expanded, call. = FALSE)
  }
  normalizePath(expanded, winslash = "/", mustWork = mustExist)
}

vsizip_path <- function(archive, inner = NULL) {
  arch <- normalize_existing_path(archive, mustExist = TRUE)
  if (is.null(inner)) {
    paste0("/vsizip/", arch)
  } else {
    paste0("/vsizip/", arch, "/", inner)
  }
}

repair_bead_geom <- function(obj, label, geom_type) {
  sv <- if (inherits(obj, "SpatVector")) obj else terra::vect(obj)
  sf_obj <- sf::st_as_sf(sv)
  if (!nrow(sf_obj)) {
    return(terra::vect(sf_obj))
  }
  geom_type <- match.arg(geom_type, c("line", "poly"))
  make_valid_fun <- sf::st_make_valid
  if (requireNamespace("lwgeom", quietly = TRUE)) {
    ns <- asNamespace("lwgeom")
    expm <- try(getNamespaceExports("lwgeom"), silent = TRUE)
    if (!inherits(expm, "try-error") && length(expm)) {
      if ("lwgeom_make_valid" %in% expm) {
        make_valid_fun <- get("lwgeom_make_valid", envir = ns)
      } else if ("st_make_valid" %in% expm) {
        make_valid_fun <- get("st_make_valid", envir = ns)
      }
    }
  }
  drop_invalid_and_empty <- function(x) {
    if (!nrow(x)) return(x)
    x <- x[!sf::st_is_empty(x), ]
    if (!nrow(x)) return(x)
    valid_idx <- sf::st_is_valid(x)
    valid_idx[is.na(valid_idx)] <- FALSE
    x[valid_idx, , drop = FALSE]
  }
  process_feature <- function(row_sf, idx) {
    tryCatch({
      geom <- sf::st_geometry(row_sf)
      geom <- suppressWarnings(make_valid_fun(geom))
      geom <- suppressWarnings(sf::st_buffer(geom, 0))
      geom <- suppressWarnings(sf::st_zm(geom, drop = TRUE, what = "ZM"))
      if (all(sf::st_is_empty(geom))) return(NULL)
      if (any(!sf::st_is_valid(geom))) {
        warning(sprintf("Dropping invalid geometry %d from BEAD layer '%s'.", idx, label), immediate. = FALSE)
        return(NULL)
      }
      row_sf$geometry <- geom
      row_sf
    }, error = function(e) {
      warning(sprintf("Dropping geometry %d from BEAD layer '%s' due to repair error: %s", idx, label, conditionMessage(e)), immediate. = FALSE)
      NULL
    })
  }
  fast_attempt <- tryCatch({
    geom <- sf::st_geometry(sf_obj)
    geom <- suppressWarnings(make_valid_fun(geom))
    geom <- suppressWarnings(sf::st_buffer(geom, 0))
    geom <- suppressWarnings(sf::st_zm(geom, drop = TRUE, what = "ZM"))
    sf_obj$geometry <- geom
    drop_invalid_and_empty(sf_obj)
  }, error = function(e) e)
  if (!inherits(fast_attempt, "error")) {
    valid <- drop_invalid_and_empty(fast_attempt)
    if (nrow(valid)) return(terra::vect(valid))
  }
  chunk_indices <- split(seq_len(nrow(sf_obj)), ceiling(seq_len(nrow(sf_obj)) / 2000L))
  repaired_list <- lapply(chunk_indices, function(idx) {
    chunk <- sf_obj[idx, , drop = FALSE]
    chunk_try <- tryCatch({
      geom <- sf::st_geometry(chunk)
      geom <- suppressWarnings(make_valid_fun(geom))
      geom <- suppressWarnings(sf::st_buffer(geom, 0))
      geom <- suppressWarnings(sf::st_zm(geom, drop = TRUE, what = "ZM"))
      chunk$geometry <- geom
      drop_invalid_and_empty(chunk)
    }, error = function(e) e)
    if (inherits(chunk_try, "error")) {
      cleaned <- lapply(seq_along(idx), function(j) process_feature(chunk[j, , drop = FALSE], idx[[j]]))
      cleaned <- cleaned[!vapply(cleaned, is.null, logical(1))]
      if (!length(cleaned)) return(NULL)
      do.call(rbind, cleaned)
    } else if (!nrow(chunk_try)) {
      NULL
    } else {
      chunk_try
    }
  })
  repaired_list <- repaired_list[!vapply(repaired_list, is.null, logical(1))]
  if (!length(repaired_list)) {
    warning(sprintf("BEAD layer '%s' became empty after geometry repairs; skipping this layer.", label), immediate. = FALSE)
    return(terra::vect(sf_obj[0, , drop = FALSE]))
  }
  terra::vect(do.call(rbind, repaired_list))
}

load_bead_layer <- function(year, geom_type, bead_root) {
  bead_root <- normalize_existing_path(bead_root, mustExist = TRUE)
  geom_type <- match.arg(geom_type, c("line", "poly"))
  message(sprintf("Loading BEAD %d %s layer from %s", year, geom_type, bead_root))
  label_tag <- function(y, g) paste0("year", y, "_", g)
  if (year == 2010) {
    archive <- file.path(bead_root, "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip")
    target <- if (geom_type == "line") {
      "EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp"
    } else {
      "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp"
    }
    source <- vsizip_path(archive, target)
    if (geom_type == "line") {
      terra::vect(source)
    } else {
      repair_bead_geom(source, label_tag(year, geom_type), geom_type)
    }
  } else if (year == 2015) {
    archive <- file.path(bead_root, "ECCC_2015_anthro_dist_corrected_to_NT1_2016_final.zip")
    target <- if (geom_type == "line") {
      "BEADlines2015_NWT_corrected_to_NT1_2016.shp"
    } else {
      "BEADpolys2015_NWT_corrected_to_NT1_2016.shp"
    }
    source <- vsizip_path(archive, target)
    if (geom_type == "line") {
      terra::vect(source)
    } else {
      repair_bead_geom(source, label_tag(year, geom_type), geom_type)
    }
  } else if (year == 2020) {
    gpkg_line <- file.path(bead_root, "NWT2020_Disturb_Perturb_Line_valid.gpkg")
    gpkg_poly <- file.path(bead_root, "NWT2020_Disturb_Perturb_Poly_valid.gpkg")
    if (geom_type == "line" && file.exists(gpkg_line)) {
      terra::vect(gpkg_line)
    } else if (geom_type == "poly" && file.exists(gpkg_poly)) {
      sfx <- sf::read_sf(gpkg_poly, quiet = TRUE)
      repair_bead_geom(sfx, label_tag(year, geom_type), geom_type)
    } else {
      archive <- file.path(bead_root, "NorthwestTerritories2020.gdb.zip")
      gdb_path <- extract_gdb(archive)
      layer <- if (geom_type == "line") {
        "NWT2020_Disturb_Perturb_Line"
      } else {
        "NWT2020_Disturb_Perturb_Poly"
      }
      if (geom_type == "line") {
        terra::vect(gdb_path, layer = layer)
      } else {
        sfx <- sf::read_sf(gdb_path, layer = layer, quiet = TRUE)
        sfx <- sf::st_cast(sfx, "MULTIPOLYGON")
        repair_bead_geom(sfx, label_tag(year, geom_type), geom_type)
      }
    }
  } else {
    stop("Unsupported BEAD year: ", year, call. = FALSE)
  }
}

ensure_spatvector <- function(obj) {
  if (inherits(obj, "SpatVector")) return(obj)
  terra::vect(obj)
}

drop_empty_geoms <- function(sv) {
  if (is.null(sv) || !inherits(sv, "SpatVector")) return(sv)
  coords <- tryCatch(terra::geom(sv, df = TRUE), error = function(...) NULL)
  if (is.null(coords) || !nrow(coords)) return(sv)
  finite <- is.finite(coords$x) & is.finite(coords$y)
  valid_ids <- coords$geom[finite]
  valid_ids <- valid_ids[!is.na(valid_ids)]
  if (!length(valid_ids)) {
    return(sv[0])
  }
  valid_ids <- sort(unique(as.integer(valid_ids)))
  if (length(valid_ids) >= nrow(sv)) {
    return(sv)
  }
  sv[valid_ids]
}

canonical_class <- function(x) {
  if (is.null(x)) return(x)
  lx <- tolower(trimws(as.character(x)))
  lx_norm <- gsub("[^a-z0-9]", "", lx)
  map <- c(
    seismic = "Seismic",
    seismiclines = "Seismic",
    road = "Road",
    roads = "Road",
    pipeline = "Pipeline",
    pipelines = "Pipeline",
    oilgas = "Oil/Gas",
    oilandgas = "Oil/Gas",
    oilgaslegacy = "Oil/Gas",
    cutblock = "Cutblock",
    cutblocks = "Cutblock",
    harvest = "Harvest",
    logging = "Harvest",
    mine = "Mine",
    mining = "Mine",
    settlement = "Settlement",
    settlements = "Settlement",
    othersettlements = "Settlement",
    wellsite = "Well site",
    wellsites = "Well site"
  )
  out <- map[ lx_norm ]
  ifelse(is.na(out), x, out)
}

extract_gdb <- function(archive_path, label = "NWT2020") {
  archive_path <- normalize_existing_path(archive_path, mustExist = TRUE)
  cache_dir <- file.path(project_root, "cache", "adqd_validation", "bead_gdb")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  target_dir <- file.path(cache_dir, label)
  gdb_dirs <- if (dir.exists(target_dir)) {
    list.dirs(target_dir, recursive = FALSE, full.names = TRUE)
  } else {
    character(0)
  }
  gdb_dirs <- gdb_dirs[grepl("\\.gdb$", gdb_dirs, ignore.case = TRUE)]
  if (!length(gdb_dirs)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(archive_path, exdir = target_dir)
    gdb_dirs <- list.dirs(target_dir, recursive = FALSE, full.names = TRUE)
    gdb_dirs <- gdb_dirs[grepl("\\.gdb$", gdb_dirs, ignore.case = TRUE)]
  }
  if (!length(gdb_dirs)) {
    stop("Unable to locate FileGDB contents extracted from ", archive_path, call. = FALSE)
  }
  gdb_dirs[[1]]
}

clip_with_sf <- function(geom, study_area) {
  geom_sf <- sf::st_as_sf(geom)
  area_sf <- sf::st_as_sf(study_area)
  geom_sf <- sf::st_make_valid(geom_sf)
  area_sf <- sf::st_make_valid(area_sf)
  inter <- tryCatch(
    suppressWarnings(sf::st_intersection(geom_sf, area_sf)),
    error = function(e) {
      warning(sprintf("Intersection failed (%s); attempting buffered repair.", conditionMessage(e)), immediate. = FALSE)
      geom_fix <- suppressWarnings(sf::st_make_valid(sf::st_buffer(geom_sf, 0)))
      area_fix <- suppressWarnings(sf::st_make_valid(sf::st_buffer(area_sf, 0)))
      tryCatch(
        suppressWarnings(sf::st_intersection(geom_fix, area_fix)),
        error = function(e2) {
          warning(sprintf("Intersection still failing after repair: %s. Dropping geometry.", conditionMessage(e2)), immediate. = FALSE)
          geom_sf[0, ]
        }
      )
    }
  )
  if (!nrow(inter)) return(geom[0])
  tryCatch(
    terra::vect(inter),
    error = function(e) {
      warning(sprintf("Failed to convert clipped geometry to SpatVector (%s); dropping geometry.", conditionMessage(e)), immediate. = FALSE)
      geom[0]
    }
  )
}

process_lines_sf <- function(sv, study_area, width, label) {
  if (is.null(sv) || !length(sv)) return(sv)
  geom_sf <- sf::st_as_sf(if (inherits(sv, "SpatVector")) sv else terra::vect(sv))
  geom_sf <- suppressWarnings(sf::st_make_valid(geom_sf))
  sa_sf <- sf::st_as_sf(study_area)
  if (sf::st_crs(geom_sf) != sf::st_crs(sa_sf)) {
    geom_sf <- sf::st_transform(geom_sf, sf::st_crs(sa_sf))
  }
  geom_sf <- geom_sf[!sf::st_is_empty(geom_sf), ]
  if (!nrow(geom_sf)) return(geom_sf[0, ])
  buf <- try({
    suppressWarnings(sf::st_buffer(geom_sf, width))
  }, silent = TRUE)
  if (inherits(buf, "try-error") || !nrow(buf)) {
    warning(sprintf("sf::st_buffer failed for %s; returning empty.", label), immediate. = FALSE)
    return(geom_sf[0, ])
  }
  buf <- suppressWarnings(sf::st_make_valid(buf))
  inter <- try(suppressWarnings(sf::st_intersection(buf, sa_sf)), silent = TRUE)
  if (inherits(inter, "try-error")) {
    warning(sprintf("sf::st_intersection failed for %s; returning empty.", label), immediate. = FALSE)
    return(buf[0, ])
  }
  inter <- inter[!sf::st_is_empty(inter), ]
  if (!nrow(inter)) return(inter)
  terra::vect(inter)
}

safe_buffer <- function(sv, width, label, geom_kind) {
  geom_kind <- match.arg(geom_kind, c("line", "poly"))
  if (is.null(sv) || !length(sv)) return(sv)
  if (is.na(width) || width <= 0) return(sv)
  buf_try <- try(suppressWarnings(terra::buffer(sv, width = width)), silent = TRUE)
  if (!inherits(buf_try, "try-error")) return(buf_try)
  err_msg <- conditionMessage(attr(buf_try, "condition"))
  warning(sprintf("terra::buffer failed for %s (%s); attempting geometry repair.", label, err_msg), immediate. = FALSE)
  repaired <- try(suppressWarnings(repair_bead_geom(sv, label, geom_kind)), silent = TRUE)
  if (!inherits(repaired, "try-error") && inherits(repaired, "SpatVector") && length(repaired)) {
    buf_try <- try(suppressWarnings(terra::buffer(repaired, width = width)), silent = TRUE)
    if (!inherits(buf_try, "try-error")) return(buf_try)
  }
  warning(sprintf("terra::buffer still failing for %s; retrying with sf::st_buffer.", label), immediate. = FALSE)
  sf_buf <- try({
    sf_obj <- sf::st_as_sf(if (inherits(sv, "SpatVector")) sv else terra::vect(sv))
    sf_obj <- suppressWarnings(sf::st_make_valid(sf_obj))
    sf_obj <- suppressWarnings(sf::st_buffer(sf_obj, width))
    sf_obj <- suppressWarnings(sf::st_zm(sf_obj, drop = TRUE, what = "ZM"))
    sf_obj <- sf_obj[!sf::st_is_empty(sf_obj), ]
    if (!nrow(sf_obj)) stop("Empty after sf buffer")
    terra::vect(sf_obj)
  }, silent = TRUE)
  if (!inherits(sf_buf, "try-error")) return(sf_buf)
  # Last resort: chunked sf buffering to avoid GEOS vector asserts
  chunk_buf <- function(obj, chunk_size = 500L) {
    idx <- split(seq_len(nrow(obj)), ceiling(seq_along(seq_len(nrow(obj))) / chunk_size))
    parts <- lapply(idx, function(i) {
      piece <- obj[i, , drop = FALSE]
      piece <- suppressWarnings(sf::st_make_valid(piece))
      piece <- suppressWarnings(sf::st_buffer(piece, width))
      piece <- suppressWarnings(sf::st_zm(piece, drop = TRUE, what = "ZM"))
      piece[!sf::st_is_empty(piece), ]
    })
    parts <- parts[ vapply(parts, nrow, integer(1)) > 0 ]
    if (!length(parts)) return(NULL)
    terra::vect(do.call(rbind, parts))
  }
  chunk_try <- try(chunk_buf(sf::st_as_sf(if (inherits(sv, "SpatVector")) sv else terra::vect(sv))), silent = TRUE)
  if (!inherits(chunk_try, "try-error") && !is.null(chunk_try)) return(chunk_try)
  warning(sprintf("Buffer still failing for %s; returning unbuffered geometry.", label), immediate. = FALSE)
  sv
}

prepare_layer <- function(sv, study_area, line_buffer = 30, polygon_buffer = 0) {
  if (is.null(sv)) return(NULL)
  if (!inherits(sv, "SpatVector")) {
    sv <- terra::vect(sv)
  }
  sv <- drop_empty_geoms(sv)
  if (!length(sv)) return(NULL)
  study_area <- ensure_spatvector(study_area)
  if (!terra::same.crs(sv, study_area)) {
    sv <- terra::project(sv, terra::crs(study_area))
  }
  geom_type <- tolower(terra::geomtype(sv)[1])
  buffer_dist <- if (geom_type %in% c("lines", "line")) line_buffer else polygon_buffer
  buffer_label <- sprintf("BEAD_%s", geom_type)
  if (!is.na(buffer_dist) && buffer_dist > 0) {
    sv <- safe_buffer(sv, buffer_dist, buffer_label, if (startsWith(geom_type, "line")) "line" else "poly")
  } else if (geom_type %in% c("lines", "line")) {
    sv <- safe_buffer(sv, line_buffer, buffer_label, "line")
  }
  message(sprintf("Cropping/intersecting %s layer with %d features", geom_type, length(sv)))
  sv <- suppressWarnings(terra::crop(sv, study_area))
  message("Running terra::intersect ...")
  is_poly_geom <- startsWith(geom_type, "poly")
  if (is_poly_geom) {
    sv <- clip_with_sf(sv, study_area)
  } else {
    sv_try <- try(suppressWarnings(terra::intersect(sv, study_area)), silent = TRUE)
    if (inherits(sv_try, "try-error")) {
      err_msg <- conditionMessage(attr(sv_try, "condition"))
      warning(sprintf("terra::intersect failed (%s); retrying with sf::st_intersection.", err_msg), immediate. = FALSE)
      sv <- clip_with_sf(sv, study_area)
    } else {
      sv <- sv_try
    }
  }
  message(sprintf("Finished intersection; %d features remain", if (!is.null(sv)) length(sv) else 0))
  sv <- sv[!terra::is.empty(sv)]
  if (!"Class" %in% names(sv)) {
    sv$Class <- "Unknown"
  } else {
    sv$Class <- trimws(as.character(sv$Class))
    sv$Class[is.na(sv$Class) | !nzchar(sv$Class)] <- "Unknown"
    sv$Class <- canonical_class(sv$Class)
    sv <- sv[sv$Class != "NotDisturbance", ]
  }
  sv <- tryCatch(terra::makeValid(sv), error = function(e) sv)
  sv[!terra::is.empty(sv)]
}

split_by_class <- function(sv) {
  if (is.null(sv) || !length(sv)) return(list())
  classes <- unique(sv$Class)
  setNames(lapply(classes, function(cls) {
    subset <- sv[sv$Class == cls, ]
    subset[!terra::is.empty(subset)]
  }), nm = classes)
}

subtract_geom <- function(new_geom, base_geom) {
  if (is.null(new_geom) || !length(new_geom)) return(NULL)
  if (is.null(base_geom) || !length(base_geom)) return(new_geom)
  out <- try(terra::erase(new_geom, base_geom), silent = TRUE)
  if (inherits(out, "try-error")) {
    warning("Failed to subtract baseline geometry; returning new geometry as-is.")
    return(new_geom)
  }
  out[!terra::is.empty(out)]
}

summarize_class_areas <- function(class_list) {
  if (!length(class_list)) {
    return(data.table(Class = character(), area_km2 = numeric()))
  }
  rows <- lapply(names(class_list), function(cls) {
    geom <- class_list[[cls]]
    if (is.null(geom) || !length(geom)) {
      area <- 0
    } else {
      area <- sum(terra::expanse(geom, unit = "km"), na.rm = TRUE)
    }
    data.table(Class = cls, area_km2 = ifelse(is.finite(area), area, 0))
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

load_observed_interval <- function(baseline_year, comparison_year, study_area, bead_root,
                                   line_buffer, polygon_buffer) {
  base_line <- load_bead_layer(baseline_year, "line", bead_root)
  base_poly <- load_bead_layer(baseline_year, "poly", bead_root)
  comp_line <- load_bead_layer(comparison_year, "line", bead_root)
  comp_poly <- load_bead_layer(comparison_year, "poly", bead_root)

  study_area_sv <- ensure_spatvector(study_area)
  base_line <- prepare_layer(base_line, study_area_sv, line_buffer, polygon_buffer)
  base_poly <- prepare_layer(base_poly, study_area_sv, line_buffer, polygon_buffer)
  comp_line <- prepare_layer(comp_line, study_area_sv, line_buffer, polygon_buffer)
  comp_poly <- prepare_layer(comp_poly, study_area_sv, line_buffer, polygon_buffer)

  base_combined <- list(base_line, base_poly)
  base_combined <- Filter(function(x) !is.null(x) && length(x), base_combined)
  if (length(base_combined) > 1) {
    base_geom <- do.call(rbind, base_combined)
  } else if (length(base_combined) == 1) {
    base_geom <- base_combined[[1]]
  } else {
    base_geom <- terra::vect()
  }
  comp_combined <- list(comp_line, comp_poly)
  comp_combined <- Filter(function(x) !is.null(x) && length(x), comp_combined)
  if (length(comp_combined) > 1) {
    comp_geom <- do.call(rbind, comp_combined)
  } else if (length(comp_combined) == 1) {
    comp_geom <- comp_combined[[1]]
  } else {
    comp_geom <- terra::vect()
  }

  base_by_class <- split_by_class(base_geom)
  comp_by_class <- split_by_class(comp_geom)
  class_names <- sort(unique(c(names(base_by_class), names(comp_by_class))))
  new_classes <- setNames(vector("list", length(class_names)), class_names)
  for (cls in class_names) {
    new_classes[[cls]] <- subtract_geom(comp_by_class[[cls]], base_by_class[[cls]])
  }
  areas_dt <- summarize_class_areas(new_classes)
  list(
    classes = new_classes,
    areas = areas_dt,
    label = sprintf("%d_%d", baseline_year, comparison_year)
  )
}

extract_replicate_id <- function(path) {
  base <- basename(path)
  m <- regexec("([0-9]+)$", base)
  reg <- regmatches(base, m)
  if (length(reg) && length(reg[[1]]) >= 2) {
    val <- suppressWarnings(as.integer(reg[[1]][2]))
    if (!is.na(val)) return(val)
  }
  parts <- unlist(strsplit(base, "_"))
  candidate <- suppressWarnings(as.integer(tail(parts, 1)))
  if (!is.na(candidate)) return(candidate)
  1L
}

fallback_class_name <- function(dir_name) {
  parts <- strsplit(dir_name, "_")[[1]]
  if (length(parts) >= 2) {
    parts[[length(parts) - 1]]
  } else {
    dir_name
  }
}

prepare_simulated_geoms <- function(sim_root, replicates, study_area,
                                    line_buffer, polygon_buffer) {
  sim_root <- normalize_existing_path(sim_root, mustExist = TRUE)
  shp_files <- list.files(sim_root, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  shp_files <- shp_files[startsWith(basename(shp_files), "disturbances_")]
  if (!length(shp_files)) {
    stop("No disturbance shapefiles were found under ", sim_root, call. = FALSE)
  }
  study_area_sv <- ensure_spatvector(study_area)
  rep_lists <- setNames(vector("list", length(replicates)), replicates)
  area_rows <- list()

  infer_class_from_name <- function(path) {
    base <- basename(path)
    m <- regexec("^disturbances_[^_]+_([^_]+)_", base)
    reg <- regmatches(base, m)
    if (length(reg) && length(reg[[1]]) >= 2) return(reg[[1]][2])
    fallback_class_name(dirname(path))
  }

  for (shp in shp_files) {
    message(sprintf("Preparing simulated shapefile: %s", shp))
    dir_name <- basename(dirname(shp))
    rep_id <- extract_replicate_id(dirname(shp))
    if (!rep_id %in% replicates) next
    sv <- tryCatch(terra::vect(shp), error = function(e) NULL)
    if (is.null(sv) || !length(sv)) next
    sv <- drop_empty_geoms(sv)
    if (!length(sv)) next
    geom_type <- tolower(terra::geomtype(sv)[1])
    geom_kind <- if (startsWith(geom_type, "line")) "line" else "poly"
    if (geom_kind == "line") {
      # Fully sf-based buffering and clipping to avoid GEOS vector asserts on large line sets
      sv <- process_lines_sf(sv, study_area_sv, line_buffer, basename(shp))
      sv <- ensure_spatvector(sv)
    } else {
      sv <- tryCatch(
        repair_bead_geom(sv, basename(shp), geom_kind),
        error = function(e) {
          warning(sprintf("Failed to repair simulated shapefile %s prior to buffering (%s); continuing with original geometry.", shp, conditionMessage(e)), immediate. = FALSE)
          sv
        }
      )
      buffer_dist <- if (geom_type %in% c("lines", "line")) line_buffer else polygon_buffer
      buffer_label <- basename(shp)
      if (!is.na(buffer_dist) && buffer_dist > 0) {
        sv <- safe_buffer(sv, buffer_dist, buffer_label, geom_kind)
      } else if (geom_type %in% c("lines", "line")) {
        sv <- safe_buffer(sv, line_buffer, buffer_label, "line")
      }
      if (!terra::same.crs(sv, study_area_sv)) {
        sv <- terra::project(sv, terra::crs(study_area_sv))
      }
      sv_orig <- sv
      sv_crop <- try(suppressWarnings(terra::crop(sv, study_area_sv)), silent = TRUE)
      need_intersection <- TRUE
      if (inherits(sv_crop, "try-error")) {
        err_msg <- conditionMessage(attr(sv_crop, "condition"))
        warning(sprintf("terra::crop failed for simulated layer %s (%s); attempting geometry repair.", shp, err_msg), immediate. = FALSE)
        repaired <- try(suppressWarnings(repair_bead_geom(sv_orig, basename(shp), geom_kind)), silent = TRUE)
        if (!inherits(repaired, "try-error") && inherits(repaired, "SpatVector") && length(repaired)) {
          sv <- repaired
          sv_crop <- try(suppressWarnings(terra::crop(sv, study_area_sv)), silent = TRUE)
        }
        if (inherits(sv_crop, "try-error")) {
          warning(sprintf("terra::crop still failing for %s; retrying with sf::st_intersection.", shp), immediate. = FALSE)
          sv <- clip_with_sf(sv, study_area_sv)
          need_intersection <- FALSE
        } else {
          sv <- sv_crop
        }
      } else {
        sv <- sv_crop
      }
      if (need_intersection) {
        sv_try <- try(suppressWarnings(terra::intersect(sv, study_area_sv)), silent = TRUE)
        if (inherits(sv_try, "try-error")) {
          err_msg <- conditionMessage(attr(sv_try, "condition"))
          warning(sprintf("terra::intersect failed for simulated layer %s (%s); attempting geometry repair.", shp, err_msg), immediate. = FALSE)
          repaired <- try(suppressWarnings(repair_bead_geom(sv, basename(shp), geom_kind)), silent = TRUE)
          if (!inherits(repaired, "try-error") && inherits(repaired, "SpatVector") && length(repaired)) {
            sv <- repaired
            sv_try <- try(suppressWarnings(terra::intersect(sv, study_area_sv)), silent = TRUE)
          }
          if (inherits(sv_try, "try-error")) {
            warning(sprintf("terra::intersect still failing for %s; retrying with sf::st_intersection.", shp), immediate. = FALSE)
            sv <- clip_with_sf(sv, study_area_sv)
          } else {
            sv <- sv_try
          }
        } else {
          sv <- sv_try
        }
      }
    }
    if (!"Class" %in% names(sv)) {
      sv$Class <- infer_class_from_name(shp)
    } else {
      sv$Class <- trimws(as.character(sv$Class))
      sv$Class[is.na(sv$Class) | !nzchar(sv$Class)] <- infer_class_from_name(shp)
    }
    sv$Class <- canonical_class(sv$Class)
    # Drop non-disturbance / potential-only classes from validation
    sv <- sv[!(sv$Class %in% c("NotDisturbance", "hydroPotential", "ITI")), ]
    sv <- sv[!terra::is.empty(sv)]
    if (!length(sv)) next
    sv <- tryCatch(terra::makeValid(sv), error = function(e) sv)
    by_class <- split_by_class(sv)
    if (!length(by_class)) next
    if (is.null(rep_lists[[as.character(rep_id)]])) {
      rep_lists[[as.character(rep_id)]] <- list()
    }
    for (cls in names(by_class)) {
      geom <- by_class[[cls]]
      if (is.null(rep_lists[[as.character(rep_id)]][[cls]])) {
        rep_lists[[as.character(rep_id)]][[cls]] <- geom
      } else {
        rep_lists[[as.character(rep_id)]][[cls]] <- rbind(
          rep_lists[[as.character(rep_id)]][[cls]],
          geom
        )
      }
      area_val <- sum(terra::expanse(geom, unit = "km"), na.rm = TRUE)
      area_rows[[length(area_rows) + 1L]] <- data.table(
        replicate = rep_id,
        Class = cls,
        sim_area_km2 = ifelse(is.finite(area_val), area_val, 0)
      )
    }
  }
  area_dt <- if (length(area_rows)) data.table::rbindlist(area_rows, use.names = TRUE) else data.table()
  list(geoms = rep_lists, areas = area_dt)
}

make_template_raster <- function(study_area, resolution) {
  sa <- ensure_spatvector(study_area)
  r <- terra::rast(extent = terra::ext(sa), resolution = resolution, crs = terra::crs(sa))
  r[] <- 0
  terra::mask(r, sa)
}

make_class_raster <- function(class_list, template, class_ids) {
  r <- template
  r[] <- 0
  for (cls in names(class_list)) {
    geom <- class_list[[cls]]
    if (is.null(geom) || !length(geom)) next
    geom$class_id <- class_ids[[cls]]
    tmp <- terra::rasterize(geom, template, field = "class_id", touches = TRUE, background = NA_real_)
    idx <- !is.na(tmp[])
    if (any(idx, na.rm = TRUE)) {
      vals <- tmp[idx]
      r[idx] <- vals
    }
  }
  r
}

compute_confusion <- function(reference_raster, comparison_raster, class_ids) {
  ref_vals <- terra::values(reference_raster, mat = FALSE)
  cmp_vals <- terra::values(comparison_raster, mat = FALSE)
  valid <- !is.na(ref_vals) & !is.na(cmp_vals)
  ref_vals <- ref_vals[valid]
  cmp_vals <- cmp_vals[valid]
  levels <- c(0, unname(class_ids))
  tab <- table(
    factor(ref_vals, levels = levels),
    factor(cmp_vals, levels = levels)
  )
  tab
}

map_id_to_class <- function(class_ids) {
  c(`0` = "background", setNames(names(class_ids), class_ids))
}

summarize_disagreement <- function(tab) {
  N <- sum(tab)
  diag_sum <- sum(diag(tab))
  overall <- if (N > 0) diag_sum / N else NA_real_
  row_tot <- rowSums(tab)
  col_tot <- colSums(tab)
  quantity <- if (N > 0) 0.5 * sum(abs(row_tot - col_tot)) / N else NA_real_
  total_disagreement <- if (is.na(overall)) NA_real_ else 1 - overall
  allocation <- if (is.na(total_disagreement) || is.na(quantity)) NA_real_ else total_disagreement - quantity
  if (!is.na(allocation) && allocation < 0) allocation <- 0
  list(
    overall = overall,
    quantity = quantity,
    allocation = allocation,
    total = total_disagreement
  )
}

write_quantity_tables <- function(interval_label, years, study_area_km2,
                                  observed_dt, sim_dt, output_dir, analysis_mode) {
  sim_summary <- if (nrow(sim_dt)) {
    sim_dt[, .(
      sim_area_mean_km2 = mean(sim_area_km2, na.rm = TRUE),
      sim_area_sd_km2 = if (.N > 1) stats::sd(sim_area_km2, na.rm = TRUE) else 0
    ), by = Class]
  } else {
    data.table(Class = character(), sim_area_mean_km2 = numeric(), sim_area_sd_km2 = numeric())
  }
  combined <- merge(observed_dt, sim_summary, by = "Class", all = TRUE)
  combined[is.na(area_km2), area_km2 := 0]
  combined[is.na(sim_area_mean_km2), sim_area_mean_km2 := 0]
  combined[is.na(sim_area_sd_km2), sim_area_sd_km2 := 0]
  combined[, observed_rate_km2_per_year := if (years > 0) area_km2 / years else NA_real_]
  combined[, simulated_rate_km2_per_year := if (years > 0) sim_area_mean_km2 / years else NA_real_]
  combined[, observed_rate_pct_per_year := if (study_area_km2 > 0 && years > 0) {
    (area_km2 / (study_area_km2 * years)) * 100
  } else {
    NA_real_
  }]
  combined[, simulated_rate_pct_per_year := if (study_area_km2 > 0 && years > 0) {
    (sim_area_mean_km2 / (study_area_km2 * years)) * 100
  } else {
    NA_real_
  }]
  combined[, bias_pct_per_year := simulated_rate_pct_per_year - observed_rate_pct_per_year]
  combined[, bias_km2_per_year := simulated_rate_km2_per_year - observed_rate_km2_per_year]
  combined[, analysis_mode := analysis_mode]
  data.table::setorder(combined, Class)
  data.table::fwrite(combined, file = file.path(output_dir, "quantity_metrics.csv"))

  rmse_pct <- if (nrow(combined)) {
    diffs <- combined$simulated_rate_pct_per_year - combined$observed_rate_pct_per_year
    diffs <- diffs[is.finite(diffs)]
    if (length(diffs)) sqrt(mean(diffs^2)) else NA_real_
  } else {
    NA_real_
  }
  global_bias <- if (nrow(combined)) {
    diffs <- combined$simulated_rate_pct_per_year - combined$observed_rate_pct_per_year
    diffs <- diffs[is.finite(diffs)]
    if (length(diffs)) mean(diffs) else NA_real_
  } else {
    NA_real_
  }
  total_observed <- sum(combined$area_km2, na.rm = TRUE)
  total_sim <- sum(combined$sim_area_mean_km2, na.rm = TRUE)
  summary_dt <- data.table(
    interval = interval_label,
    years = years,
    study_area_km2 = study_area_km2,
    total_observed_area_km2 = total_observed,
    total_simulated_area_km2 = total_sim,
    overall_bias_pct_per_year = global_bias,
    rmse_pct_per_year = rmse_pct,
    analysis_mode = analysis_mode
  )
  data.table::fwrite(summary_dt, file = file.path(output_dir, "quantity_summary.csv"))
  combined
}

build_dataclass_mapping <- function() {
  csv_path <- file.path(project_root, "modules", "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
  if (!file.exists(csv_path)) {
    return(data.table(Class = character(), dataClass = character()))
  }
  dt <- data.table::fread(csv_path)
  map_dt <- unique(dt[nzchar(classToSearch), .(Class = classToSearch, dataClass)])
  map_dt <- map_dt[!grepl("^potential", dataClass, ignore.case = TRUE)]
  if (!nrow(map_dt)) return(map_dt)
  map_dt <- map_dt[order(Class, dataClass)]
  map_dt <- map_dt[, .(dataClass = dataClass[1]), by = Class]
  map_dt
}

write_dataclass_metrics <- function(interval_label, combined_dt, output_dir, analysis_mode) {
  if (is.null(combined_dt) || !nrow(combined_dt)) return(invisible(NULL))
  map_dt <- build_dataclass_mapping()
  if (!nrow(map_dt)) return(invisible(NULL))
  normalize_class <- function(x) tolower(gsub("[^a-z0-9]", "", x))
  map_dt[, Class_norm := normalize_class(Class)]
  map_dt[, dataClass_norm := normalize_class(dataClass)]
  combined_dt[, Class_norm := normalize_class(Class)]
  joined <- merge(
    combined_dt,
    map_dt[, .(Class_norm, dataClass, dataClass_norm)],
    by = "Class_norm",
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  joined[is.na(dataClass), dataClass := Class]
  joined[is.na(dataClass_norm), dataClass_norm := normalize_class(dataClass)]
  if (!nrow(joined)) return(invisible(NULL))
  agg <- joined[, .(
    observed_rate_pct_per_year = sum(observed_rate_pct_per_year, na.rm = TRUE),
    simulated_rate_pct_per_year = sum(simulated_rate_pct_per_year, na.rm = TRUE)
  ), by = dataClass][order(dataClass)]
  agg[, analysis_mode := analysis_mode]
  data.table::fwrite(
    agg,
    file = file.path(output_dir, "quantity_metrics_by_dataClass.csv")
  )
  invisible(NULL)
}

write_confusion_outputs <- function(interval_label, replicate_results, output_dir, analysis_mode) {
  if (!length(replicate_results)) return(invisible(NULL))
  confusion_rows <- list()
  disagreement_rows <- list()
  for (entry in replicate_results) {
    rep_id <- entry$replicate
    tab <- entry$table
    mapping <- entry$mapping
    df <- as.data.frame(tab)
    names(df) <- c("reference_id", "comparison_id", "count")
    df$replicate <- rep_id
    df$interval <- interval_label
    df$analysis_mode <- analysis_mode
    df$reference <- mapping[as.character(df$reference_id)]
    df$comparison <- mapping[as.character(df$comparison_id)]
    confusion_rows[[length(confusion_rows) + 1L]] <- df
    disag <- summarize_disagreement(tab)
    disagreement_rows[[length(disagreement_rows) + 1L]] <- data.table(
      interval = interval_label,
      replicate = rep_id,
      overall_agreement = disag$overall,
      quantity_disagreement = disag$quantity,
      allocation_disagreement = disag$allocation,
      total_disagreement = disag$total,
      analysis_mode = analysis_mode
    )
  }
  confusion_dt <- data.table::rbindlist(confusion_rows, use.names = TRUE, fill = TRUE)
  data.table::fwrite(confusion_dt, file = file.path(output_dir, "confusion_matrix.csv"))
  disagreement_dt <- data.table::rbindlist(disagreement_rows, use.names = TRUE, fill = TRUE)
  data.table::fwrite(disagreement_dt, file = file.path(output_dir, "disagreement.csv"))
}

run_interval <- function(interval, opts, study_area_sv, study_area_km2) {
  baseline_year <- interval[1]
  comparison_year <- interval[2]
  if (comparison_year <= baseline_year) {
    stop("Comparison year must be greater than baseline year.", call. = FALSE)
  }
  message(sprintf("Processing interval %d-%d ...", baseline_year, comparison_year))
  observed <- load_observed_interval(
    baseline_year = baseline_year,
    comparison_year = comparison_year,
    study_area = study_area_sv,
    bead_root = opts$bead_root,
    line_buffer = opts$line_buffer,
    polygon_buffer = opts$polygon_buffer
  )
  sim <- prepare_simulated_geoms(
    sim_root = opts$simulation_root,
    replicates = opts$replicates,
    study_area = study_area_sv,
    line_buffer = opts$line_buffer,
    polygon_buffer = opts$polygon_buffer
  )
  analysis_mode <- opts$analysis_mode
  interval_label <- observed$label
  interval_dir <- file.path(opts$output_root, interval_label)
  dir.create(interval_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "Analysis mode:",
      paste0(" - ", opts$analysis_mode),
      "Notes:",
      " - adqd_holdout: single simulated increment 2016-2020 applied to BEAD 2015→2020.",
      " - adqd_verification: simulated increment 2010→2015 aligned to BEAD 2010→2015."
    ),
    con = file.path(interval_dir, "assumption.txt")
  )
  years <- comparison_year - baseline_year
  combined_dt <- write_quantity_tables(
    interval_label = interval_label,
    years = years,
    study_area_km2 = study_area_km2,
    observed_dt = observed$areas,
    sim_dt = sim$areas,
    output_dir = interval_dir,
    analysis_mode = analysis_mode
  )
  write_dataclass_metrics(interval_label, combined_dt, interval_dir, analysis_mode)
  template <- make_template_raster(study_area_sv, opts$raster_resolution)
  class_names <- sort(unique(c(names(observed$classes), unlist(lapply(sim$geoms, names)))))
  class_names <- class_names[nzchar(class_names)]
  class_ids <- setNames(seq_along(class_names), class_names)
  obs_raster <- make_class_raster(observed$classes, template, class_ids)
  replicate_results <- list()
  for (rep_id in names(sim$geoms)) {
    class_list <- sim$geoms[[rep_id]]
    if (is.null(class_list) || !length(class_list)) next
    sim_raster <- make_class_raster(class_list, template, class_ids)
    tab <- compute_confusion(obs_raster, sim_raster, class_ids)
    replicate_results[[length(replicate_results) + 1L]] <- list(
      replicate = as.integer(rep_id),
      table = tab,
      mapping = map_id_to_class(class_ids)
    )
  }
  if (length(replicate_results)) {
    write_confusion_outputs(interval_label, replicate_results, interval_dir, analysis_mode)
  } else {
    warning("No simulated geometries matched the requested replicates; skipping map comparison.")
  }
  message(sprintf("Interval %s metrics written to %s", interval_label, interval_dir))
}

main <- function() {
  opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    print_usage()
    quit(save = "no", status = 0, runLast = FALSE)
  }
  if (isTRUE(opts$caribou_buffer)) {
    opts$line_buffer <- 500
    opts$polygon_buffer <- max(opts$polygon_buffer, 500)
  }
  opts$simulation_root <- normalize_existing_path(opts$simulation_root, mustExist = TRUE)
  opts$output_root <- normalize_existing_path(opts$output_root, mustExist = FALSE)
  opts$bead_root <- normalize_existing_path(opts$bead_root, mustExist = TRUE)
  opts$study_area <- normalize_existing_path(opts$study_area, mustExist = TRUE)
  opts$replicates <- sort(unique(as.integer(opts$replicates)))
  dir.create(opts$output_root, recursive = TRUE, showWarnings = FALSE)
  study_area_sv <- ensure_spatvector(opts$study_area)
  study_area_km2 <- sum(terra::expanse(study_area_sv, unit = "km"), na.rm = TRUE)
  intervals <- opts$intervals
  if (!length(intervals)) {
    stop("At least one baseline/comparison interval must be provided.", call. = FALSE)
  }
  for (interval in intervals) {
    run_interval(interval, opts, study_area_sv, study_area_km2)
  }
}

main()
