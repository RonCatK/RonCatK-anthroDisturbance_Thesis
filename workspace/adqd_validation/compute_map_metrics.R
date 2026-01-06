#!/usr/bin/env Rscript
# Computes BEAD vs simulation confusion, quantity, and disagreement metrics for AD/QD runs.
# How simulated years are selected:
# - Filenames: disturbances_<YEAR>_<CLASS>[_rep<REP>].shp (legacy patterns supported).
# - Default inclusion rule: increment (year > baseline && year <= comparison).
# - Override: --year-rule=exact or --no-year-filter.

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
})

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
default_analysis_mode <- "adqd_holdout"
default_output_root <- file.path(project_root, "outputs", "adqd_validation", "results", default_analysis_mode)

parse_cli_args <- function(args) {
  opts <- list(
    simulation_root = file.path(project_root, "outputs", "adqd_validation", "ADQD_HOLDOUT"),
    output_root = default_output_root,
    bead_root = file.path(project_root, "data", "raw", "ECCC"),
    study_area = file.path(project_root, "data", "study_area", "NWT_boundary.shp"),
    intervals = list(c(2015L, 2020L)),
    replicates = 1:5,
    line_buffer = 30,
    polygon_buffer = 0,
    raster_resolution = 15,
    analysis_mode = default_analysis_mode,
    caribou_buffer = FALSE,
    skip_buffering = FALSE,
    no_year_filter = FALSE,
    year_rule = "increment",
    overlap_rule = "priority",
    help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$help <- TRUE
    } else if (identical(arg, "--caribou-buffer") || identical(arg, "--caribou")) {
      opts$caribou_buffer <- TRUE
    } else if (identical(arg, "--skip-buffering")) {
      opts$skip_buffering <- TRUE
    } else if (identical(arg, "--no-year-filter")) {
      opts$no_year_filter <- TRUE
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
    } else if (grepl("^--year-rule=", arg, ignore.case = TRUE)) {
      val <- trimws(sub("^--year-rule=", "", arg, ignore.case = TRUE))
      if (nzchar(val)) opts$year_rule <- tolower(val)
    } else if (grepl("^--overlap-rule=", arg, ignore.case = TRUE)) {
      val <- trimws(sub("^--overlap-rule=", "", arg, ignore.case = TRUE))
      if (nzchar(val)) opts$overlap_rule <- tolower(val)
    }
  }
  opts
}

print_usage <- function() {
  cat(paste0(
    "Usage: Rscript workspace/adqd_validation/compute_map_metrics.R [options]\n",
    "  --simulation-root=DIR   Path to AD/QD run outputs (default outputs/adqd_validation/ADQD_HOLDOUT).\n",
    "  --output-root=DIR       Directory to store metric tables (default outputs/adqd_validation/results/<analysis_mode>).\n",
    "  --bead-root=DIR         Folder with BEAD data archives (default data/raw/ECCC).\n",
    "  --study-area=FILE       Study area polygon used for clipping (default data/study_area/NWT_boundary.shp).\n",
    "  --intervals=a:b,c:d     Comma-separated baseline:comparison year pairs (default 2015:2020).\n",
    "  --replicates=list       Replicate IDs to analyse (default 1:5).\n",
    "  --line-buffer=VALUE     Buffer (m) applied to line features before calculating areas (default 30).\n",
    "  --polygon-buffer=VALUE  Optional buffer (m) applied to polygons (default 0).\n",
    "  --resolution=VALUE      Raster resolution in metres for map comparison (default 15).\n",
    "  --caribou-buffer        Use 500 m buffers for all disturbance geometries (caribou mode).\n",
    "  --caribou               Alias for --caribou-buffer.\n",
    "  --skip-buffering        Disable buffering (line/polygon buffers set to 0).\n",
    "  --no-year-filter        Disable simulated year filtering (legacy behavior).\n",
    "  --analysis-mode=LABEL   Tag written to outputs (default adqd_holdout).\n",
    "  --year-rule=RULE        Simulated year rule: increment or exact (default increment).\n",
    "  --overlap-rule=RULE     Overlap handling: last_wins, first_wins, priority (default priority).\n",
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

combine_spatvectors <- function(vectors) {
  vectors <- Filter(function(x) !is.null(x) && length(x), vectors)
  vectors <- lapply(vectors, function(x) x[!terra::is.empty(x)])
  vectors <- Filter(function(x) !is.null(x) && length(x), vectors)
  if (!length(vectors)) return(NULL)
  out <- vectors[[1]]
  if (length(vectors) > 1) {
    for (idx in 2:length(vectors)) {
      out <- tryCatch(
        rbind(out, vectors[[idx]]),
        error = function(e) {
          warning(sprintf("Failed to rbind SpatVector %d (%s); skipping.", idx, conditionMessage(e)), immediate. = FALSE)
          out
        }
      )
    }
  }
  out
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

get_class_order <- function() {
  c(
    "Seismic",
    "Road",
    "Pipeline",
    "Oil/Gas",
    "Well site",
    "OilGas_Total",
    "Cutblock",
    "Harvest",
    "Mine",
    "Settlement",
    "Agriculture",
    "OtherAnthro",
    "Unknown"
  )
}

build_class_ids <- function(class_names, class_order) {
  class_names <- class_names[!is.na(class_names) & nzchar(class_names)]
  ordered <- unique(c(class_order, sort(setdiff(class_names, class_order))))
  ordered <- ordered[nzchar(ordered)]
  setNames(seq_along(ordered), ordered)
}

build_evaluation_crosswalk <- function() {
  list(
    mapping = list(
      OilGas_Total = c("Oil/Gas", "Well site"),
      Seismic = "Seismic",
      Road = "Road",
      Pipeline = "Pipeline",
      Cutblock = "Cutblock",
      Mine = "Mine",
      Settlement = "Settlement"
    ),
    out_of_scope = c("Agriculture", "Harvest")
  )
}

map_class_to_crosswalk <- function(class_name, crosswalk) {
  if (is.null(class_name) || is.na(class_name)) return(NA_character_)
  mapping <- crosswalk$mapping
  out_of_scope <- crosswalk$out_of_scope
  if (!is.null(out_of_scope) && class_name %in% out_of_scope) return(NA_character_)
  hits <- names(mapping)[vapply(mapping, function(x) class_name %in% x, logical(1))]
  if (length(hits)) return(hits[[1]])
  "OtherAnthro"
}

apply_crosswalk_to_class_list <- function(class_list, crosswalk) {
  if (!length(class_list)) return(list())
  out <- list()
  for (cls in names(class_list)) {
    target <- map_class_to_crosswalk(cls, crosswalk)
    if (is.na(target) || !nzchar(target)) next
    geom <- class_list[[cls]]
    if (is.null(geom) || !length(geom)) next
    if (is.null(out[[target]])) {
      out[[target]] <- geom
    } else {
      out[[target]] <- rbind(out[[target]], geom)
    }
  }
  out
}

apply_crosswalk_to_area_dt <- function(area_dt, crosswalk, value_col, group_cols = NULL) {
  if (is.null(area_dt) || !nrow(area_dt)) return(data.table())
  dt <- data.table::copy(area_dt)
  if (!"Class" %in% names(dt)) {
    stop("Crosswalk expects a Class column in the area table.", call. = FALSE)
  }
  dt[, Class_target := vapply(Class, map_class_to_crosswalk, character(1), crosswalk = crosswalk)]
  dt <- dt[!is.na(Class_target) & nzchar(Class_target)]
  if (!nrow(dt)) return(data.table())
  group_cols <- unique(c(group_cols, "Class_target"))
  dt <- dt[, .(value = sum(get(value_col), na.rm = TRUE)), by = group_cols]
  data.table::setnames(dt, "Class_target", "Class")
  data.table::setnames(dt, "value", value_col)
  dt
}

write_crosswalk_file <- function(crosswalk, output_dir) {
  mapping <- crosswalk$mapping
  out_of_scope <- crosswalk$out_of_scope
  lines <- c("mapping:")
  for (nm in names(mapping)) {
    members <- mapping[[nm]]
    if (!length(members)) {
      lines <- c(lines, sprintf("  %s: []", nm))
    } else {
      lines <- c(lines, sprintf("  %s:", nm))
      lines <- c(lines, sprintf("    - %s", members))
    }
  }
  if (!is.null(out_of_scope) && length(out_of_scope)) {
    lines <- c(lines, "out_of_scope:")
    lines <- c(lines, sprintf("  - %s", out_of_scope))
  } else {
    lines <- c(lines, "out_of_scope: []")
  }
  lines <- c(lines, "unmapped_to: OtherAnthro")
  writeLines(lines, con = file.path(output_dir, "crosswalk_used.yaml"))
  invisible(NULL)
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

summarize_class_gross_areas <- function(class_list) {
  if (!length(class_list)) {
    return(data.table(Class = character(), gross_area_overlap_inflated_km2 = numeric()))
  }
  rows <- lapply(names(class_list), function(cls) {
    geom <- class_list[[cls]]
    if (is.null(geom) || !length(geom)) {
      area <- 0
    } else {
      area <- sum(terra::expanse(geom, unit = "km"), na.rm = TRUE)
    }
    data.table(Class = cls, gross_area_overlap_inflated_km2 = ifelse(is.finite(area), area, 0))
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
  gross_dt <- summarize_class_gross_areas(new_classes)
  list(
    classes = new_classes,
    gross_areas = gross_dt,
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

parse_simulated_file_metadata <- function(sim_root) {
  sim_root <- normalize_existing_path(sim_root, mustExist = TRUE)
  shp_files <- list.files(sim_root, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  shp_files <- shp_files[startsWith(basename(shp_files), "disturbances_")]
  if (!length(shp_files)) {
    warning("No disturbance shapefiles were found under ", sim_root, immediate. = FALSE)
    return(data.table())
  }
  parse_one <- function(path) {
    base_name <- tools::file_path_sans_ext(basename(path))
    rep_val <- NA_integer_
    rep_match <- regexec("_rep([0-9]+)", base_name, ignore.case = TRUE)
    rep_reg <- regmatches(base_name, rep_match)
    if (length(rep_reg) && length(rep_reg[[1]]) >= 2) {
      rep_val <- suppressWarnings(as.integer(rep_reg[[1]][2]))
    }
    if (is.na(rep_val)) {
      rep_val <- extract_replicate_id(dirname(path))
    }
    year_val <- NA_integer_
    class_val <- NA_character_
    parse_ok <- TRUE
    notes <- character()
    m1 <- regexec("^disturbances_([0-9]{4})_([A-Za-z0-9_]+?)(?:_rep([0-9]+))?$", base_name)
    reg1 <- regmatches(base_name, m1)
    if (length(reg1) && length(reg1[[1]]) >= 3) {
      year_val <- suppressWarnings(as.integer(reg1[[1]][2]))
      class_val <- reg1[[1]][3]
      if (length(reg1[[1]]) >= 4 && nzchar(reg1[[1]][4])) {
        rep_val <- suppressWarnings(as.integer(reg1[[1]][4]))
      }
      notes <- c(notes, "pattern_year_class")
    } else {
      rem <- sub("^disturbances_", "", base_name)
      parts <- strsplit(rem, "_")[[1]]
      year_idx <- which(grepl("^(19|20)[0-9]{2}$", parts))
      if (length(year_idx)) {
        idx <- tail(year_idx, 1)
        year_val <- suppressWarnings(as.integer(parts[[idx]]))
        if (idx > 1) {
          class_val <- parts[[idx - 1]]
        }
        if (length(year_idx) > 1) {
          notes <- c(notes, "multiple_year_tokens")
        }
        notes <- c(notes, "class_from_token_before_year")
      } else if (length(parts) >= 2) {
        class_val <- parts[[2]]
        notes <- c(notes, "class_from_second_token_no_year")
      }
    }
    if (is.na(year_val)) {
      notes <- c(notes, "year_missing")
    }
    if (is.null(class_val) || is.na(class_val) || !nzchar(class_val)) {
      parse_ok <- FALSE
      notes <- c(notes, "class_parse_failed")
    }
    class_val <- canonical_class(class_val)
    data.table(
      path = path,
      base_name = base_name,
      year = year_val,
      Class = class_val,
      replicate = rep_val,
      parse_ok = parse_ok,
      parse_note = paste(notes, collapse = ";")
    )
  }
  dt <- data.table::rbindlist(lapply(shp_files, parse_one), use.names = TRUE, fill = TRUE)
  if (nrow(dt)) {
    bad <- dt[parse_ok == FALSE]
    if (nrow(bad)) {
      warning(sprintf("Parsed %d simulated files; excluding %d with missing/invalid class.", nrow(dt), nrow(bad)), immediate. = FALSE)
    }
    missing_year <- dt[is.na(year)]
    if (nrow(missing_year)) {
      warning(sprintf("Parsed %d simulated files; %d missing year tokens (excluded unless --no-year-filter).", nrow(dt), nrow(missing_year)), immediate. = FALSE)
    }
  }
  dt
}

apply_year_rule <- function(years, baseline_year, comparison_year, rule) {
  rule <- match.arg(rule, c("increment", "exact"))
  if (rule == "increment") {
    years > baseline_year & years <= comparison_year
  } else {
    years == comparison_year
  }
}

build_sim_file_index <- function(file_dt, baseline_year, comparison_year, year_rule, no_year_filter,
                                 replicates, class_mode, class_filter = NULL, class_mapper = NULL) {
  if (is.null(class_mapper)) {
    class_mapper <- function(x) x
  }
  dt <- data.table::copy(file_dt)
  if (!nrow(dt)) return(dt)
  dt[, class_target := vapply(Class, class_mapper, character(1))]
  dt[, year_ok := !is.na(year)]
  if (!isTRUE(no_year_filter)) {
  dt[year_ok == FALSE, parse_ok := FALSE]
  }
  if (isTRUE(no_year_filter)) {
    dt[, selected_year := TRUE]
  } else {
    dt[, selected_year := apply_year_rule(year, baseline_year, comparison_year, year_rule)]
    dt[is.na(selected_year), selected_year := FALSE]
  }
  dt[, selected_rep := replicate %in% replicates]
  if (is.null(class_filter)) {
    dt[, selected_class := !is.na(class_target) & nzchar(class_target)]
  } else {
    dt[, selected_class := !is.na(class_target) & class_target %in% class_filter]
  }
  dt[, selected := parse_ok & selected_year & selected_rep & selected_class]
  dt[, `:=`(
    baseline_year = baseline_year,
    comparison_year = comparison_year,
    year_rule = year_rule,
    no_year_filter = isTRUE(no_year_filter),
    class_mode = class_mode
  )]
  dt
}

prepare_simulated_geoms <- function(sim_files, study_area,
                                    line_buffer, polygon_buffer) {
  if (is.null(sim_files) || !nrow(sim_files)) {
    stop("No simulated files provided to prepare_simulated_geoms.", call. = FALSE)
  }
  required_cols <- c("path", "Class", "replicate")
  missing <- setdiff(required_cols, names(sim_files))
  if (length(missing)) {
    stop("Simulated file index missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  sim_files <- sim_files[!is.na(Class) & nzchar(Class)]
  if (!nrow(sim_files)) {
    stop("No simulated files with valid class names were selected.", call. = FALSE)
  }
  study_area_sv <- ensure_spatvector(study_area)
  rep_ids <- sort(unique(sim_files$replicate))
  rep_lists <- setNames(vector("list", length(rep_ids)), rep_ids)
  area_rows <- list()
  buffer_warnings <- character()
  is_line_like_layer <- function(path) {
    tag <- tolower(basename(path))
    grepl("(seismic|pipeline|road|roads|powerline|powerlines|otherlines)", tag)
  }

  for (row_idx in seq_len(nrow(sim_files))) {
    shp <- sim_files$path[[row_idx]]
    cls <- sim_files$Class[[row_idx]]
    rep_id <- sim_files$replicate[[row_idx]]
    if (is.na(rep_id)) next
    if (!file.exists(shp)) {
      warning(sprintf("Simulated shapefile not found: %s", shp), immediate. = FALSE)
      next
    }
    message(sprintf("Preparing simulated shapefile: %s", shp))
    sv <- tryCatch(terra::vect(shp), error = function(e) NULL)
    if (is.null(sv) || !length(sv)) next
    sv <- drop_empty_geoms(sv)
    if (!length(sv)) next
    geom_type <- tolower(terra::geomtype(sv)[1])
    if (geom_type %in% c("polygons", "polygon") && is_line_like_layer(shp) && !is.na(polygon_buffer) && polygon_buffer > 0) {
      msg <- sprintf("Layer %s looks line-like but is polygonal; polygon_buffer=%s may double-buffer.", basename(shp), polygon_buffer)
      warning(msg, immediate. = FALSE)
      buffer_warnings <- c(buffer_warnings, msg)
    }
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
    cls <- canonical_class(cls)
    sv$Class <- cls
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
    for (class_name in names(by_class)) {
      geom <- by_class[[class_name]]
      if (is.null(rep_lists[[as.character(rep_id)]][[class_name]])) {
        rep_lists[[as.character(rep_id)]][[class_name]] <- geom
      } else {
        rep_lists[[as.character(rep_id)]][[class_name]] <- rbind(
          rep_lists[[as.character(rep_id)]][[class_name]],
          geom
        )
      }
      area_val <- sum(terra::expanse(geom, unit = "km"), na.rm = TRUE)
      area_rows[[length(area_rows) + 1L]] <- data.table(
        replicate = rep_id,
        Class = class_name,
        gross_area_overlap_inflated_km2 = ifelse(is.finite(area_val), area_val, 0)
      )
    }
  }
  area_dt <- if (length(area_rows)) data.table::rbindlist(area_rows, use.names = TRUE) else data.table()
  list(geoms = rep_lists, gross_areas = area_dt, warnings = buffer_warnings)
}

make_template_raster <- function(study_area, resolution) {
  sa <- ensure_spatvector(study_area)
  r <- terra::rast(extent = terra::ext(sa), resolution = resolution, crs = terra::crs(sa))
  r[] <- 0
  terra::mask(r, sa)
}

write_study_area_sanity <- function(template, study_area_km2, output_dir, analysis_mode, interval_label,
                                    raster_resolution) {
  res_val <- terra::res(template)
  ncell_total <- terra::ncell(template)
  vals <- terra::values(template, mat = FALSE)
  in_mask <- sum(!is.na(vals))
  mask_na_count <- sum(is.na(vals))
  cell_area_km2 <- prod(res_val) / 1e6
  derived_area_km2 <- in_mask * cell_area_km2
  area_diff_km2 <- derived_area_km2 - study_area_km2
  area_diff_pct <- if (study_area_km2 > 0) (area_diff_km2 / study_area_km2) * 100 else NA_real_
  sanity_dt <- data.table(
    interval = interval_label,
    analysis_mode = analysis_mode,
    raster_resolution_m = raster_resolution,
    res_x_m = res_val[1],
    res_y_m = res_val[2],
    ncell_total = ncell_total,
    ncell_in_mask = in_mask,
    ncell_outside_mask = mask_na_count,
    cell_area_km2 = cell_area_km2,
    derived_area_km2 = derived_area_km2,
    study_area_km2 = study_area_km2,
    area_diff_km2 = area_diff_km2,
    area_diff_pct = area_diff_pct
  )
  data.table::fwrite(sanity_dt, file = file.path(output_dir, "study_area_sanity.csv"))
  invisible(list(cell_area_km2 = cell_area_km2, ncell_in_mask = in_mask, mask_na_count = mask_na_count))
}

make_class_raster <- function(class_list, template, class_ids,
                              overlap_rule = "priority", raster_order = NULL) {
  overlap_rule <- match.arg(overlap_rule, c("last_wins", "first_wins", "priority"))
  effective_rule <- if (overlap_rule == "priority") "first_wins" else overlap_rule
  if (is.null(raster_order) || !length(raster_order)) {
    raster_order <- names(class_ids)
  }
  raster_order <- raster_order[raster_order %in% names(class_ids)]
  r <- template
  r[!is.na(r)] <- 0
  if (!length(class_list) || !length(raster_order)) return(r)
  r_vals <- terra::values(r, mat = FALSE)
  for (cls in raster_order) {
    if (!cls %in% names(class_list)) next
    geom <- class_list[[cls]]
    if (is.null(geom) || !length(geom)) next
    geom <- geom[!terra::is.empty(geom)]
    if (!length(geom)) next
    geom$class_id <- class_ids[[cls]]
    tmp <- terra::rasterize(geom, template, field = "class_id", touches = TRUE, background = NA_real_)
    tmp <- terra::mask(tmp, template)
    tmp_vals <- terra::values(tmp, mat = FALSE)
    idx <- !is.na(tmp_vals)
    if (effective_rule != "last_wins") {
      idx <- idx & (r_vals == 0)
    }
    if (any(idx, na.rm = TRUE)) {
      r_vals[idx] <- tmp_vals[idx]
    }
  }
  terra::setValues(r, r_vals)
}

make_binary_raster <- function(class_list, template) {
  r <- template
  r[!is.na(r)] <- 0
  if (!length(class_list)) return(r)
  geom <- combine_spatvectors(class_list)
  if (is.null(geom) || !length(geom)) return(r)
  geom$flag <- 1
  tmp <- terra::rasterize(geom, template, field = "flag", touches = TRUE, background = NA_real_)
  tmp <- terra::mask(tmp, template)
  idx <- !is.na(tmp[])
  if (any(idx, na.rm = TRUE)) {
    r[idx] <- 1
  }
  r
}

calc_unique_area_from_raster <- function(class_raster, class_ids, cell_area_km2) {
  vals <- terra::values(class_raster, mat = FALSE)
  vals <- vals[!is.na(vals)]
  if (!length(vals)) {
    return(data.table(Class = names(class_ids), unique_area_km2 = 0))
  }
  tab <- table(factor(vals, levels = unname(class_ids)))
  data.table(Class = names(class_ids), unique_area_km2 = as.numeric(tab) * cell_area_km2)
}

calc_binary_area_km2 <- function(binary_raster, cell_area_km2) {
  vals <- terra::values(binary_raster, mat = FALSE)
  sum(vals == 1, na.rm = TRUE) * cell_area_km2
}

calc_binary_class_areas <- function(class_list, template, cell_area_km2) {
  if (!length(class_list)) {
    return(data.table(Class = character(), unique_area_km2 = numeric()))
  }
  rows <- lapply(names(class_list), function(cls) {
    geom <- class_list[[cls]]
    if (is.null(geom) || !length(geom)) {
      area <- 0
    } else {
      bin <- make_binary_raster(list(tmp = geom), template)
      area <- calc_binary_area_km2(bin, cell_area_km2)
    }
    data.table(Class = cls, unique_area_km2 = area)
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

compute_confusion <- function(reference_raster, comparison_raster, class_ids, drop_bg_bg = FALSE) {
  ref_vals <- terra::values(reference_raster, mat = FALSE)
  cmp_vals <- terra::values(comparison_raster, mat = FALSE)
  valid <- !is.na(ref_vals) & !is.na(cmp_vals)
  if (drop_bg_bg) {
    valid <- valid & !(ref_vals == 0 & cmp_vals == 0)
  }
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

compute_binary_metrics <- function(reference_binary, comparison_binary) {
  ref_vals <- terra::values(reference_binary, mat = FALSE)
  cmp_vals <- terra::values(comparison_binary, mat = FALSE)
  valid <- !is.na(ref_vals) & !is.na(cmp_vals)
  ref_vals <- ref_vals[valid]
  cmp_vals <- cmp_vals[valid]
  ref_pos <- ref_vals == 1
  cmp_pos <- cmp_vals == 1
  tp <- sum(ref_pos & cmp_pos)
  fp <- sum(!ref_pos & cmp_pos)
  fn <- sum(ref_pos & !cmp_pos)
  tn <- sum(!ref_pos & !cmp_pos)
  n <- tp + fp + fn + tn
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  iou <- if (tp + fp + fn > 0) tp / (tp + fp + fn) else NA_real_
  prevalence_ref <- if (n > 0) (tp + fn) / n else NA_real_
  prevalence_cmp <- if (n > 0) (tp + fp) / n else NA_real_
  omission <- if (tp + fn > 0) fn / (tp + fn) else NA_real_
  commission <- if (tp + fp > 0) fp / (tp + fp) else NA_real_
  accuracy <- if (n > 0) (tp + tn) / n else NA_real_
  data.table(
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    n_cells = n,
    precision = precision,
    recall = recall,
    f1 = f1,
    iou = iou,
    prevalence_ref = prevalence_ref,
    prevalence_sim = prevalence_cmp,
    omission_rate = omission,
    commission_rate = commission,
    accuracy = accuracy
  )
}

assert_mask_preserved <- function(template_na_count, raster, label) {
  raster_na <- sum(is.na(terra::values(raster, mat = FALSE)))
  if (!isTRUE(all.equal(raster_na, template_na_count))) {
    stop(sprintf(
      "Mask preservation check failed for %s (template NA=%s, raster NA=%s).",
      label, template_na_count, raster_na
    ), call. = FALSE)
  }
  invisible(TRUE)
}

assert_raster_determinism <- function(raster_a, raster_b, label) {
  vals_a <- terra::values(raster_a, mat = FALSE)
  vals_b <- terra::values(raster_b, mat = FALSE)
  if (!isTRUE(all.equal(vals_a, vals_b))) {
    stop(sprintf("Determinism check failed for %s rasterization.", label), call. = FALSE)
  }
  invisible(TRUE)
}

order_class_rows <- function(dt, class_order) {
  if (is.null(dt) || !nrow(dt) || !"Class" %in% names(dt)) return(dt)
  idx <- match(dt$Class, class_order)
  dt <- dt[order(ifelse(is.na(idx), Inf, idx), dt$Class)]
  dt
}

write_run_metadata <- function(rows, output_dir) {
  if (!length(rows)) return(invisible(NULL))
  dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
  data.table::fwrite(dt, file = file.path(output_dir, "run_metadata.csv"))
  invisible(NULL)
}

compute_grid_corroboration <- function(reference_binary, comparison_binary, scales_km,
                                       interval_label, replicate_id, class_mode, analysis_mode) {
  res_val <- terra::res(reference_binary)
  base_res <- res_val[1]
  rows <- lapply(scales_km, function(scale_km) {
    target_m <- scale_km * 1000
    fact <- max(1L, round(target_m / base_res))
    grid_res_m <- fact * base_res
    ref_agg <- terra::aggregate(reference_binary, fact = fact, fun = mean, na.rm = TRUE)
    cmp_agg <- terra::aggregate(comparison_binary, fact = fact, fun = mean, na.rm = TRUE)
    ref_vals <- terra::values(ref_agg, mat = FALSE)
    cmp_vals <- terra::values(cmp_agg, mat = FALSE)
    valid <- !is.na(ref_vals) & !is.na(cmp_vals)
    ref_vals <- ref_vals[valid]
    cmp_vals <- cmp_vals[valid]
    n_cells <- length(ref_vals)
    rmse <- if (n_cells) sqrt(mean((cmp_vals - ref_vals)^2)) else NA_real_
    bias <- if (n_cells) mean(cmp_vals - ref_vals) else NA_real_
    spearman <- if (n_cells > 1) {
      stats::cor(cmp_vals, ref_vals, method = "spearman", use = "complete.obs")
    } else {
      NA_real_
    }
    data.table(
      interval = interval_label,
      replicate = replicate_id,
      class_mode = class_mode,
      grid_km = scale_km,
      grid_res_m = grid_res_m,
      rmse = rmse,
      bias = bias,
      spearman_r = spearman,
      n_cells = n_cells,
      analysis_mode = analysis_mode
    )
  })
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

compute_linear_distance_metrics <- function(observed_classes, simulated_classes, template, linear_classes,
                                            interval_label, replicate_id, class_mode, analysis_mode) {
  obs_subset <- observed_classes[names(observed_classes) %in% linear_classes]
  sim_subset <- simulated_classes[names(simulated_classes) %in% linear_classes]
  if (!length(obs_subset)) {
    return(data.table(
      interval = interval_label,
      replicate = replicate_id,
      class_mode = class_mode,
      n_observed_pixels = 0,
      median_distance_m = NA_real_,
      p90_distance_m = NA_real_,
      analysis_mode = analysis_mode
    ))
  }
  obs_bin <- make_binary_raster(obs_subset, template)
  sim_bin <- make_binary_raster(sim_subset, template)
  sim_source <- sim_bin
  sim_source[sim_source == 0] <- NA
  if (all(is.na(terra::values(sim_source, mat = FALSE)))) {
    return(data.table(
      interval = interval_label,
      replicate = replicate_id,
      class_mode = class_mode,
      n_observed_pixels = sum(terra::values(obs_bin, mat = FALSE) == 1, na.rm = TRUE),
      median_distance_m = NA_real_,
      p90_distance_m = NA_real_,
      analysis_mode = analysis_mode
    ))
  }
  dist_r <- terra::distance(sim_source)
  obs_idx <- terra::values(obs_bin, mat = FALSE) == 1
  dist_vals <- terra::values(dist_r, mat = FALSE)[obs_idx]
  dist_vals <- dist_vals[is.finite(dist_vals)]
  if (!length(dist_vals)) {
    return(data.table(
      interval = interval_label,
      replicate = replicate_id,
      class_mode = class_mode,
      n_observed_pixels = sum(obs_idx, na.rm = TRUE),
      median_distance_m = NA_real_,
      p90_distance_m = NA_real_,
      analysis_mode = analysis_mode
    ))
  }
  if (length(dist_vals) > 5000000) {
    dist_vals <- sample(dist_vals, 5000000)
  }
  data.table(
    interval = interval_label,
    replicate = replicate_id,
    class_mode = class_mode,
    n_observed_pixels = length(dist_vals),
    median_distance_m = stats::median(dist_vals),
    p90_distance_m = stats::quantile(dist_vals, probs = 0.9, names = FALSE, type = 7),
    analysis_mode = analysis_mode
  )
}

append_summary_row <- function(summary_row, output_root) {
  if (is.null(summary_row) || !nrow(summary_row)) return(invisible(NULL))
  path <- file.path(output_root, "adqd_summary.csv")
  if (file.exists(path)) {
    header <- tryCatch(readLines(path, n = 1), error = function(e) "")
    header_cols <- if (length(header) && nzchar(header)) strsplit(header, ",")[[1]] else character()
    if (length(header_cols) && !identical(header_cols, names(summary_row))) {
      warning("adqd_summary.csv schema changed; overwriting with new columns.", immediate. = FALSE)
      data.table::fwrite(summary_row, file = path)
      return(invisible(NULL))
    }
    data.table::fwrite(summary_row, file = path, append = TRUE)
  } else {
    data.table::fwrite(summary_row, file = path)
  }
  invisible(NULL)
}

write_quantity_tables <- function(interval_label, years, study_area_km2,
                                  observed_unique_dt, observed_gross_dt,
                                  sim_unique_dt, sim_gross_dt,
                                  output_dir, analysis_mode, class_mode,
                                  class_order) {
  sim_unique_summary <- if (nrow(sim_unique_dt)) {
    sim_unique_dt[, .(
      sim_unique_area_mean_km2 = mean(unique_area_km2, na.rm = TRUE),
      sim_unique_area_sd_km2 = if (.N > 1) stats::sd(unique_area_km2, na.rm = TRUE) else 0
    ), by = Class]
  } else {
    data.table(Class = character(), sim_unique_area_mean_km2 = numeric(), sim_unique_area_sd_km2 = numeric())
  }
  sim_gross_summary <- if (nrow(sim_gross_dt)) {
    sim_gross_dt[, .(
      sim_gross_area_overlap_inflated_mean_km2 = mean(gross_area_overlap_inflated_km2, na.rm = TRUE),
      sim_gross_area_overlap_inflated_sd_km2 = if (.N > 1) stats::sd(gross_area_overlap_inflated_km2, na.rm = TRUE) else 0
    ), by = Class]
  } else {
    data.table(
      Class = character(),
      sim_gross_area_overlap_inflated_mean_km2 = numeric(),
      sim_gross_area_overlap_inflated_sd_km2 = numeric()
    )
  }
  obs_unique <- if (nrow(observed_unique_dt)) {
    data.table::copy(observed_unique_dt)
  } else {
    data.table(Class = character(), unique_area_km2 = numeric())
  }
  obs_gross <- if (nrow(observed_gross_dt)) {
    data.table::copy(observed_gross_dt)
  } else {
    data.table(Class = character(), gross_area_overlap_inflated_km2 = numeric())
  }
  data.table::setnames(obs_unique, "unique_area_km2", "observed_unique_area_km2")
  data.table::setnames(obs_gross, "gross_area_overlap_inflated_km2", "observed_gross_area_overlap_inflated_km2")
  combined <- merge(obs_unique, obs_gross, by = "Class", all = TRUE)
  combined <- merge(combined, sim_unique_summary, by = "Class", all = TRUE)
  combined <- merge(combined, sim_gross_summary, by = "Class", all = TRUE)
  cols_to_zero <- c(
    "observed_unique_area_km2",
    "observed_gross_area_overlap_inflated_km2",
    "sim_unique_area_mean_km2",
    "sim_unique_area_sd_km2",
    "sim_gross_area_overlap_inflated_mean_km2",
    "sim_gross_area_overlap_inflated_sd_km2"
  )
  for (col in cols_to_zero) {
    if (!col %in% names(combined)) combined[[col]] <- 0
    combined[is.na(get(col)), (col) := 0]
  }
  combined[, observed_rate_km2_per_year := if (years > 0) observed_unique_area_km2 / years else NA_real_]
  combined[, simulated_rate_km2_per_year := if (years > 0) sim_unique_area_mean_km2 / years else NA_real_]
  combined[, observed_rate_pct_per_year := if (study_area_km2 > 0 && years > 0) {
    (observed_unique_area_km2 / (study_area_km2 * years)) * 100
  } else {
    NA_real_
  }]
  combined[, simulated_rate_pct_per_year := if (study_area_km2 > 0 && years > 0) {
    (sim_unique_area_mean_km2 / (study_area_km2 * years)) * 100
  } else {
    NA_real_
  }]
  combined[, bias_pct_per_year := simulated_rate_pct_per_year - observed_rate_pct_per_year]
  combined[, bias_km2_per_year := simulated_rate_km2_per_year - observed_rate_km2_per_year]
  combined[, analysis_mode := analysis_mode]
  combined[, class_mode := class_mode]
  combined <- order_class_rows(combined, class_order)
  suffix <- if (class_mode == "native") "" else paste0("_", class_mode)
  data.table::fwrite(combined, file = file.path(output_dir, paste0("quantity_metrics", suffix, ".csv")))

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
  total_observed_unique <- sum(combined$observed_unique_area_km2, na.rm = TRUE)
  total_sim_unique <- sum(combined$sim_unique_area_mean_km2, na.rm = TRUE)
  total_observed_gross <- sum(combined$observed_gross_area_overlap_inflated_km2, na.rm = TRUE)
  total_sim_gross <- sum(combined$sim_gross_area_overlap_inflated_mean_km2, na.rm = TRUE)
  summary_dt <- data.table(
    interval = interval_label,
    years = years,
    study_area_km2 = study_area_km2,
    total_observed_unique_area_km2 = total_observed_unique,
    total_simulated_unique_area_km2 = total_sim_unique,
    total_observed_gross_area_overlap_inflated_km2 = total_observed_gross,
    total_simulated_gross_area_overlap_inflated_km2 = total_sim_gross,
    overall_bias_pct_per_year = global_bias,
    rmse_pct_per_year = rmse_pct,
    analysis_mode = analysis_mode,
    class_mode = class_mode
  )
  data.table::fwrite(summary_dt, file = file.path(output_dir, paste0("quantity_summary", suffix, ".csv")))
  combined
}

write_class_bias_unique <- function(combined_dt, output_dir, class_mode) {
  if (is.null(combined_dt) || !nrow(combined_dt)) return(invisible(NULL))
  needed <- c(
    "Class",
    "observed_unique_area_km2",
    "sim_unique_area_mean_km2",
    "observed_rate_pct_per_year",
    "simulated_rate_pct_per_year",
    "bias_pct_per_year",
    "bias_km2_per_year"
  )
  missing <- setdiff(needed, names(combined_dt))
  if (length(missing)) return(invisible(NULL))
  out <- combined_dt[, ..needed]
  suffix <- if (class_mode == "native") "" else paste0("_", class_mode)
  data.table::fwrite(out, file = file.path(output_dir, paste0("class_bias_unique", suffix, ".csv")))
  invisible(NULL)
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

write_dataclass_metrics <- function(interval_label, combined_dt, output_dir, analysis_mode, class_mode) {
  if (!identical(class_mode, "native")) return(invisible(NULL))
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

write_confusion_outputs <- function(interval_label, replicate_results, output_dir, analysis_mode, class_mode) {
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
    df$class_mode <- class_mode
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
      analysis_mode = analysis_mode,
      class_mode = class_mode
    )
  }
  confusion_dt <- data.table::rbindlist(confusion_rows, use.names = TRUE, fill = TRUE)
  suffix <- if (class_mode == "native") "" else paste0("_", class_mode)
  data.table::fwrite(confusion_dt, file = file.path(output_dir, paste0("confusion_matrix", suffix, ".csv")))
  disagreement_dt <- data.table::rbindlist(disagreement_rows, use.names = TRUE, fill = TRUE)
  data.table::fwrite(disagreement_dt, file = file.path(output_dir, paste0("disagreement", suffix, ".csv")))
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
  sim_files_all <- parse_simulated_file_metadata(opts$simulation_root)
  if (!nrow(sim_files_all)) {
    stop("No disturbance shapefiles were found under ", opts$simulation_root, call. = FALSE)
  }
  analysis_mode <- opts$analysis_mode
  interval_label <- observed$label
  interval_dir <- file.path(opts$output_root, interval_label)
  dir.create(interval_dir, recursive = TRUE, showWarnings = FALSE)
  class_order <- get_class_order()
  crosswalk <- build_evaluation_crosswalk()
  year_rule_desc <- if (opts$year_rule == "increment") {
    "year > baseline && year <= comparison"
  } else {
    "year == comparison"
  }
  year_select_line <- if (isTRUE(opts$no_year_filter)) {
    " - simulated years: no filter (all parsed files considered)."
  } else {
    sprintf(" - simulated years: rule %s (%s).", opts$year_rule, year_rule_desc)
  }
  assumption_lines <- c(
    "Analysis mode:",
    paste0(" - ", opts$analysis_mode),
    sprintf("Interval: baseline %d to comparison %d.", baseline_year, comparison_year),
    "Notes:",
    " - simulated files parsed from disturbances_<YEAR>_<CLASS>[_rep<REP>].shp; selections recorded in sim_file_index.csv.",
    year_select_line,
    sprintf(" - overlap_rule: %s (priority = earlier class_order wins; last_wins = later order wins).", opts$overlap_rule),
    sprintf(" - caribou_buffer: %s (line buffer %sm, polygon buffer %sm).",
            if (isTRUE(opts$caribou_buffer)) "enabled" else "disabled",
            opts$line_buffer, opts$polygon_buffer),
    sprintf(" - skip_buffering: %s (buffers forced to 0).", if (isTRUE(opts$skip_buffering)) "enabled" else "disabled"),
    " - raster overlaps follow overlap_rule using class_order.txt for the rasterization order.",
    " - unique areas are the headline; gross overlap-inflated areas are diagnostics only.",
    " - crosswalk output includes OilGas_Total and excludes out-of-scope classes."
  )
  if (isTRUE(opts$caribou_buffer)) {
    assumption_lines <- c(
      assumption_lines,
      " - caribou headline outputs: foreground_metrics.csv and grid_corroboration.csv.",
      " - buffered_footprint_by_class_nonadditive.csv is secondary and non-additive."
    )
  }
  writeLines(assumption_lines, con = file.path(interval_dir, "assumption.txt"))
  years <- comparison_year - baseline_year
  template <- make_template_raster(study_area_sv, opts$raster_resolution)
  sanity <- write_study_area_sanity(
    template,
    study_area_km2,
    interval_dir,
    analysis_mode,
    interval_label,
    opts$raster_resolution
  )
  cell_area_km2 <- sanity$cell_area_km2
  mask_na_count <- sanity$mask_na_count
  writeLines(c("class_order:", paste0(" - ", class_order)), con = file.path(interval_dir, "class_order.txt"))
  write_crosswalk_file(crosswalk, interval_dir)
  grid_scales_km <- c(1, 5, 10)
  linear_classes <- c("Seismic", "Road", "Pipeline")

  mode_configs <- list(
    native = list(
      observed_classes = observed$classes,
      observed_gross = observed$gross_areas,
      class_mapper = function(x) x,
      map_sim_classes = function(x) x,
      map_sim_gross = function(x) x
    ),
    crosswalk = list(
      observed_classes = apply_crosswalk_to_class_list(observed$classes, crosswalk),
      observed_gross = apply_crosswalk_to_area_dt(observed$gross_areas, crosswalk,
        value_col = "gross_area_overlap_inflated_km2"
      ),
      class_mapper = function(x) map_class_to_crosswalk(x, crosswalk),
      map_sim_classes = function(x) lapply(x, apply_crosswalk_to_class_list, crosswalk = crosswalk),
      map_sim_gross = function(x) apply_crosswalk_to_area_dt(x, crosswalk,
        value_col = "gross_area_overlap_inflated_km2",
        group_cols = "replicate"
      )
    )
  )

  foreground_rows <- list()
  grid_rows <- list()
  linear_rows <- list()
  buffered_rows <- list()
  summary_rows <- list()
  metadata_rows <- list()
  sim_index_rows <- list()

  for (class_mode in names(mode_configs)) {
    mode <- mode_configs[[class_mode]]
    obs_classes <- mode$observed_classes
    obs_gross <- mode$observed_gross
    if (is.null(obs_classes)) obs_classes <- list()
    if (is.null(obs_gross)) obs_gross <- data.table()

    sim_index_base <- build_sim_file_index(
      file_dt = sim_files_all,
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      year_rule = opts$year_rule,
      no_year_filter = opts$no_year_filter,
      replicates = opts$replicates,
      class_mode = class_mode,
      class_filter = NULL,
      class_mapper = mode$class_mapper
    )
    base_selected <- sim_index_base[parse_ok & selected_year & selected_rep & selected_class]
    classes_available <- sort(unique(base_selected$class_target))
    classes_available <- classes_available[!is.na(classes_available) & nzchar(classes_available)]

    class_names <- unique(c(names(obs_classes), classes_available))
    class_ids <- build_class_ids(class_names, class_order)
    raster_order <- names(class_ids)

    sim_index_mode <- build_sim_file_index(
      file_dt = sim_files_all,
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      year_rule = opts$year_rule,
      no_year_filter = opts$no_year_filter,
      replicates = opts$replicates,
      class_mode = class_mode,
      class_filter = names(class_ids),
      class_mapper = mode$class_mapper
    )
    sim_index_mode[, `:=`(
      interval = interval_label,
      analysis_mode = analysis_mode
    )]
    sim_index_rows[[length(sim_index_rows) + 1L]] <- sim_index_mode

    sim_selected <- sim_index_mode[selected == TRUE]
    n_selected <- nrow(sim_selected)
    n_total <- nrow(sim_index_mode)
    selected_years <- sort(unique(sim_selected$year[!is.na(sim_selected$year)]))
    selected_reps <- sort(unique(sim_selected$replicate[!is.na(sim_selected$replicate)]))
    selected_classes <- sort(unique(sim_selected$class_target))
    selected_classes <- selected_classes[!is.na(selected_classes) & nzchar(selected_classes)]
    year_range_label <- if (length(selected_years)) {
      sprintf("%d..%d", min(selected_years), max(selected_years))
    } else {
      "unknown"
    }
    reps_label <- if (length(selected_reps)) {
      paste(selected_reps, collapse = ", ")
    } else {
      "none"
    }
    message(sprintf(
      "Simulated inputs: selected %d/%d files, years [%s], reps: %s (class_mode=%s)",
      n_selected, n_total, year_range_label, reps_label, class_mode
    ))

    if (!n_selected) {
      expected_rule <- if (opts$year_rule == "increment") {
        sprintf("year > %d and year <= %d", baseline_year, comparison_year)
      } else {
        sprintf("year == %d", comparison_year)
      }
      stop(paste0(
        "No simulated files selected for interval ", interval_label,
        " (baseline ", baseline_year, ", comparison ", comparison_year, "). ",
        "Simulation root: ", opts$simulation_root, ". ",
        "Expected years: ", expected_rule,
        " (rule: ", opts$year_rule, "). ",
        "Filename pattern: disturbances_<YEAR>_<CLASS>[_rep<REP>].shp. ",
        "Use --no-year-filter to bypass year filtering if needed."
      ), call. = FALSE)
    }

    if (!isTRUE(opts$no_year_filter) && length(selected_years)) {
      invalid_years <- selected_years[
        !apply_year_rule(selected_years, baseline_year, comparison_year, opts$year_rule)
      ]
      if (length(invalid_years)) {
        stop(sprintf(
          "Selected simulated years fall outside the %s rule for interval %s: %s",
          opts$year_rule, interval_label, paste(invalid_years, collapse = ", ")
        ), call. = FALSE)
      }
    }

    sim <- prepare_simulated_geoms(
      sim_selected,
      study_area_sv,
      opts$line_buffer,
      opts$polygon_buffer
    )
    buffer_warnings <- sim$warnings
    sim_classes <- mode$map_sim_classes(sim$geoms)
    sim_gross <- mode$map_sim_gross(sim$gross_areas)
    if (is.null(sim_classes)) sim_classes <- list()
    if (is.null(sim_gross)) sim_gross <- data.table()

    metadata_rows[[length(metadata_rows) + 1L]] <- data.table(
      interval = interval_label,
      analysis_mode = analysis_mode,
      class_mode = class_mode,
      baseline_year = baseline_year,
      comparison_year = comparison_year,
      year_inclusion_rule = opts$year_rule,
      no_year_filter = isTRUE(opts$no_year_filter),
      n_sim_files_total = nrow(sim_files_all),
      n_sim_files_selected = n_selected,
      years_selected_min = if (length(selected_years)) min(selected_years) else NA_integer_,
      years_selected_max = if (length(selected_years)) max(selected_years) else NA_integer_,
      years_selected = if (length(selected_years)) paste(selected_years, collapse = "|") else NA_character_,
      classes_selected = if (length(selected_classes)) paste(selected_classes, collapse = "|") else NA_character_,
      n_replications_detected = length(selected_reps),
      overlap_rule = opts$overlap_rule,
      rasterization_order = paste(raster_order, collapse = "|"),
      raster_resolution_m = opts$raster_resolution,
      line_buffer_m = opts$line_buffer,
      polygon_buffer_m = opts$polygon_buffer,
      caribou_buffer = isTRUE(opts$caribou_buffer),
      skip_buffering = isTRUE(opts$skip_buffering),
      buffering_warning_count = length(buffer_warnings),
      buffering_warnings = paste(buffer_warnings, collapse = " | ")
    )
    obs_raster <- make_class_raster(
      obs_classes,
      template,
      class_ids,
      overlap_rule = opts$overlap_rule,
      raster_order = raster_order
    )
    assert_mask_preserved(mask_na_count, obs_raster, sprintf("observed (%s)", class_mode))
    obs_raster_check <- make_class_raster(
      obs_classes,
      template,
      class_ids,
      overlap_rule = opts$overlap_rule,
      raster_order = raster_order
    )
    assert_raster_determinism(obs_raster, obs_raster_check, sprintf("observed (%s)", class_mode))
    obs_unique_dt <- calc_unique_area_from_raster(obs_raster, class_ids, cell_area_km2)
    obs_binary <- make_binary_raster(obs_classes, template)
    obs_binary_area <- calc_binary_area_km2(obs_binary, cell_area_km2)

    if (isTRUE(opts$caribou_buffer)) {
      obs_buf_dt <- calc_binary_class_areas(obs_classes, template, cell_area_km2)
      if (nrow(obs_buf_dt)) {
        data.table::setnames(obs_buf_dt, "unique_area_km2", "buffered_unique_area_nonadditive_km2")
        obs_buf_dt[, `:=`(
          dataset = "observed",
          replicate = NA_integer_,
          interval = interval_label,
          analysis_mode = analysis_mode,
          class_mode = class_mode,
          non_additive = TRUE
        )]
        buffered_rows[[length(buffered_rows) + 1L]] <- obs_buf_dt
      }
    }

    sim_unique_rows <- list()
    replicate_results <- list()
    determinism_checked <- FALSE
    for (rep_id in names(sim_classes)) {
      class_list <- sim_classes[[rep_id]]
      if (is.null(class_list) || !length(class_list)) next
      sim_raster <- make_class_raster(
        class_list,
        template,
        class_ids,
        overlap_rule = opts$overlap_rule,
        raster_order = raster_order
      )
      assert_mask_preserved(mask_na_count, sim_raster, sprintf("simulated rep %s (%s)", rep_id, class_mode))
      if (!determinism_checked) {
        sim_raster_check <- make_class_raster(
          class_list,
          template,
          class_ids,
          overlap_rule = opts$overlap_rule,
          raster_order = raster_order
        )
        assert_raster_determinism(sim_raster, sim_raster_check, sprintf("simulated rep %s (%s)", rep_id, class_mode))
        determinism_checked <- TRUE
      }
      tab <- compute_confusion(obs_raster, sim_raster, class_ids, drop_bg_bg = FALSE)
      replicate_results[[length(replicate_results) + 1L]] <- list(
        replicate = as.integer(rep_id),
        table = tab,
        mapping = map_id_to_class(class_ids)
      )

      change_tab <- compute_confusion(obs_raster, sim_raster, class_ids, drop_bg_bg = TRUE)
      change_disag <- summarize_disagreement(change_tab)
      change_n <- sum(change_tab)

      sim_binary <- make_binary_raster(class_list, template)
      sim_binary_area <- calc_binary_area_km2(sim_binary, cell_area_km2)
      bin_metrics <- compute_binary_metrics(obs_binary, sim_binary)
      bin_metrics[, `:=`(
        interval = interval_label,
        replicate = as.integer(rep_id),
        analysis_mode = analysis_mode,
        class_mode = class_mode,
        ref_disturbed_area_km2 = obs_binary_area,
        sim_disturbed_area_km2 = sim_binary_area,
        change_mask_overall_agreement = change_disag$overall,
        change_mask_quantity_disagreement = change_disag$quantity,
        change_mask_allocation_disagreement = change_disag$allocation,
        change_mask_total_disagreement = change_disag$total,
        change_mask_n_cells = change_n
      )]
      foreground_rows[[length(foreground_rows) + 1L]] <- bin_metrics

      sim_unique_dt <- calc_unique_area_from_raster(sim_raster, class_ids, cell_area_km2)
      sim_unique_dt[, replicate := as.integer(rep_id)]
      sim_unique_rows[[length(sim_unique_rows) + 1L]] <- sim_unique_dt

      grid_rows[[length(grid_rows) + 1L]] <- compute_grid_corroboration(
        reference_binary = obs_binary,
        comparison_binary = sim_binary,
        scales_km = grid_scales_km,
        interval_label = interval_label,
        replicate_id = as.integer(rep_id),
        class_mode = class_mode,
        analysis_mode = analysis_mode
      )

      linear_rows[[length(linear_rows) + 1L]] <- compute_linear_distance_metrics(
        observed_classes = obs_classes,
        simulated_classes = class_list,
        template = template,
        linear_classes = linear_classes,
        interval_label = interval_label,
        replicate_id = as.integer(rep_id),
        class_mode = class_mode,
        analysis_mode = analysis_mode
      )

      overall_disag <- summarize_disagreement(tab)
      summary_rows[[length(summary_rows) + 1L]] <- data.table(
        interval = interval_label,
        replicate = as.integer(rep_id),
        class_mode = class_mode,
        analysis_mode = analysis_mode,
        overall_agreement = overall_disag$overall,
        quantity_disagreement = overall_disag$quantity,
        allocation_disagreement = overall_disag$allocation,
        total_disagreement = overall_disag$total,
        change_mask_overall_agreement = change_disag$overall,
        change_mask_quantity_disagreement = change_disag$quantity,
        change_mask_allocation_disagreement = change_disag$allocation,
        change_mask_total_disagreement = change_disag$total,
        binary_f1 = bin_metrics$f1,
        binary_iou = bin_metrics$iou,
        binary_precision = bin_metrics$precision,
        binary_recall = bin_metrics$recall,
        prevalence_ref = bin_metrics$prevalence_ref,
        prevalence_sim = bin_metrics$prevalence_sim
      )

      if (isTRUE(opts$caribou_buffer)) {
        sim_buf_dt <- calc_binary_class_areas(class_list, template, cell_area_km2)
        if (nrow(sim_buf_dt)) {
          data.table::setnames(sim_buf_dt, "unique_area_km2", "buffered_unique_area_nonadditive_km2")
          sim_buf_dt[, `:=`(
            dataset = "simulated",
            replicate = as.integer(rep_id),
            interval = interval_label,
            analysis_mode = analysis_mode,
            class_mode = class_mode,
            non_additive = TRUE
          )]
          buffered_rows[[length(buffered_rows) + 1L]] <- sim_buf_dt
        }
      }
    }

    if (length(replicate_results)) {
      write_confusion_outputs(interval_label, replicate_results, interval_dir, analysis_mode, class_mode)
    } else {
      warning(sprintf("No simulated geometries matched the requested replicates for class mode %s; skipping map comparison.", class_mode))
    }

    sim_unique_dt <- if (length(sim_unique_rows)) {
      data.table::rbindlist(sim_unique_rows, use.names = TRUE, fill = TRUE)
    } else {
      data.table()
    }
    combined_dt <- write_quantity_tables(
      interval_label = interval_label,
      years = years,
      study_area_km2 = study_area_km2,
      observed_unique_dt = obs_unique_dt,
      observed_gross_dt = obs_gross,
      sim_unique_dt = sim_unique_dt,
      sim_gross_dt = sim_gross,
      output_dir = interval_dir,
      analysis_mode = analysis_mode,
      class_mode = class_mode,
      class_order = class_order
    )
    write_class_bias_unique(combined_dt, interval_dir, class_mode)
    write_dataclass_metrics(interval_label, combined_dt, interval_dir, analysis_mode, class_mode)
  }

  if (length(sim_index_rows)) {
    sim_index_dt <- data.table::rbindlist(sim_index_rows, use.names = TRUE, fill = TRUE)
    data.table::fwrite(sim_index_dt, file = file.path(interval_dir, "sim_file_index.csv"))
  }
  if (length(foreground_rows)) {
    foreground_dt <- data.table::rbindlist(foreground_rows, use.names = TRUE, fill = TRUE)
    data.table::fwrite(foreground_dt, file = file.path(interval_dir, "foreground_metrics.csv"))
  }
  grid_dt <- if (length(grid_rows)) {
    data.table::rbindlist(grid_rows, use.names = TRUE, fill = TRUE)
  } else {
    data.table()
  }
  if (nrow(grid_dt)) {
    data.table::fwrite(grid_dt, file = file.path(interval_dir, "grid_corroboration.csv"))
  }
  if (length(linear_rows)) {
    linear_dt <- data.table::rbindlist(linear_rows, use.names = TRUE, fill = TRUE)
    data.table::fwrite(linear_dt, file = file.path(interval_dir, "linear_distance_metrics.csv"))
  }
  if (isTRUE(opts$caribou_buffer) && length(buffered_rows)) {
    buffered_dt <- data.table::rbindlist(buffered_rows, use.names = TRUE, fill = TRUE)
    data.table::fwrite(
      buffered_dt,
      file = file.path(interval_dir, "buffered_footprint_by_class_nonadditive.csv")
    )
  }
  if (length(summary_rows)) {
    summary_dt <- data.table::rbindlist(summary_rows, use.names = TRUE, fill = TRUE)
    if (nrow(grid_dt)) {
      grid_metrics <- grid_dt[, .(
        interval,
        replicate,
        class_mode,
        analysis_mode,
        grid_km,
        rmse,
        bias,
        spearman_r
      )]
      rmse_wide <- data.table::dcast(
        grid_metrics,
        interval + replicate + class_mode + analysis_mode ~ grid_km,
        value.var = "rmse"
      )
      bias_wide <- data.table::dcast(
        grid_metrics,
        interval + replicate + class_mode + analysis_mode ~ grid_km,
        value.var = "bias"
      )
      spearman_wide <- data.table::dcast(
        grid_metrics,
        interval + replicate + class_mode + analysis_mode ~ grid_km,
        value.var = "spearman_r"
      )
      if (ncol(rmse_wide) > 4) {
        data.table::setnames(rmse_wide, names(rmse_wide)[-(1:4)],
                             paste0("grid_rmse_", names(rmse_wide)[-(1:4)], "km"))
      }
      if (ncol(bias_wide) > 4) {
        data.table::setnames(bias_wide, names(bias_wide)[-(1:4)],
                             paste0("grid_bias_", names(bias_wide)[-(1:4)], "km"))
      }
      if (ncol(spearman_wide) > 4) {
        data.table::setnames(spearman_wide, names(spearman_wide)[-(1:4)],
                             paste0("grid_spearman_", names(spearman_wide)[-(1:4)], "km"))
      }
      summary_dt <- merge(summary_dt, rmse_wide,
        by = c("interval", "replicate", "class_mode", "analysis_mode"),
        all.x = TRUE
      )
      summary_dt <- merge(summary_dt, bias_wide,
        by = c("interval", "replicate", "class_mode", "analysis_mode"),
        all.x = TRUE
      )
      summary_dt <- merge(summary_dt, spearman_wide,
        by = c("interval", "replicate", "class_mode", "analysis_mode"),
        all.x = TRUE
      )
    }
    if (exists("linear_dt", inherits = FALSE) && nrow(linear_dt)) {
      summary_dt <- merge(
        summary_dt,
        linear_dt[, .(
          interval,
          replicate,
          class_mode,
          analysis_mode,
          linear_median_distance_m = median_distance_m,
          linear_p90_distance_m = p90_distance_m
        )],
        by = c("interval", "replicate", "class_mode", "analysis_mode"),
        all.x = TRUE
      )
    }
    append_summary_row(summary_dt, opts$output_root)
  }
  write_run_metadata(metadata_rows, interval_dir)
  message(sprintf("Interval %s metrics written to %s", interval_label, interval_dir))
}

main <- function() {
  opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    print_usage()
    quit(save = "no", status = 0, runLast = FALSE)
  }
  if (!identical(opts$analysis_mode, default_analysis_mode) &&
      identical(opts$output_root, default_output_root)) {
    opts$output_root <- file.path(project_root, "outputs", "adqd_validation", "results", opts$analysis_mode)
  }
  if (isTRUE(opts$caribou_buffer)) {
    opts$line_buffer <- 500
    opts$polygon_buffer <- max(opts$polygon_buffer, 500)
  }
  if (isTRUE(opts$skip_buffering)) {
    opts$line_buffer <- 0
    opts$polygon_buffer <- 0
  }
  opts$simulation_root <- normalize_existing_path(opts$simulation_root, mustExist = TRUE)
  opts$output_root <- normalize_existing_path(opts$output_root, mustExist = FALSE)
  opts$bead_root <- normalize_existing_path(opts$bead_root, mustExist = TRUE)
  opts$study_area <- normalize_existing_path(opts$study_area, mustExist = TRUE)
  opts$replicates <- sort(unique(as.integer(opts$replicates)))
  valid_year_rule <- c("increment", "exact")
  if (!opts$year_rule %in% valid_year_rule) {
    stop("Invalid year_rule: ", opts$year_rule,
         ". Choose from: ", paste(valid_year_rule, collapse = ", "), call. = FALSE)
  }
  valid_overlap <- c("last_wins", "first_wins", "priority")
  if (!opts$overlap_rule %in% valid_overlap) {
    stop("Invalid overlap_rule: ", opts$overlap_rule,
         ". Choose from: ", paste(valid_overlap, collapse = ", "), call. = FALSE)
  }
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

if (!isFALSE(getOption("adqd_compute_map_metrics.run_main", TRUE))) {
  main()
}
