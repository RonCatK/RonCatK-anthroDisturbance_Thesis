#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
  library(SpaDES.core)
  library(terra)
  library(reproducible)
  library(digest)
  library(googledrive)
})

`%||%` <- function(a, b) if (is.null(a) || isTRUE(is.na(a))) b else a

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

normalize_path <- function(path_value, must_exist = FALSE) {
  if (is.null(path_value) || !nzchar(path_value)) return(NA_character_)
  expanded <- path.expand(path_value)
  candidates <- unique(c(expanded, file.path(project_root, expanded)))
  for (cand in candidates) {
    norm <- tryCatch(normalizePath(cand, winslash = "/", mustWork = must_exist), error = function(...) NULL)
    if (!is.null(norm)) return(norm)
  }
  expanded
}

ensure_dir <- function(path_value) {
  if (is.null(path_value) || !nzchar(path_value)) return(NA_character_)
  dir.create(path_value, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path_value, winslash = "/", mustWork = TRUE)
}

dir_has_data <- function(path_value) {
  if (!dir.exists(path_value)) return(FALSE)
  entries <- list.files(path_value, all.files = TRUE, no.. = TRUE)
  entries <- entries[entries != ".gitkeep"]
  length(entries) > 0
}

read_disturbance_dt <- function(primary, fallback) {
  if (!is.na(primary) && file.exists(primary)) {
    data.table::fread(primary)
  } else if (!is.na(fallback) && file.exists(fallback)) {
    data.table::fread(fallback)
  } else {
    stop("disturbanceDT.csv not found at ", primary, " or ", fallback, call. = FALSE)
  }
}

drop_wind_rows <- function(dt) {
  if (is.null(dt) || !nrow(dt)) return(dt)
  cols <- intersect(c("dataName", "classToSearch", "dataClass", "fileName", "dataType"), names(dt))
  combined <- apply(dt[, cols, with = FALSE], 1, paste, collapse = "|")
  mask <- grepl("wind", combined, ignore.case = TRUE)
  if ("dataType" %in% names(dt)) {
    mask <- mask | tolower(dt$dataType %||% "") == "mif"
  }
  dt[!mask]
}

verify_checksums <- function(root_dir, skip = FALSE) {
  if (isTRUE(skip)) return(invisible(TRUE))
  candidates <- c(
    file.path(root_dir, "CHECKSUMS.txt"),
    file.path(root_dir, "checksums.txt")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) {
    message("No CHECKSUMS.txt found under ", root_dir)
    return(invisible(TRUE))
  }

  checks <- tryCatch(data.table::fread(candidates[[1]]), error = function(...) NULL)
  if (is.null(checks) || !nrow(checks) || !"file" %in% names(checks)) {
    message("CHECKSUMS.txt is missing required columns under ", root_dir)
    return(invisible(FALSE))
  }

  checks[, path := {
    f <- as.character(file)
    is_abs <- grepl("^(/|[A-Za-z]:)", f)
    target <- ifelse(is_abs, f, file.path(root_dir, f))
    normalizePath(target, winslash = "/", mustWork = FALSE)
  }]

  missing <- checks[!file.exists(path)]
  if (nrow(missing)) {
    warning("Missing inputs for checksum verification: ", paste(missing$file, collapse = ", "), immediate. = TRUE)
  }

  mismatched <- character(0)
  for (i in seq_len(nrow(checks))) {
    row <- checks[i]
    if (!file.exists(row$path)) next
    algo <- if (!"algorithm" %in% names(checks) || is.na(row$algorithm)) "xxhash64" else as.character(row$algorithm)
    digest_val <- tryCatch(digest::digest(row$path, algo = algo, file = TRUE), error = function(...) NA_character_)
    if (!is.na(row$checksum) && !is.na(digest_val) && !identical(row$checksum, digest_val)) {
      mismatched <- c(mismatched, row$file)
    }
  }
  if (length(mismatched)) {
    warning("Checksum mismatch for: ", paste(unique(mismatched), collapse = ", "), immediate. = TRUE)
    return(invisible(FALSE))
  }

  message("Checksums OK for ", root_dir)
  invisible(TRUE)
}

run_dataprep <- function(input_root,
                         disturbance_dt_path,
                         study_area_path,
                         rtm_path,
                         study_area_name,
                         include_wind = FALSE,
                         force = FALSE) {
  input_root <- ensure_dir(input_root)
  if (!force && dir_has_data(input_root)) {
    message("Skipping DataPrep (already has data): ", input_root)
    return(invisible(TRUE))
  }

  module_data <- file.path(project_root, "modules", "anthroDisturbance_DataPrep", "data")
  dt <- read_disturbance_dt(disturbance_dt_path, file.path(module_data, "disturbanceDT.csv"))
  if (!isTRUE(include_wind)) {
    dt <- drop_wind_rows(dt)
  }

  study_area <- terra::vect(study_area_path)
  rtm <- terra::rast(rtm_path)

  paths <- list(
    modulePath  = file.path(project_root, "modules"),
    inputPath   = input_root,
    outputPath  = file.path(project_root, "outputs"),
    cachePath   = file.path(project_root, "scratch", "cache", "dataprep"),
    scratchPath = file.path(project_root, "scratch")
  )
  dir.create(paths$cachePath, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$scratchPath, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$outputPath, recursive = TRUE, showWarnings = FALSE)

  SpaDES.core::setPaths(
    modulePath  = paths$modulePath,
    inputPath   = paths$inputPath,
    outputPath  = paths$outputPath,
    cachePath   = paths$cachePath,
    scratchPath = paths$scratchPath
  )

  options(
    reproducible.cachePath = paths$cachePath,
    reproducible.destinationPath = paths$inputPath
  )

  terra::terraOptions(todisk = TRUE, memfrac = 0.6)
  try(googledrive::drive_deauth(), silent = TRUE)

  params <- list(
    anthroDisturbance_DataPrep = list(
      studyAreaName = study_area_name,
      useSavedList = FALSE,
      checkDisturbanceProportions = FALSE,
      whatNotToCombine = "potential"
    )
  )

  sim <- simInit(
    times   = list(start = 2011, end = 2011, timeunit = "year"),
    params  = params,
    modules = list("anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep"),
    paths   = paths,
    objects = list(
      disturbanceDT = dt,
      studyArea = study_area,
      rasterToMatch = rtm
    )
  )
  sim <- spades(sim)

  dt_out <- file.path(input_root, "disturbanceDT.csv")
  if (!file.exists(dt_out) || force) {
    data.table::fwrite(dt, dt_out, quote = TRUE)
  }

  message("DataPrep complete for ", input_root)
  invisible(TRUE)
}

download_drive_file <- function(url, dest, force = FALSE) {
  if (!force && file.exists(dest)) return(invisible(TRUE))
  id <- tryCatch(googledrive::as_id(url), error = function(...) NULL)
  if (is.null(id)) stop("Invalid Google Drive URL: ", url, call. = FALSE)
  googledrive::drive_deauth()
  googledrive::drive_download(file = id, path = dest, overwrite = TRUE)
  invisible(TRUE)
}

download_url_file <- function(url, dest, force = FALSE) {
  if (!force && file.exists(dest)) return(invisible(TRUE))
  utils::download.file(url, dest, mode = "wb", quiet = FALSE)
  invisible(TRUE)
}

download_bead_archives <- function(bead_root,
                                   force = FALSE,
                                   bead_2020_url = NULL,
                                   bead_2020_archive = NULL) {
  bead_root <- ensure_dir(bead_root)
  url_new <- "https://drive.google.com/file/d/1sxAa0wwwt7iwiHD7zB0DDnjfqyIQjKI2"
  url_old <- paste0(
    "https://www.ec.gc.ca/data_donnees/STB-DGST/003/",
    "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-OLD.zip"
  )

  archive_new <- file.path(bead_root, "ECCC_2015_anthro_dist_corrected_to_NT1_2016_final.zip")
  archive_old <- file.path(bead_root, "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip")
  archive_2020 <- file.path(bead_root, "NorthwestTerritories2020.gdb.zip")

  if (!file.exists(archive_new) || force) {
    message("Downloading BEAD 2015 archive to ", archive_new)
    download_drive_file(url_new, archive_new, force = TRUE)
  } else {
    message("BEAD 2015 archive present: ", archive_new)
  }

  if (!file.exists(archive_old) || force) {
    message("Downloading BEAD 2010 archive to ", archive_old)
    download_url_file(url_old, archive_old, force = TRUE)
  } else {
    message("BEAD 2010 archive present: ", archive_old)
  }

  if (file.exists(archive_2020)) {
    message("BEAD 2020 archive present: ", archive_2020)
    return(invisible(TRUE))
  }

  if (!is.null(bead_2020_archive) && nzchar(bead_2020_archive) && file.exists(bead_2020_archive)) {
    message("Copying BEAD 2020 archive from ", bead_2020_archive)
    file.copy(bead_2020_archive, archive_2020, overwrite = TRUE)
    return(invisible(TRUE))
  }

  if (!is.null(bead_2020_url) && nzchar(bead_2020_url)) {
    message("Downloading BEAD 2020 archive to ", archive_2020)
    if (grepl("drive.google.com", bead_2020_url, fixed = TRUE) ||
      grepl("docs.google.com", bead_2020_url, fixed = TRUE)) {
      download_drive_file(bead_2020_url, archive_2020, force = TRUE)
    } else {
      download_url_file(bead_2020_url, archive_2020, force = TRUE)
    }
    return(invisible(TRUE))
  }

  warning(paste0(
    "BEAD 2020 archive not found. Provide --bead-2020-archive or --bead-2020-url if needed: ",
    archive_2020
  ), immediate. = TRUE)
  invisible(FALSE)
}

option_list <- list(
  optparse::make_option(
    "--profile",
    type = "character",
    default = "all",
    help = "Comma-separated profiles to prepare: raw, bead, synthetic, ua_sa, or all (default all)."
  ),
  optparse::make_option(
    "--force",
    action = "store_true",
    default = FALSE,
    help = "Force re-download/rebuild even if data is present."
  ),
  optparse::make_option(
    "--include-wind",
    action = "store_true",
    default = FALSE,
    help = "Include wind/mif layers when preparing DataPrep inputs."
  ),
  optparse::make_option(
    "--skip-checksums",
    action = "store_true",
    default = FALSE,
    help = "Skip checksum verification even when CHECKSUMS.txt exists."
  ),
  optparse::make_option(
    "--verify-only",
    action = "store_true",
    default = FALSE,
    help = "Only verify checksums for selected profiles; skip downloads and DataPrep."
  ),
  optparse::make_option(
    "--bead-root",
    type = "character",
    default = file.path("data", "raw", "ECCC"),
    help = "Directory to store BEAD archives (default data/raw/ECCC)."
  ),
  optparse::make_option(
    "--bead-2020-url",
    type = "character",
    default = "",
    help = "Optional URL for NorthwestTerritories2020.gdb.zip (if missing locally)."
  ),
  optparse::make_option(
    "--bead-2020-archive",
    type = "character",
    default = "",
    help = "Optional local path to NorthwestTerritories2020.gdb.zip to copy into bead-root."
  )
)

opts <- optparse::parse_args(optparse::OptionParser(option_list = option_list))

profiles <- trimws(unlist(strsplit(opts$profile, ",", fixed = TRUE)))
profiles <- profiles[nzchar(profiles)]
if ("all" %in% profiles) {
  profiles <- c("raw", "bead", "synthetic")
}
profiles <- unique(profiles)

module_data <- file.path(project_root, "modules", "anthroDisturbance_DataPrep", "data")

data_raw_root <- normalize_path(file.path("data", "raw"), must_exist = FALSE)
bead_root <- normalize_path(opts$`bead-root`, must_exist = FALSE)
synthetic_root <- normalize_path(file.path("data", "synthetic", "rates", "synthetic_inputs"), must_exist = FALSE)

if ("raw" %in% profiles) {
  message("Preparing raw inputs under data/raw")
  if (!isTRUE(opts$`verify-only`)) {
    run_dataprep(
      input_root = data_raw_root,
      disturbance_dt_path = normalize_path(file.path("data", "raw", "disturbanceDT.csv"), must_exist = FALSE),
      study_area_path = normalize_path(file.path(module_data, "NT1_BCR6.shp"), must_exist = TRUE),
      rtm_path = normalize_path(file.path(module_data, "RTM.tif"), must_exist = TRUE),
      study_area_name = "NT1",
      include_wind = isTRUE(opts$`include-wind`),
      force = isTRUE(opts$force)
    )
  } else {
    message("verify-only enabled; skipping DataPrep for raw inputs.")
  }
  verify_checksums(data_raw_root, skip = isTRUE(opts$`skip-checksums`))
}

if ("bead" %in% profiles) {
  message("Preparing BEAD comparison inputs")
  if (!isTRUE(opts$`verify-only`)) {
    bead_input_root <- normalize_path(file.path("data", "preprocessed", "comparison", "BEAD2010"), must_exist = FALSE)
    study_area_path <- normalize_path(file.path("data", "study_area", "NWT_boundary.shp"), must_exist = FALSE)
    rtm_path <- normalize_path(file.path("data", "study_area", "NWT_RTM_250m.tif"), must_exist = FALSE)
    if (is.na(study_area_path) || !file.exists(study_area_path)) {
      stop("Missing BEAD study area: data/study_area/NWT_boundary.shp", call. = FALSE)
    }
    if (is.na(rtm_path) || !file.exists(rtm_path)) {
      stop("Missing BEAD rasterToMatch: data/study_area/NWT_RTM_250m.tif", call. = FALSE)
    }

    run_dataprep(
      input_root = bead_input_root,
      disturbance_dt_path = normalize_path(file.path("data", "raw", "disturbanceDT.csv"), must_exist = FALSE),
      study_area_path = study_area_path,
      rtm_path = rtm_path,
      study_area_name = "NWT",
      include_wind = isTRUE(opts$`include-wind`),
      force = isTRUE(opts$force)
    )

    download_bead_archives(
      bead_root = bead_root,
      force = isTRUE(opts$force),
      bead_2020_url = opts$`bead-2020-url`,
      bead_2020_archive = opts$`bead-2020-archive`
    )
  } else {
    message("verify-only enabled; skipping BEAD DataPrep + downloads.")
  }
  verify_checksums(bead_root, skip = isTRUE(opts$`skip-checksums`))
}

if ("synthetic" %in% profiles) {
  message("Verifying synthetic inputs under data/synthetic")
  if (!dir.exists(synthetic_root)) {
    warning("Synthetic input root not found: ", synthetic_root, immediate. = TRUE)
  } else {
    verify_checksums(synthetic_root, skip = isTRUE(opts$`skip-checksums`))
  }
}

if ("ua_sa" %in% profiles) {
  message("Preparing UA/SA prebuilt inputs")
  if (isTRUE(opts$`verify-only`)) {
    message("verify-only enabled; skipping UA/SA prebuild.")
  } else {
    status <- system2("Rscript", c(file.path(project_root, "workspace", "helpers", "prebuild_dataprep.R")))
    if (!identical(status, 0L)) {
      stop("prebuild_dataprep.R failed with status ", status, call. = FALSE)
    }
  }
}

message("Data preparation complete.")
