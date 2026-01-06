#!/usr/bin/env Rscript

# Prebuild disturbance inputs for UA/SA: run only the DataPrep modules,
# persist their outputs, and rewrite DisturbanceDT/ checksums to local files.

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(file_arg, args)
  if (length(hit)) {
    normalizePath(sub(file_arg, "", args[hit][1]), winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
}

script_path <- get_script_path()
project_root <- if (!is.na(script_path)) {
  normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(".", winslash = "/", mustWork = TRUE)
}
setwd(project_root)

if (file.exists("renv/activate.R")) {
  # Intentionally not loading renv here; rely on the current R library paths/environment.
}

suppressPackageStartupMessages({
  library(data.table)
  library(SpaDES.core)
  library(terra)
  library(reproducible)
  library(digest)
  library(googledrive)
})

resolve_first_existing <- function(paths) {
  hits <- paths[file.exists(paths)]
  if (length(hits)) normalizePath(hits[[1]], winslash = "/", mustWork = TRUE) else NA_character_
}

terra::terraOptions(todisk = TRUE, memfrac = 0.6)
try(googledrive::drive_deauth(), silent = TRUE)

paths <- list(
  modulePath  = file.path(project_root, "modules"),
  inputPath   = file.path(project_root, "data"),
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
  reproducible.destinationPath = paths$outputPath
)

module_data <- file.path(paths$modulePath, "anthroDisturbance_DataPrep", "data")
disturbance_dt_path <- resolve_first_existing(c(
  file.path(paths$inputPath, "raw", "disturbanceDT.csv"),
  file.path(module_data, "disturbanceDT.csv")
))
study_area_path <- resolve_first_existing(c(
  file.path(paths$inputPath, "study_area", "aoi_southwest_NWT.shp"),
  file.path(module_data, "NT1_BCR6.shp")
))
rtm_path <- resolve_first_existing(c(
  file.path(paths$inputPath, "study_area", "aoi_southwest_NWT_RTM_250m.tif"),
  file.path(paths$inputPath, "raw", "RTM.tif"),
  file.path(module_data, "RTM.tif")
))
if (is.na(disturbance_dt_path)) stop("disturbanceDT.csv not found under data/raw or module defaults.")
if (is.na(study_area_path)) stop("Study area shapefile not found under data/study_area or module defaults.")
if (is.na(rtm_path)) stop("rasterToMatch not found under data/study_area, data/raw, or module defaults.")

times <- list(start = 2011, end = 2051, timeunit = "year")
modules <- list("anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep")

objects <- list(
  disturbanceDT = data.table::fread(disturbance_dt_path),
  studyArea     = terra::vect(study_area_path),
  rasterToMatch = terra::rast(rtm_path)
)

if (any(objects$disturbanceDT$dataType == "mif")) {
  message("Dropping MIF-based entries (e.g., potentialWindTurbines) to avoid upstream GDAL assertions.")
  objects$disturbanceDT <- objects$disturbanceDT[dataType != "mif"]
}

params <- list(
  anthroDisturbance_DataPrep = list(
    studyAreaName = "ua_sa",
    useSavedList = FALSE,
    checkDisturbanceProportions = FALSE,
    whatNotToCombine = "potential"
  )
)

sim <- simInit(
  times   = times,
  params  = params,
  modules = modules,
  paths   = paths,
  objects = objects
)
sim <- spades(sim)

pot_names <- unlist(lapply(sim$disturbanceList, function(x) names(x)[grepl("potential", names(x))]))
if (!length(pot_names)) {
  stop("No potential layers found in disturbanceList after DataPrep; check inputs.")
}

prebuilt <- list(
  disturbanceList = sim$disturbanceList,
  disturbanceDT   = sim$disturbanceDT,
  studyArea       = sim$studyArea,
  rasterToMatch   = sim$rasterToMatch,
  meta = list(
    created        = Sys.time(),
    config_version = "1",
    description    = "Prebuilt DataPrep (anthroDisturbance_DataPrep + potentialResourcesNT_DataPrep) for UA/SA",
    aoi            = "aoi_southwest_NWT",
    resolution     = if (inherits(sim$rasterToMatch, "SpatRaster")) terra::res(sim$rasterToMatch) else NA_real_
  )
)

ua_sa_dir <- file.path(paths$inputPath, "preprocessed", "ua_sa")
dir.create(ua_sa_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(prebuilt, file.path(ua_sa_dir, "disturbance_inputs.rds"))

normalize_file_url <- function(u) {
  if (is.null(u)) return(NA_character_)
  u <- as.character(u)
  u[!nzchar(u)] <- NA_character_
  out <- ifelse(startsWith(u, "file://"), substring(u, 8), u)
  out
}

local_storage <- file.path(ua_sa_dir, "raw_inputs")
dir.create(local_storage, recursive = TRUE, showWarnings = FALSE)

disturbanceDT <- data.table::copy(sim$disturbanceDT)
disturbanceDT[, source_path := normalize_file_url(URL)]
disturbanceDT[!is.na(source_path),
              source_path := normalizePath(source_path, winslash = "/", mustWork = FALSE)]

copy_map <- disturbanceDT[!is.na(source_path) & file.exists(source_path),
                          .(source_path = unique(source_path))]
copy_map[, dest := normalizePath(file.path(local_storage, basename(source_path)),
                                 winslash = "/", mustWork = FALSE)]

for (i in seq_len(nrow(copy_map))) {
  from <- copy_map$source_path[[i]]
  to <- copy_map$dest[[i]]
  if (!file.exists(to)) {
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(from = from, to = to, overwrite = FALSE)
  }
}

disturbanceDT[!is.na(source_path) & file.exists(source_path),
              URL := paste0("file://", normalizePath(file.path(local_storage,
                                                               basename(source_path)),
                                                    winslash = "/", mustWork = FALSE))]
disturbanceDT[, source_path := NULL]

data.table::fwrite(disturbanceDT,
                   file.path(ua_sa_dir, "DisturbanceDT.csv"),
                   quote = TRUE)

local_files <- unique(gsub("^file://", "", disturbanceDT$URL))
local_files <- local_files[file.exists(local_files)]
if (length(local_files)) {
  checks <- data.table::data.table(
    file = normalizePath(local_files, winslash = "/", mustWork = TRUE),
    checksum = vapply(local_files, digest::digest, character(1),
                      algo = "xxhash64", file = TRUE),
    algorithm = "xxhash64",
    filesize = file.info(local_files)$size
  )
  data.table::fwrite(checks,
                     file.path(ua_sa_dir, "checksums.txt"),
                     sep = " ",
                     quote = TRUE)
}

message("DataPrep prebuild complete. Outputs in: ", ua_sa_dir)
