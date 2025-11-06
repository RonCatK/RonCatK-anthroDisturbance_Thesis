#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  requireNamespace("SpaDES.core")
  requireNamespace("reproducible")
  requireNamespace("terra")
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(flag, default = NULL) {
  hit <- args[grepl(paste0("^", flag, "="), args, ignore.case = TRUE)]
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[1], ignore.case = TRUE)
}

startYear <- as.integer(parse_arg("--start-year", 2020))
endYear <- as.integer(parse_arg("--end-year", 2030))
fireModule <- parse_arg("--fire-module", "scfm")
modulePathArg <- parse_arg("--module-path", NA_character_)
outputDirArg <- parse_arg(
  "--output-dir",
  file.path(getwd(), "scratch", "validation", "ua", "fire_precompute")
)
maskDirArg <- parse_arg(
  "--mask-dir",
  Sys.getenv("UA_FIRE_MASK_DIR", unset = file.path(getwd(), "data", "raw", "validation", "ua", "fire"))
)
configPath <- parse_arg("--config", NA_character_)

if (is.na(startYear) || is.na(endYear)) {
  stop("start-year and end-year must be numeric.", call. = FALSE)
}
if (startYear > endYear) {
  stop("start-year cannot be greater than end-year.", call. = FALSE)
}

moduleCandidates <- unique(c(
  modulePathArg,
  file.path(getwd(), "modules"),
  file.path(getwd(), "modules", fireModule)
))
moduleCandidates <- moduleCandidates[nzchar(moduleCandidates)]
moduleCandidates <- moduleCandidates[dir.exists(moduleCandidates)]
if (!length(moduleCandidates)) {
  stop("Unable to resolve a modulePath that contains ", fireModule, ".", call. = FALSE)
}

maskDir <- normalizePath(maskDirArg, winslash = "/", mustWork = FALSE)
if (!dir.exists(maskDir)) dir.create(maskDir, recursive = TRUE, showWarnings = FALSE)
outputDir <- normalizePath(outputDirArg, winslash = "/", mustWork = FALSE)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
cacheDir <- file.path(outputDir, "cache")
scratchDir <- file.path(outputDir, "scratch")
dir.create(cacheDir, recursive = TRUE, showWarnings = FALSE)
dir.create(scratchDir, recursive = TRUE, showWarnings = FALSE)

source(file.path(getwd(), "validation", "ua", "ua_inputs.R"))

config_env <- new.env(parent = baseenv())
if (!is.na(configPath)) {
  if (!file.exists(configPath)) {
    stop("Config file not found: ", configPath, call. = FALSE)
  }
  sys.source(configPath, envir = config_env)
}

fireModules <- config_env$fire_modules %||% fireModule
rawFireParams <- config_env$fire_params %||% list()
fireObjects <- config_env$fire_objects %||% list()

objects <- modifyList(
  list(studyArea = studyArea, rasterToMatch = rasterToMatch),
  fireObjects
)

paths <- list(
  modulePath = moduleCandidates,
  outputPath = outputDir,
  cachePath = cacheDir,
  scratchPath = scratchDir,
  inputPath = Sys.getenv("SCFM_INPUT_PATH", unset = file.path(getwd(), "data", "raw"))
)

times <- list(start = startYear, end = endYear)
params <- list()
if (is.list(rawFireParams) && length(rawFireParams) && !is.null(names(rawFireParams)) &&
    all(names(rawFireParams) %in% fireModules)) {
  params <- rawFireParams
} else {
    params[[fireModules[1]]] <- rawFireParams
}

message("Running fire module ", paste(fireModules, collapse = ", "),
        " for years ", startYear, "-", endYear)

sim <- SpaDES.core::simInit(
  times = times,
  params = params,
  modules = fireModules,
  objects = objects,
  paths = paths
)
sim <- SpaDES.core::spades(sim)

yrs <- seq(startYear, endYear)
for (yr in yrs) {
  pattern <- sprintf("rstCurrentBurn.*%d", yr)
  burnFiles <- list.files(
    paths$outputPath,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  if (!length(burnFiles)) {
    warning("No fire raster found for year ", yr, ". Skipping.", call. = FALSE)
    next
  }
  fi <- burnFiles[which.max(file.info(burnFiles)$mtime)]
  r <- terra::rast(fi)
  if (!terra::compareGeom(r, rasterToMatch, stopOnError = FALSE)) {
    r <- terra::project(r, rasterToMatch, method = "near")
    r <- terra::resample(r, rasterToMatch, method = "near")
    r <- terra::mask(r, studyArea)
  }
  outFile <- file.path(maskDir, sprintf("rstCurrentBurn_%04d.tif", yr))
  terra::writeRaster(r, outFile, overwrite = TRUE)
  message("Saved ", outFile)
}

message("Fire mask precompute complete.")
