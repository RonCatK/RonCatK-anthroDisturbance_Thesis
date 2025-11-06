#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  requireNamespace("terra")
  requireNamespace("glue")
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(flag, default = NULL) {
  hit <- args[grepl(paste0("^", flag, "="), args, ignore.case = TRUE)]
  if (!length(hit)) return(default)
  sub(paste0("^", flag, "="), "", hit[1], ignore.case = TRUE)
}

startYear <- as.integer(parse_arg("--start-year", 2020))
endYear <- as.integer(parse_arg("--end-year", 2030))
burnRate <- as.numeric(parse_arg("--burn-rate", 0.018))
clusterRadius <- as.numeric(parse_arg("--cluster-radius", 500)) # metres
regrowthYears <- as.integer(parse_arg("--regrowth-years", 5))
seedBase <- as.integer(parse_arg("--seed", 1349))
maskDir <- parse_arg(
  "--mask-dir",
  Sys.getenv("UA_FIRE_MASK_DIR", unset = file.path(getwd(), "data", "raw", "validation", "ua", "fire"))
)
inputsPath <- parse_arg(
  "--inputs-script",
  file.path(getwd(), "validation", "ua", "ua_inputs.R")
)

if (is.na(startYear) || is.na(endYear) || startYear > endYear) {
  stop("Invalid start/end year supplied.", call. = FALSE)
}
if (!nzchar(maskDir)) {
  stop("mask-dir must point to a writable directory", call. = FALSE)
}
maskDir <- normalizePath(maskDir, winslash = "/", mustWork = FALSE)
if (!dir.exists(maskDir)) dir.create(maskDir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(inputsPath)) {
  stop("Unable to locate ua_inputs script at ", inputsPath, call. = FALSE)
}

source(inputsPath)

if (!exists("studyArea") || !exists("rasterToMatch")) {
  stop("ua_inputs.R must create objects named studyArea and rasterToMatch.", call. = FALSE)
}

rtm <- rasterToMatch
study <- studyArea
resxy <- terra::res(rtm)
cellArea <- abs(resxy[1] * resxy[2])
validCells <- which(!is.na(terra::values(rtm)))
validCount <- length(validCells)
totalArea <- validCount * cellArea

terra::crs(study) <- terra::crs(rtm)

ageMap <- terra::rast(rtm)
ageMap[] <- regrowthYears + 1

burn_masks <- list()

for (yr in seq(startYear, endYear)) {
  set.seed(seedBase + yr)

  targetArea <- burnRate * totalArea
  clusterArea <- pi * (clusterRadius ^ 2)
  nSeeds <- max(1, round(targetArea / clusterArea))

  ageVals <- ageMap[]
  weights <- pmax(ageVals, 0)
  weights <- weights / max(weights, na.rm = TRUE)
  weights[is.na(weights)] <- 0
  weights <- weights ^ 1.5
  if (sum(weights, na.rm = TRUE) == 0) {
    weights <- rep(1, length(weights))
  }

  seedCells <- sample(validCells, size = min(nSeeds, validCount), replace = FALSE, prob = weights[validCells])
  coords <- terra::xyFromCell(rtm, seedCells)
  pts <- terra::vect(coords, type = "points", crs = terra::crs(rtm))
  buffers <- terra::buffer(pts, width = clusterRadius)
  buffers <- terra::intersect(buffers, study)

  burnRaster <- terra::rast(rtm)
  if (terra::nrow(buffers) > 0) {
    burnRaster <- terra::rasterize(buffers, burnRaster, field = 1, background = 0, touches = TRUE)
  } else {
    burnRaster[] <- 0
  }

  burnRaster <- terra::mask(burnRaster, study, updatevalue = 0)
  burnRaster <- terra::ifel(is.na(burnRaster), 0, burnRaster)
  burnRaster <- terra::ifel(burnRaster > 0, 1, 0)

  ageMap <- ageMap + 1
  ageMap <- terra::ifel(is.na(ageMap), regrowthYears + 1, ageMap)
  aged <- ageMap
  aged[burnRaster == 1] <- 0
  ageMap <- aged

  outFile <- file.path(maskDir, sprintf("rstCurrentBurn_%04d.tif", yr))
  terra::writeRaster(burnRaster, outFile, datatype = "INT1U", overwrite = TRUE)
  message(glue::glue("Year {yr}: wrote {outFile} (burned ~{round(100*sum(burnRaster[], na.rm=TRUE)/validCount, 2)}% of cells)"))
}

message("Synthetic fire mask generation complete.")
