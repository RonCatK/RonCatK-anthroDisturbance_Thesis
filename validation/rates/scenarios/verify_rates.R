#!/usr/bin/env Rscript

## Verification script for synthetic disturbance-rate runs
## ---------------------------------------------------------
## Calculates (1) baseline statistics from the inputs, (2) target disturbed
## area derived from the supplied DisturbanceRate table, and (3) realized
## disturbance added by the module outputs. Results are written to a CSV
## summary and echoed to the console.

suppressPackageStartupMessages({
  library(data.table)
  library(terra)
  library(sf)
})

args <- commandArgs(trailingOnly = FALSE)
scriptFile <- sub("^--file=", "", args[grep("^--file=", args)])
scriptDir <- if (length(scriptFile)) {
  normalizePath(dirname(scriptFile), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
trailing <- commandArgs(trailingOnly = TRUE)
scenarioIdArg <- if (length(trailing) >= 1) trailing[1] else "scenario_rate_test_1"
scenarioRoot <- normalizePath(file.path(scriptDir, "..", ".."), winslash = "/", mustWork = TRUE)
projectRoot <- normalizePath(file.path(scenarioRoot, ".."), winslash = "/", mustWork = TRUE)
# locate testing root relative to project
scenarioCsv <- file.path(projectRoot, "validation", "rates", "scenarios", "scenarios.csv")
if (!file.exists(scenarioCsv)) {
  stop("Scenario table not found at ", scenarioCsv, call. = FALSE)
}
scenarioTable <- fread(scenarioCsv)
scenarioRow <- scenarioTable[scenario_id == scenarioIdArg]
if (!nrow(scenarioRow)) stop("Scenario not found in scenarios.csv: ", scenarioIdArg)
scenarioId <- scenarioRow$scenario_id[1]
scenarioStart <- scenarioRow$start_year[1]
scenarioEnd <- scenarioRow$end_year[1]
scenarioDurationYears <- scenarioEnd - scenarioStart
if (!is.finite(scenarioDurationYears) || scenarioDurationYears <= 0) {
  warning("Scenario duration is non-positive; defaulting to 1 year for rate calculations.")
  scenarioDurationYears <- 1
}

rateFile <- scenarioRow$disturbance_rate_file[1]
if (is.na(rateFile) || !nzchar(rateFile)) {
  stop("Scenario does not reference a disturbanceRateFile; supply one to run verification.")
}
rateFile <- normalizePath(rateFile, winslash = "/", mustWork = TRUE)

syntheticRoot <- Sys.getenv("VALIDATION_SYNTHETIC_ROOT",
                             unset = file.path(projectRoot, "data", "synthetic"))
candidateStats <- c(
  file.path(syntheticRoot, "rates", "medium_aoi_optimal_stats.csv"),
  file.path(projectRoot, "validation", "rates", "medium_aoi_optimal_stats.csv"),
  file.path(scenarioRoot, "medium_aoi_optimal_stats.csv")
)
candidateGpkg <- c(
  file.path(syntheticRoot, "rates", "medium_aoi_optimal.gpkg"),
  file.path(projectRoot, "validation", "rates", "medium_aoi_optimal.gpkg"),
  file.path(scenarioRoot, "medium_aoi_optimal.gpkg")
)
statsPath <- NULL
gpkgPath <- NULL
for (cand in candidateStats) {
  if (file.exists(cand)) {
    statsPath <- cand
    break
  }
}
for (cand in candidateGpkg) {
  if (file.exists(cand)) {
    gpkgPath <- cand
    break
  }
}
if (is.null(statsPath)) stop("Stats CSV missing in data/synthetic/rates or validation/rates.")
if (is.null(gpkgPath)) stop("Synthetic GeoPackage missing in data/synthetic/rates or validation/rates.")
statsPath <- normalizePath(statsPath, winslash = "/", mustWork = TRUE)
gpkgPath <- normalizePath(gpkgPath, winslash = "/", mustWork = TRUE)
studyAreaCandidates <- c(
  file.path(projectRoot, "data", "study_area", "aoi_southwest_NWT.shp"),
  file.path(projectRoot, "data", "study_area", "aoi_southwest_NWT.gpkg"),
  file.path(projectRoot, "data", "medium_aoi.shp"),
  file.path(projectRoot, "data", "medium_aoi.gpkg"),
  file.path(scenarioRoot, "medium_aoi.shp"),
  file.path(scenarioRoot, "medium_aoi.gpkg")
)
studyAreaPath <- NULL
for (cand in studyAreaCandidates) {
  if (file.exists(cand)) {
    studyAreaPath <- cand
    break
  }
}
if (is.null(studyAreaPath)) stop("Study area not found in expected locations.", call. = FALSE)

message("Scenario: ", scenarioId)
message("  Using rate file: ", rateFile)
message("  Years simulated: ", scenarioStart, "-", scenarioEnd,
        " (", scenarioDurationYears, " years)")
baselineStats <- fread(statsPath, na.strings = c("", "NA"))
rateTable <- fread(rateFile)

studyArea <- vect(studyAreaPath)
studyAreaProjected <- tryCatch(
  terra::project(studyArea, "EPSG:3347"),
  error = function(e) studyArea
)
studyAreaAreaKm2 <- sum(expanse(studyAreaProjected, unit = "km"), na.rm = TRUE)
message(sprintf("  Study area: %.1f km^2", studyAreaAreaKm2))

statsWide <- copy(baselineStats)
numCols <- c("total_area_km2", "total_length_km", "point_count")
statsWide[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
statsWide <- statsWide[, .(
  layerName = dataClass,
  geometry,
  area_km2 = total_area_km2,
  length_km = total_length_km,
  point_count = point_count
)]

potentialStats <- copy(statsWide)
setnames(
  potentialStats,
  old = c("layerName", "geometry", "area_km2", "length_km", "point_count"),
  new = c("dataClass", "potential_geometry", "potential_area_km2",
          "potential_length_km", "potential_point_count")
)
baselineStatsForRealized <- copy(statsWide)
setnames(
  baselineStatsForRealized,
  old = c("layerName", "geometry", "area_km2", "length_km", "point_count"),
  new = c("realizedLayer", "baseline_geometry", "baseline_area_km2",
          "baseline_length_km", "baseline_point_count")
)

rateTable[, scenarioId := scenarioId]
rateTable[, realizedLayer := fifelse(
  disturbanceType == "Connecting" & !is.na(disturbanceEnd) & nzchar(disturbanceEnd),
  disturbanceEnd,
  disturbanceOrigin
)]
rateTable <- merge(rateTable, potentialStats, by = "dataClass", all.x = TRUE)
rateTable <- merge(rateTable, baselineStatsForRealized, by = "realizedLayer", all.x = TRUE)
rateTable[, target_area_km2 := fifelse(
  !is.na(disturbanceRate),
  (disturbanceRate / 100) * studyAreaAreaKm2 * scenarioDurationYears,
  NA_real_
)]

runDirs <- list.files(file.path(projectRoot, "outputs", "rates"),
                      pattern = paste0("^rates_", scenarioId, "_"),
                      full.names = FALSE)
if (!length(runDirs)) stop("No run directory found for scenario ", scenarioId)
runDirs <- sort(runDirs, decreasing = TRUE)
runDir <- file.path(projectRoot, "outputs", "rates", runDirs[1])
message("  Run directory: ", runDirs[1])

should_skip <- function(fname) {
  any(grepl("_IC_", fname, fixed = TRUE),
      grepl("_newOnly_", fname, fixed = TRUE))
}

parse_meta <- function(fname) {
  if (should_skip(fname)) return(NULL)
  base <- sub("\\.shp$", "", fname)
  if (!grepl("^disturbances_", base)) return(NULL)
  year_match <- regexec("_([0-9]{4})_", base, perl = TRUE)
  match_components <- regmatches(base, year_match)[[1]]
  if (length(match_components) < 2) return(NULL)
  year <- suppressWarnings(as.integer(match_components[2]))
  if (is.na(year)) return(NULL)
  core <- sub(paste0("_", year, "_.*$"), "", sub("^disturbances_", "", base))
  parts <- strsplit(core, "_", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  list(
    sector = parts[1],
    layer = paste(parts[-1], collapse = "_"),
    year = year
  )
}

summarise_vector <- function(path, meta, scenarioStart) {
  vec <- tryCatch(vect(path), error = function(e) NULL)
  if (is.null(vec)) return(NULL)
  filtered_flag <- FALSE
  if ("createdInS" %in% names(vec)) {
    created_vals <- vec[["createdInS"]]
    if (is.null(created_vals)) {
      created_vals <- NA_real_
    } else if (is.data.frame(created_vals)) {
      created_vals <- created_vals[[1]]
    }
    created_vals <- suppressWarnings(as.numeric(created_vals))
    idx <- which(!is.na(created_vals) & created_vals > scenarioStart)
    if (!length(idx)) return(NULL)
    vec <- vec[idx, ]
    filtered_flag <- TRUE
  }
  if (terra::nrow(vec) == 0) return(NULL)
  geomType <- tolower(geomtype(vec))
  area_km2 <- if (geomType == "polygons") {
    sum(expanse(vec, unit = "km"), na.rm = TRUE)
  } else NA_real_
  length_km <- if (geomType == "lines") {
    sfObj <- sf::st_as_sf(vec)
    sum(as.numeric(sf::st_length(sfObj)), na.rm = TRUE) / 1000
  } else NA_real_
  count <- if (geomType == "points") nrow(vec) else NA_real_
  data.table(
    dataName = meta$sector,
    realizedLayer = meta$layer,
    year = meta$year,
    realized_geometry = geomType,
    total_area_km2 = area_km2,
    total_length_km = length_km,
    total_count = count,
    filtered_created = filtered_flag
  )
}

list_shapefiles <- function(rootDir) {
  top <- list.files(rootDir, pattern = "\\.shp$", full.names = TRUE)
  subdirs <- list.dirs(rootDir, recursive = TRUE, full.names = TRUE)
  subdirs <- subdirs[subdirs != rootDir]
  pkg_shps <- unlist(lapply(subdirs, function(dirPath) {
    shpFiles <- list.files(dirPath, pattern = "\\.shp$", full.names = TRUE)
    zipPaths <- list.files(dirPath, pattern = "\\.zip$", full.names = TRUE)
    if (length(zipPaths)) {
      extra <- unlist(lapply(zipPaths, function(zp) {
        zipInfo <- try(utils::unzip(zp, list = TRUE), silent = TRUE)
        if (inherits(zipInfo, "try-error")) return(character())
        shpNames <- zipInfo$Name[grepl("\\.shp$", zipInfo$Name, ignore.case = TRUE)]
        if (!length(shpNames)) return(character())
        zpNorm <- normalizePath(zp, winslash = "/", mustWork = TRUE)
        sprintf("/vsizip/%s/%s", zpNorm, shpNames)
      }))
      shpFiles <- c(shpFiles, extra)
    }
    shpFiles
  }), recursive = FALSE)
  unique(c(top, pkg_shps))
}

shpFiles <- list_shapefiles(runDir)
vectorSummaries <- lapply(shpFiles, function(path) {
  meta <- parse_meta(basename(path))
  if (is.null(meta)) return(NULL)
  summarise_vector(path, meta, scenarioStart)
})
vectorSummaries <- Filter(Negate(is.null), vectorSummaries)
realizedByYear <- if (length(vectorSummaries)) {
  rbindlist(vectorSummaries, fill = TRUE)
} else {
  warning("No disturbance layers discovered in ", runDir)
  data.table(
    dataName = character(),
    realizedLayer = character(),
    year = integer(),
    realized_geometry = character(),
    total_area_km2 = numeric(),
    total_length_km = numeric(),
    total_count = numeric(),
    filtered_created = logical()
  )
}

setorderv(realizedByYear, c("dataName", "realizedLayer", "year"))
realizedLatest <- realizedByYear[!is.na(year), .SD[.N], by = .(dataName, realizedLayer)]
setnames(realizedLatest, "year", "latest_year")

rateSummary <- rateTable[, .(
  scenarioId,
  dataName,
  dataClass,
  disturbanceType,
  disturbanceOrigin,
  disturbanceEnd,
  realizedLayer,
  disturbanceRate,
  target_area_km2,
  potential_geometry,
  potential_area_km2,
  potential_length_km,
  potential_point_count,
  baseline_geometry,
  baseline_area_km2,
  baseline_length_km,
  baseline_point_count
)]

joined <- merge(rateSummary, realizedLatest,
                by = c("dataName", "realizedLayer"),
                all.x = TRUE)

joined[, realized_total_area_km2 := fifelse(!is.na(total_area_km2),
                                            total_area_km2,
                                            0)]
joined[, realized_total_length_km := fifelse(!is.na(total_length_km),
                                             total_length_km,
                                             0)]
joined[, realized_total_count := fifelse(!is.na(total_count),
                                         total_count,
                                         0)]

joined[, filtered_created := as.logical(filtered_created)]

joined[, realized_new_area_km2 := fifelse(
  !is.na(realized_total_area_km2),
  fifelse(filtered_created %in% TRUE,
          realized_total_area_km2,
          ifelse(!is.na(baseline_area_km2),
                 pmax(realized_total_area_km2 - baseline_area_km2, 0),
                 realized_total_area_km2)),
  NA_real_
)]

joined[, realized_new_length_km := fifelse(
  !is.na(realized_total_length_km),
  fifelse(filtered_created %in% TRUE,
          realized_total_length_km,
          ifelse(!is.na(baseline_length_km),
                 pmax(realized_total_length_km - baseline_length_km, 0),
                 realized_total_length_km)),
  NA_real_
)]

joined[, realized_new_count := fifelse(
  !is.na(realized_total_count),
  fifelse(filtered_created %in% TRUE,
          realized_total_count,
          ifelse(!is.na(baseline_point_count),
                 pmax(realized_total_count - baseline_point_count, 0),
                 realized_total_count)),
  NA_real_
)]

joined[, actual_rate_pct := fifelse(
  !is.na(realized_new_area_km2) & studyAreaAreaKm2 > 0 & scenarioDurationYears > 0,
  (realized_new_area_km2 / (studyAreaAreaKm2 * scenarioDurationYears)) * 100,
  NA_real_
)]
joined[, rate_gap_pct := fifelse(
  !is.na(actual_rate_pct) & !is.na(disturbanceRate),
  actual_rate_pct - disturbanceRate,
  NA_real_
)]

joined[, `:=`(
  disturbanceRate = signif(disturbanceRate, digits = 6),
  actual_rate_pct = ifelse(is.na(actual_rate_pct), NA_real_, round(actual_rate_pct, 3)),
  rate_gap_pct = ifelse(is.na(rate_gap_pct), NA_real_, round(rate_gap_pct, 3))
)]

joined[, studyArea_km2 := studyAreaAreaKm2]
joined[, scenario_start := scenarioStart]
joined[, scenario_end := scenarioEnd]
joined[, scenario_duration_years := scenarioDurationYears]
if ("latest_year" %in% names(joined)) {
  mismatched <- joined[!is.na(latest_year) & latest_year < scenario_end]
  if (nrow(mismatched)) {
    warning("Some layers end before scenario_end (",
            scenarioRow$endYear, "): ",
            paste(unique(mismatched$realizedLayer), collapse = ", "))
  }
}

setorder(joined, disturbanceType, dataName, realizedLayer)

coalesce <- function(x, y) {
  ifelse(is.na(x), y, x)
}

console_summary <- function(row) {
  prefix <- sprintf("[%s/%s] %s -> %s",
                    row$disturbanceType,
                    row$dataName,
                    row$dataClass,
                    row$realizedLayer)
  if (!is.na(row$realized_new_area_km2)) {
    targetTxt <- if (!is.na(row$target_area_km2)) {
      sprintf("target %.3f km^2", row$target_area_km2)
    } else "target n/a"
    rateVal <- coalesce(row$actual_rate_pct, NA_real_)
    actualTxt <- if (!is.na(rateVal)) {
      sprintf("realized %.3f km^2 (%.3f%%)", row$realized_new_area_km2, rateVal)
    } else {
      sprintf("realized %.3f km^2", row$realized_new_area_km2)
    }
    gapTxt <- if (!is.na(row$rate_gap_pct)) {
      sprintf("gap %+0.3f%%", row$rate_gap_pct)
    } else "gap n/a"
    message(prefix, " | ", targetTxt, " | ", actualTxt, " | ", gapTxt)
  } else if (!is.na(row$realized_new_length_km)) {
    message(prefix, " | realized ", sprintf("%.3f km", row$realized_new_length_km))
  } else if (!is.na(row$realized_new_count)) {
    message(prefix, " | realized ", sprintf("%.0f features", row$realized_new_count))
  } else {
    message(prefix, " | no outputs detected")
  }
}

if (nrow(joined)) {
  for (i in seq_len(nrow(joined))) {
    console_summary(joined[i])
  }
}

priorityCols <- c(
  "scenarioId",
  "dataName",
  "realizedLayer",
  "disturbanceType",
  "disturbanceOrigin",
  "disturbanceEnd",
  "disturbanceRate",
  "actual_rate_pct",
  "rate_gap_pct",
  "target_area_km2",
  "realized_new_area_km2",
  "realized_new_length_km",
  "realized_new_count",
  "latest_year",
  "realized_geometry",
  "studyArea_km2",
  "scenario_start",
  "scenario_end",
  "scenario_duration_years"
)
otherCols <- setdiff(names(joined), priorityCols)
setcolorder(joined, c(priorityCols, otherCols))

timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
runOutputPath <- file.path(
  runDir,
  sprintf("verification_%s_%s.csv", scenarioId, timestamp)
)
fwrite(joined, runOutputPath)
message("Verification written to ", runOutputPath)
