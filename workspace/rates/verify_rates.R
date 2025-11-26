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
  library(yaml)
})

args_full <- commandArgs(trailingOnly = FALSE)
scriptFile <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
scriptDir <- if (length(scriptFile)) {
  normalizePath(dirname(scriptFile), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
suiteRoot <- scriptDir
projectRoot <- normalizePath(file.path(scriptDir, "..", ".."), winslash = "/", mustWork = TRUE)

parse_cli <- function(trailing) {
  opts <- list(config = NULL, scenario = NULL)
  if (!length(trailing)) return(opts)
  for (arg in trailing) {
    if (grepl("^--config=", arg, ignore.case = TRUE)) {
      opts$config <- sub("^--config=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--scenario=", arg, ignore.case = TRUE)) {
      opts$scenario <- sub("^--scenario=", "", arg, ignore.case = TRUE)
    } else if ((is.null(opts$config) || !nzchar(opts$config)) &&
               grepl("\\.ya?ml$", arg, ignore.case = TRUE)) {
      opts$config <- arg
    } else if (is.null(opts$scenario) || !nzchar(opts$scenario)) {
      opts$scenario <- arg
    }
  }
  opts
}

trailing <- commandArgs(trailingOnly = TRUE)
cli <- parse_cli(trailing)

resolve_config_from_scenario <- function(scenario) {
  if (is.null(scenario) || !nzchar(scenario)) return(NULL)
config_dir <- file.path(projectRoot, "workspace", "rates", "config")
  if (!dir.exists(config_dir)) return(NULL)
  candidates <- list.files(config_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (!length(candidates)) return(NULL)
  for (path in candidates) {
    cfg <- tryCatch(yaml::read_yaml(path), error = function(...) NULL)
    if (is.null(cfg)) next
    scenario_id <- cfg$metadata$scenario_id
    run_name <- cfg$run_name
    if (identical(scenario, scenario_id) || identical(scenario, run_name)) {
      return(path)
    }
  }
  NULL
}

configPath <- cli$config
if (is.null(configPath)) {
  configPath <- resolve_config_from_scenario(cli$scenario)
}
if (is.null(configPath)) {
  config_dir <- file.path(projectRoot, "workspace", "rates", "config")
  candidates <- list.files(config_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (!length(candidates)) stop("No config specified and none found under workspace/rates/config.", call. = FALSE)
  configPath <- candidates[[1]]
  message("Config not specified; defaulting to ", basename(configPath))
}
if (!file.exists(configPath)) {
  altPath <- sub("config/generated", "config", configPath, fixed = TRUE)
  if (file.exists(altPath)) {
    configPath <- altPath
  }
}
configPath <- normalizePath(configPath, winslash = "/", mustWork = TRUE)
cfg <- yaml::read_yaml(configPath)
genParams <- cfg$params$anthroDisturbance_Generator %||% list()
bufferPolygons500m <- isTRUE(genParams$disturbanceRateRelatesToBufferedArea)
defaultLineBuffer <- suppressWarnings(as.numeric(genParams$growthStepEnlargingLines))
if (length(defaultLineBuffer) == 0 || is.na(defaultLineBuffer) || defaultLineBuffer <= 0) {
  defaultLineBuffer <- 30
}
lineBufferMeters <- if (bufferPolygons500m) 500 else defaultLineBuffer
polygonBufferMeters <- if (bufferPolygons500m) 500 else 0

scenarioId <- cfg$metadata$scenario_id %||% cfg$run_name %||% tools::file_path_sans_ext(basename(configPath))
runName <- cfg$run_name %||% scenarioId
suiteName <- cfg$suite %||% "rates"
scenarioStart <- suppressWarnings(as.numeric(cfg$times$start))
scenarioEnd <- suppressWarnings(as.numeric(cfg$times$end))
if (!is.finite(scenarioStart) || !is.finite(scenarioEnd)) {
  stop("Config missing valid times$start / times$end values: ", configPath, call. = FALSE)
}
scenarioDurationYears <- scenarioEnd - scenarioStart
if (!is.finite(scenarioDurationYears) || scenarioDurationYears <= 0) {
  warning("Scenario duration is non-positive; defaulting to 1 year for rate calculations.")
  scenarioDurationYears <- 1
}

rateFile <- cfg$disturbance_rate_file %||% cfg$disturbanceRateFile
if (is.null(rateFile) || !nzchar(rateFile)) {
  stop("Config does not specify disturbance_rate_file; cannot verify.", call. = FALSE)
}
if (!grepl("^(/|[A-Za-z]:)", rateFile)) {
  rateFile <- file.path(projectRoot, rateFile)
}
rateFile <- normalizePath(rateFile, winslash = "/", mustWork = TRUE)

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.logical(a) && length(a) == 1 && is.na(a)) return(b)
  if (is.character(a) && !nzchar(a[1])) return(b)
  a
}

resolve_with_root <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  if (grepl("^(/|[A-Za-z]:)", path)) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(projectRoot, path), winslash = "/", mustWork = FALSE)
  }
}

load_disturbance_dt <- function(input_root, cfg = NULL) {
  candidates <- c(
    file.path(input_root, "DisturbanceDT.csv"),
    file.path(input_root, "disturbanceDT.csv"),
    file.path(projectRoot, "modules", "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
  )
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) return(NULL)
  dt <- tryCatch(data.table::fread(candidates[[1]]), error = function(...) NULL)
  if (is.null(dt)) return(NULL)
  dt
}

syntheticRoot <- Sys.getenv(
  "VALIDATION_SYNTHETIC_ROOT",
  unset = file.path(projectRoot, "data", "synthetic")
)
studyAreaCandidates <- c(
  file.path(projectRoot, "data", "synthetic", "rates", "studyArea", "syntheticAOI.shp"),
  file.path(projectRoot, "data", "synthetic", "rates", "studyArea", "syntheticAOI.gpkg"),
  file.path(projectRoot, "data", "study_area", "aoi_southwest_NWT.shp"),
  file.path(projectRoot, "data", "study_area", "aoi_southwest_NWT.gpkg"),
  file.path(projectRoot, "data", "medium_aoi.shp"),
  file.path(projectRoot, "data", "medium_aoi.gpkg"),
  file.path(suiteRoot, "medium_aoi.shp"),
  file.path(suiteRoot, "medium_aoi.gpkg")
)
studyAreaPath <- NULL
for (cand in studyAreaCandidates) {
  if (file.exists(cand)) {
    studyAreaPath <- cand
    break
  }
}
if (is.null(studyAreaPath)) stop("Study area not found in expected locations.", call. = FALSE)

disturbanceDT <- load_disturbance_dt(cfg$paths$input_root, cfg)
if (is.null(disturbanceDT) || !nrow(disturbanceDT)) {
  stop("disturbanceDT missing or empty under ", cfg$paths$input_root, call. = FALSE)
}

resolve_shapefile <- function(root, file_name) {
  if (!length(file_name)) return(character())
  file_name <- as.character(file_name)
  res <- vapply(seq_along(file_name), function(idx) {
    name <- file_name[idx]
    if (is.na(name) || !nzchar(name)) return(NA_character_)
    candidate <- file.path(root, name)
    if (file.exists(candidate)) return(candidate)
    matches <- list.files(root, pattern = paste0("^", tools::file_path_sans_ext(name), "\\.shp$"),
                          recursive = TRUE, full.names = TRUE)
    if (length(matches)) return(matches[[1]])
    NA_character_
  }, character(1))
  res
}

build_baseline_stats <- function(dt, input_root) {
  dt <- copy(dt)
  dt <- dt[grepl("\\.shp$", fileName, ignore.case = TRUE)]
  dt[, path := resolve_shapefile(input_root, fileName)]
  dt <- dt[!is.na(path)]
  if (!nrow(dt)) return(data.table())
  dt <- unique(dt[, .(dataName, dataClass, path)])
  stats <- dt[, {
    vec <- tryCatch(vect(path), error = function(e) NULL)
    if (is.null(vec) || !nrow(vec)) return(NULL)
    vec <- tryCatch(terra::project(vec, "EPSG:3347"), error = function(...) vec)
    geom <- tolower(terra::geomtype(vec)[1])
    area <- if (geom == "polygons") sum(expanse(vec, unit = "km"), na.rm = TRUE) else NA_real_
    length <- if (geom == "lines") {
      sf_obj <- sf::st_as_sf(vec)
      sum(as.numeric(sf::st_length(sf_obj)), na.rm = TRUE) / 1000
    } else NA_real_
    point_count <- if (geom == "points") terra::nrow(vec) else NA_real_
    data.table(
      geometry = geom,
      area_km2 = area,
      length_km = length,
      point_count = point_count
    )
  }, by = .(dataName, dataClass)]
  stats
}

baselineStats <- build_baseline_stats(disturbanceDT, cfg$paths$input_root)
if (!nrow(baselineStats)) stop("Unable to compute baseline stats from shapefiles under ", cfg$paths$input_root, call. = FALSE)

message("Scenario: ", scenarioId)
message("  Config: ", configPath)
message("  Using rate file: ", rateFile)
message("  Years simulated: ", scenarioStart, "-", scenarioEnd,
        " (", scenarioDurationYears, " years)")
statsWide <- copy(baselineStats)
statsWide[, `:=`(
  area_km2 = as.numeric(area_km2),
  length_km = as.numeric(length_km),
  point_count = as.numeric(point_count)
)]

rateTable <- fread(rateFile)

studyArea <- vect(studyAreaPath)
studyAreaProjected <- tryCatch(
  terra::project(studyArea, "EPSG:3347"),
  error = function(e) studyArea
)
studyAreaAreaKm2 <- sum(expanse(studyAreaProjected, unit = "km"), na.rm = TRUE)
message(sprintf("  Study area: %.1f km^2", studyAreaAreaKm2))

statsWide <- statsWide[, .(
  layerName = dataClass,
  geometry,
  area_km2,
  length_km,
  point_count
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
rateTable[, disturbanceEnd_chr := as.character(disturbanceEnd)]
rateTable[, disturbanceOrigin_chr := as.character(disturbanceOrigin)]
rateTable[, realizedLayer := fifelse(
  disturbanceType == "Connecting" &
    !is.na(disturbanceEnd_chr) & nzchar(disturbanceEnd_chr),
  disturbanceEnd_chr,
  disturbanceOrigin_chr
)]
rateTable[, c("disturbanceEnd_chr", "disturbanceOrigin_chr") := NULL]
rateTable <- merge(rateTable, potentialStats, by = "dataClass", all.x = TRUE)
rateTable <- merge(rateTable, baselineStatsForRealized, by = "realizedLayer", all.x = TRUE)
rateTable[, target_area_km2 := fifelse(
  !is.na(disturbanceRate),
  (disturbanceRate / 100) * studyAreaAreaKm2 * scenarioDurationYears,
  NA_real_
)]

outputRoot <- resolve_with_root(cfg$paths$output_root %||% "outputs")
if (!dir.exists(outputRoot)) {
  stop("Output root not found: ", outputRoot, call. = FALSE)
}
runRoot <- file.path(outputRoot, suiteName, runName)
if (!dir.exists(runRoot)) {
  stop("Run directory not found: ", runRoot, " (did you run the scenario?)", call. = FALSE)
}
repDirs <- dir(runRoot, pattern = "^rep_", full.names = TRUE)
if (!length(repDirs)) {
  stop("No replicate directories found under ", runRoot, call. = FALSE)
}
repDirs <- sort(repDirs, decreasing = TRUE)
runDir <- repDirs[1]
message("  Run directory: ", normalizePath(runDir, winslash = "/", mustWork = TRUE))

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
  area_geom <- vec
  if (geomType == "polygons" && polygonBufferMeters > 0) {
    area_geom <- tryCatch(terra::buffer(vec, width = polygonBufferMeters), error = function(e) area_geom)
  } else if (geomType == "lines" &&
             identical(tolower(meta$layer), "seismiclines")) {
    area_geom <- tryCatch(terra::buffer(vec, width = lineBufferMeters), error = function(e) area_geom)
  }
  area_type <- tolower(geomtype(area_geom))
  area_km2 <- if (!inherits(area_geom, "SpatVector") || terra::nrow(area_geom) == 0 ||
                   area_type != "polygons") {
    NA_real_
  } else {
    sum(expanse(area_geom, unit = "km"), na.rm = TRUE)
  }
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
  resolutionVector,
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

default_width <- lineBufferMeters
res_vec <- if ("resolutionVector" %in% names(joined)) {
  suppressWarnings(as.numeric(joined[["resolutionVector"]]))
} else {
  rep(NA_real_, nrow(joined))
}
res_vec[!is.finite(res_vec)] <- default_width
joined[, width_km := res_vec / 1000]

joined[, realized_equiv_area_km2 := realized_new_area_km2]
joined[realized_geometry == "lines" & !is.na(realized_new_length_km),
       realized_equiv_area_km2 := realized_new_length_km * width_km]
joined[realized_geometry == "points" & !is.na(realized_new_count) & !is.na(width_km),
       realized_equiv_area_km2 := realized_new_count * (width_km^2)]

joined[, target_equiv_length_km := fifelse(
  !is.na(target_area_km2) & !is.na(width_km) & width_km > 0,
  target_area_km2 / width_km,
  NA_real_
)]

joined[, potential_capacity_km2 := fifelse(
  realized_geometry == "lines" & !is.na(potential_length_km) & !is.na(width_km),
  potential_length_km * width_km,
  potential_area_km2
)]

joined[, capacity_gap_km2 := fifelse(
  !is.na(target_area_km2) & !is.na(potential_capacity_km2),
  pmax(target_area_km2 - potential_capacity_km2, 0),
  NA_real_
)]
joined[, capacity_flag := fifelse(
  !is.na(capacity_gap_km2) & capacity_gap_km2 > 0,
  "insufficient_potential",
  NA_character_
)]

joined[, actual_rate_pct := fifelse(
  !is.na(realized_equiv_area_km2) & studyAreaAreaKm2 > 0 & scenarioDurationYears > 0,
  (realized_equiv_area_km2 / (studyAreaAreaKm2 * scenarioDurationYears)) * 100,
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
            scenario_end, "): ",
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
  realized_area <- coalesce(row$realized_equiv_area_km2, row$realized_new_area_km2)
  realized_pct <- row$actual_rate_pct
  if (!is.na(realized_area)) {
    targetTxt <- if (!is.na(row$target_area_km2)) {
      sprintf("target %.3f km^2", row$target_area_km2)
    } else "target n/a"
    actualTxt <- if (!is.na(realized_pct)) {
      sprintf("realized %.3f km^2 (%.3f%%)", realized_area, realized_pct)
    } else {
      sprintf("realized %.3f km^2", realized_area)
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
resultsDir <- file.path(projectRoot, "workspace", "rates", "results")
dir.create(resultsDir, recursive = TRUE, showWarnings = FALSE)
config_base <- tools::file_path_sans_ext(basename(configPath))
results_filename <- sprintf("verification_%s.csv", config_base)
resultsPath <- file.path(resultsDir, results_filename)
runOutputPath <- file.path(runDir, results_filename)
fwrite(joined, resultsPath)
invisible(file.copy(resultsPath, runOutputPath, overwrite = TRUE))
message("Verification written to ", resultsPath)
