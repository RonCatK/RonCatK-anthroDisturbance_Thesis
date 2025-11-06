#!/usr/bin/env Rscript

# Synthetic disturbance-rate suite driven by a scenario matrix.
# Mirrors the system-suite workflow: scenarios come from a CSV with status
# tracking, and each row executes through SpaDES while writing updated
# bookkeeping back to disk.

args_full <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_full[grep("^--file=", args_full)])
suite_root <- if (length(script_path)) normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
suite_root_env <- Sys.getenv("RATES_SUITE_ROOT", unset = "")
if (nzchar(suite_root_env)) {
  suite_root <- normalizePath(suite_root_env, winslash = "/", mustWork = TRUE)
}
rates_root <- normalizePath(file.path(suite_root, ".."), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(rates_root, "..", ".."), winslash = "/", mustWork = TRUE)

require_cache <- file.path(project_root, "cache", "Require_rates_suite")
dir.create(require_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(REQUIRE_HOME = normalizePath(require_cache, winslash = "/", mustWork = TRUE))
options(
  Require.offlineMode = TRUE,
  Require.install = FALSE,
  Require.checkInternet = FALSE
)
if (requireNamespace("Require", quietly = TRUE)) {
  try(Require::setRequireHome(Sys.getenv("REQUIRE_HOME")), silent = TRUE)
}

suppressPackageStartupMessages({
  corePkgs <- c("data.table", "terra", "SpaDES.core", "reproducible", "sf", "truncnorm", "msm")
  missing <- corePkgs[!vapply(corePkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "),
         "; install them before running this suite.", call. = FALSE)
  }
  invisible(lapply(corePkgs, require, character.only = TRUE))
  if (requireNamespace("googledrive", quietly = TRUE)) {
    googledrive::drive_deauth()
  }
})

`%||%` <- function(a, b) if (is.null(a)) b else a

timestamp_tag <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
timestamp_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

ensure_path <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

relative_to_root <- function(pathValue, root) {
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) {
    return(NA_character_)
  }
  normalized <- normalizePath(pathValue, winslash = "/", mustWork = FALSE)
  rootNorm <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(rootNorm, "/")
  if (startsWith(normalized, prefix)) {
    sub(prefix, "", normalized, fixed = TRUE)
  } else {
    normalized
  }
}

parse_bool <- function(x, default = NA) {
  if (length(x) > 1) {
    return(vapply(x, parse_bool, logical(1), default = default))
  }
  if (length(x) == 0 || is.null(x) || is.na(x)) return(default)
  if (is.logical(x)) return(ifelse(is.na(x), default, x))
  val <- trimws(tolower(as.character(x)))
  if (!nzchar(val)) return(default)
  if (val %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (val %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  warning(sprintf("Unrecognised logical flag '%s'; using default (%s).", val, default), call. = FALSE)
  default
}

parse_numeric <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) return(NA_real_)
  val <- suppressWarnings(as.numeric(x))
  if (all(is.na(val))) return(NA_real_)
  val
}

layer_catalog <- data.table::data.table(
  dataName = c(
    "forestry", "forestry",
    "mining", "mining",
    "oilGas", "oilGas", "oilGas", "oilGas", "oilGas", "oilGas",
    "settlements", "settlements", "settlements",
    "roads",
    "Energy", "Energy", "Energy", "Energy"
  ),
  dataClass = c(
    "cutblocks", "potentialCutblocks",
    "mining", "potentialMining",
    "oilGas", "seismicLines", "potentialSeismicLines", "potentialOilGas",
    "pipeline", "potentialPipeline",
    "settlements", "otherPolygons", "potentialOtherPolygons",
    "roads",
    "powerLines", "potentialPowerLines", "windTurbines", "potentialWindTurbines"
  ),
  layerName = c(
    "cutblocks", "potentialCutblocks",
    "mining", "potentialMining",
    "oilGas", "seismicLines", "potentialSeismicLines", "potentialOilGas",
    "pipeline", "potentialPipeline",
    "settlements", "otherPolygons", "potentialOtherPolygons",
    "roads",
    "powerLines", "potentialPowerLines", "windTurbines", "potentialWindTurbines"
  ),
  geometry = c(
    "polygons", "polygons",
    "polygons", "polygons",
    "polygons", "lines", "polygons", "polygons", "lines", "lines",
    "polygons", "polygons", "polygons",
    "lines",
    "lines", "lines", "points", "points"
  )
)

make_synthetic_rates <- function(totalRate) {
  baseProps <- c(
    seismicLines = 0.28,
    cutblocks = 0.20,
    oilGas = 0.20,
    mining = 0.10,
    settlements = 0.12,
    otherPolygons = 0.05,
    windTurbines = 0.05
  )
  baseProps <- baseProps / sum(baseProps)
  rates <- round(totalRate * baseProps * 100, 6)
  gen_rows <- data.table::data.table(
    dataName = c("oilGas", "forestry", "oilGas", "mining",
                 "settlements", "settlements", "Energy"),
    dataClass = c("potentialSeismicLines", "potentialCutblocks", "potentialOilGas",
                  "potentialMining", "settlements", "potentialOtherPolygons",
                  "potentialWindTurbines"),
    disturbanceType = c("Generating", "Generating", "Generating", "Generating",
                        "Enlarging", "Enlarging", "Generating"),
    disturbanceOrigin = c("seismicLines", "cutblocks", "oilGas", "mining",
                          "settlements", "otherPolygons", "windTurbines"),
    disturbanceEnd = "",
    disturbanceRate = as.numeric(rates[c("seismicLines", "cutblocks", "oilGas",
                                         "mining", "settlements", "otherPolygons",
                                         "windTurbines")]),
    disturbanceSize = c(NA_character_, NA_character_, NA_character_, NA_character_,
                        NA_character_, NA_character_, "62500"),
    disturbanceInterval = 1L,
    potentialField = NA_character_,
    resolutionVector = 30
  )
  connect_rows <- data.table::data.table(
    dataName = c("oilGas", "Energy"),
    dataClass = c("potentialPipeline", "potentialPowerLines"),
    disturbanceType = "Connecting",
    disturbanceOrigin = c("oilGas", "windTurbines"),
    disturbanceEnd = c("pipeline", "powerLines"),
    disturbanceRate = as.numeric(rates[c("oilGas", "windTurbines")]),
    disturbanceSize = NA_character_,
    disturbanceInterval = 1L,
    potentialField = NA_character_,
    resolutionVector = 15
  )
  data.table::rbindlist(list(gen_rows, connect_rows), use.names = TRUE, fill = TRUE)
}

create_disturbance_dt <- function(layerIndex) {
  layerIndex[, .(
    dataName,
    classToSearch = "",
    fieldToSearch = "",
    dataClass,
    fileName = paste0(layerName, ".shp"),
    dataType = "shapefile",
    URL = paste0("file://", filePath)
  )]
}

ensure_synthetic_gpkg <- function(rawSuiteRoot, projectRoot, ratesScratch) {
  sourceGpkg <- file.path(rawSuiteRoot, "medium_aoi_optimal.gpkg")
  sourceStats <- file.path(rawSuiteRoot, "medium_aoi_optimal_stats.csv")
  if (file.exists(sourceGpkg) && file.exists(sourceStats)) {
    return(list(gpkg = sourceGpkg, stats = sourceStats))
  }
  message("Synthetic GeoPackage missing; rebuilding via scripts/synthetic_data_aoi.R ...")
  scriptPath <- file.path(projectRoot, "scripts", "synthetic_data_aoi.R")
  if (!file.exists(scriptPath)) {
    stop("Synthetic builder script not found at ", scriptPath, call. = FALSE)
  }
  status <- system2("Rscript", scriptPath)
  if (!identical(status, 0L)) {
    stop("Rebuild of synthetic GeoPackage failed (exit code ", status, ").", call. = FALSE)
  }
  scratchDir <- file.path(ratesScratch, "medium_aoi_optimal")
  scratchGpkg <- file.path(scratchDir, "medium_aoi_optimal.gpkg")
  scratchStats <- file.path(scratchDir, "medium_aoi_optimal_stats.csv")
  if (!file.exists(scratchGpkg) || !file.exists(scratchStats)) {
    stop("Synthetic builder did not produce expected outputs under ", scratchDir, call. = FALSE)
  }
  file.copy(scratchGpkg, sourceGpkg, overwrite = TRUE)
  file.copy(scratchStats, sourceStats, overwrite = TRUE)
  list(gpkg = sourceGpkg, stats = sourceStats)
}

write_synthetic_layers <- function(catalog, gpkg, inputDir, libraryDir) {
  catalog[, {
    vec <- terra::vect(gpkg, layer = layerName)
    if (!nrow(vec)) {
      warning("Layer '", layerName, "' is empty in GPKG; skipping.", call. = FALSE)
      next
    }
    if (!"Class" %in% names(vec)) vec$Class <- dataClass
    if (grepl("^potential", dataClass, ignore.case = TRUE) && !"Potential" %in% names(vec)) {
      vec$Potential <- seq_len(nrow(vec))
    }
    layerPath <- file.path(inputDir, paste0(layerName, ".shp"))
    if (file.exists(layerPath)) {
      existing <- list.files(inputDir, pattern = paste0("^", layerName, "\\.(shp|shx|dbf|prj|cpg)$"), full.names = TRUE)
      if (length(existing)) file.remove(existing)
    }
    sf_obj <- sf::st_as_sf(vec)
    sf::st_write(sf_obj, layerPath, delete_layer = TRUE, quiet = TRUE)
    sidecars <- list.files(inputDir, pattern = paste0("^", layerName, "\\.(shp|shx|dbf|prj|cpg)$"), full.names = TRUE)
    if (!length(sidecars)) {
      stop("Failed to export shapefile sidecars for ", layerName, call. = FALSE)
    }
    zipPath <- file.path(libraryDir, paste0(layerName, ".zip"))
    if (file.exists(zipPath)) file.remove(zipPath)
    utils::zip(zipfile = zipPath, files = sidecars, flags = "-j")
    .(filePath = normalizePath(zipPath, winslash = "/", mustWork = TRUE))
  }, by = .(dataName, dataClass, layerName, geometry)]
}

create_local_rtm <- function(studyArea, resolution = 250) {
  saProj <- terra::project(studyArea, terra::crs(studyArea))
  rtm <- terra::rast(extent = terra::ext(saProj), resolution = resolution, crs = terra::crs(saProj))
  terra::values(rtm) <- runif(terra::ncell(rtm))
  # smooth to remove single-cell spikes and create gradients
  rtm <- terra::focal(rtm, w = matrix(1, nrow = 3, ncol = 3), fun = mean, na.rm = TRUE, expand = TRUE)
  rtm <- terra::mask(rtm, saProj)
  rtm <- rtm - terra::global(rtm, fun = "min", na.rm = TRUE)[1, 1]
  max_val <- terra::global(rtm, fun = "max", na.rm = TRUE)[1, 1]
  if (!is.na(max_val) && max_val > 0) {
    rtm <- rtm / max_val
  }
  rtm
}

summarize_outputs <- function(outputDir, scenarioId, totalRate) {
  shpFiles <- list.files(outputDir, pattern = "\\.shp$", full.names = TRUE)
  if (!length(shpFiles)) return(data.table::data.table())
  summaries <- lapply(shpFiles, function(path) {
    fname <- sub("\\.shp$", "", basename(path))
    parts <- strsplit(fname, "_", fixed = TRUE)[[1]]
    if (length(parts) < 4) return(NULL)
    sector <- parts[2]
    layer <- paste(parts[3:(length(parts) - 1)], collapse = "_")
    year <- suppressWarnings(as.integer(parts[length(parts)]))
    vec <- tryCatch(terra::vect(path), error = function(e) NULL)
    if (is.null(vec)) return(NULL)
    geomType <- tryCatch(terra::geomtype(vec)[1], error = function(e) NA_character_)
    projVec <- tryCatch(terra::project(vec, "EPSG:3347"), error = function(e) vec)
    area <- if (identical(geomType, "polygons")) sum(terra::expanse(projVec, unit = "km"), na.rm = TRUE) else NA_real_
    len <- if (identical(geomType, "lines")) {
      geomInfo <- terra::geom(projVec, TRUE)
      if ("length" %in% colnames(geomInfo)) sum(geomInfo[, "length"], na.rm = TRUE) / 1000 else NA_real_
    } else NA_real_
    cnt <- if (identical(geomType, "points")) terra::nrow(projVec) else NA_integer_
    data.table::data.table(
      scenario_id = scenarioId,
      total_rate = totalRate,
      file = basename(path),
      sector = sector,
      layer = layer,
      year = year,
      geometry = geomType,
      area_km2 = area,
      length_km = len,
      feature_count = cnt
    )
  })
  data.table::rbindlist(Filter(Negate(is.null), summaries), use.names = TRUE, fill = TRUE)
}

resolve_module_path <- function(projectRoot, override) {
  candidates <- unique(trimws(c(
    override,
    file.path(projectRoot, "modules"),
    file.path(projectRoot, "modules_Testing")
  )))
  candidates <- candidates[nzchar(candidates)]
  for (cand in candidates) {
    expanded <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (dir.exists(file.path(expanded, "anthroDisturbance_Generator"))) {
      return(normalizePath(expanded, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Unable to resolve module path. Checked: ", paste(candidates, collapse = ", "), call. = FALSE)
}

resolve_rate_table <- function(cfg, ctx) {
  ratePath <- cfg$disturbance_rate_file
  if (!is.null(ratePath) && nzchar(ratePath)) {
    candidates <- unique(c(
      ratePath,
      path.expand(ratePath),
      file.path(ctx$suiteRoot, ratePath),
      file.path(ctx$projectRoot, ratePath)
    ))
    existing <- candidates[file.exists(candidates)]
    if (!length(existing)) {
      stop("Disturbance rate file not found for scenario ", cfg$scenario_id, ": ", ratePath, call. = FALSE)
    }
    rateFile <- normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
    tbl <- data.table::fread(rateFile)
    if ("disturbanceSize" %in% names(tbl)) {
      tbl[, disturbanceSize := gsub("^rtnorm", "msm::rtnorm", disturbanceSize)]
    }
    tbl
  } else {
    make_synthetic_rates(cfg$total_rate)
  }
}

build_rates_context <- function() {
  suiteRoot <- suite_root
  ratesRoot <- rates_root
  projectRoot <- project_root

  requireCache <- ensure_path(file.path(projectRoot, "cache", "Require_rates"))
  Sys.setenv(REQUIRE_HOME = normalizePath(requireCache, winslash = "/", mustWork = TRUE))
  options(
    Require.offlineMode = TRUE,
    Require.install = FALSE,
    Require.checkInternet = FALSE
  )

  syntheticRoot <- Sys.getenv("VALIDATION_SYNTHETIC_ROOT", unset = file.path(projectRoot, "data", "synthetic"))
  rawSuiteRoot <- ensure_path(file.path(syntheticRoot, "rates"))
  suiteInputs <- ensure_path(file.path(rawSuiteRoot, "synthetic_inputs"))
  suiteLibrary <- ensure_path(file.path(rawSuiteRoot, "synthetic_library"))
  scenarioOutputsRoot <- ensure_path(file.path(projectRoot, "outputs", "rates"))
  packagedOutputsRoot <- ensure_path(file.path(projectRoot, "outputs", "rates", "packaged"))
  ratesScratch <- ensure_path(file.path(projectRoot, "scratch", "rates"))
  logsDir <- ensure_path(file.path(ratesScratch, "logs"))
  cachePath <- ensure_path(file.path(projectRoot, "cache", "validation", "rates"))

  syntheticPaths <- ensure_synthetic_gpkg(rawSuiteRoot, projectRoot, ratesScratch)
  layerIndex <- write_synthetic_layers(layer_catalog, syntheticPaths$gpkg, suiteInputs, suiteLibrary)
  disturbanceDT <- create_disturbance_dt(layerIndex)
  statsDT <- data.table::fread(syntheticPaths$stats)

  studyAreaCandidates <- c(
    file.path(syntheticRoot, "study_area", "aoi_southwest_NWT.shp"),
    file.path(syntheticRoot, "study_area", "aoi_southwest_NWT.gpkg"),
    file.path(projectRoot, "data", "study_area", "aoi_southwest_NWT.shp"),
    file.path(projectRoot, "data", "study_area", "aoi_southwest_NWT.gpkg"),
    file.path(syntheticRoot, "study_area", "medium_aoi.shp"),
    file.path(projectRoot, "data", "medium_aoi.shp"),
    file.path(projectRoot, "data", "medium_aoi.gpkg")
  )
  studyAreaPath <- NULL
  for (candidate in studyAreaCandidates) {
    if (file.exists(candidate)) {
      studyAreaPath <- candidate
      break
    }
  }
  if (is.null(studyAreaPath)) {
    stop("Study area not found in synthetic or data directories.", call. = FALSE)
  }
  studyArea <- terra::vect(studyAreaPath)
  rasterToMatch <- reproducible::Cache(create_local_rtm, studyArea = studyArea,
                                       cachePath = cachePath, cacheId = "rates_rtm")

  list(
    projectRoot = projectRoot,
    suiteRoot = suiteRoot,
    ratesRoot = ratesRoot,
    syntheticRoot = syntheticRoot,
    rawSuiteRoot = rawSuiteRoot,
    suiteInputs = suiteInputs,
    suiteLibrary = suiteLibrary,
    scenarioOutputsRoot = scenarioOutputsRoot,
    packagedOutputsRoot = packagedOutputsRoot,
    scratchRoot = ratesScratch,
    logsDir = logsDir,
    cachePath = cachePath,
    disturbanceDT = disturbanceDT,
    statsDT = statsDT,
    studyArea = studyArea,
    rasterToMatch = rasterToMatch,
    packageScript = file.path(projectRoot, "validation", "rates", "package_outputs.R"),
    verifyScript = file.path(projectRoot, "validation", "rates", "scenarios", "verify_rates.R"),
    runDataPath = file.path(ratesScratch, "run_data.csv")
  )
}

parse_cli_args <- function(args) {
  opts <- list(
    csv = NULL,
    scenario_ids = character(0),
    force = FALSE,
    dry_run = FALSE,
    mode = "default",
    show_help = FALSE
  )
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$show_help <- TRUE
    } else if (identical(arg, "--force")) {
      opts$force <- TRUE
    } else if (identical(arg, "--dry-run")) {
      opts$dry_run <- TRUE
    } else if (grepl("^--csv=", arg, ignore.case = TRUE)) {
      opts$csv <- sub("^--csv=", "", arg, ignore.case = TRUE)
    } else if (grepl("^--scenario=", arg, ignore.case = TRUE)) {
      vals <- sub("^--scenario=", "", arg, ignore.case = TRUE)
      vals <- unlist(strsplit(vals, "[,;]"))
      vals <- trimws(vals[nzchar(vals)])
      opts$scenario_ids <- unique(c(opts$scenario_ids, vals))
    } else if (grepl("^--mode=", arg, ignore.case = TRUE)) {
      opts$mode <- tolower(sub("^--mode=", "", arg, ignore.case = TRUE))
    } else {
      warning(sprintf("Ignoring unrecognised argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

ensure_scenario_col_order <- function(dt) {
  desired <- c(
    "scenario_id", "description", "status", "active", "last_run_date", "notes",
    "total_rate", "mode", "run_interval", "start_year", "end_year", "use_fire", "fire_module",
    "disturbance_rate_file", "save_diagnostics", "use_cluster_method", "package_outputs", "module_path",
    "seed", "log_path", "output_path", "verification_csv", "last_updated"
  )
  present <- desired[desired %in% names(dt)]
  remaining <- setdiff(names(dt), desired)
  data.table::setcolorder(dt, c(present, remaining))
}

strip_helper_cols <- function(dt) {
  drop <- c("row_id", "active_flag", "status_chr")
  dt[, setdiff(names(dt), drop), with = FALSE]
}

scenario_cfg_from_row <- function(row, ctx) {
  scenarioId <- as.character(row$scenario_id %||% row$description %||% paste0("scenario_", row$row_id %||% ""))
  mode <- tolower(trimws(as.character(row$mode %||% "vector")))
  if (!mode %in% c("vector", "raster")) {
    stop("Scenario ", scenarioId, " has unsupported mode '", mode, "'.", call. = FALSE)
  }
  fireModules <- character(0)
  rawFireMod <- row$fire_module %||% NA_character_
  if (!is.na(rawFireMod)) {
    toks <- trimws(unlist(strsplit(as.character(rawFireMod), "[,;]")))
    toks <- toks[nzchar(toks)]
    if (length(toks)) fireModules <- unique(toks)
  }
  list(
    scenario_id = scenarioId,
    description = as.character(row$description %||% ""),
    total_rate = parse_numeric(row$total_rate %||% 0),
    mode = mode,
    run_interval = as.integer(parse_numeric(row$run_interval %||% 1L)),
    start_year = as.integer(parse_numeric(row$start_year %||% 2011)),
    end_year = as.integer(parse_numeric(row$end_year %||% 2021)),
    use_fire = parse_bool(row$use_fire, default = FALSE),
    disturbance_rate_file = as.character(row$disturbance_rate_file %||% ""),
    save_diagnostics = parse_bool(row$save_diagnostics, default = FALSE),
    use_cluster_method = parse_bool(row$use_cluster_method, default = FALSE),
    package_outputs = parse_bool(row$package_outputs, default = FALSE),
    module_path = resolve_module_path(ctx$projectRoot, row$module_path %||% NA_character_),
    fire_modules = fireModules,
    seed = {
      seedVal <- parse_numeric(row$seed)
      if (is.na(seedVal)) NULL else as.integer(seedVal)
    }
  )
}

run_rate_scenario <- function(cfg, ctx) {
  runTag <- timestamp_tag()
  runName <- cfg$run_name %||% paste("rates", cfg$scenario_id, cfg$mode, runTag, sep = "_")
  outputPath <- ensure_path(file.path(ctx$scenarioOutputsRoot, runName))
  logFile <- file.path(ctx$logsDir, paste0(runName, ".log"))
  if (file.exists(logFile)) file.remove(logFile)
  con <- file(logFile, open = "wt")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  sink(con, type = "output")
  sink(con, type = "message")
  on.exit({
    try(sink(type = "message"), silent = TRUE)
    try(sink(type = "output"), silent = TRUE)
  }, add = TRUE)

  status <- "SUCCESS"
  errMsg <- NA_character_
  summaryDT <- data.table::data.table()

  rateTable <- tryCatch(resolve_rate_table(cfg, ctx), error = function(e) {
    status <<- "FAIL"
    errMsg <<- conditionMessage(e)
    NULL
  })
  if (is.null(rateTable)) {
    return(list(status = status, log = relative_to_root(logFile, ctx$projectRoot),
                outputPath = relative_to_root(outputPath, ctx$projectRoot),
                message = errMsg, summary = summaryDT, runName = runName))
  }

  if (!is.null(cfg$seed)) {
   set.seed(cfg$seed)
  }

  baseModules <- c("anthroDisturbance_DataPrep", "anthroDisturbance_Generator")
  if (isTRUE(cfg$use_fire) && length(cfg$fire_modules)) {
    generator_idx <- match("anthroDisturbance_Generator", baseModules)
    insert_after <- if (is.na(generator_idx)) length(baseModules) else max(0, generator_idx - 1)
    modules <- append(baseModules, cfg$fire_modules, after = insert_after)
    modules <- modules[!duplicated(modules)]
  } else {
    modules <- baseModules
  }
  if (isTRUE(cfg$use_fire) && !length(cfg$fire_modules)) {
    stop("Scenario '", cfg$scenario_id, "' sets use_fire = TRUE but no fire_module was provided.", call. = FALSE)
  }
  generatedAsRaster <- identical(cfg$mode, "raster")

  projectPaths <- list(
    modulePath = cfg$module_path,
    cachePath = ctx$cachePath,
    scratchPath = ctx$scratchRoot,
    inputPath = ctx$suiteInputs,
    outputPath = outputPath
  )

  oldOptions <- options(
    spades.allowInitDuringSimInit = TRUE,
    spades.DTthreads = 1,
    spades.project.fast = FALSE,
    spades.scratchPath = projectPaths$scratchPath,
    run_scenario.debug = TRUE,
    reproducible.useMemoise = FALSE,
    reproducible.destinationPath = outputPath,
    reproducible.inputPaths = ctx$suiteInputs,
    reproducible.cacheSaveFormat = "rds",
    reproducible.gdalwarp = TRUE,
    reproducible.useInternet = FALSE,
    reproducible.cachePath = projectPaths$cachePath,
    spades.inputPath = ctx$suiteInputs,
    spades.outputPath = outputPath,
    spades.modulePath = projectPaths$modulePath
  )
  on.exit(options(oldOptions), add = TRUE)

  if (inherits(ctx$rasterToMatch, "SpatRaster")) {
    try({
      rt <- ctx$rasterToMatch
      totalCells <- terra::ncell(rt)
      naCount <- terra::global(is.na(rt), fun = "sum", na.rm = TRUE)[1, 1]
      validCells <- if (is.na(naCount)) NA_real_ else totalCells - naCount
      minmax <- terra::minmax(rt)
      pctValid <- if (!is.na(validCells)) (validCells / totalCells) * 100 else NA_real_
      message(sprintf(
        "RasterToMatch summary: cells=%s, valid=%s (%.2f%%), value range [%s, %s]",
        format(totalCells, big.mark = ","),
        if (is.na(validCells)) "NA" else format(validCells, big.mark = ","),
        if (is.na(pctValid)) NaN else pctValid,
        format(minmax[1, 1]),
        format(minmax[2, 1])
      ))
    }, silent = TRUE)
  }

  params <- list(
    anthroDisturbance_DataPrep = list(
      useSavedList = FALSE,
      checkDisturbanceProportions = FALSE,
      studyAreaName = "synthetic_rates"
    ),
    anthroDisturbance_Generator = list(
      .runName = runName,
      generatedDisturbanceAsRaster = generatedAsRaster,
      disturbFirstYear = FALSE,
      growthStepGenerating = 0.005,
      growthStepEnlargingPolys = 0.005,
      growthStepEnlargingLines = 0.05,
      disturbanceRateRelatesToBufferedArea = FALSE,
      verboseDiagnostics = isTRUE(cfg$save_diagnostics),
      useClusterMethod = isTRUE(cfg$use_cluster_method),
      siteSelectionAsDistributing = NA,
      maskWaterAndMountainsFromLines = FALSE,
      totalDisturbanceRate = NULL,
      .inputFolderFireLayer = outputPath
    )
  )

  objects <- list(
    disturbanceDT = ctx$disturbanceDT,
    DisturbanceRate = rateTable,
    studyArea = ctx$studyArea,
    rasterToMatch = ctx$rasterToMatch
  )

  sim <- tryCatch(
    SpaDES.core::simInit(
      times = list(start = cfg$start_year, end = cfg$end_year),
      params = params,
      modules = modules,
      objects = objects,
      paths = projectPaths,
      packages = c(
        "RCurl", "XML", "igraph", "qs",
        "SpaDES.tools", "SpaDES.core", "reproducible",
        "Require (>= 1.0.1)"
      )
    ),
    error = function(e) {
      status <<- "FAIL"
      errMsg <<- paste("simInit failed:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(sim)) {
    return(list(status = status, log = relative_to_root(logFile, ctx$projectRoot),
                outputPath = relative_to_root(outputPath, ctx$projectRoot),
                message = errMsg, summary = summaryDT, runName = runName))
  }

  if (!is.null(rateTable) && nrow(rateTable)) {
    try({
      if (data.table::is.data.table(sim$disturbanceParameters)) {
        dp <- sim$disturbanceParameters
        joinCols <- intersect(
          c("dataName", "dataClass", "disturbanceType", "disturbanceOrigin", "disturbanceEnd"),
          names(dp)
        )
        if (length(joinCols)) {
          rt <- data.table::copy(rateTable)
          if ("disturbanceRate" %in% names(rt)) {
            rt[, disturbanceRate := suppressWarnings(as.numeric(disturbanceRate))]
          }
          if ("disturbanceInterval" %in% names(rt)) {
            rt[, disturbanceInterval := suppressWarnings(as.integer(disturbanceInterval))]
          }
          if ("resolutionVector" %in% names(rt)) {
            rt[, resolutionVector := suppressWarnings(as.integer(resolutionVector))]
          }
          dp[rt, disturbanceRate := fifelse(!is.na(i.disturbanceRate), i.disturbanceRate, disturbanceRate),
             on = joinCols]
          if ("disturbanceInterval" %in% names(rt) && "disturbanceInterval" %in% names(dp)) {
            dp[rt, disturbanceInterval := fifelse(!is.na(i.disturbanceInterval), i.disturbanceInterval, disturbanceInterval),
               on = joinCols]
          }
          if ("disturbanceSize" %in% names(rt) && "disturbanceSize" %in% names(dp)) {
            dp[rt, disturbanceSize := ifelse(!is.na(i.disturbanceSize) & nzchar(i.disturbanceSize),
                                             i.disturbanceSize, disturbanceSize),
               on = joinCols]
          }
          if ("resolutionVector" %in% names(rt) && "resolutionVector" %in% names(dp)) {
            dp[rt, resolutionVector := fifelse(!is.na(i.resolutionVector), i.resolutionVector, resolutionVector),
               on = joinCols]
          }
          sim$disturbanceParameters <- dp
        }
      }
      sim$DisturbanceRate <- rateTable
    }, silent = TRUE)
  }

  simResult <- tryCatch(
    SpaDES.core::spades(sim),
    error = function(e) {
      status <<- "FAIL"
      errMsg <<- paste("spades failed:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(simResult)) {
    return(list(status = status, log = relative_to_root(logFile, ctx$projectRoot),
                outputPath = relative_to_root(outputPath, ctx$projectRoot),
                message = errMsg, summary = summaryDT, runName = runName))
  }

  dpPath <- file.path(outputPath, "disturbanceParameters.csv")
  if ("disturbanceParameters" %in% names(simResult)) {
    tryCatch(data.table::fwrite(simResult$disturbanceParameters, dpPath), error = function(e) {
      message("Failed to write disturbanceParameters: ", e$message)
    })
  }

  summaryDT <- summarize_outputs(outputPath, cfg$scenario_id, cfg$total_rate)

  verificationPath <- NA_character_
  if (file.exists(ctx$verifyScript)) {
    message("Running verification for ", runName)
    verifyCmd <- c(ctx$verifyScript, cfg$scenario_id)
    verifyOutput <- tryCatch(
      system2("Rscript", verifyCmd, stdout = TRUE, stderr = TRUE),
      error = function(e) {
        message("Verification failed to launch: ", conditionMessage(e))
        structure(character(), status = 1L)
      }
    )
    if (length(verifyOutput)) {
      writeLines(verifyOutput)
      writtenLine <- verifyOutput[grepl("^Verification written to\\s+", verifyOutput)]
      if (length(writtenLine)) {
        verificationPath <- trimws(sub("^Verification written to\\s+", "", writtenLine[1]))
      }
    }
    verifyStatus <- attr(verifyOutput, "status")
    if (!is.null(verifyStatus) && verifyStatus != 0) {
      message("Verification exited with status ", verifyStatus)
    }
  } else {
    message("Verification script missing at ", ctx$verifyScript)
  }

  if (isTRUE(cfg$package_outputs) && file.exists(ctx$packageScript)) {
    message("Packaging outputs for ", runName)
    tryCatch(system2("Rscript", c(ctx$packageScript, paste0("--run=", runName))),
             warning = function(w) message("Packaging warning: ", conditionMessage(w)),
             error = function(e) message("Packaging failed: ", conditionMessage(e)))
    pkgDest <- ensure_path(file.path(ctx$packagedOutputsRoot, runName))
    pkgFiles <- list.files(outputPath, pattern = "\\.(zip|gpkg|csv)$", full.names = TRUE)
    if (length(pkgFiles)) file.copy(pkgFiles, pkgDest, overwrite = TRUE)
  }

  list(
    status = status,
    log = relative_to_root(logFile, ctx$projectRoot),
    outputPath = relative_to_root(outputPath, ctx$projectRoot),
    message = if (is.na(errMsg)) "" else errMsg,
    summary = summaryDT,
    verification = list(
      csv = relative_to_root(verificationPath, ctx$projectRoot),
      absolute_csv = if (is.na(verificationPath) || !nzchar(verificationPath)) NA_character_ else normalizePath(verificationPath, winslash = "/", mustWork = FALSE)
    ),
    runName = runName
  )
}

execute_scenarios_from_csv <- function(ctx, csv_path, scenario_ids = NULL,
                                        force = FALSE, dry_run = FALSE,
                                        mode = c("default", "respect", "all")) {
  mode <- match.arg(tolower(mode), c("default", "respect", "all"))
  if (!file.exists(csv_path)) {
    stop("Scenario CSV not found: ", csv_path, call. = FALSE)
  }
  dt <- data.table::fread(csv_path, fill = TRUE)
  char_cols <- c("scenario_id", "description", "status", "active", "notes",
                 "mode", "disturbance_rate_file", "log_path", "output_path",
                 "verification_csv", "last_run_date", "last_updated")
  for (col in char_cols) {
    if (!col %in% names(dt)) {
      dt[, (col) := NA_character_]
    } else if (!is.character(dt[[col]])) {
      dt[, (col) := as.character(dt[[col]])]
    }
  }
  ensure_scenario_col_order(dt)
  if (!"scenario_id" %in% names(dt)) {
    stop("Scenario CSV missing required 'scenario_id' column.", call. = FALSE)
  }
  if (!"active" %in% names(dt)) dt[, active := TRUE]
  if (!"status" %in% names(dt)) dt[, status := NA_character_]
  dt[, status_chr := tolower(trimws(as.character(status)))]

  if (mode == "default") {
    dt[, active := (is.na(status_chr) | status_chr == "" | status_chr %in% c("fail", "pending"))]
  } else if (mode == "all") {
    dt[, active := TRUE]
  }

  dt[, row_id := .I]
  dt[, active_flag := {
    if (is.logical(active)) active else parse_bool(active, default = TRUE)
  }]

  runnable <- dt[active_flag == TRUE]
  if (!is.null(scenario_ids) && length(scenario_ids)) {
    runnable <- runnable[scenario_id %in% scenario_ids]
  }
  if (!force) {
    runnable <- runnable[!(status_chr %in% c("success", "skip"))]
  }

  if (!nrow(runnable)) {
    message("No scenarios selected for execution.")
    if (!dry_run) {
      ensure_scenario_col_order(dt)
      out <- strip_helper_cols(dt)
      data.table::fwrite(out, csv_path)
    }
    return(list(results = list(), table = strip_helper_cols(dt), summaries = list()))
  }

  selected_ids <- runnable$scenario_id
  message("Selected scenarios: ", paste(selected_ids, collapse = ", "))
  if (dry_run) {
    message("Dry-run mode: no scenarios executed.")
    return(list(results = lapply(selected_ids, function(id) list(scenario_id = id, status = "DRY_RUN")),
                table = strip_helper_cols(dt), summaries = list()))
  }

  results <- list()
  summaries <- list()
  for (i in seq_len(nrow(runnable))) {
    row_info <- runnable[i]
    idx <- row_info$row_id
    rowList <- as.list(dt[idx])
    cfg <- scenario_cfg_from_row(rowList, ctx)
    message(sprintf("Running scenario '%s' ...", rowList$scenario_id))
    res <- run_rate_scenario(cfg, ctx)
    results[[length(results) + 1]] <- c(list(scenario_id = rowList$scenario_id), res)

    dt[idx, `:=`(
      status = res$status,
      log_path = res$log,
      output_path = res$outputPath,
      verification_csv = if (!is.null(res$verification)) res$verification$csv else NA_character_,
      last_updated = timestamp_now(),
      last_run_date = format(Sys.Date(), "%Y-%m-%d")
    )]

    if (identical(res$status, "SUCCESS")) {
      dt[idx, notes := ""]
    } else if (!is.null(res$message) && nzchar(res$message)) {
      oldNote <- dt[idx, notes]
      noteVal <- if (is.na(oldNote) || !nzchar(oldNote)) res$message else paste(oldNote, res$message, sep = " | ")
      dt[idx, notes := noteVal]
    }

    if (!is.null(res$summary) && nrow(res$summary)) {
      res$summary[, run_name := res$runName]
      summaries[[length(summaries) + 1]] <- res$summary
    }

    runRec <- data.table::data.table(
      timestamp = Sys.time(),
      runName = res$runName,
      scenario_id = cfg$scenario_id,
      totalRate = cfg$total_rate,
      mode = cfg$mode,
      runInterval = cfg$run_interval,
      startYear = cfg$start_year,
      endYear = cfg$end_year,
      useFire = cfg$use_fire,
      fireModules = if (length(cfg$fire_modules)) paste(cfg$fire_modules, collapse = ";") else NA_character_,
      packageOutputs = cfg$package_outputs,
      log = res$log,
      outputPath = res$outputPath,
      verificationCsv = if (!is.null(res$verification)) res$verification$csv else NA_character_,
      verificationCsvAbs = if (!is.null(res$verification)) res$verification$absolute_csv else NA_character_,
      status = res$status,
      message = res$message
    )
    if (file.exists(ctx$runDataPath)) {
      old <- tryCatch(data.table::fread(ctx$runDataPath), error = function(e) NULL)
      if (!is.null(old)) runRec <- data.table::rbindlist(list(old, runRec), use.names = TRUE, fill = TRUE)
    }
    data.table::fwrite(runRec, ctx$runDataPath)
  }

  ensure_scenario_col_order(dt)
  out <- strip_helper_cols(dt)
  data.table::fwrite(out, csv_path)
  list(results = results, table = out, summaries = summaries)
}

maybe_run_from_cli <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  ctx <- build_rates_context()
  opts <- parse_cli_args(args)
  if (opts$show_help) {
    cat(paste(
      "Usage: Rscript validation/rates/scenarios/run_rates_suite.R",
      "        [--csv=PATH] [--scenario=id1,id2] [--force] [--mode=name] [--dry-run]",
      "  --csv=PATH        Scenario matrix (default: validation/rates/scenarios/scenarios.csv)",
      "  --scenario=IDS    Comma-separated scenario_id list to run",
      "  --force           Run even if status already SUCCESS",
      "  --mode=NAME       default|respect|all (selection rules)",
      "  --dry-run         Show selected scenarios without executing",
      sep = "\n"
    ))
    quit(save = "no", status = 0, runLast = FALSE)
  }

  csvPath <- opts$csv %||% file.path(ctx$suiteRoot, "scenarios.csv")
  csvPath <- normalizePath(csvPath, winslash = "/", mustWork = TRUE)
  res <- execute_scenarios_from_csv(ctx, csvPath,
                                    scenario_ids = opts$scenario_ids,
                                    force = opts$force,
                                    dry_run = opts$dry_run,
                                    mode = opts$mode)

  if (!opts$dry_run) {
    if (length(res$summaries)) {
      summaryAll <- data.table::rbindlist(res$summaries, use.names = TRUE, fill = TRUE)
      if (nrow(summaryAll)) {
        summaryPath <- file.path(ctx$scratchRoot, sprintf("rate_summaries_%s.csv", timestamp_tag()))
        data.table::fwrite(summaryAll, summaryPath)
        message("Wrote summary metrics to ", summaryPath)
      }
    }
    failCount <- sum(vapply(res$results, function(x) identical(x$status, "FAIL"), logical(1)))
    quit(save = "no", status = if (failCount > 0) 1 else 0, runLast = FALSE)
  }
  quit(save = "no", status = 0, runLast = FALSE)
}

if (!interactive() && Sys.getenv("RATES_SUITE_IMPORT", unset = "0") != "1") {
  maybe_run_from_cli()
}
