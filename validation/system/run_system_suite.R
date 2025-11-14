#!/usr/bin/env Rscript

# Lightweight scenario runner used by scenario scripts.
# Scenario scripts should define a `cfg` list (see below) and call `run_scenario(cfg)`.
#
# Required packages (pre-installed): terra, data.table, SpaDES.project, SpaDES.core, reproducible

suppressPackageStartupMessages({
  ok <- vapply(c("terra", "data.table", "SpaDES.project", "SpaDES.core", "reproducible", "geodata"),
               requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  if (!all(ok)) {
    stop(sprintf("Missing required packages: %s", paste(names(ok)[!ok], collapse = ", ")),
         call. = FALSE)
  }
  # Attach key packages so module code that uses unqualified functions (e.g., prepInputs) works reliably
  try(suppressPackageStartupMessages(library(reproducible)), silent = TRUE)
  try(suppressPackageStartupMessages(library(SpaDES.core)), silent = TRUE)
  try(suppressPackageStartupMessages(library(SpaDES.tools)), silent = TRUE)
  try(suppressPackageStartupMessages(library(geodata)), silent = TRUE)
})

if (!exists("elevation_30s", mode = "function")) {
  elevation_30s <- geodata::elevation_30s
}

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (tolower(Sys.getenv("RUN_SCENARIO_DEBUG", unset = "0")) %in% c("1","true","yes","y")) {
  options(run_scenario.debug = TRUE)
}

# Force Require to stay offline and reuse only pre-installed packages.
options(
  Require.install = FALSE,
  Require.offlineMode = TRUE
)
require_cache <- file.path(project_root, "cache", "Require_runner")
dir.create(require_cache, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(REQUIRE_HOME = require_cache)

# suite configuration (allows reusing this runner for other validation suites)
suite_id <- getOption("validation.suite_id", Sys.getenv("VALIDATION_SUITE_ID", unset = "system"))
if (is.null(suite_id) || !nzchar(suite_id)) suite_id <- "system"
suite_label <- getOption("validation.suite_label", suite_id)

suite_entrypoint <- getOption(
  "validation.suite_entrypoint",
  file.path("validation", suite_id, paste0("run_", suite_id, "_suite.R"))
)

suite_validation_root <- getOption("validation.suite_root",
                                   file.path(project_root, "validation", suite_id))
suite_validation_root <- normalizePath(suite_validation_root, winslash = "/",
                                       mustWork = FALSE)

suite_scratch_root <- getOption("validation.scratch_root",
                                file.path(project_root, "scratch", "validation", suite_id))
suite_scratch_root <- normalizePath(suite_scratch_root, winslash = "/",
                                    mustWork = FALSE)

suite_cache_root <- getOption("validation.cache_root",
                              file.path(project_root, "cache", "validation", suite_id))
suite_cache_root <- normalizePath(suite_cache_root, winslash = "/",
                                  mustWork = FALSE)

suite_outputs_root <- getOption("validation.outputs_root",
                                file.path(project_root, "outputs", "validation", suite_id))
suite_outputs_root <- normalizePath(suite_outputs_root, winslash = "/",
                                    mustWork = FALSE)

suite_default_csv <- getOption("validation.default_csv",
                               file.path(suite_validation_root, "testing_runs.csv"))
suite_default_csv <- normalizePath(suite_default_csv, winslash = "/",
                                   mustWork = FALSE)

# --- helpers -----------------------------------------------------------------

ensure_path <- function(p) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  normalizePath(p, winslash = "/", mustWork = TRUE)
}

resolve_fire_mask_location <- function(pathValue) {
  out <- list(dir = NULL, file = NULL)
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) {
    return(out)
  }
  candidates <- unique(c(
    pathValue,
    path.expand(pathValue),
    file.path(project_root, pathValue)
  ))
  for (candidate in candidates) {
    expanded <- tryCatch(
      normalizePath(candidate, winslash = "/", mustWork = FALSE),
      error = function(...) NULL
    )
    if (is.null(expanded)) next
    if (dir.exists(expanded)) {
      out$dir <- expanded
      return(out)
    }
    if (file.exists(expanded)) {
      parent <- dirname(expanded)
      if (dir.exists(parent)) {
        out$dir <- parent
        out$file <- expanded
        return(out)
      }
    }
  }
  out
}

locate_input_data <- function() {
  candidates <- unique(c(
    Sys.getenv("VALIDATION_INPUT_ROOT", unset = Sys.getenv("PRE_DOWNLOADED_PATH", unset = "")),
    file.path(project_root, "data", "synthetic", "raw"),
    file.path(project_root, "data", "raw"),
    file.path(project_root, "data", "pre_downloaded"),
    file.path(project_root, "..", "data", "pre_downloaded"),
    file.path(project_root, "..", "..", "data", "pre_downloaded")
  ))
  for (candidate in candidates) {
    if (!nzchar(candidate)) next
    expanded <- path.expand(candidate)
    if (dir.exists(expanded)) {
      return(normalizePath(expanded, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Input data folder not found. Set VALIDATION_INPUT_ROOT or populate ./data/raw (fallback: ./data/pre_downloaded).",
       call. = FALSE)
}

create_local_rtm <- function(studyArea, resolution = 250) {
  stopifnot(inherits(studyArea, "SpatVector"))
  saProj <- terra::project(studyArea, terra::crs(studyArea))
  rtm <- terra::rast(extent = terra::ext(saProj), resolution = resolution, crs = terra::crs(saProj))
  rtm[] <- 0
  terra::mask(rtm, saProj)
}

apply_input_path <- function(project, inputPath) {
  project$paths$inputPath <- inputPath
  projOpts <- attr(project, "projectOptions")
  if (is.null(projOpts)) projOpts <- list()
  projOpts$spades.inputPath <- inputPath
  projOpts$reproducible.inputPaths <- inputPath
  attr(project, "projectOptions") <- projOpts
  project
}

timestamp_tag <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

timestamp_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

relative_to_root <- function(pathValue) {
  if (is.null(pathValue) || is.na(pathValue) || !nzchar(pathValue)) {
    return(NA_character_)
  }
  normalized <- normalizePath(pathValue, winslash = "/", mustWork = FALSE)
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (startsWith(normalized, prefix)) {
    sub(prefix, "", normalized, fixed = TRUE)
  } else {
    normalized
  }
}

collapse_paths <- function(paths) {
  if (is.null(paths) || !length(paths)) return("")
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- unique(paths)
  if (!length(paths)) "" else paste(paths, collapse = ";")
}

safe_merge <- function(a, b) {
  # recursively merges named lists with b overriding a
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  stopifnot(is.list(a), is.list(b))
  out <- a
  for (nm in names(b)) {
    if (!is.null(out[[nm]]) && is.list(out[[nm]]) && is.list(b[[nm]])) {
      out[[nm]] <- safe_merge(out[[nm]], b[[nm]])
    } else {
      out[[nm]] <- b[[nm]]
    }
  }
  out
}

prepare_disturbanceDT <- function(modulePath, preDownloadedPath, useWindData = TRUE) {
  csv1 <- file.path(modulePath, "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
  if (!file.exists(csv1)) return(NULL)
  dt <- data.table::fread(csv1)

  to_file_url <- function(path) {
    if (is.na(path) || !nzchar(path)) return(path)
    if (grepl("^file://", path, ignore.case = TRUE)) return(path)
    if (dir.exists(path)) return(path)
    paste0("file://", path)
  }

  resolveLocalURL <- function(fileName, dataType) {
    if (!nzchar(fileName)) return(NA_character_)
    primary <- normalizePath(file.path(preDownloadedPath, fileName), winslash = "/", mustWork = FALSE)
    if (file.exists(primary)) {
      if (grepl("\\.shp$", fileName, ignore.case = TRUE)) {
        base <- sub("\\.shp$", "", fileName, ignore.case = TRUE)
        zipCand <- normalizePath(file.path(preDownloadedPath, paste0(base, ".zip")), winslash = "/", mustWork = FALSE)
        if (file.exists(zipCand)) return(to_file_url(zipCand))
        zips <- list.files(
          preDownloadedPath,
          pattern = paste0("^", gsub("\\.", "\\.", basename(base)), ".*\\.zip$"),
          ignore.case = TRUE,
          full.names = TRUE
        )
        if (length(zips) > 0) return(to_file_url(normalizePath(zips[[1]], winslash = "/", mustWork = FALSE)))
        prefix <- sub("_[^_]*$", "", basename(base))
        if (nzchar(prefix)) {
          zips2 <- list.files(
            preDownloadedPath,
            pattern = paste0("^", gsub("\\.", "\\.", prefix), ".*\\.zip$"),
            ignore.case = TRUE,
            full.names = TRUE
          )
          if (length(zips2) > 0) return(to_file_url(normalizePath(zips2[[1]], winslash = "/", mustWork = FALSE)))
        }
      }
      if (tolower(dataType) == "mif" && grepl("\\.zip$", primary, ignore.case = TRUE)) {
        return(to_file_url(primary))
      }
      return(to_file_url(primary))
    }
    if (grepl("\\.shp$", fileName, ignore.case = TRUE)) {
      zipCand <- normalizePath(file.path(preDownloadedPath, sub("\\.shp$", ".zip", fileName, ignore.case = TRUE)),
                               winslash = "/", mustWork = FALSE)
      if (file.exists(zipCand)) return(to_file_url(zipCand))
    }
    if (grepl("\\.zip$", fileName, ignore.case = TRUE)) {
      base <- sub("\\.zip$", "", fileName, ignore.case = TRUE)
      shpCand <- normalizePath(file.path(preDownloadedPath, paste0(base, ".shp")), winslash = "/", mustWork = FALSE)
      if (file.exists(shpCand)) return(to_file_url(shpCand))
      gdbCand <- normalizePath(file.path(preDownloadedPath, base), winslash = "/", mustWork = FALSE)
      if (dir.exists(gdbCand)) return(gdbCand)
    }
    if (grepl("\\.gdb$", fileName, ignore.case = TRUE)) {
      zipGDB <- normalizePath(file.path(preDownloadedPath, paste0(basename(fileName), ".zip")), winslash = "/", mustWork = FALSE)
      if (file.exists(zipGDB)) return(to_file_url(zipGDB))
    }
    NA_character_
  }

  dt$URL <- mapply(resolveLocalURL, dt$fileName, dt$dataType, USE.NAMES = FALSE)

  nrnIdx <- grepl("^NRN_NT_13_0_ROADSEG\\.shp$", dt$fileName, ignore.case = TRUE)
  if (any(nrnIdx)) {
    nrnZip <- list.files(preDownloadedPath, pattern = "^NRN_NT_13_0_.*\\.zip$", ignore.case = TRUE, full.names = TRUE)
    if (length(nrnZip) > 0) {
      dt$URL[nrnIdx] <- to_file_url(normalizePath(nrnZip[[1]], winslash = "/", mustWork = FALSE))
    }
  }

  dt <- dt[!(tolower(dataName) == "roads" & grepl("NRN_NT_13_0_ROADSEG\\.shp$", fileName, ignore.case = TRUE))]

  if (!useWindData) {
    dt <- dt[dataName != "Energy"]
  } else {
    dt <- dt[!(dataName == "Energy" & dataClass %in% c("windTurbines", "potentialWindTurbines"))]
  }

  if (anyNA(dt$URL)) {
    missing <- unique(dt[is.na(URL), .(dataName, dataClass, fileName)])
    msg <- paste(capture.output(print(missing)), collapse = " | ")
    warning(paste0("Some disturbanceDT rows could not be resolved to local files; they will be dropped: ", msg), immediate. = TRUE)
    dt <- dt[!is.na(URL)]
  }

  fastMode <- tolower(Sys.getenv("DIAGNOSE_FAST", unset = "1")) %in% c("1","true","yes","y")
  enableAllDatasets <- tolower(Sys.getenv("ENABLE_ALL_DATASETS", unset = "0")) %in% c("1","true","yes","y")
  if (!enableAllDatasets && fastMode) {
    dt <- dt[!dataName %in% c("Energy", "roads", "forestry", "mining")]
  }

  dt
}

default_what_to_combine <- function() {
  data.table::data.table(
    datasetName = c("oilGas", "oilGas"),
    dataClasses = c("potentialOilGas", "potentialOilGas"),
    toDifferentiate = c(NA_character_, "C2H4_BCR6_NT1"),
    activeProcess = c(NA_character_, NA_character_)
  )
}

coerce_fire_param_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NULL)
  val <- trimws(as.character(value[[1]]))
  if (!nzchar(val)) return(NULL)
  low <- tolower(val)
  if (low %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (low %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  maybe_num <- suppressWarnings(as.numeric(val))
  if (!is.na(maybe_num)) return(maybe_num)
  val
}

parse_fire_generation_spec <- function(value) {
  if (is.null(value) || length(value) == 0) return(NULL)
  raw <- trimws(as.character(value[[1]]))
  if (!nzchar(raw)) return(NULL)
  low <- tolower(raw)
  if (low %in% c("todo", "placeholder", "none", "na", "null", "skip")) return(NULL)

  base <- raw
  inner <- ""

  if (grepl("\\(", raw) && grepl("\\)", raw)) {
    base <- sub("\\(.*$", "", raw)
    inner <- sub("^[^(]*\\((.*)\\)[^)]*$", "\\1", raw)
  } else if (grepl(":", raw, fixed = TRUE)) {
    pieces <- strsplit(raw, ":", fixed = TRUE)[[1]]
    base <- pieces[[1]]
    if (length(pieces) > 1) {
      inner <- paste(pieces[-1], collapse = ":")
    }
  }
  base_trim <- trimws(base)

  params <- list()
  if (nzchar(inner)) {
    tokens <- strsplit(inner, "[;,]")[[1]]
    tokens <- trimws(tokens)
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens)) {
      for (token in tokens) {
        kv <- strsplit(token, "=", fixed = TRUE)[[1]]
        if (length(kv) == 2) {
          params[[trimws(kv[1])]] <- coerce_fire_param_value(kv[2])
        } else {
          params[[paste0("arg", length(params) + 1)]] <- coerce_fire_param_value(kv[length(kv)])
        }
      }
    }
  }

  list(
    raw = raw,
    module = base_trim,
    strategy = tolower(base_trim),
    params = params
  )
}

# Create an on-the-fly fire mask (SpatRaster) aligned to rasterToMatch
# Supported patterns via fireSpec$params:
#   pattern: 'checker' | 'sparse' | 'full' | 'none' (default 'checker')
#   burnModulo: integer for checker (default 5)
#   p: probability for sparse (default 0.01)
#   burnValue: numeric cell value for burned cells (default 1)
build_provided_fire_mask <- function(rtm, fireSpec, startYear = NULL) {
  stopifnot(inherits(rtm, "SpatRaster"))

  first_non_empty <- function(vals) {
    if (is.null(vals)) return(NULL)
    for (val in vals) {
      if (is.null(val) || isTRUE(is.na(val))) next
      valChar <- trimws(as.character(val))
      if (nzchar(valChar)) return(valChar)
    }
    NULL
  }

  maskDir <- first_non_empty(list(
    fireSpec$params$resolved_dir,
    fireSpec$params$dir,
    fireSpec$params$path,
    fireSpec$params$folder,
    fireSpec$params$location,
    fireSpec$params$source,
    fireSpec$params$root
  ))
  maskFileParam <- first_non_empty(list(
    fireSpec$params$resolved_file,
    fireSpec$params$file,
    fireSpec$params$filename,
    fireSpec$params$layer
  ))
  burnValue <- as.numeric(fireSpec$params$burnValue %||% 1)
  startYearInt <- if (!is.null(startYear)) suppressWarnings(as.integer(startYear)) else NA_integer_

  candidateFiles <- character()
  if (!is.null(maskFileParam)) {
    candidates <- unique(c(
      maskFileParam,
      path.expand(maskFileParam)
    ))
    if (!is.null(maskDir) && nzchar(maskDir)) {
      candidates <- unique(c(candidates, file.path(maskDir, maskFileParam)))
    }
    for (cand in candidates) {
      expanded <- tryCatch(
        normalizePath(cand, winslash = "/", mustWork = FALSE),
        error = function(...) NULL
      )
      if (is.null(expanded)) next
      if (file.exists(expanded)) {
        candidateFiles <- unique(c(candidateFiles, expanded))
      }
    }
  }
  if (!is.null(maskDir) && nzchar(maskDir)) {
    dirExpanded <- tryCatch(
      normalizePath(maskDir, winslash = "/", mustWork = FALSE),
      error = function(...) NULL
    )
    if (!is.null(dirExpanded) && dir.exists(dirExpanded)) {
      maskDir <- dirExpanded
      dirFiles <- list.files(maskDir, pattern = "\\.tif(f)?$", full.names = TRUE)
      if (length(dirFiles)) {
        burnHits <- dirFiles[grepl("rstCurrentBurn", basename(dirFiles), ignore.case = TRUE)]
        if (length(burnHits)) {
          dirFiles <- unique(c(burnHits, setdiff(dirFiles, burnHits)))
        }
        candidateFiles <- unique(c(candidateFiles, dirFiles))
      }
    }
  }
  if (length(candidateFiles)) {
    candidateFiles <- candidateFiles[file.exists(candidateFiles)]
    if (!is.na(startYearInt)) {
      patternYear <- sprintf("%04d", startYearInt)
      idx <- which(grepl(patternYear, basename(candidateFiles), fixed = TRUE))
      if (length(idx)) {
        candidateFiles <- c(candidateFiles[idx], candidateFiles[-idx])
      }
    }
    for (filePath in candidateFiles) {
      burn <- tryCatch(terra::rast(filePath), error = function(...) NULL)
      if (is.null(burn)) next
      burn <- tryCatch({
        if (!terra::compareGeom(burn, rtm, stopOnError = FALSE)) {
          terra::project(burn, rtm, method = "near")
        } else {
          burn
        }
      }, error = function(...) {
        tryCatch(terra::resample(burn, rtm, method = "near"), error = function(...) NULL)
      })
      if (is.null(burn)) next
      burn <- terra::mask(burn, rtm)
      vals <- terra::values(burn)
      vals[is.na(vals)] <- 0
      if (!is.na(burnValue)) {
        vals <- ifelse(vals > 0, burnValue, 0)
      }
      terra::values(burn) <- vals
      return(burn)
    }
  }

  pattern <- tolower(as.character(fireSpec$params$pattern %||% "checker"))
  burn <- terra::rast(rtm)
  terra::values(burn) <- 0
  nc <- terra::ncell(burn)
  if (pattern %in% c("none", "zero", "off")) {
    return(burn)
  }
  if (pattern %in% c("full", "all", "ones")) {
    terra::values(burn) <- burnValue
    return(burn)
  }
  if (pattern %in% c("sparse", "random")) {
    set.seed(as.integer(fireSpec$params$seed %||% NA_integer_))
    p <- as.numeric(fireSpec$params$p %||% 0.01)
    idx <- which(stats::runif(nc) < p)
    if (length(idx)) terra::values(burn)[idx] <- burnValue
    return(burn)
  }
  # default: checker
  modulo <- as.integer(fireSpec$params$burnModulo %||% 5)
  if (is.na(modulo) || modulo < 1) modulo <- 5
  xy <- terra::xyFromCell(burn, seq_len(nc))
  pat <- (floor(xy[, 1] / modulo) + floor(xy[, 2] / modulo)) %% modulo
  idx <- which(pat == 0)
  if (length(idx)) terra::values(burn)[idx] <- burnValue
  burn
}

inject_fire_module <- function(modules, params, fireSpec) {
  if (is.null(fireSpec)) {
    return(list(modules = modules, params = params, moduleName = NULL))
  }

  # Fast-path: provided mask strategies don't load a module
  strat <- tolower(trimws(fireSpec$strategy %||% ""))
  if (nzchar(strat) && strat %in% c("provided_mask", "provided", "mask", "fire_mask")) {
    return(list(modules = modules, params = params, moduleName = NULL))
  }

  moduleName <- trimws(as.character(fireSpec$module %||% fireSpec$strategy))
  if (!nzchar(moduleName)) {
    warning("Unable to resolve fire_generation strategy; skipping fire module injection.", call. = FALSE)
    return(list(modules = modules, params = params, moduleName = NULL))
  }
  if (tolower(moduleName) %in% c("synthetic_stub", "stub", "systemtest_fire")) {
    stop("The systemTest_Fire stub is no longer supported; please specify a real fire module in fire_generation.", call. = FALSE)
  }

  if (moduleName %in% modules) {
    modules <- modules[!duplicated(modules)]
  } else {
    anthroIdx <- match("anthroDisturbance_Generator", modules)
    if (is.na(anthroIdx)) {
      modules <- c(modules, moduleName)
    } else {
      modules <- append(modules, moduleName, after = max(0, anthroIdx - 1))
    }
    modules <- modules[!duplicated(modules)]
  }

  if (length(fireSpec$params)) {
    moduleParams <- params[[moduleName]] %||% list()
    for (nm in names(fireSpec$params)) {
      moduleParams[[nm]] <- fireSpec$params[[nm]]
    }
    params[[moduleName]] <- moduleParams
  }

  list(modules = modules, params = params, moduleName = moduleName)
}

# --- public API ---------------------------------------------------------------

# cfg list fields (typical):
# - branch: optional descriptive tag (no longer tied to module selection)
# - scenario_name: string used in run name
# - study_area: terra SpatVector
# - modules: character vector of module names (order)
# - times: list(start=2011, end=2021)
# - use_eccc: logical (if TRUE, ignore total_rate and use ECCC-derived rates; if FALSE and disturbance_rate is provided, uses it)
# - total_rate: numeric or NULL
# - disturbance_rate: optional data.table with columns: dataName, dataClass, disturbanceType, disturbanceOrigin, disturbanceRate (percent per year)
# - use_wind_data: logical, default TRUE
# - params: named list for modules; runner will inject .runName and .inputFolderFireLayer into anthroDisturbance_Generator
# - options: list of SpaDES options overrides
# - run_name: optional explicit run name
# - output_path: optional explicit output path
# - module_path: optional module path override
# - run_records_csv: optional path; defaults to scratch/run_data.csv

run_scenario <- function(cfg) {
  # basic validation
  for (req in c("scenario_name", "study_area")) {
    if (is.null(cfg[[req]])) stop(sprintf("cfg$%s is required", req), call. = FALSE)
  }
  if (!inherits(cfg$study_area, "SpatVector")) stop("cfg$study_area must be a terra::SpatVector", call. = FALSE)

  # paths
  scratchRoot <- ensure_path(suite_scratch_root)
  logsDir <- ensure_path(file.path(scratchRoot, "scenario_logs"))
  scratchPath <- scratchRoot
  cachePath <- ensure_path(suite_cache_root)
  inputDataPath <- locate_input_data()
  outputsRoot <- ensure_path(suite_outputs_root)

  # resources tuning (conservative defaults; allow user to override externally if needed)
  CORES <- as.integer(Sys.getenv("CORES", unset = "8"))
  options(spades.DTthreads = CORES)
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(CORES, restore_after_fork = TRUE)
  }
  Sys.setenv(OMP_NUM_THREADS = CORES, OPENBLAS_NUM_THREADS = CORES, MKL_NUM_THREADS = CORES)
  if (requireNamespace("terra", quietly = TRUE)) terra::terraOptions(tempdir = scratchPath)

  # modules & paths
  default_modules <- c("anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator")
  branch <- as.character(cfg$branch %||% "")
  modulePath <- cfg$module_path %||% file.path(project_root, "modules")
  modules <- default_modules
  modules <- modules[!is.na(modules) & nzchar(modules)]
  if (isTRUE(getOption("run_scenario.debug", FALSE))) {
    message("[runner] modules vector: ", paste(modules, collapse = ", "))
  }

  # Patch convertHTTPsToGH to tolerate NA/blank entries (known SpaDES.project bug).
  if (!isTRUE(getOption("run_scenario.patched_convertHTTPsToGH", FALSE))) {
    try({
      spaNS <- asNamespace("SpaDES.project")
      if (exists("convertHTTPsToGH", envir = spaNS, inherits = FALSE)) {
        original_convertHTTPsToGH <- get("convertHTTPsToGH", envir = spaNS)
        patched_convertHTTPsToGH <- function(url) {
          if (is.null(url)) return(url)
          out <- url
          idx <- which(!is.na(url) & nzchar(url))
          if (length(idx)) {
            out[idx] <- original_convertHTTPsToGH(url[idx])
          }
          out
        }
        if (bindingIsLocked("convertHTTPsToGH", spaNS)) {
          unlockBinding("convertHTTPsToGH", spaNS)
        }
        assign("convertHTTPsToGH", patched_convertHTTPsToGH, envir = spaNS)
        lockBinding("convertHTTPsToGH", spaNS)
        options(run_scenario.patched_convertHTTPsToGH = TRUE)
      }
    }, silent = TRUE)
  }
  fireSpec <- cfg$fire_generation
  if (!is.null(fireSpec) && !(is.list(fireSpec) && !is.null(fireSpec$strategy))) {
    fireSpec <- parse_fire_generation_spec(fireSpec)
  }
  if (is.null(fireSpec) && !is.null(cfg$metadata) && is.list(cfg$metadata) &&
      !is.null(cfg$metadata$fire_generation)) {
    fireSpec <- parse_fire_generation_spec(cfg$metadata$fire_generation)
  }
  fireSpecLabel <- NA_character_
  if (!is.null(fireSpec)) {
    fireSpecLabel <- if (!is.null(fireSpec$raw) && nzchar(fireSpec$raw)) fireSpec$raw else fireSpec$strategy
    # Avoid attaching provided_mask label to metadata to prevent unintended downstream eval
    if (!tolower(trimws(fireSpec$strategy %||% "")) %in% c("provided_mask","provided","mask","fire_mask")) {
      cfg$metadata <- safe_merge(cfg$metadata %||% list(), list(fire_generation = fireSpecLabel))
    }
  }
  fireMaskDir <- NULL
  fireMaskFile <- NULL
  if (!is.null(fireSpec)) {
    strat <- tolower(trimws(fireSpec$strategy %||% ""))
    isProvidedMask <- nzchar(strat) && strat %in% c("provided_mask", "provided", "mask", "fire_mask")
    if (isProvidedMask) {
      fileFields <- c("resolved_file", "file", "filename", "layer")
      for (field in fileFields) {
        val <- fireSpec$params[[field]]
        if (is.null(val) || isTRUE(is.na(val))) next
        valChar <- trimws(as.character(val))
        if (!nzchar(valChar)) next
        candidates <- unique(c(
          valChar,
          path.expand(valChar),
          file.path(project_root, valChar)
        ))
        for (cand in candidates) {
          expanded <- tryCatch(
            normalizePath(cand, winslash = "/", mustWork = FALSE),
            error = function(...) NULL
          )
          if (is.null(expanded) || !file.exists(expanded)) next
          fireMaskFile <- expanded
          fireMaskDir <- dirname(expanded)
          break
        }
        if (!is.null(fireMaskFile)) break
      }
      if (is.null(fireMaskDir)) {
        pathFields <- c("resolved_dir", "dir", "path", "folder", "location", "source", "root")
        for (field in pathFields) {
          val <- fireSpec$params[[field]]
          if (is.null(val) || isTRUE(is.na(val))) next
          valChar <- trimws(as.character(val))
          if (!nzchar(valChar)) next
          loc <- resolve_fire_mask_location(valChar)
          if (!is.null(loc$dir) && nzchar(loc$dir)) {
            fireMaskDir <- loc$dir
            if (!is.null(loc$file) && nzchar(loc$file) && is.null(fireMaskFile)) {
              fireMaskFile <- loc$file
            }
            break
          }
        }
      }
      if (!is.null(fireMaskDir) && nzchar(fireMaskDir)) {
        fireSpec$params$resolved_dir <- fireMaskDir
        if (!is.null(fireMaskFile) && nzchar(fireMaskFile)) {
          fireSpec$params$resolved_file <- fireMaskFile
        }
      }
    }
  }
  fireModuleName <- NULL

  # run name & output path
  ecccTag <- if (isTRUE(cfg$use_eccc)) "ECCC" else if (!is.null(cfg$total_rate)) sprintf("Supplied_%0.2f", cfg$total_rate) else "Unspecified"
  namePieces <- c()
  if (nzchar(branch)) namePieces <- c(namePieces, branch)
  namePieces <- c(namePieces, cfg$scenario_name, ecccTag, timestamp_tag())
  runName <- cfg$run_name %||% paste(namePieces, collapse = "_")
  outputPath <- cfg$output_path %||% file.path(outputsRoot, runName)
  ensure_path(outputPath)
  outputPathRel <- relative_to_root(outputPath)

  # disturbance table toggle for wind
  useWind <- if (is.null(cfg$use_wind_data)) TRUE else isTRUE(cfg$use_wind_data)
  disturbanceDT <- prepare_disturbanceDT(modulePath, inputDataPath, useWindData = useWind)

  # assemble params with sensible defaults; allow overrides
  genDefaults <- list(
    .inputFolderFireLayer = fireMaskDir %||% outputPath,
    .runName = runName,
    generatedDisturbanceAsRaster = FALSE,
    growthStepGenerating = 0.01,
    totalDisturbanceRate = if (isTRUE(cfg$use_eccc)) NULL else cfg$total_rate,
    saveInitialDisturbances = FALSE,
    useClusterMethod = FALSE,
    runClusteringInParallel = FALSE,
    maskWaterAndMountainsFromLines = FALSE
  )
  params <- list(
    potentialResourcesNT_DataPrep = list(
      whatToCombine = default_what_to_combine()
    ),
    anthroDisturbance_Generator = genDefaults
  )
  if (!is.null(cfg$params) && is.list(cfg$params)) {
    params <- safe_merge(params, cfg$params)
  }
  fireIntegration <- inject_fire_module(modules, params, fireSpec)
  modules <- fireIntegration$modules
  modules <- modules[!is.na(modules) & nzchar(modules)]
  params <- fireIntegration$params
  fireModuleName <- fireIntegration$moduleName

  # options & times
  times <- cfg$times %||% list(start = 2011, end = 2031)
  optsDefaults <- list(
    spades.allowInitDuringSimInit = TRUE,
    reproducible.cacheSaveFormat = "rds",
    repos = c(
      pikpiam = "https://pik-piam.r-universe.dev",
      CRAN = "https://cloud.r-project.org"
    ),
    spades.recoveryMode = 0,
    spades.DTthreads = 1,
    spades.scratchPath = scratchPath,
    reproducible.gdalwarp = TRUE,
    reproducible.inputPaths = inputDataPath,
    reproducible.destinationPath = outputPath,
    reproducible.useMemoise = FALSE
  )
  opts <- safe_merge(optsDefaults, cfg$options %||% list())
  if (!is.null(cfg$seed) && !is.na(cfg$seed)) {
    seedInt <- as.integer(cfg$seed)
    if (!is.na(seedInt)) {
      set.seed(seedInt)
      on.exit({
        try({
          if (exists("Random.seed", envir = .GlobalEnv)) remove(list = "Random.seed", envir = .GlobalEnv)
        }, silent = TRUE)
      }, add = TRUE)
      opts$reproducible.seed <- seedInt
      opts$spades.seed <- seedInt
    }
  }

  # setup project
  # Package set — align with runMe_Testing.R to avoid GH-pinned repos and unloading issues
  pkgs <- c("googledrive", "RCurl", "XML", "igraph", "qs", "usethis", "geodata",
            "SpaDES.tools", "SpaDES.core", "reproducible", "Require (>= 1.0.1)")

  out <- SpaDES.project::setupProject(
    runName = runName,
    paths = list(projectPath = project_root,
                 modulePath = modulePath,
                 cachePath = cachePath,
                 scratchPath = scratchPath,
                 outputPath = outputPath,
                 inputPath = inputDataPath),
    modules = modules,
    options = opts,
    times = times,
    functions = file.path(project_root, "R", "studyAreaMakers.R"),
    authorizeGDrive = NULL,
    shortProvinceName = "NT",
    studyArea = cfg$study_area,
    rasterToMatch = create_local_rtm(cfg$study_area),
    disturbanceDT = disturbanceDT,
    params = params,
    DisturbanceRate = cfg$disturbance_rate %||% NULL,
    packages = pkgs
  )

  out <- apply_input_path(out, inputDataPath)

  # Inject a provided fire mask if requested (fast route, no fire module)
  if (!is.null(fireSpec) && tolower(fireSpec$strategy) %in% c("provided_mask", "provided", "mask", "fire_mask")) {
    # Build a mask aligned to rasterToMatch; keep it static for fast coverage
    burn <- build_provided_fire_mask(
      rtm = create_local_rtm(cfg$study_area),
      fireSpec = fireSpec,
      startYear = times$start %||% NA_real_
    )
    # Attach as an initial object so anthroDisturbance_Generator can use rstCurrentBurn
    objs <- out$objects %||% list()
    objs$rstCurrentBurn <- burn
    out$objects <- objs
    # Also write to outputs so any file-based lookup can find it
    try({
      terra::writeRaster(burn, filename = file.path(outputPath, "rstCurrentBurn.tif"), overwrite = TRUE)
    }, silent = TRUE)
  }

  # log sink
  logFile <- file.path(logsDir, paste0(runName, "_", timestamp_tag(), ".log"))
  logRel <- relative_to_root(logFile)
  con <- file(logFile, open = "wt"); on.exit(try(close(con), silent = TRUE), add = TRUE)
  sink(con, type = "output"); on.exit(try(sink(type = "output"), silent = TRUE), add = TRUE)
  sink(con, type = "message"); on.exit(try(sink(type = "message"), silent = TRUE), add = TRUE)

  status <- "SUCCESS"; errMsg <- NA_character_
  if (isTRUE(getOption("run_scenario.debug", FALSE))) {
    res <- do.call(SpaDES.core::simInitAndSpades, out)
  } else {
    res <- tryCatch({
      do.call(SpaDES.core::simInitAndSpades, out)
    }, error = function(e) {
      status <<- "FAIL"
      errMsg <<- conditionMessage(e)
      NULL
    })
  }

  # basic diagnosis: output dir exists and has files
  okOutputs <- dir.exists(outputPath) && length(list.files(outputPath, all.files = TRUE, recursive = TRUE)) > 0
  diag <- list(output_exists = okOutputs)
  if (!is.null(fireModuleName)) {
    diag$fire_module <- fireModuleName
  }

  # record run
  genParams <- params$anthroDisturbance_Generator
  siteSel <- genParams$siteSelectionAsDistributing
  siteSelStr <- if (is.null(siteSel) || (length(siteSel) && all(is.na(siteSel)))) {
    NA_character_
  } else if (length(siteSel) == 0) {
    NA_character_
  } else {
    paste(siteSel[!is.na(siteSel)], collapse = ";")
  }
  connectingVal <- if (!is.null(genParams$connectingBlockSize)) as.numeric(genParams$connectingBlockSize) else NA_real_
  branchLabel <- if (nzchar(branch)) branch else NA_character_
  rec <- data.table::data.table(
    timestamp = Sys.time(),
    runName = runName,
    branch = branchLabel,
    scenario = cfg$scenario_name,
    modules = paste(modules, collapse = ";"),
    useECCC = isTRUE(cfg$use_eccc),
    totalRate = if (isTRUE(cfg$use_eccc)) NA_real_ else as.numeric(cfg$total_rate),
    useWind = useWind,
    startYear = as.integer(times$start),
    endYear = as.integer(times$end),
    seed = if (!is.null(cfg$seed)) as.integer(cfg$seed) else NA_integer_,
    status = status,
    message = if (is.na(errMsg)) "" else errMsg,
    log = logRel,
    outputPath = outputPathRel,
    outputExists = okOutputs,
    fireGeneration = if (is.na(fireSpecLabel)) NA_character_ else fireSpecLabel,
    generatedDisturbanceAsRaster = isTRUE(genParams$generatedDisturbanceAsRaster),
    useClusterMethod = isTRUE(genParams$useClusterMethod),
    refinedStructure = isTRUE(genParams$refinedStructure),
    maskWaterAndMountains = isTRUE(genParams$maskWaterAndMountainsFromLines),
    connectingBlockSize = connectingVal,
    siteSelection = siteSelStr,
    verboseDiagnostics = isTRUE(genParams$verboseDiagnostics),
    disturbFirstYear = isTRUE(genParams$disturbFirstYear),
    saveInitialDisturbances = isTRUE(genParams$saveInitialDisturbances),
    runClusteringInParallel = isTRUE(genParams$runClusteringInParallel),
    growthStepGenerating = as.numeric(genParams$growthStepGenerating %||% NA_real_),
    growthStepEnlargingPolys = as.numeric(genParams$growthStepEnlargingPolys %||% NA_real_),
    growthStepEnlargingLines = as.numeric(genParams$growthStepEnlargingLines %||% NA_real_),
    disturbanceRateRelatesToBufferedArea = isTRUE(genParams$disturbanceRateRelatesToBufferedArea)
  )
  runCsv <- cfg$run_records_csv %||% file.path(scratchRoot, "run_data.csv")
  if (file.exists(runCsv)) {
    old <- tryCatch(data.table::fread(runCsv), error = function(e) NULL)
    if (!is.null(old)) rec <- data.table::rbindlist(list(old, rec), use.names = TRUE, fill = TRUE)
  }
  data.table::fwrite(rec, runCsv)

  list(
    status = status,
    log = logRel,
    outputPath = outputPathRel,
    diagnostics = diag,
    message = if (is.na(errMsg)) "" else errMsg
  )
}

# null-coalescing helper
`%||%` <- function(a, b) if (is.null(a)) b else a

# --- CSV-driven execution utilities -----------------------------------------

parse_bool <- function(x, default = NA) {
  if (length(x) > 1) {
    return(vapply(x, parse_bool, logical(1), default = default))
  }
  if (length(x) == 0 || is.null(x) || is.na(x)) return(default)
  if (is.logical(x)) return(ifelse(is.na(x), default, x))
  val <- tolower(trimws(as.character(x)))
  if (!nzchar(val)) return(default)
  if (val %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (val %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  warning(sprintf("Unrecognized logical flag '%s'; using default (%s).", val, default), call. = FALSE)
  default
}

parse_numeric <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) return(NA_real_)
  val <- suppressWarnings(as.numeric(x))
  if (all(is.na(val))) return(NA_real_)
  val
}

parse_iterations <- function(x, default = 1L) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) return(as.integer(default))
  val <- suppressWarnings(as.integer(x))
  if (length(val) == 0 || is.na(val) || val < 1L) return(as.integer(default))
  as.integer(val)
}

generate_run_seeds <- function(count, base_seed = NA_integer_, scenario_id = NULL) {
  count <- as.integer(count)
  if (is.na(count) || count <= 0) return(integer())
  max_seed <- min(.Machine$integer.max - 1000L, 1000000000L)
  normalize_seed <- function(val) {
    if (is.null(val) || length(val) == 0) return(NA_integer_)
    val <- suppressWarnings(as.numeric(val[1]))
    if (is.na(val) || !is.finite(val)) return(NA_integer_)
    val <- floor(val)
    maxInt <- .Machine$integer.max - 1L
    if (maxInt <= 0L) maxInt <- 2147483647L
    # ensure positive modulo without overflow
    val <- ((val %% maxInt) + maxInt) %% maxInt
    as.integer(val)
  }

  old_seed <- tryCatch(get(".Random.seed", envir = .GlobalEnv), error = function(...) NULL)
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)

  baseSeedNorm <- normalize_seed(base_seed)
  if (!is.na(baseSeedNorm)) {
    set.seed(baseSeedNorm)
  } else {
    timeSeed <- normalize_seed(as.numeric(Sys.time()) * 1000)
    scenarioSeed <- if (!is.null(scenario_id) && nzchar(as.character(scenario_id))) {
      normalize_seed(sum(as.integer(charToRaw(as.character(scenario_id))), na.rm = TRUE))
    } else {
      NA_integer_
    }
    fallback <- normalize_seed(timeSeed)
    if (!is.na(scenarioSeed)) {
      if (is.na(fallback)) {
        fallback <- scenarioSeed
      } else {
        fallback <- normalize_seed(fallback + scenarioSeed)
      }
    }
    if (is.na(fallback)) fallback <- 42L
    set.seed(fallback)
  }

  replaceFlag <- count > max_seed
  as.integer(sample.int(max_seed, size = count, replace = replaceFlag))
}

parse_site_selection <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) return(NULL)
  val <- trimws(as.character(x))
  if (!nzchar(val)) return(NULL)
  lv <- tolower(val)
  if (lv %in% c("default")) return(NULL)
  if (lv %in% c("na", "null", "none", "skip", "character(0)", "empty")) return(NA_character_)
  parts <- strsplit(val, "[;,]")[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(character(0))
  parts
}

resolve_path <- function(pathCandidate) {
  val <- trimws(as.character(pathCandidate %||% ""))
  if (!nzchar(val)) return(NA_character_)
  candidates <- unique(c(
    val,
    path.expand(val),
    file.path(project_root, val),
    file.path(suite_validation_root, val),
    file.path(project_root, "data", val)
  ))
  for (cand in candidates) {
    if (!nzchar(cand)) next
    if (file.exists(cand)) {
      return(normalizePath(cand, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

load_params_file <- function(path, parent_env = parent.frame()) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(list(cfg = NULL, params = NULL))
  }
  env <- new.env(parent = parent_env)
  res <- tryCatch(
    source(path, local = env, keep.source = FALSE),
    error = function(e) {
      stop(sprintf("Failed to load params_file '%s': %s", path, conditionMessage(e)), call. = FALSE)
    }
  )
  val <- res$value
  if (is.null(val)) {
    overrides_obj <- if (exists("overrides", envir = env, inherits = FALSE)) get("overrides", env) else NULL
    if (is.list(overrides_obj) && length(overrides_obj)) {
      val <- overrides_obj
    } else {
      cfg_obj <- if (exists("cfg", envir = env, inherits = FALSE)) get("cfg", env) else NULL
      params_obj <- if (exists("params", envir = env, inherits = FALSE)) get("params", env) else NULL
      if (!is.null(cfg_obj) || !is.null(params_obj)) {
        val <- list(cfg = cfg_obj, params = params_obj)
      }
    }
  }
  if (is.null(val)) return(list(cfg = NULL, params = NULL))
  if (!is.list(val)) {
    stop(sprintf("params_file '%s' must evaluate to a list (received class: %s).",
                 path, paste(class(val), collapse = "/")), call. = FALSE)
  }
  cfg_out <- NULL
  params_out <- NULL
  if (!length(val)) return(list(cfg = NULL, params = NULL))
  val_names <- names(val)
  if (!is.null(val_names) && (("cfg" %in% val_names) || ("params" %in% val_names))) {
    if ("cfg" %in% val_names) cfg_out <- val$cfg
    if ("params" %in% val_names) params_out <- val$params
    extras <- setdiff(val_names, c("cfg", "params"))
    if (length(extras)) {
      params_out <- safe_merge(params_out, val[extras])
    }
  } else {
    params_out <- val
  }
  if (!is.null(cfg_out) && !is.list(cfg_out)) {
    stop(sprintf("cfg entry in params_file '%s' must be a list.", path), call. = FALSE)
  }
  if (!is.null(params_out) && !is.list(params_out)) {
    stop(sprintf("params entry in params_file '%s' must be a list.", path), call. = FALSE)
  }
  list(cfg = cfg_out, params = params_out)
}

resolve_study_area_path <- function(value) {
  val <- trimws(as.character(value %||% ""))
  if (!nzchar(val)) stop("study_area is required for CSV-driven runs.", call. = FALSE)
  candidates <- unique(c(
    val,
    path.expand(val),
    file.path(project_root, val),
    file.path(project_root, "data", "synthetic", "study_area", val),
    file.path(project_root, "data", "synthetic", "study_area", paste0(val, ".shp")),
    file.path(project_root, "data", "synthetic", "study_area", paste0(val, ".gpkg")),
    file.path(project_root, "data", "study_area", val),
    file.path(project_root, "data", "study_area", paste0(val, ".shp")),
    file.path(project_root, "data", "study_area", paste0(val, ".gpkg")),
    file.path(project_root, "data", val),
    file.path(project_root, "data", paste0(val, ".shp")),
    file.path(project_root, "data", paste0(val, ".gpkg")),
    file.path(project_root, "scratch", val)
  ))
  for (cand in candidates) {
    if (!nzchar(cand)) next
    if (file.exists(cand)) {
      return(normalizePath(cand, winslash = "/", mustWork = TRUE))
    }
  }
  stop(sprintf("Unable to resolve study area path for value '%s'.", value), call. = FALSE)
}

load_study_area_vect <- function(value) {
  path <- resolve_study_area_path(value)
  terra::vect(path)
}

scenario_cfg_from_row <- function(row) {
  scenarioId <- as.character(row$scenario_id %||% row$description %||% paste0("scenario_", row$row_id %||% ""))
  branch <- row$branch
  branch <- if (is.null(branch)) "" else trimws(as.character(branch))
  useEccc <- parse_bool(row$use_eccc, default = TRUE)
  totalRate <- parse_numeric(row$total_rate)
  seedVal <- parse_numeric(row$seed)

  cfg <- list(
    branch = if (nzchar(branch)) branch else NULL,
    scenario_name = scenarioId,
    study_area = load_study_area_vect(row$study_area),
    use_eccc = useEccc,
    seed = if (!is.na(seedVal)) as.integer(seedVal) else NULL
  )

  if (!isTRUE(useEccc) && !is.na(totalRate)) {
    cfg$total_rate <- totalRate
  }

  useWindCsv <- parse_bool(row$use_wind_data, default = NA)
  if (!is.na(useWindCsv)) cfg$use_wind_data <- useWindCsv

  startYearCsv <- parse_numeric(row$start_year)
  endYearCsv <- parse_numeric(row$end_year)
  if (!is.na(startYearCsv) || !is.na(endYearCsv)) {
    cfg$times <- list(
      start = if (!is.na(startYearCsv)) as.integer(startYearCsv) else 2011L,
      end = if (!is.na(endYearCsv)) as.integer(endYearCsv) else 2031L
    )
  }

  dRatePath <- resolve_path(row$disturbance_rate_table)
  if (!is.na(dRatePath)) {
    cfg$disturbance_rate <- data.table::fread(dRatePath)
  }

  params <- list(anthroDisturbance_Generator = list())
  params$anthroDisturbance_Generator$generatedDisturbanceAsRaster <-
    parse_bool(row$generated_disturbance_as_raster, default = FALSE)
  params$anthroDisturbance_Generator$useClusterMethod <-
    parse_bool(row$use_cluster_method, default = TRUE)
  params$anthroDisturbance_Generator$refinedStructure <-
    parse_bool(row$refined_structure, default = FALSE)
  params$anthroDisturbance_Generator$maskWaterAndMountainsFromLines <-
    parse_bool(row$mask_water_and_mountains, default = TRUE)

  connSize <- parse_numeric(row$connecting_block_size)
  if (!is.na(connSize)) {
    params$anthroDisturbance_Generator$connectingBlockSize <- connSize
  }

  siteSel <- parse_site_selection(row$site_selection)
  if (!is.null(siteSel)) {
    params$anthroDisturbance_Generator$siteSelectionAsDistributing <- siteSel
  }

  diagFlag <- parse_bool(row$diagnostics, default = FALSE)
  if (!is.na(diagFlag)) {
    params$anthroDisturbance_Generator$verboseDiagnostics <- diagFlag
  }

  disturbFirst <- parse_bool(row$disturb_first_year)
  if (!is.na(disturbFirst)) {
    params$anthroDisturbance_Generator$disturbFirstYear <- disturbFirst
  }

  saveInitial <- parse_bool(row$save_initial_disturbances)
  if (!is.na(saveInitial)) {
    params$anthroDisturbance_Generator$saveInitialDisturbances <- saveInitial
  }

  runClusterParallel <- parse_bool(row$run_clustering_in_parallel)
  if (!is.na(runClusterParallel)) {
    params$anthroDisturbance_Generator$runClusteringInParallel <- runClusterParallel
  }

  gsg <- parse_numeric(row$growth_step_generating)
  if (!is.na(gsg)) {
    params$anthroDisturbance_Generator$growthStepGenerating <- gsg
  }

  gsep <- parse_numeric(row$growth_step_enlarging_polys)
  if (!is.na(gsep)) {
    params$anthroDisturbance_Generator$growthStepEnlargingPolys <- gsep
  }

  gsel <- parse_numeric(row$growth_step_enlarging_lines)
  if (!is.na(gsel)) {
    params$anthroDisturbance_Generator$growthStepEnlargingLines <- gsel
  }

  drb <- parse_bool(row$disturbance_rate_relates_to_buffered_area)
  if (!is.na(drb)) {
    params$anthroDisturbance_Generator$disturbanceRateRelatesToBufferedArea <- drb
  }

  cfg$params <- params

  if ("params_file" %in% names(row)) {
    paramsFileRaw <- row$params_file
    if (!is.null(paramsFileRaw) && !all(is.na(paramsFileRaw))) {
      paramsFileRaw <- paramsFileRaw[!is.na(paramsFileRaw)][1]
    }
    pfStr <- trimws(as.character(paramsFileRaw %||% ""))
    if (length(pfStr) == 0 || is.na(pfStr)) {
      pfStr <- ""
    } else {
      pfStrLower <- tolower(pfStr)
      if (pfStrLower %in% c("", "na", "null", "none", "nil", "nan")) {
        pfStr <- ""
      }
    }
    if (nzchar(pfStr)) {
      paramsFilePath <- resolve_path(pfStr)
      if (is.na(paramsFilePath)) {
        stop(sprintf("Unable to resolve params_file '%s'.", pfStr), call. = FALSE)
      }
      overrides <- load_params_file(paramsFilePath, parent_env = environment())
      if (!is.null(overrides$cfg)) {
        cfg <- safe_merge(cfg, overrides$cfg)
      }
      if (!is.null(overrides$params)) {
        cfg$params <- safe_merge(cfg$params, overrides$params)
      }
    }
  }

  fireSpec <- parse_fire_generation_spec(row$fire_generation)
  if (!is.null(fireSpec)) {
    cfg$fire_generation <- fireSpec
    label <- if (!is.null(fireSpec$raw) && nzchar(fireSpec$raw)) fireSpec$raw else fireSpec$strategy
    cfg$metadata <- safe_merge(cfg$metadata %||% list(), list(fire_generation = label))
  }

  cfg
}

strip_helper_cols <- function(dt) {
  drop <- c("row_id", "active_flag", "status_chr")
  dt[, setdiff(names(dt), drop), with = FALSE]
}

ensure_testing_col_order <- function(dt) {
  desired <- c(
    "scenario_id", "status", "active", "last_run_date", "notes", "seed",
    "replicates",
    "description", "branch", "study_area", "use_eccc", "total_rate",
    "disturbance_rate_table", "generated_disturbance_as_raster",
    "use_cluster_method", "refined_structure", "mask_water_and_mountains",
    "connecting_block_size", "site_selection", "diagnostics",
    "disturb_first_year", "save_initial_disturbances",
    "run_clustering_in_parallel", "growth_step_generating",
    "growth_step_enlarging_polys", "growth_step_enlarging_lines",
    "disturbance_rate_relates_to_buffered_area", "use_wind_data",
    "start_year", "end_year", "fire_generation", "params_file",
    "log_path", "output_path",
    "last_updated"
  )
  present <- desired[desired %in% names(dt)]
  remaining <- setdiff(names(dt), desired)
  data.table::setcolorder(dt, c(present, remaining))
  dt
}

execute_scenarios_from_csv <- function(csv_path,
                                       scenario_ids = NULL,
                                       force = FALSE,
                                       dry_run = FALSE,
                                       mode = c("default", "respect", "all")) {
  mode <- match.arg(tolower(mode), c("default", "respect", "all"))
  if (!file.exists(csv_path)) {
    stop(sprintf("CSV file not found: %s", csv_path), call. = FALSE)
  }
  dt <- data.table::fread(csv_path, fill = TRUE)
  char_cols <- c("scenario_id", "description", "branch", "study_area",
                 "disturbance_rate_table", "site_selection", "fire_generation",
                 "log_path", "output_path", "status", "notes", "last_updated",
                 "last_run_date", "seed", "params_file")
  for (col in char_cols) {
    if (!col %in% names(dt)) {
      dt[, (col) := NA_character_]
    } else if (!is.character(dt[[col]])) {
      dt[, (col) := as.character(dt[[col]])]
    }
  }
  if (!"replicates" %in% names(dt)) dt[, replicates := NA_integer_]
  ensure_testing_col_order(dt)
  if (!"scenario_id" %in% names(dt)) {
    stop("CSV missing required 'scenario_id' column.", call. = FALSE)
  }
  if (!"active" %in% names(dt)) dt[, active := TRUE]
  if (!"last_updated" %in% names(dt)) dt[, last_updated := NA_character_]
  if (!"last_run_date" %in% names(dt)) dt[, last_run_date := NA_character_]
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
    runnable <- runnable[!(status_chr %in% c("success", "placeholder", "skip"))]
  }

  if (!nrow(runnable)) {
    message("No scenarios selected for execution.")
    if (!dry_run) {
      ensure_testing_col_order(dt)
      out <- strip_helper_cols(dt)
      data.table::fwrite(out, csv_path)
    }
    return(list(results = list(), table = strip_helper_cols(dt)))
  }

  selected_ids <- runnable$scenario_id
  message(sprintf("Selected scenarios: %s", paste(selected_ids, collapse = ", ")))
  if (dry_run) {
    message("Dry-run mode: no scenarios executed.")
    return(list(
      results = lapply(selected_ids, function(id) list(scenario_id = id, status = "DRY_RUN")),
      table = strip_helper_cols(dt)
    ))
  }

  results <- list()
  for (i in seq_len(nrow(runnable))) {
    row_info <- runnable[i]
    idx <- row_info$row_id
    rowList <- as.list(dt[idx])
    baseScenarioId <- as.character(rowList$scenario_id)
    iterCount <- parse_iterations(rowList$replicates, default = 1L)
    baseSeed <- parse_numeric(rowList$seed)
    iterSeeds <- if (iterCount > 1L) {
      generate_run_seeds(iterCount, base_seed = baseSeed, scenario_id = baseScenarioId)
    } else if (!is.na(baseSeed)) {
      as.integer(baseSeed)
    } else {
      NA_integer_
    }

    if (iterCount > 1L) {
      message(sprintf("Running scenario '%s' (row %d) with %d runs...", baseScenarioId, idx, iterCount))
    } else {
      message(sprintf("Running scenario '%s' (row %d)...", baseScenarioId, idx))
    }

    runSummaries <- vector("list", iterCount)
    runStatuses <- character(iterCount)
    runLogs <- character(iterCount)
    runOutputs <- character(iterCount)
    runMessages <- character(iterCount)

    for (iter in seq_len(iterCount)) {
      iterScenarioId <- if (iterCount > 1L) {
        sprintf("%s_run_%02d", baseScenarioId, iter)
      } else {
        baseScenarioId
      }
      if (iterCount > 1L) {
        message(sprintf("  Iteration %d/%d -> %s", iter, iterCount, iterScenarioId))
      }
      iterRow <- rowList
      iterRow$scenario_id <- iterScenarioId
      cfg <- scenario_cfg_from_row(iterRow)
      if (iterCount > 1L) {
        cfg$seed <- iterSeeds[iter]
      } else if (!is.na(iterSeeds)) {
        cfg$seed <- as.integer(iterSeeds)
      }
      res <- run_scenario(cfg)
      logRel <- relative_to_root(res$log)
      outRel <- relative_to_root(res$outputPath)
      runStatuses[iter] <- res$status %||% "FAIL"
      runLogs[iter] <- logRel
      runOutputs[iter] <- outRel
      msg <- res$message
      if (is.null(msg) || is.na(msg)) msg <- ""
      runMessages[iter] <- msg
      seedVal <- if (iterCount > 1L) iterSeeds[iter] else if (!is.na(iterSeeds)) as.integer(iterSeeds) else NA_integer_
      runSummaries[[iter]] <- list(
        iteration = iter,
        run_id = iterScenarioId,
        status = res$status,
        seed = seedVal,
        log_path = logRel,
        output_path = outRel,
        message = msg
      )
    }

    successMask <- runStatuses == "SUCCESS"
    overallStatus <- if (all(successMask)) "SUCCESS" else "FAIL"
    dt[idx, `:=`(
      status = overallStatus,
      log_path = collapse_paths(runLogs),
      output_path = collapse_paths(runOutputs),
      last_updated = timestamp_now(),
      last_run_date = format(Sys.time(), "%Y-%m-%d")
    )]

    if (identical(overallStatus, "SUCCESS")) {
      dt[idx, notes := ""]
    } else {
      msgs <- runMessages
      msgs[is.na(msgs)] <- ""
      detail <- paste(sprintf(
        "run_%02d:%s%s",
        seq_len(iterCount),
        runStatuses,
        ifelse(nzchar(msgs), paste0(" (", msgs, ")"), "")
      ), collapse = " | ")
      dt[idx, notes := detail]
    }

    results[[length(results) + 1]] <- list(
      scenario_id = baseScenarioId,
      status = overallStatus,
      runs = runSummaries,
      log_paths = runLogs,
      output_paths = runOutputs
    )
  }

  ensure_testing_col_order(dt)
  out <- strip_helper_cols(dt)
  data.table::fwrite(out, csv_path)
  list(results = results, table = out)
}

parse_cli_args <- function(args) {
  opts <- list(csv = suite_default_csv,
               scenario_ids = character(0),
               force = FALSE,
               dry_run = FALSE,
               show_help = FALSE,
               mode = "default")
  if (!length(args)) return(opts)
  for (arg in args) {
    if (identical(arg, "--help") || identical(arg, "-h")) {
      opts$show_help <- TRUE
    } else if (identical(arg, "--force")) {
      opts$force <- TRUE
    } else if (identical(arg, "--dry-run")) {
      opts$dry_run <- TRUE
    } else if (grepl("^--csv=", arg)) {
      opts$csv <- sub("^--csv=", "", arg)
    } else if (grepl("^--scenario=", arg)) {
      vals <- sub("^--scenario=", "", arg)
      vals <- unlist(strsplit(vals, "[,;]"))
      vals <- trimws(vals[nzchar(vals)])
      opts$scenario_ids <- unique(c(opts$scenario_ids, vals))
    } else if (grepl("^--mode=", arg, ignore.case = TRUE)) {
      val <- tolower(sub("^--mode=", "", arg, ignore.case = TRUE))
      opts$mode <- val
    } else {
      warning(sprintf("Ignoring unrecognized argument: %s", arg), call. = FALSE)
    }
  }
  opts
}

maybe_run_from_cli <- function() {
  if (!interactive() && sys.nframe() <= 1L) {
    cli_opts <- parse_cli_args(commandArgs(trailingOnly = TRUE))
    if (cli_opts$show_help) {
      entry_abs <- tryCatch({
        if (!nzchar(suite_entrypoint)) stop("no entrypoint")
        if (!grepl("^[A-Za-z]:|^/", suite_entrypoint)) {
          file.path(project_root, suite_entrypoint)
        } else {
          suite_entrypoint
        }
      }, error = function(...) file.path("validation", suite_id, paste0("run_", suite_id, "_suite.R")))
      entry_disp <- relative_to_root(entry_abs)
      if (is.na(entry_disp)) entry_disp <- entry_abs
      csv_disp <- relative_to_root(suite_default_csv)
      if (is.na(csv_disp)) csv_disp <- suite_default_csv
      cat(paste(
        sprintf("Usage: Rscript %s [--csv=PATH] [--scenario=id1,id2] [--force] [--dry-run]\n", entry_disp),
        sprintf("  --csv=PATH        Path to CSV file (default: %s)\n", csv_disp),
        "  --scenario=IDS    Comma-separated scenario_id list to run (defaults to all active pending)\n",
        "  --force           Run even if status already SUCCESS or SKIP\n",
        "  --mode=NAME       Mode selector: default|respect|all (default: default)\n",
        "  --dry-run         Show which scenarios would run without executing\n",
        sep = ""
      ))
      quit(save = "no", status = 0, runLast = FALSE)
    }
    csvPath <- normalizePath(cli_opts$csv, winslash = "/", mustWork = FALSE)
    res <- execute_scenarios_from_csv(
      csv_path = csvPath,
      scenario_ids = cli_opts$scenario_ids,
      force = cli_opts$force,
      dry_run = cli_opts$dry_run,
      mode = cli_opts$mode
    )
    if (!cli_opts$dry_run) {
      failCount <- sum(vapply(res$results, function(x) identical(x$status, "FAIL"), logical(1)))
      quit(save = "no", status = if (failCount > 0) 1 else 0, runLast = FALSE)
    }
    quit(save = "no", status = 0, runLast = FALSE)
  }
}

maybe_run_from_cli()
