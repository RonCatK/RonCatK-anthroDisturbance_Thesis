suppressPackageStartupMessages({
  requireNamespace("data.table")
  requireNamespace("future.apply")
  requireNamespace("future")
  requireNamespace("glue")
  requireNamespace("withr")
  requireNamespace("SpaDES.core")
  requireNamespace("digest")
  requireNamespace("msm")
  # UA: try to make all declared module packages available (close to production)
  suppressWarnings({
    for (pkg in c(
      # core utils used by modules
      "fasterize", "stars", "nngeo", "roads", "truncnorm", "spaths"
    )) {
      try(requireNamespace(pkg), silent = TRUE)
      try(library(pkg, character.only = TRUE), silent = TRUE)
    }
  })
  # Attach key packages used unqualified inside modules
  try(library(data.table), silent = TRUE)
  try(library(reproducible), silent = TRUE)
  try(library(SpaDES.core), silent = TRUE)
  try(library(SpaDES.tools), silent = TRUE)
  try(library(geodata), silent = TRUE)
  try(library(digest), silent = TRUE)
  try(library(msm), silent = TRUE)
  # Force foreach to sequential backend; avoid implicit parallel workers
  try({ if (requireNamespace('foreach', quietly = TRUE)) foreach::registerDoSEQ() }, silent = TRUE)
  try({ if (requireNamespace('doParallel', quietly = TRUE)) doParallel::stopImplicitCluster() }, silent = TRUE)
  # Minimize native thread parallelism in BLAS/GDAL
  Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
  try({
    if (requireNamespace('googledrive', quietly = TRUE)) {
      googledrive::drive_deauth()
    }
  }, silent = TRUE)
})

source(file.path("validation", "ua", "ua_utils.R"))
source(file.path("validation", "ua", "ua_scenarios.R"))
source(file.path("validation", "ua", "ua_metrics.R"))

print_usage <- function() {
  cat(paste(
    "Usage: Rscript validation/ua/run_ua_replicates.R [options]\n",
    "  --module-path PATH     Directory containing anthroDisturbance_Generator\n",
    "  --replicates N         Number of replicates per scenario (default 10)\n",
    "  --start-year YYYY      Simulation start year (default 2020)\n",
    "  --end-year YYYY        Simulation end year (default 2030)\n",
    "  --parallel {yes|no}    Use multisession parallelism (default yes)\n",
    "  --skip-scenarios REGEX Skip scenarios whose label matches REGEX\n",
    "  --dry-run              Show selected scenarios and exit\n",
    "  --help                 Show this message and exit\n",
    sep = "\n"
  ))
}

parse_cli_args <- function(args) {
  opts <- list(
    help = FALSE,
    module_path = NA_character_,
    replicates = 10L,
    start_year = 2020L,
    end_year = 2030L,
    parallel = "yes",
    skip_scenarios = NA_character_,
    dry_run = FALSE,
    extra = character(0)
  )
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("--help", "-h")) {
      opts$help <- TRUE
    } else if (grepl("^--module-path=", arg, ignore.case = TRUE)) {
      opts$module_path <- sub("^--module-path=", "", arg, ignore.case = TRUE)
    } else if (identical(arg, "--module-path")) {
      if (i == length(args)) stop("--module-path expects a value", call. = FALSE)
      i <- i + 1L
      opts$module_path <- args[[i]]
    } else if (grepl("^--replicates=", arg, ignore.case = TRUE)) {
      opts$replicates <- as.integer(sub("^--replicates=", "", arg, ignore.case = TRUE))
    } else if (identical(arg, "--replicates")) {
      if (i == length(args)) stop("--replicates expects a value", call. = FALSE)
      i <- i + 1L
      opts$replicates <- as.integer(args[[i]])
    } else if (grepl("^--start-year=", arg, ignore.case = TRUE)) {
      opts$start_year <- as.integer(sub("^--start-year=", "", arg, ignore.case = TRUE))
    } else if (identical(arg, "--start-year")) {
      if (i == length(args)) stop("--start-year expects a value", call. = FALSE)
      i <- i + 1L
      opts$start_year <- as.integer(args[[i]])
    } else if (grepl("^--end-year=", arg, ignore.case = TRUE)) {
      opts$end_year <- as.integer(sub("^--end-year=", "", arg, ignore.case = TRUE))
    } else if (identical(arg, "--end-year")) {
      if (i == length(args)) stop("--end-year expects a value", call. = FALSE)
      i <- i + 1L
      opts$end_year <- as.integer(args[[i]])
    } else if (grepl("^--parallel=", arg, ignore.case = TRUE)) {
      opts$parallel <- sub("^--parallel=", "", arg, ignore.case = TRUE)
    } else if (identical(arg, "--parallel")) {
      if (i == length(args)) stop("--parallel expects a value", call. = FALSE)
      i <- i + 1L
      opts$parallel <- args[[i]]
    } else if (grepl("^--skip-scenarios=", arg, ignore.case = TRUE)) {
      opts$skip_scenarios <- sub("^--skip-scenarios=", "", arg, ignore.case = TRUE)
    } else if (identical(arg, "--skip-scenarios")) {
      if (i == length(args)) stop("--skip-scenarios expects a value", call. = FALSE)
      i <- i + 1L
      opts$skip_scenarios <- args[[i]]
    } else if (grepl("^--dry-run$", arg, ignore.case = TRUE)) {
      opts$dry_run <- TRUE
    } else if (grepl("^--", arg)) {
      warning(sprintf("Ignoring unrecognised option: %s", arg), call. = FALSE)
    } else {
      opts$extra <- c(opts$extra, arg)
    }
    i <- i + 1L
  }
  opts
}

args_cli <- commandArgs(trailingOnly = TRUE)
cli_opts <- parse_cli_args(args_cli)
if (cli_opts$help) {
  print_usage()
  quit(save = "no", status = 0, runLast = FALSE)
}

R <- as.integer(cli_opts$replicates)
startYear <- as.integer(cli_opts$start_year)
endYear <- as.integer(cli_opts$end_year)
useParallel <- tolower(cli_opts$parallel) %in% c("yes", "y", "true", "1")
dryRun <- isTRUE(cli_opts$dry_run)

fireMaskDir <- Sys.getenv(
  "UA_FIRE_MASK_DIR",
  unset = file.path(getwd(), "data", "raw", "validation", "ua", "fire")
)
if (!nzchar(fireMaskDir)) {
  fireMaskDir <- file.path(getwd(), "data", "raw", "validation", "ua", "fire")
}
fireMaskDir <- normalizePath(fireMaskDir, winslash = "/", mustWork = FALSE)
if (!dir.exists(fireMaskDir)) {
  dir.create(fireMaskDir, recursive = TRUE, showWarnings = FALSE)
}

syntheticRoot <- Sys.getenv("VALIDATION_SYNTHETIC_ROOT", unset = file.path(getwd(), "data", "synthetic"))

# Load user-provided inputs (objects). Path is configurable via envvar UA_INPUTS.
# Accepted: an R script that assigns required objects in its environment,
# or an RDS file containing a named list with those objects.
inputs_env <- new.env(parent = baseenv())
inputs_path <- Sys.getenv("UA_INPUTS", unset = file.path(getwd(), "validation", "ua", "ua_inputs.R"))
if (file.exists(inputs_path) && grepl("\\.R(ds)?$", inputs_path, ignore.case = TRUE)) {
  if (grepl("\\.rds$", inputs_path, ignore.case = TRUE)) {
    lst <- readRDS(inputs_path)
    stopifnot(is.list(lst))
    list2env(lst, envir = inputs_env)
  } else {
    sys.source(inputs_path, envir = inputs_env)
  }
} else {
  stop("Inputs file not found: ", inputs_path)
}

# Defaults: if studyArea missing, try data/study_area/aoi_southwest_NWT.shp; if rasterToMatch missing, derive from studyArea
if (!exists("studyArea", envir = inputs_env, inherits = FALSE)) {
  studyDirs <- c(
    file.path(syntheticRoot, "study_area"),
    file.path(getwd(), "data", "study_area"),
    file.path(getwd(), "data")
  )
  shp <- NULL
  for (dirPath in studyDirs) {
    if (!dir.exists(dirPath)) next
    cand <- file.path(dirPath, "aoi_southwest_NWT.shp")
    if (file.exists(cand)) { shp <- cand; break }
    cand <- file.path(dirPath, "aoi_southwest_NWT.gpkg")
    if (file.exists(cand)) { shp <- cand; break }
    cand <- file.path(dirPath, "medium_aoi.shp")
    if (file.exists(cand)) { shp <- cand; break }
    cand <- file.path(dirPath, "medium_aoi.gpkg")
    if (file.exists(cand)) { shp <- cand; break }
  }
  if (!is.null(shp) && file.exists(shp)) {
    try({ assign("studyArea", terra::vect(shp), envir = inputs_env) }, silent = TRUE)
  }
}
if (!exists("rasterToMatch", envir = inputs_env, inherits = FALSE) && exists("studyArea", envir = inputs_env, inherits = FALSE)) {
  sa <- get("studyArea", envir = inputs_env, inherits = FALSE)
  if (inherits(sa, "SpatVector")) {
    try({ assign("rasterToMatch", create_local_rtm(sa, resolution = 250), envir = inputs_env) }, silent = TRUE)
  }
}

## placeholder; disturbanceDT fallback injected after modulePath is known

# Build scenarios and optionally skip some via regex
scnDT <- build_scenario_grid()
skipRegex <- cli_opts$skip_scenarios
if (!is.na(skipRegex) && nzchar(skipRegex)) {
  scnDT <- scnDT[!grepl(skipRegex, label)]
}

# Determine modulePath and base paths
default_candidates <- c(
  getwd(),
  file.path(getwd(), "modules"),
  file.path(getwd(), "modules_Testing"),
  dirname(getwd())
)
modulePath <- if (is.na(cli_opts$module_path) || !nzchar(cli_opts$module_path)) {
  find_module_path(default_candidates)
} else {
  find_module_path(c(cli_opts$module_path, default_candidates))
}

# Prefer pre-downloaded path as inputPath if present
syntheticRaw <- file.path(syntheticRoot, "raw")
inputCandidates <- unique(c(
  Sys.getenv("VALIDATION_INPUT_ROOT", unset = ""),
  Sys.getenv("PRE_DOWNLOADED_PATH", unset = ""),
  syntheticRaw,
  file.path(getwd(), "data", "raw"),
  file.path(getwd(), "data", "pre_downloaded")
))
inputPath <- NULL
for (cand in inputCandidates) {
  if (!nzchar(cand)) next
  expanded <- path.expand(cand)
  if (dir.exists(expanded)) { inputPath <- expanded; break }
}
if (is.null(inputPath)) inputPath <- getwd()
outputsRoot <- file.path(getwd(), "outputs", "validation", "ua")
dir.create(outputsRoot, recursive = TRUE, showWarnings = FALSE)
resultsDir <- file.path(outputsRoot, "results")
figuresDir <- file.path(outputsRoot, "figures")
dir.create(resultsDir, showWarnings = FALSE, recursive = TRUE)
dir.create(figuresDir, showWarnings = FALSE, recursive = TRUE)
outputPath <- outputsRoot

# Ensure disturbanceDT exists using packaged CSV (now that modulePath is known)
if (!exists("disturbanceDT", envir = inputs_env, inherits = FALSE)) {
  csv1 <- file.path(modulePath, "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
  if (file.exists(csv1)) {
    try({ assign("disturbanceDT", data.table::fread(csv1), envir = inputs_env) }, silent = TRUE)
  }
}

# Mirror system_harness: rewrite disturbanceDT URLs to local pre_downloaded files
rewrite_disturbanceDT_urls <- function(dt, preDown) {
  if (is.null(dt) || !NROW(dt) || !"URL" %in% names(dt)) return(dt)
  to_file_url <- function(path) {
    if (is.na(path) || !nzchar(path)) return(path)
    if (grepl("^file://", path, ignore.case = TRUE)) return(path)
    paste0("file://", path)
  }
  safe_loc <- function(fname) {
    if (!nzchar(fname) || is.na(fname)) return(NA_character_)
    p <- file.path(preDown, fname)
    if (file.exists(p)) return(to_file_url(p))
    # try any matching zip in preDown by base name prefix
    base <- sub("\\.shp$", "", fname, ignore.case = TRUE)
    cand <- list.files(preDown, pattern = paste0("^", gsub("\\.", "\\.", basename(base)), ".*\\.zip$"),
                       ignore.case = TRUE, full.names = TRUE)
    if (length(cand)) return(to_file_url(cand[[1]]))
    NA_character_
  }
  # If URL looks remote, try to map to local
  dt[, URL := {
    u <- URL
    # Leave file:// URLs untouched
    needs <- !grepl("^file://", u)
    if (any(needs)) {
      targ <- if ("fileName" %in% names(dt)) dt$fileName else basename(u)
      repl <- mapply(function(flag, nm, url) if (isTRUE(flag)) safe_loc(nm) else url,
                     needs, targ, u, SIMPLIFY = TRUE, USE.NAMES = FALSE)
      u[needs] <- ifelse(!is.na(repl), repl, u[needs])
    }
    u
  }]
  dt
}

# Apply URL rewrite if possible
preDownCandidates <- unique(c(
  Sys.getenv("PRE_DOWNLOADED_PATH", unset = ""),
  file.path(getwd(), "data", "pre_downloaded"),
  file.path(getwd(), "data", "raw")
))
preDownCandidates <- preDownCandidates[nzchar(preDownCandidates)]
preDown <- preDownCandidates[dir.exists(preDownCandidates)][1]
if (!is.na(preDown) && exists("disturbanceDT", envir = inputs_env, inherits = FALSE)) {
  dt <- get("disturbanceDT", envir = inputs_env)
  # Optional: exclude sectors via UA_EXCLUDE_SECTORS (comma-separated), e.g., 'roads,Energy'
  exSec <- trimws(strsplit(Sys.getenv("UA_EXCLUDE_SECTORS", unset = ""), ",")[[1]])
  exSec <- exSec[nzchar(exSec)]
  if (length(exSec)) {
    dt <- dt[!dataName %in% exSec]
  }
  dt2 <- try(rewrite_disturbanceDT_urls(dt, preDown), silent = TRUE)
  if (!inherits(dt2, "try-error")) {
    # Pre-stage archives into reproducible.destinationPath to avoid in-place overwrite issues
    destRoot <- getOption("reproducible.destinationPath", outputPath)
    # Copy any known files by fileName if present
    if ("fileName" %in% names(dt2)) {
      for (i in seq_len(nrow(dt2))) {
        fn <- dt2$fileName[i]
        if (!is.na(fn) && nzchar(fn)) {
          src <- file.path(preDown, fn)
          if (file.exists(src)) {
            dest <- file.path(destRoot, fn)
            dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
            if (!file.exists(dest)) {
              file.copy(src, dest, overwrite = FALSE)
            }
            # Point URL to the copy in destRoot using file:// scheme so download.file() accepts it
            destUrl <- paste0("file://", normalizePath(dest, winslash = "/", mustWork = FALSE))
            dt2$URL[i] <- destUrl
          }
        }
      }
    }
    assign("disturbanceDT", dt2, envir = inputs_env)
  }
}

# Ensure disturbanceList exists using packaged qs (fast path)
if (!exists("disturbanceList", envir = inputs_env, inherits = FALSE)) {
}

# If disturbanceList is a wrapped list of .qs paths, unwrap into SpatVector objects
if (exists("disturbanceList", envir = inputs_env, inherits = FALSE)) {
  dl <- get("disturbanceList", envir = inputs_env)
  unwrapped <- try(maybe_unwrap_disturbanceList(dl, file.path(modulePath, "anthroDisturbance_Generator", "data")), silent = TRUE)
  if (!inherits(unwrapped, "try-error")) assign("disturbanceList", unwrapped, envir = inputs_env)
}

# Optional fast mode for testing: force per-class disturbance interval to yearly
if (tolower(Sys.getenv("UA_FAST", unset = "0")) %in% c("1","true","yes","y")) {
  if (exists("disturbanceParameters", envir = inputs_env, inherits = FALSE)) {
    dp <- get("disturbanceParameters", envir = inputs_env)
    if (data.table::is.data.table(dp) || is.data.frame(dp)) {
      dp$disturbanceInterval <- 1L
      assign("disturbanceParameters", dp, envir = inputs_env)
    }
  }
}

# Ensure required objects exist in inputs for a scenario
.requirement_satisfied <- function(name, env) {
  if (exists(name, envir = env, inherits = FALSE)) return(TRUE)
  if (identical(name, "rstCurrentBurn")) {
    yrs <- seq(startYear, endYear)
    files <- file.path(fireMaskDir, sprintf("rstCurrentBurn_%04d.tif", yrs))
    return(all(file.exists(files)))
  }
  FALSE
}

.scenario_has_requirements <- function(requires, env) {
  if (!length(requires)) return(TRUE)
  all(vapply(requires, .requirement_satisfied, logical(1), env = env))
}

# Build objects list for simInit, including optional layers only when scenario requires them
.objects_for_scenario <- function(env, requires) {
  # Always include these if present
  baseNames <- c("studyArea", "rasterToMatch", "disturbanceList", "disturbanceParameters",
                 "disturbanceDT", "DisturbanceRate")
  # Optional layers; include only when explicitly required by scenario
  optMap <- c(rstCurrentBurn = "rstCurrentBurn", DEM = "DEM", featuresToAvoid = "featuresToAvoid")
  obj <- mget(intersect(baseNames, ls(env)), envir = env, ifnotfound = list(NULL))
  obj <- obj[!vapply(obj, is.null, logical(1))]
  for (nm in names(optMap)) {
    if (nm %in% requires && exists(optMap[[nm]], envir = env, inherits = FALSE)) {
      obj[[optMap[[nm]]]] <- get(optMap[[nm]], envir = env, inherits = FALSE)
    }
  }
  obj
}

# If DisturbanceRate is absent, create a simple default mapping from module params
if (!exists("DisturbanceRate", envir = inputs_env, inherits = FALSE)) {
  try({
    mp <- find_module_path(default_candidates)
    paramsFile <- file.path(mp, "anthroDisturbance_Generator", "data", "paramsGeneral.txt")
    if (file.exists(paramsFile)) {
      dp <- data.table::data.table(dget(paramsFile))
      keyCols <- c("dataName", "dataClass", "disturbanceType", "disturbanceOrigin")
      dp <- unique(dp[, ..keyCols])
      # Set a conservative default rate (percent of total area per year)
      dp[, disturbanceRate := 0.2]
      assign("DisturbanceRate", dp, envir = inputs_env)
    }
  }, silent = TRUE)
}

# Parallel plan
if (useParallel) {
  future::plan(future::multisession)
} else {
  future::plan(future::sequential)
}

# Cartesian of scenarios x replicates
taskDT <- data.table::CJ(scenario_id = scnDT$scenario_id, rep_id = seq_len(R))
taskDT <- merge(taskDT, scnDT[, .(scenario_id, label, psim, requires)], by = "scenario_id", sort = TRUE)

# Validate and drop scenarios missing prerequisites
validScenarios <- vapply(seq_len(nrow(scnDT)), function(i) .scenario_has_requirements(scnDT$requires[[i]], inputs_env), logical(1))
if (any(!validScenarios)) {
  dropped <- scnDT[!validScenarios]
  if (nrow(dropped)) {
    for (i in seq_len(nrow(dropped))) {
      msg <- glue::glue("Skipping scenario {dropped$scenario_id[i]} ({dropped$label[i]}) due to missing: {paste(dropped$requires[[i]], collapse=', ')}")
      message(msg)
    }
  }
  scnDT <- scnDT[validScenarios]
  taskDT <- taskDT[scenario_id %in% scnDT$scenario_id]
}

if (dryRun) {
  sel <- unique(taskDT$scenario_id)
  if (!length(sel)) {
    message("[UA] No scenarios remain after filtering prerequisites.")
  } else {
    message("[UA] Scenarios selected: ", paste(sel, collapse = ", "))
    message(sprintf("[UA] Replicates per scenario: %d", R))
  }
  quit(save = "no", status = 0, runLast = FALSE)
}

modules <- c("anthroDisturbance_DataPrep", "potentialResourcesNT_DataPrep", "anthroDisturbance_Generator")
if (tolower(Sys.getenv("UA_MINIMAL", unset = "0")) %in% c("1","true","yes","y")) {
  modules <- c("anthroDisturbance_Generator")
}
times <- list(start = startYear, end = endYear)
paths <- list(modulePath = modulePath, inputPath = inputPath, outputPath = outputPath)

# Avoid auto-install and loading of reqdPkgs by SpaDES; we attach what we need manually above.
options(spades.loadReqdPkgs = TRUE,
        spades.installPackageDeps = FALSE,
        Require.install = FALSE,
        Require.offlineMode = TRUE,
        # Prefer local pre-downloaded assets and avoid forced re-downloads
        reproducible.useCache = TRUE,
        reproducible.checkMD5sums = FALSE,
        reproducible.overwrite = TRUE)

# Hint reproducible to use local pre-downloaded datasets, if available. Write outputs elsewhere
if (!is.na(preDown) && dir.exists(preDown)) {
  options(reproducible.inputPaths = preDown)
}
localDest <- file.path(outputPath, "preprocessed_local")
dir.create(localDest, showWarnings = FALSE, recursive = TRUE)
options(reproducible.destinationPath = localDest)

# Run tasks
t0 <- Sys.time()

runner <- function(i) {
  row <- taskDT[i]
  scn_id <- as.integer(row$scenario_id)
  rep_id <- as.integer(row$rep_id)
  lbl <- as.character(row$label)

  # Params: module defaults from scenario + CRN seeds + replicate-specific runName suffix
  p <- row$psim[[1]]
  seeds <- seed_list_for_rep(rep_id)
  p$.seed <- seeds[["anthroDisturbance_Generator"]]
  p$runName <- glue::glue("{p$runName}_rep{rep_id}")
  # Ensure module-internal clustering does not use parallel workers
  p$runClusteringInParallel <- FALSE
  # Disable blocking to avoid heavy connecting loops
  p$connectingBlockSize <- NULL
  # If DisturbanceRate is provided via inputs, avoid setting totalDisturbanceRate
  # Avoid remote fetch: point ECCC URLs to NA to force local targetFile lookup
  p$urlNEW <- NA_character_
  p$urlOLD <- NA_character_
  p$archiveNEW <- NA_character_
  p$archiveOLD <- NA_character_
  # potentialResourcesNT_DataPrep needs whatToCombine as a data.table; provide explicit default
  if ("potentialResourcesNT_DataPrep" %in% modules) {
    whatToCombine <- data.table::data.table(
      datasetName = c("oilGas", "oilGas", "mining", "mining"),
      dataClasses = c("potentialOilGas", "potentialOilGas", "potentialMining", "potentialMining"),
      toDifferentiate = c(NA, "C2H4_BCR6_NT1", "CLAIM_STAT", "PERMIT_STA"),
      activeProcess = c(NA, NA, "CLAIM_STAT", "PERMIT_STA")
    )
    paramsList <- list(
      potentialResourcesNT_DataPrep = list(whatToCombine = whatToCombine),
      anthroDisturbance_Generator = p
    )
  } else {
    paramsList <- list(anthroDisturbance_Generator = p)
  }

  # Objects for this scenario
  objs <- .objects_for_scenario(inputs_env, row$requires[[1]])
  message(sprintf("[UA] Objects passed: %s", paste(names(objs), collapse = ", ")))
  if (!is.null(objs$DisturbanceRate)) {
    message(sprintf("[UA] DisturbanceRate rows: %s", NROW(objs$DisturbanceRate)))
  }

  # simInit + spades
  sim <- SpaDES.core::simInit(times = times, params = paramsList, modules = modules,
                              paths = paths, objects = objs, loadPkgs = FALSE)
  sim <- SpaDES.core::spades(sim)

  # Metrics
  m <- extract_metrics(sim, scenario_id = scn_id, rep_id = rep_id)
  list(
    metrics = m,
    index = data.table::data.table(
      scenario_id = scn_id,
      label = lbl,
      rep_id = rep_id,
      seed_calculatingSize = as.integer(p$.seed[["calculatingSize"]]),
      seed_calculatingRate = as.integer(p$.seed[["calculatingRate"]]),
      seed_generatingDisturbances = as.integer(p$.seed[["generatingDisturbances"]]),
      seed_updatingDisturbanceList = as.integer(p$.seed[["updatingDisturbanceList"]])
    )
  )
}

resList <- future.apply::future_lapply(seq_len(nrow(taskDT)), runner, future.seed = TRUE)
t1 <- Sys.time()

# Bind results
metrics_raw <- data.table::rbindlist(lapply(resList, `[[`, "metrics"), use.names = TRUE, fill = TRUE)
idx <- data.table::rbindlist(lapply(resList, `[[`, "index"), use.names = TRUE, fill = TRUE)

# Join labels only if we have rows
if (nrow(metrics_raw)) {
  metrics_raw <- merge(metrics_raw, scnDT[, .(scenario_id, label)], by = "scenario_id", all.x = TRUE)
}

# Write raw metrics (can be empty)
data.table::fwrite(metrics_raw, file = file.path(resultsDir, "metrics_raw.csv"))

# Summary: mean ± 95% CI by scenario_id, label, year, metric, sector
if (nrow(metrics_raw)) {
  sumDT <- metrics_raw[, .(
    n = .N,
    mean = mean(value, na.rm = TRUE),
    sd = stats::sd(value, na.rm = TRUE)
  ), by = .(scenario_id, label, year, metric, sector)]
  sumDT[, `:=`(se = sd / sqrt(pmax(n, 1)), lwr = mean - 1.96 * se, upr = mean + 1.96 * se)]
  data.table::fwrite(sumDT, file = file.path(resultsDir, "metrics_summary.csv"))
} else {
  data.table::fwrite(data.table::data.table(), file = file.path(resultsDir, "metrics_summary.csv"))
}

# Replicate index for reproducibility
data.table::fwrite(idx, file = file.path(resultsDir, "replicate_index.csv"))

# Minimal acceptance checks (guarded)
if (Sys.getenv("UA_RUN_ACCEPTANCE", unset = "0") == "1") {
  stopifnot(file.exists(file.path(resultsDir, "metrics_raw.csv")))
  stopifnot(file.exists(file.path(resultsDir, "metrics_summary.csv")))
  stopifnot(file.exists(file.path(resultsDir, "replicate_index.csv")))
}

invisible(list(elapsed = difftime(t1, t0, units = "secs")))
