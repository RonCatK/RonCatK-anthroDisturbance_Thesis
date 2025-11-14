suppressPackageStartupMessages({
  requireNamespace("data.table")
  requireNamespace("terra")
})

to_file_url <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(path)
  if (grepl("^file://", path, ignore.case = TRUE)) return(path)
  paste0("file://", normalizePath(path, winslash = "/", mustWork = FALSE))
}

parse_flag <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  if (is.logical(value)) return(ifelse(is.na(value), default, value))
  val <- trimws(tolower(as.character(value)[1]))
  if (!nzchar(val)) return(default)
  if (val %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (val %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  default
}

# rewrite_disturbance_dt_urls: convert remote URLs in disturbanceDT to local file:// references
rewrite_disturbance_dt_urls <- function(dt, preDown) {
  if (is.null(dt) || !NROW(dt) || !"URL" %in% names(dt)) return(dt)
  if (is.null(preDown) || !nzchar(preDown) || !dir.exists(preDown)) return(dt)
  preDown <- normalizePath(preDown, winslash = "/", mustWork = TRUE)

  safe_loc <- function(fname) {
    if (!nzchar(fname) || is.na(fname)) return(NA_character_)
    primary <- normalizePath(file.path(preDown, fname), winslash = "/", mustWork = FALSE)
    if (file.exists(primary)) {
      if (grepl("\\.shp$", fname, ignore.case = TRUE)) {
        base <- sub("\\.shp$", "", fname, ignore.case = TRUE)
        zipCand <- normalizePath(file.path(preDown, paste0(base, ".zip")), winslash = "/", mustWork = FALSE)
        if (file.exists(zipCand)) return(to_file_url(zipCand))
        zips <- list.files(
          preDown,
          pattern = paste0("^", gsub("\\.", "\\\\.", basename(base)), ".*\\.zip$"),
          ignore.case = TRUE,
          full.names = TRUE
        )
        if (length(zips)) return(to_file_url(zips[[1]]))
        prefix <- sub("_[^_]*$", "", basename(base))
        if (nzchar(prefix)) {
          zips2 <- list.files(
            preDown,
            pattern = paste0("^", gsub("\\.", "\\\\.", prefix), ".*\\.zip$"),
            ignore.case = TRUE,
            full.names = TRUE
          )
          if (length(zips2)) return(to_file_url(zips2[[1]]))
        }
      }
      return(to_file_url(primary))
    }
    if (grepl("\\.shp$", fname, ignore.case = TRUE)) {
      zipCand <- normalizePath(file.path(preDown, sub("\\.shp$", ".zip", fname, ignore.case = TRUE)),
                               winslash = "/", mustWork = FALSE)
      if (file.exists(zipCand)) return(to_file_url(zipCand))
    }
    if (grepl("\\.zip$", fname, ignore.case = TRUE)) {
      base <- sub("\\.zip$", "", fname, ignore.case = TRUE)
      shpCand <- normalizePath(file.path(preDown, paste0(base, ".shp")), winslash = "/", mustWork = FALSE)
      if (file.exists(shpCand)) return(to_file_url(shpCand))
      gdbCand <- normalizePath(file.path(preDown, base), winslash = "/", mustWork = FALSE)
      if (dir.exists(gdbCand)) return(gdbCand)
    }
    if (grepl("\\.gdb$", fname, ignore.case = TRUE)) {
      zipGDB <- normalizePath(file.path(preDown, paste0(basename(fname), ".zip")), winslash = "/", mustWork = FALSE)
      if (file.exists(zipGDB)) return(to_file_url(zipGDB))
    }
    NA_character_
  }

  dt[, URL := {
    u <- URL
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

prepare_disturbance_dt <- function(module_path,
                                   pre_downloaded_path = NULL,
                                   use_wind_data = TRUE,
                                   drop_wind_layers = TRUE,
                                   fast_mode = Sys.getenv("DIAGNOSE_FAST", unset = "1"),
                                   enable_all_datasets = Sys.getenv("ENABLE_ALL_DATASETS", unset = "0"),
                                   extra_exclude = character(),
                                   output_csv = NULL) {
  csv_path <- file.path(module_path, "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
  if (!file.exists(csv_path)) return(NULL)

  dt <- data.table::fread(csv_path)

  preDown <- pre_downloaded_path
  if (!is.null(preDown) && nzchar(preDown) && dir.exists(preDown)) {
    dt <- rewrite_disturbance_dt_urls(dt, preDown)
    nrnIdx <- grepl("^NRN_NT_13_0_ROADSEG\\.shp$", dt$fileName, ignore.case = TRUE)
    if (any(nrnIdx)) {
      nrnZip <- list.files(preDown, pattern = "^NRN_NT_13_0_.*\\.zip$", ignore.case = TRUE, full.names = TRUE)
      if (length(nrnZip)) {
        dt$URL[nrnIdx] <- to_file_url(nrnZip[[1]])
      }
    }
  }

  dt <- dt[!(tolower(dataName) == "roads" & grepl("NRN_NT_13_0_ROADSEG\\.shp$", fileName, ignore.case = TRUE))]

  if (!isTRUE(use_wind_data)) {
    dt <- dt[dataName != "Energy"]
  } else if (isTRUE(drop_wind_layers)) {
    dt <- dt[!(dataName == "Energy" & dataClass %in% c("windTurbines", "potentialWindTurbines"))]
  }

  if (length(extra_exclude)) {
    dt <- dt[!dataName %in% extra_exclude]
  }

  if (anyNA(dt$URL)) {
    missing <- unique(dt[is.na(URL), .(dataName, dataClass, fileName)])
    msg <- paste(capture.output(print(missing)), collapse = " | ")
    warning(paste0("Some disturbanceDT rows could not be resolved to local files; they will be dropped: ", msg),
            immediate. = TRUE)
    dt <- dt[!is.na(URL)]
  }

  fast_flag <- parse_flag(fast_mode, default = TRUE)
  enable_all_flag <- parse_flag(enable_all_datasets, default = FALSE)
  if (!enable_all_flag && fast_flag) {
    dt <- dt[!dataName %in% c("Energy", "roads", "forestry", "mining")]
  }

  if (!is.null(output_csv) && nzchar(output_csv)) {
    dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(dt, output_csv)
  }

  dt
}

create_local_rtm <- function(studyArea, resolution = 250) {
  if (!inherits(studyArea, "SpatVector")) {
    sa <- terra::vect(studyArea)
  } else {
    sa <- studyArea
  }
  stopifnot(inherits(sa, "SpatVector"))
  rtm <- terra::rast(extent = terra::ext(sa), resolution = resolution, crs = terra::crs(sa))
  rtm[] <- 0
  terra::mask(rtm, sa)
}
