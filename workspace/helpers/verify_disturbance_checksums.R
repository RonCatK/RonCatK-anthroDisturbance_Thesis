#!/usr/bin/env Rscript

## Script to verify that the files listed in a synthetic DisturbanceDT table
## still match the recorded xxhash64 checksums under the same folder.

invisible(suppressPackageStartupMessages({
  requireNamespace("data.table", quietly = TRUE)
}))

`%||%` <- function(a, b) {
  if (is.null(a)) {
    return(b)
  }
  if (is.character(a) && !nzchar(a[1])) {
    return(b)
  }
  if (is.logical(a) && length(a) == 1 && is.na(a)) {
    return(b)
  }
  a
}

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[[1]] %||% "data/synthetic/rates/synthetic_inputs",
  winslash = "/", mustWork = FALSE
)
if (!dir.exists(root)) {
  stop("Root folder not found: ", root, call. = FALSE)
}

dist_candidates <- file.path(root, c(
  "disturbanceDT.csv", "DisturbanceDT.csv",
  "disturbancesDT.csv", "DisturbancesDT.csv"
))
dist_path <- dist_candidates[file.exists(dist_candidates)][1]
if (is.na(dist_path)) {
  stop("No DisturbanceDT file was found under ", root, call. = FALSE)
}

disturbance_dt <- data.table::fread(dist_path)
required_cols <- c("fileName", "URL")
missing_cols <- setdiff(required_cols, names(disturbance_dt))
if (length(missing_cols)) {
  stop("DisturbanceDT missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

component_exts <- c("cpg", "dbf", "prj", "shp", "shx")
component_bases <- unique(
  na.omit(
    vapply(disturbance_dt$fileName, tools::file_path_sans_ext, character(1), USE.NAMES = FALSE)
  )
)
component_candidates <- list.files(
  root,
  pattern = paste0("\\.(", paste(component_exts, collapse = "|"), ")$"),
  recursive = TRUE, full.names = TRUE
)
component_dt <- data.table::data.table(
  file = basename(component_candidates),
  path = normalizePath(component_candidates, winslash = "/", mustWork = FALSE),
  dir = normalizePath(dirname(component_candidates), winslash = "/", mustWork = FALSE)
)

select_component <- function(base) {
  files <- sprintf("%s.%s", base, component_exts)
  found <- list()
  for (fname in files) {
    matches <- component_dt[file == fname]
    if (nrow(matches)) {
      root_match <- matches[dir == root]
      picked <- if (nrow(root_match)) root_match$path[[1]] else matches$path[[1]]
      found[[length(found) + 1]] <- list(file = fname, path = picked)
    }
  }
  if (!length(found)) {
    return(NULL)
  }
  data.table::rbindlist(found)
}

component_candidates_list <- lapply(component_bases, select_component)
component_candidates_list <- component_candidates_list[!vapply(component_candidates_list, is.null, logical(1))]
if (length(component_candidates_list)) {
  component_rows <- data.table::rbindlist(component_candidates_list, use.names = TRUE, fill = TRUE)
  component_rows <- unique(component_rows, by = c("file", "path"))
} else {
  component_rows <- data.table::data.table(file = character(), path = character())
}

normalize_url <- function(url) {
  if (is.na(url) || !nzchar(url)) {
    return(NA_character_)
  }
  trimmed <- sub("^file://", "", url, ignore.case = TRUE)
  trimmed <- sub("^file:/", "", trimmed, ignore.case = TRUE)
  trimmed <- sub("^file:", "", trimmed, ignore.case = TRUE)
  if (!nzchar(trimmed)) {
    return(NA_character_)
  }
  trimmed <- sub("^//+", "/", trimmed)
  if (file.exists(trimmed)) {
    return(normalizePath(trimmed, winslash = "/", mustWork = FALSE))
  }
  candidate <- file.path(root, trimmed)
  if (file.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
  }
  candidate <- file.path(root, basename(trimmed))
  if (file.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
  }
  NA_character_
}

url_paths <- unique(na.omit(vapply(disturbance_dt$URL, normalize_url, character(1), USE.NAMES = FALSE)))
url_rows <- data.table::data.table(
  file = basename(url_paths),
  path = url_paths
)
url_rows <- unique(url_rows, by = c("file", "path"))

files_dt <- data.table::data.table(file = character(), path = character())
if (nrow(component_rows)) files_dt <- rbind(files_dt, component_rows, use.names = TRUE)
if (nrow(url_rows)) files_dt <- rbind(files_dt, url_rows, use.names = TRUE)
files_dt <- unique(files_dt, by = c("file", "path"))

if (is.null(files_dt) || !nrow(files_dt)) {
  stop("No candidate files were discovered for checksum verification.", call. = FALSE)
}

checksum_path <- file.path(root, "CHECKSUMS.txt")
if (!file.exists(checksum_path)) {
  stop("CHECKSUMS.txt not found under ", root, call. = FALSE)
}
checks <- data.table::fread(checksum_path)
if (!all(c("file", "checksum", "filesize", "algorithm") %in% names(checks))) {
  stop("CHECKSUMS.txt must define file, checksum, filesize, algorithm columns.", call. = FALSE)
}
checks[, entry_order := seq_len(.N)]

xxhsum_bin <- Sys.which("xxhsum")
if (!nzchar(xxhsum_bin)) {
  stop("xxhsum command not available on PATH.", call. = FALSE)
}

compute_hash <- function(path) {
  out <- tryCatch(
    system2(xxhsum_bin, c("-H1", path), stdout = TRUE, stderr = TRUE),
    error = function(e) stop("xxhsum failed for ", path, ": ", conditionMessage(e), call. = FALSE)
  )
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("xxhsum returned non-zero status for ", path, " (code ", status, ").", call. = FALSE)
  }
  if (!length(out)) stop("xxhsum produced no output for ", path, call. = FALSE)
  checksum <- strsplit(out[[1]], "\\s+")[[1]][[1]]
  size <- file.info(path)$size
  if (is.na(size)) stop("Unable to stat ", path, call. = FALSE)
  list(checksum = checksum, filesize = as.integer(size))
}

updates <- list()
for (row_idx in seq_len(nrow(files_dt))) {
  file_name <- files_dt$file[[row_idx]]
  file_path <- files_dt$path[[row_idx]]
  if (!file.exists(file_path)) {
    warning("Candidate missing; skipping ", file_path, call. = FALSE)
    next
  }
  stats <- compute_hash(file_path)
  matches <- which(checks$file == file_name)
  if (length(matches)) {
    idx <- matches[[1]]
    current <- checks[idx]
    if (current$checksum != stats$checksum || current$filesize != stats$filesize) {
      checks[idx, `:=`(
        checksum = stats$checksum,
        filesize = stats$filesize,
        algorithm = "xxhash64"
      )]
      updates[[length(updates) + 1]] <- list(
        file = file_name,
        path = file_path,
        old_checksum = current$checksum,
        new_checksum = stats$checksum,
        old_size = current$filesize,
        new_size = stats$filesize
      )
    }
  } else {
    new_order <- max(checks$entry_order, na.rm = TRUE) + 1L
    checks <- rbind(checks, data.table::data.table(
      file = file_name,
      checksum = stats$checksum,
      filesize = stats$filesize,
      algorithm = "xxhash64",
      entry_order = new_order
    ), use.names = TRUE, fill = TRUE)
    updates[[length(updates) + 1]] <- list(
      file = file_name,
      path = file_path,
      old_checksum = NA_character_,
      new_checksum = stats$checksum,
      old_size = NA_integer_,
      new_size = stats$filesize
    )
  }
}

if (!length(updates)) {
  message("All ", nrow(files_dt), " candidates already match CHECKSUMS.txt.")
  quit(save = "no", status = 0)
}

checks <- checks[order(entry_order)]
lines <- c("\"file\" \"checksum\" \"filesize\" \"algorithm\"")
for (i in seq_len(nrow(checks))) {
  lines <- c(lines, sprintf(
    "\"%s\" \"%s\" \"%d\" \"%s\"",
    checks$file[[i]],
    checks$checksum[[i]],
    checks$filesize[[i]],
    checks$algorithm[[i]]
  ))
}
tmpfile <- tempfile("checksums", tmpdir = dirname(checksum_path))
writeLines(lines, tmpfile)
file.rename(tmpfile, checksum_path)

message("Recalculated ", length(updates), " checksum(s) under ", checksum_path, ".")
for (update in updates) {
  change <- sprintf(
    "  %s: %s -> %s (size %s -> %s)",
    update$file,
    update$old_checksum %||% "<missing>",
    update$new_checksum,
    ifelse(is.na(update$old_size), "?", format(update$old_size, scientific = FALSE)),
    update$new_size
  )
  message(change)
}
