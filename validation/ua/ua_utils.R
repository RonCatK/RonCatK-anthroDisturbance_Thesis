## Utility helpers for UA scaffold
suppressPackageStartupMessages({
  requireNamespace("data.table")
  requireNamespace("terra")
  requireNamespace("sf")
})

# find_module_path: return a directory that contains the anthroDisturbance_Generator module
find_module_path <- function(candidates) {
  stopifnot(is.character(candidates), length(candidates) >= 1)
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  # Accept either the parent directory that contains the module subfolder,
  # or a direct path to the module folder (in which case return its parent).
  for (p in candidates) {
    # Case 1: p is a parent directory containing the module subfolder
    if (dir.exists(file.path(p, "anthroDisturbance_Generator"))) return(p)
    # Case 2: p itself is the module folder; return its parent
    if (basename(p) == "anthroDisturbance_Generator" && dir.exists(p)) return(dirname(p))
  }
  stop("Could not find a module path containing 'anthroDisturbance_Generator'.\n",
       "Checked: ", paste(candidates, collapse = ", "))
}

# seed_list_for_rep: ensure CRNs across scenarios by using the same rep_id
seed_list_for_rep <- function(rep_id, base_offsets = c(size = 1L, rate = 2L, gen = 3L, upd = 4L)) {
  rep_id <- as.integer(rep_id)
  base_offsets <- setNames(as.integer(base_offsets), names(base_offsets))
  list(anthroDisturbance_Generator = list(
    calculatingSize = rep_id * 1000L + base_offsets[["size"]],
    calculatingRate = rep_id * 1000L + base_offsets[["rate"]],
    generatingDisturbances = rep_id * 1000L + base_offsets[["gen"]],
    updatingDisturbanceList = rep_id * 1000L + base_offsets[["upd"]]
  ))
}

# year_key: helper to map tick/year to module key
year_key <- function(t) paste0("Year", t)

# bind_safely: rbind data.tables with union of columns
bind_safely <- function(...) {
  dts <- list(...)
  dts <- Filter(function(x) data.table::is.data.table(x) && nrow(x) >= 0, dts)
  if (length(dts) == 0) return(data.table::data.table())
  allCols <- unique(unlist(lapply(dts, names), use.names = FALSE))
  dts <- lapply(dts, function(dt) {
    miss <- setdiff(allCols, names(dt))
    if (length(miss)) dt[, (miss) := NA]
    data.table::setcolorder(dt, allCols)
    dt
  })
  data.table::rbindlist(dts, use.names = TRUE, fill = TRUE)
}

# Internal: test if SpatVector is lines or polygons
.sv_geom_kind <- function(x) {
  if (!inherits(x, "SpatVector")) return(NA_character_)
  sfx <- try(suppressWarnings(sf::st_as_sf(x)), silent = TRUE)
  if (inherits(sfx, "try-error")) return(NA_character_)
  gtypes <- unique(as.character(sf::st_geometry_type(sfx, by_geometry = TRUE)))
  if (all(grepl("LINE", gtypes))) return("lines")
  if (any(grepl("POLYGON", gtypes))) return("polygons")
  return("other")
}

# geom_area_km2: area of polygons/raster in km^2; NA for lines
geom_area_km2 <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (inherits(x, "SpatRaster")) {
    # Sum cell areas where x is not NA; terra::cellSize respects projection
    cs <- terra::cellSize(x, unit = "km")
    m <- terra::mask(cs, x) # keep only non-NA cells of x
    vals <- terra::values(m, mat = FALSE)
    return(sum(vals, na.rm = TRUE))
  }
  if (inherits(x, "SpatVector")) {
    kind <- .sv_geom_kind(x)
    if (identical(kind, "lines")) return(NA_real_)
    if (identical(kind, "polygons")) {
      a <- try(suppressWarnings(terra::expanse(x, unit = "km")), silent = TRUE)
      if (inherits(a, "try-error")) return(NA_real_)
      return(sum(a, na.rm = TRUE))
    }
  }
  NA_real_
}

# Optional: length in km for SpatVector lines
geom_length_km <- function(x) {
  if (!inherits(x, "SpatVector")) return(NA_real_)
  kind <- .sv_geom_kind(x)
  if (!identical(kind, "lines")) return(NA_real_)
  sfx <- suppressWarnings(sf::st_as_sf(x))
  # st_length uses CRS units; convert to numeric meters and then km
  sum(as.numeric(sf::st_length(sfx)), na.rm = TRUE) / 1000
}

# create_local_rtm: simple rasterToMatch derived from studyArea
create_local_rtm <- function(studyArea, resolution = 250) {
  stopifnot(inherits(studyArea, "SpatVector"))
  sa <- terra::vect(studyArea)
  rtm <- terra::rast(extent = terra::ext(sa), resolution = resolution, crs = terra::crs(sa))
  rtm[] <- 0
  terra::mask(rtm, sa)
}
