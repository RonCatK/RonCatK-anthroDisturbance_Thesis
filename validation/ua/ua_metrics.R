suppressPackageStartupMessages({
  requireNamespace("data.table")
  requireNamespace("terra")
  requireNamespace("sf")
})

source(file.path("validation", "ua", "ua_utils.R"))

# Dissolve all features into a single multipart geometry (polygons)
.dissolve_all <- function(x) {
  if (is.null(x) || !inherits(x, "SpatVector")) return(x)
  if (nrow(x) == 0) return(x)
  x$.__merge__ <- 1L
  terra::dissolve(x, by = "__merge__")
}

# Determine if a SpatVector is polygonal or linear
.sv_kind <- function(x) {
  if (!inherits(x, "SpatVector")) return(NA_character_)
  sfx <- try(suppressWarnings(sf::st_as_sf(x)), silent = TRUE)
  if (inherits(sfx, "try-error")) return(NA_character_)
  gtypes <- unique(as.character(sf::st_geometry_type(sfx, by_geometry = TRUE)))
  if (all(grepl("LINE", gtypes))) return("lines")
  if (any(grepl("POLYGON", gtypes))) return("polygons")
  return("other")
}

# extract_metrics: read per-year additions and compute per-year totals and current totals
extract_metrics <- function(sim, scenario_id, rep_id) {
  out <- list()

  # Identify years available from currentDisturbanceLayer
  cdl <- sim$currentDisturbanceLayer
  if (is.null(cdl) || !is.list(cdl)) return(data.table::data.table())
  yearNames <- names(cdl)
  yearNames <- yearNames[grepl("^Year\\d+", yearNames)]
  if (length(yearNames) == 0) return(data.table::data.table())
  years <- sort(as.integer(sub("^Year", "", yearNames)))

  # Accumulators for current totals per sector
  accumPoly <- list()   # cumulative polygons per sector (SpatVector)
  accumRast <- list()   # cumulative raster mask per sector (SpatRaster)
  accumLen  <- list()   # cumulative length (km) for line sectors

  for (yy in years) {
    ykey <- year_key(yy)
    ylist <- cdl[[ykey]]
    if (is.null(ylist)) next
    if (!is.list(ylist)) ylist <- list(unknown = ylist)

    # Per-sector yearly additions (areas)
    per_sector_rows <- lapply(names(ylist), function(sector) {
      obj <- ylist[[sector]]

      # Update accumulators for current totals by sector
      if (inherits(obj, "SpatVector")) {
        kind <- .sv_kind(obj)
        if (identical(kind, "polygons")) {
          if (is.null(accumPoly[[sector]])) accumPoly[[sector]] <- obj else {
            # rbind + dissolve to approximate union of cumulative polygons
            accumPoly[[sector]] <- .dissolve_all(rbind(accumPoly[[sector]], obj))
          }
        } else if (identical(kind, "lines")) {
          addLen <- geom_length_km(obj)
          accumLen[[sector]] <- sum(c(accumLen[[sector]], addLen), na.rm = TRUE)
        }
      } else if (inherits(obj, "SpatRaster")) {
        if (is.null(accumRast[[sector]])) {
          # Use binary mask (1 for disturbed cells)
          accumRast[[sector]] <- terra::ifel(!is.na(obj), 1, NA)
        } else {
          # add new disturbed cells without double counting
          accumRast[[sector]] <- terra::ifel(!is.na(accumRast[[sector]]) | !is.na(obj), 1, NA)
        }
      }

      # Yearly new area per sector (NA for lines)
      val <- geom_area_km2(obj)
      data.table::data.table(
        scenario_id = scenario_id,
        rep_id = rep_id,
        year = yy,
        metric = "sector_yearly_new_area_km2",
        sector = sector,
        value = val
      )
    })
    per_sector_dt <- data.table::rbindlist(per_sector_rows, use.names = TRUE, fill = TRUE)

    # Total yearly new area across sectors (fast path = sum of per-sector areas)
    total_area <- sum(per_sector_dt$value, na.rm = TRUE)

    # Attempt exact union if all sector objects are polygon vectors
    exact_union <- FALSE
    if (exact_union) {
      polySectors <- names(ylist)[sapply(ylist, function(o) inherits(o, "SpatVector") && .sv_kind(o) == "polygons")]
      if (length(polySectors) == length(ylist) && length(polySectors) > 0) {
        vv <- do.call(terra::rbind, ylist[polySectors])
        vv <- .dissolve_all(vv)
        total_area <- geom_area_km2(vv)
      }
    }

    total_dt <- data.table::data.table(
      scenario_id = scenario_id,
      rep_id = rep_id,
      year = yy,
      metric = "total_yearly_new_area_km2",
      sector = NA_character_,
      value = total_area
    )

    # Current totals after this tick (per sector)
    cur_rows <- list()
    # Polygons
    if (length(accumPoly)) {
      cur_rows[["poly_area"]] <- data.table::rbindlist(lapply(names(accumPoly), function(sector) {
        areaKm2 <- geom_area_km2(accumPoly[[sector]])
        data.table::data.table(
          scenario_id = scenario_id,
          rep_id = rep_id,
          year = yy,
          metric = "sector_current_total_area_km2",
          sector = sector,
          value = areaKm2
        )
      }), use.names = TRUE, fill = TRUE)
    }
    # Rasters
    if (length(accumRast)) {
      cur_rows[["rast_area"]] <- data.table::rbindlist(lapply(names(accumRast), function(sector) {
        areaKm2 <- geom_area_km2(accumRast[[sector]])
        data.table::data.table(
          scenario_id = scenario_id,
          rep_id = rep_id,
          year = yy,
          metric = "sector_current_total_area_km2",
          sector = sector,
          value = areaKm2
        )
      }), use.names = TRUE, fill = TRUE)
    }
    # Lines
    if (length(accumLen)) {
      cur_rows[["line_len"]] <- data.table::rbindlist(lapply(names(accumLen), function(sector) {
        data.table::data.table(
          scenario_id = scenario_id,
          rep_id = rep_id,
          year = yy,
          metric = "sector_current_total_length_km",
          sector = sector,
          value = as.numeric(accumLen[[sector]])
        )
      }), use.names = TRUE, fill = TRUE)
    }

    cur_dt <- if (length(cur_rows)) data.table::rbindlist(cur_rows, use.names = TRUE, fill = TRUE) else data.table::data.table()

    out[[as.character(yy)]] <- bind_safely(per_sector_dt, total_dt, cur_dt)
  }

  if (!length(out)) return(data.table::data.table())
  do.call(bind_safely, out)
}
