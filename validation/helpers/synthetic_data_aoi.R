#' Create synthetic disturbance features for southwest NWT AOI testing
#'
#' This helper script fabricates per-dataClass vector layers with controlled
#' metrics (area, length, counts) inside the southwest NWT study area so that
#' disturbance-rate tests can rely on deterministic inputs.

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(data.table)
  library(units)
})

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
study_area_candidates <- c(
  file.path(project_root, "data", "study_area", "aoi_southwest_NWT.shp"),
  file.path(project_root, "data", "study_area", "aoi_southwest_NWT.gpkg"),
  file.path(project_root, "data", "medium_aoi.shp")
)
study_area_path <- NULL
for (cand in study_area_candidates) {
  if (file.exists(cand)) {
    study_area_path <- cand
    break
  }
}
if (is.null(study_area_path)) {
  stop("Expected study area at data/study_area/aoi_southwest_NWT.(shp|gpkg)", call. = FALSE)
}

output_dir <- file.path(project_root, "scratch", "medium_aoi_optimal")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
gpkg_path <- file.path(output_dir, "medium_aoi_optimal.gpkg")
unlink(c(gpkg_path,
         paste0(gpkg_path, "-wal"),
         paste0(gpkg_path, "-shm")))

study_area <- terra::vect(study_area_path)
study_area <- terra::project(study_area, "EPSG:3347")
study_area_area_km2 <- terra::expanse(study_area, unit = "km")

# restrict placements to a large rectangle inside the study area
sa_ext <- terra::ext(study_area)
sa_xmin <- terra::xmin(sa_ext)
sa_xmax <- terra::xmax(sa_ext)
sa_ymin <- terra::ymin(sa_ext)
sa_ymax <- terra::ymax(sa_ext)
range_x <- sa_xmax - sa_xmin
range_y <- sa_ymax - sa_ymin
margin_x <- 0.15 * range_x
margin_y <- 0.15 * range_y
init_ext <- terra::ext(
  sa_xmin + margin_x,
  sa_xmax - margin_x,
  sa_ymin + margin_y,
  sa_ymax - margin_y
)
make_rect_from_ext <- function(extent) {
  coords <- matrix(
    c(extent$xmin, extent$ymin,
      extent$xmax, extent$ymin,
      extent$xmax, extent$ymax,
      extent$xmin, extent$ymax,
      extent$xmin, extent$ymin),
    ncol = 2,
    byrow = TRUE
  )
  terra::vect(coords, type = "polygons", crs = terra::crs(study_area))
}
placement_rect <- make_rect_from_ext(init_ext)
placement_area <- suppressWarnings(terra::intersect(study_area, placement_rect))
shrink_factor <- 0.5
while (terra::nrow(placement_area) == 0 && shrink_factor > 0.05) {
  margin_x <- margin_x * shrink_factor
  margin_y <- margin_y * shrink_factor
  new_ext <- terra::ext(
    sa_xmin + margin_x,
    sa_xmax - margin_x,
    sa_ymin + margin_y,
    sa_ymax - margin_y
  )
  placement_rect <- make_rect_from_ext(new_ext)
  placement_area <- suppressWarnings(terra::intersect(study_area, placement_rect))
  shrink_factor <- shrink_factor * 0.75
}
if (terra::nrow(placement_area) == 0) {
  stop("Failed to derive interior placement rectangle within study area.", call. = FALSE)
}
placement_ext <- terra::ext(placement_area)
placement_xmin <- terra::xmin(placement_ext)
placement_xmax <- terra::xmax(placement_ext)
placement_ymin <- terra::ymin(placement_ext)
placement_ymax <- terra::ymax(placement_ext)
placement_range_x <- placement_xmax - placement_xmin
placement_range_y <- placement_ymax - placement_ymin
placement_area_km2 <- terra::expanse(placement_area, unit = "km")

clamp_bounds <- function(min_bound, max_bound, half_size, buffer = 500) {
  lower <- min_bound + half_size + buffer
  upper <- max_bound - half_size - buffer
  if (lower > upper) {
    lower <- min_bound + half_size
    upper <- max_bound - half_size
  }
  list(lower = lower, upper = upper)
}

clamp_center <- function(center, min_bound, max_bound, half_size, buffer = 500) {
  bounds <- clamp_bounds(min_bound, max_bound, half_size, buffer)
  pmax(pmin(center, bounds$upper), bounds$lower)
}

total_area <- function(vec, unit = "m") {
  if (!inherits(vec, "SpatVector") || terra::nrow(vec) == 0) return(0)
  vals <- terra::expanse(vec, unit = unit)
  if (is.data.frame(vals)) {
    if ("area" %in% names(vals)) {
      vals <- vals[["area"]]
    }
  }
  sum(as.numeric(vals), na.rm = TRUE)
}

make_rect_center <- function(cx, cy, width_m, height_m) {
  half_w <- width_m / 2
  half_h <- height_m / 2
  coords <- matrix(
    c(cx - half_w, cy - half_h,
      cx + half_w, cy - half_h,
      cx + half_w, cy + half_h,
      cx - half_w, cy + half_h,
      cx - half_w, cy - half_h),
    ncol = 2,
    byrow = TRUE
  )
  terra::vect(coords, type = "polygons", crs = terra::crs(study_area))
}

make_line <- function(start, end) {
  terra::vect(rbind(start, end), type = "lines", crs = terra::crs(study_area))
}

grow_within_bounds <- function(center, base_dim, proposal_dim, min_bound, max_bound, buffer = 2000) {
  half_base <- base_dim / 2
  half_prop <- max(proposal_dim / 2, half_base * 1.05)
  max_half <- min(center - min_bound, max_bound - center) - buffer
  if (!is.finite(max_half)) max_half <- half_prop
  if (max_half <= half_base) {
    half <- min(max_half, half_base * 1.01)
    if (!is.finite(half) || half <= 0) {
      half <- half_base * 1.05
    }
  } else {
    half <- min(half_prop, max_half)
    if (half < half_base * 1.01) {
      half <- min(max_half, half_base * 1.01)
    }
  }
  2 * half
}

sample_truncnorm <- function(n, mean, sd, lower = -Inf, upper = Inf, max_iter = 1000L) {
  if (n <= 0L) return(numeric(0))
  vals <- stats::rnorm(n, mean, sd)
  invalid <- which(vals < lower | vals > upper)
  iter <- 0L
  while (length(invalid) && iter < max_iter) {
    vals[invalid] <- stats::rnorm(length(invalid), mean, sd)
    invalid <- which(vals < lower | vals > upper)
    iter <- iter + 1L
  }
  if (length(invalid)) {
    vals[vals < lower] <- lower
    vals[vals > upper] <- upper
  }
  vals
}

write_layer <- function(sf_obj, layer_name) {
  if (!file.exists(gpkg_path)) {
    sf::st_write(sf_obj, gpkg_path, layer = layer_name,
                 delete_dsn = TRUE, quiet = TRUE)
  } else {
    sf::st_write(sf_obj, gpkg_path, layer = layer_name,
                 delete_layer = TRUE, quiet = TRUE)
  }
}

actual_specs <- data.table(
  dataClass = c("cutblocks", "mining", "oilGas", "otherPolygons", "settlements"),
  n_features = c(4L, 3L, 3L, 3L, 3L),
  width_km = c(5.0, 4.0, 4.0, 3.5, 4.0),
  height_km = c(5.0, 3.0, 2.5, 2.2, 3.0),
  origin_year = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
)
actual_specs[, `:=`(
  width_m = width_km * 1000,
  height_m = height_km * 1000,
  area_km2 = width_km * height_km,
  y_ratio = seq(0.18, 0.82, length.out = .N)
)]

potential_specs <- data.table(
  dataClass = c("potentialCutblocks", "potentialMining", "potentialOilGas",
                "potentialOtherPolygons"),
  sourceClass = c("cutblocks", "mining", "oilGas", "otherPolygons"),
  n_features = c(8L, 3L, 4L, 4L),
  scale_min = c(2.1, 2.3, 2.4, 2.2),
  scale_max = c(2.8, 3.1, 3.2, 2.9),
  origin_year = c(1960L, NA_integer_, NA_integer_, NA_integer_)
)
potential_specs <- merge(
  potential_specs,
  actual_specs[, .(sourceClass = dataClass, base_y_ratio = y_ratio)],
  by = "sourceClass",
  all.x = TRUE,
  sort = FALSE
)
potential_specs[, y_ratio := pmin(base_y_ratio + 0.05, 0.95)]

line_specs <- data.table(
  dataClass = c("pipeline", "powerLines", "roads"),
  n_features = c(3L, 4L, 5L)
)

point_specs <- data.table(
  dataClass = c("windTurbines", "potentialWindTurbines"),
  n_features = c(15L, 25L)
)
point_specs <- merge(
  point_specs,
  data.table(
    dataClass = c("windTurbines", "potentialWindTurbines"),
    rowClass = c("settlements", "settlements"),
    y_offset = c(-0.04, 0.03)
  ),
  by = "dataClass",
  all.x = TRUE,
  sort = FALSE
)

set.seed(20251102)

stats <- list()
generated_vectors <- list()
actual_layout <- list()
potential_layout <- list()
potential_map <- setNames(potential_specs$dataClass, potential_specs$sourceClass)

message("Generating polygon features...")
for (i in seq_len(nrow(actual_specs))) {
  spec <- actual_specs[i]
  x_ratios <- if (spec$n_features == 1L) 0.5 else seq(0.18, 0.82, length.out = spec$n_features)
  x_centers <- placement_xmin + x_ratios * placement_range_x
  x_centers <- clamp_center(x_centers, placement_xmin, placement_xmax, spec$width_m / 2, buffer = 2000)
  y_center <- placement_ymin + spec$y_ratio * placement_range_y
  y_center <- clamp_center(y_center, placement_ymin, placement_ymax, spec$height_m / 2, buffer = 2000)
  features <- vector("list", spec$n_features)
  for (j in seq_len(spec$n_features)) {
    rect <- make_rect_center(x_centers[j], y_center, spec$width_m, spec$height_m)
    rect$feature_id <- j
    rect$dataClass <- spec$dataClass
    rect$target_area_km2 <- spec$area_km2
    features[[j]] <- rect[, c("feature_id", "dataClass", "target_area_km2")]
  }
  class_vect <- do.call(rbind, features)
  if (!is.na(spec$origin_year)) {
    class_vect$ORIGIN <- as.integer(spec$origin_year)
  }
  sf_obj <- sf::st_as_sf(class_vect)
  if (!is.na(spec$origin_year)) {
    sf_obj$ORIGIN <- as.integer(spec$origin_year)
  }
  write_layer(sf_obj, spec$dataClass)
  generated_vectors[[spec$dataClass]] <- class_vect
  centroid_x <- mean(x_centers)
  centroid_y <- y_center
  bbox <- terra::ext(class_vect)
  stats[[length(stats) + 1L]] <- data.table(
    dataClass = spec$dataClass,
    geometry = "polygons",
    n_features = spec$n_features,
    total_area_km2 = sum(as.numeric(sf::st_area(sf_obj))) / 1e6,
    total_length_km = NA_real_,
    point_count = NA_integer_,
    centroid_x = centroid_x,
    centroid_y = centroid_y,
    bbox_xmin = bbox$xmin,
    bbox_ymin = bbox$ymin,
    bbox_xmax = bbox$xmax,
    bbox_ymax = bbox$ymax
  )
  actual_layout[[spec$dataClass]] <- list(
    x = x_centers,
    y = rep(y_center, spec$n_features),
    width_m = rep(spec$width_m, spec$n_features),
    height_m = rep(spec$height_m, spec$n_features),
    y_top = y_center + spec$height_m / 2,
    y_bottom = y_center - spec$height_m / 2
  )
}

message("Generating potential polygon features...")
for (i in seq_len(nrow(potential_specs))) {
  spec <- potential_specs[i]
  base_info <- actual_layout[[spec$sourceClass]]
  if (is.null(base_info)) {
    stop("Missing base geometry for ", spec$sourceClass, " while building ", spec$dataClass, call. = FALSE)
  }

  base_count <- length(base_info$x)
  targets <- spec$n_features
  share <- rep(floor(targets / base_count), base_count)
  remainder <- targets - sum(share)
  if (remainder > 0) {
    share[seq_len(remainder)] <- share[seq_len(remainder)] + 1L
  }

  features <- vector("list", targets)
  x_centers <- numeric(targets)
  y_centers <- numeric(targets)
  width_vals <- numeric(targets)
  height_vals <- numeric(targets)

  pot_idx <- 1L
  for (b in seq_len(base_count)) {
    n_here <- share[b]
    if (n_here <= 0L) next
    base_x <- base_info$x[b]
    base_y <- base_info$y[b]
    base_width <- base_info$width_m[b]
    base_height <- base_info$height_m[b]
    scales <- if (n_here == 1L) rep(spec$scale_max, 1L) else seq(spec$scale_min, spec$scale_max, length.out = n_here)
    for (s in scales) {
      if (pot_idx > targets) break
      width_m <- base_width * s
      height_m <- base_height * s
      width_m <- grow_within_bounds(base_x, base_width, width_m, placement_xmin, placement_xmax)
      height_m <- grow_within_bounds(base_y, base_height, height_m, placement_ymin, placement_ymax)
      rect <- make_rect_center(base_x, base_y, width_m, height_m)
      rect$feature_id <- pot_idx
      rect$dataClass <- spec$dataClass
      rect$target_area_km2 <- (width_m * height_m) / 1e6
      rect$Potential <- pot_idx
      features[[pot_idx]] <- rect[, c("feature_id", "dataClass", "target_area_km2", "Potential")]
      x_centers[pot_idx] <- base_x
      y_centers[pot_idx] <- base_y
      width_vals[pot_idx] <- width_m
      height_vals[pot_idx] <- height_m
      pot_idx <- pot_idx + 1L
    }
  }

  features <- features[!vapply(features, is.null, logical(1))]
  if (!length(features)) next
  class_vect <- do.call(rbind, features)
  if (!is.na(spec$origin_year)) {
    class_vect$ORIGIN <- as.integer(spec$origin_year)
  }
  sf_obj <- sf::st_as_sf(class_vect)
  if (!"Potential" %in% names(sf_obj)) {
    sf_obj$Potential <- seq_len(nrow(sf_obj))
  }
  if (!is.na(spec$origin_year)) {
    sf_obj$ORIGIN <- as.integer(spec$origin_year)
  }
  write_layer(sf_obj, spec$dataClass)
  generated_vectors[[spec$dataClass]] <- class_vect
  bbox <- terra::ext(class_vect)
  stats[[length(stats) + 1L]] <- data.table(
    dataClass = spec$dataClass,
    geometry = "polygons",
    n_features = nrow(sf_obj),
    total_area_km2 = sum(as.numeric(sf::st_area(sf_obj))) / 1e6,
    total_length_km = NA_real_,
    point_count = NA_integer_,
    centroid_x = mean(x_centers[seq_len(nrow(sf_obj))]),
    centroid_y = mean(y_centers[seq_len(nrow(sf_obj))]),
    bbox_xmin = bbox$xmin,
    bbox_ymin = bbox$ymin,
    bbox_xmax = bbox$xmax,
    bbox_ymax = bbox$ymax
  )
  potential_layout[[spec$dataClass]] <- list(
    x = x_centers[seq_len(nrow(sf_obj))],
    y = y_centers[seq_len(nrow(sf_obj))],
    width_m = width_vals[seq_len(nrow(sf_obj))],
    height_m = height_vals[seq_len(nrow(sf_obj))]
  )
}

message("Generating seismic line features...")
seismic_seed <- 20251107
if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
  seed_backup <- get(".Random.seed", envir = .GlobalEnv)
} else {
  seed_backup <- NULL
}
set.seed(seismic_seed)

pot_oil <- generated_vectors[["potentialOilGas"]]
seismic_count <- 36L
seismic_lengths <- sample_truncnorm(
  seismic_count,
  mean = 1100,
  sd = 350,
  lower = 200,
  upper = 2000
)
seismic_angles <- runif(seismic_count, min = 5, max = 175)

seismic_lines <- list()
angles_kept <- numeric(0)
if (!is.null(pot_oil) && terra::nrow(pot_oil) > 0) {
  n_feat <- terra::nrow(pot_oil)
  base_per <- rep(floor(seismic_count / n_feat), n_feat)
  remainder <- seismic_count - sum(base_per)
  if (remainder > 0) {
    base_per[seq_len(remainder)] <- base_per[seq_len(remainder)] + 1L
  }

  collected <- list()
  idx_col <- 1L
  for (feat_idx in seq_len(n_feat)) {
    take_n <- base_per[feat_idx]
    if (take_n <= 0L) next
    feat_poly <- pot_oil[feat_idx]
    f_ext <- terra::ext(feat_poly)
    west_limit <- f_ext$xmin + 0.4 * (f_ext$xmax - f_ext$xmin)
    left_ext <- terra::ext(f_ext$xmin, west_limit, f_ext$ymin, f_ext$ymax)
    left_clip <- suppressWarnings(terra::crop(feat_poly, left_ext))
    if (is.null(left_clip) || !inherits(left_clip, "SpatVector") || terra::nrow(left_clip) == 0L) {
      left_clip <- feat_poly
    }
    centers_feat <- tryCatch(
      terra::spatSample(left_clip, size = take_n, method = "random"),
      error = function(e) NULL
    )
    if (is.null(centers_feat) || terra::nrow(centers_feat) < take_n) {
      missing <- take_n - if (!is.null(centers_feat)) terra::nrow(centers_feat) else 0L
      if (missing > 0L) {
        extra <- tryCatch(terra::spatSample(feat_poly, size = missing, method = "random"), error = function(e) NULL)
        if (!is.null(extra)) {
          centers_feat <- if (is.null(centers_feat)) extra else rbind(centers_feat, extra)
        }
      }
    }
    if (is.null(centers_feat) || terra::nrow(centers_feat) == 0L) {
      centers_feat <- terra::spatSample(placement_area, size = take_n, method = "random")
    }
    collected[[idx_col]] <- centers_feat
    idx_col <- idx_col + 1L
  }

  if (length(collected)) {
    base_centers <- do.call(rbind, collected)
  } else {
    base_centers <- terra::spatSample(pot_oil, size = seismic_count, method = "random")
  }

  if (terra::nrow(base_centers) < seismic_count) {
    filler <- terra::spatSample(pot_oil, size = seismic_count - terra::nrow(base_centers), method = "random")
    base_centers <- rbind(base_centers, filler)
  }

  center_geom <- terra::geom(base_centers)[, c("x", "y"), drop = FALSE]
  for (idx in seq_len(nrow(center_geom))) {
    cx <- center_geom[idx, "x"]
    cy <- center_geom[idx, "y"]
    length_m <- seismic_lengths[idx]
    angle_deg <- seismic_angles[idx]
    angle_rad <- angle_deg * pi / 180
    half <- length_m / 2
    attempt <- 0L
    repeat {
      start <- c(cx - cos(angle_rad) * half, cy - sin(angle_rad) * half)
      end <- c(cx + cos(angle_rad) * half, cy + sin(angle_rad) * half)
      within_bounds <- all(start[1] >= placement_xmin, start[1] <= placement_xmax,
                           start[2] >= placement_ymin, start[2] <= placement_ymax,
                           end[1]   >= placement_xmin, end[1]   <= placement_xmax,
                           end[2]   >= placement_ymin, end[2]   <= placement_ymax)
      if (within_bounds || half < 150) break
      half <- half * 0.85
      attempt <- attempt + 1L
      if (attempt > 8L) break
    }
    if (half < 150) next
    line_vec <- make_line(start, end)
    line_vec$origin_angle <- angle_deg
    seismic_lines[[length(seismic_lines) + 1L]] <- line_vec
    angles_kept <- c(angles_kept, angle_deg)
  }
}

  if (length(seismic_lines)) {
    line_vect <- do.call(rbind, seismic_lines)
    sf_obj <- sf::st_as_sf(line_vect)
    if (!nrow(sf_obj)) {
      sf_obj <- NULL
    } else {
      sf_obj$feature_id <- seq_len(nrow(sf_obj))
      sf_obj$dataClass <- "seismicLines"
      line_lengths <- as.numeric(units::set_units(sf::st_length(sf_obj), "m"))
      sf_obj$lineLength <- line_lengths
      sf_obj$calculatedLength <- line_lengths
      sf_obj$angles <- angles_kept[seq_len(nrow(sf_obj))]
      if (!is.null(pot_oil) && terra::nrow(pot_oil) > 0) {
        pot_sf <- sf::st_as_sf(pot_oil)
        line_id_geom <- sf::st_sf(feature_id = sf_obj$feature_id, geometry = sf::st_geometry(sf_obj))
        joined <- suppressWarnings(sf::st_intersection(line_id_geom, pot_sf[, c("Potential")]))
        pot_assign <- vapply(sf_obj$feature_id, function(fid) {
          vals <- joined$Potential[joined$feature_id == fid]
          vals <- vals[!is.na(vals)]
          if (!length(vals)) NA_real_ else vals[1]
        }, numeric(1))
        if (anyNA(pot_assign)) {
          fallback <- pot_oil$Potential
          pot_assign[is.na(pot_assign)] <- rep(fallback, length.out = sum(is.na(pot_assign)))
        }
        sf_obj$Potential <- pot_assign
      } else {
        sf_obj$Potential <- seq_len(nrow(sf_obj))
      }
      sf_obj$Pot_Clus <- sprintf("%s_%02d", sf_obj$Potential, seq_len(nrow(sf_obj)))
      sf_obj$Class <- "seismicLines"
      write_layer(sf_obj, "seismicLines")
      seismic_vect <- terra::vect(sf_obj)
      generated_vectors[["seismicLines"]] <- seismic_vect
      bbox <- terra::ext(seismic_vect)
      mid_coords <- sf::st_coordinates(sf::st_line_sample(sf_obj, sample = 0.5))
      stats[[length(stats) + 1L]] <- data.table(
        dataClass = "seismicLines",
        geometry = "lines",
        n_features = nrow(sf_obj),
        total_area_km2 = NA_real_,
        total_length_km = sum(line_lengths) / 1000,
        point_count = NA_integer_,
        centroid_x = mean(mid_coords[, "X"]),
        centroid_y = mean(mid_coords[, "Y"]),
        bbox_xmin = bbox$xmin,
        bbox_ymin = bbox$ymin,
        bbox_xmax = bbox$xmax,
        bbox_ymax = bbox$ymax
      )
    }
  }

  if (!is.null(pot_oil) && terra::nrow(pot_oil) > 0) {
    pot_seis <- vector("list", terra::nrow(pot_oil))
    for (idx in seq_len(terra::nrow(pot_oil))) {
      feat_poly <- pot_oil[idx]
      p_val <- feat_poly$Potential[1]
      f_ext <- terra::ext(feat_poly)
      west_limit <- f_ext$xmin + 0.45 * (f_ext$xmax - f_ext$xmin)
      left_ext <- terra::ext(f_ext$xmin, west_limit, f_ext$ymin, f_ext$ymax)
      left_clip <- suppressWarnings(terra::crop(feat_poly, left_ext))
      if (is.null(left_clip) || !inherits(left_clip, "SpatVector") || terra::nrow(left_clip) == 0L) {
        left_clip <- feat_poly
      }
      left_clip$Potential <- p_val
      pot_seis[[idx]] <- left_clip
    }
    pot_seis <- pot_seis[!vapply(pot_seis, is.null, logical(1))]
    if (length(pot_seis)) {
      pot_seis_vect <- do.call(rbind, pot_seis)
      pot_seis_sf <- sf::st_as_sf(pot_seis_vect)
      write_layer(pot_seis_sf, "potentialSeismicLines")
      generated_vectors[["potentialSeismicLines"]] <- pot_seis_vect
      bbox <- terra::ext(pot_seis_vect)
      stats[[length(stats) + 1L]] <- data.table(
        dataClass = "potentialSeismicLines",
        geometry = "polygons",
        n_features = terra::nrow(pot_seis_vect),
        total_area_km2 = sum(terra::expanse(pot_seis_vect, unit = "km"), na.rm = TRUE),
        total_length_km = NA_real_,
        point_count = NA_integer_,
        centroid_x = mean(terra::geom(terra::centroids(pot_seis_vect))[, "x"]),
        centroid_y = mean(terra::geom(terra::centroids(pot_seis_vect))[, "y"]),
        bbox_xmin = bbox$xmin,
        bbox_ymin = bbox$ymin,
        bbox_xmax = bbox$xmax,
        bbox_ymax = bbox$ymax
      )
    }
  }

if (!is.null(seed_backup)) {
  assign(".Random.seed", seed_backup, envir = .GlobalEnv)
}

message("Generating connecting line features...")
pairs_for_settlement_mine <- function(limit, set_coords, mine_coords) {
  if (!nrow(set_coords) || !nrow(mine_coords) || limit <= 0) {
    return(data.table::data.table())
  }
  combos <- data.table::CJ(set_idx = seq_len(nrow(set_coords)), mine_idx = seq_len(nrow(mine_coords)))
  combos[, dist := sqrt((set_coords[set_idx, 1] - mine_coords[mine_idx, 1])^2 +
                        (set_coords[set_idx, 2] - mine_coords[mine_idx, 2])^2)]
  data.table::setorder(combos, dist)
  combos[seq_len(min(nrow(combos), limit))]
}

get_centroid_coords <- function(vec) {
  cent <- terra::centroids(vec)
  terra::geom(cent)[, c("x", "y"), drop = FALSE]
}

shift_point_y <- function(pt, dy) {
  new_y <- pt[2] + dy
  new_y <- pmin(pmax(new_y, placement_ymin + 1000), placement_ymax - 1000)
  c(pt[1], new_y)
}

roads_spec <- line_specs[dataClass == "roads"]
settlements_vect <- generated_vectors[["settlements"]]
mining_vect <- generated_vectors[["mining"]]
oil_vect <- generated_vectors[["oilGas"]]

if (nrow(roads_spec) == 1L && !is.null(settlements_vect) && !is.null(mining_vect)) {
  set_coords <- get_centroid_coords(settlements_vect)
  mine_coords <- get_centroid_coords(mining_vect)
  pair_table <- pairs_for_settlement_mine(roads_spec$n_features, set_coords, mine_coords)
  if (nrow(pair_table)) {
    road_segments <- vector("list", nrow(pair_table))
    for (j in seq_len(nrow(pair_table))) {
      s_idx <- pair_table$set_idx[j]
      m_idx <- pair_table$mine_idx[j]
      road_segments[[j]] <- make_line(set_coords[s_idx, ], mine_coords[m_idx, ])
    }
    class_vect <- do.call(rbind, road_segments)
    class_vect$feature_id <- seq_len(terra::nrow(class_vect))
    class_vect$dataClass <- "roads"
    sf_obj <- sf::st_as_sf(class_vect)
    length_km <- as.numeric(sf::st_length(sf_obj)) / 1000
    sf_obj$target_length_km <- round(length_km, 3)
    write_layer(sf_obj, "roads")
    generated_vectors[["roads"]] <- terra::vect(sf_obj)
    bbox <- terra::ext(class_vect)
    mid_coords <- sf::st_coordinates(sf::st_line_sample(sf_obj, sample = 0.5))
    stats[[length(stats) + 1L]] <- data.table(
      dataClass = "roads",
      geometry = "lines",
      n_features = nrow(sf_obj),
      total_area_km2 = NA_real_,
      total_length_km = sum(length_km),
      point_count = NA_integer_,
      centroid_x = mean(mid_coords[, "X"]),
      centroid_y = mean(mid_coords[, "Y"]),
      bbox_xmin = bbox$xmin,
      bbox_ymin = bbox$ymin,
      bbox_xmax = bbox$xmax,
      bbox_ymax = bbox$ymax
    )
  }
}

power_spec <- line_specs[dataClass == "powerLines"]
if (nrow(power_spec) == 1L && !is.null(settlements_vect) && !is.null(mining_vect)) {
  set_coords <- get_centroid_coords(settlements_vect)
  mine_coords <- get_centroid_coords(mining_vect)
  pair_table <- pairs_for_settlement_mine(power_spec$n_features, set_coords, mine_coords)
  if (nrow(pair_table)) {
    power_segments <- vector("list", nrow(pair_table))
    for (j in seq_len(nrow(pair_table))) {
      s_idx <- pair_table$set_idx[j]
      m_idx <- pair_table$mine_idx[j]
      start_pt <- shift_point_y(set_coords[s_idx, ], 600)
      end_pt <- shift_point_y(mine_coords[m_idx, ], -600)
      power_segments[[j]] <- make_line(start_pt, end_pt)
    }
    class_vect <- do.call(rbind, power_segments)
    class_vect$feature_id <- seq_len(terra::nrow(class_vect))
    class_vect$dataClass <- "powerLines"
    sf_obj <- sf::st_as_sf(class_vect)
    length_km <- as.numeric(sf::st_length(sf_obj)) / 1000
    sf_obj$target_length_km <- round(length_km, 3)
    write_layer(sf_obj, "powerLines")
    generated_vectors[["powerLines"]] <- terra::vect(sf_obj)
    bbox <- terra::ext(class_vect)
    mid_coords <- sf::st_coordinates(sf::st_line_sample(sf_obj, sample = 0.5))
    stats[[length(stats) + 1L]] <- data.table(
      dataClass = "powerLines",
      geometry = "lines",
      n_features = nrow(sf_obj),
      total_area_km2 = NA_real_,
      total_length_km = sum(length_km),
      point_count = NA_integer_,
      centroid_x = mean(mid_coords[, "X"]),
      centroid_y = mean(mid_coords[, "Y"]),
      bbox_xmin = bbox$xmin,
      bbox_ymin = bbox$ymin,
      bbox_xmax = bbox$xmax,
      bbox_ymax = bbox$ymax
    )

    pot_power_sf <- sf_obj
    pot_power_sf$dataClass <- "potentialPowerLines"
    pot_power_sf$feature_id <- seq_len(nrow(pot_power_sf))
    if (!"Potential" %in% names(pot_power_sf)) {
      if ("target_length_km" %in% names(pot_power_sf)) {
        pot_power_sf$Potential <- pot_power_sf$target_length_km
      } else {
        pot_power_sf$Potential <- seq_len(nrow(pot_power_sf))
      }
    }
    write_layer(pot_power_sf, "potentialPowerLines")
    generated_vectors[["potentialPowerLines"]] <- terra::vect(pot_power_sf)
    stats[[length(stats) + 1L]] <- data.table(
      dataClass = "potentialPowerLines",
      geometry = "lines",
      n_features = nrow(pot_power_sf),
      total_area_km2 = NA_real_,
      total_length_km = sum(length_km),
      point_count = NA_integer_,
      centroid_x = mean(mid_coords[, "X"]),
      centroid_y = mean(mid_coords[, "Y"]),
      bbox_xmin = bbox$xmin,
      bbox_ymin = bbox$ymin,
      bbox_xmax = bbox$xmax,
      bbox_ymax = bbox$ymax
    )
  }
}

pipeline_spec <- line_specs[dataClass == "pipeline"]
if (nrow(pipeline_spec) == 1L) {
  y_value <- placement_ymax - 2500
  y_value <- max(min(y_value, placement_ymax - 1500), placement_ymin + 1500)
  x_positions <- seq(placement_xmin + 2000, placement_xmax - 2000,
                     length.out = pipeline_spec$n_features + 1)
  pipe_segments <- vector("list", pipeline_spec$n_features)
  for (idx in seq_len(pipeline_spec$n_features)) {
    start <- c(x_positions[idx], y_value)
    end <- c(x_positions[idx + 1], y_value)
    pipe_segments[[idx]] <- make_line(start, end)
  }
  class_vect <- do.call(rbind, pipe_segments)
  class_vect$feature_id <- seq_len(terra::nrow(class_vect))
  class_vect$dataClass <- "pipeline"
  sf_obj <- sf::st_as_sf(class_vect)
  length_km <- as.numeric(sf::st_length(sf_obj)) / 1000
  sf_obj$target_length_km <- round(length_km, 3)
  write_layer(sf_obj, "pipeline")
  generated_vectors[["pipeline"]] <- terra::vect(sf_obj)
  bbox <- terra::ext(class_vect)
  mid_coords <- sf::st_coordinates(sf::st_line_sample(sf_obj, sample = 0.5))
  stats[[length(stats) + 1L]] <- data.table(
    dataClass = "pipeline",
    geometry = "lines",
    n_features = nrow(sf_obj),
    total_area_km2 = NA_real_,
    total_length_km = sum(length_km),
    point_count = NA_integer_,
    centroid_x = mean(mid_coords[, "X"]),
    centroid_y = mean(mid_coords[, "Y"]),
    bbox_xmin = bbox$xmin,
    bbox_ymin = bbox$ymin,
    bbox_xmax = bbox$xmax,
    bbox_ymax = bbox$ymax
  )

  pot_pipe_sf <- sf_obj
  pot_pipe_sf$dataClass <- "potentialPipeline"
  pot_pipe_sf$feature_id <- seq_len(nrow(pot_pipe_sf))
  if (!"Potential" %in% names(pot_pipe_sf)) {
    if ("target_length_km" %in% names(pot_pipe_sf)) {
      pot_pipe_sf$Potential <- pot_pipe_sf$target_length_km
    } else {
      pot_pipe_sf$Potential <- seq_len(nrow(pot_pipe_sf))
    }
  }
  write_layer(pot_pipe_sf, "potentialPipeline")
  generated_vectors[["potentialPipeline"]] <- terra::vect(pot_pipe_sf)
  stats[[length(stats) + 1L]] <- data.table(
    dataClass = "potentialPipeline",
    geometry = "lines",
    n_features = nrow(pot_pipe_sf),
    total_area_km2 = NA_real_,
    total_length_km = sum(length_km),
    point_count = NA_integer_,
    centroid_x = mean(mid_coords[, "X"]),
    centroid_y = mean(mid_coords[, "Y"]),
    bbox_xmin = bbox$xmin,
    bbox_ymin = bbox$ymin,
    bbox_xmax = bbox$xmax,
    bbox_ymax = bbox$ymax
  )
}

message("Generating point features...")
for (i in seq_len(nrow(point_specs))) {
  spec <- point_specs[i]
  target <- spec$n_features
  row_info <- if (spec$rowClass %in% names(actual_layout)) {
    actual_layout[[spec$rowClass]]
  } else if (spec$rowClass %in% names(potential_layout)) {
    potential_layout[[spec$rowClass]]
  } else {
    NULL
  }
  base_y <- if (!is.null(row_info) && length(row_info$y)) {
    mean(row_info$y)
  } else if (spec$dataClass == "windTurbines") {
    placement_ymin + 0.78 * placement_range_y
  } else {
    placement_ymin + 0.9 * placement_range_y
  }
  y_value <- base_y + spec$y_offset * placement_range_y
  y_value <- clamp_center(y_value, placement_ymin, placement_ymax, 50, buffer = 2000)
  x_ratios <- seq(0.2, 0.8, length.out = target)
  x_centers <- placement_xmin + x_ratios * placement_range_x
  x_centers <- clamp_center(x_centers, placement_xmin, placement_xmax, 50, buffer = 1500)
  features <- vector("list", target)
  coords_matrix <- matrix(NA_real_, nrow = target, ncol = 2)
  isPotWind <- identical(as.character(spec$dataClass), "potentialWindTurbines")
  geomType <- if (isPotWind) "polygons" else "points"
  for (j in seq_len(target)) {
    coords_matrix[j, ] <- c(x_centers[j], y_value)
    if (isPotWind) {
      side_m <- sqrt(62500)
      rect <- make_rect_center(coords_matrix[j, 1], coords_matrix[j, 2], side_m, side_m)
      rect$feature_id <- j
      rect$dataClass <- spec$dataClass
      rect$Potential <- j
      rect$target_area_km2 <- (side_m * side_m) / 1e6
      features[[j]] <- rect[, c("feature_id", "dataClass", "Potential", "target_area_km2")]
    } else {
      pt <- terra::vect(matrix(coords_matrix[j, ], ncol = 2),
                        type = "points", crs = terra::crs(study_area))
      pt$feature_id <- j
      pt$dataClass <- spec$dataClass
      features[[j]] <- pt[, c("feature_id", "dataClass")]
    }
  }
  class_vect <- do.call(rbind, features)
  sf_obj <- sf::st_as_sf(class_vect)
  write_layer(sf_obj, spec$dataClass)
  generated_vectors[[spec$dataClass]] <- terra::vect(sf_obj)
  bbox <- terra::ext(class_vect)
  stats[[length(stats) + 1L]] <- data.table(
    dataClass = spec$dataClass,
    geometry = geomType,
    n_features = target,
    total_area_km2 = if (isPotWind) sum(as.numeric(sf::st_area(sf_obj))) / 1e6 else NA_real_,
    total_length_km = NA_real_,
    point_count = if (!isPotWind) target else NA_integer_,
    centroid_x = mean(coords_matrix[, 1]),
    centroid_y = mean(coords_matrix[, 2]),
    bbox_xmin = bbox$xmin,
    bbox_ymin = bbox$ymin,
    bbox_xmax = bbox$xmax,
    bbox_ymax = bbox$ymax
  )
}

stats_dt <- rbindlist(stats, use.names = TRUE, fill = TRUE)

poly_total <- stats_dt[geometry == "polygons", sum(total_area_km2, na.rm = TRUE)]
line_total <- stats_dt[geometry == "lines", sum(total_length_km, na.rm = TRUE)]
point_total <- stats_dt[geometry == "points", sum(point_count, na.rm = TRUE)]

stats_dt[, area_share := NA_real_]
stats_dt[, length_share := NA_real_]
stats_dt[, count_share := NA_real_]
if (!is.na(poly_total) && poly_total > 0) {
  stats_dt[geometry == "polygons", area_share := total_area_km2 / poly_total]
}
if (!is.na(line_total) && line_total > 0) {
  stats_dt[geometry == "lines", length_share := total_length_km / line_total]
}
if (!is.na(point_total) && point_total > 0) {
  stats_dt[geometry == "points", count_share := point_count / point_total]
}

geometry_order <- c("polygons", "lines", "points")
stats_dt[, geometry := factor(geometry, levels = geometry_order)]
setorder(stats_dt, geometry, dataClass)
stats_dt[, geometry := as.character(geometry)]

stats_dt[, `:=`(
  total_area_km2 = round(total_area_km2, 3),
  total_length_km = round(total_length_km, 3),
  area_share = round(area_share, 4),
  length_share = round(length_share, 4),
  count_share = round(count_share, 4),
  centroid_x = round(centroid_x, 2),
  centroid_y = round(centroid_y, 2),
  bbox_xmin = round(bbox_xmin, 2),
  bbox_ymin = round(bbox_ymin, 2),
  bbox_xmax = round(bbox_xmax, 2),
  bbox_ymax = round(bbox_ymax, 2)
)]

stats_csv <- file.path(output_dir, "medium_aoi_optimal_stats.csv")
fwrite(stats_dt, stats_csv)

message("Synthetic dataset written to: ", gpkg_path)
message("Summary stats written to: ", stats_csv)
message("Study area total area (km^2): ", round(study_area_area_km2, 2))
message("Placement rectangle area (km^2): ", round(placement_area_km2, 2))
