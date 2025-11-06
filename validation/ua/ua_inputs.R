## Self-contained inputs for UA runs (medium AOI)
requireNamespace("terra")
requireNamespace("data.table")
requireNamespace("reproducible")

# Study area from local shapefile
sa_path <- file.path(getwd(), "data", "study_area", "aoi_southwest_NWT.shp")
stopifnot(!is.null(sa_path) && file.exists(sa_path))
studyArea <- terra::vect(sa_path)

# Raster to match (simple mask)
rtm <- terra::rast(extent = terra::ext(studyArea), resolution = 250, crs = terra::crs(studyArea))
rtm[] <- 0
rasterToMatch <- terra::mask(rtm, studyArea)

study_hash <- reproducible::.robustDigest(studyArea)
data_root <- file.path(getwd(), "data", "raw")

align_to_rtm <- function(x, method = "bilinear") {
  if (!terra::compareGeom(x, rasterToMatch, stopOnError = FALSE)) {
    x <- terra::project(x, rasterToMatch, method = method)
  }
  terra::mask(terra::resample(x, rasterToMatch, method = method), studyArea)
}

find_processed_raster <- function(prefix) {
  pat <- paste0("^", prefix, "_", study_hash, "(\\..+)?$")
  hit <- list.files(data_root, pattern = pat, full.names = TRUE)
  if (!length(hit)) {
    hit <- list.files(data_root, pattern = paste0("^", prefix, "_"), full.names = TRUE)
  }
  hit[1]
}

dem_path <- find_processed_raster("DEM")
if (!is.na(dem_path) && nzchar(dem_path)) {
  DEM <- align_to_rtm(terra::rast(dem_path), method = "bilinear")
  names(DEM) <- "elevation"
}

water_path <- find_processed_raster("water")
wet_path <- find_processed_raster("wet")
if (!is.na(water_path) || !is.na(wet_path) || exists("DEM")) {
  features_template <- terra::rast(rasterToMatch)
  features_template[] <- NA

  if (exists("DEM")) {
    high_ground <- DEM >= 550
    high_ground[is.na(high_ground)] <- 0
    features_template[high_ground == 1] <- 1
  }
  if (!is.na(wet_path) && nzchar(wet_path)) {
    wet <- align_to_rtm(terra::rast(wet_path), method = "near")
    features_template[wet > 0.8] <- 1
  }
  if (!is.na(water_path) && nzchar(water_path)) {
    water <- align_to_rtm(terra::rast(water_path), method = "near")
    features_template[water > 0.8] <- 1
  }
  features_template[is.na(rasterToMatch)] <- 1
  featuresToAvoid <- features_template
}

# Locate module directories for packaged inputs
module_candidates <- c(
  file.path(getwd(), "modules"),
  file.path(getwd(), "modules_Testing")
)
module_dir <- NULL
for (cand in module_candidates) {
  if (dir.exists(file.path(cand, "anthroDisturbance_Generator"))) {
    module_dir <- cand
    break
  }
}
stopifnot(!is.null(module_dir))
gen_data <- file.path(module_dir, "anthroDisturbance_Generator", "data")

# Disturbance parameters from packaged txt (optional but recommended)
params_file <- file.path(gen_data, "paramsGeneral.txt")
if (file.exists(params_file)) {
  disturbanceParameters <- data.table::data.table(dget(params_file))
}

# Disturbance catalog
dp_csv <- file.path(module_dir, "anthroDisturbance_DataPrep", "data", "disturbanceDT.csv")
if (file.exists(dp_csv)) {
  disturbanceDT <- data.table::fread(dp_csv)
}

# Provide a simple default DisturbanceRate to avoid ECCC fetch
if (exists("disturbanceParameters")) {
  keyCols <- c("dataName", "dataClass", "disturbanceType", "disturbanceOrigin")
  DisturbanceRate <- unique(disturbanceParameters[, ..keyCols])
  DisturbanceRate[, disturbanceRate := 0.2]
}

invisible(TRUE)
