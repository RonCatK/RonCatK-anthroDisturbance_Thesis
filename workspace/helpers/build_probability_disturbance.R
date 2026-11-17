# Build probabilityDisturbance YAML from baseline potential layers.
# This script reads the specified potential layers, computes area-weighted
# probabilities per Potential class, and writes a YAML snippet that can be
# plugged into UA configs under params$anthroDisturbance_Generator.
#
# Run from repo root:
#   Rscript workspace/helpers/build_probability_disturbance.R
#
# Output:
#   workspace/uncertainty/config/probabilityDisturbance.yaml

suppressPackageStartupMessages({
  library(terra)
  library(data.table)
  library(yaml)
})

# mapping: origin -> file path for potential layer
potential_layers <- list(
  seismicLines = "data/raw/oilGas_seismicLines_Seismic/NorthwestTerritories_15m_Disturb_Perturb_Line.shp",
  cutblocks    = "data/raw/forestry_potentialCutblocks_potentialCutblocks/NT_FORCOV.shp",
  oilGas       = "data/raw/oilGas_potentialOilGas_ITI/ECO_ITI_PR_OilandGasRights_GCS_NAD27.shp"
)

study_area_path <- "data/raw/NT1_BCR6.shp"
rtm_path        <- "data/raw/RTM.tif"
studyArea <- tryCatch(terra::vect(study_area_path), error = function(e) stop("studyArea not found: ", conditionMessage(e)))
rtm       <- tryCatch(terra::rast(rtm_path), error = function(e) stop("rasterToMatch not found: ", conditionMessage(e)))

compute_prob <- function(path) {
  if (!file.exists(path)) stop("Potential layer not found: ", path)
  v <- terra::vect(path)
  if (!"Potential" %in% names(v)) v$Potential <- 1L
  target_crs <- terra::crs(rtm)
  v <- tryCatch(terra::project(v, target_crs), error = function(...) v)
  sa <- tryCatch(terra::project(studyArea, target_crs), error = function(...) studyArea)
  # rasterize to avoid topology issues
  cell_area <- prod(terra::res(rtm))
  rst <- terra::rast(rtm)
  rst[] <- NA
  rst <- terra::rasterize(v, rst, field = "Potential", touches = TRUE, background = NA)
  rst <- terra::mask(rst, sa)
  fr <- terra::freq(rst, digits = 0)
  fr <- fr[!is.na(fr[, "value"]), , drop = FALSE]
  if (is.null(fr) || !nrow(fr)) return(NULL)
  dt <- data.table(Potential = fr[, "value"], area = fr[, "count"] * cell_area)
  dt <- dt[is.finite(area) & area > 0][, .(area = sum(area)), by = Potential]
  dt[, probPoly := area / sum(area)]
  dt[, .(Potential = as.integer(Potential), probPoly = as.numeric(probPoly))]
}

probs <- lapply(names(potential_layers), function(origin) {
  compute_prob(potential_layers[[origin]])
})
names(probs) <- names(potential_layers)

out <- list(
  anthroDisturbance_Generator = list(
    probabilityDisturbance = probs
  )
)

out_path <- file.path("workspace", "uncertainty", "config", "probabilityDisturbance.yaml")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(as.yaml(out), out_path)
message("Wrote ", out_path)
