suppressPackageStartupMessages({
  requireNamespace("data.table")
  requireNamespace("glue")
})

# build_scenario_grid: toggle grid for anthroDisturbance_Generator
# Returns a data.table with: scenario_id, label, psim(list), requires(list)
build_scenario_grid <- function() {
  scenarios <- list(
    list(
      id = "S1",
      label = "Vector | Exhausting | No Mask | ECCC rates",
      psim = list(
        generatedDisturbanceAsRaster = FALSE,
        siteSelectionAsDistributing = NA_character_,
        maskWaterAndMountainsFromLines = FALSE,
        useRoadsPackage = FALSE,
        runInterval = 1L,
        disturbFirstYear = TRUE,
        growthStepEnlargingPolys = 5L,
        growthStepEnlargingLines = 5L,
        growthStepGenerating = 1L,
        saveInitialDisturbances = FALSE,
        saveCurrentDisturbances = FALSE,
        disturbanceRateRelatesToBufferedArea = FALSE,
        seismicLineGrids = 1L,
        connectingBlockSize = 50L,
        outputsFolder = tempdir()
      ),
      requires = c("studyArea","rasterToMatch","disturbanceParameters",
                   "disturbanceDT","DisturbanceRate"),
      skip = FALSE
    ),
    list(
      id = "S2",
      label = "Vector | Distributing=all | Mask ON | Fire gating | ECCC rates",
      psim = list(
        generatedDisturbanceAsRaster = FALSE,
        siteSelectionAsDistributing = c("windTurbines", "settlements", "oilGas",
                                        "cutblocks", "mining", "seismicLines"),
        maskWaterAndMountainsFromLines = TRUE,
        useRoadsPackage = FALSE,
        runInterval = 1L,
        disturbFirstYear = TRUE,
        growthStepEnlargingPolys = 5L,
        growthStepEnlargingLines = 5L,
        growthStepGenerating = 1L,
        saveInitialDisturbances = FALSE,
        saveCurrentDisturbances = FALSE,
        disturbanceRateRelatesToBufferedArea = FALSE,
        seismicLineGrids = 1L,
        connectingBlockSize = 50L,
        outputsFolder = tempdir()
      ),
      requires = c("studyArea","rasterToMatch","disturbanceParameters",
                   "disturbanceDT","DisturbanceRate","rstCurrentBurn","featuresToAvoid"),
      skip = FALSE
    ),
    list(
      id = "S3",
      label = "Raster | Exhausting | No Mask | ECCC rates",
      psim = list(
        generatedDisturbanceAsRaster = TRUE,
        siteSelectionAsDistributing = NA_character_,
        maskWaterAndMountainsFromLines = FALSE,
        useRoadsPackage = FALSE,
        runInterval = 1L,
        disturbFirstYear = TRUE,
        growthStepEnlargingPolys = 5L,
        growthStepEnlargingLines = 5L,
        growthStepGenerating = 1L,
        saveInitialDisturbances = FALSE,
        saveCurrentDisturbances = FALSE,
        disturbanceRateRelatesToBufferedArea = FALSE,
        seismicLineGrids = 1L,
        connectingBlockSize = 50L,
        outputsFolder = tempdir()
      ),
      requires = c("studyArea","rasterToMatch","disturbanceParameters",
                   "disturbanceDT","DisturbanceRate"),
      skip = FALSE
    ),
    list(
      id = "S4",
      label = "Vector | Exhausting | Mask ON | No Fire | ECCC rates",
      psim = list(
        generatedDisturbanceAsRaster = FALSE,
        siteSelectionAsDistributing = NA_character_,
        maskWaterAndMountainsFromLines = TRUE,
        useRoadsPackage = FALSE,
        runInterval = 1L,
        disturbFirstYear = TRUE,
        growthStepEnlargingPolys = 5L,
        growthStepEnlargingLines = 5L,
        growthStepGenerating = 1L,
        saveInitialDisturbances = FALSE,
        saveCurrentDisturbances = FALSE,
        disturbanceRateRelatesToBufferedArea = FALSE,
        seismicLineGrids = 1L,
        connectingBlockSize = 50L,
        outputsFolder = tempdir()
      ),
      requires = c("studyArea","rasterToMatch","disturbanceParameters",
                   "disturbanceDT","DisturbanceRate","featuresToAvoid"),
      skip = FALSE
    ),
    list(
      id = "SR",
      label = "Vector | Roads package ON (skip for now)",
      psim = list(
        generatedDisturbanceAsRaster = FALSE,
        siteSelectionAsDistributing = NA_character_,
        maskWaterAndMountainsFromLines = FALSE,
        useRoadsPackage = TRUE,
        runInterval = 1L,
        disturbFirstYear = TRUE,
        growthStepEnlargingPolys = 5L,
        growthStepEnlargingLines = 5L,
        growthStepGenerating = 1L,
        saveInitialDisturbances = FALSE,
        saveCurrentDisturbances = FALSE,
        disturbanceRateRelatesToBufferedArea = FALSE,
        seismicLineGrids = 1L,
        connectingBlockSize = 50L,
        outputsFolder = tempdir()
      ),
      requires = c("studyArea","rasterToMatch","disturbanceParameters",
                   "disturbanceDT","DisturbanceRate","DEM"),
      skip = TRUE
    )
  )

  dt <- data.table::rbindlist(lapply(scenarios, function(sc) {
    data.table::data.table(
      scenario_id = sc$id,
      label = sc$label,
      psim = list(sc$psim),
      requires = list(sc$requires),
      skip = sc$skip
    )
  }), use.names = TRUE, fill = TRUE)

  dt[, psim := lapply(seq_len(.N), function(i) {
    p <- psim[[i]]
    p$runName <- scenario_id[i]
    p
  })]

  skipped <- dt[isTRUE(skip)]
  if (nrow(skipped)) {
    for (i in seq_len(nrow(skipped))) {
      message(glue::glue("[UA] Scenario {skipped$scenario_id[i]} marked skip=TRUE; omitting from run list."))
    }
  }

  dt[!dt$skip, .(scenario_id, label, psim, requires)]
}
