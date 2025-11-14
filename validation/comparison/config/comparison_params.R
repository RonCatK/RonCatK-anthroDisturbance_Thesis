params <- list(
  anthroDisturbance_Generator = list(
    runInterval = 1,
    diffYears = "2010_2015",
    totalDisturbanceRate = NA_real_,
    saveInitialDisturbances = FALSE,
    saveCurrentDisturbances = TRUE,
    generatedDisturbanceAsRaster = FALSE,
    disturbanceRateRelatesToBufferedArea = TRUE,
    growthStepGenerating = 1,
    growthStepEnlargingPolys = 5,
    growthStepEnlargingLines = 5,
    siteSelectionAsDistributing = NA_character_,
    probabilityDisturbance = NULL,
    connectingBlockSize = 50L,
    seismicLineGrids = 1L,
    useClusterMethod = FALSE,
    runClusteringInParallel = FALSE,
    refinedStructure = FALSE,
    clusterDistance = 10,
    distanceNewLinesFactor = 1,
    useRoadsPackage = FALSE,
    maskWaterAndMountainsFromLines = TRUE,
    altitudeCut = 550,
    checkDisturbancesForBuffer = FALSE,
    disturbFirstYear = FALSE,
    .inputFolderFireLayer = file.path(project_root, "data", "synthetic", "fire"),
    outputsFolder = file.path(project_root, "outputs", "comparison"),
    runName = "VAL_2016_2020",
    .seed = list(
      anthroDisturbance_Generator = list(
        calculatingSize = 1L,
        calculatingRate = 2L,
        generatingDisturbances = 3L,
        updatingDisturbanceList = 4L
      )
    )
  )
)

cfg <- list(
  run_name = "VAL_2016_2020",
  times = list(start = 2016L, end = 2020L),
  output_path = file.path(project_root, "outputs", "comparison", "VAL_2016_2020"),
  metadata = list(comparison_analysis = TRUE, run_label = "Similarity_2016_2020")
)

list(cfg = cfg, params = params)
