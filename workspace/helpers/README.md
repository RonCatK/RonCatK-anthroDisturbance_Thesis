# Workspace helpers

Utility scripts that support multiple suites live here so they can be versioned and documented in one place. Run each script from the project root.

## `prebuild_dataprep.R`

Runs only the DataPrep modules (`anthroDisturbance_DataPrep`, `potentialResourcesNT_DataPrep`) and saves their outputs into `data/preprocessed/ua_sa`. This caches disturbance lists, study area, raster-to-match, and a local copy of the `DisturbanceDT` (with file:// URLs + `checksums.txt`) so UA/SA experiments can reuse deterministic inputs without touching the cloud sources.

```bash
Rscript workspace/helpers/prebuild_dataprep.R
```

## `build_probability_disturbance.R`

Rasterizes the baseline potential layers (seismic, cutblocks, oil/gas), computes area-weighted probabilities per Potential class, and emits `workspace/uncertainty/config/probabilityDisturbance.yaml`. Include the resulting YAML under `params$anthroDisturbance_Generator$probabilityDisturbance` when building UA/SA designs.

```bash
Rscript workspace/helpers/build_probability_disturbance.R
```

## `export_qgis_package.R`

Harvests the latest successful AD/QD run (per `workspace/adqd_validation/runs.csv`), snaps/cleans geometries, and builds a QGIS-ready GeoPackage + layer manifest under `scratch/qgis_packages/<timestamp>`. Optional buffers and sliver filters keep the export stable for comparison in QGIS/Arc. Use this when stakeholders need quick map packages without unpacking the full outputs tree.

```bash
Rscript workspace/helpers/export_qgis_package.R
```
