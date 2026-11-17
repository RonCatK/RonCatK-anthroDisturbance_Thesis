# Workspace helpers

Utility scripts that support multiple suites live here so they can be versioned and documented in one place. Run each script from the project root.

## `prepare_data.R`

Downloads and stages required inputs for end-to-end runs (raw inputs, BEAD comparison archives, and optional UA/SA prebuild), and verifies `CHECKSUMS.txt` where available. Use `--verify-only` to skip downloads/DataPrep and only check checksums.

```bash
Rscript workspace/helpers/prepare_data.R --profile=all
```

Profiles: `raw`, `bead`, `synthetic`, `ua_sa`, or `all` (comma-separated). Use `--bead-2020-url` or `--bead-2020-archive` if the 2020 BEAD archive is not already present.

## `fetch_core_data.sh`

Downloads or unpacks the core data archive into `data/`, with optional checksum verification.

```bash
bash workspace/helpers/fetch_core_data.sh --url <CORE_ARCHIVE_URL>
```

## `package_core_data.sh`

Builds the core data archive from local inputs (curated `data/raw/disturbanceDT.csv`, BEAD archives, synthetic rates).

```bash
bash workspace/helpers/package_core_data.sh --out outputs/data/core_data_YYYYMMDD.tar.gz
```

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

Harvests the latest successful AD/QD run (per `outputs/traceability/suite_runs/adqd_validation_runs.csv`), snaps/cleans geometries, and builds a QGIS-ready GeoPackage + layer manifest under `scratch/qgis_packages/<timestamp>`. Optional buffers and sliver filters keep the export stable for comparison in QGIS/Arc. Use this when stakeholders need quick map packages without unpacking the full outputs tree.

```bash
Rscript workspace/helpers/export_qgis_package.R
```

## `run_all_module_tests.R`

Runs unit test suites for each module submodule and writes artifacts under `outputs/traceability/unit_tests/`.

```bash
Rscript workspace/helpers/run_all_module_tests.R
```

## `validate_runner_configs.R`

Validates every runnable YAML in `workspace/**/config/` against the runner schema and writes a report under `outputs/traceability/system_tests/`.

```bash
Rscript workspace/helpers/validate_runner_configs.R
```

## `snapshot_suite_evidence.R`

Copies long-running suite evidence (runs + metrics) into `docs/traceability/evidence/` so the traceability matrix can be generated without re-running suites.

```bash
Rscript workspace/helpers/snapshot_suite_evidence.R
```

## `generate_traceability_matrix.R`

Builds the traceability matrix from `docs/traceability/traceability_requirements.csv`, using the evidence and artifacts present locally.

```bash
Rscript workspace/helpers/generate_traceability_matrix.R
```

## `verify_disturbance_checksums.R`

Recomputes xxhash64 checksums for synthetic disturbance inputs and updates the local `CHECKSUMS.txt` when needed.

```bash
Rscript workspace/helpers/verify_disturbance_checksums.R data/synthetic/rates/synthetic_inputs
```
