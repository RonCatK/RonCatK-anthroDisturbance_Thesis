# Synthetic Disturbance Rate Tests

This harness mirrors the **anthroDisturbance_Generator** module’s documented
workflow so that synthetic validations exercise the same objects, event order,
and data dependencies described in `module overview.txt`.

## Goals
- Build inputs through the same SpaDES modules (`anthroDisturbance_DataPrep`
  plus any fire modules you specify) that the generator expects.
- Keep the disturbance list/potential layers in their SpaDES-native formats
  (`sim$disturbanceList`, `sim$disturbanceParameters`, `DisturbanceRate`).
- Permit running either vector (`generateDisturbancesShp`) or raster
  (`generateDisturbances`) pipelines.
- Capture diagnostics and packaged outputs with consistent run metadata.

## Layout
- `scenarios.csv` — scenario matrix with status tracking. Each row includes the
  run definition (rate, temporal settings, mode, per-class rate file, optional fire module, and
  packaging toggles) plus `status`, `active`, and bookkeeping columns updated by
  the suite after every execution.
- `run_rates_suite.R` — main entry point. Builds/refreshes synthetic library
  assets, assembles SpaDES inputs, executes each scenario, and optionally
  packages outputs.
- `data/raw/validation/rates/` — staging area containing `synthetic_inputs/`,
  `synthetic_library/`, and the generated GeoPackages used by the suite.
- `outputs/validation/rates/` — scenario results (SpaDES outputs) with
  packaged artefacts under `outputs/validation/rates/packaged/`.
- `scratch/validation/rates/` — diagnostics and per-run log files.

## Running
```bash
Rscript validation/runner.R --suite=rates \
  --scenarios=validation/rates/scenarios/scenarios.csv
```

### What happens
1. Ensure the medium AOI synthetic GeoPackage/stats exist (re-using
   `scripts/synthetic_data_aoi.R` when missing).
2. Materialise inputs into `data/raw/validation/rates/synthetic_inputs/`
   (one Shapefile per layer + zipped companion), harmonising sector assignments as described
   in the module overview (pipelines reside under `oilGas`, power connectors
   under `Energy`).
3. For each scenario:
   - Prepare a SpaDES project configuration with start/end years and
     `runInterval`.
   - Construct `DisturbanceRate` (from file or derived proportions) and wire it
     together with `disturbanceDT`.
   - If `use_fire=TRUE`, prepend the fire module names listed in the `fire_module`
     column so those modules can produce `rstCurrentBurn`.
   - Trigger `SpaDES.core::simInitAndSpades`, capture diagnostics, and write a
     structured run summary inside `outputs/validation/rates/<runName>/`.
   - When `package=TRUE`, call the root `package_outputs.R` helper so the run
     also ships GeoPackage bundles.

## Extending
- To add bespoke disturbance proportions, drop a CSV with the generator’s
  schema (see `makeScenarioRates()` in `run_rates_suite.R`) and reference it in
  the `disturbanceRateFile` column.
- Additional modules can be inserted by editing `modulesBase` / scenario metadata
  (e.g., populate the `fire_module` column to load historic fire simulators alongside
  the disturbance generator).
- Helper functions that convert GeoPackage layers to Shapefiles live at the top
  of `run_rates_suite.R` so they can be sourced from other test harnesses if
  needed.

## Relationship to legacy scripts
The original `validation/rates/run_rates_legacy.R` remains available for
quick rate sweeps. This harness lives under `validation/rates` so both
approaches can operate without trampling each other. Whenever the synthetic
GeoPackage schema changes, re-run the generator script first so both workflows
stay in sync.
