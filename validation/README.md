# Validation Suites

The `validation/` subtree gathers every verification workflow that supports the
anthroDisturbance thesis: regression-style system replays, synthetic
disturbance-rate checks, and uncertainty-analysis replicates. Everything is
orchestrated through a single command-line entry point while preserving the
original per-suite scripts so legacy automation keeps working.

## Architecture at a Glance

- `validation/runner.R` routes `--suite=system|comparison|rates|ua` requests to the
  appropriate harness. Any additional CLI flags are forwarded unchanged to the
  suite runner.
- Each suite keeps its own scripts, helper functions, and documentation under a
  dedicated subdirectory (`system/`, `comparison/`, `rates/`, `ua/`). The UA v2
  folder now hosts metrics + plotting helpers that post-process system-suite UA
  runs.
- Inputs are staged under `data/raw/validation/<suite>/` (plus
  `data/study_area/` for common spatial assets). Synthetic data helpers fall
  back to `data/synthetic/` when available.
- Outputs land in `outputs/validation/<suite>/`. Logs, diagnostics, and run
  metadata mirror to `scratch/validation/<suite>/`.
- Environment variables such as `VALIDATION_INPUT_ROOT`, `VALIDATION_SYNTHETIC_ROOT`,
  `PRE_DOWNLOADED_PATH`, and `UA_INPUTS` let you point the harnesses at alternate
  data roots without editing scripts.

## Running the Suites

```bash
# Show available suites and default behaviour
Rscript validation/runner.R --help

# System regression harness (reruns FAILED + PENDING rows by default)
Rscript validation/runner.R --suite=system

# Synthetic disturbance-rate scenarios (pass a matrix or individual scenario IDs)
Rscript validation/runner.R --suite=rates --scenarios=validation/rates/scenarios/scenarios.csv

# Uncertainty analysis replicates
Rscript validation/runner.R --suite=ua --replicates=10 --start-year=2020 --end-year=2030

# UA v2 scenario rows (execute via system suite, then harvest metrics)
Rscript validation/runner.R --suite=system --scenario=ua_cluster_cb5,ua_cluster_cb12
Rscript validation/ua_v2/ua_metrics.R --scenario=ua_cluster_cb5,ua_cluster_cb12
```

Every suite exposes a `--dry-run` flag to inspect the workload before execution.
Consult the per-suite runner (`validation/system/run_system_suite.R`,
`validation/rates/scenarios/run_rates_suite.R`, `validation/ua/run_ua_replicates.R`)
for the complete option set.

## Suite Reference

### System Verification (`validation/system/`)

- **Purpose**: Replay curated SpaDES scenarios end-to-end to catch regressions in
  the disturbance generator.
- **Key assets**:
  - `testing_runs.csv` – the scenario matrix with `status`, `active`, `seed`,
    and bookkeeping columns (`last_run_date`, notes, log paths).
  - `run_system_suite.R` – loads scenarios from the matrix, manages SpaDES setup,
    writes status updates, and captures reproducibility data.
  - `run_system_matrix.R` – convenience wrapper that simply reruns the eligible
    rows from the matrix.
- **Typical workflow**:
  1. Ensure each scenario’s required inputs exist under
     `data/raw/validation/system/` (or export `VALIDATION_INPUT_ROOT`).
  2. Flag rows to execute (`status` in `{PENDING, FAIL}` or `active=TRUE` when
     running in `mode=respect`).
  3. Launch via the central runner or directly with
     `Rscript validation/system/run_system_suite.R --dry-run`.
- **Outputs**: Scenario results in `outputs/validation/system/<run_name>/`,
  per-run logs in `scratch/validation/system/`, and a cumulative ledger in
  `scratch/validation/system/run_data.csv`.
- **Notable flags**: `--mode=default|respect|all`, `--scenario=id1,id2`,
  `--force`, `--csv=PATH`, `--dry-run`.

### Disturbance-Rate Verification (`validation/rates/`)

- **Purpose**: Validate vector and raster disturbance pipelines using synthetic
  AOI inputs that mirror the module’s documented workflow.
- **Key assets**:
  - `scenarios/scenarios.csv` – matrix describing rates, temporal settings,
    optional fire modules, packaging toggles, and status columns.
  - `scenarios/run_rates_suite.R` – builds synthetic libraries (if missing),
    assembles SpaDES inputs, executes each scenario, and optionally packages
    outputs using `package_outputs.R`.
  - `package_outputs.R` – standalone helper that converts generated shapefiles
    into GeoPackages with `_newOnly` layers for diagnostics.
- **Typical workflow**:
  1. Generate synthetic GeoPackages with `scripts/synthetic_data_aoi.R` or stage
     inputs under `data/raw/validation/rates/`.
  2. Review or edit `scenarios.csv` to select runs (`status`, `active`, notes).
  3. Execute through the central runner or by calling
     `Rscript validation/rates/scenarios/run_rates_suite.R`.
- **Outputs**: SpaDES outputs in `outputs/validation/rates/<run_name>/`,
  packaged artefacts in `outputs/validation/rates/packaged/`, diagnostics under
  `scratch/validation/rates/`, and scenario status updates written back to the
  matrix.
- **Notable flags**: `--mode=default|respect|all`, `--scenario=id`,
  `--csv=PATH`, `--force`, `--package={yes|no}`, `--dry-run`.

### Uncertainty Analysis v2 (`validation/ua_v2/`)

- **Purpose**: Post-process the UA scenario rows that now run through the system
  suite (e.g., `ua_cluster_cb5`, `ua_cluster_cb12`) and turn the resulting
  multi-replicate outputs into metrics + figures.
- **Key assets**:
  - `ua_runs.csv` – preserved as a reference copy of the historical scenario
    grid (parameter combinations, notes, replicate counts). Use it as a guide
    when editing the UA rows that now live inside `validation/system/testing_runs.csv`.
  - `ua_metrics.R` – scans the system suite’s `output_path` column, computes
    per-replicate metrics from the exported disturbance shapefiles, summarizes
    the distribution, and records `metrics_raw.csv`, `metrics_summary.csv`, and
    `replicate_index.csv` under `outputs/validation/ua_v2/<scenario>/results/`.
  - `ua_figures.R` – lightweight CLI that turns `metrics_summary.csv` into
    uncertainty-band plots (`figures/uq_*.png`).
  - `DistRates.csv` – cached ECCC disturbance rates that UA rows reference via
    the system matrix.
- **Workflow**:
  1. Activate the UA rows (those whose `scenario_id` starts with `ua_`) in
     `validation/system/testing_runs.csv`, then rerun them through the system
     harness (e.g., `Rscript validation/runner.R --suite=system --scenario=ua_cluster_cb5`).
  2. Execute `Rscript validation/ua_v2/ua_metrics.R --scenario=ua_cluster_cb5`
     (add more comma-separated scenario IDs as needed). The script inspects the
     `output_path` column, processes each replicate folder, and writes results
     under `outputs/validation/ua_v2/`.
  3. Figures are generated automatically when `ua_metrics.R` finishes (or rerun
     manually with `Rscript validation/ua_v2/ua_figures.R --metrics=...`).
- **Outputs**: `outputs/validation/ua_v2/<scenario>/results/metrics_raw.csv`,
  `metrics_summary.csv`, `replicate_index.csv`, plus `figures/uq_*.png`. All
  files reference the original system output/log paths for traceability.
- **Notable flags (ua_metrics.R)**: `--testing-csv=PATH`,
  `--scenario=id1,id2`, `--results-root=DIR`, `--figures=true|false`.

### Uncertainty Analysis (legacy) (`validation/ua/`)

- **Purpose**: Run the anthroDisturbance uncertainty-analysis scenarios (vector
  vs raster, masking, fire gating, etc.) across multiple replicates and extract
  interpretable metrics.
- **Key assets**:
  - `run_ua_replicates.R` – main harness that materialises inputs, discovers the
    module path, prepares SpaDES parameters, and launches scenarios × replicates
    using `future.apply`.
  - `ua_inputs.R` – default input scaffold that loads the study area, raster to
    match, mask layers, disturbance catalog, and fallback `DisturbanceRate`.
    Override the file path through the `UA_INPUTS` environment variable or point
    to an `.Rds` file containing the required objects.
  - `ua_scenarios.R` – defines the scenario grid (`scenario_id`, label, parameter
    list, required objects). Scenarios marked `skip=TRUE` are omitted automatically.
  - `ua_metrics.R` – extracts yearly sector totals, cumulative area/length
    metrics, and summary statistics across replicates.
  - `ua_utils.R` – shared helpers (module discovery, seeding strategy, geometry
    utilities).
  - `precompute_wind_layers.R` – optional helper that crops the national wind
    potential MIF tiles to the UA study area and materialises a local shapefile
    (`data/raw/validation/ua/wind/`) so runs never rely on remote downloads.
- **Inputs**: Expects objects named `studyArea`, `rasterToMatch`,
  `disturbanceParameters`, `disturbanceDT`, `disturbanceList`, and optionally
  `DisturbanceRate`, `rstCurrentBurn`, `featuresToAvoid`, `DEM`. Missing objects
  are backfilled when possible (e.g., reading packaged CSV/QS assets).
- **Outputs**: Metrics written to
  `outputs/validation/ua/results/metrics_raw.csv`,
  `metrics_summary.csv`, and `replicate_index.csv`; run artefacts (figures, logs)
  collect under `outputs/validation/ua/` and `scratch/validation/ua/`.
- **Notable flags**: `--replicates`, `--start-year`, `--end-year`,
  `--parallel=yes|no`, `--skip-scenarios=<regex>`, `--module-path=PATH`,
  `--dry-run`.
- **Environment toggles**:
  - `UA_INPUTS` – alternate .R / .Rds supplying input objects.
  - `VALIDATION_SYNTHETIC_ROOT`, `VALIDATION_INPUT_ROOT`, `PRE_DOWNLOADED_PATH`
    – override data directories.
  - `UA_EXCLUDE_SECTORS` – comma-separated list of sectors to drop from
    `disturbanceDT`.
  - `UA_FAST`, `UA_MINIMAL` – shortcuts that reduce per-run workload (e.g.,
    annual disturbance interval, load only the generator module).
  - `UA_RUN_ACCEPTANCE` – assert the expected output files exist.

## Data and Scratch Layout

- `data/raw/validation/<suite>/` – canonical location for large inputs that are
  not committed to the repository.
- `data/synthetic/` – cached synthetic AOI materials used by the rate suite.
- `outputs/validation/<suite>/` – spaDES outputs, packaged artefacts, and per-run
  results (UA metrics, rate GeoPackages, system run folders).
- `scratch/validation/<suite>/` – logs, intermediate CSVs, and diagnostics that
  support traceability without polluting the outputs directory.

All scripts avoid destructive edits outside these roots and honour the
`VALIDATION_*` environment variables before falling back to repository defaults.

## Extending and Maintenance Tips

- Add new system scenarios by appending rows to `testing_runs.csv` and seeding
  any required columns (`moduleOverrides`, `fire_generation`, `notes`).
- Introduce new rate checks by cloning an existing row in
  `validation/rates/scenarios/scenarios.csv` and pointing to the desired
  disturbance-rate CSV or fire module combination.
- Use `validation/ua_v2/ua_runs.csv` as a reference when editing the UA rows
  inside `validation/system/testing_runs.csv` (those whose ids start with `ua_`);
  copy the desired parameter combination, set `status=PENDING`, and let the
  system suite execute them.
- For legacy runs, extend `ua_scenarios.R` with a new entry or migrate the
  scenario into the v2 matrix.
  `id`, customise `psim`, and declare required objects so the harness can ensure
  prerequisites exist.
- When large data assets change, refresh the corresponding raw inputs (e.g.,
  rerun the synthetic AOI generator) before re-running the suite.
- Shared helpers that stage disturbance sources and reusable study-area rasters
  live under `validation/helpers/`; update `disturbance_data.R` when the module
  schema or local asset locations change.
- Capture reproducibility context by committing updated matrices (`testing_runs.csv`,
  `scenarios.csv`) and preserving the `scratch/.../run_data.csv` outputs outside
  version control if you need provenance records.
