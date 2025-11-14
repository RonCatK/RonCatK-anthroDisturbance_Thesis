# System Test Harness

This folder contains everything needed to exercise the end-to-end disturbance
generator from the command line. It is intended to be
dropped into CI/CD workflows or used locally as the regression test harness.

## Contents

- `testing_runs.csv` – scenario matrix; each row describes a scenario, its
  parameterisation, latest status, and bookkeeping columns (`active`,
  `last_run_date`, `seed`, `replicates`, `params_file`, diagnostics, etc.). All
  system harness rows are prefixed with `system_` to distinguish them from the
  UA analysis scenarios that share the same CSV.
- `run_system_suite.R` – primary entry point for running one or more scenarios. Handles
  SpaDES setup, logging, and CSV updates while writing results to
  `outputs/validation/system/<run>` and logs to `scratch/validation/system`.
- `run_system_matrix.R` – convenience wrapper that reuses the runner to execute
  the currently selected scenarios from the matrix.

## Quick start

1. Ensure required inputs live in `data/raw/validation/system/` (or export the
   path via `VALIDATION_INPUT_ROOT`).
2. Review `testing_runs.csv`:
   - `status` reflects the last harness result (`SUCCESS`, `FAIL`, `PENDING`,
     `SKIP`, etc.).
   - `active` toggles whether a row is eligible for selection when `mode=respect`
     or when running the matrix script directly.
   - `last_run_date` is auto-populated (YYYY-MM-DD) whenever the scenario
     executes successfully or fails.
   - `seed` (optional numeric) fixes the random number stream for reproducible
     SpaDES runs; leave blank to accept the session default.
   - `replicates` defaults to `1`. Set `>1` to run a scenario multiple times;
     the runner appends `_run_##` to each replicate name, assigns unique seeds,
     and records all log/output paths as `;`-delimited lists in the CSV.
  - `params_file` (optional) points at an R script/RDS bundle with additional
    configuration or ready-to-use R objects. Most scenarios leave it blank now
    that the UA rows rely on the generator’s built-in masking logic.
   - Scenario ids coming from this suite use the `system_` prefix; UA rows keep
     their own `ua_` ids but still run through the same harness.
3. Launch scenarios using one of the scripts below.

## Running scenarios

```bash
# default mode (rerun PENDING + FAIL rows)
Rscript validation/runner.R --suite=system

# honour explicit active flags
Rscript validation/runner.R --suite=system --mode=respect

# run a subset
Rscript validation/runner.R --suite=system --scenario=system_medium_eccc_shp_clusters,system_medium_supplied_connecting_blocks

# inspect without executing
Rscript validation/runner.R --suite=system --dry-run

# override seed for reproducibility
Rscript validation/runner.R --suite=system --scenario=system_medium_eccc_shp_clusters --mode=respect
# (set the `seed` column in testing_runs.csv for reusable values)
```

Modes accepted by both scripts:

- `default` (default): sets `active = status ∈ {PENDING, FAIL, blank}` before
  selecting scenarios.
- `respect`: leaves the CSV untouched; only rows explicitly marked `active=TRUE`
  are considered.
- `all`: forces every scenario active for the run.

Other useful flags: `--force` (rerun even if `SUCCESS`/`SKIP`), `--csv=PATH`
(alternate matrix), and `--dry-run`.

## Running the full matrix

`run_system_matrix.R` is a lightweight wrapper around the runner:

```bash
Rscript validation/system/run_system_matrix.R            # mode=default
Rscript validation/system/run_system_matrix.R --mode=respect
Rscript validation/system/run_system_matrix.R --dry-run  # preview only
```

It selects the applicable rows (default: status `PENDING`/`FAIL`), invokes the
harness, and writes updated status/log columns back to `testing_runs.csv`.

## Outputs and logs

- Scenario outputs live in `outputs/validation/system/<run name>/`. Multi-replicate
  runs append `_run_##` to keep folders unique.
- Per-run logs land in `scratch/validation/system/scenario_logs/` (relative paths recorded in the
  CSV; multiple paths are separated with `;` when `replicates > 1`).
- All runs append to `scratch/validation/system/run_data.csv` with detailed parameters,
  diagnostics, run seed, and relative log/output paths.
- The resolved disturbance catalogue is staged at
  `scratch/validation/system/disturbanceDT_local.csv` and reused by subsequent runs.

## Fire module integration

The harness understands the `fire_generation` column in `testing_runs.csv`:

- Provide the SpaDES fire module name(s) to load (e.g., `historicFires`), or use
  the special `provided_mask(...)` helper to synthesise a quick raster mask in place.
- Pass key/value pairs inside the parentheses to configure either the module parameters
  or the provided-mask generator (`provided_mask(pattern=checker;burnModulo=5)`).
- When pointing at precomputed fire layers, include a `dir=` (or `path=`) argument; the
  harness resolves it relative to the project root and forwards it to the simulation
  as the `.inputFolderFireLayer`.
- Injected fire modules are noted in `scratch/run_data.csv` and the scenario
  notes for traceability.

## UA analysis rows

`testing_runs.csv` now carries two UA-oriented scenarios (`ua_cluster_cb5`,
`ua_cluster_cb12`) that reuse this harness but point at the precomputed
`validation/system/DistRates.csv`. Each is configured for 10 replicates to ensure
analysis outputs are preserved per run (`_run_##`). Invoke them with the same CLI
flags shown above (e.g., `--scenario=ua_cluster_cb5`) when you want to refresh the
UA comparison products alongside the system tests.

This setup keeps large data assets out of version control while providing a
repeatable, scriptable test harness for system-level verification.
