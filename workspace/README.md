# Workspace runner

`workspace/runner.R` is the generic entry point for all thesis suites (`system/`, `rates/`, `sensitivity/`, `uncertainty/`, `adqd_validation/`, `verification/`, etc.). Every suite contributes runnable configs (YAML / JSON / R list) that share the same schema so they can reuse logging, seeding, inputs, and output conventions.

## Usage

```bash
# Run a single config
Rscript workspace/runner.R workspace/system/config/system_cluster_dense_connectors.yaml

# Run from another directory (set project root explicitly)
RUNNER_PROJECT_ROOT=/path/to/repo Rscript workspace/runner.R /tmp/my_config.yaml
```

The script enforces one positional argument (the config path). It auto-detects the project root by locating itself one directory up, but you can override this via the `runner.project_root` option inside R or the `RUNNER_PROJECT_ROOT` environment variable before launching.

## Config schema

All configs support the following structure:

```yaml
suite: system                # suite label (determines runs.csv + output folder)
run_name: system_example     # short identifier (used for outputs/logs)
description: optional text
config_version: "1"          # optional
modules:
  - anthroDisturbance_DataPrep
  - anthroDisturbance_Generator
times:
  start: 2015
  end: 2025
  timeunit: year
paths:
  input_root: data/preprocessed/example
  output_root: outputs
  scratch_root: scratch
  module_path: modules
data_profile: preprocessed
metadata:
  tag: anything you want
params:
  anthroDisturbance_Generator:
    totalDisturbanceRate: 2.5
n_reps: 5
seed_base: 12345             # or explicit `seeds` vector
input_behaviour:
  allow_download_if_missing: false
```

- `suite` drives folder conventions (`outputs/<suite>/<run_name>/rep_*`, `scratch/<suite>/<run_name>/rep_*.log`, `workspace/<suite>/runs.csv`).
- `paths` can be omitted to fall back to `outputs`, `scratch`, and `modules` in the repo root.
- `metadata` is copied verbatim into `runs.csv` so you can track additional labels.
- `seed_base` expands to a deterministic seed per replicate (`seed_base + rep_idx - 1`). Alternatively provide a `seeds` vector matching `n_reps`.

See the suite-specific READMEs under `workspace/<suite>/` for concrete config examples and helper scripts that auto-generate configs.

## Behaviour highlights

- **Validation** – the runner checks required packages up front and aborts early with a clear error if any are missing.
- **Thread control** – it caps BLAS/OMP threads per job by inspecting `RUNNER_CONCURRENT_JOBS` and `RUNNER_CORES_PER_JOB`, ensuring multiple `run_system.sh`/`run_rates_suite.R` invocations can coexist without oversubscribing CPUs.
- **Logging** – console + message output for each replicate is piped to `scratch/<suite>/<run_name>/rep_xxx.log`. `runs.csv` receives one row per replicate (timestamp, suite, run, seed, config file, status, optional error message).
- **Input hygiene** – when the `DisturbanceDT` references local files the runner rewrites URLs as `file://` paths so modules avoid re-downloading data already staged by `prebuild_dataprep.R`.
- **Failures** – if a config errors before the first replicate finishes, the runner still appends an `error` row to the suite’s `runs.csv` with the normalized output/log paths so you can inspect artifacts manually.

## Adding new suites/configs

1. Create a directory under `workspace/<suite_name>/` with a README that describes the purpose of the suite (see existing folders for patterns).
2. Place configs under `workspace/<suite_name>/config/`.
3. Run them through the runner or via helper wrappers. Each suite can maintain its own `runs.csv`, packaging scripts, and metrics collectors while relying on the shared runner behaviour described above.
