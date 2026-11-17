# Requirements & Traceability

This folder holds the CI-updated traceability matrix used for the thesis verification narrative.
The submission snapshot (“at a glance”) lives at `docs/traceability_matrix.csv`.

## Files

- `docs/traceability/traceability_requirements.csv` – the editable requirements spec (source of truth).
- `docs/traceability/traceability_matrix.csv` – the generated matrix (ID, tier, requirement, evidence, acceptance, metric, status, cadence, as_of).
- `docs/traceability/traceability_matrix.md` – the human-readable matrix.

## How it works

1. CI (or a local run) produces evidence artifacts (unit test JUnit XML, coverage text, config validation report) under `outputs/traceability/`, while long-running suite snapshots live in `docs/traceability/evidence/`.
2. `workspace/helpers/generate_traceability_matrix.R` reads `docs/traceability/traceability_requirements.csv` and fills Evidence + Metric + Status using those artifacts.
3. CI writes `docs/traceability/traceability_matrix.md` for human-readable viewing and commits the updated outputs on `main`.

## Evidence reference format

In `docs/traceability/traceability_requirements.csv`, `evidence_ref` and `metric_ref` accept one or more entries separated by `;`:

- `junit:<path>` – parses JUnit XML (`tests.xml`) and reports pass/fail + counts.
- `coverage_txt:<path>` – parses a `coverage.txt` percentage.
- `junit_bundle:<path1>,<path2>,...` – aggregates multiple junit files/dirs into a single pass-rate summary.
- `coverage_bundle:<path1>,<path2>,...` – reports the minimum coverage across multiple `coverage.txt` files.
- `config_validation:<path>` – parses the config validation CSV report.
- `runs_csv:<path>[?include_run_name_regex=...&exclude_run_name_regex=...]` – parses a suite `runs.csv` and reports latest-status success rate per `(run_name, replicate)`.
- `adqd_summary_dir:<dir>` – finds the latest `*__adqd_summary.csv` under an ADQD metrics folder and reports mean agreement/F1/etc.
- `adqd_summary_csv:<path>` – parses a specific `*__adqd_summary.csv` and reports mean agreement/F1/etc.
- `ua_key_metrics_csv:<path>` – parses `ua_key_metrics_summary.csv` and reports key medians.
- `pct_disturbance_summary_csv:<path>` – parses a system-suite PercentageDisturbances summary and reports robust absolute deltas.
- `morris_qc_csv:<path>` – parses `outputs/sensitivity/results/morris_qc_summary.csv` and reports kept/no-op/confounded pair counts.
- `file:<path>` – simple presence check.

Optional query params (examples):
- `junit_bundle:... ?include_regex=calculateRate|calculateSize` – filter junit files by basename regex.
- `coverage_txt:... ?min=70` – set a minimum coverage % for status evaluation.
- `coverage_bundle:... ?min=70` – same as above for bundles.
- `runs_csv:... ?min_success_rate=100` – set a minimum success rate % for status evaluation.
- `pct_disturbance_summary_csv:... ?median_abs_pct_diff_max=5&p95_abs_pct_diff_max=500` – set robust tolerances for system deltas.

## Status column

`Status` is computed from the acceptance rules implied by each `metric_ref` entry (e.g., junit pass/fail, min coverage threshold, success-rate threshold). This keeps “artifact parsed” separate from “requirement met.”

## as_of column

`as_of` is the repo commit date (from `git log -1 --format=%cs`) for the revision the matrix was generated against. This avoids churn in CI/scheduled runs when evidence hasn’t changed.

## Run locally

```bash
# (optional) generate unit-test artifacts for the three module submodules
Rscript workspace/helpers/run_all_module_tests.R

# generate system-test artifact for runner config parsing
Rscript workspace/helpers/validate_runner_configs.R

# (optional) snapshot long-running suite evidence (runs + key metric summaries)
# into docs/traceability/evidence/ so CI can include it without re-running the suites
Rscript workspace/helpers/snapshot_suite_evidence.R

# generate the matrix outputs
Rscript workspace/helpers/generate_traceability_matrix.R
```

## CI

The workflow in `.github/workflows/traceability-matrix.yml` runs the same scripts, uploads the evidence bundle as a workflow artifact, and commits the updated matrix outputs on the `main` branch.
It also runs after the `Module CI Aggregator` workflow completes, so module CI dispatches can refresh the matrix.

Note: `workspace/helpers/validate_runner_configs.R` validates runner configs (YAML that contain runner keys like `suite`, `modules`, `times`) and marks other YAML files in `workspace/**/config` as skipped.
