# Disturbance Rate Validation

This folder consolidates the rate-verification workflows:

- `scenarios/` – structured harness with a status-tracked scenario matrix and the
  `run_rates_suite.R` entry point (CLI matches the system suite: `--csv`,
  `--scenario`, `--mode`, `--force`, `--dry-run`).
- Input staging now lives in `data/synthetic/rates/` (synthetic_inputs,
  synthetic_library, GeoPackages).
- Scenario outputs land in `outputs/rates/<run>` with packaged artefacts under
  `outputs/rates/packaged/`.
- Diagnostics and run logs are mirrored to `scratch/rates/`.

Launch via the central runner:

```bash
# inspect matrix selection
Rscript validation/runner.R --suite=rates --dry-run
# run a specific scenario row
Rscript validation/runner.R --suite=rates --scenario=rate_vector_baseline
```

You can still invoke individual scripts directly if required.
