# AD/QD validation suite

This suite reproduces the anthropogenic disturbance (AD) and quadratic disagreement (QD) verification runs that fed the thesis analysis. Two canned runner configs live under `config/`:

- `adqd_verification.yaml` & `adqd_verification_params.R` – 2010–2015 baseline rerun that proves the generator can recreate the calibration period.
- `adqd_holdout.yaml` & `adqd_holdout_params.R` – 2015–2020 hold-out interval using the module-derived growth rates.

`workspace/runner.R` sews these configs into the unified logging/outputs conventions. Successful runs drop results under `outputs/adqd_validation/<RUN_NAME>/rep_xxx` with logs under `scratch/adqd_validation/<RUN_NAME>/rep_xxx.log`. The aggregated status table lives in `workspace/adqd_validation/runs.csv`.

## Running the scenarios

```bash
# Verification (2010–2015)
Rscript workspace/runner.R workspace/adqd_validation/config/adqd_verification.yaml

# Hold-out (2015–2020)
Rscript workspace/runner.R workspace/adqd_validation/config/adqd_holdout.yaml
```

The configs inherit inputs from `data/preprocessed/comparison`. If any upstream artifacts move, adjust the `paths` block or override modules/parameters via the paired `*_params.R` helpers (documented in `config/README.md`).

## Map-comparison / confusion metrics

`compute_map_metrics.R` rasterizes the BEAD footprints and simulated outputs to generate confusion, quantity, disagreement, and buffer-sensitive summaries. Typical invocation:

```bash
Rscript workspace/adqd_validation/compute_map_metrics.R \
  --simulation-root=outputs/adqd_validation/ADQD_HOLDOUT \
  --output-root=scratch/adqd_validation/metrics/ADQD_HOLDOUT \
  --bead-root=data/raw/ECCC \
  --intervals=2015:2020 \
  --replicates=1:5
```

Key switches:

- `--analysis-mode=LABEL` tags each export so hold-out vs verification tables stay apart.
- `--caribou-buffer` inflates both line and polygon buffers to 500 m to mimic the caribou reporting convention.
- `--line-buffer` / `--polygon-buffer` / `--resolution` tune the geometry buffering and raster comparison resolution.

Outputs (CSV/GeoPackage) land under `scratch/adqd_validation/metrics/<ANALYSIS_MODE>` and are organized by interval + replicate. Pair those summaries with `runs.csv` to look up seeds and log paths.

## Legacy rate table

`disturbanceRates_legacy.csv` documents the historic manually curated rates that shipped with the early comparison harness. Runs now prefer the module-calculated rate splits derived from BEAD diffs, but this CSV remains available as an audit trail or fallback if a regression demands the legacy values.
