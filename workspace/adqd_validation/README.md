# AD/QD validation suite

This suite reproduces the anthropogenic disturbance (AD) and quadratic disagreement (QD) verification runs that fed the thesis analysis. Canned runner configs live under `config/`:

- `adqd_verification.yaml` & `adqd_verification_params.R` – 2010–2020 rerun (full NWT) using calibration-period rates.
- `adqd_holdout.yaml` & `adqd_holdout_params.R` – 2010–2020 rerun (full NWT) seeded off the hold-out tuning set.
- `adqd_decadal.yaml` – full-NWT, 2010–2020 decadal validation with checkpoints against BEAD 2015 and 2020.

`workspace/runner.R` sews these configs into the unified logging/outputs conventions. Successful runs drop results under `outputs/adqd_validation/<RUN_NAME>/rep_xxx` with logs under `scratch/adqd_validation/<RUN_NAME>/rep_xxx.log`. The aggregated status table lives in `workspace/adqd_validation/runs.csv`.

## Running the scenarios

```bash
# Verification (2010–2020)
Rscript workspace/runner.R workspace/adqd_validation/config/adqd_verification.yaml

# Hold-out (2010–2020)
Rscript workspace/runner.R workspace/adqd_validation/config/adqd_holdout.yaml

# Hold-out (2015–2020, baseline 2015)
Rscript workspace/runner.R workspace/adqd_validation/config/adqd_holdout_2015_2020.yaml

# Decadal validation (2010–2020, full NWT)
Rscript workspace/runner.R workspace/adqd_validation/config/adqd_decadal.yaml
```

The configs inherit inputs from `data/preprocessed/comparison`. If any upstream artifacts move, adjust the `paths` block or override modules/parameters via the paired `*_params.R` helpers (documented in `config/README.md`).

## Map-comparison / confusion metrics

`compute_map_metrics.R` rasterizes the BEAD footprints and simulated outputs to generate confusion, quantity, disagreement, and buffer-sensitive summaries. Typical invocation:

```bash
Rscript workspace/adqd_validation/compute_map_metrics.R \
  --simulation-root=outputs/adqd_validation/ADQD_DECADAL \
  --output-root=scratch/adqd_validation/metrics/ADQD_DECADAL \
  --bead-root=data/raw/ECCC \
  --intervals=2010:2020 \
  --replicates=1
```

Key switches:

- `--analysis-mode=LABEL` tags each export so hold-out vs verification tables stay apart.
- `--caribou-buffer` inflates both line and polygon buffers to 500 m to mimic the caribou reporting convention.
- `--skip-buffering` disables buffering (line/polygon buffers set to 0) for pre-buffered inputs.
- `--year-rule=RULE` selects simulated years (`increment` = year > baseline && year <= comparison; `exact` = year == comparison).
- `--no-year-filter` disables simulated year filtering (legacy behavior; use only if filenames lack years).
- `--line-buffer` / `--polygon-buffer` / `--resolution` tune the geometry buffering and raster comparison resolution.
- `--overlap-rule=RULE` controls overlap resolution (`priority`, `first_wins`, or `last_wins`).

Simulated year selection:

- Disturbance files are parsed from `disturbances_<YEAR>_<CLASS>[_rep<REP>].shp`.
- By default, intervals use the `increment` rule (year > baseline && year <= comparison); override with `--year-rule=exact`.
- Selections are recorded in `sim_file_index.csv` alongside `run_metadata.csv` so interval provenance is explicit.

Outputs (CSV/GeoPackage) land under `scratch/adqd_validation/metrics/<ANALYSIS_MODE>` and are organized by interval + replicate. Pair those summaries with `runs.csv` to look up seeds and log paths.

Key output conventions:

- `quantity_metrics.csv` (and `quantity_metrics_crosswalk.csv`) report per-class `observed_unique_area_km2`/`sim_unique_area_mean_km2` as the headline, with `*_gross_area_overlap_inflated_*` kept as diagnostics only.
- `quantity_summary.csv` (and `*_crosswalk.csv`) totals are based on unique area; overlap-inflated gross totals are included for context.
- `class_bias_unique.csv` (and `class_bias_unique_crosswalk.csv`) provides the thesis-ready bias table based on unique areas only.
- `foreground_metrics.csv` includes disturbed-vs-background precision/recall/F1/IoU, prevalence, omission/commission, plus change-mask (non-bg) agreement.
- `disagreement.csv` and `confusion_matrix.csv` are native-class outputs; crosswalked versions are suffixed with `_crosswalk`.
- `grid_corroboration.csv` captures 1/5/10 km disturbed-fraction RMSE, bias, and Spearman correlation.
- `linear_distance_metrics.csv` summarizes median and p90 distances from observed linear disturbances to simulated ones.
- `adqd_summary.csv` (written to the metrics root) appends a compact row per interval/replicate with headline metrics.
- `crosswalk_used.yaml` and `class_order.txt` are written per interval for traceability.
- `sim_file_index.csv` logs every simulated file parsed plus the interval/class-mode selection flags.
- Crosswalk aggregation merges `Oil/Gas` + `Well site` into `OilGas_Total`, and excludes out-of-scope classes (currently `Agriculture` and `Harvest`).

Caribou buffer notes:

- Binary disturbed-vs-background metrics are the primary headline for 500 m buffering.
- `buffered_footprint_by_class_nonadditive.csv` reports per-class buffered footprints using multi-label rasterization; totals overlap and are not additive.
- `run_metadata.csv` records `overlap_rule` and the rasterization order used to resolve overlaps.
- Class-level confusion uses the documented class order as a priority rule and should be interpreted cautiously.

## Legacy rate table

`disturbanceRates_legacy.csv` documents the historic manually curated rates that shipped with the early comparison harness. Runs now prefer the module-calculated rate splits derived from BEAD diffs, but this CSV remains available as an audit trail or fallback if a regression demands the legacy values.
