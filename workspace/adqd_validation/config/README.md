# Comparison config overrides

Each file in this directory returns `list(cfg = ..., params = ...)` that the
comparison harness (`run_comparison_suite.R`) reads when the scenario matrix
(`validation/comparison/comparison_runs.csv`) lists the file under
`moduleOverrides`. The `params` list overrides SpaDES module arguments, while
`cfg` supplies the per-scenario metadata and `output_path`.

## Files

- `comparison_params.R` – stationary-pattern comparison (2016–2020) that applies
  BEAD 2010–2015 rates; metadata highlights the assumption and run label.
- `comparison_params_timealigned_2010_2015.R` – time-aligned verification run
  (`baseline2010`, 2010–2015) that keeps the same rates but updates the UI to
  match the calibration interval.
- `comparison_params_timealigned_2015_2020.R` – time-aligned hold-out run
  (`baseline2015`, 2015–2020) that uses the same core parameters but a shifted
  baseline/time window.
- `adqd_verification_params.R` – AD/QD verification scenario that uses the
  module-derived rates over 2010–2020 (single replicate) for the AD/QD summary
  statistics.
- `adqd_holdout_params.R` – AD/QD hold-out scenario for 2010–2020 (single
  replicate) with refined clustering parameters.
- `adqd_decadal.yaml` – full-NWT 2010–2020 validation run meant to be checked
  against BEAD 2015 and 2020 checkpoints in a single sweep.

## Legacy rate table

The adqd suite no longer supplies `disturbance_rate_file` via the configuration
overrides; both scenarios derive their growth rates directly from the ECCC
footprint difference between `diffYears`. The previous CSV is retained as a
reference under `workspace/adqd_validation/disturbanceRates_legacy.csv`, but it
is not loaded automatically and the generator will fall back to the module's
default `DisturbanceRate` when calculating rates.

Adjust these files when the comparison scenarios need new seeds, metadata tags,
or module parameters; the runner automatically sources the returned lists to keep
the comparison pipeline declarative.
