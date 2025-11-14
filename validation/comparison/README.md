# Comparison Validation Suite

The comparison suite mirrors the layout of the system harness but is scoped to a
single reproducible run that feeds the similarity analysis workflows.

## Contents

- `testing_runs.csv` – scenario matrix with a single active run wired to the
  comparison configuration.
- `run_comparison_suite.R` – entry point that delegates to the shared validation
  runner with comparison-specific paths.
- `run_comparison_matrix.R` – convenience wrapper for launching the active rows
  in the matrix (identical semantics to the system version).
- `config/comparison_params.R` – parameter overrides applied to the generator,
  including the requested similarity settings and seeds.

## Running the scenario

```bash
# run the comparison suite from the umbrella runner
Rscript validation/runner.R --suite=comparison

# or invoke the suite entry point directly
Rscript validation/comparison/run_comparison_suite.R
```

Both commands reuse the shared harness and write outputs to
`outputs/comparison/VAL_2016_2020/` with logs captured under
`scratch/validation/comparison/`.

## Similarity analysis hook

Downstream similarity diagnostics can read the simulation results from
`outputs/comparison/VAL_2016_2020/`. The run metadata (including log location)
is appended to `scratch/validation/comparison/run_data.csv` after every
execution for reproducibility.

