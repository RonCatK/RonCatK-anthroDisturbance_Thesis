# Uncertainty analysis (UA) suite

This mirrors the legacy `validation/ua` workflow but targets the generic `workspace/runner.R`. UA scenarios are expressed as standalone runner configs and can be run one-by-one or in bulk.

## Files

- `config/ua_base.yaml` – short-window base UA scenario.
- `config/ua_runs.csv` – lightweight index of UA configs (columns: `run_name`, `cfg`, `desc`).
- `run_all.sh` – convenience wrapper to run all UA configs via `runner.R`.

## UA design helper

Use `workspace/uncertainty/build_ua_design.R` to draw random UA samples around the base config:

```bash
Rscript workspace/uncertainty/build_ua_design.R \
  --samples=10 \
  --seed=20240601 \
  --replicates=5 \
  --runs-mode=replace
```

Highlights:

- The RNG is seeded (default `12345`) so re-running with the same options reproduces the same parameter table; pass `--seed=NA` to opt out.
- `ua_runs.csv` is rewritten by default (`--runs-mode=replace`) so obsolete rows are removed and run names stay unique; switch to `append` if you truly want to accumulate.
- Each generated run is tagged with the seed (`rng_seed` column) alongside the sampled parameter values in both `ua_runs.csv` and `ua_design_points.csv`.
- Run configs inherit `n_reps` from the base scenario unless `--replicates` is supplied, so the replicate count in each YAML and CSV row always matches what will be executed.

## Running UA scenarios

```bash
# run all UA configs defined here
bash workspace/uncertainty/run_all.sh

# or run a single config
Rscript workspace/runner.R workspace/uncertainty/config/ua_base.yaml
```

Outputs: `outputs/uncertainty/<run_name>/rep_*`  
Logs: `scratch/uncertainty/<run_name>/rep_*.log`  
Run index: appended to `workspace/uncertainty/runs.csv` by `runner.R`.

## Notes

- Paths default to `data/raw`/`outputs`/`scratch` and modules under `modules/`.
- Replicates default to the base config’s `n_reps` (10 in `ua_base`). Override via either the YAML or `build_ua_design.R --replicates`. Replicates derive their seeds from `seed_base`, so `seed_base + rep_idx` stays deterministic per run.
- Wind data is currently disabled in `ua_base` (`use_wind_data: no`). Flip it back on per scenario if required.

## Parameter constraints / sanitization

UA configs are cleaned on write so that no-op knobs are dropped:
- `generatedDisturbanceAsRaster = TRUE` ⇒ removes vector/line-only controls (cluster toggles, grid counts, clustering distances, siteSelection/probabilityDisturbance, mask/altitude, roads).
- `generatedDisturbanceAsRaster = FALSE` + `useClusterMethod = TRUE` ⇒ removes `seismicLineGrids`.
- `generatedDisturbanceAsRaster = FALSE` + `useClusterMethod = FALSE` ⇒ removes `distanceNewLinesFactor` and `refinedStructure`.
- `maskWaterAndMountainsFromLines = FALSE` ⇒ removes `altitudeCut`.

This sanitization happens inside `build_ua_design.R` (see `sanitize_anthro_params()`), so toggling cluster/raster settings implicitly removes conflicting parameters from the emitted configs and runs index. `probabilityDisturbance` is injected from `config/probabilityDisturbance.yaml` only when the base config does not already define one.

`totalDisturbanceRate` is treated as a total %/year target and is partitioned across classes using ECCC proportions; it is ignored if a `DisturbanceRate` table is provided.
