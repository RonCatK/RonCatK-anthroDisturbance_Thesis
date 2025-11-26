# Sensitivity (Morris) suite

This folder mirrors the validation SA workflow but targets the new generic `workspace/runner.R`. It builds Morris designs, materialises per-run configs, and runs them through the runner.

## Workflow

1. **Prepare a base config**  
   Edit `config/sa_base.yaml` if needed (paths, modules, times). This acts as the template for all Morris points.

2. **Generate Morris design + configs**  
   ```
   Rscript workspace/sensitivity/build_morris_design.R \
     --base-config=workspace/sensitivity/config/sa_base.yaml \
     --parameters=workspace/sensitivity/config/morris_parameters.yaml \
     --trajectories=10 --levels=6 --replicates=3
   ```
   This writes:
   - Per-run configs under `workspace/sensitivity/config/generated/sa_morris_*.yaml`
   - Design metadata `workspace/sensitivity/config/morris_design_points.csv`
   - Scenario index `workspace/sensitivity/config/sa_runs.csv`

3. **Run the design**  
   ```
   for cfg in workspace/sensitivity/config/generated/sa_morris_*.yaml; do
     Rscript workspace/runner.R "$cfg"
   done
   ```
   Outputs go to `outputs/sensitivity/<run_name>/rep_*`; logs to `scratch/sensitivity/<run_name>/rep_*.log`; runs.csv rows append to `workspace/sensitivity/runs.csv`.

4. **Analyse**  
   The design metadata (`morris_design_points.csv`) aligns trajectory/point indices with the parameter values injected into each run. Feed outputs + design to your UA/SA metric scripts as needed.

## Parameter constraints / sanitization

The generator ignores some knobs depending on the mode. The Morris builder now strips incompatible fields automatically when writing configs:
- `generatedDisturbanceAsRaster = TRUE` ⇒ drops all vector/line-only controls (`useClusterMethod`, clustering distances, grid counts, siteSelection/probabilityDisturbance, mask/altitude, roads).
- `generatedDisturbanceAsRaster = FALSE` + `useClusterMethod = TRUE` ⇒ drops `seismicLineGrids` (cluster workflow does not use it).
- `generatedDisturbanceAsRaster = FALSE` + `useClusterMethod = FALSE` ⇒ drops `distanceNewLinesFactor` and `refinedStructure` (grid workflow ignores them).
- `maskWaterAndMountainsFromLines = FALSE` ⇒ drops `altitudeCut`.

`totalDisturbanceRate` is a global target (% of study area per year) that is split across disturbance classes using ECCC proportions; it is **ignored** if you supply a `DisturbanceRate` table (the module errors if both are non-null).
