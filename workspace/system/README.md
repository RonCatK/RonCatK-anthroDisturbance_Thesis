# System suite

The system suite stress-tests the anthroDisturbance workflow across a curated set of extreme configs (cluster-heavy, raster-heavy, probabilistic, etc.). Each YAML under `workspace/system/config/` captures one deterministic scenario that can be re-run at any time through the shared runner.

## Layout

- `config/system_*.yaml` – runner configs, one per scenario.
- `run_system.sh` – thin orchestrator that loops over every config (or subsets by status).
- `runs.csv` – append-only log populated by `workspace/runner.R`, one row per replicate (stored under `outputs/traceability/suite_runs/system_runs.csv`).

Successful runs emit outputs into `outputs/system/<run_name>/rep_XXX` with logs in `scratch/system/<run_name>/rep_XXX.log`.

## Running configs

```bash
# Run every config
bash workspace/system/run_system.sh

# Only re-run configs whose previous rows failed
bash workspace/system/run_system.sh --mode=failed

# Only run configs that have never produced a runs.csv row
bash workspace/system/run_system.sh --mode=missing
```

The wrapper inspects `outputs/traceability/suite_runs/system_runs.csv` to decide what to execute. Internally it calls:

```bash
Rscript workspace/runner.R workspace/system/config/system_cluster_dense_connectors.yaml
```

Feel free to invoke the runner manually with any config for ad-hoc debugging or to experiment with new parameters before committing a YAML.

## Adding new scenarios

1. Copy an existing `system_*.yaml` into `workspace/system/config/`.
2. Update `run_name`, `description`, and any module parameters (`anthroDisturbance_Generator` is the usual focus).
3. Run via `run_system.sh` or the runner directly. The new row shows up in `outputs/traceability/suite_runs/system_runs.csv` once the run finishes.

Keep run names short but descriptive; the outputs/log folders mirror this identifier, making it easy to diff results across branches.
