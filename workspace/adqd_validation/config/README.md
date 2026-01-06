# ADQD runner configs

Each YAML in this folder is a runnable config for `workspace/runner.R`. These scenarios underpin the thesis AD/QD validation workflow.

## Configs

- `adqd_verification.yaml` – verification run (2010–2020, full NWT).
- `adqd_verification_caribou.yaml` – verification run with 500 m buffer convention.
- `adqd_holdout.yaml` – hold-out run (2010–2020, full NWT).
- `adqd_holdout_caribou.yaml` – hold-out run with 500 m buffer convention.
- `adqd_verification_maps_run.yaml` – helper config for map-metric exports.

Run any config with:

```bash
Rscript workspace/runner.R workspace/adqd_validation/config/<config>.yaml
```
