# RonCatK-anthroDisturbance_Thesis

Reference repo for the anthropogenic disturbance thesis: model code (modules), runnable suites (workspace), inputs/outputs, and the final thesis deliverables.

## Repo structure

The repo follows a strict layout. See `docs/repo_structure.md` for what belongs where and the outputs/logs conventions.

## Quickstart

Clone with submodules and restore the R environment:

```bash
git clone --recurse-submodules https://github.com/RonCatK/RonCatK-anthroDisturbance_Thesis.git
cd RonCatK-anthroDisturbance_Thesis
Rscript -e 'renv::restore()'
```

Run the fast verification surface:

```bash
Rscript workspace/helpers/run_all_module_tests.R
Rscript workspace/helpers/validate_runner_configs.R
Rscript workspace/helpers/generate_traceability_matrix.R
```

Fetch the core data archive (recommended for reproducing the thesis inputs):

```bash
Rscript workspace/helpers/prepare_data.R --profile=all
```

Run the full end-to-end pipeline (runs data prep unless you skip it):

```bash
bash workspace/run_end_to_end.sh
```

Use `bash workspace/run_end_to_end.sh --skip=prep,sa` (comma-separated) to omit stages.

## Traceability

- Editable spec: `docs/traceability/traceability_requirements.csv`
- CSV output: `docs/traceability/traceability_matrix.csv`
- MD table: `docs/traceability/traceability_matrix.md`
- Evidence snapshots: `docs/traceability/evidence/`

See `docs/traceability/README.md` for details.

## Suites

Suite configs and runners live under `workspace/`. Start with `workspace/README.md` and the suite-specific READMEs.

## Thesis deliverables

Final thesis + appendices are in `docs/`:

- `docs/Thesis_Ronald-Kilian_final.pdf`
- `docs/Appendix A - ODD Model Description of the SpaDES Anthropogenic Disturbance Simulation.pdf`
- `docs/Appendix B - Traceability Matrix.pdf`
- `docs/traceability_matrix.csv` (submission snapshot, “at a glance”)
