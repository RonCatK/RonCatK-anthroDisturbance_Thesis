# Repo structure

This repository uses a strict top-level layout so all scripts and suites share consistent paths.

## Top-level folders

- `modules/` – spaDES modules (git submodules).
- `workspace/` – runnable suites, configs, and analysis scripts that orchestrate runs.
- `data/` – inputs (tracked test fixtures + download manifest; large/raw/preprocessed data are ignored).
- `outputs/` – generated outputs (suite results, figures, metrics). Ignored except `.gitkeep`.
- `scratch/` – logs and temporary files. Ignored except `.gitkeep`.
- `docs/` – thesis deliverables, traceability specs/outputs, and repo documentation.

## Infrastructure files

Infrastructure lives at the repo root (e.g., `.github/`, `renv/`, `renv.lock`, `.gitignore`, `.gitmodules`, `Dockerfile`, `README.md`, `LICENSE`, `.Rprofile`).

## Output conventions

- Runner outputs: `outputs/<suite>/<run_name>/rep_###/`
- Runner logs: `scratch/<suite>/<run_name>/rep_###.log`
- Traceability specs + outputs: `docs/traceability/`
- Traceability evidence snapshots: `docs/traceability/evidence/`

## Data conventions

- Download manifest: `data/download_links.csv`
- Core data manifest: `data/core_manifest.csv`
- Raw downloads: `data/raw/` (ignored)
- Preprocessed inputs: `data/preprocessed/` (ignored)
- Synthetic fixtures: `data/synthetic/` (ignored)
- Test fixtures: `data/testing/` (tracked)
- Study area inputs: `data/study_area/` (tracked)

See `docs/data_packaging.md` for the core data archive workflow.
