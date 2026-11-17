#!/usr/bin/env bash
set -euo pipefail

# Run all UA configs found under workspace/uncertainty/config/generated without
# relying on ua_runs.csv.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CFG_DIR="${ROOT}/uncertainty/config/generated"

if [[ ! -d "${CFG_DIR}" ]]; then
  echo "Config directory not found: ${CFG_DIR}" >&2
  exit 1
fi

shopt -s nullglob
configs=("${CFG_DIR}"/*.yaml)
if [[ ${#configs[@]} -eq 0 ]]; then
  echo "No generated UA configs found in ${CFG_DIR}" >&2
  exit 0
fi

for cfg in "${configs[@]}"; do
  echo "Running $(basename "${cfg}")"
  Rscript "${ROOT}/runner.R" "${cfg}"
done
