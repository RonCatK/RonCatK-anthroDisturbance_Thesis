#!/usr/bin/env bash
# Keep going even if a single replicate fails; the runner writes status=error
# to runs.csv, so we just log and continue.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CFG_DIR="${ROOT}/sensitivity/config/generated"

if [[ ! -d "${CFG_DIR}" ]]; then
  echo "Config dir not found: ${CFG_DIR}" >&2
  exit 1
fi

for cfg in "${CFG_DIR}"/sa_morris_*.yaml; do
  [[ -f "${cfg}" ]] || continue
  echo "Running ${cfg}..."
  if ! Rscript "${ROOT}/runner.R" "${cfg}"; then
    echo "Run failed for ${cfg}; marked FAILED in runs.csv. Continuing..." >&2
  fi
done
