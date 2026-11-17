#!/usr/bin/env bash
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${WORKSPACE_ROOT}/.." && pwd)"
CONFIG_DIR="${WORKSPACE_ROOT}/e2e_dummy/config"

usage() {
  cat <<USAGE
Usage: $(basename "$0") [--config-dir=DIR]

Runs lightweight model runs using synthetic inputs (no metrics).
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --config-dir=*)
      CONFIG_DIR="${1#*=}"
      ;;
    --config-dir)
      shift
      [[ $# -gt 0 ]] || { usage; exit 1; }
      CONFIG_DIR="$1"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
  shift
done

ensure_cmd() {
  command -v "$1" >/dev/null 2>&1 || { echo "Missing required command: $1" >&2; exit 1; }
}

run_step() {
  local label="$1"
  shift
  echo "== ${label} =="
  "$@"
}

ensure_cmd Rscript

synthetic_root="${PROJECT_ROOT}/data/synthetic/rates"
inputs_dir="${synthetic_root}/synthetic_inputs"
study_area="${synthetic_root}/studyArea/syntheticAOI.shp"
rtm="${synthetic_root}/studyArea/rtm_50m.tif"

if [[ ! -f "${inputs_dir}/disturbanceDT.csv" ]]; then
  echo "Missing synthetic disturbanceDT.csv: ${inputs_dir}/disturbanceDT.csv" >&2
  exit 1
fi
if [[ ! -f "${study_area}" ]]; then
  echo "Missing synthetic study area: ${study_area}" >&2
  exit 1
fi
if [[ ! -f "${rtm}" ]]; then
  echo "Missing synthetic rasterToMatch: ${rtm}" >&2
  exit 1
fi

if [[ ! -d "${CONFIG_DIR}" ]]; then
  echo "Config directory not found: ${CONFIG_DIR}" >&2
  exit 1
fi

mapfile -t configs < <(find "${CONFIG_DIR}" -maxdepth 1 -name '*.yaml' | sort)
if [[ ${#configs[@]} -eq 0 ]]; then
  echo "No dummy configs found under ${CONFIG_DIR}" >&2
  exit 1
fi

for cfg in "${configs[@]}"; do
  run_step "Dummy run $(basename "${cfg}")" Rscript "${WORKSPACE_ROOT}/runner.R" "${cfg}"
done

echo "Dummy end-to-end run complete."
