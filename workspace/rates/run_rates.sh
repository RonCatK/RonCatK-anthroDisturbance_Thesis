#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROJECT_ROOT="$(cd "${ROOT}/.." && pwd)"
SUITE_DIR="${ROOT}/rates"
CONFIG_DIR="${SUITE_DIR}/config"
RUNNER="${ROOT}/runner.R"
RUNS_CSV="${PROJECT_ROOT}/outputs/traceability/suite_runs/rates_runs.csv"

if [[ ! -f "${RUNNER}" ]]; then
  echo "runner not found at ${RUNNER}" >&2
  exit 1
fi

if [[ ! -d "${CONFIG_DIR}" ]]; then
  echo "config directory missing: ${CONFIG_DIR}" >&2
  exit 1
fi

usage() {
  cat <<EOF
Usage: $(basename "$0") [--mode=all|failed|missing]

  --mode=all      run every config (default)
  --mode=failed   rerun configs whose last entry in outputs/traceability/suite_runs/rates_runs.csv errored
  --mode=missing  run configs that have no entry in outputs/traceability/suite_runs/rates_runs.csv
EOF
  exit 1
}

MODE="all"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode=*)
      MODE="${1#*=}"
      ;;
    --mode)
      shift
      [[ $# -gt 0 ]] || usage
      MODE="$1"
      ;;
    -m)
      shift
      [[ $# -gt 0 ]] || usage
      MODE="$1"
      ;;
    -h|--help)
      usage
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      ;;
  esac
  shift
done

if [[ ! "${MODE}" =~ ^(all|failed|missing)$ ]]; then
  echo "Invalid mode: ${MODE}" >&2
  usage
fi

declare -A config_status_map
if [[ -f "${RUNS_CSV}" ]]; then
  tail -n +2 "${RUNS_CSV}" | while IFS=, read -r timestamp suite run_name replicate seed config_file modules data_profile input_root output_dir log_file status error_message; do
    [[ -z "${config_file}" ]] && continue
    timestamp=${timestamp//\"/}
    config_file=${config_file//\"/}
    status=${status//\"/}
    case "${timestamp}" in
      timestamp|\"timestamp\"|"" )
        continue
        ;;
    esac
    config_status_map["${config_file}"]="${status}"
  done
fi

mapfile -t configs < <(find "${CONFIG_DIR}" -maxdepth 1 -name '*.yaml' | sort)
if [[ ${#configs[@]} -eq 0 ]]; then
  echo "No configs found under ${CONFIG_DIR}" >&2
  exit 1
fi

configs_to_run=()
for cfg in "${configs[@]}"; do
  rel_path=${cfg#${ROOT}/}
  rel_config="workspace/${rel_path}"
  status="${config_status_map[$rel_config]:-}"
  if [[ -z "${status}" ]]; then
    status="${config_status_map[${rel_path}]:-}"
  fi
  should_run=false
  case "${MODE}" in
    all)
      should_run=true
      ;;
    failed)
      [[ "${status}" == "error" ]] && should_run=true
      ;;
    missing)
      [[ -z "${status}" ]] && should_run=true
      ;;
  esac
  if [[ "${should_run}" == true ]]; then
    configs_to_run+=("${cfg}")
  fi
done

if [[ ${#configs_to_run[@]} -eq 0 ]]; then
  echo "No configs to run for mode ${MODE}"
  exit 0
fi

overall_status=0
for cfg in "${configs_to_run[@]}"; do
  echo "Running ${cfg}"
  if ! Rscript "${RUNNER}" "${cfg}"; then
    echo "Run failed for ${cfg}" >&2
    overall_status=1
  fi
done

exit "${overall_status}"
