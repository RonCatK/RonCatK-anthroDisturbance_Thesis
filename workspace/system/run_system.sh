#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG_DIR="${ROOT}/system/config"
RUNNER="${ROOT}/runner.R"

if [[ ! -x "${RUNNER}" && ! -f "${RUNNER}" ]]; then
  echo "runner not found at ${RUNNER}" >&2
  exit 1
fi

shopt -s nullglob
configs=(${CONFIG_DIR}/system_*.yaml)
shopt -u nullglob

if [[ ${#configs[@]} -eq 0 ]]; then
  echo "No system configs found in ${CONFIG_DIR}" >&2
  exit 1
fi
usage() {
  cat <<EOF
Usage: $(basename "$0") [--mode=all|failed|missing]

  --mode=all      (default) run every system config
  --mode=failed   rerun configs whose previous row in workspace/system/runs.csv had status != success
  --mode=missing  run configs that have no entry in workspace/system/runs.csv
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
RUNS_CSV="${ROOT}/system/runs.csv"
if [[ -f "${RUNS_CSV}" ]]; then
  while IFS=, read -r timestamp suite run_name replicate seed config_file modules data_profile input_root output_dir log_file status error_message; do
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
  done < <(tail -n +2 "${RUNS_CSV}")
fi

configs_to_run=()
for cfg in "${configs[@]}"; do
  rel_path="${cfg#${ROOT}/}"
  rel="workspace/${rel_path}"
  status="${config_status_map[$rel]:-}"
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
