#!/usr/bin/env bash
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${WORKSPACE_ROOT}/.." && pwd)"

MODE="quick"
SKIP_PREP="false"

usage() {
  cat <<USAGE
Usage: $(basename "$0") [--quick|--full] [--skip-prepare]

Quick mode (default) runs lightweight checks to validate CLI wiring and config parsing.
Full mode runs the end-to-end pipeline (downloads data, runs suites, builds figures).
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --quick)
      MODE="quick"
      ;;
    --full)
      MODE="full"
      ;;
    --skip-prepare)
      SKIP_PREP="true"
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

ensure_cmd bash
ensure_cmd Rscript

for dir in modules workspace data outputs scratch docs; do
  if [[ ! -d "${PROJECT_ROOT}/${dir}" ]]; then
    echo "Missing expected directory: ${dir}" >&2
    exit 1
  fi
 done

if [[ "${MODE}" == "full" ]]; then
  run_step "End-to-end pipeline" bash "${WORKSPACE_ROOT}/run_end_to_end.sh"
  echo "Smoke test (full) complete."
  exit 0
fi

run_step "Validate runner configs" Rscript "${WORKSPACE_ROOT}/helpers/validate_runner_configs.R"

if [[ "${SKIP_PREP}" != "true" ]]; then
  run_step "Prepare data (synthetic check)" Rscript "${WORKSPACE_ROOT}/helpers/prepare_data.R" --profile=synthetic --skip-checksums
fi

run_step "SA metrics CLI" Rscript "${WORKSPACE_ROOT}/sensitivity/collect_morris_metrics.R" --help
run_step "SA finalize CLI" Rscript "${WORKSPACE_ROOT}/sensitivity/finalize_morris_metrics.R" --help
run_step "SA figure CLI" Rscript "${WORKSPACE_ROOT}/sensitivity/plot_morris_linear_length_boxplot.R" --help

run_step "UA metrics CLI" Rscript "${WORKSPACE_ROOT}/uncertainty/finalize_ua_metrics.R" --help

run_step "ADQD metrics CLI" Rscript "${WORKSPACE_ROOT}/adqd_validation/compute_map_metrics.R" --help
run_step "ADQD batch CLI" Rscript "${WORKSPACE_ROOT}/adqd_validation/run_adqd_metrics.R" --help
run_step "ADQD figures/maps CLI" Rscript "${WORKSPACE_ROOT}/adqd_validation/plot_adqd_figures.R" --help

echo "Smoke test (quick) complete."
