#!/usr/bin/env bash
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${WORKSPACE_ROOT}/.." && pwd)"

usage() {
  cat <<USAGE
Usage: $(basename "$0") [--skip=prep,tests,system,sa,ua,adqd,figures,traceability]
       $(basename "$0") [--core-data-url=URL | --core-data-archive=PATH] [--no-core-verify]

Runs the full thesis pipeline in order:
  prep -> tests -> system -> sa -> ua -> adqd -> figures -> traceability

Use --skip to omit one or more stages (comma-separated).
Use --core-data-url/--core-data-archive to fetch the packaged inputs before prep.
USAGE
}

SKIP_LIST=""
CORE_DATA_URL=""
CORE_DATA_ARCHIVE=""
CORE_DATA_VERIFY="true"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --skip=*)
      SKIP_LIST="${1#*=}"
      ;;
    --skip)
      shift
      [[ $# -gt 0 ]] || { usage; exit 1; }
      SKIP_LIST="$1"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --core-data-url=*)
      CORE_DATA_URL="${1#*=}"
      ;;
    --core-data-url)
      shift
      [[ $# -gt 0 ]] || { usage; exit 1; }
      CORE_DATA_URL="$1"
      ;;
    --core-data-archive=*)
      CORE_DATA_ARCHIVE="${1#*=}"
      ;;
    --core-data-archive)
      shift
      [[ $# -gt 0 ]] || { usage; exit 1; }
      CORE_DATA_ARCHIVE="$1"
      ;;
    --no-core-verify)
      CORE_DATA_VERIFY="false"
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 1
      ;;
  esac
  shift
done

should_skip() {
  local needle="$1"
  [[ -n "${SKIP_LIST}" ]] && [[ ",${SKIP_LIST}," == *",${needle},"* ]]
}

run_step() {
  local label="$1"
  shift
  echo "== ${label} =="
  "$@"
}

has_glob() {
  compgen -G "$1" > /dev/null
}

if [[ -n "${CORE_DATA_URL}" && -n "${CORE_DATA_ARCHIVE}" ]]; then
  echo "Provide only one of --core-data-url or --core-data-archive." >&2
  exit 1
fi

if [[ -n "${CORE_DATA_URL}" || -n "${CORE_DATA_ARCHIVE}" ]]; then
  fetch_args=()
  if [[ -n "${CORE_DATA_URL}" ]]; then
    fetch_args+=(--url "${CORE_DATA_URL}")
  else
    fetch_args+=(--archive "${CORE_DATA_ARCHIVE}")
  fi
  if [[ "${CORE_DATA_VERIFY}" != "true" ]]; then
    fetch_args+=(--no-verify)
  fi
  run_step "Fetch core data" bash "${WORKSPACE_ROOT}/helpers/fetch_core_data.sh" "${fetch_args[@]}"
fi

if ! should_skip prep; then
  prep_args=(--profile=all)
  if [[ -n "${BEAD_2020_URL:-}" ]]; then
    prep_args+=(--bead-2020-url="${BEAD_2020_URL}")
  fi
  if [[ -n "${BEAD_2020_ARCHIVE:-}" ]]; then
    prep_args+=(--bead-2020-archive="${BEAD_2020_ARCHIVE}")
  fi
  run_step "Prepare data" Rscript "${WORKSPACE_ROOT}/helpers/prepare_data.R" "${prep_args[@]}"
fi

if ! should_skip tests; then
  run_step "Module tests" Rscript "${WORKSPACE_ROOT}/helpers/run_all_module_tests.R"
  run_step "Runner config validation" Rscript "${WORKSPACE_ROOT}/helpers/validate_runner_configs.R"
fi

if ! should_skip system; then
  run_step "System suite" bash "${WORKSPACE_ROOT}/system/run_system.sh"
fi

if ! should_skip sa; then
  if ! has_glob "${WORKSPACE_ROOT}/sensitivity/config/generated/sa_morris_*.yaml"; then
    run_step "Generate SA configs" Rscript "${WORKSPACE_ROOT}/sensitivity/build_morris_design.R"
  fi
  run_step "Run SA (Morris)" bash "${WORKSPACE_ROOT}/sensitivity/run_morris.sh"
  run_step "Finalize SA metrics" Rscript "${WORKSPACE_ROOT}/sensitivity/finalize_morris_metrics.R"
fi

if ! should_skip ua; then
  if ! has_glob "${WORKSPACE_ROOT}/uncertainty/config/generated/ua_random_*.yaml"; then
    run_step "Generate UA random configs" Rscript "${WORKSPACE_ROOT}/uncertainty/build_ua_design.R"
  fi
  if ! has_glob "${WORKSPACE_ROOT}/uncertainty/config/generated_site_selection/ua_sitesel_*.yaml"; then
    run_step "Generate UA site-selection configs" Rscript "${WORKSPACE_ROOT}/uncertainty/build_site_selection_design.R"
  fi
  run_step "Run UA random design" bash "${WORKSPACE_ROOT}/uncertainty/run_generated.sh"
  run_step "Run UA site selection" bash "${WORKSPACE_ROOT}/uncertainty/run_site_selection.sh"
  run_step "Finalize UA metrics" Rscript "${WORKSPACE_ROOT}/uncertainty/finalize_ua_metrics.R"
fi

if ! should_skip adqd; then
  shopt -s nullglob
  adqd_configs=("${WORKSPACE_ROOT}/adqd_validation/config"/adqd_*.yaml)
  shopt -u nullglob
  for cfg in "${adqd_configs[@]}"; do
    base="$(basename "${cfg}")"
    if [[ "${base}" == *"maps"* ]]; then
      continue
    fi
    run_step "ADQD run ${base}" Rscript "${WORKSPACE_ROOT}/runner.R" "${cfg}"
  done
  run_step "ADQD metrics" Rscript "${WORKSPACE_ROOT}/adqd_validation/run_adqd_metrics.R"
fi

if ! should_skip figures; then
  run_step "Snapshot suite evidence" Rscript "${WORKSPACE_ROOT}/helpers/snapshot_suite_evidence.R"
  run_step "SA figure" Rscript "${WORKSPACE_ROOT}/sensitivity/plot_morris_linear_length_boxplot.R"
  run_step "ADQD figures/maps" Rscript "${WORKSPACE_ROOT}/adqd_validation/plot_adqd_figures.R"
fi

if ! should_skip traceability; then
  run_step "Generate traceability matrix" Rscript "${WORKSPACE_ROOT}/helpers/generate_traceability_matrix.R"
fi

message="End-to-end run complete."
echo "$message"
