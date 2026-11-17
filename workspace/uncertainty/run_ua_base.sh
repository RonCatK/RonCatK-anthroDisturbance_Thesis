#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: run_ua_base.sh [--runs N]

Runs the ua_base.yaml scenario once via workspace/runner.R. The UA config’s
`n_reps` controls how many replicate folders (rep_###) are created under the
run folder. Pass --runs N to override `n_reps` in the base config.
USAGE
}

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CFG="${ROOT}/uncertainty/config/ua_base.yaml"
RUNS_OVERRIDE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--runs)
      RUNS_OVERRIDE="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ ! -f "${CFG}" ]]; then
  echo "Base config not found: ${CFG}" >&2
  exit 1
fi

tmp_files=()
cleanup() {
  for f in "${tmp_files[@]:-}"; do
    [[ -f "${f}" ]] && rm -f "${f}"
  done
}
trap cleanup EXIT

cfg_to_run="${CFG}"

if [[ -n "${RUNS_OVERRIDE}" ]]; then
  if ! [[ "${RUNS_OVERRIDE}" =~ ^[0-9]+$ ]] || [[ "${RUNS_OVERRIDE}" -lt 1 ]]; then
    echo "--runs must be a positive integer" >&2
    exit 1
  fi
  tmp_cfg="$(mktemp "${TMPDIR:-/tmp}/ua_base_run_XXXX.yaml")"
  tmp_files+=("${tmp_cfg}")
  python3 - "$CFG" "$tmp_cfg" "$RUNS_OVERRIDE" <<'PY'
import pathlib, re, sys
src, dst, n_reps = sys.argv[1:]
text = pathlib.Path(src).read_text()
text = re.sub(r'^n_reps:.*$', f'n_reps: {n_reps}', text, flags=re.MULTILINE)
pathlib.Path(dst).write_text(text)
PY
  cfg_to_run="${tmp_cfg}"
  echo "Overriding n_reps to ${RUNS_OVERRIDE} in ${cfg_to_run}"
fi

echo "Running UA base with config ${cfg_to_run}"
Rscript "${ROOT}/runner.R" "${cfg_to_run}"
