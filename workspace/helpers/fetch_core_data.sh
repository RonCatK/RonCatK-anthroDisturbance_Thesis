#!/usr/bin/env bash
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${WORKSPACE_ROOT}/.." && pwd)"

usage() {
  cat <<USAGE
Usage: $(basename "$0") --url=URL [--no-verify]
       $(basename "$0") --archive=PATH [--no-verify]

Downloads or unpacks the core data archive into the repo data/ folder.
Use --no-verify to skip checksum verification after extraction.
USAGE
}

URL=""
ARCHIVE=""
VERIFY=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    --url=*)
      URL="${1#*=}"
      ;;
    --url)
      shift
      URL="${1:-}"
      ;;
    --archive=*)
      ARCHIVE="${1#*=}"
      ;;
    --archive)
      shift
      ARCHIVE="${1:-}"
      ;;
    --no-verify)
      VERIFY=0
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

if [[ -z "${URL}" && -z "${ARCHIVE}" ]]; then
  echo "Provide --url or --archive." >&2
  usage
  exit 1
fi

if [[ -n "${URL}" ]]; then
  mkdir -p "${PROJECT_ROOT}/scratch/data_downloads"
  ARCHIVE="${PROJECT_ROOT}/scratch/data_downloads/core_data_$(date +%Y%m%d_%H%M%S).tar.gz"
  if command -v curl >/dev/null 2>&1; then
    curl -L --fail -o "${ARCHIVE}" "${URL}"
  elif command -v wget >/dev/null 2>&1; then
    wget -O "${ARCHIVE}" "${URL}"
  else
    echo "curl or wget is required to download ${URL}" >&2
    exit 1
  fi
fi

if [[ ! -f "${ARCHIVE}" ]]; then
  echo "Archive not found: ${ARCHIVE}" >&2
  exit 1
fi

if [[ -d "${PROJECT_ROOT}/data/raw" ]] && [[ -n "$(ls -A "${PROJECT_ROOT}/data/raw" 2>/dev/null)" ]]; then
  echo "Warning: data/raw is not empty; extraction may overwrite files." >&2
fi
if [[ -d "${PROJECT_ROOT}/data/synthetic" ]] && [[ -n "$(ls -A "${PROJECT_ROOT}/data/synthetic" 2>/dev/null)" ]]; then
  echo "Warning: data/synthetic is not empty; extraction may overwrite files." >&2
fi

case "${ARCHIVE}" in
  *.tar.gz|*.tgz)
    tar -xzf "${ARCHIVE}" -C "${PROJECT_ROOT}"
    ;;
  *.zip)
    unzip -q "${ARCHIVE}" -d "${PROJECT_ROOT}"
    ;;
  *)
    echo "Unsupported archive type: ${ARCHIVE}" >&2
    exit 1
    ;;
esac

if [[ "${VERIFY}" -eq 1 ]]; then
  Rscript "${WORKSPACE_ROOT}/prepare_data.R" --profile=raw,bead,synthetic --verify-only
else
  echo "Skipped checksum verification."
fi
