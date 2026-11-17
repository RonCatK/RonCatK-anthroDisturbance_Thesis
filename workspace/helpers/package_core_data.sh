#!/usr/bin/env bash
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${WORKSPACE_ROOT}/.." && pwd)"

usage() {
  cat <<USAGE
Usage: $(basename "$0") [--out=PATH] [--staging=DIR]

Builds a core data archive containing the minimal datasets needed for the
end-to-end thesis runs (raw inputs, BEAD archives, synthetic rates).

Requires data/raw/disturbanceDT.csv plus the referenced data files.
USAGE
}

OUT_ARCHIVE=""
STAGING_DIR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --out=*)
      OUT_ARCHIVE="${1#*=}"
      ;;
    --out)
      shift
      OUT_ARCHIVE="${1:-}"
      ;;
    --staging=*)
      STAGING_DIR="${1#*=}"
      ;;
    --staging)
      shift
      STAGING_DIR="${1:-}"
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

if [[ -z "${OUT_ARCHIVE}" ]]; then
  mkdir -p "${PROJECT_ROOT}/outputs/data"
  OUT_ARCHIVE="${PROJECT_ROOT}/outputs/data/core_data_$(date +%Y%m%d_%H%M%S).tar.gz"
fi

if [[ -z "${STAGING_DIR}" ]]; then
  STAGING_DIR="${PROJECT_ROOT}/scratch/core_data_package_$(date +%Y%m%d_%H%M%S)"
fi

if [[ -e "${STAGING_DIR}" ]]; then
  echo "Staging directory already exists: ${STAGING_DIR}" >&2
  exit 1
fi

RAW_DT="${PROJECT_ROOT}/data/raw/disturbanceDT.csv"
if [[ ! -f "${RAW_DT}" ]]; then
  echo "Missing ${RAW_DT}. Core packaging expects the curated disturbanceDT.csv under data/raw." >&2
  exit 1
fi

mkdir -p "${STAGING_DIR}/data/raw/ECCC"
mkdir -p "${STAGING_DIR}/data/synthetic"

RAW_LIST="$(mktemp)"
python - "${RAW_DT}" "${RAW_LIST}" <<'PY'
import csv
import os
import sys
import urllib.parse

src = sys.argv[1]
out_list = sys.argv[2]
files = set()

with open(src, newline="") as f:
    reader = csv.DictReader(f)
    for row in reader:
        url = (row.get("URL") or "").strip()
        base = ""
        if url.startswith("file://"):
            path = urllib.parse.unquote(url[7:])
            base = os.path.basename(path)
        if not base:
            base = os.path.basename(row.get("fileName") or "")
        if base:
            files.add(base)

with open(out_list, "w", newline="") as f:
    for name in sorted(files):
        f.write(name + "\n")
PY

while IFS= read -r fname; do
  [[ -n "${fname}" ]] || continue
  src="${PROJECT_ROOT}/data/raw/${fname}"
  if [[ ! -f "${src}" ]]; then
    echo "Missing raw input: ${src}" >&2
    exit 1
  fi
  cp -a "${src}" "${STAGING_DIR}/data/raw/"
done < "${RAW_LIST}"

python - "${RAW_DT}" "${STAGING_DIR}/data/raw/disturbanceDT.csv" <<'PY'
import csv
import os
import sys
import urllib.parse

src = sys.argv[1]
dst = sys.argv[2]

with open(src, newline="") as f_in, open(dst, "w", newline="") as f_out:
    reader = csv.DictReader(f_in)
    fieldnames = reader.fieldnames or []
    writer = csv.DictWriter(f_out, fieldnames=fieldnames)
    writer.writeheader()
    for row in reader:
        url = (row.get("URL") or "").strip()
        base = ""
        if url.startswith("file://"):
            path = urllib.parse.unquote(url[7:])
            base = os.path.basename(path)
        if not base:
            base = os.path.basename(row.get("fileName") or "")
        if base:
            row["URL"] = f"file://data/raw/{base}"
        writer.writerow(row)
PY

BEAD_FILES=(
  "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip"
  "ECCC_2015_anthro_dist_corrected_to_NT1_2016_final.zip"
  "NorthwestTerritories2020.gdb.zip"
)

for fname in "${BEAD_FILES[@]}"; do
  src="${PROJECT_ROOT}/data/raw/ECCC/${fname}"
  if [[ ! -f "${src}" ]]; then
    echo "Missing BEAD archive: ${src}" >&2
    exit 1
  fi
  cp -a "${src}" "${STAGING_DIR}/data/raw/ECCC/"
done

if [[ -d "${PROJECT_ROOT}/data/synthetic/rates" ]]; then
  cp -a "${PROJECT_ROOT}/data/synthetic/rates" "${STAGING_DIR}/data/synthetic/"
else
  echo "Missing data/synthetic/rates; expected synthetic inputs for rates." >&2
  exit 1
fi

if [[ -f "${PROJECT_ROOT}/data/core_manifest.csv" ]]; then
  cp -a "${PROJECT_ROOT}/data/core_manifest.csv" "${STAGING_DIR}/data/core_manifest.csv"
fi

RAW_CHECKSUM_LIST="$(mktemp)"
cat "${RAW_LIST}" > "${RAW_CHECKSUM_LIST}"
echo "disturbanceDT.csv" >> "${RAW_CHECKSUM_LIST}"

Rscript - "${STAGING_DIR}/data/raw" "${RAW_CHECKSUM_LIST}" "${STAGING_DIR}/data/raw/CHECKSUMS.txt" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1]]
list_path <- args[[2]]
out_path <- args[[3]]

suppressPackageStartupMessages({
  library(digest)
  library(data.table)
})

files <- readLines(list_path, warn = FALSE)
files <- files[nzchar(files)]

rows <- lapply(files, function(fname) {
  path <- file.path(root, fname)
  if (!file.exists(path)) stop("Missing file for checksum: ", path, call. = FALSE)
  data.table(
    file = fname,
    checksum = digest(path, algo = "xxhash64", file = TRUE),
    algorithm = "xxhash64",
    filesize = as.integer(file.info(path)$size)
  )
})
dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
data.table::fwrite(dt, out_path, sep = " ", quote = TRUE)
RS

BEAD_LIST="$(mktemp)"
printf "%s\n" "${BEAD_FILES[@]}" > "${BEAD_LIST}"

Rscript - "${STAGING_DIR}/data/raw/ECCC" "${BEAD_LIST}" "${STAGING_DIR}/data/raw/ECCC/CHECKSUMS.txt" <<'RS'
args <- commandArgs(trailingOnly = TRUE)
root <- args[[1]]
list_path <- args[[2]]
out_path <- args[[3]]

suppressPackageStartupMessages({
  library(digest)
  library(data.table)
})

files <- readLines(list_path, warn = FALSE)
files <- files[nzchar(files)]

rows <- lapply(files, function(fname) {
  path <- file.path(root, fname)
  if (!file.exists(path)) stop("Missing file for checksum: ", path, call. = FALSE)
  data.table(
    file = fname,
    checksum = digest(path, algo = "xxhash64", file = TRUE),
    algorithm = "xxhash64",
    filesize = as.integer(file.info(path)$size)
  )
})
dt <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
data.table::fwrite(dt, out_path, sep = " ", quote = TRUE)
RS

mkdir -p "$(dirname "${OUT_ARCHIVE}")"
tar -czf "${OUT_ARCHIVE}" -C "${STAGING_DIR}" data

echo "Core data archive created: ${OUT_ARCHIVE}"
echo "Staging directory: ${STAGING_DIR}"
