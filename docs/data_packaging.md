# Data packaging

This repo supports a single **core data package** that bundles the inputs needed for end-to-end runs. The package is a compressed archive containing:

- `data/raw/` (curated inputs referenced by the thesis `disturbanceDT.csv`)
- `data/raw/ECCC/` (BEAD 2010/2015/2020 archives)
- `data/synthetic/rates/` (synthetic fixtures used by the rates suite)

Derived inputs under `data/preprocessed/` are **not** bundled; they are rebuilt by `workspace/helpers/prepare_data.R` when needed.

See `data/core_manifest.csv` for the exact file list and source URLs.

## Download + verify

```bash
# Download from a hosted archive URL
bash workspace/helpers/fetch_core_data.sh --url <CORE_ARCHIVE_URL>

# Or unpack a local archive
bash workspace/helpers/fetch_core_data.sh --archive /path/to/core_data.tar.gz

# Optional: verify checksums (no downloads, no DataPrep)
Rscript workspace/helpers/prepare_data.R --profile=raw,bead,synthetic --verify-only
```

You can also fetch the core archive directly from the end-to-end script:

```bash
bash workspace/run_end_to_end.sh --core-data-url <CORE_ARCHIVE_URL>
```

## Build a core archive (maintainers)

```bash
bash workspace/helpers/package_core_data.sh --out outputs/data/core_data_YYYYMMDD.tar.gz
```

This script expects:

- `data/raw/disturbanceDT.csv` (the curated thesis inputs)
- the referenced `data/raw/*` inputs (zips + rasters)
- `data/raw/ECCC/*.zip` BEAD archives
- `data/synthetic/rates/` fixtures

The script rewrites `data/raw/disturbanceDT.csv` inside the archive so URLs point to `file://data/raw/...`, and it emits `CHECKSUMS.txt` files for `data/raw` and `data/raw/ECCC`.
