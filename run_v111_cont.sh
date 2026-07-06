#!/bin/bash
# run_v111_cont.sh — Parallel continuation for v1.1.1 rerun.
#
# Waits for ADQD_VERIFICATION_V111 rep_003 to finish inside the current
# sequential container, then kills it and runs:
#   - ADQD_VERIFICATION_V111_CONT  reps 4-20  (n_par_reps=4 inside one container)
#   - ADQD_HOLDOUT_V111            reps 1-20  (n_par_reps=4 inside one container)
# sequentially (one container at a time) to stay safely under ~40 GB RAM per job.
#
# Memory budget: 1 container × n_par=4 forks × ~10 GB (CoW-adjusted peak)
#   ≈ 35-40 GB, well within the 81 GB available.
#
# Launch: nohup bash run_v111_cont.sh > run_v111_cont.log 2>&1 &
set -uo pipefail

REPO=/home/ron/Documents/VSCode/RonCatK-anthroDisturbance_Thesis-main
OLD_REPO=/home/ron/Documents/VSCode/Repos/RonCatK-anthroDisturbance_Thesis
IMAGE=adqd-gen:v1.1.1
CURRENT_CONTAINER="reverent_lewin"

log() { echo "[$(date '+%Y-%m-%dT%H:%M:%S')] $*"; }
cd "$REPO"

# ── Wait for rep_003 to finish, then kill the sequential container ─────────────
log "Watching container '$CURRENT_CONTAINER' for [rep_004] to appear (= rep_003 done)..."
while true; do
  # Container already gone — either all reps finished naturally or it was killed.
  if ! podman container exists "$CURRENT_CONTAINER" 2>/dev/null; then
    log "Container $CURRENT_CONTAINER is already gone."
    break
  fi
  # rep_004 appearing in logs means rep_003 just completed.
  if podman logs "$CURRENT_CONTAINER" 2>&1 | grep -qF '[rep_004]'; then
    log "Detected [rep_004] start — rep_003 complete. Stopping sequential container."
    podman stop "$CURRENT_CONTAINER" 2>/dev/null || podman kill "$CURRENT_CONTAINER" 2>/dev/null
    sleep 5
    break
  fi
  sleep 30
done

# ── Helper ────────────────────────────────────────────────────────────────────
run_ensemble() {
  local cfg="$1"
  local label="$2"
  log "=== Starting: $label ==="
  local t0; t0=$(date +%s)
  local rc=0
  podman run --rm \
    -e RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    -v "$REPO":/work \
    -v "$OLD_REPO":"$OLD_REPO":ro \
    -v /mnt/ssd1:/mnt/ssd1 \
    -w /work \
    "$IMAGE" \
    Rscript --vanilla workspace/runner.R "$cfg" || rc=$?
  local elapsed=$(( ($(date +%s) - t0) / 60 ))
  if [ "$rc" -eq 0 ]; then
    log "=== DONE: $label (${elapsed} min) ==="
  else
    log "=== WARNING: $label exited $rc (${elapsed} min) — check traceability CSV ==="
  fi
}

# ── ADQD_VERIFICATION_V111 reps 4-20 (n_par_reps=4 inside runner) ─────────────
run_ensemble \
  workspace/adqd_validation/config/adqd_verification_v111_cont.yaml \
  "ADQD_VERIFICATION_V111_CONT (reps 4-20, n_par=4)"

# ── ADQD_HOLDOUT_V111 reps 1-20 (n_par_reps=4 inside runner) ─────────────────
run_ensemble \
  workspace/adqd_validation/config/adqd_holdout_v111.yaml \
  "ADQD_HOLDOUT_V111 (20 reps, n_par=4)"

log "=== ALL CONTINUATION ENSEMBLES COMPLETE ==="
