# Run logs

Per-run log files (`rep_###.log`) for runs recorded in `../suite_runs/*.csv`, laid out as
`<suite>/<run_name>/rep_###.log`.

* **Coverage.** 356 of the 366 distinct log paths referenced by the ledgers are included. `MANIFEST.csv`
  lists all 366 with `included = yes/no` and, for the excluded ones, the reason.
* **Selection rule.** A log is included only if its file modification time agrees with the timestamp of its
  ledger row (from 1 hour before to 36 hours after). Logs that were overwritten by a later re-execution of
  the same run name are therefore not included, because they do not describe the ledgered run.
  Excluded: the 4 system runs (original December 2025 logs overwritten by a later re-execution),
  1 validation run (original log not found), 4 sensitivity and 1 uncertainty run (log modified after the
  ledger row).
* **Source.** Sensitivity and uncertainty logs come from the working directory used for the runs. The 39
  validation logs come from an archived copy of that directory, because the original files were overwritten
  in 2026.
* **Only change to the files.** The absolute path of the original working copy of this repository on the
  machine that ran the suites was replaced by `<REPO>` (5,418 occurrences). Nothing else was edited.
  `MANIFEST.csv` gives the SHA-256 of each file before (`sha256_original`) and after (`sha256_published`)
  the replacement.
* **Times.** `ledger_timestamp` and `original_mtime` are local time (UTC+01:00 in November–December 2025).
* **Not recorded in the logs.** Module commits and script revisions.
