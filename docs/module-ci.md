Module CI is intended to run inside each module repository, not in this thesis repo. Each module contains its own `.github/workflows/tests.yml` that:

- runs light tests (skipping heavy files where configured) on pushes and pull requests
- allows `workflow_dispatch` with `mode=heavy` to run the full suite
- enforces a coverage floor (`COVERAGE_MIN`), failing the job if coverage drops
- uploads artifacts under `artifacts/tests/<mode>/`
- can optionally send a `repository_dispatch` back to this repo so results are aggregated

To aggregate in this thesis repo, configure a `AGGREGATOR_TOKEN` secret in each module repo with permission to dispatch to `RonCatK/RonCatK-anthroDisturbance_Thesis`. The `Module CI Aggregator` workflow here listens for the `module-ci-report` dispatch and stores the payloads in `outputs/traceability/module-reports/`.
The traceability workflow runs after the aggregator completes, so module CI dispatches can trigger a matrix refresh.
