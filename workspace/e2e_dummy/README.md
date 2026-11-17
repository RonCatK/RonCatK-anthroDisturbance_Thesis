# Dummy end-to-end runs

This suite provides small, synthetic runs to validate the model pipeline without
heavy data downloads or metrics. It uses the synthetic rates fixtures under
`data/synthetic/rates/`.

Run all dummy configs:

```bash
bash workspace/run_end_to_end_dummy.sh
```

Configs live under `workspace/e2e_dummy/config/` and use `suite: e2e_dummy`
so outputs stay isolated from the thesis evidence runs.
