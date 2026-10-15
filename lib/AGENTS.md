# AGENTS.md (lib/)

The `backtest-stats` library, wrapped as `Backtest_stats`; `backtest_stats.ml` lists the
modules.

- Dependency order: `Dist` -> `Series` -> `Bootstrap` -> `Metrics` -> `Multiple_testing`,
  `Var_backtest` -> `Gate` -> `Json` -> `Report`.
- Sharpe ratios are per period unless the name says annualized; returns are simple
  fractions. Keep that in every new function and say it in the `.mli`.
- Matrices are `float array array`, one row per strategy, one column per period.
- No hidden randomness: every resampler takes `~rng`.
- A new statistic needs a reference value (scipy or a published example) in a test, and
  a field in `Report`, `Json` and, if it gates, `Gate.Evidence` and `Gate.Reason`.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/all`.
See `../AGENTS.md`.
