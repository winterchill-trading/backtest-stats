# AGENTS.md (test/)

Expect tests, Quickcheck properties and the CLI cram test.

- Reference values come from scipy, statsmodels and numpy (scripts are not committed; the
  comment above each table says which function produced it) and from published examples.
- `sim.ml` generates seeded normal returns for Monte Carlo checks: test size under the
  null, PBO near one half for noise, exact p-values against brute-force enumeration.
- `cli.t/` holds the fixtures (`grid.csv`, `grid.parquet` written by DuckDB from the CSV,
  `var.csv`, `pvalues.csv`, `policy.sexp`) and `run.t`; it rounds JSON with `jq`.
- Accept new output from the repository directory:
  `opam exec --switch 5.2.0+ox -- dune promote --root ..`.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/runtest`.
See `../AGENTS.md`.
