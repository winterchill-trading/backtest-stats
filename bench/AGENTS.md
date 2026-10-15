# AGENTS.md (bench/)

`bench.ml` measures the main statistics with core_bench at realistic sizes. It is not
part of `dune test` and has no pinned thresholds.

- The README's table comes from an Apple M-series machine with a release build; quote the
  machine and profile with any new figure.
- Run: `opam exec --switch 5.2.0+ox -- dune exec --root .. ./backtest-stats/bench/bench.exe -- -quota 1`.

See `../AGENTS.md`.
