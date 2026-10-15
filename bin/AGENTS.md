# AGENTS.md (bin/)

The `backtest-stats` executable.

- `table.ml` reads CSV (header row) or Parquet (by the `.parquet` suffix). The time
  column is the first named `time`, `timestamp`, `date` or `ts`; Parquet integer times
  use the column's logical type, otherwise the unit is guessed from the magnitude.
- `main.ml` holds the subcommands; output is JSON on stdout, errors on stderr with exit
  status 1, and `gate` exits 2 on reject.
- Every subcommand and flag change needs a line in `../test/cli.t/run.t` and the README's
  command-line section.

Run: `opam exec --switch 5.2.0+ox -- dune exec --root .. ./backtest-stats/bin/main.exe -- -help`.
See `../AGENTS.md`.
