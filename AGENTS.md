# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `lib/`, `bin/`, `test/`, `bench/` and `.github/`. The
workspace-wide guide is `../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`backtest-stats` decides whether a backtest result is real: adjusted, probabilistic and
deflated Sharpe ratios, reality check and SPA bootstrap tests, the probability of backtest
overfitting, VaR backtests with exact p-values, and an acceptance gate configured from a
sexp. It works on plain return series and strategy-by-period matrices, venue-neutral.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `backtest-stats` library (`Backtest_stats`): `Series`, `Bootstrap`, `Metrics`, `Multiple_testing`, `Var_backtest`, `Gate`, `Report`, `Json`, `Dist` |
| `bin/` | The `backtest-stats` binary: CSV and Parquet loading (`table.ml`), subcommands (`main.ml`) |
| `test/` | Expect tests against reference values, Quickcheck properties, seeded simulations, the CLI cram test with fixtures in `cli.t/` |
| `bench/` | core_bench latency benchmarks |
| `.github/` | CI workflow |
| `dune-project` | Package metadata; `dune build` regenerates `backtest-stats.opam` |

## Build, test, docs

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/all
opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/doc
```

Parallel work: prefix with `DUNE_BUILD_DIR=_build-backtest-stats` (relative to the
workspace root). Run the binary and the benchmarks:

```sh
opam exec --switch 5.2.0+ox -- dune exec --root .. ./backtest-stats/bin/main.exe -- report test/cli.t/grid.csv
opam exec --switch 5.2.0+ox -- dune exec --root .. ./backtest-stats/bench/bench.exe -- -quota 1
```

Subcommands: `report`, `gate` (exit 2 on reject), `spa`, `pbo`, `var`, `adjust`; `-help`
on each lists the flags. Standalone: `opam repo add winterchill
https://github.com/winterchill-trading/opam-repository.git`, then `dune build && dune test`.
The cram test needs `jq` on the PATH.

## Conventions

- Every module has an `.mli` with odoc on every value; `open! Core`, labelled arguments,
  ppx_jane.
- Statistics are checked against independent references (scipy, statsmodels, numpy, the
  published deflated Sharpe example, the Basel table); write where a number comes from in
  a comment next to it.
- Randomness only through an explicit `Random.State.t`; tests and reports are seeded.
- Expect output prints rounded numbers (6 digits, or 4 decimals in the cram test): `exp`
  and `log` can differ in the last bit between macOS arm64 and Linux amd64.
- Allocation: the metric loops use `let mutable` accumulators; the test pins only that
  allocation does not grow with the series (an upper bound, not a count).
- Format with `opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/fmt --auto-promote`;
  never `dune fmt` across the workspace.
- `CHANGES.md`: a line under `## Unreleased` for every user-visible change.

## Dependencies and dependents

- Library: `core`, `yojson`. Binary: also `core_unix` and `parquet` (org). Benchmarks:
  `core_bench`.
- Intended consumers: leader rankings and copy backtests (through `Report.t`); none wired
  yet.
- CI pins: `parquet`.

## CI

`ci.yml` runs the infra `ox-setup` action (cached switch, `parquet` pinned), `dune build`,
`dune test`, installs `result odoc` with the pins and builds the docs with odoc warnings
failing the job, and calls the shared `obj-magic` workflow. Check a run with
`gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`; never poll.

## Pitfalls

- A `[...]` code span holding an unbalanced bracket (`[0, n)`) is an odoc error; write
  ranges in words.
- Verbatim blocks in `.mli` comments must be indented to the opening `{v`.
- `dune promote --root ..` resolves the root from the current directory: run it from the
  repository directory, or it writes `.corrected` files into a nested copy.
- A float `ref` captured by an `Array.iter` or `Array.fold` closure is boxed on every
  update; use a `for` loop with `let mutable`.
- The SPA p-value is 1 whenever the best strategy lost in sample (the statistic is
  truncated at zero), so it is not uniform under the null; test its size, not its mean.
- Base's `Random.State.make_self_init` is forbidden in inline tests; seed explicitly.

## Where to read more

- `README.md`: formulas, worked examples, CLI, performance, limitations.
- `../specs/repos/backtest-stats.md`.
- `../oxcaml-claude-skills/skills/janestreet-ocaml-style/` and
  `../oxcaml-claude-skills/skills/oxcaml-performance/`.
- `../oxcaml-claude-skills/AGENTS.md` and `../oxcaml-claude-skills/GOTCHAS.md`.

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch; dry-run first.
- Publish, tag or release.
- Commit credentials or print secrets; poll CI in a loop.
