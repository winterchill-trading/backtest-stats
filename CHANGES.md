# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[semantic versioning](https://semver.org/) (0.x: minor versions may break the API).

## Unreleased

## 0.1.0

### Added

- `Series`: timestamped return series with validation, construction from prices or PnL,
  splitting, calendar resampling, excess returns and periods-per-year inference.
- `Bootstrap`: seeded i.i.d., circular block and stationary bootstrap indices and
  percentile intervals.
- `Metrics`: moments, Sharpe and Sortino ratios, autocorrelation-adjusted annualization,
  skew- and kurtosis-adjusted Sharpe ratio, its standard error and interval, the
  probabilistic and deflated Sharpe ratios, minimum track record length, drawdown
  statistics with bootstrap intervals.
- `Multiple_testing`: reality check and SPA bootstrap p-values, Bonferroni, Holm and
  Benjamini-Hochberg adjustments, probability of backtest overfitting by combinatorially
  symmetric cross-validation.
- `Var_backtest`: Kupiec coverage, Christoffersen independence and conditional coverage
  tests with exact small-sample and asymptotic p-values, traffic-light zones.
- `Gate`: acceptance policy from a sexp, typed rejection reasons and verdicts.
- `Report` and `Json`: one record per strategy with every statistic, evidence and
  verdict, as sexp and JSON.
- `backtest-stats` binary: `report`, `gate`, `spa`, `pbo`, `var` and `adjust` over CSV or
  Parquet input, JSON output.
