(lang dune 3.17)

(name backtest-stats)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/backtest-stats))

(package
 (name backtest-stats)
 (synopsis
  "Backtest statistics: adjusted and deflated Sharpe ratios, multiple-testing bootstraps, overfitting probability, VaR backtests and an acceptance gate")
 (description
  "Venue-neutral statistics over return and PnL series. Sharpe and Sortino ratios with autocorrelation and non-normality adjustments, the probabilistic and deflated Sharpe ratios, minimum track record length and drawdown confidence intervals; White's reality check and the SPA test with a stationary block bootstrap; Bonferroni, Holm and Benjamini-Hochberg adjustments; the probability of backtest overfitting by combinatorially symmetric cross-validation; Kupiec and Christoffersen VaR backtests with exact small-sample p-values and the traffic-light zones; and an acceptance gate configured from a sexp that returns a typed verdict. A command-line tool reads CSV or Parquet and writes JSON reports.")
 (tags
  (ocaml oxcaml backtesting statistics quant))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (yojson
   (>= 2.0))
  (parquet
   (>= 0.1.0))
  (core_bench
   (and
    :with-test
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (odoc :with-doc)))
