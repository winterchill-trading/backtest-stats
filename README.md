# backtest-stats

Statistics for deciding whether a backtest result is real. The library works on plain
return series (one simple return per period) and strategy-by-period matrices, so it does
not care which venue, asset or simulator produced them. It answers four questions:

1. How good is this track record once autocorrelation, skewness, fat tails and its length
   are taken into account? (`Metrics`)
2. Is the best of many strategies better than a benchmark, or is it the luck of the best
   draw? (`Multiple_testing`: reality check, SPA, p-value adjustments)
3. How likely is it that choosing the in-sample winner picked an overfit configuration?
   (`Multiple_testing.Pbo`)
4. Does a VaR model produce the right number of exceptions, and are they independent?
   (`Var_backtest`)

`Gate` turns the answers into an accept or reject verdict from a policy kept in a sexp
file, and `Report` puts everything about one strategy in one record that other tools can
store and show. The `backtest-stats` binary does the same from CSV or Parquet files and
prints JSON.

Pure OCaml on Core; the binary also uses the org's `parquet` reader. Built with OxCaml.

## Build

From the repository inside the workspace:

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @backtest-stats/all @backtest-stats/runtest
```

Standalone, after `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`
on an OxCaml switch: `dune build && dune test`.

## Modules

| Module | Contents |
| --- | --- |
| `Series` | Timestamped return series: validation, from prices or PnL, split, resample by calendar bucket, excess over a benchmark, periods per year from the spacing |
| `Bootstrap` | Seeded i.i.d., circular block and stationary bootstrap indices; percentile intervals |
| `Metrics` | Moments, Sharpe and Sortino ratios, autocorrelation-adjusted annualization, skew- and kurtosis-adjusted Sharpe, its standard error and interval, PSR, expected maximum Sharpe of N trials, DSR, minimum track record length, drawdowns with bootstrap intervals |
| `Multiple_testing` | Reality check, SPA test (lower, consistent, upper p-values), Bonferroni, Holm and Benjamini-Hochberg adjustments, PBO by combinatorially symmetric cross-validation |
| `Var_backtest` | Hit sequences, Kupiec coverage, Christoffersen independence and conditional coverage with exact and asymptotic p-values, traffic-light zones |
| `Gate` | Policy (sexp), evidence, typed reasons, verdict |
| `Report` | One record with every statistic, the evidence and the verdict; sexp and JSON |
| `Json` | JSON encoders for every result type |
| `Dist` | Normal CDF and quantile, chi-square survival, binomial, log-gamma |

## Formulas

Notation: `r_t` the per-period returns, `n` their number, `mu` and `sigma` their mean and
sample standard deviation, `g3` the skewness and `g4` the kurtosis (moment ratios with
`1/n` central moments; `g4 = 3` for a normal distribution), `Phi` the standard normal CDF.
Sharpe ratios are per period unless a name says "annualized".

### Sharpe ratio and its adjustments

- Sharpe ratio: `SR = (mu - rf) / sigma`; annualized with `q` periods per year as
  `SR * sqrt q`, which assumes no autocorrelation.
- Autocorrelation-adjusted annualization: `SR * q / sqrt (q + 2 * sum_{k=1}^{q-1} (q - k) rho_k)`,
  with `rho_k` the sample autocorrelations. Lags beyond `max_lag` (default 10) count as
  zero. Positive autocorrelation (smoothed returns) lowers the ratio.
- Adjusted for skewness and kurtosis: `SR * (1 + g3 / 6 * SR - (g4 - 3) / 24 * SR^2)`.
- Sortino: `(mu - target) / sqrt (1/n * sum min(0, r_t - target)^2)`.
- Standard error of the estimated Sharpe ratio for non-normal returns:
  `sqrt ((1 - g3 * SR + (g4 - 1) / 4 * SR^2) / (n - 1))`.

### Probabilistic and deflated Sharpe ratios

- Probabilistic Sharpe ratio (PSR), the probability that the true Sharpe ratio exceeds a
  benchmark `SR*`: `Phi ((SR - SR*) * sqrt (n - 1) / sqrt (1 - g3 * SR + (g4 - 1) / 4 * SR^2))`.
- Expected maximum Sharpe ratio of `N` independent trials with no skill whose Sharpe
  ratios have variance `V`:
  `E[max] = sqrt V * ((1 - gamma) * Phi^-1 (1 - 1/N) + gamma * Phi^-1 (1 - 1/(N e)))`, with
  `gamma` the Euler-Mascheroni constant.
- Deflated Sharpe ratio (DSR): the PSR with `SR* = E[max]`. It answers "is the selected
  strategy's Sharpe ratio positive once the selection among N trials is accounted for?"
  It falls as trials are added (a property test checks this).
- Minimum track record length: the `n` at which the PSR reaches confidence `c`,
  `1 + (1 - g3 * SR + (g4 - 1) / 4 * SR^2) * (Phi^-1 (c) / (SR - SR*))^2`.

### Drawdowns

On the equity curve `V_t = prod (1 + r_s)` starting at 1: drawdown `1 - V_t / max_{s<=t} V_s`;
maximum drawdown; longest run of periods below the running peak; mean drawdown; ulcer
index (root mean square drawdown). Confidence intervals come from percentiles of the same
statistics over block-bootstrap resamples, since drawdowns depend on the order of returns.

### Bootstraps

Indices into the sample, so all rows of a matrix are resampled with the same periods.
The stationary scheme starts a new block at a uniform position with probability `1/L` at
each step (geometric block lengths, mean `L`) and wraps around the end; the circular block
scheme uses blocks of fixed length `L`. Default `L`: the cube root of `n`.

### Reality check and SPA

Input: `d_kt`, strategy `k`'s return minus the benchmark's in period `t`, with means `d_k`.

- Reality check: statistic `V = max_k sqrt n * d_k`; bootstrap values
  `V*_b = max_k sqrt n * (d*_kb - d_k)`; p-value the share of `V*_b >= V`.
- SPA: studentize with `omega_k`, the bootstrap standard deviation of `sqrt n * d*_k`:
  `T = max (0, max_k sqrt n * d_k / omega_k)`, and
  `T*_b = max (0, max_k sqrt n * (d*_kb - g(d_k)) / omega_k)`. The re-centring `g` gives three
  p-values: upper `g(x) = x` (all strategies re-centred, conservative), lower
  `g(x) = max (x, 0)` (liberal) and consistent
  `g(x) = x` if `x >= -sqrt (omega^2 / n * 2 log log n)`, else 0. A strategy that clearly loses
  keeps its negative mean in the null distribution instead of inflating it, which is why
  SPA keeps its power when many poor strategies are in the set and the reality check does
  not (`test/test_multiple_testing.ml` shows a case with reality check p-value 1.0 and
  consistent SPA p-value 0.48).

### Multiple-testing adjustments

For `m` p-values sorted ascending `p_(1) <= ... <= p_(m)`: Bonferroni `min (1, m p)`;
Holm `max_{j<=i} min (1, (m - j + 1) p_(j))`; Benjamini-Hochberg
`min_{j>=i} min (1, m p_(j) / j)`. Results come back in input order.

### Probability of backtest overfitting

Combinatorially symmetric cross-validation: cut the periods into `S` contiguous blocks
(default 16). For each of the `C(S, S/2)` ways to pick half the blocks as in-sample, find
the strategy with the best in-sample performance (Sharpe ratio or mean), compute its
out-of-sample rank among the `N` strategies (ascending, ties averaged), `w = rank / (N + 1)`,
and the logit `log (w / (1 - w))`. PBO is the share of logits at or below zero. The result
also has the least-squares slope of out-of-sample on in-sample performance (degradation)
and the share of combinations where the selected strategy lost out of sample.

### VaR backtests

Hit sequence: `I_t = 1` when the loss exceeded the VaR forecast; `x` hits in `n`
periods, null rate `p`; `n_ij` counts of a state `i` followed by `j`; `pi = x / n`,
`pi_01 = n_01 / (n_00 + n_01)`, `pi_11 = n_11 / (n_10 + n_11)`.

- Kupiec coverage: `LR_uc = -2 log ((1 - p)^(n - x) p^x / ((1 - pi)^(n - x) pi^x))`, one degree of freedom.
- Christoffersen independence: `LR_ind = -2 log (L(pi_hat) / L(pi_01, pi_11))` over the
  transitions, one degree of freedom.
- Conditional coverage: `LR_cc = LR_uc + LR_ind`, two degrees of freedom.
- Exact p-values: the probability under the null of a statistic at least as large as the
  observed one. Kupiec enumerates the binomial distribution. The Christoffersen tests use
  the exact joint distribution of the transition counts of `n` Bernoulli draws, computed by
  dynamic programming over (first state, last state, `n_01`, `n_11`), up to 500
  observations by default (cubic cost); beyond that the asymptotic value is reported and
  `exact` is false. The independence test leaves the rate unspecified, so its exact p-value
  uses the observed rate. A property test checks both against brute-force enumeration of
  every sequence up to length 12.
- Traffic light: green while `P(X <= x) < 0.95` for `X ~ Binomial(n, p)`, yellow below
  0.9999, red from there.

## Worked examples

### The deflated Sharpe ratio, published example

A strategy with annualized Sharpe ratio 2.5 over 1250 daily observations (250 a year),
skewness -3 and kurtosis 10, selected among 100 trials whose annualized Sharpe ratios have
variance 1/2:

```ocaml
let benchmark = Metrics.expected_max_sharpe ~trials:100 ~variance:(0.5 /. 250.)
(* 0.113172 per day *)
let dsr =
  Metrics.probabilistic_sharpe_of_moments
    ~benchmark
    ~sharpe:(2.5 /. Float.sqrt 250.)
    { n = 1250; mean = 0.; std = 0.; skewness = -3.; kurtosis = 10. }
(* 0.900397: the published value is 0.9004 *)
```

### Basel traffic light, 250 days at 99%

| Exceptions | 0-4 | 5 | 6 | 7 | 8 | 9 | 10+ |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Cumulative probability | up to 89.22% | 95.88% | 98.63% | 99.60% | 99.89% | 99.97% | 99.99% |
| Zone | green | yellow | yellow | yellow | yellow | yellow | red |
| Plus factor | 0 | 0.40 | 0.50 | 0.65 | 0.75 | 0.85 | 1 |

`test/test_var_backtest.ml` pins this table.

### Two years of monthly returns

For the 24 returns in `test/test_metrics.ml` (mean 0.43% a month, skewness -0.61):
Sharpe 0.384 a month, 1.33 annualized by `sqrt 12`, 4.48 with the autocorrelation
adjustment (the lag-1 autocorrelation is -0.48, so the square-root rule understates it),
PSR against zero 0.947, minimum track record length 24.7 months: the record is just short
of 95% confidence. Values agree with numpy and scipy to 1e-9.

### Gating a parameter grid

Thirty configurations over three years of daily returns, one with a real edge
(`test/test_report.ml`):

```ocaml
let report =
  Report.create
    ~name:"config-12"
    ~out_of_sample_periods:252
    ~candidates:grid            (* 30 rows of 756 returns *)
    ~policy:Gate.Policy.default
    ~periods_per_year:252.
    (Series.of_returns grid.(12))
  |> Or_error.ok_exn
```

| Planted edge | Annualized Sharpe | PSR | DSR | PBO | SPA p-value | Verdict |
| --- | --- | --- | --- | --- | --- | --- |
| 0.25% a day | 3.53 | 1.000 | 0.996 | 0.011 | 0.000 | accept |
| 0.15% a day | 1.91 | 1.000 | 0.687 | 0.121 | 0.020 | reject: DSR below 0.95 |
| none (best of 30 noise) | 1.60 | 0.997 | 0.573 | 0.243 | 0.094 | reject: OOS PSR, DSR, PBO, SPA |

The best of thirty noise configurations has an in-sample Sharpe ratio of 1.6 and a PSR of
0.997; only the statistics that know about the other 29 trials catch it.

### Command line

```sh
backtest-stats report grid.csv -oos-fraction 0.3 -benchmark-column benchmark
backtest-stats gate grid.parquet -policy policy.sexp -oos-fraction 0.3   # exit 2 on reject
backtest-stats spa grid.csv -benchmark-column benchmark -samples 2000 -block-length 5
backtest-stats pbo grid.csv -splits 16 -metric sharpe
backtest-stats var var.csv -rate 0.01 -returns-column return -var-column var
backtest-stats adjust pvalues.csv -column p_value
```

Input files have a header row, an optional time column (`time`, `timestamp`, `date` or
`ts`) and one numeric column per strategy. `report` and `gate` report the column given by
`-column`, or else the best in-sample Sharpe ratio (what a selection would pick), and
treat every other numeric column except the benchmark as a candidate that was tried.
`test/cli.t/run.t` runs every command on committed fixtures.

## For other tools

`Report.t` is the hook: it derives `sexp`, `compare` and `equal`, has `Report.to_json`,
and holds the `Gate.Evidence.t` it was judged on and the `Gate.Verdict.t` when a policy
was given. A ranking or a backtest can call `Report.create` with its candidate matrix and
store the result next to its output. `Gate.Reason.to_string_hum` gives one line per failed
check for people.

## Performance

Apple M-series, release build, `bench/bench.exe -quota 1`:

| Operation | Time |
| --- | --- |
| Sharpe ratio, 2520 returns | 6.1 us |
| PSR or DSR, 2520 returns | 10.6 us |
| Autocorrelation-adjusted Sharpe, 2520 returns | 51 us |
| Drawdown statistics, 2520 returns | 3.4 us |
| Stationary bootstrap indices, 2520 | 26 us |
| Reality check or SPA, 20 strategies x 504 periods, 200 resamples | 2.6 ms |
| PBO, 20 x 504, 16 splits (12870 combinations) | 13 ms |
| Kupiec exact, 250 | 21 us |
| Christoffersen exact, 250 | 64 ms |

The metric loops keep their accumulators unboxed; an expect test checks that their
allocation does not grow with the length of the series.

## Limitations

- Returns are simple per-period fractions; log returns must be converted first.
- The SPA studentization uses the bootstrap variance, not a kernel estimator.
- PBO needs at least two strategies and at most 24 blocks; blocks are contiguous and of
  near-equal size (the remainder spreads over the blocks).
- The exact Christoffersen p-values cost about `n^3 / 4` steps; at 500 observations
  expect roughly half a second.
- `Series.infer_periods_per_year` assumes a 365.25-day calendar; pass 252 for daily
  equity data.

## License

MIT.
