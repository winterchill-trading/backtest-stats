Fixtures: grid.csv holds 500 days of six strategy columns and a benchmark (the
`carry` column has a real edge), grid.parquet the same table written by DuckDB,
var.csv 250 days of returns and 99% VaR forecasts with clustered exceptions.
Numbers are rounded with jq so the output is the same on every platform.

  $ round='def r: if type == "number" then (. * 10000 | round) / 10000 else . end; walk(r)'

Without -column the report picks the best in-sample Sharpe ratio, as a selection would,
and measures it against all seven columns tried:

  $ backtest-stats report grid.csv -oos-fraction 0.3 -benchmark-column benchmark \
  >   | jq -c "$round | {name, periods, periods_per_year, sharpe_annualized, psr, trials, dsr, pbo, rc: .reality_check_p_value, spa: .spa.p_value_consistent, oos: .out_of_sample}"
  {"name":"carry","periods":500,"periods_per_year":365.25,"sharpe_annualized":3.5079,"psr":1,"trials":{"count":6,"sharpe_variance":0.009},"dsr":0.9086,"pbo":0.0354,"rc":0.004,"spa":0.002,"oos":{"periods":150,"sharpe_annualized":3.9207,"psr":0.9942}}

Parquet input gives the same report:

  $ backtest-stats report grid.csv -oos-fraction 0.3 -samples 200 > csv.json
  $ backtest-stats report grid.parquet -oos-fraction 0.3 -samples 200 > parquet.json
  $ cmp csv.json parquet.json && echo same
  same

The gate prints the report and exits with status 2 when the policy rejects. It accepts
the selected column here and rejects a column without an edge:

  $ cat policy.sexp
  ((min_periods 250)
   (min_out_of_sample_fraction 0.3)
   (min_psr 0.95)
   (min_out_of_sample_psr 0.9)
   (min_dsr 0.9)
   (max_pbo 0.2)
   (max_spa_p_value 0.05))
  $ backtest-stats gate grid.csv -policy policy.sexp -oos-fraction 0.3 -benchmark-column benchmark \
  >   | jq -c "$round | .verdict"
  {"accept":true}
  $ backtest-stats gate grid.csv -policy policy.sexp -oos-fraction 0.3 -column mom_10 > gate.json
  [2]
  $ jq -r '.verdict.reasons[].message' gate.json
  probabilistic Sharpe ratio 0.043 below 0.950
  out-of-sample probabilistic Sharpe ratio 0.054 below 0.900
  deflated Sharpe ratio 0.000 below 0.900

SPA and the reality check over all strategy columns against the benchmark:

  $ backtest-stats spa grid.csv -benchmark-column benchmark | jq -c "$round"
  {"strategies":["mom_5","mom_10","mom_20","rev_5","rev_10","carry"],"best":"carry","reality_check":{"statistic":0.0396,"p_value":0.004,"best":5,"samples":1000},"spa":{"statistic":3.5529,"p_value_lower":0,"p_value_consistent":0,"p_value_upper":0,"best":5,"samples":1000}}

The probability of backtest overfitting without the strategy that has an edge:

  $ cut -d, -f1-6 grid.csv > noise.csv
  $ backtest-stats pbo noise.csv -splits 10 | jq -c "$round"
  {"pbo":0.3333,"combinations":252,"strategies":5,"splits":10,"degradation_slope":-0.3773,"probability_of_loss":0.3254}

VaR backtests:

  $ backtest-stats var var.csv -rate 0.01 -returns-column return -var-column var | jq -c "$round"
  {"counts":{"observations":250,"exceptions":6,"n00":240,"n01":3,"n10":3,"n11":3},"expected_rate":0.01,"unconditional_coverage":{"statistic":3.5554,"p_value":0.1222,"asymptotic_p_value":0.0594,"exact":true},"independence":{"statistic":15.9153,"p_value":0,"asymptotic_p_value":0.0001,"exact":true},"conditional_coverage":{"statistic":19.4707,"p_value":0,"asymptotic_p_value":0.0001,"exact":true},"zone":"Yellow"}

Adjusted p-values:

  $ backtest-stats adjust pvalues.csv | jq -c "$round"
  {"p_value":[0.01,0.04,0.03,0.005,0.2,0.04,0.5],"bonferroni":[0.07,0.28,0.21,0.035,1,0.28,1],"holm":[0.06,0.16,0.15,0.035,0.4,0.16,0.5],"benjamini_hochberg":[0.035,0.056,0.056,0.035,0.2333,0.056,0.5]}

Errors go to stderr with status 1:

  $ backtest-stats report grid.csv -column nope
  backtest-stats: ("no such column" (name nope)
   (available (mom_5 mom_10 mom_20 rev_5 rev_10 carry benchmark)))
  [1]
