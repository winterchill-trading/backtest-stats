open! Core
open Backtest_stats

(* JSON floats rounded to 6 significant digits: exp and log can differ in the last bit
   between macOS arm64 and Linux amd64, and the expect output must match on both. *)
let rec round_json : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Float x -> `Float (Float.of_string (sprintf "%.6g" x))
  | `Assoc kv -> `Assoc (List.map kv ~f:(fun (k, v) -> k, round_json v))
  | `List l -> `List (List.map l ~f:round_json)
  | other -> other
;;

let print_json j = print_endline (Yojson.Safe.pretty_to_string (round_json j))

(* A grid of 30 configurations over 3 years of daily returns; configuration 12 has a real
   edge, the others are noise. *)
let candidates ~edge =
  let rng = Random.State.make [| 11 |] in
  Array.init 30 ~f:(fun k ->
    let mean = if k = 12 then edge else 0. in
    Sim.returns rng ~n:756 ~mean ~std:0.01)
;;

let report ~edge ~policy =
  let rows = candidates ~edge in
  let series = Series.of_returns rows.(12) in
  Report.create
    ~name:(sprintf "config-12-edge-%g" edge)
    ~out_of_sample_periods:252
    ~candidates:rows
    ~bootstrap_samples:500
    ~seed:3
    ~policy
    ~periods_per_year:252.
    series
  |> Or_error.ok_exn
;;

let%expect_test "a real edge passes the default gate" =
  let r = report ~edge:0.0025 ~policy:Gate.Policy.default in
  print_json (Report.to_json r);
  [%expect
    {|
    {
      "name": "config-12-edge-0.0025",
      "periods": 756,
      "periods_per_year": 252.0,
      "start": "1970-01-01 00:00:00.000000000Z",
      "stop": "1972-01-26 00:00:00.000000000Z",
      "total_return": 4.02571,
      "annualized_return": 0.712901,
      "annualized_volatility": 0.156116,
      "sharpe_annualized": 3.5288,
      "sharpe_autocorrelation_adjusted": 3.34682,
      "sortino_annualized": 6.10588,
      "skewness": 0.0946775,
      "kurtosis": 2.85059,
      "psr_benchmark_sharpe": 0.0,
      "psr": 1.0,
      "min_track_record_length": 55.8516,
      "trials": { "count": 30, "sharpe_variance": 0.00359563 },
      "dsr": 0.996421,
      "drawdown": {
        "max_drawdown": 0.102964,
        "max_duration": 97,
        "current": 0.0113607,
        "mean": 0.0176077,
        "ulcer_index": 0.0275096
      },
      "drawdown_intervals": {
        "max_drawdown": {
          "estimate": 0.102964,
          "lower": 0.0561213,
          "upper": 0.143832,
          "level": 0.95
        },
        "max_duration": {
          "estimate": 97.0,
          "lower": 33.0,
          "upper": 142.1,
          "level": 0.95
        }
      },
      "out_of_sample": {
        "periods": 252,
        "sharpe_annualized": 3.65349,
        "psr": 0.999873
      },
      "pbo": 0.0111888,
      "reality_check_p_value": 0.0,
      "spa": {
        "statistic": 5.84732,
        "p_value_lower": 0.0,
        "p_value_consistent": 0.0,
        "p_value_upper": 0.0,
        "best": 12,
        "samples": 500
      },
      "evidence": {
        "periods": 756,
        "out_of_sample_periods": 252,
        "psr": 1.0,
        "out_of_sample_psr": 0.999873,
        "dsr": 0.996421,
        "pbo": 0.0111888,
        "spa_p_value": 0.0,
        "max_drawdown": 0.102964
      },
      "verdict": { "accept": true }
    }
    |}]
;;

let%expect_test "a modest edge among thirty trials fails only the deflated Sharpe check" =
  let r = report ~edge:0.0015 ~policy:Gate.Policy.default in
  printf
    "annualized Sharpe %.2f, psr %.3f, dsr %.3f, pbo %.3f\n"
    r.sharpe_annualized
    r.psr
    (Option.value_exn r.dsr)
    (Option.value_exn r.pbo);
  print_json (Json.verdict (Option.value_exn r.verdict));
  [%expect
    {|
    annualized Sharpe 1.91, psr 1.000, dsr 0.687, pbo 0.121
    {
      "accept": false,
      "reasons": [
        {
          "kind": "dsr_below",
          "required": 0.95,
          "actual": 0.687087,
          "message": "deflated Sharpe ratio 0.687 below 0.950"
        }
      ]
    }
    |}]
;;

let%expect_test "the best of thirty noise configurations is rejected" =
  let rows = candidates ~edge:0. in
  let best =
    Array.foldi rows ~init:0 ~f:(fun k best row ->
      if Float.( > ) (Metrics.sharpe row) (Metrics.sharpe rows.(best)) then k else best)
  in
  let r =
    Report.create
      ~name:"best-of-noise"
      ~out_of_sample_periods:252
      ~candidates:rows
      ~bootstrap_samples:500
      ~seed:3
      ~policy:Gate.Policy.default
      ~periods_per_year:252.
      (Series.of_returns rows.(best))
    |> Or_error.ok_exn
  in
  printf
    "best %d, in-sample annualized Sharpe %.2f, psr %.3f, dsr %.3f\n"
    best
    r.sharpe_annualized
    r.psr
    (Option.value_exn r.dsr);
  print_json (Json.verdict (Option.value_exn r.verdict));
  [%expect
    {|
    best 13, in-sample annualized Sharpe 1.60, psr 0.997, dsr 0.573
    {
      "accept": false,
      "reasons": [
        {
          "kind": "out_of_sample_psr_below",
          "required": 0.9,
          "actual": 0.819628,
          "message": "out-of-sample probabilistic Sharpe ratio 0.820 below 0.900"
        },
        {
          "kind": "dsr_below",
          "required": 0.95,
          "actual": 0.572974,
          "message": "deflated Sharpe ratio 0.573 below 0.950"
        },
        {
          "kind": "pbo_above",
          "limit": 0.2,
          "actual": 0.242813,
          "message": "probability of backtest overfitting 0.24 above 0.20"
        },
        {
          "kind": "spa_p_value_above",
          "limit": 0.05,
          "actual": 0.094,
          "message": "SPA p-value 0.094 above 0.050"
        }
      ]
    }
    |}]
;;

let%expect_test "sexp round trip" =
  let r = report ~edge:0.0015 ~policy:Gate.Policy.permissive in
  let back = Report.t_of_sexp (Report.sexp_of_t r) in
  print_s [%sexp (Report.equal r back : bool)];
  [%expect {| true |}]
;;

let%expect_test "errors" =
  let s = Series.of_returns [| 0.01; 0.02 |] in
  print_s [%sexp (Report.create ~periods_per_year:252. s |> Or_error.is_error : bool)];
  let s = Series.of_returns [| 0.01; 0.02; 0.; 0.01 |] in
  print_s
    [%sexp
      (Report.create ~candidates:[| [| 0. |]; [| 0. |] |] ~periods_per_year:252. s
       |> Or_error.is_error
       : bool)];
  [%expect {|
    true
    true
    |}]
;;
