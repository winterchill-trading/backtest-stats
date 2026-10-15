(* Latency of the main statistics with core_bench. Not part of [dune test]:

   {v dune exec bench/bench.exe -- -quota 2 v}

   Sizes are typical of the uses this library is for: ten years of daily returns for the
   per-series statistics, a 20-configuration grid over two years for the multiple-testing
   tools, and the 250-day window of the VaR backtests. *)

open! Core
open Backtest_stats
open Core_bench

let gaussian rng =
  let u1 = Float.max Float.min_positive_normal_value (Random.State.float rng 1.) in
  let u2 = Random.State.float rng 1. in
  Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
;;

let rng = Random.State.make [| 1 |]
let returns n = Array.init n ~f:(fun _ -> 0.0004 +. (0.01 *. gaussian rng))
let daily = returns 2520
let grid = Array.init 20 ~f:(fun _ -> returns 504)
let idx = Array.create ~len:2520 0
let hits = Array.init 250 ~f:(fun t -> t % 47 = 0 || t = 101)

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"sharpe (2520)" (fun () -> Metrics.sharpe daily)
       ; Bench.Test.create ~name:"probabilistic_sharpe (2520)" (fun () ->
           Metrics.probabilistic_sharpe daily)
       ; Bench.Test.create ~name:"deflated_sharpe (2520, 100 trials)" (fun () ->
           Metrics.deflated_sharpe ~trials:100 ~trials_sharpe_variance:0.0005 daily)
       ; Bench.Test.create ~name:"autocorrelation-adjusted sharpe (2520)" (fun () ->
           Metrics.sharpe_autocorrelation_adjusted ~periods_per_year:252 daily)
       ; Bench.Test.create ~name:"drawdown (2520)" (fun () ->
           Metrics.Drawdown.of_returns daily)
       ; Bench.Test.create ~name:"stationary bootstrap indices (2520)" (fun () ->
           Bootstrap.fill_indices (Stationary { mean_block_length = 10. }) ~rng idx)
       ; Bench.Test.create ~name:"reality check (20 x 504, 200 resamples)" (fun () ->
           Multiple_testing.Reality_check.run ~samples:200 ~rng grid)
       ; Bench.Test.create ~name:"spa (20 x 504, 200 resamples)" (fun () ->
           Multiple_testing.Spa.run ~samples:200 ~rng grid)
       ; Bench.Test.create ~name:"pbo (20 x 504, 16 splits)" (fun () ->
           Multiple_testing.Pbo.run ~splits:16 grid)
       ; Bench.Test.create ~name:"kupiec exact (250)" (fun () ->
           Var_backtest.kupiec ~expected_rate:0.01 hits)
       ; Bench.Test.create ~name:"christoffersen exact (250)" (fun () ->
           Var_backtest.christoffersen_conditional_coverage ~expected_rate:0.01 hits)
       ])
;;
