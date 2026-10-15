open! Core
open Backtest_stats
module M = Multiple_testing

let print_ps ps =
  print_endline (Array.map ps ~f:(sprintf "%.6f") |> String.concat_array ~sep:" ")
;;

let%expect_test "adjusted p-values match statsmodels multipletests" =
  let ps = [| 0.01; 0.04; 0.03; 0.005; 0.20; 0.04; 0.5 |] in
  print_ps (M.Adjust.bonferroni ps);
  print_ps (M.Adjust.holm ps);
  print_ps (M.Adjust.benjamini_hochberg ps);
  print_s [%sexp (M.Adjust.reject ~alpha:0.05 (M.Adjust.holm ps) : bool array)];
  [%expect
    {|
    0.070000 0.280000 0.210000 0.035000 1.000000 0.280000 1.000000
    0.060000 0.160000 0.150000 0.035000 0.400000 0.160000 0.500000
    0.035000 0.056000 0.056000 0.035000 0.233333 0.056000 0.500000
    (false false false true false false false)
    |}]
;;

let%expect_test "reality check and SPA: no edge, then one planted edge" =
  let rng = Random.State.make [| 1 |] in
  let noise = Sim.noise_matrix rng ~strategies:20 ~periods:500 ~std:0.01 in
  let rc = M.Reality_check.run ~rng:(Random.State.make [| 2 |]) noise in
  let spa = M.Spa.run ~rng:(Random.State.make [| 2 |]) noise in
  print_s [%sexp (rc : M.Reality_check.t)];
  print_s [%sexp (spa : M.Spa.t)];
  let edge = Array.copy noise in
  edge.(7) <- Sim.returns rng ~n:500 ~mean:0.002 ~std:0.01;
  let rc = M.Reality_check.run ~rng:(Random.State.make [| 2 |]) edge in
  let spa = M.Spa.run ~rng:(Random.State.make [| 2 |]) edge in
  print_s [%sexp (rc : M.Reality_check.t)];
  print_s [%sexp (spa : M.Spa.t)];
  [%expect
    {|
    ((statistic 0.017315900584672337) (p_value 0.438) (best 8) (samples 1000))
    ((statistic 1.8637044325325254) (p_value_lower 0.236)
     (p_value_consistent 0.397) (p_value_upper 0.413) (best 8) (samples 1000))
    ((statistic 0.059251277703719706) (p_value 0) (best 7) (samples 1000))
    ((statistic 6.0975798742114726) (p_value_lower 0) (p_value_consistent 0)
     (p_value_upper 0) (best 7) (samples 1000))
    |}]
;;

let%expect_test "many poor strategies make the reality check conservative, not SPA" =
  (* One modest edge among 50 strategies that lose clearly: the reality check re-centres
     the losers and their spread inflates the null; SPA's consistent p-value does not. *)
  let rng = Random.State.make [| 3 |] in
  let rows =
    Array.init 50 ~f:(fun k ->
      if k = 0
      then Sim.returns rng ~n:400 ~mean:0.0012 ~std:0.01
      else Sim.returns rng ~n:400 ~mean:(-0.002) ~std:0.03)
  in
  let rc = M.Reality_check.run ~rng:(Random.State.make [| 4 |]) rows in
  let spa = M.Spa.run ~rng:(Random.State.make [| 4 |]) rows in
  printf
    "reality check %.3f; spa lower %.3f consistent %.3f upper %.3f\n"
    rc.p_value
    spa.p_value_lower
    spa.p_value_consistent
    spa.p_value_upper;
  [%expect {| reality check 1.000; spa lower 0.157 consistent 0.477 upper 0.655 |}]
;;

(* Under the null (one strategy with zero mean excess return) a valid test rejects at its
   nominal rate. Each Quickcheck case runs 200 experiments from its seed and checks the
   rejection rates at 5% and 10%; the bands are about three binomial standard deviations
   wide. The reality check p-value is also uniform, so its mean is near 1/2. The SPA
   statistic is truncated at zero, so its p-value is 1 whenever the strategy lost in
   sample: it rejects at the nominal rate but is not uniform above 1/2. *)
let%test_unit "bootstrap p-values reject at the nominal rate under the null" =
  Quickcheck.test ~trials:3 (Int.gen_incl 0 1_000_000) ~f:(fun seed ->
    let rng = Random.State.make [| seed |] in
    let experiments = 200 in
    let rc_ps = Array.create ~len:experiments 0. in
    let spa_ps = Array.create ~len:experiments 0. in
    for e = 0 to experiments - 1 do
      let rows = [| Sim.returns rng ~n:120 ~mean:0. ~std:0.01 |] in
      rc_ps.(e) <- (M.Reality_check.run ~samples:200 ~rng rows).p_value;
      spa_ps.(e) <- (M.Spa.run ~samples:200 ~rng rows).p_value_consistent
    done;
    let rate ps alpha =
      Float.of_int (Array.count ps ~f:(fun p -> Float.( <= ) p alpha))
      /. Float.of_int experiments
    in
    List.iter
      [ "reality check", rc_ps; "spa", spa_ps ]
      ~f:(fun (name, ps) ->
        let r5 = rate ps 0.05
        and r10 = rate ps 0.1 in
        if Float.( > ) r5 0.105 || Float.( < ) r10 0.04 || Float.( > ) r10 0.17
        then raise_s [%message "wrong size" name (seed : int) (r5 : float) (r10 : float)]);
    let mean = Metrics.mean rc_ps in
    if Float.( < ) mean 0.42 || Float.( > ) mean 0.58
    then
      raise_s [%message "reality check p-values not uniform" (seed : int) (mean : float)])
;;

let%expect_test "PBO on a small hand-checkable case" =
  (* Two strategies, four blocks of one period each; [Mean] metric. Strategy 0 wins
     periods 0 and 1, strategy 1 wins periods 2 and 3. When the in-sample half is
     {0 , 1}
     or
     {2 , 3}
     the winner loses out of sample (logit log(1/3 / 2/3)); in the other four combinations
     both tie in and out of sample, the averaged rank is the median and the logit is 0,
     which counts as overfit. *)
  let rows = [| [| 1.; 1.; 0.; 0. |]; [| 0.; 0.; 1.; 1. |] |] in
  let r = M.Pbo.run ~splits:4 ~metric:Mean rows in
  print_s
    [%sexp
      (r.combinations : int)
      , (r.pbo : float)
      , (Array.map r.logits ~f:(fun l -> Float.round_decimal l ~decimal_digits:4)
         : float array)];
  [%expect {| (6 1 (-0.6931 0 0 0 0 -0.6931)) |}]
;;

let%expect_test "PBO: noise near one half, a planted edge near zero" =
  let rng = Random.State.make [| 5 |] in
  let noise = Sim.noise_matrix rng ~strategies:20 ~periods:800 ~std:0.01 in
  let r = M.Pbo.run ~splits:16 noise in
  printf
    "noise: pbo %.3f over %d combinations, loss %.3f, slope %.3f\n"
    r.pbo
    r.combinations
    r.probability_of_loss
    r.degradation_slope;
  let edge = Array.copy noise in
  edge.(3) <- Sim.returns rng ~n:800 ~mean:0.003 ~std:0.01;
  let r = M.Pbo.run ~splits:16 edge in
  printf "edge: pbo %.3f, loss %.3f\n" r.pbo r.probability_of_loss;
  [%expect
    {|
    noise: pbo 0.375 over 12870 combinations, loss 0.408, slope -0.854
    edge: pbo 0.000, loss 0.000
    |}]
;;

let%test_unit "PBO averages one half for strategies without edge" =
  Quickcheck.test ~trials:5 (Int.gen_incl 0 1_000_000) ~f:(fun seed ->
    let rng = Random.State.make [| seed |] in
    let runs = 20 in
    let total = ref 0. in
    for _ = 1 to runs do
      let rows = Sim.noise_matrix rng ~strategies:10 ~periods:200 ~std:0.01 in
      total := !total +. (M.Pbo.run ~splits:8 rows).pbo
    done;
    let avg = !total /. Float.of_int runs in
    if Float.( < ) avg 0.38 || Float.( > ) avg 0.62
    then raise_s [%message "PBO for noise" (seed : int) (avg : float)])
;;
