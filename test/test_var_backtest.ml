open! Core
open Backtest_stats
module V = Var_backtest

let hits_at ~n positions =
  let h = Array.create ~len:n false in
  List.iter positions ~f:(fun i -> h.(i) <- true);
  h
;;

let print_test name (t : V.Test.t) =
  printf
    "%-12s stat %.6f p %.6f asymptotic %.6f exact %b\n"
    name
    t.statistic
    t.p_value
    t.asymptotic_p_value
    t.exact
;;

let%expect_test "traffic-light table for 250 days at 99%" =
  (* The Basel backtesting framework's table: cumulative probabilities 8.11%, 28.58%,
     54.32%, 75.81%, 89.22%, 95.88%, 98.63%, 99.60%, 99.89%, 99.97%, 99.99%. *)
  for exceptions = 0 to 10 do
    printf
      "%2d %6.2f%% %s %.2f\n"
      exceptions
      (100.
       *. V.Traffic_light.cumulative_probability
            ~expected_rate:0.01
            ~observations:250
            ~exceptions)
      (V.Traffic_light.classify ~expected_rate:0.01 ~observations:250 ~exceptions
       |> V.Traffic_light.sexp_of_zone
       |> Sexp.to_string)
      (V.Traffic_light.basel_plus_factor ~exceptions)
  done;
  [%expect
    {|
     0   8.11% Green 0.00
     1  28.58% Green 0.00
     2  54.32% Green 0.00
     3  75.81% Green 0.00
     4  89.22% Green 0.00
     5  95.88% Yellow 0.40
     6  98.63% Yellow 0.50
     7  99.60% Yellow 0.65
     8  99.89% Yellow 0.75
     9  99.97% Yellow 0.85
    10  99.99% Red 1.00
    |}]
;;

(* Statistics and asymptotic p-values from an independent numpy/scipy implementation; the
   Kupiec exact p-value from scipy's binomial pmf; the Christoffersen exact p-values agree
   with 100k-path Monte Carlo runs (0.0302 and 0.0296, standard error 0.0005). *)
let%expect_test "clustered exceptions in a 250-day window" =
  let h = hits_at ~n:250 [ 17; 18; 90; 160; 230 ] in
  let r = V.run ~expected_rate:0.01 h in
  print_s [%sexp (r.counts : V.Counts.t), (r.zone : V.Traffic_light.zone)];
  print_test "coverage" r.unconditional_coverage;
  print_test "independence" r.independence;
  print_test "conditional" r.conditional_coverage;
  [%expect
    {|
    (((observations 250) (exceptions 5) (n00 240) (n01 4) (n10 4) (n11 1))
     Yellow)
    coverage     stat 1.956810 p 0.188871 asymptotic 0.161855 exact true
    independence stat 3.153989 p 0.031040 asymptotic 0.075742 exact true
    conditional  stat 5.110799 p 0.029498 asymptotic 0.077661 exact true
    |}]
;;

let%expect_test "strong clustering" =
  let h = hits_at ~n:250 [ 17; 18; 90; 140; 141; 142; 230 ] in
  let r = V.run ~expected_rate:0.01 h in
  print_test "coverage" r.unconditional_coverage;
  print_test "independence" r.independence;
  print_test "conditional" r.conditional_coverage;
  let r = V.run ~exact_limit:100 ~expected_rate:0.01 h in
  print_test "asymptotic" r.conditional_coverage;
  [%expect
    {|
    coverage     stat 5.496990 p 0.013701 asymptotic 0.019049 exact true
    independence stat 13.487564 p 0.000081 asymptotic 0.000240 exact true
    conditional  stat 18.984554 p 0.000024 asymptotic 0.000075 exact true
    asymptotic   stat 18.984554 p 0.000075 asymptotic 0.000075 exact false
    |}]
;;

let%expect_test "no exceptions" =
  let r = V.run ~expected_rate:0.01 (Array.create ~len:250 false) in
  print_test "coverage" r.unconditional_coverage;
  print_test "independence" r.independence;
  print_test "conditional" r.conditional_coverage;
  [%expect
    {|
    coverage     stat 5.025168 p 0.094760 asymptotic 0.024982 exact true
    independence stat 0.000000 p 1.000000 asymptotic 1.000000 exact true
    conditional  stat 5.025168 p 0.110557 asymptotic 0.081059 exact true
    |}]
;;

(* Brute force over all 2^n sequences: the exact p-value is the total probability of
   sequences whose statistic is at least the observed one. *)
let brute_force ~n ~p ~stat ~observed =
  let total = ref 0. in
  for mask = 0 to (1 lsl n) - 1 do
    let h = Array.init n ~f:(fun i -> mask land (1 lsl i) <> 0) in
    let x = Array.count h ~f:Fn.id in
    let pr = (p **. Float.of_int x) *. ((1. -. p) **. Float.of_int (n - x)) in
    if Float.( >= ) (stat h) (observed -. (1e-9 *. Float.max 1. observed))
    then total := !total +. pr
  done;
  !total
;;

let%test_unit "exact p-values match brute-force enumeration" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind n = Int.gen_incl 2 12 in
    let%bind p = Float.gen_incl 0.05 0.5 in
    let%map h = List.gen_with_length n Bool.quickcheck_generator in
    n, p, Array.of_list h
  in
  Quickcheck.test ~trials:200 gen ~f:(fun (n, p, h) ->
    let cc h = (V.christoffersen_conditional_coverage ~expected_rate:p h).statistic in
    let uc h = (V.kupiec ~expected_rate:p h).statistic in
    let close a b = Float.( < ) (Float.abs (a -. b)) 1e-12 in
    let t = V.christoffersen_conditional_coverage ~expected_rate:p h in
    assert (close t.p_value (brute_force ~n ~p ~stat:cc ~observed:t.statistic));
    let t = V.kupiec ~expected_rate:p h in
    assert (close t.p_value (brute_force ~n ~p ~stat:uc ~observed:t.statistic));
    let x = Array.count h ~f:Fn.id in
    if x > 0 && x < n
    then (
      let ind h = (V.christoffersen_independence h).statistic in
      let t = V.christoffersen_independence h in
      let rate = Float.of_int x /. Float.of_int n in
      assert (close t.p_value (brute_force ~n ~p:rate ~stat:ind ~observed:t.statistic))))
;;

let%expect_test "hits from returns and VaR forecasts" =
  print_s
    [%sexp
      (V.hits ~returns:[| -0.03; -0.01; 0.02; -0.025 |] ~var:[| 0.02; 0.02; 0.02; 0.025 |]
       : bool array)];
  [%expect {| (true false false false) |}]
;;
