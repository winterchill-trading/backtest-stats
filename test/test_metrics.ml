open! Core
open Backtest_stats

(* Two years of monthly returns. Reference values from numpy and scipy.stats (skew and
   kurtosis with fisher=False, which are the 1/n moment ratios used here). *)
let returns =
  [| 0.012
   ; -0.004
   ; 0.021
   ; 0.003
   ; -0.015
   ; 0.008
   ; 0.017
   ; -0.022
   ; 0.011
   ; 0.006
   ; 0.014
   ; -0.009
   ; 0.002
   ; 0.019
   ; -0.003
   ; 0.007
   ; -0.011
   ; 0.016
   ; 0.004
   ; 0.009
   ; -0.006
   ; 0.013
   ; 0.001
   ; 0.010
  |]
;;

let check name ~expected actual =
  let err = Float.abs (actual -. expected) /. Float.max 1e-300 (Float.abs expected) in
  if Float.( > ) err 1e-9
  then printf "%s: expected %.12g got %.12g\n" name expected actual
  else printf "%s %.6f\n" name actual
;;

let%expect_test "moments and ratios against the reference" =
  let m = Metrics.Moments.of_array returns in
  check "mean" ~expected:0.00429166666667 m.mean;
  check "std" ~expected:0.0111646926657 m.std;
  check "skewness" ~expected:(-0.611786417785) m.skewness;
  check "kurtosis" ~expected:2.66515551747 m.kurtosis;
  check "sharpe" ~expected:0.384396310331 (Metrics.sharpe returns);
  check
    "annualized"
    ~expected:1.33158787947
    (Metrics.annualize_sharpe ~periods_per_year:12. (Metrics.sharpe returns));
  check
    "autocorrelation lag 1"
    ~expected:(-0.479966306723)
    (Metrics.autocorrelation returns ~lag:1);
  check
    "annualized, autocorrelation-adjusted"
    ~expected:4.47749170727
    (Metrics.sharpe_autocorrelation_adjusted ~periods_per_year:12 returns);
  check
    "adjusted for skew and kurtosis"
    ~expected:0.370122442524
    (Metrics.adjusted_sharpe returns);
  check "sortino" ~expected:0.674370356132 (Metrics.sortino returns);
  check "psr against 0" ~expected:0.947268329757 (Metrics.probabilistic_sharpe returns);
  check
    "psr against 0.1"
    ~expected:0.884495706426
    (Metrics.probabilistic_sharpe ~benchmark:0.1 returns);
  check
    "min track record length"
    ~expected:24.7426253276
    (Metrics.min_track_record_length returns |> Option.value_exn);
  [%expect
    {|
    mean 0.004292
    std 0.011165
    skewness -0.611786
    kurtosis 2.665156
    sharpe 0.384396
    annualized 1.331588
    autocorrelation lag 1 -0.479966
    annualized, autocorrelation-adjusted 4.477492
    adjusted for skew and kurtosis 0.370122
    sortino 0.674370
    psr against 0 0.947268
    psr against 0.1 0.884496
    min track record length 24.742625
    |}]
;;

let%expect_test "drawdowns" =
  let d = Metrics.Drawdown.of_returns returns in
  check "max drawdown" ~expected:0.022 d.max_drawdown;
  check "mean drawdown" ~expected:0.00419539383333 d.mean;
  check "ulcer index" ~expected:0.00714877486165 d.ulcer_index;
  print_s [%sexp (d.max_duration : int), (d.current : float)];
  let d = Metrics.Drawdown.of_returns [| 0.1; -0.5; 0.2; 0.2; 0.5 |] in
  print_s [%sexp (d : Metrics.Drawdown.t)];
  [%expect
    {|
    max drawdown 0.022000
    mean drawdown 0.004195
    ulcer index 0.007149
    (3 0)
    ((max_drawdown 0.5) (max_duration 3) (current 0) (mean 0.23600000000000004)
     (ulcer_index 0.31253799768988094))
    |}]
;;

let%expect_test "deflated Sharpe ratio, published worked example" =
  (* 100 trials whose annualized Sharpe ratios have variance 1/2, a selected strategy with
     annualized Sharpe 2.5 over 1250 daily observations, skewness -3 and kurtosis

     10. The published answer is an expected maximum of 0.1132 per day and a deflated
     Sharpe ratio of 0.9004. *)
  let periods_per_year = 250. in
  let benchmark =
    Metrics.expected_max_sharpe ~trials:100 ~variance:(0.5 /. periods_per_year)
  in
  let sharpe = 2.5 /. Float.sqrt periods_per_year in
  let moments =
    { Metrics.Moments.n = 1250; mean = 0.; std = 0.; skewness = -3.; kurtosis = 10. }
  in
  check "expected max sharpe" ~expected:0.113172001865 benchmark;
  check
    "deflated sharpe"
    ~expected:0.900396834449
    (Metrics.probabilistic_sharpe_of_moments ~benchmark ~sharpe moments);
  [%expect {|
    expected max sharpe 0.113172
    deflated sharpe 0.900397
    |}]
;;

let%expect_test "edge cases" =
  print_s [%sexp (Metrics.expected_max_sharpe ~trials:1 ~variance:1. : float)];
  print_s
    [%sexp
      (Metrics.min_track_record_length (Array.map returns ~f:Float.neg) : float option)];
  print_s [%sexp (Metrics.sortino [| 0.01; 0.02 |] : float)];
  [%expect {|
    0
    ()
    INF
    |}]
;;

let%expect_test "drawdown intervals bracket the estimate" =
  let rng = Random.State.make [| 7 |] in
  let i = Metrics.Drawdown.confidence_intervals ~samples:500 ~rng returns in
  printf
    "%.4f in [%.4f, %.4f]; duration %.0f in [%.0f, %.0f]\n"
    i.max_drawdown.estimate
    i.max_drawdown.lower
    i.max_drawdown.upper
    i.max_duration.estimate
    i.max_duration.lower
    i.max_duration.upper;
  [%expect {| 0.0220 in [0.0110, 0.0455]; duration 3 in [1, 14] |}]
;;

let gen_returns =
  let open Quickcheck.Let_syntax in
  let%bind n = Int.gen_incl 30 300 in
  let%bind mu = Float.gen_incl 0.001 0.01 in
  let%map xs = List.gen_with_length n (Float.gen_incl (-0.03) 0.03) in
  Array.of_list_map xs ~f:(fun x -> x +. mu)
;;

let%test_unit "deflated Sharpe ratio decreases as trials grow" =
  Quickcheck.test
    (Quickcheck.Generator.both gen_returns (Float.gen_incl 1e-5 1e-2))
    ~f:(fun (xs, variance) ->
      let dsr trials =
        Metrics.deflated_sharpe ~trials ~trials_sharpe_variance:variance xs
      in
      let values = List.map [ 1; 2; 5; 10; 100; 1000; 100_000 ] ~f:dsr in
      List.iter2_exn (List.drop_last_exn values) (List.tl_exn values) ~f:(fun a b ->
        [%test_pred: float * float] (fun (a, b) -> Float.( >= ) a b) (a, b));
      [%test_result: float] (dsr 1) ~expect:(Metrics.probabilistic_sharpe xs))
;;

let%test_unit "autocorrelation adjustment with no lags is square-root scaling" =
  Quickcheck.test gen_returns ~f:(fun xs ->
    let a = Metrics.sharpe_autocorrelation_adjusted ~max_lag:0 ~periods_per_year:252 xs in
    let b = Metrics.annualize_sharpe ~periods_per_year:252. (Metrics.sharpe xs) in
    assert (Float.( < ) (Float.abs (a -. b)) 1e-12))
;;

let%test_unit "psr at the track record length reaches the confidence" =
  Quickcheck.test gen_returns ~f:(fun xs ->
    match Metrics.min_track_record_length ~confidence:0.9 xs with
    | None -> ()
    | Some n ->
      let m = { (Metrics.Moments.of_array xs) with n = Float.iround_up_exn n } in
      let psr =
        Metrics.probabilistic_sharpe_of_moments
          ~benchmark:0.
          ~sharpe:(Metrics.sharpe xs)
          m
      in
      assert (Float.( >= ) psr (0.9 -. 1e-9)))
;;

let words f =
  let before = Gc.minor_words () in
  ignore (Sys.opaque_identity (f ()) : float);
  Gc.minor_words () - before
;;

(* The loops keep their float accumulators unboxed, so allocation does not grow with the
   length of the series. Only an upper bound is pinned: the few words of boxed results
   differ between macOS arm64 and Linux amd64 builds. *)
let%expect_test "allocation does not grow with the series length" =
  let short = Array.init 100 ~f:(fun i -> Float.of_int (i % 7) /. 100.) in
  let long = Array.init 10_000 ~f:(fun i -> Float.of_int (i % 7) /. 100.) in
  List.iter
    [ "sharpe", Metrics.sharpe ?risk_free:None
    ; "probabilistic_sharpe", Metrics.probabilistic_sharpe ?benchmark:None
    ; "sortino", Metrics.sortino ?target:None
    ]
    ~f:(fun (name, f) ->
      let a = words (fun () -> f short)
      and b = words (fun () -> f long) in
      printf "%s %b\n" name (a = b && b < 100));
  [%expect {|
    sharpe true
    probabilistic_sharpe true
    sortino true
    |}]
;;
