open! Core
open Backtest_stats

(* Reference values from scipy.stats (norm, chi2, binom) and scipy.special.gammaln,
   printed to 12 significant digits; the comparison tolerance is 1e-12 relative. *)
let check name ~expected actual =
  let err = Float.abs (actual -. expected) /. Float.max 1e-300 (Float.abs expected) in
  if Float.( > ) err 1e-11
  then
    printf "%s: expected %.15g got %.15g (relative error %.2e)\n" name expected actual err
;;

let%expect_test "normal cdf and quantile against scipy" =
  List.iter
    [ -3., 0.00134989803163009
    ; -1.5, 0.0668072012688581
    ; 0., 0.5
    ; 0.5, 0.691462461274013
    ; 1.96, 0.97500210485178
    ; 4., 0.999968328758167
    ]
    ~f:(fun (x, expected) -> check (sprintf "cdf %g" x) ~expected (Dist.normal_cdf x));
  List.iter
    [ 1e-10, -6.36134090240406
    ; 0.001, -3.09023230616781
    ; 0.025, -1.95996398454005
    ; 0.95, 1.64485362695147
    ; 0.99, 2.32634787404084
    ; 0.999999, 4.75342430881709
    ]
    ~f:(fun (p, expected) -> check (sprintf "q %g" p) ~expected (Dist.normal_quantile p));
  printf
    "%g %g %g\n"
    (Dist.normal_quantile 0.5)
    (Dist.normal_quantile 0.)
    (Dist.normal_quantile 1.);
  [%expect {| 0 -inf inf |}]
;;

let%expect_test "chi-square survival against scipy" =
  List.iter
    [ 1, 3.841458820694124, 0.0499999999999999
    ; 2, 5.991464547107979, 0.0500000000000001
    ; 3, 7.8, 0.0503310978598533
    ; 5, 1.0, 0.962565773247296
    ; 10, 25.0, 0.00534550548713407
    ]
    ~f:(fun (df, x, expected) ->
      check (sprintf "chi2 %d %g" df x) ~expected (Dist.chi_square_sf ~df x));
  [%expect {| |}]
;;

let%expect_test "binomial against scipy, Basel window of 250 days at 1%" =
  List.iter
    [ 0, 0.0810585161621814, 0.0810585161621814
    ; 2, 0.257417234521773, 0.543168973315726
    ; 4, 0.134070929138742, 0.892187626903625
    ; 5, 0.0666291890265263, 0.958816815930152
    ; 10, 0.000196291439693468, 0.999946101370953
    ]
    ~f:(fun (k, pmf, cdf) ->
      check (sprintf "pmf %d" k) ~expected:pmf (Dist.binomial_pmf ~n:250 ~p:0.01 k);
      check (sprintf "cdf %d" k) ~expected:cdf (Dist.binomial_cdf ~n:250 ~p:0.01 k));
  [%expect {| |}]
;;

let%expect_test "log gamma against scipy" =
  List.iter
    [ 0.1, 2.25271265173421
    ; 0.5, 0.5723649429247
    ; 2.5, 0.284682870472919
    ; 10., 12.8018274800815
    ; 100.5, 361.435540467778
    ]
    ~f:(fun (x, expected) -> check (sprintf "lg %g" x) ~expected (Dist.log_gamma x));
  (* Lanczos gives -8.9e-16 rather than exactly 0 at 1. *)
  printf "%b\n" Float.(abs (Dist.log_gamma 1.) < 1e-14);
  [%expect {| true |}]
;;

let%test_unit "quantile inverts the cdf" =
  Quickcheck.test
    (Float.gen_incl 1e-12 (1. -. 1e-12))
    ~f:(fun p ->
      let x = Dist.normal_quantile p in
      let back = Dist.normal_cdf x in
      if Float.( > ) (Float.abs (back -. p)) (1e-13 +. (1e-12 *. p))
      then raise_s [%message "round trip" (p : float) (x : float) (back : float)])
;;
