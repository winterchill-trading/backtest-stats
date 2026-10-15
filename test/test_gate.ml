open! Core
open Backtest_stats

let evidence =
  { Gate.Evidence.periods = 750
  ; out_of_sample_periods = 250
  ; psr = Some 0.99
  ; out_of_sample_psr = Some 0.93
  ; dsr = Some 0.97
  ; pbo = Some 0.1
  ; spa_p_value = Some 0.01
  ; max_drawdown = Some 0.12
  }
;;

let show policy evidence =
  match Gate.evaluate policy evidence with
  | Accept -> print_endline "accept"
  | Reject reasons ->
    print_endline "reject";
    List.iter reasons ~f:(fun r -> print_endline ("  " ^ Gate.Reason.to_string_hum r))
;;

let%expect_test "policy from a sexp" =
  let policy =
    Gate.Policy.of_sexp_checked
      (Sexp.of_string
         "((min_periods 500) (min_out_of_sample_fraction 0.3) (min_psr 0.95) (max_pbo \
          0.2))")
    |> Or_error.ok_exn
  in
  print_s [%sexp (policy : Gate.Policy.t)];
  show policy evidence;
  print_s
    [%sexp
      (Gate.Policy.of_sexp_checked (Sexp.of_string "((max_pbo 1.5))")
       : Gate.Policy.t Or_error.t)];
  print_s
    [%sexp
      (Gate.Policy.of_sexp_checked (Sexp.of_string "((max_pbp 0.5))") |> Or_error.is_error
       : bool)];
  [%expect
    {|
    ((min_periods 500) (min_out_of_sample_fraction 0.3) (psr_benchmark_sharpe 0)
     (min_psr 0.95) (max_pbo 0.2))
    accept
    (Error ("Gate.Policy: probability outside [0, 1]" max_pbo (p 1.5)))
    true
    |}]
;;

let%expect_test "default policy: accept, then every way to fail" =
  show Gate.Policy.default evidence;
  show
    Gate.Policy.default
    { periods = 100
    ; out_of_sample_periods = 10
    ; psr = Some 0.9
    ; out_of_sample_psr = Some Float.nan
    ; dsr = Some 0.5
    ; pbo = Some 0.45
    ; spa_p_value = None
    ; max_drawdown = None
    };
  show { Gate.Policy.default with max_drawdown = Some 0.1 } evidence;
  [%expect
    {|
    accept
    reject
      100 periods, at least 250 required
      out-of-sample share 0.10 below 0.30
      probabilistic Sharpe ratio 0.900 below 0.950
      out-of-sample probabilistic Sharpe ratio nan below 0.900
      deflated Sharpe ratio 0.500 below 0.950
      probability of backtest overfitting 0.45 above 0.20
      SPA p-value required by the policy but not measured
    reject
      maximum drawdown 0.120 above 0.100
    |}]
;;

let%expect_test "verdict sexp" =
  print_s
    [%sexp
      (Gate.evaluate Gate.Policy.default { evidence with pbo = Some 0.3 }
       : Gate.Verdict.t)];
  [%expect {| (Reject ((Pbo_above (limit 0.2) (actual 0.3)))) |}]
;;

let gen_evidence =
  let open Quickcheck.Let_syntax in
  let prob = Option.quickcheck_generator (Float.gen_incl 0. 1.) in
  let%map periods = Int.gen_incl 0 2000
  and oos = Int.gen_incl 0 2000
  and psr = prob
  and out_of_sample_psr = prob
  and dsr = prob
  and pbo = prob
  and spa_p_value = prob
  and max_drawdown = prob in
  { Gate.Evidence.periods
  ; out_of_sample_periods = Int.min periods oos
  ; psr
  ; out_of_sample_psr
  ; dsr
  ; pbo
  ; spa_p_value
  ; max_drawdown
  }
;;

let%test_unit "the permissive policy accepts anything" =
  Quickcheck.test gen_evidence ~f:(fun e ->
    assert (Gate.Verdict.is_accept (Gate.evaluate Gate.Policy.permissive e)))
;;

let%test_unit "tightening a threshold never turns a reject into an accept" =
  Quickcheck.test
    (Quickcheck.Generator.both gen_evidence (Float.gen_incl 0. 1.))
    ~f:(fun (e, t) ->
      let loose = { Gate.Policy.default with max_pbo = Some (Float.max t 0.2) } in
      let tight = { Gate.Policy.default with max_pbo = Some (Float.min t 0.2) } in
      if not (Gate.Verdict.is_accept (Gate.evaluate loose e))
      then assert (not (Gate.Verdict.is_accept (Gate.evaluate tight e))))
;;
