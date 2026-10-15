open! Core
open Backtest_stats

let day d = Time_ns.add Time_ns.epoch (Time_ns.Span.of_day (Float.of_int d))
let hour h = Time_ns.add Time_ns.epoch (Time_ns.Span.of_hr h)

let print_series s =
  Array.iteri (Series.times s) ~f:(fun i time ->
    printf "%s %.6f\n" (Time_ns.to_string_utc time) (Series.return_at s i))
;;

let%expect_test "validation" =
  let show r = print_s [%sexp (Or_error.is_ok r : bool)] in
  show (Series.create ~times:[| day 0; day 1 |] ~returns:[| 0.01; 0.02 |]);
  show (Series.create ~times:[| day 1; day 1 |] ~returns:[| 0.01; 0.02 |]);
  show (Series.create ~times:[| day 0 |] ~returns:[| Float.nan |]);
  show (Series.create ~times:[| day 0 |] ~returns:[| -1. |]);
  show (Series.create ~times:[| day 0 |] ~returns:[| 0.; 1. |]);
  [%expect {|
    true
    false
    false
    false
    false
    |}]
;;

let%expect_test "prices to returns and back" =
  let prices = [| 100.; 110.; 99.; 99. |] in
  let s = Series.of_prices ~times:(Array.init 4 ~f:day) ~prices |> Or_error.ok_exn in
  print_series s;
  printf
    "equity %s\n"
    (Series.equity_curve ~start:100. s
     |> Array.map ~f:(sprintf "%.4f")
     |> String.concat_array ~sep:" ");
  printf "compound %.6f\n" (Series.compound (Series.returns s));
  [%expect
    {|
    1970-01-02 00:00:00.000000000Z 0.100000
    1970-01-03 00:00:00.000000000Z -0.100000
    1970-01-04 00:00:00.000000000Z 0.000000
    equity 110.0000 99.0000 99.0000
    compound -0.010000
    |}]
;;

let%expect_test "hourly returns resampled to days compound within each day" =
  let times = [| hour 1.; hour 5.; hour 23.; hour 25.; hour 70. |] in
  let s =
    Series.create ~times ~returns:[| 0.1; 0.1; -0.5; 0.2; 0.01 |] |> Or_error.ok_exn
  in
  print_series (Series.resample s ~every:Time_ns.Span.day);
  [%expect
    {|
    1970-01-01 23:00:00.000000000Z -0.395000
    1970-01-02 01:00:00.000000000Z 0.200000
    1970-01-03 22:00:00.000000000Z 0.010000
    |}]
;;

let%expect_test "split and excess" =
  let s = Series.of_returns [| 0.01; 0.02; 0.03; 0.04; 0.05 |] in
  let a, b = Series.split s ~in_sample_fraction:0.7 in
  printf "%d %d\n" (Series.length a) (Series.length b);
  let bench = Series.of_returns [| 0.01; 0.01; 0.01; 0.01; 0.01 |] in
  print_series (Series.excess s ~over:bench |> Or_error.ok_exn);
  print_s
    [%sexp
      (Series.excess s ~over:(Series.of_returns [| 0. |]) |> Or_error.is_error : bool)];
  [%expect
    {|
    3 2
    1970-01-01 00:00:00.000000000Z 0.000000
    1970-01-02 00:00:00.000000000Z 0.010000
    1970-01-03 00:00:00.000000000Z 0.020000
    1970-01-04 00:00:00.000000000Z 0.030000
    1970-01-05 00:00:00.000000000Z 0.040000
    true
    |}]
;;

let%test_unit "resampling preserves total compounded growth" =
  Quickcheck.test
    (List.gen_non_empty (Float.gen_incl (-0.5) 0.5))
    ~f:(fun returns ->
      let s = Series.of_returns ~step:(Time_ns.Span.of_hr 5.) (Array.of_list returns) in
      let r = Series.resample s ~every:Time_ns.Span.day in
      let a = Series.compound (Series.returns s)
      and b = Series.compound (Series.returns r) in
      [%test_pred: float] (fun d -> Float.( < ) d 1e-9) (Float.abs (a -. b)))
;;

let%expect_test "periods per year from the spacing" =
  let ppy step =
    Series.infer_periods_per_year (Series.of_returns ~step (Array.create ~len:10 0.))
  in
  printf
    "%.2f %.2f %.2f\n"
    (ppy Time_ns.Span.day)
    (ppy Time_ns.Span.hour)
    (ppy (Time_ns.Span.of_day 7.));
  [%expect {| 365.25 8766.00 52.18 |}]
;;
