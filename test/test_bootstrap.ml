open! Core
open Backtest_stats

let rng () = Random.State.make [| 42 |]

let%expect_test "seeded indices are reproducible" =
  List.iter
    [ Bootstrap.Scheme.Iid
    ; Circular_block { length = 3 }
    ; Stationary { mean_block_length = 3. }
    ]
    ~f:(fun scheme ->
      let a = Bootstrap.indices scheme ~rng:(rng ()) ~n:12 in
      let b = Bootstrap.indices scheme ~rng:(rng ()) ~n:12 in
      assert ([%equal: int array] a b);
      print_s [%sexp (scheme : Bootstrap.Scheme.t), (a : int array)]);
  [%expect
    {|
    (Iid (4 4 7 4 3 6 10 10 10 10 11 5))
    ((Circular_block (length 3)) (4 5 6 4 5 6 7 8 9 4 5 6))
    ((Stationary (mean_block_length 3)) (4 5 6 7 8 5 2 3 0 1 2 3))
    |}]
;;

let%expect_test "default block length" =
  print_s
    [%sexp
      (List.map [ 0; 1; 8; 100; 1000; 2520 ] ~f:Bootstrap.default_block_length : int list)];
  [%expect {| (1 1 2 5 10 14) |}]
;;

let%expect_test "percentile interval" =
  let xs = Array.init 101 ~f:Float.of_int in
  Array.permute xs ~random_state:(rng ());
  print_s [%sexp (Bootstrap.percentile_interval ~level:0.9 xs : float * float)];
  [%expect {| (4.9999999999999991 95) |}]
;;

let%expect_test "filling indices does not allocate" =
  let dst = Array.create ~len:1000 0 in
  List.iter
    [ Bootstrap.Scheme.Iid
    ; Circular_block { length = 10 }
    ; Stationary { mean_block_length = 10. }
    ]
    ~f:(fun scheme ->
      let rng = rng () in
      let before = Gc.minor_words () in
      Bootstrap.fill_indices scheme ~rng dst;
      let words = Gc.minor_words () - before in
      print_s [%sexp (scheme : Bootstrap.Scheme.t), (words : int)]);
  [%expect
    {|
    (Iid 0)
    ((Circular_block (length 10)) 0)
    ((Stationary (mean_block_length 10)) 0)
    |}]
;;

let gen_case =
  let open Quickcheck.Let_syntax in
  let%bind n = Int.gen_incl 1 300 in
  let%bind seed = Int.gen_incl 0 1_000_000 in
  let%map scheme =
    Quickcheck.Generator.union
      [ return Bootstrap.Scheme.Iid
      ; (let%map length = Int.gen_incl 1 20 in
         Bootstrap.Scheme.Circular_block { length })
      ; (let%map l = Float.gen_incl 1. 20. in
         Bootstrap.Scheme.Stationary { mean_block_length = l })
      ]
  in
  n, seed, scheme
;;

let%test_unit "indices are in range and blocks are contiguous modulo n" =
  Quickcheck.test gen_case ~f:(fun (n, seed, scheme) ->
    let idx = Bootstrap.indices scheme ~rng:(Random.State.make [| seed |]) ~n in
    assert (Array.for_all idx ~f:(fun i -> i >= 0 && i < n));
    match scheme with
    | Circular_block { length } ->
      Array.iteri idx ~f:(fun i x ->
        if i mod length <> 0 then assert (x = (idx.(i - 1) + 1) mod n))
    | Iid | Stationary _ -> ())
;;

let%expect_test "stationary blocks have the requested mean length" =
  let n = 200_000 in
  let idx = Bootstrap.indices (Stationary { mean_block_length = 8. }) ~rng:(rng ()) ~n in
  let starts = ref 1 in
  for i = 1 to n - 1 do
    if idx.(i) <> (idx.(i - 1) + 1) mod n then incr starts
  done;
  printf "%.2f\n" (Float.of_int n /. Float.of_int !starts);
  [%expect {| 8.09 |}]
;;
