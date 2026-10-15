open! Core

module Test = struct
  type t =
    { statistic : float
    ; p_value : float
    ; asymptotic_p_value : float
    ; exact : bool
    }
  [@@deriving sexp, compare, equal]
end

let hits ~returns ~var =
  if Array.length returns <> Array.length var
  then raise_s [%message "Var_backtest.hits: returns and var differ in length"];
  Array.map2_exn returns var ~f:(fun r v -> Float.( < ) r (-.v))
;;

module Counts = struct
  type t =
    { observations : int
    ; exceptions : int
    ; n00 : int
    ; n01 : int
    ; n10 : int
    ; n11 : int
    }
  [@@deriving sexp, compare, equal]

  let of_hits hits =
    let n = Array.length hits in
    let c = Array.create ~len:4 0 in
    for t = 1 to n - 1 do
      let i = (if hits.(t - 1) then 2 else 0) + if hits.(t) then 1 else 0 in
      c.(i) <- c.(i) + 1
    done;
    { observations = n
    ; exceptions = Array.count hits ~f:Fn.id
    ; n00 = c.(0)
    ; n01 = c.(1)
    ; n10 = c.(2)
    ; n11 = c.(3)
    }
  ;;
end

let check_rate p =
  if not (Float.( > ) p 0. && Float.( < ) p 1.)
  then raise_s [%message "Var_backtest: expected_rate must be in (0, 1)" (p : float)]
;;

let x_log_y = Dist.x_log_y

(* Log-likelihood ratio statistics from counts. *)
let lr_uc ~p ~n ~x =
  let n = Float.of_int n
  and x = Float.of_int x in
  let pi = x /. n in
  -2.
  *. (x_log_y (n -. x) (1. -. p)
      +. x_log_y x p
      -. x_log_y (n -. x) (1. -. pi)
      -. x_log_y x pi)
;;

let lr_ind ~n00 ~n01 ~n10 ~n11 =
  let f = Float.of_int in
  let total = n00 + n01 + n10 + n11 in
  if total = 0
  then 0.
  else (
    let pi = f (n01 + n11) /. f total in
    let pi01 = if n00 + n01 = 0 then 0. else f n01 /. f (n00 + n01) in
    let pi11 = if n10 + n11 = 0 then 0. else f n11 /. f (n10 + n11) in
    let restricted = x_log_y (f (n00 + n10)) (1. -. pi) +. x_log_y (f (n01 + n11)) pi in
    let unrestricted =
      x_log_y (f n00) (1. -. pi01)
      +. x_log_y (f n01) pi01
      +. x_log_y (f n10) (1. -. pi11)
      +. x_log_y (f n11) pi11
    in
    (* rounding can make the difference a hair negative *)
    Float.max 0. (2. *. (unrestricted -. restricted)))
;;

(* Statistics at least as large as the observed one, up to rounding. *)
let at_least observed s = Float.( >= ) s (observed -. (1e-9 *. Float.max 1. observed))

let kupiec ~expected_rate:p hits =
  check_rate p;
  let n = Array.length hits in
  if n = 0 then raise_s [%message "Var_backtest.kupiec: empty hit sequence"];
  let x = Array.count hits ~f:Fn.id in
  let statistic = lr_uc ~p ~n ~x in
  let exact_p = ref 0. in
  for k = 0 to n do
    if at_least statistic (lr_uc ~p ~n ~x:k)
    then exact_p := !exact_p +. Dist.binomial_pmf ~n ~p k
  done;
  { Test.statistic
  ; p_value = Float.min 1. !exact_p
  ; asymptotic_p_value = Dist.chi_square_sf ~df:1 statistic
  ; exact = true
  }
;;

(* Exact joint distribution of the transition counts of n i.i.d. Bernoulli(p) draws.
   State: first value, last value, n01, n11 (n10 and n00 follow from them). Calls [f] on
   every reachable state with its probability. *)
let iter_transition_distribution ~n ~p ~f =
  let h = (n / 2) + 1 in
  let w = n in
  let size = 4 * h * w in
  let cur = ref (Array.create ~len:size 0.) in
  let next = ref (Array.create ~len:size 0.) in
  let index ~first ~last ~n01 ~n11 = (((((first * 2) + last) * h) + n01) * w) + n11 in
  !cur.(index ~first:0 ~last:0 ~n01:0 ~n11:0) <- 1. -. p;
  !cur.(index ~first:1 ~last:1 ~n01:0 ~n11:0) <- p;
  for t = 1 to n - 1 do
    (* after t draws: n01 <= t / 2 + 1, n11 <= t - 1 *)
    let max01 = Int.min (h - 1) ((t / 2) + 1) in
    let max11 = Int.min (w - 1) t in
    let nx = !next in
    for first = 0 to 1 do
      for last = 0 to 1 do
        for n01 = 0 to max01 do
          for n11 = 0 to max11 do
            nx.(index ~first ~last ~n01 ~n11) <- 0.
          done
        done
      done
    done;
    let cu = !cur in
    for first = 0 to 1 do
      for last = 0 to 1 do
        for n01 = 0 to Int.min (h - 1) ((t / 2) + 1) do
          for n11 = 0 to Int.min (w - 1) (t - 1) do
            let pr = cu.(index ~first ~last ~n01 ~n11) in
            if Float.( > ) pr 0.
            then
              if last = 0
              then (
                let i0 = index ~first ~last:0 ~n01 ~n11 in
                nx.(i0) <- nx.(i0) +. (pr *. (1. -. p));
                let i1 = index ~first ~last:1 ~n01:(n01 + 1) ~n11 in
                nx.(i1) <- nx.(i1) +. (pr *. p))
              else (
                let i0 = index ~first ~last:0 ~n01 ~n11 in
                nx.(i0) <- nx.(i0) +. (pr *. (1. -. p));
                let i1 = index ~first ~last:1 ~n01 ~n11:(n11 + 1) in
                nx.(i1) <- nx.(i1) +. (pr *. p))
          done
        done
      done
    done;
    cur := nx;
    next := cu
  done;
  let cu = !cur in
  for first = 0 to 1 do
    for last = 0 to 1 do
      for n01 = 0 to h - 1 do
        for n11 = 0 to w - 1 do
          let pr = cu.(index ~first ~last ~n01 ~n11) in
          if Float.( > ) pr 0.
          then (
            let n10 = n01 + first - last in
            let n00 = n - 1 - n01 - n10 - n11 in
            let x = n11 + n01 + first in
            f ~n00 ~n01 ~n10 ~n11 ~x pr)
        done
      done
    done
  done
;;

let exact_tail ~n ~p ~statistic ~stat_of =
  let total = ref 0. in
  iter_transition_distribution ~n ~p ~f:(fun ~n00 ~n01 ~n10 ~n11 ~x pr ->
    if at_least statistic (stat_of ~n00 ~n01 ~n10 ~n11 ~x) then total := !total +. pr);
  Float.min 1. !total
;;

let christoffersen_independence ?(exact_limit = 500) hits =
  let c = Counts.of_hits hits in
  if c.observations < 2
  then raise_s [%message "Var_backtest: need at least two observations"];
  let statistic = lr_ind ~n00:c.n00 ~n01:c.n01 ~n10:c.n10 ~n11:c.n11 in
  let asymptotic_p_value = Dist.chi_square_sf ~df:1 statistic in
  let rate = Float.of_int c.exceptions /. Float.of_int c.observations in
  if c.exceptions = 0 || c.exceptions = c.observations
  then { Test.statistic; p_value = 1.; asymptotic_p_value; exact = true }
  else if c.observations > exact_limit
  then { statistic; p_value = asymptotic_p_value; asymptotic_p_value; exact = false }
  else (
    let p_value =
      exact_tail
        ~n:c.observations
        ~p:rate
        ~statistic
        ~stat_of:(fun ~n00 ~n01 ~n10 ~n11 ~x:_ -> lr_ind ~n00 ~n01 ~n10 ~n11)
    in
    { statistic; p_value; asymptotic_p_value; exact = true })
;;

let christoffersen_conditional_coverage ?(exact_limit = 500) ~expected_rate:p hits =
  check_rate p;
  let c = Counts.of_hits hits in
  let n = c.observations in
  if n < 2 then raise_s [%message "Var_backtest: need at least two observations"];
  let stat ~n00 ~n01 ~n10 ~n11 ~x = lr_uc ~p ~n ~x +. lr_ind ~n00 ~n01 ~n10 ~n11 in
  let statistic = stat ~n00:c.n00 ~n01:c.n01 ~n10:c.n10 ~n11:c.n11 ~x:c.exceptions in
  let asymptotic_p_value = Dist.chi_square_sf ~df:2 statistic in
  if n > exact_limit
  then { Test.statistic; p_value = asymptotic_p_value; asymptotic_p_value; exact = false }
  else (
    let p_value = exact_tail ~n ~p ~statistic ~stat_of:stat in
    { statistic; p_value; asymptotic_p_value; exact = true })
;;

module Traffic_light = struct
  type zone =
    | Green
    | Yellow
    | Red
  [@@deriving sexp, compare, equal]

  let cumulative_probability ~expected_rate ~observations ~exceptions =
    Dist.binomial_cdf ~n:observations ~p:expected_rate exceptions
  ;;

  let classify ~expected_rate ~observations ~exceptions =
    let c = cumulative_probability ~expected_rate ~observations ~exceptions in
    if Float.( < ) c 0.95 then Green else if Float.( < ) c 0.9999 then Yellow else Red
  ;;

  let basel_plus_factor ~exceptions =
    match exceptions with
    | e when e <= 4 -> 0.
    | 5 -> 0.40
    | 6 -> 0.50
    | 7 -> 0.65
    | 8 -> 0.75
    | 9 -> 0.85
    | _ -> 1.
  ;;
end

type t =
  { counts : Counts.t
  ; expected_rate : float
  ; unconditional_coverage : Test.t
  ; independence : Test.t
  ; conditional_coverage : Test.t
  ; zone : Traffic_light.zone
  }
[@@deriving sexp, compare, equal]

let run ?exact_limit ~expected_rate hits =
  let counts = Counts.of_hits hits in
  { counts
  ; expected_rate
  ; unconditional_coverage = kupiec ~expected_rate hits
  ; independence = christoffersen_independence ?exact_limit hits
  ; conditional_coverage =
      christoffersen_conditional_coverage ?exact_limit ~expected_rate hits
  ; zone =
      Traffic_light.classify
        ~expected_rate
        ~observations:counts.observations
        ~exceptions:counts.exceptions
  }
;;
