open! Core

let check_matrix ~who rows =
  let m = Array.length rows in
  if m = 0 then raise_s [%message "empty performance matrix" (who : string)];
  let n = Array.length rows.(0) in
  if n < 2 then raise_s [%message "need at least two periods" (who : string) (n : int)];
  Array.iteri rows ~f:(fun k row ->
    if Array.length row <> n
    then raise_s [%message "rows differ in length" (who : string) (k : int)]);
  m, n
;;

let excess_over ~benchmark rows =
  Array.map rows ~f:(fun row ->
    if Array.length row <> Array.length benchmark
    then raise_s [%message "Multiple_testing.excess_over: length differs from benchmark"];
    Array.map2_exn row benchmark ~f:( -. ))
;;

let default_scheme n =
  Bootstrap.Scheme.Stationary
    { mean_block_length = Float.of_int (Bootstrap.default_block_length n) }
;;

let row_mean row =
  let s = ref 0. in
  for t = 0 to Array.length row - 1 do
    s := !s +. row.(t)
  done;
  !s /. Float.of_int (Array.length row)
;;

(* Bootstrap means: out.(b).(k) is the mean of row k read at the b-th index draw. *)
let bootstrap_means ~scheme ~samples ~rng rows ~n =
  let idx = Array.create ~len:n 0 in
  Array.init samples ~f:(fun _ ->
    Bootstrap.fill_indices scheme ~rng idx;
    Array.map rows ~f:(fun row ->
      let s = ref 0. in
      for t = 0 to n - 1 do
        s := !s +. row.(idx.(t))
      done;
      !s /. Float.of_int n))
;;

let argmax xs =
  let best = ref 0 in
  Array.iteri xs ~f:(fun i x -> if Float.( > ) x xs.(!best) then best := i);
  !best
;;

module Reality_check = struct
  type t =
    { statistic : float
    ; p_value : float
    ; best : int
    ; samples : int
    }
  [@@deriving sexp, compare, equal]

  let run ?scheme ?(samples = 1000) ~rng rows =
    let _m, n = check_matrix ~who:"Reality_check" rows in
    let scheme = Option.value scheme ~default:(default_scheme n) in
    let root_n = Float.sqrt (Float.of_int n) in
    let means = Array.map rows ~f:row_mean in
    let best = argmax means in
    let statistic = root_n *. means.(best) in
    let boot = bootstrap_means ~scheme ~samples ~rng rows ~n in
    let exceed =
      Array.count boot ~f:(fun bm ->
        let v = ref Float.neg_infinity in
        Array.iteri bm ~f:(fun k x -> v := Float.max !v (root_n *. (x -. means.(k))));
        Float.( >= ) !v statistic)
    in
    { statistic; p_value = Float.of_int exceed /. Float.of_int samples; best; samples }
  ;;
end

module Spa = struct
  type t =
    { statistic : float
    ; p_value_lower : float
    ; p_value_consistent : float
    ; p_value_upper : float
    ; best : int
    ; samples : int
    }
  [@@deriving sexp, compare, equal]

  let run ?scheme ?(samples = 1000) ~rng rows =
    let m, n = check_matrix ~who:"Spa" rows in
    let scheme = Option.value scheme ~default:(default_scheme n) in
    let nf = Float.of_int n in
    let root_n = Float.sqrt nf in
    let means = Array.map rows ~f:row_mean in
    let boot = bootstrap_means ~scheme ~samples ~rng rows ~n in
    (* omega_k^2: bootstrap variance of sqrt n * mean_k *)
    let omega =
      Array.init m ~f:(fun k ->
        let s = ref 0. in
        Array.iter boot ~f:(fun bm ->
          s := !s +. Float.square (root_n *. (bm.(k) -. means.(k))));
        Float.sqrt (!s /. Float.of_int samples))
    in
    (* A strategy whose bootstrap means never move (a constant row) has no sampling
       variation to studentize by; it takes no part in the statistic. *)
    let usable k = Float.( > ) omega.(k) 0. in
    let t_k =
      Array.init m ~f:(fun k ->
        if usable k then root_n *. means.(k) /. omega.(k) else Float.neg_infinity)
    in
    let best = argmax t_k in
    let statistic = Float.max 0. t_k.(best) in
    let loglog = if Float.( > ) nf Float.(exp 1.) then Float.log (Float.log nf) else 0. in
    let g_lower k = Float.max means.(k) 0. in
    let g_consistent k =
      let threshold = -.Float.sqrt (Float.square omega.(k) /. nf *. 2. *. loglog) in
      if Float.( >= ) means.(k) threshold then means.(k) else 0.
    in
    let g_upper k = means.(k) in
    let p_value g =
      let centre = Array.init m ~f:g in
      let exceed =
        Array.count boot ~f:(fun bm ->
          let v = ref 0. in
          for k = 0 to m - 1 do
            if usable k
            then v := Float.max !v (root_n *. (bm.(k) -. centre.(k)) /. omega.(k))
          done;
          Float.( >= ) !v statistic)
      in
      Float.of_int exceed /. Float.of_int samples
    in
    { statistic
    ; p_value_lower = p_value g_lower
    ; p_value_consistent = p_value g_consistent
    ; p_value_upper = p_value g_upper
    ; best
    ; samples
    }
  ;;
end

module Adjust = struct
  let check ps =
    Array.iter ps ~f:(fun p ->
      if not (Float.( >= ) p 0. && Float.( <= ) p 1.)
      then raise_s [%message "Adjust: p-value outside [0, 1]" (p : float)])
  ;;

  let bonferroni ps =
    check ps;
    let m = Float.of_int (Array.length ps) in
    Array.map ps ~f:(fun p -> Float.min 1. (m *. p))
  ;;

  let order ps =
    let idx = Array.init (Array.length ps) ~f:Fn.id in
    Array.stable_sort idx ~compare:(fun i j -> Float.compare ps.(i) ps.(j));
    idx
  ;;

  let holm ps =
    check ps;
    let m = Array.length ps in
    let idx = order ps in
    let out = Array.create ~len:m 0. in
    let running = ref 0. in
    Array.iteri idx ~f:(fun j i ->
      running := Float.max !running (Float.min 1. (Float.of_int (m - j) *. ps.(i)));
      out.(i) <- !running);
    out
  ;;

  let benjamini_hochberg ps =
    check ps;
    let m = Array.length ps in
    let idx = order ps in
    let out = Array.create ~len:m 0. in
    let running = ref 1. in
    for j = m - 1 downto 0 do
      let i = idx.(j) in
      running
      := Float.min
           !running
           (Float.min 1. (Float.of_int m *. ps.(i) /. Float.of_int (j + 1)));
      out.(i) <- !running
    done;
    out
  ;;

  let reject ~alpha adjusted = Array.map adjusted ~f:(fun p -> Float.( <= ) p alpha)
end

module Pbo = struct
  module Metric = struct
    type t =
      | Sharpe
      | Mean
    [@@deriving sexp, compare, equal]
  end

  type t =
    { pbo : float
    ; combinations : int
    ; strategies : int
    ; splits : int
    ; logits : float array
    ; in_sample : float array
    ; out_of_sample : float array
    ; degradation_slope : float
    ; probability_of_loss : float
    }
  [@@deriving sexp_of]

  (* All subsets of [0, s) of size s / 2, as bit masks, in increasing order. *)
  let half_subsets s =
    let k = s / 2 in
    let out = Queue.create () in
    for mask = 0 to (1 lsl s) - 1 do
      if Int.popcount mask = k then Queue.enqueue out mask
    done;
    Queue.to_array out
  ;;

  let performance (metric : Metric.t) ~count ~sum ~sum_sq =
    let nf = Float.of_int count in
    let mean = sum /. nf in
    match metric with
    | Mean -> mean
    | Sharpe ->
      let var = (sum_sq -. (nf *. mean *. mean)) /. (nf -. 1.) in
      if Float.( <= ) var 0. then 0. else mean /. Float.sqrt var
  ;;

  (* Rank of [xs.(i)] among [xs], ascending from 1, ties averaged. *)
  let relative_rank xs i =
    let below = Array.count xs ~f:(fun x -> Float.( < ) x xs.(i)) in
    let equal = Array.count xs ~f:(fun x -> Float.( = ) x xs.(i)) in
    Float.of_int below +. ((Float.of_int equal +. 1.) /. 2.)
  ;;

  let run ?(splits = 16) ?(metric = Metric.Sharpe) rows =
    let m, n = check_matrix ~who:"Pbo" rows in
    if m < 2 then raise_s [%message "Pbo: need at least two strategies" (m : int)];
    if splits < 2 || splits % 2 <> 0 || splits > n || splits > 24
    then
      raise_s
        [%message
          "Pbo: splits must be even, between 2 and min(24, periods)"
            (splits : int)
            (n : int)];
    let bound s = s * n / splits in
    (* per strategy and block: count, sum and sum of squares *)
    let stats =
      Array.map rows ~f:(fun row ->
        Array.init splits ~f:(fun s ->
          let sum = ref 0.
          and sq = ref 0. in
          for t = bound s to bound (s + 1) - 1 do
            sum := !sum +. row.(t);
            sq := !sq +. (row.(t) *. row.(t))
          done;
          bound (s + 1) - bound s, !sum, !sq))
    in
    let combos = half_subsets splits in
    let c = Array.length combos in
    let logits = Array.create ~len:c 0. in
    let is_perf = Array.create ~len:c 0. in
    let oos_perf = Array.create ~len:c 0. in
    let ins = Array.create ~len:m 0. in
    let oos = Array.create ~len:m 0. in
    Array.iteri combos ~f:(fun ci mask ->
      for k = 0 to m - 1 do
        let ic = ref 0
        and isum = ref 0.
        and isq = ref 0.
        and oc = ref 0
        and osum = ref 0.
        and osq = ref 0. in
        for s = 0 to splits - 1 do
          let cnt, sum, sq = stats.(k).(s) in
          if mask land (1 lsl s) <> 0
          then (
            ic := !ic + cnt;
            isum := !isum +. sum;
            isq := !isq +. sq)
          else (
            oc := !oc + cnt;
            osum := !osum +. sum;
            osq := !osq +. sq)
        done;
        ins.(k) <- performance metric ~count:!ic ~sum:!isum ~sum_sq:!isq;
        oos.(k) <- performance metric ~count:!oc ~sum:!osum ~sum_sq:!osq
      done;
      let star = argmax ins in
      let w = relative_rank oos star /. Float.of_int (m + 1) in
      logits.(ci) <- Float.log (w /. (1. -. w));
      is_perf.(ci) <- ins.(star);
      oos_perf.(ci) <- oos.(star));
    let cf = Float.of_int c in
    let slope =
      let mx = Array.fold is_perf ~init:0. ~f:( +. ) /. cf in
      let my = Array.fold oos_perf ~init:0. ~f:( +. ) /. cf in
      let sxy = ref 0.
      and sxx = ref 0. in
      for i = 0 to c - 1 do
        sxy := !sxy +. ((is_perf.(i) -. mx) *. (oos_perf.(i) -. my));
        sxx := !sxx +. Float.square (is_perf.(i) -. mx)
      done;
      if Float.( = ) !sxx 0. then Float.nan else !sxy /. !sxx
    in
    { pbo = Float.of_int (Array.count logits ~f:(fun l -> Float.( <= ) l 0.)) /. cf
    ; combinations = c
    ; strategies = m
    ; splits
    ; logits
    ; in_sample = is_perf
    ; out_of_sample = oos_perf
    ; degradation_slope = slope
    ; probability_of_loss =
        Float.of_int (Array.count oos_perf ~f:(fun x -> Float.( < ) x 0.)) /. cf
    }
  ;;
end
