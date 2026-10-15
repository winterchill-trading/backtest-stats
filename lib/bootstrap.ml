open! Core

module Scheme = struct
  type t =
    | Iid
    | Circular_block of { length : int }
    | Stationary of { mean_block_length : float }
  [@@deriving sexp, compare, equal]

  let validate = function
    | Iid -> ()
    | Circular_block { length } ->
      if length < 1
      then raise_s [%message "Bootstrap: block length must be at least 1" (length : int)]
    | Stationary { mean_block_length } ->
      if not (Float.is_finite mean_block_length && Float.( >= ) mean_block_length 1.)
      then
        raise_s
          [%message
            "Bootstrap: mean block length must be at least 1" (mean_block_length : float)]
  ;;
end

let default_block_length n =
  if n <= 1
  then 1
  else Int.max 1 (Float.iround_nearest_exn (Float.of_int n **. (1. /. 3.)))
;;

let fill_indices (scheme : Scheme.t) ~rng dst =
  let n = Array.length dst in
  if n > 0
  then (
    match scheme with
    | Iid ->
      for i = 0 to n - 1 do
        dst.(i) <- Random.State.int rng n
      done
    | Circular_block { length } ->
      Scheme.validate scheme;
      let i = ref 0 in
      while !i < n do
        let start = Random.State.int rng n in
        let j = ref 0 in
        while !j < length && !i < n do
          dst.(!i) <- (start + !j) mod n;
          incr i;
          incr j
        done
      done
    | Stationary { mean_block_length } ->
      Scheme.validate scheme;
      (* Start a new block with probability 1 / L at each step: block lengths are
         geometric with mean L. *)
      let p_new = 1. /. mean_block_length in
      dst.(0) <- Random.State.int rng n;
      for i = 1 to n - 1 do
        if Float.( < ) (Random.State.float rng 1.) p_new
        then dst.(i) <- Random.State.int rng n
        else dst.(i) <- (dst.(i - 1) + 1) mod n
      done)
;;

let indices scheme ~rng ~n =
  let dst = Array.create ~len:n 0 in
  fill_indices scheme ~rng dst;
  dst
;;

let resample scheme ~rng xs =
  let idx = indices scheme ~rng ~n:(Array.length xs) in
  Array.map idx ~f:(fun i -> xs.(i))
;;

let series scheme ~rng s = Series.with_returns s (resample scheme ~rng (Series.returns s))

let quantile_sorted sorted q =
  let n = Array.length sorted in
  if n = 0 then raise_s [%message "Bootstrap.quantile_sorted: empty array"];
  let h = q *. Float.of_int (n - 1) in
  let lo = Int.clamp_exn (Float.iround_down_exn h) ~min:0 ~max:(n - 1) in
  let hi = Int.min (n - 1) (lo + 1) in
  let w = h -. Float.of_int lo in
  sorted.(lo) +. (w *. (sorted.(hi) -. sorted.(lo)))
;;

let percentile_interval ~level stats =
  if not (Float.( > ) level 0. && Float.( < ) level 1.)
  then
    raise_s
      [%message "Bootstrap.percentile_interval: level outside (0, 1)" (level : float)];
  Array.sort stats ~compare:Float.compare;
  let a = (1. -. level) /. 2. in
  quantile_sorted stats a, quantile_sorted stats (1. -. a)
;;
