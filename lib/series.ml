open! Core

type t =
  { times : Time_ns.t array
  ; returns : float array
  }
[@@deriving sexp_of]

let validate ~times ~returns =
  let n = Array.length returns in
  if Array.length times <> n
  then
    Or_error.error_s
      [%message
        "Series: times and returns differ in length"
          ~times:(Array.length times : int)
          ~returns:(n : int)]
  else (
    let bad_return =
      Array.findi returns ~f:(fun _ r ->
        (not (Float.is_finite r)) || Float.( <= ) r (-1.))
    in
    let bad_time =
      Array.findi times ~f:(fun i time -> i > 0 && Time_ns.( <= ) time times.(i - 1))
    in
    match bad_return, bad_time with
    | Some (i, r), _ ->
      Or_error.error_s
        [%message "Series: return not finite or at or below -1" (i : int) (r : float)]
    | None, Some (i, time) ->
      Or_error.error_s
        [%message "Series: times must strictly increase" (i : int) (time : Time_ns.t)]
    | None, None -> Ok ())
;;

let create ~times ~returns =
  let%map.Or_error () = validate ~times ~returns in
  { times = Array.copy times; returns = Array.copy returns }
;;

let of_returns ?(start = Time_ns.epoch) ?(step = Time_ns.Span.day) returns =
  let times =
    Array.init (Array.length returns) ~f:(fun i ->
      Time_ns.add start (Time_ns.Span.scale_int step i))
  in
  create ~times ~returns |> Or_error.ok_exn
;;

let of_prices ~times ~prices =
  let n = Array.length prices in
  if Array.length times <> n
  then Or_error.error_s [%message "Series.of_prices: times and prices differ in length"]
  else (
    match Array.find prices ~f:(fun p -> not (Float.is_finite p && Float.( > ) p 0.)) with
    | Some p ->
      Or_error.error_s [%message "Series.of_prices: price must be positive" (p : float)]
    | None ->
      if n < 2
      then create ~times:[||] ~returns:[||]
      else (
        let returns =
          Array.init (n - 1) ~f:(fun i -> (prices.(i + 1) /. prices.(i)) -. 1.)
        in
        create ~times:(Array.sub times ~pos:1 ~len:(n - 1)) ~returns))
;;

let of_pnl ~times ~pnl ~capital =
  if not (Float.is_finite capital && Float.( > ) capital 0.)
  then
    Or_error.error_s
      [%message "Series.of_pnl: capital must be positive" (capital : float)]
  else create ~times ~returns:(Array.map pnl ~f:(fun x -> x /. capital))
;;

let length t = Array.length t.returns
let is_empty t = length t = 0
let returns t = Array.copy t.returns
let times t = Array.copy t.times
let return_at t i = t.returns.(i)
let time_at t i = t.times.(i)

let sub t ~pos ~len =
  { times = Array.sub t.times ~pos ~len; returns = Array.sub t.returns ~pos ~len }
;;

let split t ~in_sample_fraction =
  if not (Float.( >= ) in_sample_fraction 0. && Float.( <= ) in_sample_fraction 1.)
  then
    raise_s
      [%message "Series.split: fraction outside [0, 1]" (in_sample_fraction : float)];
  let n = length t in
  let k = Float.iround_down_exn (in_sample_fraction *. Float.of_int n) in
  sub t ~pos:0 ~len:k, sub t ~pos:k ~len:(n - k)
;;

let compound returns =
  Array.fold returns ~init:1. ~f:(fun acc r -> acc *. (1. +. r)) -. 1.
;;

let resample t ~every =
  if Time_ns.Span.( <= ) every Time_ns.Span.zero
  then
    raise_s [%message "Series.resample: span must be positive" (every : Time_ns.Span.t)];
  let every_ns = Time_ns.Span.to_int_ns every in
  let bucket time = Int.( /% ) (Time_ns.to_int_ns_since_epoch time) every_ns in
  let times = Queue.create () in
  let returns = Queue.create () in
  let n = length t in
  let i = ref 0 in
  while !i < n do
    let b = bucket t.times.(!i) in
    let growth = ref 1. in
    let last = ref !i in
    while !i < n && bucket t.times.(!i) = b do
      growth := !growth *. (1. +. t.returns.(!i));
      last := !i;
      incr i
    done;
    Queue.enqueue times t.times.(!last);
    Queue.enqueue returns (!growth -. 1.)
  done;
  { times = Queue.to_array times; returns = Queue.to_array returns }
;;

let excess t ~over =
  if length t <> length over
     || not (Array.for_all2_exn t.times over.times ~f:Time_ns.equal)
  then
    Or_error.error_s [%message "Series.excess: the two series must have the same times"]
  else
    Ok
      { times = Array.copy t.times
      ; returns = Array.map2_exn t.returns over.returns ~f:(fun a b -> a -. b)
      }
;;

let log_returns t = Array.map t.returns ~f:Float.log1p

let equity_curve ?(start = 1.) t =
  let v = ref start in
  Array.map t.returns ~f:(fun r ->
    v := !v *. (1. +. r);
    !v)
;;

let with_returns t returns =
  if Array.length returns <> length t
  then raise_s [%message "Series.with_returns: length differs"];
  { t with returns }
;;

let infer_periods_per_year t =
  let n = length t in
  if n < 2 then raise_s [%message "Series.infer_periods_per_year: need two periods"];
  let gaps =
    Array.init (n - 1) ~f:(fun i ->
      Time_ns.Span.to_day (Time_ns.diff t.times.(i + 1) t.times.(i)))
  in
  Array.sort gaps ~compare:Float.compare;
  let median =
    if (n - 1) % 2 = 1
    then gaps.((n - 1) / 2)
    else (gaps.(((n - 1) / 2) - 1) +. gaps.((n - 1) / 2)) /. 2.
  in
  365.25 /. median
;;
