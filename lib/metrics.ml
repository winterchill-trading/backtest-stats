open! Core

let euler_mascheroni = 0.5772156649015329

let mean xs =
  let n = Array.length xs in
  if n = 0 then raise_s [%message "Metrics.mean: empty series"];
  (* for loops over [let mutable] floats keep the accumulators unboxed; a fold with a
     closure allocates a boxed float per element *)
  let mutable s = 0. in
  for i = 0 to n - 1 do
    s <- s +. Array.unsafe_get xs i
  done;
  s /. Float.of_int n
;;

let std xs =
  let n = Array.length xs in
  if n < 2 then raise_s [%message "Metrics.std: need at least two observations" (n : int)];
  let m = mean xs in
  let mutable ss = 0. in
  for i = 0 to n - 1 do
    let d = Array.unsafe_get xs i -. m in
    ss <- ss +. (d *. d)
  done;
  Float.sqrt (ss /. Float.of_int (n - 1))
;;

module Moments = struct
  type t =
    { n : int
    ; mean : float
    ; std : float
    ; skewness : float
    ; kurtosis : float
    }
  [@@deriving sexp, compare, equal]

  let of_array xs =
    let n = Array.length xs in
    if n < 2
    then raise_s [%message "Metrics.Moments: need at least two observations" (n : int)];
    let m = mean xs in
    let mutable s2 = 0. in
    let mutable s3 = 0. in
    let mutable s4 = 0. in
    for i = 0 to n - 1 do
      let d = Array.unsafe_get xs i -. m in
      let d2 = d *. d in
      s2 <- s2 +. d2;
      s3 <- s3 +. (d2 *. d);
      s4 <- s4 +. (d2 *. d2)
    done;
    let nf = Float.of_int n in
    let m2 = s2 /. nf
    and m3 = s3 /. nf
    and m4 = s4 /. nf in
    { n
    ; mean = m
    ; std = Float.sqrt (m2 *. nf /. (nf -. 1.))
    ; skewness = m3 /. (m2 **. 1.5)
    ; kurtosis = m4 /. (m2 *. m2)
    }
  ;;
end

module Interval = struct
  type t =
    { estimate : float
    ; lower : float
    ; upper : float
    ; level : float
    }
  [@@deriving sexp, compare, equal]
end

let autocorrelation xs ~lag =
  let n = Array.length xs in
  if lag < 0 || lag >= n
  then
    raise_s [%message "Metrics.autocorrelation: lag out of range" (lag : int) (n : int)];
  let m = mean xs in
  let mutable num = 0. in
  let mutable den = 0. in
  for t = 0 to n - 1 do
    let d = xs.(t) -. m in
    den <- den +. (d *. d);
    if t >= lag then num <- num +. (d *. (xs.(t - lag) -. m))
  done;
  num /. den
;;

let sharpe ?(risk_free = 0.) xs = (mean xs -. risk_free) /. std xs
let annualize_sharpe ~periods_per_year sr = sr *. Float.sqrt periods_per_year

let sharpe_autocorrelation_adjusted ?risk_free ?(max_lag = 10) ~periods_per_year xs =
  let q = periods_per_year in
  if q < 1 then raise_s [%message "Metrics: periods_per_year must be positive" (q : int)];
  let lags = Int.min (q - 1) (Int.min max_lag (Array.length xs - 1)) in
  let sum = ref 0. in
  for k = 1 to lags do
    sum := !sum +. (Float.of_int (q - k) *. autocorrelation xs ~lag:k)
  done;
  let denom = Float.of_int q +. (2. *. !sum) in
  if Float.( <= ) denom 0.
  then Float.nan
  else sharpe ?risk_free xs *. Float.of_int q /. Float.sqrt denom
;;

let adjusted_sharpe ?risk_free xs =
  let m = Moments.of_array xs in
  let sr = sharpe ?risk_free xs in
  sr *. (1. +. (m.skewness /. 6. *. sr) -. ((m.kurtosis -. 3.) /. 24. *. sr *. sr))
;;

let sortino ?(target = 0.) xs =
  let n = Array.length xs in
  let mutable acc = 0. in
  for i = 0 to n - 1 do
    let d = Float.min 0. (Array.unsafe_get xs i -. target) in
    acc <- acc +. (d *. d)
  done;
  let dd = Float.sqrt (acc /. Float.of_int n) in
  let excess = mean xs -. target in
  if Float.( = ) dd 0.
  then if Float.( > ) excess 0. then Float.infinity else Float.nan
  else excess /. dd
;;

let sharpe_variance_term ~sharpe:sr (m : Moments.t) =
  1. -. (m.skewness *. sr) +. ((m.kurtosis -. 1.) /. 4. *. sr *. sr)
;;

let sharpe_standard_error ~sharpe (m : Moments.t) =
  Float.sqrt (sharpe_variance_term ~sharpe m /. Float.of_int (m.n - 1))
;;

let sharpe_confidence_interval ?(level = 0.95) xs =
  let m = Moments.of_array xs in
  let sr = sharpe xs in
  let se = sharpe_standard_error ~sharpe:sr m in
  let z = Dist.normal_quantile (1. -. ((1. -. level) /. 2.)) in
  { Interval.estimate = sr; lower = sr -. (z *. se); upper = sr +. (z *. se); level }
;;

let probabilistic_sharpe_of_moments ~benchmark ~sharpe (m : Moments.t) =
  let v = sharpe_variance_term ~sharpe m in
  Dist.normal_cdf
    ((sharpe -. benchmark) *. Float.sqrt (Float.of_int (m.n - 1)) /. Float.sqrt v)
;;

let probabilistic_sharpe ?(benchmark = 0.) xs =
  probabilistic_sharpe_of_moments ~benchmark ~sharpe:(sharpe xs) (Moments.of_array xs)
;;

let expected_max_sharpe ~trials ~variance =
  if trials < 1
  then raise_s [%message "Metrics: trials must be at least 1" (trials : int)];
  if Float.( < ) variance 0.
  then raise_s [%message "Metrics: variance must be non-negative" (variance : float)];
  if trials = 1
  then 0.
  else (
    let n = Float.of_int trials in
    Float.sqrt variance
    *. (((1. -. euler_mascheroni) *. Dist.normal_quantile (1. -. (1. /. n)))
        +. (euler_mascheroni *. Dist.normal_quantile (1. -. (1. /. (n *. Float.exp 1.))))
       ))
;;

let deflated_sharpe ~trials ~trials_sharpe_variance xs =
  let benchmark = expected_max_sharpe ~trials ~variance:trials_sharpe_variance in
  probabilistic_sharpe ~benchmark xs
;;

let trials_sharpe_variance trials =
  let srs = Array.map trials ~f:(fun xs -> sharpe xs) in
  if Array.length srs < 2 then 0. else Float.square (std srs)
;;

let min_track_record_length ?(benchmark = 0.) ?(confidence = 0.95) xs =
  let m = Moments.of_array xs in
  let sr = sharpe xs in
  if Float.( <= ) sr benchmark
  then None
  else (
    let z = Dist.normal_quantile confidence in
    Some
      (1. +. (sharpe_variance_term ~sharpe:sr m *. Float.square (z /. (sr -. benchmark)))))
;;

module Drawdown = struct
  type t =
    { max_drawdown : float
    ; max_duration : int
    ; current : float
    ; mean : float
    ; ulcer_index : float
    }
  [@@deriving sexp, compare, equal]

  let of_returns xs =
    let n = Array.length xs in
    let mutable value = 1. in
    let mutable peak = 1. in
    let mutable max_dd = 0. in
    let mutable run = 0 in
    let mutable max_run = 0 in
    let mutable sum = 0. in
    let mutable sum_sq = 0. in
    let mutable dd = 0. in
    for i = 0 to n - 1 do
      value <- value *. (1. +. Array.unsafe_get xs i);
      if Float.( >= ) value peak
      then (
        peak <- value;
        run <- 0)
      else run <- run + 1;
      dd <- 1. -. (value /. peak);
      (* a comparison instead of [Float.max], whose call boxes its result *)
      if Float.( > ) dd max_dd then max_dd <- dd;
      if run > max_run then max_run <- run;
      sum <- sum +. dd;
      sum_sq <- sum_sq +. (dd *. dd)
    done;
    let nf = Float.of_int (Int.max 1 n) in
    { max_drawdown = max_dd
    ; max_duration = max_run
    ; current = dd
    ; mean = sum /. nf
    ; ulcer_index = Float.sqrt (sum_sq /. nf)
    }
  ;;

  let max_drawdown xs = (of_returns xs).max_drawdown

  type intervals =
    { max_drawdown : Interval.t
    ; max_duration : Interval.t
    }
  [@@deriving sexp, compare, equal]

  let confidence_intervals ?scheme ?(samples = 1000) ?(level = 0.95) ~rng xs =
    let n = Array.length xs in
    let scheme =
      Option.value
        scheme
        ~default:
          (Bootstrap.Scheme.Stationary
             { mean_block_length = Float.of_int (Bootstrap.default_block_length n) })
    in
    let est = of_returns xs in
    let idx = Array.create ~len:n 0 in
    let buf = Array.create ~len:n 0. in
    let dds = Array.create ~len:samples 0. in
    let durs = Array.create ~len:samples 0. in
    for b = 0 to samples - 1 do
      Bootstrap.fill_indices scheme ~rng idx;
      for i = 0 to n - 1 do
        buf.(i) <- xs.(idx.(i))
      done;
      let d = of_returns buf in
      dds.(b) <- d.max_drawdown;
      durs.(b) <- Float.of_int d.max_duration
    done;
    let interval estimate stats =
      let lower, upper = Bootstrap.percentile_interval ~level stats in
      { Interval.estimate; lower; upper; level }
    in
    { max_drawdown = interval est.max_drawdown dds
    ; max_duration = interval (Float.of_int est.max_duration) durs
    }
  ;;
end
