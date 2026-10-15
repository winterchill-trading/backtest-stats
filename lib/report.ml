open! Core

module Trials = struct
  type t =
    { count : int
    ; sharpe_variance : float
    }
  [@@deriving sexp, compare, equal]
end

module Out_of_sample = struct
  type t =
    { periods : int
    ; sharpe_annualized : float
    ; psr : float
    }
  [@@deriving sexp, compare, equal]
end

type t =
  { name : string
  ; periods : int
  ; periods_per_year : float
  ; start : Time_ns.Alternate_sexp.t option
  ; stop : Time_ns.Alternate_sexp.t option
  ; total_return : float
  ; annualized_return : float
  ; annualized_volatility : float
  ; sharpe_annualized : float
  ; sharpe_autocorrelation_adjusted : float
  ; sortino_annualized : float
  ; skewness : float
  ; kurtosis : float
  ; psr_benchmark_sharpe : float
  ; psr : float
  ; min_track_record_length : float option
  ; trials : Trials.t option
  ; dsr : float option
  ; drawdown : Metrics.Drawdown.t
  ; drawdown_intervals : Metrics.Drawdown.intervals option
  ; out_of_sample : Out_of_sample.t option
  ; pbo : float option
  ; reality_check_p_value : float option
  ; spa : Multiple_testing.Spa.t option
  ; evidence : Gate.Evidence.t
  ; verdict : Gate.Verdict.t option
  }
[@@deriving sexp, compare, equal]

let default_pbo_splits periods =
  let s = Int.min 16 periods in
  if s % 2 = 0 then s else s - 1
;;

let create
  ?(name = "strategy")
  ?(out_of_sample_periods = 0)
  ?psr_benchmark_sharpe
  ?trials
  ?candidates
  ?benchmark
  ?pbo_splits
  ?(bootstrap_samples = 1000)
  ?(seed = 0)
  ?policy
  ~periods_per_year
  series
  =
  Or_error.try_with_join (fun () ->
    let open Or_error.Let_syntax in
    let n = Series.length series in
    let%bind () =
      if n < 3
      then Or_error.error_s [%message "Report: need at least three periods" (n : int)]
      else Ok ()
    in
    let%bind () =
      if out_of_sample_periods <> 0
         && (out_of_sample_periods < 3 || out_of_sample_periods > n)
      then
        Or_error.error_s
          [%message
            "Report: out-of-sample part must have 3 to n periods"
              (out_of_sample_periods : int)]
      else Ok ()
    in
    let%bind candidates =
      match candidates with
      | Some rows when Array.length rows >= 2 ->
        if Array.for_all rows ~f:(fun r -> Array.length r = n)
        then Ok (Some rows)
        else
          Or_error.error_s
            [%message "Report: candidate rows must match the series length"]
      | Some _ | None -> Ok None
    in
    let returns = Series.returns series in
    let moments = Metrics.Moments.of_array returns in
    let sharpe = Metrics.sharpe returns in
    let psr_benchmark_sharpe =
      match psr_benchmark_sharpe, policy with
      | Some b, _ -> b
      | None, Some (p : Gate.Policy.t) -> p.psr_benchmark_sharpe
      | None, None -> 0.
    in
    let psr =
      Metrics.probabilistic_sharpe_of_moments
        ~benchmark:psr_benchmark_sharpe
        ~sharpe
        moments
    in
    let trials =
      match trials, candidates with
      | Some t, _ -> Some t
      | None, Some rows ->
        Some
          { Trials.count = Array.length rows
          ; sharpe_variance = Metrics.trials_sharpe_variance rows
          }
      | None, None -> None
    in
    let dsr =
      Option.map trials ~f:(fun t ->
        Metrics.deflated_sharpe
          ~trials:t.count
          ~trials_sharpe_variance:t.sharpe_variance
          returns)
    in
    let rng = Random.State.make [| seed |] in
    let drawdown = Metrics.Drawdown.of_returns returns in
    let drawdown_intervals =
      Some (Metrics.Drawdown.confidence_intervals ~samples:bootstrap_samples ~rng returns)
    in
    let out_of_sample =
      if out_of_sample_periods = 0
      then None
      else (
        let oos =
          Array.sub returns ~pos:(n - out_of_sample_periods) ~len:out_of_sample_periods
        in
        Some
          { Out_of_sample.periods = out_of_sample_periods
          ; sharpe_annualized =
              Metrics.annualize_sharpe ~periods_per_year (Metrics.sharpe oos)
          ; psr = Metrics.probabilistic_sharpe ~benchmark:psr_benchmark_sharpe oos
          })
    in
    let pbo, reality_check_p_value, spa =
      match candidates with
      | None -> None, None, None
      | Some rows ->
        let splits = Option.value pbo_splits ~default:(default_pbo_splits n) in
        let pbo = (Multiple_testing.Pbo.run ~splits rows).pbo in
        let excess =
          match benchmark with
          | None -> rows
          | Some benchmark -> Multiple_testing.excess_over ~benchmark rows
        in
        let rc =
          Multiple_testing.Reality_check.run ~samples:bootstrap_samples ~rng excess
        in
        let spa = Multiple_testing.Spa.run ~samples:bootstrap_samples ~rng excess in
        Some pbo, Some rc.p_value, Some spa
    in
    let evidence =
      { Gate.Evidence.periods = n
      ; out_of_sample_periods
      ; psr = Some psr
      ; out_of_sample_psr = Option.map out_of_sample ~f:(fun o -> o.psr)
      ; dsr
      ; pbo
      ; spa_p_value = Option.map spa ~f:(fun s -> s.p_value_consistent)
      ; max_drawdown = Some drawdown.max_drawdown
      }
    in
    let total_return = Series.compound returns in
    let ppy_int = Int.max 1 (Float.iround_nearest_exn periods_per_year) in
    return
      { name
      ; periods = n
      ; periods_per_year
      ; start = Some (Series.time_at series 0)
      ; stop = Some (Series.time_at series (n - 1))
      ; total_return
      ; annualized_return =
          ((1. +. total_return) **. (periods_per_year /. Float.of_int n)) -. 1.
      ; annualized_volatility = moments.std *. Float.sqrt periods_per_year
      ; sharpe_annualized = Metrics.annualize_sharpe ~periods_per_year sharpe
      ; sharpe_autocorrelation_adjusted =
          Metrics.sharpe_autocorrelation_adjusted ~periods_per_year:ppy_int returns
      ; sortino_annualized = Metrics.sortino returns *. Float.sqrt periods_per_year
      ; skewness = moments.skewness
      ; kurtosis = moments.kurtosis
      ; psr_benchmark_sharpe
      ; psr
      ; min_track_record_length =
          Metrics.min_track_record_length ~benchmark:psr_benchmark_sharpe returns
      ; trials
      ; dsr
      ; drawdown
      ; drawdown_intervals
      ; out_of_sample
      ; pbo
      ; reality_check_p_value
      ; spa
      ; evidence
      ; verdict = Option.map policy ~f:(fun p -> Gate.evaluate p evidence)
      })
;;

let to_json t =
  let f = Json.float in
  let opt g = function
    | None -> `Null
    | Some x -> g x
  in
  let time x = `String (Time_ns.to_string_utc x) in
  `Assoc
    [ "name", `String t.name
    ; "periods", `Int t.periods
    ; "periods_per_year", f t.periods_per_year
    ; "start", opt time t.start
    ; "stop", opt time t.stop
    ; "total_return", f t.total_return
    ; "annualized_return", f t.annualized_return
    ; "annualized_volatility", f t.annualized_volatility
    ; "sharpe_annualized", f t.sharpe_annualized
    ; "sharpe_autocorrelation_adjusted", f t.sharpe_autocorrelation_adjusted
    ; "sortino_annualized", f t.sortino_annualized
    ; "skewness", f t.skewness
    ; "kurtosis", f t.kurtosis
    ; "psr_benchmark_sharpe", f t.psr_benchmark_sharpe
    ; "psr", f t.psr
    ; "min_track_record_length", opt f t.min_track_record_length
    ; ( "trials"
      , opt
          (fun (tr : Trials.t) ->
            `Assoc [ "count", `Int tr.count; "sharpe_variance", f tr.sharpe_variance ])
          t.trials )
    ; "dsr", opt f t.dsr
    ; "drawdown", Json.drawdown t.drawdown
    ; "drawdown_intervals", opt Json.drawdown_intervals t.drawdown_intervals
    ; ( "out_of_sample"
      , opt
          (fun (o : Out_of_sample.t) ->
            `Assoc
              [ "periods", `Int o.periods
              ; "sharpe_annualized", f o.sharpe_annualized
              ; "psr", f o.psr
              ])
          t.out_of_sample )
    ; "pbo", opt f t.pbo
    ; "reality_check_p_value", opt f t.reality_check_p_value
    ; "spa", opt Json.spa t.spa
    ; "evidence", Json.evidence t.evidence
    ; "verdict", opt Json.verdict t.verdict
    ]
;;
