open! Core

let float x = if Float.is_finite x then `Float x else `Null

let option f = function
  | None -> `Null
  | Some x -> f x
;;

let interval (i : Metrics.Interval.t) =
  `Assoc
    [ "estimate", float i.estimate
    ; "lower", float i.lower
    ; "upper", float i.upper
    ; "level", float i.level
    ]
;;

let drawdown (d : Metrics.Drawdown.t) =
  `Assoc
    [ "max_drawdown", float d.max_drawdown
    ; "max_duration", `Int d.max_duration
    ; "current", float d.current
    ; "mean", float d.mean
    ; "ulcer_index", float d.ulcer_index
    ]
;;

let drawdown_intervals (d : Metrics.Drawdown.intervals) =
  `Assoc
    [ "max_drawdown", interval d.max_drawdown; "max_duration", interval d.max_duration ]
;;

let reality_check (r : Multiple_testing.Reality_check.t) =
  `Assoc
    [ "statistic", float r.statistic
    ; "p_value", float r.p_value
    ; "best", `Int r.best
    ; "samples", `Int r.samples
    ]
;;

let spa (s : Multiple_testing.Spa.t) =
  `Assoc
    [ "statistic", float s.statistic
    ; "p_value_lower", float s.p_value_lower
    ; "p_value_consistent", float s.p_value_consistent
    ; "p_value_upper", float s.p_value_upper
    ; "best", `Int s.best
    ; "samples", `Int s.samples
    ]
;;

let pbo (p : Multiple_testing.Pbo.t) =
  `Assoc
    [ "pbo", float p.pbo
    ; "combinations", `Int p.combinations
    ; "strategies", `Int p.strategies
    ; "splits", `Int p.splits
    ; "degradation_slope", float p.degradation_slope
    ; "probability_of_loss", float p.probability_of_loss
    ]
;;

let var_test (t : Var_backtest.Test.t) =
  `Assoc
    [ "statistic", float t.statistic
    ; "p_value", float t.p_value
    ; "asymptotic_p_value", float t.asymptotic_p_value
    ; "exact", `Bool t.exact
    ]
;;

let var_backtest (v : Var_backtest.t) =
  let c = v.counts in
  `Assoc
    [ ( "counts"
      , `Assoc
          [ "observations", `Int c.observations
          ; "exceptions", `Int c.exceptions
          ; "n00", `Int c.n00
          ; "n01", `Int c.n01
          ; "n10", `Int c.n10
          ; "n11", `Int c.n11
          ] )
    ; "expected_rate", float v.expected_rate
    ; "unconditional_coverage", var_test v.unconditional_coverage
    ; "independence", var_test v.independence
    ; "conditional_coverage", var_test v.conditional_coverage
    ; "zone", `String (Sexp.to_string (Var_backtest.Traffic_light.sexp_of_zone v.zone))
    ]
;;

let evidence (e : Gate.Evidence.t) =
  `Assoc
    [ "periods", `Int e.periods
    ; "out_of_sample_periods", `Int e.out_of_sample_periods
    ; "psr", option float e.psr
    ; "out_of_sample_psr", option float e.out_of_sample_psr
    ; "dsr", option float e.dsr
    ; "pbo", option float e.pbo
    ; "spa_p_value", option float e.spa_p_value
    ; "max_drawdown", option float e.max_drawdown
    ]
;;

let reason (r : Gate.Reason.t) =
  let kind, fields =
    match r with
    | Too_few_periods { required; actual } ->
      "too_few_periods", [ "required", `Int required; "actual", `Int actual ]
    | Out_of_sample_too_short { required; actual } ->
      "out_of_sample_too_short", [ "required", float required; "actual", float actual ]
    | Psr_below { required; actual } ->
      "psr_below", [ "required", float required; "actual", float actual ]
    | Out_of_sample_psr_below { required; actual } ->
      "out_of_sample_psr_below", [ "required", float required; "actual", float actual ]
    | Dsr_below { required; actual } ->
      "dsr_below", [ "required", float required; "actual", float actual ]
    | Pbo_above { limit; actual } ->
      "pbo_above", [ "limit", float limit; "actual", float actual ]
    | Spa_p_value_above { limit; actual } ->
      "spa_p_value_above", [ "limit", float limit; "actual", float actual ]
    | Drawdown_above { limit; actual } ->
      "drawdown_above", [ "limit", float limit; "actual", float actual ]
    | Missing_evidence what -> "missing_evidence", [ "statistic", `String what ]
  in
  `Assoc
    ((("kind", `String kind) :: fields)
     @ [ "message", `String (Gate.Reason.to_string_hum r) ])
;;

let verdict : Gate.Verdict.t -> Yojson.Safe.t = function
  | Accept -> `Assoc [ "accept", `Bool true ]
  | Reject reasons ->
    `Assoc [ "accept", `Bool false; "reasons", `List (List.map reasons ~f:reason) ]
;;
