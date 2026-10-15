open! Core

module Policy = struct
  type t =
    { min_periods : int [@default 0]
    ; min_out_of_sample_fraction : float [@default 0.]
    ; psr_benchmark_sharpe : float [@default 0.]
    ; min_psr : float option [@sexp.option]
    ; min_out_of_sample_psr : float option [@sexp.option]
    ; min_dsr : float option [@sexp.option]
    ; max_pbo : float option [@sexp.option]
    ; max_spa_p_value : float option [@sexp.option]
    ; max_drawdown : float option [@sexp.option]
    }
  [@@deriving sexp, compare, equal]

  let permissive =
    { min_periods = 0
    ; min_out_of_sample_fraction = 0.
    ; psr_benchmark_sharpe = 0.
    ; min_psr = None
    ; min_out_of_sample_psr = None
    ; min_dsr = None
    ; max_pbo = None
    ; max_spa_p_value = None
    ; max_drawdown = None
    }
  ;;

  let default =
    { permissive with
      min_periods = 250
    ; min_out_of_sample_fraction = 0.3
    ; min_psr = Some 0.95
    ; min_out_of_sample_psr = Some 0.9
    ; min_dsr = Some 0.95
    ; max_pbo = Some 0.2
    ; max_spa_p_value = Some 0.05
    }
  ;;

  let validate t =
    let prob name = function
      | Some p when not (Float.( >= ) p 0. && Float.( <= ) p 1.) ->
        Some
          (Error.create_s
             [%message "Gate.Policy: probability outside [0, 1]" name (p : float)])
      | _ -> None
    in
    let errors =
      List.filter_opt
        [ (if t.min_periods < 0
           then Some (Error.of_string "Gate.Policy: min_periods is negative")
           else None)
        ; prob "min_out_of_sample_fraction" (Some t.min_out_of_sample_fraction)
        ; prob "min_psr" t.min_psr
        ; prob "min_out_of_sample_psr" t.min_out_of_sample_psr
        ; prob "min_dsr" t.min_dsr
        ; prob "max_pbo" t.max_pbo
        ; prob "max_spa_p_value" t.max_spa_p_value
        ; (match t.max_drawdown with
           | Some d when not (Float.( >= ) d 0.) ->
             Some (Error.of_string "Gate.Policy: max_drawdown is negative")
           | _ -> None)
        ]
    in
    if List.is_empty errors then Ok t else Error (Error.of_list errors)
  ;;

  let of_sexp_checked sexp = Or_error.try_with_join (fun () -> validate (t_of_sexp sexp))
  let load path = Or_error.try_with_join (fun () -> of_sexp_checked (Sexp.load_sexp path))
end

module Evidence = struct
  type t =
    { periods : int
    ; out_of_sample_periods : int
    ; psr : float option
    ; out_of_sample_psr : float option
    ; dsr : float option
    ; pbo : float option
    ; spa_p_value : float option
    ; max_drawdown : float option
    }
  [@@deriving sexp, compare, equal]
end

module Reason = struct
  type t =
    | Too_few_periods of
        { required : int
        ; actual : int
        }
    | Out_of_sample_too_short of
        { required : float
        ; actual : float
        }
    | Psr_below of
        { required : float
        ; actual : float
        }
    | Out_of_sample_psr_below of
        { required : float
        ; actual : float
        }
    | Dsr_below of
        { required : float
        ; actual : float
        }
    | Pbo_above of
        { limit : float
        ; actual : float
        }
    | Spa_p_value_above of
        { limit : float
        ; actual : float
        }
    | Drawdown_above of
        { limit : float
        ; actual : float
        }
    | Missing_evidence of string
  [@@deriving sexp, compare, equal]

  let to_string_hum = function
    | Too_few_periods { required; actual } ->
      sprintf "%d periods, at least %d required" actual required
    | Out_of_sample_too_short { required; actual } ->
      sprintf "out-of-sample share %.2f below %.2f" actual required
    | Psr_below { required; actual } ->
      sprintf "probabilistic Sharpe ratio %.3f below %.3f" actual required
    | Out_of_sample_psr_below { required; actual } ->
      sprintf "out-of-sample probabilistic Sharpe ratio %.3f below %.3f" actual required
    | Dsr_below { required; actual } ->
      sprintf "deflated Sharpe ratio %.3f below %.3f" actual required
    | Pbo_above { limit; actual } ->
      sprintf "probability of backtest overfitting %.2f above %.2f" actual limit
    | Spa_p_value_above { limit; actual } ->
      sprintf "SPA p-value %.3f above %.3f" actual limit
    | Drawdown_above { limit; actual } ->
      sprintf "maximum drawdown %.3f above %.3f" actual limit
    | Missing_evidence what -> sprintf "%s required by the policy but not measured" what
  ;;
end

module Verdict = struct
  type t =
    | Accept
    | Reject of Reason.t list
  [@@deriving sexp, compare, equal]

  let is_accept = function
    | Accept -> true
    | Reject _ -> false
  ;;
end

let evaluate (policy : Policy.t) (e : Evidence.t) =
  let min_check ~what threshold actual reason =
    match threshold, actual with
    | None, _ -> None
    | Some _, None -> Some (Reason.Missing_evidence what)
    | Some required, Some actual ->
      (* NaN evidence fails the check rather than slipping through a comparison *)
      if Float.( >= ) actual required then None else Some (reason ~required ~actual)
  in
  let max_check ~what threshold actual reason =
    match threshold, actual with
    | None, _ -> None
    | Some _, None -> Some (Reason.Missing_evidence what)
    | Some limit, Some actual ->
      if Float.( <= ) actual limit then None else Some (reason ~limit ~actual)
  in
  let oos_fraction =
    if e.periods = 0
    then 0.
    else Float.of_int e.out_of_sample_periods /. Float.of_int e.periods
  in
  let reasons =
    List.filter_opt
      [ (if e.periods < policy.min_periods
         then
           Some
             (Reason.Too_few_periods { required = policy.min_periods; actual = e.periods })
         else None)
      ; (if Float.( < ) oos_fraction policy.min_out_of_sample_fraction
         then
           Some
             (Reason.Out_of_sample_too_short
                { required = policy.min_out_of_sample_fraction; actual = oos_fraction })
         else None)
      ; min_check
          ~what:"probabilistic Sharpe ratio"
          policy.min_psr
          e.psr
          (fun ~required ~actual -> Psr_below { required; actual })
      ; min_check
          ~what:"out-of-sample probabilistic Sharpe ratio"
          policy.min_out_of_sample_psr
          e.out_of_sample_psr
          (fun ~required ~actual -> Out_of_sample_psr_below { required; actual })
      ; min_check
          ~what:"deflated Sharpe ratio"
          policy.min_dsr
          e.dsr
          (fun ~required ~actual -> Dsr_below { required; actual })
      ; max_check
          ~what:"probability of backtest overfitting"
          policy.max_pbo
          e.pbo
          (fun ~limit ~actual -> Pbo_above { limit; actual })
      ; max_check
          ~what:"SPA p-value"
          policy.max_spa_p_value
          e.spa_p_value
          (fun ~limit ~actual -> Spa_p_value_above { limit; actual })
      ; max_check
          ~what:"maximum drawdown"
          policy.max_drawdown
          e.max_drawdown
          (fun ~limit ~actual -> Drawdown_above { limit; actual })
      ]
  in
  if List.is_empty reasons then Verdict.Accept else Reject reasons
;;
