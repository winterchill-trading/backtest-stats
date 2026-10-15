(** An acceptance policy for a backtested strategy, a parameter set or a ranking, kept as
    data so promoting one is a checked step with a stored verdict.

    A {!Policy.t} lists thresholds; an {!Evidence.t} holds the measured statistics;
    {!evaluate} compares them and returns every failed check as a typed reason. A policy
    that sets a threshold for a statistic the evidence lacks rejects with
    [Missing_evidence], so a gate cannot pass by omission. *)

open! Core

module Policy : sig
  (** Every field is optional in the sexp form; an absent threshold is not checked.
      Probabilities are fractions in [0, 1]; [psr_benchmark_sharpe] is a per-period Sharpe
      ratio that {!Report.create} measures the PSR against.

      {v
      ((min_periods 500)
       (min_out_of_sample_fraction 0.3)
       (min_psr 0.95)
       (min_out_of_sample_psr 0.9)
       (min_dsr 0.9)
       (max_pbo 0.2)
       (max_spa_p_value 0.05)
       (max_drawdown 0.25))
      v} *)
  type t =
    { min_periods : int
    ; min_out_of_sample_fraction : float
    ; psr_benchmark_sharpe : float
    ; min_psr : float option
    ; min_out_of_sample_psr : float option
    ; min_dsr : float option
    ; max_pbo : float option
    ; max_spa_p_value : float option
    ; max_drawdown : float option
    }
  [@@deriving sexp, compare, equal]

  (** No checks at all: accepts any evidence. *)
  val permissive : t

  (** A strict starting point: 250 periods, 30% out of sample, PSR and DSR at least 0.95,
      out-of-sample PSR at least 0.9, PBO at most 0.2, SPA p-value at most 0.05. *)
  val default : t

  (** Parses and validates (probabilities in [0, 1], counts non-negative). *)
  val of_sexp_checked : Sexp.t -> t Or_error.t

  val load : string -> t Or_error.t
end

module Evidence : sig
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

module Reason : sig
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

  (** One line for people, e.g. ["probability of backtest overfitting 0.41 above 0.20"]. *)
  val to_string_hum : t -> string
end

module Verdict : sig
  type t =
    | Accept
    | Reject of Reason.t list (** never empty *)
  [@@deriving sexp, compare, equal]

  val is_accept : t -> bool
end

val evaluate : Policy.t -> Evidence.t -> Verdict.t
