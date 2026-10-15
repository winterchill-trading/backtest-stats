(** Everything this library measures about one strategy, in one value: the record other
    tools store next to a backtest or a ranking and show to people. It carries the
    {!Gate.Evidence.t} it was judged on and, when a policy was given, the verdict.

    Ratios are annualized with [periods_per_year] where the field name says so;
    probabilities are fractions. *)

open! Core

module Trials : sig
  (** How many configurations were tried before this one was picked, and the variance of
      their per-period Sharpe ratios: the inputs of the deflated Sharpe ratio. *)
  type t =
    { count : int
    ; sharpe_variance : float
    }
  [@@deriving sexp, compare, equal]
end

module Out_of_sample : sig
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
  ; psr_benchmark_sharpe : float (** per period *)
  ; psr : float
  ; min_track_record_length : float option (** periods *)
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

(** [create ~periods_per_year series] measures [series].

    - [out_of_sample_periods]: the last periods of [series] that were not used to choose
      or fit the strategy (default 0). Their PSR is reported separately.
    - [candidates]: every configuration tried, one row of per-period returns each, over
      the same periods as [series] (the reported one among them). With two or more rows
      the report includes the PBO ([pbo_splits], default the largest even number up to 16
      that the length allows), the reality check and SPA p-values against [benchmark]
      (per-period benchmark returns, default zero), and, unless [trials] is given, the
      trial count and Sharpe variance for the deflated Sharpe ratio.
    - [psr_benchmark_sharpe]: per-period Sharpe ratio the PSR is measured against
      (default: the policy's, else 0).
    - [bootstrap_samples] (default 1000) and [seed] (default 0) drive every resampling, so
      a report is reproducible.

    Errors when the series has fewer than three periods, a candidate row's length differs
    from the series, or the out-of-sample part is shorter than three periods. *)
val create
  :  ?name:string
  -> ?out_of_sample_periods:int
  -> ?psr_benchmark_sharpe:float
  -> ?trials:Trials.t
  -> ?candidates:float array array
  -> ?benchmark:float array
  -> ?pbo_splits:int
  -> ?bootstrap_samples:int
  -> ?seed:int
  -> ?policy:Gate.Policy.t
  -> periods_per_year:float
  -> Series.t
  -> t Or_error.t

(** JSON form, field names as in the record; non-finite numbers are [null]. *)
val to_json : t -> Yojson.Safe.t
