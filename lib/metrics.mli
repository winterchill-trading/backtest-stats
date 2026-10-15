(** Performance statistics of one return series, with the corrections that make a Sharpe
    ratio comparable across track records: for autocorrelation (when annualizing), for
    skewness and fat tails (the probabilistic Sharpe ratio and its standard error), and
    for selection among many trials (the deflated Sharpe ratio).

    Unless a function says otherwise, a Sharpe ratio here is per period (not annualized)
    and returns are simple per-period fractions. Annualize with {!annualize_sharpe}. *)

open! Core

module Moments : sig
  (** Sample moments of a series. [std] uses the [n - 1] denominator; [skewness] and
      [kurtosis] are the moment ratios m3 / m2^1.5 and m4 / m2^2 with [1 / n] central
      moments. [kurtosis] is not excess kurtosis: it is 3 for a normal distribution. *)
  type t =
    { n : int
    ; mean : float
    ; std : float
    ; skewness : float
    ; kurtosis : float
    }
  [@@deriving sexp, compare, equal]

  (** Raises on fewer than two observations. *)
  val of_array : float array -> t
end

module Interval : sig
  type t =
    { estimate : float
    ; lower : float
    ; upper : float
    ; level : float
    }
  [@@deriving sexp, compare, equal]
end

val mean : float array -> float

(** Sample standard deviation ([n - 1] denominator). *)
val std : float array -> float

(** Sample autocorrelation at [lag] (the [1 / n] estimator). *)
val autocorrelation : float array -> lag:int -> float

(** [sharpe ?risk_free returns] is mean excess return over its standard deviation, per
    period. [risk_free] is a per-period rate (default 0). *)
val sharpe : ?risk_free:float -> float array -> float

(** [annualize_sharpe ~periods_per_year sr] multiplies by the square root of the number of
    periods per year: correct only for serially uncorrelated returns. *)
val annualize_sharpe : periods_per_year:float -> float -> float

(** [sharpe_autocorrelation_adjusted ~periods_per_year returns] annualizes the per-period
    Sharpe ratio with the scaling factor for serially correlated returns,
    [q / sqrt (q + 2 * sum_{k=1}^{q-1} (q - k) rho_k)] with [q = periods_per_year], in
    place of [sqrt q]. Autocorrelations beyond [max_lag] (default 10) are taken as zero,
    which keeps estimation noise from dominating the sum when [q] is large. Positive
    autocorrelation lowers the annualized ratio. Returns NaN when the variance term is not
    positive (strong negative autocorrelation). *)
val sharpe_autocorrelation_adjusted
  :  ?risk_free:float
  -> ?max_lag:int
  -> periods_per_year:int
  -> float array
  -> float

(** [adjusted_sharpe returns] corrects the per-period Sharpe ratio for skewness and excess
    kurtosis: [sr * (1 + skew / 6 * sr - (kurt - 3) / 24 * sr^2)]. Negative skewness and
    fat tails lower it. *)
val adjusted_sharpe : ?risk_free:float -> float array -> float

(** [sortino ?target returns] is [(mean - target) / downside deviation] with the downside
    deviation [sqrt (1/n * sum min(0, r - target)^2)], per period. Infinite when no return
    falls below [target]. *)
val sortino : ?target:float -> float array -> float

(** Standard error of the estimated per-period Sharpe ratio [sr] for non-normal returns:
    [sqrt ((1 - skew * sr + (kurt - 1) / 4 * sr^2) / (n - 1))]. *)
val sharpe_standard_error : sharpe:float -> Moments.t -> float

(** Two-sided normal-approximation interval for the per-period Sharpe ratio using
    {!sharpe_standard_error}. *)
val sharpe_confidence_interval : ?level:float -> float array -> Interval.t

(** [probabilistic_sharpe ?benchmark returns] is the probability that the true per-period
    Sharpe ratio exceeds [benchmark] (default 0), given the sample's length, skewness and
    kurtosis:
    [Phi ((sr - benchmark) * sqrt (n - 1) / sqrt (1 - skew * sr + (kurt - 1) / 4 * sr^2))]. *)
val probabilistic_sharpe : ?benchmark:float -> float array -> float

(** The same from summary numbers. *)
val probabilistic_sharpe_of_moments
  :  benchmark:float
  -> sharpe:float
  -> Moments.t
  -> float

(** [expected_max_sharpe ~trials ~variance] is the expected maximum of [trials]
    independent per-period Sharpe estimates with true value zero and the given variance
    across trials:
    [sqrt variance * ((1 - g) * Phi^-1 (1 - 1/N) + g * Phi^-1 (1 - 1/(N e)))] with [g] the
    Euler-Mascheroni constant. Zero for one trial. *)
val expected_max_sharpe : trials:int -> variance:float -> float

(** [deflated_sharpe ~trials ~trials_sharpe_variance returns] is the probabilistic Sharpe
    ratio of [returns] against the benchmark {!expected_max_sharpe}: the probability that
    the selected strategy's true Sharpe ratio is positive after accounting for the
    [trials] tried to find it. [trials_sharpe_variance] is the variance of the per-period
    Sharpe ratios of all trials. *)
val deflated_sharpe : trials:int -> trials_sharpe_variance:float -> float array -> float

(** The variance of the per-period Sharpe ratios of a set of trials (sample variance). *)
val trials_sharpe_variance : float array array -> float

(** [min_track_record_length ?benchmark ?confidence returns] is the number of periods
    needed for the probabilistic Sharpe ratio to reach [confidence] (default 0.95) at the
    observed Sharpe ratio, skewness and kurtosis:
    [1 + (1 - skew * sr + (kurt - 1) / 4 * sr^2) * (z / (sr - benchmark))^2]. [None] when
    the observed ratio does not exceed the benchmark. *)
val min_track_record_length
  :  ?benchmark:float
  -> ?confidence:float
  -> float array
  -> float option

module Drawdown : sig
  (** Drawdowns of the compounded equity curve starting at 1. [max_drawdown] is the
      largest fall from a running peak as a fraction of the peak; [max_duration] is the
      longest run of periods below the running peak; [current] is the drawdown after the
      last period; [mean] is the average drawdown over all periods; [ulcer_index] is the
      root mean square drawdown. *)
  type t =
    { max_drawdown : float
    ; max_duration : int
    ; current : float
    ; mean : float
    ; ulcer_index : float
    }
  [@@deriving sexp, compare, equal]

  val of_returns : float array -> t
  val max_drawdown : float array -> float

  type intervals =
    { max_drawdown : Interval.t
    ; max_duration : Interval.t
    }
  [@@deriving sexp, compare, equal]

  (** Bootstrap percentile intervals for the maximum drawdown and its duration ([samples]
      resamples, default 1000; [level] default 0.95; scheme default stationary with
      {!Bootstrap.default_block_length}). The maximum drawdown depends on the ordering of
      returns, so a block scheme keeps runs of losses together. *)
  val confidence_intervals
    :  ?scheme:Bootstrap.Scheme.t
    -> ?samples:int
    -> ?level:float
    -> rng:Random.State.t
    -> float array
    -> intervals
end
