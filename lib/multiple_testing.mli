(** Statistics for choosing the best of many strategies without fooling yourself.

    Every function here takes a performance matrix as [float array array] with one row per
    strategy and one column per period; all rows have the same length. For the reality
    check and the SPA test a row holds the strategy's return minus the benchmark's return
    in that period ({!excess_over} builds it), so positive means the strategy did better. *)

open! Core

(** [excess_over ~benchmark rows] subtracts the benchmark from every row. *)
val excess_over : benchmark:float array -> float array array -> float array array

(** White's reality check for data snooping: tests the null hypothesis that no strategy
    beats the benchmark in expected performance, with the best of them chosen after the
    fact. The statistic is [max_k sqrt n * mean_k]; its null distribution comes from
    re-centred bootstrap means [sqrt n * (mean*_k - mean_k)], resampling all strategies
    with the same period indices. *)
module Reality_check : sig
  type t =
    { statistic : float
    ; p_value : float
    ; best : int (** row index of the best strategy in sample *)
    ; samples : int
    }
  [@@deriving sexp, compare, equal]

  (** Defaults: 1000 resamples, stationary bootstrap with
      {!Bootstrap.default_block_length}. *)
  val run
    :  ?scheme:Bootstrap.Scheme.t
    -> ?samples:int
    -> rng:Random.State.t
    -> float array array
    -> t
end

(** The test for superior predictive ability (SPA): the reality check with each mean
    studentized by its bootstrap standard deviation and with poor strategies kept from
    inflating the null distribution. Three p-values bracket the answer: [upper] re-centres
    every strategy (as the reality check does, conservative), [lower] re-centres only
    strategies that beat the benchmark (liberal), and [consistent] drops a strategy from
    re-centring when its mean is below [-sqrt (omega^2 / n * 2 log log n)]; [consistent]
    is the one to report. *)
module Spa : sig
  type t =
    { statistic : float
    ; p_value_lower : float
    ; p_value_consistent : float
    ; p_value_upper : float
    ; best : int (** row index with the largest studentized mean *)
    ; samples : int
    }
  [@@deriving sexp, compare, equal]

  val run
    :  ?scheme:Bootstrap.Scheme.t
    -> ?samples:int
    -> rng:Random.State.t
    -> float array array
    -> t
end

(** Adjusted p-values for a family of tests, returned in the input order. Reject
    hypothesis [i] at level [alpha] when [adjusted.(i) <= alpha]. *)
module Adjust : sig
  (** Family-wise error control, [min 1 (m * p)]. *)
  val bonferroni : float array -> float array

  (** Holm's step-down family-wise control; uniformly more powerful than Bonferroni. *)
  val holm : float array -> float array

  (** Benjamini-Hochberg step-up false discovery rate control (independent or positively
      dependent tests). *)
  val benjamini_hochberg : float array -> float array

  val reject : alpha:float -> float array -> bool array
end

(** Probability of backtest overfitting by combinatorially symmetric cross-validation.

    The periods are cut into [splits] contiguous blocks. For every way of choosing half of
    the blocks as the in-sample set (the rest is out of sample), the strategy with the
    best in-sample performance is found and its out-of-sample relative rank [w] (rank over
    [N + 1], ties averaged) recorded as the logit [log (w / (1 - w))]. The probability of
    backtest overfitting is the share of combinations with a logit at or below zero: the
    in-sample winner did no better than the median out of sample. Around 0.5 for
    strategies with no edge; near 0 when the in-sample winner keeps winning. *)
module Pbo : sig
  module Metric : sig
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
    ; in_sample : float array (** in-sample performance of the selected strategy *)
    ; out_of_sample : float array (** its out-of-sample performance *)
    ; degradation_slope : float
    (** least-squares slope of out-of-sample on in-sample performance *)
    ; probability_of_loss : float
    (** share of combinations with out-of-sample performance below 0 *)
    }
  [@@deriving sexp_of]

  (** [run ?splits ?metric matrix] with [splits] even, at least 2 and at most the number
      of periods (default 16), [metric] default [Sharpe]. Needs at least two strategies.
      There are [C(splits, splits / 2)] combinations (12870 for 16). *)
  val run : ?splits:int -> ?metric:Metric.t -> float array array -> t
end
