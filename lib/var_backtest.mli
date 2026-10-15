(** Backtests of a value-at-risk model from its exception (hit) sequence: [true] for a
    period whose loss exceeded the VaR forecast.

    - Kupiec's unconditional coverage test: is the exception rate the expected one?
    - Christoffersen's independence test: do exceptions cluster (first-order Markov
      alternative)?
    - Christoffersen's conditional coverage test: both at once.
    - The traffic-light zones of the Basel backtesting framework.

    Every likelihood-ratio test reports its asymptotic chi-square p-value and an exact
    small-sample p-value: the probability, under the null, of a statistic at least as
    large as the one observed, computed by enumerating the exact distribution of the
    sufficient counts (binomial for Kupiec; transition counts by dynamic programming for
    the Christoffersen tests). [expected_rate] is the exception probability under the
    null, e.g. [0.01] for a 99% VaR. *)

open! Core

module Test : sig
  type t =
    { statistic : float
    ; p_value : float (** exact when [exact], else asymptotic *)
    ; asymptotic_p_value : float
    ; exact : bool
    }
  [@@deriving sexp, compare, equal]
end

(** [hits ~returns ~var] marks the periods with [returns.(t) < -. var.(t)]: [var] holds
    VaR forecasts as positive loss fractions. *)
val hits : returns:float array -> var:float array -> bool array

(** Exception counts and transition counts of a hit sequence. [n01] counts a period
    without an exception followed by one with an exception, and so on. *)
module Counts : sig
  type t =
    { observations : int
    ; exceptions : int
    ; n00 : int
    ; n01 : int
    ; n10 : int
    ; n11 : int
    }
  [@@deriving sexp, compare, equal]

  val of_hits : bool array -> t
end

(** Kupiec's proportion-of-failures test; one degree of freedom. *)
val kupiec : expected_rate:float -> bool array -> Test.t

(** Christoffersen's independence test; one degree of freedom. The exact p-value uses the
    observed exception rate as the null rate (the null leaves it unspecified). Computed
    exactly up to [exact_limit] observations (default 500; cost grows with the cube),
    asymptotically beyond. *)
val christoffersen_independence : ?exact_limit:int -> bool array -> Test.t

(** Christoffersen's conditional coverage test, the sum of the coverage and independence
    statistics; two degrees of freedom. Exact up to [exact_limit] (default 500). *)
val christoffersen_conditional_coverage
  :  ?exact_limit:int
  -> expected_rate:float
  -> bool array
  -> Test.t

module Traffic_light : sig
  type zone =
    | Green
    | Yellow
    | Red
  [@@deriving sexp, compare, equal]

  (** [cumulative_probability ~expected_rate ~observations ~exceptions] is P(X <=
      exceptions) for X ~ Binomial(observations, expected_rate). *)
  val cumulative_probability
    :  expected_rate:float
    -> observations:int
    -> exceptions:int
    -> float

  (** Green while the cumulative probability is below 95%, yellow below 99.99%, red from
      there: for 250 observations at a 1% rate, green up to 4 exceptions, yellow 5 to 9,
      red from 10. *)
  val classify : expected_rate:float -> observations:int -> exceptions:int -> zone

  (** The capital multiplier add-on of the Basel framework, defined for 250 observations
      of a 99% VaR: 0 up to 4 exceptions, then 0.40, 0.50, 0.65, 0.75, 0.85 for 5 to 9,
      and 1 from 10. *)
  val basel_plus_factor : exceptions:int -> float
end

type t =
  { counts : Counts.t
  ; expected_rate : float
  ; unconditional_coverage : Test.t
  ; independence : Test.t
  ; conditional_coverage : Test.t
  ; zone : Traffic_light.zone
  }
[@@deriving sexp, compare, equal]

(** All of the above for one hit sequence. Raises on an empty sequence or a rate outside
    (0, 1). *)
val run : ?exact_limit:int -> expected_rate:float -> bool array -> t
