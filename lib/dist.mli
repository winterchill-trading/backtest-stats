(** Distribution functions used by the tests in this library: the standard normal, the
    chi-square survival function and the binomial distribution.

    Accuracy: the normal CDF and quantile are within a few units in the last place of
    double precision over the range used here (the quantile is refined with one Halley
    step); the log-gamma function uses a Lanczos approximation, relative error below
    [1e-14]. *)

open! Core

(** [normal_cdf x] is P(Z <= x) for a standard normal Z. *)
val normal_cdf : float -> float

(** [normal_sf x] is P(Z > x), computed without cancellation for large [x]. *)
val normal_sf : float -> float

val normal_pdf : float -> float

(** [normal_quantile p] is the [x] with [normal_cdf x = p]. Returns [Float.neg_infinity]
    at 0 and [Float.infinity] at 1; raises for [p] outside the unit interval or NaN. *)
val normal_quantile : float -> float

(** Natural log of the gamma function, for positive arguments. *)
val log_gamma : float -> float

(** [regularized_gamma_q ~a x] is the upper regularized incomplete gamma function Q(a, x)
    = Gamma(a, x) / Gamma(a), for [a > 0] and [x >= 0]. *)
val regularized_gamma_q : a:float -> float -> float

(** [chi_square_sf ~df x] is P(X > x) for X chi-square with [df] degrees of freedom. *)
val chi_square_sf : df:int -> float -> float

(** [binomial_pmf ~n ~p k] is P(X = k) for X ~ Binomial(n, p). *)
val binomial_pmf : n:int -> p:float -> int -> float

(** [binomial_cdf ~n ~p k] is P(X <= k). *)
val binomial_cdf : n:int -> p:float -> int -> float

(** [x_log_y x y] is [x *. log y], with the convention [0 log 0 = 0]. Used by every
    likelihood-ratio statistic in this library. *)
val x_log_y : float -> float -> float
