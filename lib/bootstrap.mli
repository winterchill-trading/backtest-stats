(** Resampling schemes for dependent data. Every sampler takes an explicit
    [Random.State.t], so a seed reproduces a run exactly.

    - [Iid]: draw periods independently with replacement (ignores autocorrelation).
    - [Circular_block]: concatenate blocks of a fixed length starting at uniform
      positions, wrapping around the end of the sample.
    - [Stationary]: blocks of geometric length with the given mean (the stationary
      bootstrap), also wrapping; the resampled series is itself stationary.

    A sampler returns indices into the original sample, so several series observed over
    the same periods (the rows of a strategy-by-period matrix) are resampled jointly by
    applying the same indices to each. *)

open! Core

module Scheme : sig
  type t =
    | Iid
    | Circular_block of { length : int }
    | Stationary of { mean_block_length : float }
  [@@deriving sexp, compare, equal]

  (** Raises unless the block length is at least one. *)
  val validate : t -> unit
end

(** [default_block_length n] is the cube root of [n], rounded, at least 1: the
    rate-optimal order for block bootstraps of the mean. *)
val default_block_length : int -> int

(** [fill_indices scheme ~rng dst] writes [Array.length dst] resampled indices, each in
    from 0 to [Array.length dst - 1]. It does not allocate. *)
val fill_indices : Scheme.t -> rng:Random.State.t -> int array -> unit

(** [indices scheme ~rng ~n] is a fresh array of [n] resampled indices. *)
val indices : Scheme.t -> rng:Random.State.t -> n:int -> int array

(** [resample scheme ~rng xs] is [xs] read at fresh indices. *)
val resample : Scheme.t -> rng:Random.State.t -> float array -> float array

(** [series scheme ~rng s] resamples the returns of [s] and keeps its times, so the result
    is a series over the same calendar. *)
val series : Scheme.t -> rng:Random.State.t -> Series.t -> Series.t

(** [percentile_interval ~level stats] is the [(lo, hi)] percentile interval holding the
    central [level] fraction of the bootstrap statistics (linear interpolation between
    order statistics). The array is sorted in place. *)
val percentile_interval : level:float -> float array -> float * float

(** [quantile_sorted sorted q] is the [q]-quantile of an ascending array, interpolating
    linearly between order statistics. *)
val quantile_sorted : float array -> float -> float
