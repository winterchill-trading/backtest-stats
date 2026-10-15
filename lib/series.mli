(** A return series: one simple return per period, each stamped with the time the period
    ends. Times strictly increase; returns are finite. Returns are fractions ([0.01] is
    one percent), not percentages.

    A PnL series becomes a return series by dividing by the capital at risk ({!of_pnl});
    an equity or price curve by taking ratios ({!of_prices}). *)

open! Core

type t [@@deriving sexp_of]

(** [create ~times ~returns] checks that both arrays have the same length, that times
    strictly increase and that every return is finite and above [-1]. The arrays are
    copied. *)
val create : times:Time_ns.t array -> returns:float array -> t Or_error.t

(** [of_returns returns] stamps the returns at [start + i * step] (defaults: the Unix
    epoch and one day). Raises on a non-finite return or one at or below [-1]. *)
val of_returns : ?start:Time_ns.t -> ?step:Time_ns.Span.t -> float array -> t

(** Simple returns between consecutive prices (or equity values), stamped with the later
    time. [n] prices give [n - 1] returns. Prices must be positive. *)
val of_prices : times:Time_ns.t array -> prices:float array -> t Or_error.t

(** [of_pnl ~times ~pnl ~capital] divides each period's PnL by a fixed capital. *)
val of_pnl : times:Time_ns.t array -> pnl:float array -> capital:float -> t Or_error.t

val length : t -> int
val is_empty : t -> bool

(** A copy of the returns. *)
val returns : t -> float array

(** A copy of the times. *)
val times : t -> Time_ns.t array

val return_at : t -> int -> float
val time_at : t -> int -> Time_ns.t

(** [sub t ~pos ~len] is the periods [pos] to [pos + len - 1]. *)
val sub : t -> pos:int -> len:int -> t

(** [split t ~in_sample_fraction] cuts the series in two at
    [round_down (fraction * length)]: the first part is in sample, the second out of
    sample. *)
val split : t -> in_sample_fraction:float -> t * t

(** [compound returns] is the product of [1 + r] minus one. *)
val compound : float array -> float

(** [resample t ~every] compounds the returns of each calendar bucket of length [every]
    (buckets aligned to the Unix epoch, so [Time_ns.Span.day] gives UTC days) into one
    return stamped with the bucket's last time. Empty buckets are skipped. *)
val resample : t -> every:Time_ns.Span.t -> t

(** [excess t ~over] subtracts the benchmark's return period by period. The two series
    must have the same times. *)
val excess : t -> over:t -> t Or_error.t

(** Log returns, [log (1 + r)]. *)
val log_returns : t -> float array

(** [equity_curve ?start t] is the compounded value after each period, starting from
    [start] (default 1). It has [length t] points and does not include [start]. *)
val equity_curve : ?start:float -> t -> float array

(** Same times, new returns (used by resamplers). Raises if the lengths differ. *)
val with_returns : t -> float array -> t
