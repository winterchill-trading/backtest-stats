(** Numeric tables read from CSV or Parquet: an optional time column and named float
    columns of equal length. *)

open! Core

type t =
  { times : Time_ns.t array option
  ; columns : (string * float array) list (** in file order *)
  }

(** Reads [path]; Parquet when it ends in [.parquet], CSV with a header row otherwise. The
    time column is the first one named [time], [timestamp], [date] or [ts]. Times may be
    dates ([2024-01-31], midnight UTC), timestamps with or without an offset
    ([2024-01-31 12:00:00Z]; no offset means UTC) or integers since the Unix epoch, whose
    unit is taken from their size (seconds, milliseconds, microseconds or nanoseconds).
    Every other column must be numeric; empty cells and Parquet nulls are errors. *)
val load : string -> t Or_error.t

val column : t -> string -> float array Or_error.t

(** A series over the table's times, or over consecutive days from the Unix epoch when the
    table has no time column. *)
val series : t -> float array -> Backtest_stats.Series.t Or_error.t
