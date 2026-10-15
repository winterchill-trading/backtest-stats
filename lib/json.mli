(** JSON encoders for the results of this library, for reports read by other tools. Field
    names follow the OCaml records; NaN and infinities become [null]. *)

open! Core

val float : float -> Yojson.Safe.t
val interval : Metrics.Interval.t -> Yojson.Safe.t
val drawdown : Metrics.Drawdown.t -> Yojson.Safe.t
val drawdown_intervals : Metrics.Drawdown.intervals -> Yojson.Safe.t
val reality_check : Multiple_testing.Reality_check.t -> Yojson.Safe.t
val spa : Multiple_testing.Spa.t -> Yojson.Safe.t

(** Summary of a PBO run: the probability, counts, degradation slope and probability of
    loss (not the per-combination arrays). *)
val pbo : Multiple_testing.Pbo.t -> Yojson.Safe.t

val var_test : Var_backtest.Test.t -> Yojson.Safe.t
val var_backtest : Var_backtest.t -> Yojson.Safe.t
val evidence : Gate.Evidence.t -> Yojson.Safe.t

(** [{"accept": true}] or
    [{"accept": false, "reasons": [{"kind": ..., "message": ..., ...}]}]. *)
val verdict : Gate.Verdict.t -> Yojson.Safe.t
