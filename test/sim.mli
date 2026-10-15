(** Seeded random data for the tests. *)

open! Core

(** A standard normal draw (Box-Muller). *)
val gaussian : Random.State.t -> float

(** [returns rng ~n ~mean ~std] is [n] independent normal returns. *)
val returns : Random.State.t -> n:int -> mean:float -> std:float -> float array

(** [noise_matrix rng ~strategies ~periods ~std] is a matrix of zero-mean strategies. *)
val noise_matrix
  :  Random.State.t
  -> strategies:int
  -> periods:int
  -> std:float
  -> float array array
