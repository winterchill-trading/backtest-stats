open! Core

let gaussian rng =
  let u1 = Float.max Float.min_positive_normal_value (Random.State.float rng 1.) in
  let u2 = Random.State.float rng 1. in
  Float.sqrt (-2. *. Float.log u1) *. Float.cos (2. *. Float.pi *. u2)
;;

let returns rng ~n ~mean ~std = Array.init n ~f:(fun _ -> mean +. (std *. gaussian rng))

let noise_matrix rng ~strategies ~periods ~std =
  Array.init strategies ~f:(fun _ -> returns rng ~n:periods ~mean:0. ~std)
;;
