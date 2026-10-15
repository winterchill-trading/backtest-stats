open! Core

let normal_cdf x = 0.5 *. Stdlib.Float.erfc (-.x /. Float.sqrt 2.)
let normal_sf x = 0.5 *. Stdlib.Float.erfc (x /. Float.sqrt 2.)
let normal_pdf x = Float.exp (-0.5 *. x *. x) /. Float.sqrt (2. *. Float.pi)

(* Rational approximation for the lower region (relative error 1.15e-9), then one Halley
   step against the erfc-based CDF, which brings it to full double precision. *)
let a =
  [| -3.969683028665376e+01
   ; 2.209460984245205e+02
   ; -2.759285104469687e+02
   ; 1.383577518672690e+02
   ; -3.066479806614716e+01
   ; 2.506628277459239e+00
  |]
;;

let b =
  [| -5.447609879822406e+01
   ; 1.615858368580409e+02
   ; -1.556989798598866e+02
   ; 6.680131188771972e+01
   ; -1.328068155288572e+01
  |]
;;

let c =
  [| -7.784894002430293e-03
   ; -3.223964580411365e-01
   ; -2.400758277161838e+00
   ; -2.549732539343734e+00
   ; 4.374664141464968e+00
   ; 2.938163982698783e+00
  |]
;;

let d =
  [| 7.784695709041462e-03
   ; 3.224671290700398e-01
   ; 2.445134137142996e+00
   ; 3.754408661907416e+00
  |]
;;

let rational_quantile p =
  let p_low = 0.02425 in
  if Float.( < ) p p_low
  then (
    let q = Float.sqrt (-2. *. Float.log p) in
    ((((((((((c.(0) *. q) +. c.(1)) *. q) +. c.(2)) *. q) +. c.(3)) *. q) +. c.(4)) *. q)
     +. c.(5))
    /. ((((((((d.(0) *. q) +. d.(1)) *. q) +. d.(2)) *. q) +. d.(3)) *. q) +. 1.))
  else if Float.( <= ) p (1. -. p_low)
  then (
    let q = p -. 0.5 in
    let r = q *. q in
    ((((((((((a.(0) *. r) +. a.(1)) *. r) +. a.(2)) *. r) +. a.(3)) *. r) +. a.(4)) *. r)
     +. a.(5))
    *. q
    /. ((((((((((b.(0) *. r) +. b.(1)) *. r) +. b.(2)) *. r) +. b.(3)) *. r) +. b.(4))
         *. r)
        +. 1.))
  else (
    let q = Float.sqrt (-2. *. Float.log1p (-.p)) in
    -.((((((((((c.(0) *. q) +. c.(1)) *. q) +. c.(2)) *. q) +. c.(3)) *. q) +. c.(4)) *. q)
       +. c.(5))
    /. ((((((((d.(0) *. q) +. d.(1)) *. q) +. d.(2)) *. q) +. d.(3)) *. q) +. 1.))
;;

let normal_quantile p =
  if Float.is_nan p || Float.( < ) p 0. || Float.( > ) p 1.
  then raise_s [%message "Dist.normal_quantile: p outside [0, 1]" (p : float)];
  if Float.( = ) p 0.
  then Float.neg_infinity
  else if Float.( = ) p 1.
  then Float.infinity
  else (
    let x = rational_quantile p in
    (* Halley step; for the upper tail work with the survival function so that the error
       is measured relative to 1 - p rather than lost in cancellation. *)
    let e = if Float.( > ) p 0.5 then 1. -. p -. normal_sf x else normal_cdf x -. p in
    let u = e *. Float.sqrt (2. *. Float.pi) *. Float.exp (x *. x /. 2.) in
    x -. (u /. (1. +. (x *. u /. 2.))))
;;

(* Lanczos approximation, g = 7, nine coefficients. *)
let lanczos =
  [| 0.99999999999980993
   ; 676.5203681218851
   ; -1259.1392167224028
   ; 771.32342877765313
   ; -176.61502916214059
   ; 12.507343278686905
   ; -0.13857109526572012
   ; 9.9843695780195716e-6
   ; 1.5056327351493116e-7
  |]
;;

let rec log_gamma x =
  if Float.( <= ) x 0.
  then raise_s [%message "Dist.log_gamma: argument must be positive" (x : float)]
  else if Float.( < ) x 0.5
  then
    (* reflection: Gamma(x) Gamma(1 - x) = pi / sin(pi x) *)
    Float.log (Float.pi /. Float.sin (Float.pi *. x)) -. log_gamma (1. -. x)
  else (
    let x = x -. 1. in
    let sum = ref lanczos.(0) in
    for i = 1 to 8 do
      sum := !sum +. (lanczos.(i) /. (x +. Float.of_int i))
    done;
    let t = x +. 7.5 in
    (0.5 *. Float.log (2. *. Float.pi))
    +. ((x +. 0.5) *. Float.log t)
    -. t
    +. Float.log !sum)
;;

(* Series for P(a, x) when x < a + 1, Lentz continued fraction for Q(a, x) otherwise. *)
let gamma_p_series ~a x =
  let sum = ref (1. /. a) in
  let term = ref (1. /. a) in
  let ap = ref a in
  let n = ref 0 in
  while !n < 10_000 && Float.( > ) (Float.abs !term) (Float.abs !sum *. 1e-16) do
    ap := !ap +. 1.;
    term := !term *. x /. !ap;
    sum := !sum +. !term;
    incr n
  done;
  !sum *. Float.exp ((a *. Float.log x) -. x -. log_gamma a)
;;

let gamma_q_fraction ~a x =
  let tiny = 1e-300 in
  let b = ref (x +. 1. -. a) in
  let c = ref (1. /. tiny) in
  let d = ref (1. /. !b) in
  let h = ref !d in
  let i = ref 1 in
  let continue = ref true in
  while !continue && !i < 10_000 do
    let an = -.Float.of_int !i *. (Float.of_int !i -. a) in
    b := !b +. 2.;
    d := (an *. !d) +. !b;
    if Float.( < ) (Float.abs !d) tiny then d := tiny;
    c := !b +. (an /. !c);
    if Float.( < ) (Float.abs !c) tiny then c := tiny;
    d := 1. /. !d;
    let delta = !d *. !c in
    h := !h *. delta;
    if Float.( < ) (Float.abs (delta -. 1.)) 1e-16 then continue := false;
    incr i
  done;
  Float.exp ((a *. Float.log x) -. x -. log_gamma a) *. !h
;;

let regularized_gamma_q ~a x =
  if Float.( <= ) a 0. || Float.( < ) x 0.
  then
    raise_s
      [%message "Dist.regularized_gamma_q: need a > 0 and x >= 0" (a : float) (x : float)]
  else if Float.( = ) x 0.
  then 1.
  else if Float.( < ) x (a +. 1.)
  then 1. -. gamma_p_series ~a x
  else gamma_q_fraction ~a x
;;

let chi_square_sf ~df x =
  if df <= 0 then raise_s [%message "Dist.chi_square_sf: df must be positive" (df : int)];
  if Float.( <= ) x 0.
  then 1.
  else (
    match df with
    | 1 -> Stdlib.Float.erfc (Float.sqrt (x /. 2.))
    | 2 -> Float.exp (-.x /. 2.)
    | _ -> regularized_gamma_q ~a:(Float.of_int df /. 2.) (x /. 2.))
;;

let x_log_y x y = if Float.( = ) x 0. then 0. else x *. Float.log y

let binomial_pmf ~n ~p k =
  if k < 0 || k > n
  then 0.
  else if Float.( = ) p 0.
  then if k = 0 then 1. else 0.
  else if Float.( = ) p 1.
  then if k = n then 1. else 0.
  else (
    let nf = Float.of_int n
    and kf = Float.of_int k in
    Float.exp
      (log_gamma (nf +. 1.)
       -. log_gamma (kf +. 1.)
       -. log_gamma (nf -. kf +. 1.)
       +. (kf *. Float.log p)
       +. ((nf -. kf) *. Float.log1p (-.p))))
;;

let binomial_cdf ~n ~p k =
  if k < 0
  then 0.
  else if k >= n
  then 1.
  else (
    let s = ref 0. in
    for i = 0 to k do
      s := !s +. binomial_pmf ~n ~p i
    done;
    Float.min 1. !s)
;;
