(* Source for the Chapter 2 REPL examples; load the two teaching modules first. *)

(* book: polint_interface begin *)
#show Book_algorithms.polint;;
(* book: polint_interface end *)

(* book: trapz_interface begin *)
#show Book_algorithms.trapz;;
(* book: trapz_interface end *)

(* book: trapz_example begin *)
Book_algorithms.trapz (fun x -> x ** 2.) 1. 4.;;
(* book: trapz_example end *)

(* book: atan_example begin *)
atan 1. -. atan (-1.);;
(* book: atan_example end *)

(* book: legendre_example begin *)
Owl_maths_quadrature.gauss_legendre ~a:(-1.) ~b:1. 10;;
(* book: legendre_example end *)

(* book: sinc_example begin *)
let f t = if t = 0. then 1. else sin t /. t;;
Book_gaussian.gaussian f 0. 4.;;
Owl_maths.si 4.;;
(* book: sinc_example end *)

(* book: beta_example begin *)
let x = Maths.beta 3. 4.;;
let y = Maths.gamma 3. *. Maths.gamma 4. /. Maths.gamma 7.;;
abs_float (Maths.beta 3. 4. -. Maths.beta 4. 3.) < 1e-12;;
(* book: beta_example end *)

(* book: ellipse_example begin *)
let a = 4.;;
let b = 3.;;
let c = 4. *. a *. Maths.ellipe (1. -. (b /. a) ** 2.);;
(* book: ellipse_example end *)
