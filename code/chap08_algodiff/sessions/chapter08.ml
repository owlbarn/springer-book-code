#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
open Owl;;
open Owl_plplot;;
Owl_stats_prng.init 78;;
let book_checks = ref 0;;
let book_check name expected actual =
  if not (Float.is_finite actual) || abs_float (actual -. expected) > 2e-6 *. max 1. (abs_float expected)
  then failwith (Printf.sprintf "%s expected %.17g got %.17g" name expected actual);
  incr book_checks;;
let book_matrix_printer formatter value = Owl_pretty.print_dsnda ~max_row:6 ~max_col:6 ~formatter value;;
#install_printer book_matrix_printer;;

(* book: finite-step begin *)
let d =
    let _eps = 0.1 in
    let diff f x = (f (x +. _eps) -. f x) /. _eps in
    diff Owl_maths.cos 1.;;
(* book: finite-step end *)

(* book: finite-cancellation begin *)
let d =
    let _eps = 5E-16 in
    let diff f x = (f (x +. _eps) -. f x) /. _eps in
    diff Owl_maths.cos 1.;;
(* book: finite-cancellation end *)

(* book: owl-gradient begin *)
open Algodiff.D;;
let f x =
    let x1 = Mat.get x 0 0 in
    let x2 = Mat.get x 0 1 in
    Maths.(div (F 1.) (F 1. + exp (x1 * x2 + (sin x1))));;
let x = Mat.ones 1 2;;
let _ = grad f x |> unpack_arr;;
(* book: owl-gradient end *)

(* book: numerical-module begin *)
open Owl;;
module D = Owl_numdiff_generic.Make (Dense.Ndarray.D);;
let x = Arr.ones [|2|];;
let f x =
  let x1 = Arr.get x [|0|] in
  let x2 = Arr.get x [|1|] in
  Maths.(div 1. (1. +. exp (x1 *. x2 +. (sin x1))));;
(* book: numerical-module end *)

(* book: numerical-gradient begin *)
D.grad f x;;
(* book: numerical-gradient end *)

(* book: forward-type begin *)
type df = {
  mutable p: float;
  mutable t: float;
};;

let primal df = df.p;;
let tangent df = df.t;;
(* book: forward-type end *)

(* book: forward-sin begin *)
let sin_ad x =
  let p = primal x in
  let t = tangent x in
  let p' = Owl_maths.sin p in
  let t' = (Owl_maths.cos p) *. t in
  {p = p'; t = t'};;
(* book: forward-sin end *)

(* book: forward-exp begin *)
let exp_ad x =
  let p = primal x in
  let t = tangent x in
  let p' = Owl_maths.exp p in
  let t' = p' *. t in
  {p = p'; t = t'};;
(* book: forward-exp end *)

(* book: forward-add begin *)
let add_ad a b =
  let pa = primal a in
  let ta = tangent a in
  let pb = primal b in
  let tb = tangent b in
  let p' = pa +. pb in
  let t' = ta +. tb in
  {p=p'; t=t'};;
(* book: forward-add end *)

(* book: forward-mul begin *)
let mul_ad a b =
  let pa = primal a in
  let ta = tangent a in
  let pb = primal b in
  let tb = tangent b in
  let p' = pa *. pb in
  let t' = pa *. tb +. ta *. pb in
  {p = p'; t = t'};;
(* book: forward-mul end *)

(* book: forward-div begin *)
let div_ad a b =
  let pa = primal a in
  let ta = tangent a in
  let pb = primal b in
  let tb = tangent b in
  let p' = pa /. pb in
  let t' = (ta *. pb -. tb *. pa) /. (pb *. pb) in
  {p=p'; t=t'};;
(* book: forward-div end *)

(* book: forward-diff begin *)
let diff f =
  let f' x y =
    let r = f x y in
    primal r, tangent r
  in
  f';;
(* book: forward-diff end *)

(* book: forward-seeds begin *)
let x0 = {p=1.; t=1.};;
let x1 = {p=1.; t=0.};;
(* book: forward-seeds end *)

(* book: forward-example begin *)
let f x0 x1 =
  let v2 = sin_ad x0 in
  let v3 = mul_ad x0 x1 in
  let v4 = add_ad v2 v3 in
  let v5 = {p=1.; t=0.} in
  let v6 = exp_ad v4 in
  let v7 = {p=1.; t=0.} in
  let v8 = add_ad v5 v6 in
  let v9 = div_ad v7 v8 in
  v9;;
let p, t = diff f x0 x1;;
(* book: forward-example end *)

(* book: forward-checks begin *)
let logistic = 1. /. (1. +. exp (sin 1. +. 1.));;
let dx0 = -.logistic *. (1. -. logistic) *. (cos 1. +. 1.);;
let dx1 = -.logistic *. (1. -. logistic);;
book_check "forward primal" logistic p;;
book_check "forward dx0" dx0 t;;
book_check "forward dx1" dx1 (snd (diff f {p=1.;t=0.} {p=1.;t=1.}));;
book_check "forward directional" (2. *. dx0 -. 3. *. dx1) (snd (diff f {p=1.;t=2.} {p=1.;t= -.3.}));;
book_check "forward constant" 0. (tangent (exp_ad {p=2.;t=0.}));;
(* book: forward-checks end *)

(* book: reverse-type begin *)
type dr = {
  mutable p: float;
  mutable a: float ref;
  mutable adj_fun : float ref -> 
    (float * dr) list -> (float * dr) list
};;

let primal dr = dr.p;;
let adjoint dr = dr.a;;
let adj_fun dr = dr.adj_fun;;
(* book: reverse-type end *)

(* book: reverse-sin begin *)
let sin_ad dr =
  let p = primal dr in
  let p' = Owl_maths.sin p in
  let adjfun' ca t =
      let r = !ca *. (Owl_maths.cos p) in
      (r, dr) :: t
  in
  {p=p'; a=ref 0.; adj_fun=adjfun'};;
(* book: reverse-sin end *)

(* book: reverse-mul begin *)
let mul_ad dr1 dr2 =
  let p1 = primal dr1 in
  let p2 = primal dr2 in
  let p' = Owl_maths.mul p1 p2 in
  let adjfun' ca t =
      let r1 = !ca *. p2 in
      let r2 = !ca *. p1 in
      (r1, dr1) :: (r2, dr2) :: t
  in
  {p = p'; a = ref 0.; adj_fun = adjfun'};;
(* book: reverse-mul end *)

(* book: reverse-leaf begin *)
let make_reverse v =
  let a = ref 0. in
  let adj_fun _a t = t in
  {p=v; a; adj_fun};;
(* book: reverse-leaf end *)

(* book: reverse-graph begin *)
let x = make_reverse 1.;;
let y = make_reverse 2.;;
let v = mul_ad (sin_ad x) y;;
(* book: reverse-graph end *)

(* book: reverse-push begin *)
let rec reverse_push = function
  | [] -> ()
  | (v, dr) :: t ->
      let aa = adjoint dr in
      let adjfun = adj_fun dr in
      aa := !aa +. v;
      let stack = adjfun (ref v) t in
      reverse_push stack;;

let reverse_reset roots =
  let seen = ref [] in
  let rec visit dr =
    let a = adjoint dr in
    if not (List.exists (fun b -> a == b) !seen) then (
      seen := a :: !seen;
      a := 0.;
      List.iter (fun (_, parent) -> visit parent) (adj_fun dr (ref 0.) []))
  in List.iter visit roots;;
(* book: reverse-push end *)

(* book: reverse-ops begin *)
let exp_ad dr =
  let p = primal dr in
  let p' = Owl_maths.exp p in
  let adjfun' ca t =
      let r = !ca *. (Owl_maths.exp p) in
      (r, dr) :: t
  in
  {p=p'; a=ref 0.; adj_fun=adjfun'};;

let add_ad dr1 dr2 =
  let p1 = primal dr1 in
  let p2 = primal dr2 in
  let p' = Owl_maths.add p1 p2 in
  let adjfun' ca t =
    let r1 = !ca in
    let r2 = !ca in
    (r1, dr1) :: (r2, dr2) :: t
  in
  {p = p'; a = ref 0.; adj_fun = adjfun'};;

let div_ad dr1 dr2 =
  let p1 = primal dr1 in
  let p2 = primal dr2 in
  let p' = Owl_maths.div p1 p2 in
  let adjfun' ca t =
      let r1 = !ca /. p2 in
      let r2 = !ca *. (-.p1) /. (p2 *. p2) in
      (r1, dr1) :: (r2, dr2) :: t in
  {p = p'; a = ref 0.; adj_fun = adjfun'};;
(* book: reverse-ops end *)

(* book: reverse-diff begin *)
let diff f =
  let f' x =
    let r = f x in (* the forward pass *)
    reverse_reset [r];
    reverse_push [(1., r)]; (* the backward pass *)
    let x0, x1 = x in 
    primal x0, !(adjoint x0), primal x1, !(adjoint x1)
  in f';;
(* book: reverse-diff end *)

(* book: reverse-function begin *)
let f (x0, x1) =
  let v2 = sin_ad x0 in
  let v3 = mul_ad x0 x1 in
  let v4 = add_ad v2 v3 in
  let v5 = make_reverse 1. in
  let v6 = exp_ad v4 in
  let v7 = make_reverse 1. in
  let v8 = add_ad v5 v6 in
  let v9 = div_ad v7 v8 in
  v9;;
(* book: reverse-function end *)

(* book: reverse-example begin *)
let x1 = make_reverse 1.;;
let x0 = make_reverse 1.;;
let pri_x0, adj_x0, pri_x1, adj_x1 = diff f (x0, x1);;
(* book: reverse-example end *)

(* book: reverse-checks begin *)
book_check "reverse dx0" dx0 adj_x0;;
book_check "reverse dx1" dx1 adj_x1;;
let _, again0, _, again1 = diff f (x0,x1);;
book_check "reverse repeat dx0" dx0 again0;;
book_check "reverse repeat dx1" dx1 again1;;
let shared_x = make_reverse 2.;;
let shared_u = mul_ad shared_x shared_x;;
let shared_r = add_ad shared_u shared_u;;
reverse_reset [shared_r];;
reverse_push [1.,shared_r];;
book_check "reverse shared graph" 8. !(adjoint shared_x);;
(* book: reverse-checks end *)

(* book: combined-type begin *)
type t =
  | DF of float * float
  | DR of float * float ref * adjoint
and adjoint = float ref -> (float * t) list -> (float * t) list

let primal = function
  | DF (p, _) -> p
  | DR (p, _, _) -> p

let tangent = function
  | DF (_, t) -> t
  | DR (_, _, _) -> failwith "error: no tangent for DR"

let adjoint = function
  | DF (_, _) ->  failwith "error: no adjoint for DF"
  | DR (_, a, _) -> a;;
(* book: combined-type end *)

(* book: combined-seeds begin *)
let make_forward p a = DF (p, a)

let make_reverse p =
    let a = ref 0. in
    let adj_fun _a t = t in
    DR (p, a, adj_fun)

let rec reverse_push = function
  | [] -> ()
  | (v, x) :: t ->
    (match x with
     | DR (_, a, adjfun) ->
       a := !a +. v;
       let stack = adjfun (ref v) t in
       reverse_push stack
     | _ -> failwith "error: unsupported type");;

let reverse_reset roots =
  let seen = ref [] in
  let rec visit = function
    | DF _ -> ()
    | DR (_, a, adjfun) ->
      if not (List.exists (fun b -> a == b) !seen) then (
        seen := a :: !seen;
        a := 0.;
        List.iter (fun (_, parent) -> visit parent) (adjfun (ref 0.) []))
  in List.iter visit roots;;
(* book: combined-seeds end *)

(* book: combined-sin begin *)
let sin_ad x =
  let ff = Owl_maths.sin in
  let df p t = (Owl_maths.cos p) *. t in
  let dr p a = !a *. (Owl_maths.cos p) in
  match x with
  | DF (p, t) ->
    let p' = ff p in
    let t' = df p t in
    DF (p', t')
  | DR (p, _, _) ->
    let p' = ff p in
    let adjfun' a t =
      let r = dr p a in
      (r, x) :: t
    in
    DR (p', ref 0., adjfun');;
(* book: combined-sin end *)

(* book: combined-mul begin *)
let mul_ad xa xb =
  let ff = Owl_maths.mul in
  let df pa pb ta tb = pa *. tb +. ta *. pb in
  let dr pa pb a = !a *. pb, !a *. pa in
  match xa, xb with
  | DF (pa, ta), DF (pb, tb) ->
    let p' = ff pa pb in
    let t' = df pa pb ta tb in
    DF (p', t')
  | DR (pa, _, _), DR (pb, _, _) ->
    let p' = ff pa pb in
    let adjfun' a t =
      let ra, rb = dr pa pb a in
      (ra, xa) :: (rb, xb) :: t
    in
    DR (p', ref 0., adjfun')
  | _, _ -> failwith "unsupported op";;
(* book: combined-mul end *)

(* book: operator-modules begin *)
module type Unary = sig
  val ff : float -> float
  val df : float -> float -> float
  val dr : float -> float ref -> float
end
module type Binary = sig
  val ff : float -> float -> float
  val df : float -> float -> float -> float -> float
  val dr : float -> float -> float ref -> float * float
end

module Sin = struct
  let ff = Owl_maths.sin
  let df p t = (Owl_maths.cos p) *. t
  let dr p a = !a *. (Owl_maths.cos p)
end

module Mul = struct
  let ff = Owl_maths.mul
  let df pa pb ta tb = pa *. tb +. ta *. pb
  let dr pa pb a = !a *. pb, !a *. pa
end;;
(* book: operator-modules end *)

(* book: operator-builders begin *)
let unary_op (module U: Unary) = fun x ->
  match x with
  | DF (p, t) ->
    let p' = U.ff p in
    let t' = U.df p t in
    DF (p', t')
  | DR (p, _, _) ->
    let p' = U.ff p in
    let adjfun' a t =
      let r = U.dr p a in
      (r, x) :: t
    in
    DR (p', ref 0., adjfun')

  
let binary_op (module B: Binary) = fun xa xb ->
  match xa, xb with
  | DF (pa, ta), DF (pb, tb) ->
    let p' = B.ff pa pb in
    let t' = B.df pa pb ta tb in
    DF (p', t')
  | DR (pa, _, _), DR (pb, _, _) ->
    let p' = B.ff pa pb in
    let adjfun' a t =
      let ra, rb = B.dr pa pb a in
      (ra, xa) :: (rb, xb) :: t
    in
    DR (p', ref 0., adjfun')
  | _, _ -> failwith "unsupported op";;
(* book: operator-builders end *)

(* book: built-operators begin *)
let sin_ad = unary_op (module Sin : Unary)
let mul_ad = binary_op (module Mul : Binary);;

module Exp = struct
  let ff = Owl_maths.exp
  let df p t = exp p *. t
  let dr p a = !a *. exp p
end
module Add = struct
  let ff = Owl_maths.add
  let df _pa _pb ta tb = ta +. tb
  let dr _pa _pb a = !a, !a
end
module Div = struct
  let ff = Owl_maths.div
  let df pa pb ta tb = (ta *. pb -. tb *. pa) /. (pb *. pb)
  let dr pa pb a = !a /. pb, -.(!a *. pa) /. (pb *. pb)
end
let exp_ad = unary_op (module Exp : Unary)
let add_ad = binary_op (module Add : Binary)
let div_ad = binary_op (module Div : Binary);;
(* book: built-operators end *)

(* book: combined-diff begin *)
let diff f =
  let f' x =
    let x0, x1 = x in
    match x0, x1 with
    | DF (_, _), DF (_, _) ->
      f x |> tangent
    | DR (_, _, _), DR (_, _, _) ->
      let r = f x in
      reverse_reset [r];
      reverse_push [(1., r)];
      !(adjoint x0)
    | _, _ -> failwith "error: unsupported operator"
  in
  f';;
(* book: combined-diff end *)

(* book: combined-forward begin *)
let x0 = make_forward 1. 1.;;
let x1 = make_forward 1. 0.;;
let f_forward x =
    let x0, x1 = x in
    let v2 = sin_ad x0 in
    let v3 = mul_ad x0 x1 in
    let v4 = add_ad v2 v3 in
    let v5 = make_forward 1. 0. in
    let v6 = exp_ad v4 in
    let v7 = make_forward 1. 0. in
    let v8 = add_ad v5 v6 in
    let v9 = div_ad v7 v8 in
    v9;;
diff f_forward (x0, x1);;
(* book: combined-forward end *)

(* book: combined-reverse begin *)
let x0 = make_reverse 1.;;
let x1 = make_reverse 1.;;
let f_reverse x =
    let x0, x1 = x in
    let v2 = sin_ad x0 in
    let v3 = mul_ad x0 x1 in
    let v4 = add_ad v2 v3 in
    let v5 = make_reverse 1. in
    let v6 = exp_ad v4 in
    let v7 = make_reverse 1. in
    let v8 = add_ad v5 v6 in
    let v9 = div_ad v7 v8 in
    v9;;
diff f_reverse (x0, x1);;
module Book_naive = struct
  type nonrec t = t = DF of float * float | DR of float * float ref * adjoint
  let tangent = tangent
  let adjoint = adjoint
  let reverse_push = reverse_push
  let make_forward = make_forward
  let add_ad = add_ad
  let mul_ad = mul_ad
end;;
(* book: combined-reverse end *)

(* book: combined-checks begin *)
book_check "combined forward" dx0 (diff f_forward (make_forward 1. 1.,make_forward 1. 0.));;
book_check "combined reverse" dx0 (diff f_reverse (x0,x1));;
book_check "combined repeat reverse" dx0 (diff f_reverse (x0,x1));;
let shared_x = make_reverse 2.;;
let shared_u = mul_ad shared_x shared_x;;
let shared_r = add_ad shared_u shared_u;;
reverse_reset [shared_r];;
reverse_push [1.,shared_r];;
book_check "combined shared graph" 8. !(adjoint shared_x);;

(* book: combined-checks end *)

(* book: precision-modules begin *)
module S = Owl_algodiff_generic.Make 
  (Owl_algodiff_primal_ops.S)
module D = Owl_algodiff_generic.Make 
  (Owl_algodiff_primal_ops.D);;
(* book: precision-modules end *)

(* book: owl-function begin *)
module AD = Algodiff.D

let f x =
  let x0 = AD.Mat.get x 0 0 in
  let x1 = AD.Mat.get x 0 1 in
  AD.Maths.((F 1.) / (F 1. + exp (sin x0 + x0 * x1)));;
(* book: owl-function end *)

(* book: owl-input begin *)
open AD;;
let input = Arr (Dense.Matrix.D.ones 1 2);;
let result = f input |> unpack_flt;;
(* book: owl-input end *)

(* book: owl-forward begin *)
open AD;;
let x = make_forward input (Mat.ones 1 2) (tag ());;
let y = f x;;
let y' = tangent y;;
(* book: owl-forward end *)

(* book: owl-reverse begin *)
open AD;;
let x = Mat.ones 1 2;; 
let x' = make_reverse x (tag ());;
let y = f x';;                  (* forward pass *)
let _ = reverse_prop (F 1.) y;; (* backward pass *)
let y' = adjval x';; 
(* book: owl-reverse end *)

(* book: owl-mode-checks begin *)
book_check "owl reverse dx0" dx0 (A.get (unpack_arr y') [|0;0|]);;
book_check "owl reverse dx1" dx1 (A.get (unpack_arr y') [|0;1|]);;
(* book: owl-mode-checks end *)

(* book: owl-diff begin *)
open Algodiff.D

let f x = Maths.(tanh x);;
let d = diff f (F 0.1);;
(* book: owl-diff end *)

(* book: higher-order begin *)
let f x = Maths.(tanh x);;
let f1 = diff f;;
let f2 = diff f1;;
let f3 = diff f2;;
let f4 = diff f3;;
(* book: higher-order end *)

(* book: two-body begin *)
open Algodiff.D

let f y =
  let y0 = Mat.get y 0 0 in
  let y1 = Mat.get y 0 1 in
  let y2 = Mat.get y 0 2 in
  let y3 = Mat.get y 0 3 in

  let r = Maths.(sqrt ((sqr y0) + (sqr y1))) in
  let y0' = y2 in
  let y1' = y3 in
  let y2' = Maths.( neg y0 / pow r (F 3.)) in
  let y3' = Maths.( neg y1 / pow r (F 3.)) in

  let y' = Mat.ones 1 4 in
  let y' = Mat.set y' 0 0 y0' in
  let y' = Mat.set y' 0 1 y1' in
  let y' = Mat.set y' 0 2 y2' in
  let y' = Mat.set y' 0 3 y3' in
  y';;
(* book: two-body end *)

(* book: two-body-jacobian begin *)
let y = Mat.ones 1 4;;
let result = jacobian f y;;

let j = unpack_arr result |> Dense.Matrix.D.transpose;;
(* book: two-body-jacobian end *)

(* book: jacobian-eigenvalues begin *)
let eigenvalues = Owl_linalg.D.eigvals j;;
(* book: jacobian-eigenvalues end *)

(* book: newton begin *)
open Algodiff.D;;
let newton ?(eta=F 0.01) ?(eps=1e-6) ?(max_iter=10000) f x =
  let rec loop remaining x =
    let g = grad f x in
    let norm = Maths.l2norm' g |> unpack_flt in
    if not (Float.is_finite norm) then failwith "non-finite Newton gradient";
    if norm < eps then x
    else if remaining = 0 then failwith "Newton iteration limit"
    else let h = hessian f x in
      loop (remaining - 1) Maths.(x - eta * g *@ (inv h))
  in loop max_iter x;;
(* book: newton end *)

(* book: newton-example begin *)
let stationary_point =
  let f x = Maths.(cos x |> sum') in
  newton f (Mat.uniform 1 2);;
(* book: newton-example end *)

(* book: newton-checks begin *)
let quadratic x = Maths.(sum' (sqr x));;
let quadratic_root = newton ~eta:(F 1.) quadratic (pack_arr (Owl.Mat.of_array [|3.; -4.|] 1 2));;
book_check "Newton quadratic" 0. (Maths.l2norm' quadratic_root |> unpack_flt);;
book_check "Newton stationary gradient" 0. (Maths.l2norm' (grad (fun x -> Maths.(cos x |> sum')) stationary_point) |> unpack_flt);;
(* book: newton-checks end *)

(* book: owl-type begin *)
#show_type Algodiff.D.t;;
#show_type Algodiff.D.adjoint;;
#show_type Algodiff.D.register;;
#show_type Algodiff.D.label;;
#show_type Algodiff.D.op;;
(* book: owl-type end *)

(* book: perturbation-confusion begin *)
open Book_naive;;
let diff f x =
    match x with
    | DF (_, _)    ->
      f x |> tangent
    | DR (_, _, _) ->
      let r = f x in
      reverse_push [(1., r)];
      !(adjoint x);;
let f x =
    let g = diff (fun y -> add_ad x y) in
    mul_ad x (make_forward (g (make_forward 2. 1.)) 1.);;
diff f (make_forward 2. 1.);;
(* book: perturbation-confusion end *)

(* book: tagged-check begin *)
book_check "intentional naive confusion" 4. (diff f (make_forward 2. 1.));;
let tagged_result =
  let open Owl.Algodiff.D in
  let f x = Maths.(x * diff (fun y -> x + y) (F 2.)) in
  diff f (F 2.) |> unpack_flt;;
book_check "Owl nested differentiation" 1. tagged_result;;
(* book: tagged-check end *)

(* book: constant-function begin *)
open Algodiff.D;;
let f_forward x =
  let x0, x1 = x in
  let open Maths in
  let v2 = sin x0 in
  let v3 = mul x0 x1 in
  let v4 = add v2 v3 in
  let v5 = F 1. in
  let v6 = exp v4 in
  let v7 = F 1. in
  let v8 = add v5 v6 in
  div v7 v8;;
(* book: constant-function end *)

(* book: owl-sin-builder begin *)
open Algodiff.D

module Sin = struct
  let label = "sin"
  let ff_f a = F A.Scalar.(sin a)
  let ff_arr a = Arr A.(sin a)
  let df _cp ap at = Maths.(at * cos ap)
  let dr a _cp ca = Maths.(!ca * cos (primal a))
end

let new_sin_ad = Builder.build_siso (module Sin : Builder.Siso);;
(* book: owl-sin-builder end *)

(* book: owl-builder-example begin *)
let f x =
    let x1 = Mat.get x 0 0 in
    let x2 = Mat.get x 0 1 in
    Maths.(div (F 1.) (F 1. + exp (x1 * x2 + (new_sin_ad x1))));;
let x = Mat.ones 1 2;;
let _ = grad f x |> unpack_arr;;
(* book: owl-builder-example end *)

(* book: lazy-sin-module begin *)
open Algodiff.D

module Sin = struct
  let label = "sin"
  let ff_f a = F A.Scalar.(sin a)
  let ff_arr a = Arr A.(sin a)
  let df _cp ap at = Maths.(at * cos ap)
  let dr a _cp ca = Maths.(!ca * cos (primal a))
end;;
(* book: lazy-sin-module end *)

(* book: lazy-sin-builder begin *)
let _sin_ad = lazy (Builder.build_siso (module Sin : Builder.Siso));;
let new_sin_ad = Stdlib.Lazy.force _sin_ad;;
(* book: lazy-sin-builder end *)

(* book: lazy-check begin *)
let open Owl.Algodiff.D in
book_check "lazy builder derivative" (cos 0.4) (diff new_sin_ad (F 0.4) |> unpack_flt);;
Printf.printf "PASS: %d teaching/session property checks\n" !book_checks;;
(* book: lazy-check end *)
