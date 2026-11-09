#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
open Owl;;
open Owl_plplot;;
Owl_stats_prng.init 79;;
let book_checks = ref 0;;
let book_assert label value = if not value then failwith label; incr book_checks;;
let book_near label expected actual tol = book_assert label (Float.is_finite actual && abs_float (actual -. expected) < tol);;

(* book: newton-root begin *)
open Algodiff.D

let f x = Maths.(x ** (F 2.) - (F 2.))

let _ =
  let x = ref 1. in
  for _ = 0 to 6 do
    let g = diff f (F !x) |> unpack_elt in
    let v = f (F !x) |> unpack_elt in
    x := !x -. v /. g;
    Printf.printf "%.15f\n" !x
  done;;
(* book: newton-root end *)

(* book: brent-root begin *)
let f x = x *. x -. 2.;;
Owl_maths_root.brent f 0. 2.;;
(* book: brent-root end *)

(* book: root-checks begin *)
book_near "Brent sqrt2" (sqrt 2.) (Owl_maths_root.brent (fun x -> x*.x-.2.) 0. 2.) 1e-6;;
(* book: root-checks end *)

(* book: humps begin *)
open Algodiff.D

let f x = Maths.(
  (F 1.) / ((x - F 0.3) ** (F 2.) + F 0.01) +
  (F 1.) / ((x - F 0.9) ** (F 2.) + F 0.04) - F 6.)

let g = diff f
let f' x = f (F x) |> unpack_flt
let g' x = g (F x) |> unpack_flt;;
(* book: humps end *)

(* book: humps-plot begin *)
let h = Plot.create ~m:1 ~n:2 "plot_hump.png" in
Plot.set_pen_size h 1.5;
Plot.subplot h 0 0;
Plot.plot_fun ~h f' 0. 2.;
Plot.set_ylabel h "f(x)";
Plot.subplot h 0 1;
Plot.plot_fun ~h g' 0. 2.;
Plot.set_ylabel h "f'(x)";
Plot.output h;;
(* book: humps-plot end *)

(* book: critical-points begin *)
Owl_maths_root.brent g' 0. 0.4;;
Owl_maths_root.brent g' 0.4 0.7;;
Owl_maths_root.brent g' 0.7 1.0;;
(* book: critical-points end *)

(* book: critical-checks begin *)
List.iter (fun (a,b) -> let r=Owl_maths_root.brent ~xtol:1e-12 g' a b in book_near "stationary derivative" 0. (g' r) 1e-7) [0.,0.4;0.4,0.7;0.7,1.];;
book_assert "first local maximum" (diff g (F 0.300375626258) |> unpack_flt < 0.);;
book_assert "middle local minimum" (diff g (F 0.637009406269) |> unpack_flt > 0.);;
book_assert "last local maximum" (diff g (F 0.892716303287) |> unpack_flt < 0.);;
(* book: critical-checks end *)

(* book: rosenbrock begin *)
open Algodiff.D
module N = Dense.Ndarray.D

let rosenbrock a =
  let x = Mat.get a 0 0 in
  let y = Mat.get a 0 1 in
  Maths.( (F 100.) * (y - (x ** (F 2.))) ** (F 2.) +
    (F 1. - x) ** (F 2.) |> sum');;
(* book: rosenbrock end *)

(* book: gradient-init begin *)
let v = N.of_array [|2.; -0.5|] [|1; 2|]
let traj = ref (N.copy v)
let a = ref v
let eta = 0.0001
let n = 200;;
(* book: gradient-init end *)

(* book: gradient-steps begin *)
let _ =
  for i = 1 to n - 1 do
  let u = grad rosenbrock (Arr !a) |> unpack_arr in
  a := N.(sub !a (scalar_mul eta u));
  traj := N.concatenate [|!traj; (N.copy !a)|]
  done;;
(* book: gradient-steps end *)

(* book: gradient-checks begin *)
let cost x = rosenbrock (Arr x) |> unpack_flt;;
book_near "Rosenbrock known minimum" 0. (cost (N.of_array [|1.;1.|] [|1;2|])) 1e-15;;
book_near "Rosenbrock initial value" 2026. (cost v) 1e-12;;
book_assert "trajectory shape" (N.shape !traj = [|n;2|]);;
book_assert "gradient descent reduces objective" (cost !a < cost v);;
book_assert "all descent steps nonincreasing" (let ok=ref true in for i=1 to n-1 do let prev=Owl.Mat.row !traj (i-1) and curr=Owl.Mat.row !traj i in if cost curr > cost prev +. 1e-9 then ok:=false done; !ok);;
(* book: gradient-checks end *)

(* book: gradient-plot begin *)
let plot () =
  let a, b = Dense.Matrix.D.meshgrid (-2.) 2. (-1.) 3. 50 50 in
  let c = N.(scalar_mul 100.
    (pow_scalar (sub b (pow_scalar a 2.)) 2.) +
    (pow_scalar (scalar_sub 1. a) 2.)) in

  let h = Plot.create ~m:1 ~n:2 "plot_gradients.png" in
  Plot.subplot h 0 0;
  Plot.(mesh ~h ~spec:[ NoMagColor ] a b c);

  Plot.subplot h 0 1;
  Plot.contour ~h a b c;
  let vx = N.get_slice [[]; [0]] !traj in
  let vy = N.get_slice [[]; [1]] !traj in
  Plot.plot ~h vx vy;
  Plot.output h;;
plot ();;
(* book: gradient-plot end *)

(* book: owl-parameters begin *)
let p = Owl_optimise.D.Params.default ()
let _ = p.epochs <- 10.
let _ = p.gradient <- Owl_optimise.D.Gradient.GD;;
p.learning_rate <- Owl_optimise.D.Learning_Rate.Const 0.001;;
(* book: owl-parameters end *)

(* book: owl-minimise begin *)
let init_value = N.of_array [|2.;-0.5|] [|1;2|] |> pack_arr
let optimisation_state, optimised = Owl_optimise.D.minimise_fun p rosenbrock init_value;;
(* book: owl-minimise end *)

(* book: owl-result-checks begin *)
book_assert "Owl minimise returns lower loss" (rosenbrock optimised |> unpack_flt < cost v);;
book_assert "Owl returned finite loss" (Float.is_finite (rosenbrock optimised |> unpack_flt));;
(* book: owl-result-checks end *)

(* book: newton-optimise begin *)
open Algodiff.D;;
let newton ?(eta=F 0.01) ?(eps=1e-6) ?(max_iter=10000) f x =
  let rec loop remaining x =
    let g, h = gradhessian f x in
    let norm = Maths.l2norm' g |> unpack_flt in
    if not (Float.is_finite norm) then failwith "non-finite Newton gradient";
    if norm < eps then x
    else if remaining = 0 then failwith "Newton iteration limit"
    else
      loop (remaining - 1) Maths.(x - eta * g *@ (inv h))
  in loop max_iter x;;
let stationary_point =
  let f x = Maths.(cos x |> sum') in
  newton f (Mat.uniform 1 2);;
Mat.print stationary_point;;
(* book: newton-optimise end *)

(* book: newton-checks begin *)
book_near "Newton stationary gradient" 0. (grad (fun x -> Maths.(cos x |> sum')) stationary_point |> Maths.l2norm' |> unpack_flt) 1e-6;;
let root = newton ~eta:(F 1.) (fun x -> Maths.(sum' (sqr x))) (Mat.ones 1 2);;
book_near "Newton quadratic minimum" 0. (Maths.l2norm' root |> unpack_flt) 1e-12;;
book_assert "Newton iteration limit" (try ignore (newton ~max_iter:0 (fun x -> Maths.(sum' (sqr x))) (Mat.ones 1 2));false with Failure _ -> true);;
Printf.printf "PASS: %d optimisation property checks\n" !book_checks;;
(* book: newton-checks end *)
