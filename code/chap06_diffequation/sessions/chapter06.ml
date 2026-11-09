#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
#require "owl-ode";;
#require "owl-ode-odepack";;
open Owl;;
open Owl_ode;;
open Owl_ode.Types;;
open Owl_plplot;;
let book_matrix_printer formatter value =
  Owl_pretty.print_dsnda ~max_row:6 ~max_col:6 ~formatter value;;
#install_printer book_matrix_printer;;
Owl_stats_prng.init 76;;

(* book: euler-loop begin *)
let x = ref 0.;;
let y = ref 0.;;
let target = 1.;;
let step = 0.001;;
let f x y = 2. *. x *. y +. x;;
while !x < target do
  let h = min step (target -. !x) in
  y := !y +. h *. (f !x !y);
  x := !x +. h
done;;
(* book: euler-loop end *)

(* book: analytic-result begin *)
(Owl_const.e -. 1.)/. 2.;;
(* book: analytic-result end *)

(* book: euler-midpoint begin *)
#use "book_ode.ml";;
let _ =
  let target = 2.6 in
  let h = Plot.create "plot_rk01.png" in
  Plot.(plot_fun ~h ~spec:[ RGB (66,133,244); LineStyle 1; 
    LineWidth 2.; Marker "*" ] f' 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (219,68,55); LineStyle 2; 
    LineWidth 2.; Marker "+" ] (euler 0.01) 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (219,68,55); LineStyle 2;
    LineWidth 2.; Marker "." ] (euler 0.001) 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (244,180,0); LineStyle 3; 
    LineWidth 2.; Marker "+" ] (midpoint 0.01) 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (244,180,0); LineStyle 3; 
    LineWidth 2.; Marker "." ] (midpoint 0.001) 2. target);
  Plot.(legend_on h ~position:NorthWest 
    [|"Close-Form Solution"; "Euler (step = 0.01)";
    "Euler (step = 0.001)"; "Midpoint (step = 0.01)"; 
    "Midpoint (step = 0.001)"|]);
  Plot.output h;;
(* book: euler-midpoint end *)

(* book: linear-system begin *)
let f y t =
  let a = [|[|1.; -1.|];[|2.; -3.|]|]|> Mat.of_arrays in
  Mat.(a *@ y);;
(* book: linear-system end *)

(* book: time-specification begin *)
let tspec = Owl_ode.Types.
  (T1 {t0 = 0.; duration = 2.; dt=1E-3});;
(* book: time-specification end *)

(* book: initial-state begin *)
let x0 = Mat.of_array [|-1.; 1.|] 2 1;;
(* book: initial-state end *)

(* book: rk4-solution begin *)
let ts, ys = Owl_ode.Ode.odeint
  Owl_ode.Native.D.rk4 f x0 tspec ();;
(* book: rk4-solution end *)

(* book: rk4-interface begin *)
#show_val Owl_ode.Native.D.rk4;;
(* book: rk4-interface end *)

(* book: oscillator begin *)
let f y t =
  let a = [|[|0.; 1.|];[|-1.; 0.|]|]|> Mat.of_arrays in
  Mat.(a *@ y);;
let x0 = Mat.of_array [|1.; 0.|] 2 1;;
let tspec = Owl_ode.Types.(
  T1 {t0 = 0.; duration = 10.; dt=1E-2});;
let ts, ys = Owl_ode.Ode.odeint 
  Owl_ode.Native.D.euler f x0 tspec ();;
let _ =
  let h = Plot.create "plot_circle.png" in
  Plot.plot ~h (Mat.row ys 0) (Mat.row ys 1);
  Plot.output h;;
(* book: oscillator end *)

(* book: custom-rk45 begin *)
let custom_rk45 = Owl_ode.Native.D.rk45
  ~tol:1E-7 ~dtmax:0.1;;
let ts, xs = Owl_ode.Ode.odeint custom_rk45 f x0 tspec ();;
(* book: custom-rk45 end *)

(* book: adaptive-solver begin *)
let f y t = Mat.((2. $* y *$ t) +$ t);;
let tspec = Owl_ode.Types.(
  T1 {t0 = 0.; duration = 1.; dt=1E-3});;
let y0 = Mat.zeros 1 1;;
let solver = Owl_ode.Native.D.rk45 ~tol:1E-9 ~dtmax:10.0;;
let _, ys = Owl_ode.Ode.odeint solver f y0 tspec ();;
(* book: adaptive-solver end *)

(* book: adaptive-result begin *)
Mat.transpose ys;;
(* book: adaptive-result end *)

(* book: two-body begin *)
let f y _t =
  let y = Mat.to_array y in
  let r = Maths.(sqrt ((sqr y.(0)) +. (sqr y.(1)))) in
  let y0' = y.(2) in
  let y1' = y.(3) in
  let y2' = -.y.(0) /. (Maths.pow r 3.) in
  let y3' = -.y.(1) /. (Maths.pow r 3.) in
  [| [|y0'; y1'; y2'; y3'|] |] |> Mat.of_arrays;;

let y0 = Mat.of_array [|-1.; 0.; 0.5; 0.5|] 1 4;;

let tspec = Owl_ode.Types.(T1 {
  t0 = 0.;
  duration = 20.;
  dt=1E-2});;
 
let custom_solver = Native.D.rk45 ~tol:1E-9 ~dtmax:10.0;;
(* book: two-body end *)

(* book: two-body-plot begin *)
let plot () =
  let ts, ys = Ode.odeint custom_solver f y0 tspec () in
  let h = Plot.create "two_body.png" in
  let open Plot in
  plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1 ]
    (Mat.col ys 0) (Mat.col ys 1);
  scatter ~h ~spec:[ Marker "#[0x229a]"; MarkerSize 5. ]
    (Mat.zeros 1 1) (Mat.zeros 1 1);
  text ~h ~spec:[ RGB (51,51,51)] (-.0.3) 0. "Massive Object";
  output h;;
plot ();;
(* book: two-body-plot end *)

(* book: lorenz-system begin *)
let sigma = 10.;;
let beta = 8. /. 3.;;
let rho = 28.;;
let f y _t =
  let y = Mat.to_array y in
  let y0' = sigma *. (y.(1) -. y.(0)) in
  let y1' = y.(0) *. (rho -. y.(2)) -. y.(1) in
  let y2' = y.(0) *. y.(1) -. beta *. y.(2) in
  [| [|y0'; y1'; y2'|] |] |> Mat.of_arrays;;
(* book: lorenz-system end *)

(* book: lorenz-plot begin *)
let y0 = Mat.of_array [|-1.;0.;0.5|] 1 3;;
let _ =
  let ts, ys = Ode.odeint custom_solver f y0 tspec () in
  let h = Plot.create ~m:2 ~n:2 "lorenz_01.png" in
  let open Plot in
  subplot h 0 0;
  set_xlabel h "time";
  set_ylabel h "value on three axes";
  plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1 ] ts (Mat.col ys 2);
  plot ~h ~spec:[ RGB (219, 68,  55); LineStyle 1 ] ts (Mat.col ys 1);
  plot ~h ~spec:[ RGB (244, 180,  0); LineStyle 1 ] ts (Mat.col ys 0);
  subplot h 0 1;
  set_xlabel h "x-axis";
  set_ylabel h "y-axis";
  plot ~h ~spec:[ RGB (66, 133, 244) ] (Mat.col ys 0) (Mat.col ys 1);
  subplot h 1 0;
  set_xlabel h "y-axis";
  set_ylabel h "z-axis";
  plot ~h ~spec:[ RGB (66, 133, 244) ] (Mat.col ys 1) (Mat.col ys 2);
  subplot h 1 1;
  set_xlabel h "x-axis";
  set_ylabel h "z-axis";
  plot ~h ~spec:[ RGB (66, 133, 244) ] (Mat.col ys 0) (Mat.col ys 2);
  output h;;
(* book: lorenz-plot end *)

(* book: lorenz-perturbation begin *)
let comparison_solver = Native.D.rk4;;
let y00 = Mat.of_array [|-1.; -1.; 1.|] 1 3;;
let y01 = Mat.of_array [|-1.001; -1.001; 1.001|] 1 3;;
let ts0, ys0 = Ode.odeint comparison_solver f y00 tspec ();;
let ts1, ys1 = Ode.odeint comparison_solver f y01 tspec ();;
(* book: lorenz-perturbation end *)

(* book: shared-times begin *)
assert (Mat.shape ts0 = Mat.shape ts1 && Mat.approx_equal ts0 ts1);;
let ts = ts0;;
assert (Mat.shape ys0 = Mat.shape ys1);;
(* book: shared-times end *)

(* book: lorenz-distance begin *)
let _ =
  let h = Plot.create ~m:1 ~n:2 "lorenz_02.png" in
  let open Plot in
  subplot h 0 0;
  set_xlabel h "time";
  set_ylabel h "value on three axes";
  plot ~h ~spec:[ RGB (244, 180,  0); LineStyle 1 ] ts (Mat.col ys1 0);
  plot ~h ~spec:[ RGB (219, 68,  55); LineStyle 1 ] ts (Mat.col ys1 1);
  plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1 ] ts (Mat.col ys1 2);
  subplot h 0 1;
  let diff = Mat.(
    sqr ((col ys0 0) - (col ys1 0)) +
    sqr ((col ys0 1) - (col ys1 1)) +
    sqr ((col ys0 2) - (col ys1 2))
    |> sqrt
  )
  in
  plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1 ] ts diff;
  set_xlabel h "time";
  set_ylabel h "distance of two systems";
  output h;;
(* book: lorenz-distance end *)

(* book: damped-force begin *)
let a = 1.0;;
let damped_noforcing a (xs, ps) _t : Owl.Mat.mat =
  Owl.Mat.((xs *$ -1.0) + (ps *$ (-1.0 *. a)));;
(* book: damped-force end *)

(* book: symplectic-solvers begin *)
let main () =
  let x0 = Owl.Mat.of_array [| -0.25 |] 1 1 in
  let p0 = Owl.Mat.of_array [| 0.75 |] 1 1 in
  let t0, duration = 0.0, 15.0 in
  let f = damped_noforcing a in
  let tspec = T1 { t0; duration; dt=0.1 } in
  let t, sol1, _ = Ode.odeint (module Symplectic.D.Leapfrog)
    f (x0, p0) tspec () in
  let _, sol2, _ = Ode.odeint Symplectic.D.ruth3
    f (x0, p0) tspec () in
  let _, sol3, _ =
    Ode.odeint (module Symplectic.D.Symplectic_Euler)
    f (x0, p0) tspec ()
  in
  t, sol1, sol2, sol3;;
(* book: symplectic-solvers end *)

(* book: symplectic-plot begin *)
let plot_sol fname t sol1 sol2 sol3 =
  let h = Plot.create fname in
  let open Plot in
  plot ~h ~spec:[RGB (0,0,255); LineStyle 1] t (Mat.col sol1 0);
  plot ~h ~spec:[RGB (0,150,0); LineStyle 2] t (Mat.col sol2 0);
  plot ~h ~spec:[RGB (200,0,0); LineStyle 3] t (Mat.col sol3 0);
  legend_on h [| "Leapfrog"; "Ruth3"; "Symplectic Euler" |];
  output h;;
let t, sol1, sol2, sol3 = main ();;
plot_sol "damped.png" t sol1 sol2 sol3;;
(* book: symplectic-plot end *)

(* book: van-der-pol begin *)
open Owl_ode;;
open Owl_ode.Types;;
open Owl_plplot;;

let van_der_pol mu =
  fun y _t ->
    let y = Mat.to_array y in
    let a = y.(1) in
    let b = mu *. (1. -. Maths.sqr y.(0)) *. y.(1) -. y.(0) in
    [|[|a; b|]|] |> Mat.of_arrays;;
(* book: van-der-pol end *)

(* book: vdp-rk45 begin *)
let f_non_stiff = van_der_pol 1.;;
let y0 = Mat.of_array [| 0.02; 0.03 |] 1 2;;
let tspec = T1 { t0 = 0.0; dt = 0.01; duration = 30.0 };;
let ts, ys = Ode.odeint (Native.D.rk45 ~tol:1E-8 ~dtmax:0.05)
  f_non_stiff y0 tspec ();;
(* book: vdp-rk45 end *)

(* book: vdp-rk45-plot begin *)
let () =
  let fname = "vdp_rk45_nonstiff.png" in
  let h = Plot.create ~n:2 ~m:1 fname in
  let open Plot in
  subplot h 0 0;
  plot ~h ~spec:[ LineStyle 1 ] (Mat.col ys 0) (Mat.col ys 1);
  subplot h 0 1;
  plot ~h ~spec:[ LineStyle 1 ] ts Mat.(col ys 1);
  plot ~h ~spec:[ LineStyle 3 ] ts Mat.(col ys 0);
  output h;;
(* book: vdp-rk45-plot end *)

(* book: vdp-lsoda begin *)
let f_stiff = van_der_pol 1000.;;
let y0 = Mat.of_array [| 2.; 0. |] 1 2;;
let tspec = T1 { t0 = 0.0; dt = 0.01; duration = 3000.0 };;
let () =
  let ts, ys = Ode.odeint (module Owl_ode_odepack.Lsoda)
    f_stiff y0 tspec () in
  let fname = "vdp_odepack_stiff.png" in
  let h = Plot.create fname in
  let open Plot in
  set_yrange h (-2.) 2.;
  plot ~h ~spec:[ LineStyle 1 ] ts Mat.(col ys 1);
  plot ~h ~spec:[ LineStyle 3 ] ts Mat.(col ys 0);
  output h;;
(* book: vdp-lsoda end *)
