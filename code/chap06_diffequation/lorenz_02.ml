open Owl
open Owl_ode
open Owl_plplot

let y0 = Mat.of_array [|-1.; 0.; 0.5; 0.5|] 1 4
let tspec = Owl_ode.Types.(T1 {
  t0 = 0.;
  duration = 20.;
  dt=1E-2})
let custom_solver = Native.D.rk45 ~tol:1E-9 ~dtmax:10.0

let sigma = 10.
let beta = 8. /. 3.
let rho = 28.

let f y _t =
  let y = Mat.to_array y in
  let y0' = sigma *. (y.(1) -. y.(0)) in
  let y1' = y.(0) *. (rho -. y.(2)) -. y.(1) in
  let y2' = y.(0) *. y.(1) -. beta *. y.(2) in
  [| [|y0'; y1'; y2'|] |] |> Mat.of_arrays

let y00 = Mat.of_array [|-1.; -1.; 1.|] 1 3
let y01 = Mat.of_array [|-1.001; -1.001; 1.001|] 1 3
let ts0, ys0 = Ode.odeint custom_solver f y00 tspec ()
let ts1, ys1 = Ode.odeint custom_solver f y01 tspec ()

let r0, c0 = Mat.shape ys0
let r1, c1 = Mat.shape ys1
let r  = if (r0 < r1) then r0 else r1
let ts = if (r0 < r1) then ts0 else ts1
let ys0 = Mat.get_slice [[0; r-1]; []] ys0
let ys1 = Mat.get_slice [[0; r-1]; []] ys1

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
  output h