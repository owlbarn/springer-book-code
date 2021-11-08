let sigma = 10.
let beta = 8. /. 3.
let rho = 28.

let f y _t =
  let y = Mat.to_array y in
  let y0' = sigma *. (y.(1) -. y.(0)) in
  let y1' = y.(0) *. (rho -. y.(2)) -. y.(1) in
  let y2' = y.(0) *. y.(1) -. beta *. y.(2) in
  [| [|y0'; y1'; y2'|] |] |> Mat.of_arrays


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
  output h