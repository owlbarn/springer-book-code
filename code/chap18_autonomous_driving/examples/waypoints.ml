module A = Owl.Dense.Ndarray.S
module M = Owl.Dense.Matrix.D
module Plot = Owl_plplot.Plot

(* book: accumulate begin *)
let displacements = A.of_array [|2.; 0.; 1.; 1.; 0.; 2.|] [|3; 2|]
let waypoints = A.cumsum ~axis:0 displacements
(* book: accumulate end *)

let () =
  assert (A.to_array waypoints = [|2.; 0.; 3.; 1.; 3.; 3.|]);
  for i = 0 to 2 do
    Printf.printf "Point %d: (%.1f, %.1f)\n" (i + 1)
      (A.get waypoints [|i; 0|]) (A.get waypoints [|i; 1|])
  done;
  let forward = M.of_array [|0.; 2.; 3.; 3.|] 1 4 in
  let right = M.of_array [|0.; 0.; 1.; 3.|] 1 4 in
  let h = Plot.create "waypoints.pdf" in
  Plot.set_xlabel h "Right (m)";
  Plot.set_ylabel h "Forward (m)";
  Plot.set_xrange h (-0.5) 3.5;
  Plot.set_yrange h (-0.5) 3.5;
  Plot.plot ~h ~spec:[Plot.LineWidth 2.] right forward;
  Plot.output h
