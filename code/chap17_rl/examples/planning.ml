open Owl_rllib
open Owl_plplot
module Mat = Owl.Dense.Matrix.D
let () =
(* book: planning begin *)
  let grid = Gridworld.sutton_4x4 () in
  let pi, v = Dp.value_iteration ~gamma:0.95 grid.mdp in
  let _, vp = Dp.policy_iteration ~gamma:0.95 grid.mdp in
  Array.iter2 (fun a b -> assert (abs_float (a-.b) < 1e-7)) v vp;
  Gridworld.print_values grid v;
  Gridworld.print_policy grid pi;
(* book: planning end *)
  let h = Plot.create "values.pdf" in
  Plot.set_xrange h (-0.6) 3.6; Plot.set_yrange h (-0.6) 3.6;
  Plot.set_xlabel h "Column"; Plot.set_ylabel h "Row (bottom to top)";
  Plot.set_xticklabels h [0.,"0";1.,"1";2.,"2";3.,"3"];
  Plot.set_yticklabels h [0.,"0";1.,"1";2.,"2";3.,"3"];
  let arrows = [|"^";">";"v";"<"|] in
  for row = 0 to 3 do for col = 0 to 3 do
    let i = row*4+col in
    let x = float_of_int col and y = float_of_int (3-row) in
    let shade = 140 + int_of_float (100. *. (v.(i)+.3.) /.3.) in
    Plot.draw_rect ~h ~spec:[Plot.RGB (shade,shade,255); Plot.FillPattern 0]
      (x-.0.48) (y+.0.48) (x+.0.48) (y-.0.48);
    let label = if i=0 || i=15 then "G" else arrows.(pi.(i)) in
    Plot.text ~h ~spec:[Plot.RGB (0,0,0)] (x-.0.23) (y+.0.14) (Printf.sprintf "%.2f" v.(i));
    Plot.text ~h ~spec:[Plot.RGB (0,0,0)] (x-.0.05) (y-.0.23) label
  done done;
  Plot.output h;
(* book: slippery begin *)
  List.iter (fun slippery ->
    let lake = Frozen_lake.make ~slippery () in
    let _, optimal = Dp.value_iteration ~gamma:0.95 lake in
    let random = Dp.policy_evaluation ~gamma:0.95 lake
        (Array.make_matrix 16 4 0.25) in
    assert (optimal.(0) >= random.(0));
    Printf.printf "Slippery=%b: random value %.4f; optimal %.4f\n"
      slippery random.(0) optimal.(0)) [false; true]
(* book: slippery end *)
