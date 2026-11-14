open Owl_rllib
(* book: model begin *)
let grid = Gridworld.sutton_4x4 ()
let mdp = grid.mdp
let uniform = Array.make_matrix mdp.n_states mdp.n_actions 0.25
let values = Dp.policy_evaluation ~gamma:1. mdp uniform
(* book: model end *)
let () =
  print_endline "G . . .\n. . . .\n. . . .\n. . . G";
  Gridworld.print_values grid values;
  Array.iter (fun row -> Array.iter (fun ts ->
    assert (abs_float (List.fold_left (fun s (t:Mdp.transition) -> s +. t.prob) 0. ts -. 1.) < 1e-12)) row) mdp.transitions;
  for s = 0 to mdp.n_states-1 do
    let rhs = Array.init 4 (Dp.backup ~gamma:1. mdp values s)
      |> Array.fold_left (fun acc q -> acc +. 0.25 *. q) 0. in
    assert (abs_float (rhs -. values.(s)) < 1e-8)
  done;
  assert (abs_float (values.(1) +. 14.) < 1e-6);
  print_endline "Bellman residual below 1e-8 in all 16 states."
