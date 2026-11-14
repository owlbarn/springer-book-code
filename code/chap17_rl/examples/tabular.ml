(* A3. Learning without a model.

   From here on the agent cannot look at the transition probabilities. It can
   only act and observe what happens. Monte Carlo and TD(0) estimate values
   from experience; SARSA and Q-learning learn to act.

   Run: dune exec examples/classic/a3_tabular.exe *)

open Owl_rllib

module Mat = Owl.Dense.Matrix.D
open Owl_plplot
let mean xs = Array.fold_left (+.) 0. xs /. float_of_int (Array.length xs)
let rolling window ys =
  let window = min window (Array.length ys) in
  Array.init (Array.length ys-window+1) (fun i -> mean (Array.sub ys i window))
let curve h (r,g,b) label window ys =
  assert (Array.length ys > 0 && Array.for_all Float.is_finite ys);
  let smooth = rolling window ys in
  let n = Array.length smooth in
  let xs = Mat.init_2d n 1 (fun i _ -> float_of_int (i+min window (Array.length ys))) in
  let values = Mat.of_array smooth n 1 in
  Plot.plot ~h ~spec:[Plot.RGB (r,g,b); Plot.LineStyle (if r < 100 then 1 else 2); Plot.LineWidth 2.] xs values;
  let len = Array.length ys in
  Printf.printf "%s: %d episodes; last %d mean %.2f\n" label len (min window len)
    (mean (Array.sub ys (max 0 (len-window)) (min window len)))

(* Draw the key with the same RGB, dash and width as the data curves. *)
let legend h ~right ~top ~xmax ~ymin ~ymax entries =
  let longest = List.fold_left (fun n (_,label) -> max n (String.length label)) 0 entries in
  let width = 0.17 +. 0.018 *. float_of_int longest in
  let height = 0.075 *. float_of_int (List.length entries) +. 0.04 in
  let left = if right then 0.96 -. width else 0.04 in
  let upper = if top then 0.95 else 0.05 +. height in
  let x u = u *. xmax and y v = ymin +. v *. (ymax -. ymin) in
  Plot.draw_rect ~h ~spec:[Plot.RGB (255,255,255)]
    (x left) (y upper) (x (left+.width)) (y (upper-.height));
  List.iteri (fun i ((r,g,b),label) ->
    let row = upper -. 0.045 -. 0.075 *. float_of_int i in
    Plot.draw_line ~h ~spec:[Plot.RGB (r,g,b);
      Plot.LineStyle (if r < 100 then 1 else 2); Plot.LineWidth 2.]
      (x (left +. 0.02)) (y row) (x (left +. 0.12)) (y row);
    Plot.text ~h ~spec:[Plot.RGB (0,0,0)] ~dx:1.
      (x (left +. 0.15)) (y (row -. 0.018)) label) entries

let chart filename ylabel series =
  let h = Plot.create filename in
  Plot.set_xlabel h "Completed episode"; Plot.set_ylabel h ylabel;
  let xmax = List.fold_left (fun n (_,_,_,ys) -> max n (Array.length ys)) 1 series in
  let ymin = List.fold_left (fun x (_,_,w,ys) -> Array.fold_left min x (rolling w ys)) infinity series in
  let ymax = List.fold_left (fun x (_,_,w,ys) -> Array.fold_left max x (rolling w ys)) neg_infinity series in
  let margin = max 1. ((ymax -. ymin) *. 0.05) in
  Plot.set_xrange h 0. (float_of_int xmax);
  Plot.set_yrange h (ymin -. margin) (ymax +. margin);
  List.iter (fun (colour,label,window,ys) -> curve h colour label window ys) series;
  legend h ~right:(ymax <= 0.) ~top:(ymax > 0.) ~xmax:(float_of_int xmax)
    ~ymin:(ymin-.margin) ~ymax:(ymax+.margin)
    (List.map (fun (colour,label,_,_) -> colour,label) series);
  Plot.output h


let mean_last n xs =
  let len = Array.length xs in
  Array.fold_left ( +. ) 0. (Array.sub xs (len - n) n) /. float_of_int n


let () =
  let rng = Random.State.make [| 0 |] in

  (* 1. Prediction. Env.of_mdp hides the model: only reset and step remain.
     We estimate the value of the random policy on the frozen lake and compare
     with the exact answer, which we can still compute because we built the
     MDP ourselves. *)
(* book: prediction begin *)
  let lake = Frozen_lake.make () in
  let gamma = 0.95 in
  let exact = Dp.policy_evaluation ~gamma lake (Array.make_matrix 16 4 0.25) in
  let env = Env.of_mdp ~rng lake in
  let policy _ = Random.State.int rng 4 in
  (* Monte Carlo waits for the end of the episode and averages the returns. *)
  let mc = Tabular.mc_prediction ~gamma ~episodes:20_000 ~policy env in
  (* TD(0) updates after every step, using its own estimate of the next state. *)
  let td = Tabular.td0_prediction ~gamma ~alpha:0.02 ~episodes:20_000 ~policy env in
(* book: prediction end *)
  print_endline "Frozen lake, value of the random policy:";
  Printf.printf "  %5s %8s %8s %8s\n" "state" "exact" "MC" "TD(0)";
  List.iter (fun s -> Printf.printf "  %5d %8.4f %8.4f %8.4f\n" s exact.(s) mc.(s) td.(s)) [ 0; 6; 10; 14 ];

  (* 2. Control. Sutton & Barto, example 6.6. Both methods explore with an
     epsilon-greedy policy. SARSA learns the value of that exploring policy
     and keeps away from the cliff. Q-learning learns the value of the greedy
     policy and walks along the edge, where exploration sometimes makes it
     fall. *)
(* book: control begin *)
  let cliff = Cliff_walking.make () in
  let run algorithm =
    let env = Env.of_mdp ~rng cliff in
    algorithm rng ~gamma:1. ~alpha:0.5 ~epsilon:0.1 ~episodes:500 env
  in
  let q_sarsa, r_sarsa = run Tabular.sarsa in
  let q_qlearn, r_qlearn = run Tabular.q_learning in
(* book: control end *)
  chart "cliff.pdf" "Return (trailing 25 episodes)"
    [(30,90,160),"SARSA",25,r_sarsa; (190,80,40),"Q-learning",25,r_qlearn];
  print_endline "\nCliff walking, greedy policy learned by SARSA:";
  Cliff_walking.print_policy (Dp.greedy q_sarsa);
  print_endline "\nCliff walking, greedy policy learned by Q-learning:";
  Cliff_walking.print_policy (Dp.greedy q_qlearn);
  Printf.printf
    "\nMean return per episode while learning (last 100 episodes):\n  SARSA %.1f, Q-learning %.1f\n"
    (mean_last 100 r_sarsa)
    (mean_last 100 r_qlearn);

  (* 3. A larger problem: 500 states. For reference we compute the best
     possible mean return with value iteration; the learner stays below it
     because it keeps exploring. *)
  let taxi = Taxi.make () in
  let _, v_star = Dp.value_iteration ~gamma:1. taxi in
  let best = ref 0. in
  Array.iteri (fun s p -> best := !best +. (p *. v_star.(s))) taxi.initial;
  let env = Env.of_mdp ~max_steps:200 ~rng taxi in
  let _, returns = Tabular.q_learning rng ~gamma:0.99 ~alpha:0.1 ~epsilon:0.1 ~episodes:5000 env in
  print_endline "\nTaxi, Q-learning, mean return per episode:";
  List.iter
    (fun upto -> Printf.printf "  episodes %4d-%4d: %7.1f\n" (upto - 499) upto (mean_last 500 (Array.sub returns 0 upto)))
    [ 500; 1000; 2000; 5000 ];
  Printf.printf "  optimal policy, no exploration: %5.1f\n" !best

