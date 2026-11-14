open Owl_rllib
open Owl_rllib_llm
module R = Lm_rl.S
module Mat = Owl.Dense.Matrix.D
open Owl_plplot
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

let sft_steps = 500
let iterations = 250
let () =
(* book: grouped begin *)
  let rewards = [|0.;1.;0.;1.; 0.;0.;0.;0.|] in
  let advantages = Lm_rl.grpo ~group_size:4 rewards in
  Array.iter (Printf.printf "%+.3f ") advantages;
  print_newline ();
  assert (Array.for_all ((=) 0.) (Array.sub advantages 4 4));
(* book: grouped end *)
(* book: supervised begin *)
  let train, test = Lm_task.split ~seed:0 () in
  let policy = R.create ~seed:1 () in
  R.sft ~steps:sft_steps ~rng:(Random.State.make [|1|]) policy train;
  let reference = R.Graph.copy policy in
(* book: supervised end *)
  let points = ref [] in
  let evaluate i =
    let sampled = R.accuracy ~sample:true policy test in
    let greedy = R.accuracy policy test in
    points := (float_of_int i, sampled, greedy) :: !points;
    Printf.printf "Iteration %3d: sampled %.3f; greedy %.3f\n%!" i sampled greedy in
  evaluate 0;

(* book: training begin *)
  let stats = R.train_grpo ~reference ~iterations ~rngs:(Rng.create 7)
      ~problems:train policy ~report:(fun i _ ->
        if i mod 50 = 0 then evaluate i) in
(* book: training end *)
  assert (Array.length stats = iterations);
  Array.iter (fun (s:R.stats) -> assert (Float.is_finite s.reward && Float.is_finite s.kl)) stats;
  let ps = Array.of_list (List.rev !points) in
  let col f = Mat.init_2d (Array.length ps) 1 (fun i _ -> f ps.(i)) in
  let h = Plot.create "accuracy.pdf" in
  Plot.set_xlabel h "GRPO iteration"; Plot.set_ylabel h "Test accuracy";
  Plot.set_yrange h 0. 1.; Plot.set_xrange h 0. (float_of_int iterations);
  Plot.plot ~h ~spec:[Plot.RGB (30,90,160); Plot.LineStyle 1; Plot.LineWidth 2.] (col (fun (x,_,_)->x)) (col (fun (_,y,_)->y));
  Plot.plot ~h ~spec:[Plot.RGB (190,80,40); Plot.LineStyle 2; Plot.LineWidth 2.] (col (fun (x,_,_)->x)) (col (fun (_,_,y)->y));
  legend h ~right:true ~top:true ~xmax:(float_of_int iterations) ~ymin:0. ~ymax:1.
    [(30,90,160),"Sampled"; (190,80,40),"Greedy"]; Plot.output h
