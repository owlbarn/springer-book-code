open Owl_rllib
module N = Neural.Make (Owl.Neural.S.Graph)

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

let total_steps = 50_000
(* book: training begin *)
let train target_network =
  let rngs = Rng.create 42 in
  let network = N.mlp ~seed:rngs.network_seed ~shape:[|4|]
      ~hidden:[64;64] ~outputs:2 in
  let config = { Dqn.S.default with target_network } in
  let returns = Dqn.S.train ~config ~steps:total_steps ~rngs ~network
      (Cartpole.make ~rng:rngs.env ()) in
(* book: training end *)
  let env = Cartpole.make ~rng:rngs.eval () in
  let scores = Array.init 20 (fun _ ->
    let rec go obs total =
      let st = env.step (Dp.argmax (N.predict network obs)) in
      let total = total +. st.reward in
      if st.terminated || st.truncated then total else go st.obs total in
    go (env.reset ()) 0.) in
  Printf.printf "Target network=%b: greedy evaluation %.1f\n" target_network (mean scores);
  returns
let () =
  let with_target = train true in
  let without_target = train false in
  chart "dqn.pdf" "Return (trailing 25 episodes)"
    [(30,90,160),"Target network",25,with_target;
     (190,80,40),"Online bootstrap",25,without_target]
