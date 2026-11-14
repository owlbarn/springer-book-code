(* DQN from pixels. Full figure run: --steps 50000 --frames *)
open Owl_rllib
open Owl_rllib_bridges
module N = Neural.Make (Owl.Neural.S.Graph)
(* book: configuration begin *)
let total_steps = 2_048  (* Figure: 50_000 *)
let seed = 0
let config = { Dqn.S.default with gamma = 0.95;
  learning_rate = 0.0001; capacity = 5000; batch_size = 32;
  learning_starts = 512; target_every = 1000;
  epsilon_end = 0.1; exploration_steps = 10_000 }
(* book: configuration end *)
let save_frame filename (width,height,gray) =
  let oc = open_out_bin filename in
  Printf.fprintf oc "P5\n%d %d\n255\n" width height;
  output_bytes oc gray; close_out oc
let () =
  let steps = ref total_steps and frames = ref false in
  Arg.parse ["--steps",Arg.Set_int steps,"Environment steps";
             "--frames",Arg.Set frames,"Save evaluation frames"]
    (fun _ -> ()) "VizDoom DQN";
  let rngs = Rng.create seed in
(* book: environment begin *)
  let make seed =
    Doom.S.make ~downsample:4 ~seed "basic"
    |> Frame_stack.S.wrap ~frames:4 in
  let network = N.cnn ~seed:rngs.network_seed ~shape:[|30;40;4|]
      ~convs:[4,2,8; 3,2,16] ~hidden:[64] ~outputs:3 in
(* book: environment end *)
  let evaluate label =
    let raw, render = Doom.S.make_with_renderer ~downsample:4 ~seed:1000 "basic" in
    let env = Frame_stack.S.wrap ~frames:4 raw in
    let n = if !frames then 20 else 2 in
    let scores = Array.init n (fun episode ->
      let rec go obs total t =
        if !frames && episode=0 && List.mem t [0;5;10;20;40;60] then
          save_frame (Printf.sprintf "%s-%04d.pgm" label t) (render ());
        let st = env.step (Dp.argmax (N.predict network obs)) in
        let total = total +. st.reward in
        if st.terminated || st.truncated then (
          if !frames && episode=0 then save_frame (label^"-final.pgm") (render ());
          total)
        else go st.obs total (t+1) in
      go (env.reset ()) 0. 0) in
    let mean = Array.fold_left (+.) 0. scores /. float_of_int n in
    Printf.printf "%s evaluation (%d episodes): %.2f\n%!" label n mean;
    mean
  in
  let initial = evaluate "before" in
  let started = Unix.gettimeofday () in
(* book: training begin *)
  let raw = make (Random.State.bits rngs.env) in
  let env = { raw with Ndenv.step = (fun action ->
    let st = raw.step action in { st with reward = st.reward *. 0.01 }) } in
  let curve = Dqn.S.train ~config ~steps:!steps ~rngs ~network env in
(* book: training end *)
  let seconds = Unix.gettimeofday () -. started in
  let final = evaluate "after" in
  if !frames then N.Graph.save_weights network "vizdoom-weights.bin";
  let oc = open_out "vizdoom-dqn.csv" in
  output_string oc "episode,return\n";
  Array.iteri (fun i r -> assert (Float.is_finite r);
    Printf.fprintf oc "%d,%.9g\n" (i+1) (r /. 0.01)) curve;
  close_out oc;
  Printf.printf "seed=%d steps=%d episodes=%d seconds=%.2f initial=%.2f final=%.2f\n%!"
    seed !steps (Array.length curve) seconds initial final
