(* DQN from pixels. Full figure run: --steps 50000 --frames *)
open Owl_rllib
open Owl_rllib_bridges
module N = Neural.Make (Owl.Neural.S.Graph)
(* book: configuration begin *)
let total_steps = 2_048  (* Figure: 50_000 *)
let seed = 0
let config = { Dqn.S.default with
  learning_rate = 0.0001; capacity = 5000; batch_size = 32;
  learning_starts = 512; target_every = 1000;
  epsilon_end = 0.1; exploration_steps = 10_000 }
(* book: configuration end *)
let save_frame filename (width,height,gray) =
  let oc = open_out_bin filename in
  Printf.fprintf oc "P5\n%d %d\n255\n" width height;
  output_bytes oc gray; close_out oc

let () =
  let rngs = Rng.create 0 in
  let network = N.cnn ~seed:rngs.network_seed ~shape:[|30;40;4|]
      ~convs:[4,2,8;3,2,16] ~hidden:[64] ~outputs:3 in
  N.Graph.load_weights network "/results/vizdoom-weights.bin";
  let env = Doom.S.make ~downsample:4 ~seed:1000 "basic" |> Frame_stack.S.wrap ~frames:4 in

  let scores = Array.init 20 (fun episode ->
    let rec go obs sum t =
      let st = env.step (Dp.argmax (N.predict network obs)) in
      let sum = sum +. st.reward in
      if st.terminated || st.truncated then (
        Printf.printf "episode=%d return=%.2f actions=%d terminated=%b truncated=%b\n%!"
          (episode+1) sum (t+1) st.terminated st.truncated; sum)
      else go st.obs sum (t+1) in
    go (env.reset ()) 0. 0) in
  Printf.printf "mean=%.2f\n" (Array.fold_left (+.) 0. scores /. 20.)
