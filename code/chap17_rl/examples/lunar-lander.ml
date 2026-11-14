(* PPO on LunarLander. Full figure run: --steps 1000000 --frames *)
open Owl_rllib
open Owl_rllib_bridges
module N = Neural.Make (Owl.Neural.S.Graph)

(* book: configuration begin *)
let total_steps = 16_384  (* Figure: 1_000_000 steps *)
let seed = 0
let config = { Ppo.S.default with
  horizon = 1024; gamma = 0.999; lambda = 0.98;
  entropy_coef = 0.01 }
(* book: configuration end *)

let save_frame filename (width, height, rgb) =
  let oc = open_out_bin filename in
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;
  output_bytes oc rgb;
  close_out oc

let () =
  let steps = ref total_steps and frames = ref false in
  Arg.parse ["--steps", Arg.Set_int steps, "Environment steps";
             "--frames", Arg.Set frames, "Export before/after evaluation frames"]
    (fun _ -> ()) "LunarLander";
  let rngs = Rng.create seed in
(* book: networks begin *)
  let policy = N.mlp ~seed:rngs.network_seed
      ~shape:[|8|] ~hidden:[64; 64] ~outputs:4 in
  let critic = N.mlp ~seed:(rngs.network_seed + 1)
      ~shape:[|8|] ~hidden:[64; 64] ~outputs:1 in
  let make rng = Gym.S.discrete
      ~seed:(Random.State.bits rng) "LunarLander-v2" in
(* book: networks end *)
  let evaluate label =
    (* Identical held-out initial seeds before and after training. *)
    let env = Gym.S.discrete ~seed:1000 "LunarLander-v2" in
    let n = if !frames then 20 else 2 in
    let scores = Array.init n (fun _ ->
      let rec go obs sum =
        let st = env.step (Dp.argmax (N.predict policy obs)) in
        let sum = sum +. st.reward in
        if st.terminated || st.truncated then sum else go st.obs sum in
      go (env.reset ()) 0.) in
    let mean = Array.fold_left (+.) 0. scores /. float_of_int n in
    Printf.printf "%s evaluation (%d episodes): %.2f\n%!" label n mean;
    if !frames then (
      let e, render = Gym.S.discrete_with_renderer ~seed:1000 "LunarLander-v2" in
      let rec go obs t =
        if List.mem t [0; 50; 100; 200; 400] then
          save_frame (Printf.sprintf "%s-%04d.ppm" label t) (render ());
        let st = e.step (Dp.argmax (N.predict policy obs)) in
        if st.terminated || st.truncated then
          save_frame (label ^ "-final.ppm") (render ())
        else go st.obs (t + 1) in
      go (e.reset ()) 0);
    mean
  in
  let initial = evaluate "before" in
  let updates = !steps / (16 * config.horizon) in
  assert (updates > 0);
  let started = Unix.gettimeofday () in
(* book: training begin *)
  let env = Vec_env.create (Array.init 16 (fun _ -> make rngs.env)) in
  let curve = Ppo.S.train ~config ~updates ~rngs ~policy ~critic
      ~distribution:Policy.S.categorical env in
(* book: training end *)
  let seconds = Unix.gettimeofday () -. started in
  let final = evaluate "after" in
  let oc = open_out "lunar-lander.csv" in
  output_string oc "episode,return\n";
  Array.iteri (fun i r -> assert (Float.is_finite r);
    Printf.fprintf oc "%d,%.9g\n" (i+1) r) curve;
  close_out oc;
  Printf.printf "seed=%d steps=%d episodes=%d seconds=%.2f initial=%.2f final=%.2f\n%!"
    seed (updates * 16 * config.horizon) (Array.length curve) seconds initial final
