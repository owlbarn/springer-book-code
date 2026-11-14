(* Freeze each trained policy; compare gradients with/without its fitted
   baseline on the SAME independent held-out episodes. No optimizer steps. *)
open Owl_rllib
module N = Neural.Make (Owl.Neural.S.Graph)
open N
open Algodiff

let variance policy critic seed =
  let rng = Random.State.make [| seed; 91 |] in
  let env = Cartpole.make ~rng:(Random.State.make [| seed; 92 |]) () in
  let weights = Graph.mkpri policy in
  let gradient os acts targets =
    let logits, _ = Graph.forward policy (batch os) in
    let logp = Distribution.Categorical.log_prob (Distribution.Categorical.of_output logits) (one_hot 2 acts) in
    let _, gs = Graph.backward policy Maths.(neg (sum' (logp * column targets))) in
    let flat = Array.to_list gs |> List.concat_map Array.to_list
      |> List.concat_map (fun g -> unpack_arr g |> values |> Array.to_list) |> Array.of_list in
    Graph.update policy weights;
    flat
  in
  let raw = ref [] and adjusted = ref [] and squared_error = ref 0. and count = ref 0 in
  for _ = 1 to 64 do
    let rec collect obs os acts rs =
      let a = Reinforce.S.act ~rng policy obs in
      let st = env.step a in
      let os = obs :: os and acts = a :: acts and rs = st.reward :: rs in
      if st.terminated || st.truncated then os, acts, rs else collect st.obs os acts rs
    in
    let os, acts, rs = collect (env.reset ()) [] [] [] in
    let rev xs = Array.of_list (List.rev xs) in
    let os = rev os and acts = rev acts in
    let returns = Buffer.returns ~gamma:0.99 (rev rs) in
    let baseline = Graph.run (batch os) critic |> unpack_arr |> values in
    let advantages = Array.map2 ( -. ) returns baseline in
    Array.iter (fun x -> squared_error := !squared_error +. x *. x; incr count) advantages;
    let discount xs = Array.mapi (fun t x -> 0.99 ** float_of_int t *. x) xs in
    raw := gradient os acts (discount returns) :: !raw;
    adjusted := gradient os acts (discount advantages) :: !adjusted
  done;
  let trace samples =
    let n = float_of_int (List.length samples) in
    let mean = Array.make (Array.length (List.hd samples)) 0. in
    List.iter (fun xs -> Array.iteri (fun i x -> mean.(i) <- mean.(i) +. x /. n) xs) samples;
    List.fold_left (fun acc xs -> acc +. Array.fold_left ( +. ) 0.
      (Array.mapi (fun i x -> (x -. mean.(i)) ** 2.) xs)) 0. samples /. (n -. 1.)
  in
  trace !raw, trace !adjusted, !squared_error /. float_of_int !count

let () =
  let rngs = Rng.create 0 in
  let policy = N.mlp ~seed:rngs.network_seed ~shape:[|4|] ~hidden:[64] ~outputs:2 in
  let critic = N.mlp ~seed:(rngs.network_seed+1) ~shape:[|4|] ~hidden:[64] ~outputs:1 in
  ignore (Reinforce.S.train ~baseline:critic ~episodes:1000 ~rng:rngs.action
    ~policy (Cartpole.make ~rng:rngs.env ()));
(* book: measurement begin *)
  let raw, adjusted, mse = variance policy critic 0 in
  assert (Float.is_finite raw && raw > 0. && Float.is_finite adjusted);
  Printf.printf "Frozen policy, 64 shared held-out trajectories:\n";
  Printf.printf "Gradient covariance trace: raw %.6g; baseline %.6g\n" raw adjusted;
  Printf.printf "Variance ratio %.3f; critic MSE %.2f\n" (adjusted /. raw) mse
(* book: measurement end *)
