open Owl_rllib
module N = Neural.Make (Owl.Neural.S.Graph)
module A = Owl.Dense.Ndarray.S
let () =
  let rngs = Rng.create 0 in
(* book: continuous begin *)
  let policy = N.mlp ~seed:rngs.network_seed ~shape:[|3|]
      ~hidden:[64;64] ~outputs:2 in
  let critic = N.mlp ~seed:(rngs.network_seed+1) ~shape:[|3|]
      ~hidden:[64;64] ~outputs:1 in
  let distribution = Policy.S.gaussian ~low:[|-2.|] ~high:[|2.|] in
  let env = Array.init 8 (fun _ ->
    let e = Pendulum.make ~rng:(Random.State.make [|Random.State.bits rngs.env|]) () in
    { e with Ndenv.step = (fun a -> let st = e.step a in
        { st with reward = st.reward *. 0.1 }) }) |> Vec_env.create in
  let returns = Ppo.S.train ~updates:100 ~rngs ~policy ~critic ~distribution env in
(* book: continuous end *)
  assert (Array.length returns > 0 && Array.for_all Float.is_finite returns);
  let eval () =
    let e = Pendulum.make ~rng:(Random.State.make [|1000|]) () in
    let scores = Array.init 20 (fun _ ->
      let rec go obs sum =
        let mu = (N.predict policy obs).(0) |> Float.min 2. |> Float.max (-2.) in
        let st = e.step (A.of_array [|mu|] [|1|]) in
        let sum = sum +. st.reward in
        if st.terminated || st.truncated then sum else go st.obs sum in
      go (e.reset ()) 0.) in
    Array.fold_left (+.) 0. scores /. 20. in
  Printf.printf "PPO: %d steps, %d completed episodes; mean-action evaluation %.2f\n"
    (100*8*Ppo.S.default.horizon) (Array.length returns) (eval ())
