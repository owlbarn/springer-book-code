(* Record the first before/after evaluation episode, without training. *)
open Owl_rllib
open Owl_rllib_bridges
module N = Neural.Make (Owl.Neural.S.Graph)
let save_frame name (w,h,pixels) =
  let oc = open_out_bin name in
  Printf.fprintf oc "P5\n%d %d\n255\n" w h;
  output_bytes oc pixels; close_out oc
let () =
  let rngs = Rng.create 0 in
  let network = N.cnn ~seed:rngs.network_seed ~shape:[|30;40;4|]
      ~convs:[4,2,8;3,2,16] ~hidden:[64] ~outputs:3 in
  let record label =
    let raw, render = Doom.S.make_with_renderer ~downsample:4 ~seed:1000 "basic" in
    let env = Frame_stack.S.wrap ~frames:4 raw in
    let obs = env.reset () in
    save_frame (label^"-0000.pgm") (render ());
    let oc = open_out (label^"-actions.csv") in
    output_string oc "action,reward,terminated,truncated\n";
    let rec go obs total count =
      let action = Dp.argmax (N.predict network obs) in
      let st = env.step action in
      Printf.fprintf oc "%d,%.17g,%b,%b\n" action st.reward st.terminated st.truncated;
      if st.terminated || st.truncated then (
        save_frame (label^"-final.pgm") (render ());
        Printf.printf "%s: actions=%d return=%.2f terminated=%b truncated=%b\n%!"
          label (count+1) (total+.st.reward) st.terminated st.truncated)
      else go st.obs (total+.st.reward) (count+1)
    in
    go obs 0. 0; close_out oc
  in
  record "before";
  N.Graph.load_weights network Sys.argv.(1);
  record "after"
