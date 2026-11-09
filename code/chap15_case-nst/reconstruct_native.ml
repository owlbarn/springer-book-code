open Owl
open Book_style
module N = Dense.Ndarray.S
module A = Neural.S.Algodiff
let () =
  Owl_stats_prng.init 95;
  let weights = Sys.argv.(1) and inputs = Sys.argv.(2) in
  let content = Style_io.load ~size:(224,224) (Filename.concat inputs "hojo.png") |> ImageUtils.preprocess in
  let style = Style_io.load ~size:(224,224) (Filename.concat inputs "hokusai.png") |> ImageUtils.preprocess in
  let nn = Neural_transfer.load_network ~weights:(Filename.concat weights "vgg19") 224 224 in
  let layers = [|2;7;12;21;30|] in
  let records = ref [] in
  let reconstruct kind index g =
    let noise = N.(gaussian [|1;224;224;3|] |> scalar_mul 0.256) in
    let initial = A.unpack_flt (g (A.Arr noise)) in
    let started = Unix.gettimeofday () in
    let state, result = Neural_transfer.optimise ~ckpt:50 100. g noise in
    let final = A.unpack_flt (g (A.Arr result)) in
    Printf.printf "DEBUG %s %d steps=%d initial=%.17g final=%.17g finite=%b min=%.9g max=%.9g\n%!" kind index (state.current_batch-1) initial final (Array.for_all Float.is_finite (N.to_array result)) (N.min' result) (N.max' result);
    if state.current_batch<>101 || not (Float.is_finite final) || final>=initial || not(Array.for_all Float.is_finite (N.to_array result)) then
      failwith "Reconstruction must execute 100 updates and improve its real objective";
    let filename = Printf.sprintf "%s-%d.png" kind index in
    Style_io.save ~preprocessed:true result filename;
    let line = Printf.sprintf "{\"kind\":\"%s\",\"index\":%d,\"steps\":100,\"initial_loss\":%.17g,\"final_loss\":%.17g,\"seconds\":%.6f}" kind index initial final (Unix.gettimeofday ()-.started) in
    records := !records @ [line];
    Owl_io.write_file "reconstruction-metrics.json" ("[" ^ String.concat "," !records ^ "]\n");
    Printf.printf "RECONSTRUCTION %s %d initial=%.9g final=%.9g seconds=%.3f\n%!" kind index initial final (Unix.gettimeofday ()-.started)
  in
  Array.iter (fun idx -> reconstruct "content" idx (Neural_transfer.content_objective ~idx nn content)) layers;
  Array.iteri (fun n idx -> reconstruct "style" idx (Neural_transfer.style_objective ~indices:(Array.sub layers 0 (n+1)) nn style)) layers;
  print_endline "PASS: five content and five cumulative style reconstructions, each 100 genuine Adam updates"
