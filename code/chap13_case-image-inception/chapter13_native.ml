open Owl
module N=Dense.Ndarray.S
let checks=ref 0
let check name ok=incr checks;if not ok then failwith name
let () =
  Owl_stats_prng.init 84;
  let weights=Sys.argv.(1) and image=Sys.argv.(2) and ppm=Sys.argv.(3) in
  let labels=Filename.concat weights "imagenet1000.dict" in
  let dict:(int,string) Hashtbl.t=Owl_io.marshal_from_file labels in
  check "1000 ImageNet labels" (Hashtbl.length dict=1000);
  let predictions=Inception_v3.infer ~weights image in
  check "inference shape" (N.shape predictions=[|1;1000|]);
  let p=N.to_array predictions in
  check "finite probabilities" (Array.for_all (fun x->Float.is_finite x && x>=0. && x<=1.) p);
  check "probabilities sum to one" (abs_float(Array.fold_left (+.) 0. p-.1.)<1e-5);
  let top=Dense.Matrix.S.top predictions 5 in
  print_endline "Top 5 Predictions:";
  Array.iteri (fun rank pos->
    let label=Hashtbl.find dict pos.(1) in
    Printf.printf "Prediction #%d (%.6f%%) : %s\n%!" rank (p.(pos.(1))*.100.) label) top;
  check "original image recognized as giant panda" (top.(0).(1)=388 && p.(388)>0.8);
  let network=Inception_v3.load_network weights in
  Neural.S.Graph.save_weights network "current-weights.bin";
  let reloaded=Inception_v3.make_network 299 in
  Neural.S.Graph.load_weights reloaded "current-weights.bin";
  let pixels=ImageUtils.(load_ppm ppm |> extend_dim |> normalise) in
  let q=Neural.S.Graph.model reloaded pixels |> N.to_array in
  check "current-format weight save/reload preserves every prediction" (Array.for_all Fun.id (Array.mapi (fun i v->abs_float(v-.p.(i))<1e-6) q));
  let fixture=N.of_array (Array.init 18 float_of_int) [|2;3;3|] in
  ImageUtils.save_image_to_file fixture "rgb-nonsquare.ppm";
  let roundtrip=ImageUtils.load_ppm "rgb-nonsquare.ppm" in
  check "nonsquare width/height" (N.shape roundtrip=[|2;3;3|]);
  check "every RGB pixel roundtrips" (N.to_array roundtrip=N.to_array fixture);
  check "blue is channel two" (N.get roundtrip [|1;2;2|]=17.);
  check "black normalizes to minus one" (N.min' (ImageUtils.normalise (N.zeros [|1;1;3|]))=(-1.));
  check "white normalizes to plus one" (N.max' (ImageUtils.normalise (N.create [|1;1;3|] 255.))=1.);
  let open Neural.S in
  let open Graph in
  let a=input [|28;28;1|] |> conv2d ~padding:Owl_types.VALID [|7;7;1;1|] [|1;1|] |> get_network in
  let b=input [|28;28;1|] |> conv2d ~padding:Owl_types.VALID [|7;1;1;1|] [|1;1|] |> conv2d ~padding:Owl_types.VALID [|1;7;1;1|] [|1;1|] |> get_network in
  Graph.init a;Graph.init b;
  let input=N.ones [|1;28;28;1|] in
  check "7x7 output shape" (N.shape (Graph.model a input)=[|1;22;22;1|]);
  check "7x1 then 1x7 output shape" (N.shape (Graph.model b input)=[|1;22;22;1|]);
  let parameters network=Array.fold_left (fun sum node ->
    sum+Array.fold_left (fun n value->n+N.numel (Algodiff.unpack_arr value)) 0 (Neuron.save_weights node.neuron)) 0 network.topo in
  check "factored kernels parameter counts including biases" (parameters a=50 && parameters b=16);
  check "empty tuple results" (Inception_v3.to_tuples ~top:0 ~labels predictions=[||]);
  check "empty JSON results" (Inception_v3.to_json ~top:0 ~labels predictions="[]");
  check "oversized result request clamps" (Array.length (Inception_v3.to_tuples ~top:1001 ~labels predictions)=1000);
  check "negative result request rejected" (try ignore(Inception_v3.to_tuples ~top:(-1) ~labels predictions);false with Invalid_argument _->true);
  Owl_io.write_file "native-predictions.json" (Inception_v3.to_json ~top:5 ~labels predictions);
  Dense.Matrix.S.save ~out:"predictions.bin" predictions;
  let out=open_out "native-metrics.json" in
  Printf.fprintf out "{\"checks\":%d,\"top_class\":388,\"panda_probability\":%.17g}\n" !checks p.(388);close_out out;
  Printf.printf "PASS: %d original Inception/image checks\n%!" !checks
