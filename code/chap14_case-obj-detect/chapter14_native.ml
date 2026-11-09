open Owl
open Mrcnn
module N=Dense.Ndarray.S
let checks=ref 0
let check label ok=incr checks;if not ok then failwith label
let near label expected actual tolerance=check label (Float.is_finite actual && abs_float(expected-.actual)<tolerance)
let array_near label expected actual tolerance=
  check label (Array.length expected=Array.length actual && Array.for_all Fun.id (Array.mapi (fun i x->Float.is_finite x && abs_float(expected.(i)-.x)<tolerance) actual))
let geometry source =
  near "self IoU" 1. (Image.intersection_over_union [|0.;0.;2.;2.|] [|0.;0.;2.;2.|]) 1e-6;
  near "self IoU independent of normalized scale" 1. (Image.intersection_over_union [|0.;0.;0.001;0.001|] [|0.;0.;0.001;0.001|]) 1e-12;
  near "zero-area union finite" 0. (Image.intersection_over_union [|0.;0.;0.;0.|] [|0.;0.;0.;0.|]) 1e-12;
  near "disjoint IoU" 0. (Image.intersection_over_union [|0.;0.;1.;1.|] [|2.;2.;3.;3.|]) 1e-6;
  near "known overlapping IoU" (1./.7.) (Image.intersection_over_union [|0.;0.;2.;2.|] [|1.;1.;3.;3.|]) 1e-6;
  let boxes=N.of_arrays [|[|0.;0.;10.;10.|];[|0.;0.;10.;10.|];[|20.;20.;30.;30.|]|] in
  let scores=N.of_array [|0.9;0.8;0.7|] [|3|] in
  check "NMS removes duplicate and preserves disjoint box" (Image.non_max_suppression boxes scores 10 0.5=[|0;2|]);
  check "NMS max output" (Image.non_max_suppression boxes scores 1 0.5=[|0|]);
  check "NMS zero output" (Image.non_max_suppression boxes scores 0 0.5=[||]);
  let transformed=Image.apply_box_deltas boxes (N.zeros [|3;4|]) in
  array_near "zero box deltas identity" (N.to_array boxes) (N.to_array transformed) 1e-5;
  let clipped=Image.clip_boxes (N.of_arrays [|[|-2.;-1.;5.;7.|]|]) (N.of_array [|0.;0.;3.;4.|] [|4|]) in
  array_near "clip to image window" [|0.;0.;3.;4.|] (N.to_array clipped) 1e-6;
  let pixels=N.of_arrays [|[|1.;2.;50.;90.|];[|0.;0.;64.;128.|]|] in
  let normalized=Image.norm_boxes pixels [|64;128;3|] in
  let restored=Image.denorm_boxes normalized [|64;128;3|] in
  array_near "pixel-normalized box roundtrip" (N.to_array pixels) (N.to_array restored) 1e-5;
  let image=N.of_array [|0.;1.;2.;3.|] [|2;2;1|] in
  let crop=Image.crop_and_resize image (N.of_array [|0.;0.;1.;1.|] [|1;4|]) [|3;3|] in
  check "crop output shape" (N.shape crop=[|1;3;3;1|]);
  near "bilinear center" 1.5 (N.get crop [|0;1;1;0|]) 1e-6;
  near "bilinear first corner" 0. (N.get crop [|0;0;0;0|]) 1e-6;
  near "bilinear last corner" 3. (N.get crop [|0;2;2;0|]) 1e-6;
  let rgb=N.of_array [|0.;64.;255.;128.;20.;200.|] [|1;2;3|] in
  array_near "preprocess/postprocess RGB roundtrip" (N.to_array rgb) (Image.preprocess rgb |> Image.postprocess |> N.to_array) 2e-5;
  let original=Image.img_to_ndarray source in
  let original_shape=N.shape original in
  let molded,meta,window=Image.mold_inputs source in
  let meta=Image.parse_image_meta (N.expand meta 2) in
  check "metadata original shape" (meta.original_image_shape=original_shape);
  check "metadata molded shape" (meta.image_shape=N.shape molded);
  check "metadata window" (meta.window=window);
  let size=float_of_int (Configuration.get_image_size ()) in
  near "metadata scale" (min (size/.float_of_int original_shape.(0)) (size/.float_of_int original_shape.(1))) meta.scale 1e-6;
  let boxes,ids,scores,_=Image.unmold_detections (N.zeros [|2;6|]) (N.zeros [|2;2;2;81|]) [|10;20;3|] [|64;64;3|] [|16;0;48;64|] in
  check "empty detections preserve zero count" (N.shape boxes=[|0;4|] && ids=[||] && N.numel scores=0)
let () =
  Random.init 85;Owl_stats_prng.init 85;
  geometry Sys.argv.(2);
  Configuration.set_image_size 128;
  let anchors=Image.get_anchors (Configuration.get_image_shape ()) in
  let count=(N.shape anchors).(0) in
  check "five-level anchor count" (count=4092 && N.shape anchors=[|4092;4|]);
  check "finite anchor values" (Array.for_all Float.is_finite (N.to_array anchors));
  let graph=Model.mrcnn count in
  check "graph topology count" (Array.length graph.topo=graph.size && graph.size=464);
  let tensors,parameters=PortableWeights.load graph Sys.argv.(1) in
  check "all shared parameter tensors" (tensors=714 && parameters=68916160);
  let results,setup,inference=Book_mrcnn.run ~weights:Sys.argv.(1) ~size:1024 ~output:"london.png" Sys.argv.(2) in
  let source=Image.img_to_ndarray Sys.argv.(2) in
  let shape=N.shape source and count=Array.length results.class_ids in
  check "original book input crop" (shape=[|916;2048;3|]);
  check "actual detections nonempty" (count>0);
  check "detection array shapes" (N.shape results.rois=[|count;4|] && N.shape results.scores=[|count|]);
  check "COCO label bounds" (Array.for_all (fun x->x>0 && x<Configuration.num_classes) results.class_ids);
  check "expected London bus and person" (Array.mem 6 results.class_ids && Array.mem 1 results.class_ids);
  check "finite bounded scores" (Array.for_all (fun x->Float.is_finite x && x>=0. && x<=1.) (N.to_array results.scores));
  let mask_pixels=ref 0 in
  for i=0 to count-1 do
    let box=Array.map int_of_float (N.get_slice [[i];[]] results.rois |> N.to_array) in
    check "box within original image" (box.(0)>=0 && box.(1)>=0 && box.(2)<=shape.(0) && box.(3)<=shape.(1) && box.(2)>box.(0) && box.(3)>box.(1));
    let mask,y1,x1,y2,x2=results.masks i in
    check "unmolded mask shape/bounds" (N.shape mask=[|y2-y1;x2-x1|] && [|y1;x1;y2;x2|]=box);
    check "binary finite mask" (Array.for_all (fun x->x=0. || x=1.) (N.to_array mask));
    mask_pixels:= !mask_pixels+int_of_float (N.sum' mask)
  done;
  check "nonempty predicted mask content" (!mask_pixels>0);
  let metrics=open_out "native-metrics.json" in
  Printf.fprintf metrics "{\"checks\":%d,\"instances\":%d,\"mask_pixels\":%d,\"model_size\":1024,\"setup_seconds\":%.9g,\"inference_seconds\":%.9g,\"loaded_tensors\":%d,\"loaded_parameters\":%d}\n" !checks count !mask_pixels setup inference tensors parameters;close_out metrics;
  Printf.printf "PASS: %d Mask R-CNN geometry, parameter and real London checks; %d instances\n%!" !checks count
