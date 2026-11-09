open Owl
open Mrcnn
module N=Dense.Ndarray.S
(* book: run-code begin *)
let run ~weights ~size ~output source =
  Configuration.set_image_size size;
  let started=Unix.gettimeofday () in
  let detect=Model.detect ~weights () in
  let setup_seconds=Unix.gettimeofday ()-.started in
  let started=Unix.gettimeofday () in
  let Model.({rois;class_ids;scores;masks} as results)=detect source in
  let inference_seconds=Unix.gettimeofday ()-.started in
  let image=Image.img_to_ndarray source in
  Visualise.display_masks image rois masks class_ids;
  let rendered=Image.img_of_ndarray image in
  Visualise.display_labels rendered rois class_ids scores;
  Image.save output Images.Png rendered;
  Visualise.print_results class_ids rois scores;
  results,setup_seconds,inference_seconds
(* book: run-code end *)
