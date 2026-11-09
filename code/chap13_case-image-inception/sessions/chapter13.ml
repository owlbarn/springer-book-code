#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "unix";;
open Owl;;
open Owl_types;;
#use "book-dependencies.ml";;
open Neural.S;;
open Neural.S.Graph;;
module N = Dense.Ndarray.S;;
Owl_stats_prng.init 84;;
let data_directory = Filename.concat (Sys.getenv "HOME") ".owl/dataset/inception";;
let labels_file = Filename.concat data_directory "imagenet1000.dict";;

(* book: conv2d-bn begin *)
let conv2d_bn = Inception_v3.conv2d_bn;;
(* book: conv2d-bn end *)

(* book: mix-typ1 begin *)
let mix_typ1 = Inception_v3.mix_typ1;;
(* book: mix-typ1 end *)

(* book: mix-typ4 begin *)
let mix_typ4 = Inception_v3.mix_typ4;;
(* book: mix-typ4 end *)

(* book: mix-typ9 begin *)
let mix_typ9 = Inception_v3.mix_typ9;;
(* book: mix-typ9 end *)

(* book: mix-typ3 begin *)
let mix_typ3 = Inception_v3.mix_typ3;;
(* book: mix-typ3 end *)

(* book: mix-typ8 begin *)
let mix_typ8 = Inception_v3.mix_typ8;;
(* book: mix-typ8 end *)

(* book: make-network begin *)
let make_network = Inception_v3.make_network;;
(* book: make-network end *)

(* book: factorisation begin *)
open Neural.S
open Neural.S.Graph

let network_01 =
  input [|28;28;1|]
  |> conv2d ~padding:VALID [|7;7;1;1|] [|1;1|]
  |> get_network

let network_02 =
  input [|28;28;1|]
  |> conv2d ~padding:VALID [|7;1;1;1|] [|1;1|]
  |> conv2d ~padding:VALID [|1;7;1;1|] [|1;1|]
  |> get_network;;
(* book: factorisation end *)

(* book: update-weights begin *)
let update_node_weights = Inception_v3.update_node_weights;;
(* book: update-weights end *)

(* book: blue-channel begin *)
let h=2 and w=3;;
let img=N.of_array (Array.init 18 float_of_int) [|h;w;3|];;
let r=N.get_slice [[];[];[0]] img |> fun x->N.reshape x [|h;w|];;
let g=N.get_slice [[];[];[1]] img |> fun x->N.reshape x [|h;w|];;
let b=N.get_slice [[];[];[2]] img |> fun x->N.reshape x [|h;w|];;
(* book: blue-channel end *)

(* book: interleave begin *)
let img_mat=Dense.Matrix.S.zeros h (3*w);;
Dense.Matrix.S.set_slice [[];[0;-1;3]] img_mat r;;
Dense.Matrix.S.set_slice [[];[1;-1;3]] img_mat g;;
Dense.Matrix.S.set_slice [[];[2;-1;3]] img_mat b;;
(* book: interleave end *)

(* book: write-pixels begin *)
let img_arr=Dense.Matrix.S.to_arrays img_mat;;
let img_str=Bytes.init (w*h*3) (fun index ->
  let row=index/(3*w) and column=index mod (3*w) in
  Char.chr (int_of_float img_arr.(row).(column)));;
let fp=open_out_bin "book-fixture.ppm";;
Printf.fprintf fp "P6\n%d %d\n255\n" w h;;
output_bytes fp img_str;;
close_out fp;;
(* book: write-pixels end *)

(* book: ppm-header begin *)
let fp=open_in_bin "book-fixture.ppm";;
assert (input_line fp="P6");;
let w_h_line=input_line fp;;
let w,h=Scanf.sscanf w_h_line "%d %d" (fun w h->w,h);;
let maximum=int_of_string (input_line fp);;
assert (maximum=255);;
(* book: ppm-header end *)

(* book: read-pixels begin *)
let pixels=Bytes.create (w*h*3);;
really_input fp pixels 0 (Bytes.length pixels);;
close_in fp;;
(* book: read-pixels end *)

(* book: pixels-matrix begin *)
let imf_o=Array.init h (fun row ->
  Array.init (3*w) (fun column ->
    float_of_int (Char.code (Bytes.get pixels (row*3*w+column)))));;
(* book: pixels-matrix end *)

(* book: matrix-image begin *)
let matrix=Dense.Matrix.S.of_arrays imf_o;;
let recovered=N.reshape matrix [|h;w;3|];;
let r=N.get_slice [[];[];[0]] recovered;;
let g=N.get_slice [[];[];[1]] recovered;;
let b=N.get_slice [[];[];[2]] recovered;;
assert (N.to_array recovered=N.to_array img);;
(* book: matrix-image end *)

(* book: normalise begin *)
let normalise img =
  let img=N.div_scalar img 255. in
  let img=N.sub_scalar img 0.5 in
  N.mul_scalar img 2.;;
(* book: normalise end *)

(* book: infer begin *)
let labels=Inception_v3.infer ~weights:data_directory "panda.png";;
let top=5;;
let labels_json=Inception_v3.to_json ~top ~labels:labels_file labels;;
let labels_tuples=Inception_v3.to_tuples ~top ~labels:labels_file labels;;
Printf.printf "Top %d Predictions:\n" top;;
Array.iteri (fun i (name,probability)->
  Printf.printf "Prediction #%d (%.6f%%) : %s\n" i (probability*.100.) name) labels_tuples;;
(* book: infer end *)

(* book: checks begin *)
assert (N.shape labels=[|1;1000|]);;
assert (fst labels_tuples.(0)="giant panda, panda, panda bear, coon bear, Ailuropoda melanoleuca");;
assert (Inception_v3.to_tuples ~top:0 ~labels:labels_file labels=[||]);;
assert (Inception_v3.to_json ~top:0 ~labels:labels_file labels="[]");;
assert (N.min' (normalise (N.zeros [|1;1;3|]))=(-1.));;

(* book: checks end *)

