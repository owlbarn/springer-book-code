#use "topfind";;
Topfind.log := (fun _ -> ());;
#use "book-dependencies.ml";;
open Owl;;
open Mrcnn;;
open CGraph;;
open Graph;;
open AD;;
Owl_stats_prng.init 85;;
#print_depth 2;;
#print_length 20;;

(* book: inputs begin *)
Configuration.set_image_size 128;;
let anchors=Image.get_anchors (Configuration.get_image_shape ());;
let network=Model.mrcnn (Dense.Ndarray.S.shape anchors).(0);;
assert (network.size=464);;
let shape name=Neuron.get_out_shape (get_node network name).neuron;;
(* book: inputs end *)

(* book: feature-pyramid begin *)
assert (shape "fpn_p2"=[|32;32;256|]);;
assert (shape "fpn_p6"=[|2;2;256|]);;
(* book: feature-pyramid end *)

(* book: rpn begin *)
assert (shape "rpn_class"=[|4092;2|]);;
assert (shape "rpn_bbox"=[|4092;4|]);;
(* book: rpn end *)

(* book: proposals begin *)
assert (shape "ROI"=[|1000;4|]);;
(* book: proposals end *)

(* book: detections begin *)
assert (shape "mrcnn_detection"=[|100;6|]);;
(* book: detections end *)

(* book: masks begin *)
let mask_shape=shape "mrcnn_mask";;
assert (mask_shape=[|28;28;81|]);;
assert (Neuron.get_out_shape (get_outputs network).(1).neuron=[|100;28;28;81|]);;
(* book: masks end *)

(* book: outputs begin *)
assert (Array.length (get_outputs network)=2);;
(* book: outputs end *)

(* book: run-code begin *)
let run = Book_mrcnn.run;;
(* book: run-code end *)

