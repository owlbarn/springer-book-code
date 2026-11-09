#use "topfind";;
#require "owl";;
#use "book-dependencies.ml";;
open Owl;;
open Book_style;;
open Neural.S;;
open Graph;;
open Algodiff;;
module N = Dense.Ndarray.S;;
Owl_stats_prng.init 95;;
let nn = Neural_transfer.load_network 32 32;;
let pixels = N.ones [|1;32;32;3|];;

(* book: content-target begin *)
let fill_content_targets = Neural_transfer.fill_content_targets;;
let target = fill_content_targets ~idx:2 (Arr pixels) nn;;
assert (Algodiff.shape target = [|1;32;32;64|]);;
(* book: content-target end *)

(* book: run-prefix begin *)
let run' = Neural_transfer.run';;
assert (Algodiff.shape (run' (Array.sub nn.topo 0 3) (Arr pixels)) = [|1;32;32;64|]);;
(* book: run-prefix end *)

(* book: noise begin *)
let noise_image = Neural_transfer.noise_image;;
assert (N.shape (noise_image pixels) = N.shape pixels);;
(* book: noise end *)

(* book: content-loss begin *)
let c_loss = Neural_transfer.c_loss;;
assert (unpack_flt (c_loss target target) = 0.);;
(* book: content-loss end *)

(* book: content-objective begin *)
let content_objective = Neural_transfer.content_objective;;
let g = content_objective ~idx:23 nn pixels;;
assert (unpack_flt (g (Arr pixels)) = 0.);;
(* book: content-objective end *)

(* book: resume begin *)
let minimise_with_checkpoints = Neural_transfer.minimise_with_checkpoints;;
let params = Neural_transfer.adam_params ~ckpt:4 ~learning_rate:0.1 5.;;
let state, _ = minimise_with_checkpoints params (fun x -> Maths.(sum' (x * x))) (N.ones [|2|]);;
assert (state.current_batch = 6);;
(* book: resume end *)

(* book: adam begin *)
let adam_params = Neural_transfer.adam_params;;
let params = adam_params ~ckpt:50 ~learning_rate:10. 250.;;
(* book: adam end *)

(* book: gram begin *)
let gram = Neural_transfer.gram;;
let matrix = gram (Arr (N.of_array [|1.;2.;3.;4.;5.;6.;7.;8.|] [|1;2;2;2|]));;
assert (N.to_array (unpack_arr matrix) = [|10.5;12.5;12.5;15.|]);;
(* book: gram end *)

(* book: style-loss begin *)
let s_loss = Neural_transfer.s_loss;;
assert (unpack_flt (s_loss matrix matrix) = 0.);;
(* book: style-loss end *)

(* book: style-objective begin *)
let style_objective = Neural_transfer.style_objective;;
let h = style_objective ~indices:[|2;7;12;21;30|] nn pixels;;
assert (unpack_flt (h (Arr pixels)) = 0.);;
(* book: style-objective end *)

(* book: run-nst begin *)
let run_nst = Book_examples.run_nst;;
(* book: run-nst end *)

(* book: fst-layers begin *)
let conv2d_layer = Fst.conv2d_layer;;
let conv2d_trans_layer = Fst.conv2d_trans_layer;;
(* book: fst-layers end *)

(* book: residual begin *)
let residual_block = Fst.residual_block;;
(* book: residual end *)

(* book: fst-network begin *)
let make_network = Fst.make_network;;
let network = make_network 25 31;;
assert (network.size = 49);;
(* book: fst-network end *)

(* book: run-fst begin *)
let run_fst = Book_examples.run_fst;;
Fst.list_styles ();;
Style_io.save (Fst.infer ~style:1 (N.ones [|1;25;31;3|])) "fst-interactive.png";;
assert (N.shape (Style_io.load "fst-interactive.png") = [|1;25;31;3|]);;
(* book: run-fst end *)
