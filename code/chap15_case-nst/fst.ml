open Owl
open Neural.S
open Neural.S.Graph
open Neural.S.Algodiff
module N = Dense.Ndarray.S


(** Network Structure *)

(* book: fst-layers begin *)
let conv2d_layer ?(relu=true) kernel stride nn  =
  let result = 
    conv2d ~padding:SAME kernel stride nn
    |> normalisation ~decay:0. ~training:true ~axis:3
  in
  match relu with
  | true -> (result |> activation Activation.Relu)
  | _    -> result

let conv2d_trans_layer kernel stride nn = 
  transpose_conv2d ~padding:SAME kernel stride nn
  |> normalisation ~decay:0. ~training:true ~axis:3
  |> activation Activation.Relu
(* book: fst-layers end *)

(* book: residual begin *)
let residual_block wh nn = 
  let tmp = conv2d_layer [|wh; wh; 128; 128|] [|1;1|] nn
    |> conv2d_layer ~relu:false [|wh; wh; 128; 128|] [|1;1|]
  in 
  add [|nn; tmp|]
(* book: residual end *)

(* perfectly balanced -- like everything should be. *)
(* book: fst-network begin *)
let make_network h w = 
  input [|h;w;3|]
  |> conv2d_layer [|9;9;3;32|] [|1;1|]
  |> conv2d_layer [|3;3;32;64|] [|2;2|]
  |> conv2d_layer [|3;3;64;128|] [|2;2|]
  |> residual_block 3
  |> residual_block 3
  |> residual_block 3
  |> residual_block 3
  |> residual_block 3
  |> conv2d_trans_layer [|3;3;128;64|] [|2;2|]
  |> conv2d_trans_layer [|3;3;64;32|] [|2;2|]
  |> conv2d_layer ~relu:false [|9;9;32;3|] [|1;1|]
  |> lambda (fun x -> Maths.((tanh x) * (F 150.) + (F 127.5)))
  |> get_network
(* book: fst-network end *)

(* Styles *)

let styles = [|"udnie"; "wave"; "rain_princess"; "la_muse"; "scream"; "wreck"|] 

let make_style_htb () = 
  let h = Hashtbl.create 10 in
  for i = 0 to (Array.length styles - 1) do
    (* weight file: e.g. fst_udnie.weight *)
    Hashtbl.add h i ("fst_" ^ styles.(i) ^ ".weight")
  done;
  h

let style_htb = make_style_htb ()  

let list_styles () = 
  let s = ref "" in
  for i = 0 to (Array.length styles - 1) do
    s := !s ^ Printf.sprintf "Style [%d] :\t %s\n" i styles.(i)
  done; 
  let info = Printf.sprintf "Here are the usable styles:\n%s" !s in
  print_endline info

let weight_directory = Filename.concat (Sys.getenv "HOME") ".owl/dataset/style"
let infer ?(weights=weight_directory) ~style pixels =
  if style < 0 || style >= Array.length styles then invalid_arg "Style index must be 0–5";
  let shape = N.shape pixels in
  if Array.length shape <> 4 || shape.(0) <> 1 || shape.(3) <> 3 then
    invalid_arg "FST expects a single RGB image";
  let nn = make_network shape.(1) shape.(2) in
  Graph.init nn;
  Portable_weights.load_portable_weights nn (Filename.concat weights ("fst-" ^ styles.(style)));
  let result = Graph.model nn pixels in
  (* SAME striding rounds up; remove only the added bottom/right border. *)
  N.get_slice [[];[0;shape.(1)-1];[0;shape.(2)-1];[]] result
let run ?(weights=weight_directory) ?(style=0) content_img output_img =
  let pixels = Style_io.load content_img in
  let result = infer ~weights ~style pixels in
  Style_io.save result output_img
