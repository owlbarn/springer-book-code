open Owl
open Neural.S
open Neural.S.Graph
open Neural.S.Algodiff
module N = Dense.Ndarray.S

#zoo "86a1748bbc898f2e42538839edba00e1" (* ImageUtils  *)

(** Network Structure *)

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

let residual_block wh nn = 
  let tmp = conv2d_layer [|wh; wh; 128; 128|] [|1;1|] nn
    |> conv2d_layer ~relu:false [|wh; wh; 128; 128|] [|1;1|]
  in 
  add [|nn; tmp|]

(* perfectly balanced -- like everything should be. *)
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

(* Image helper functions *)
let _convert img_name = 
  let base = Filename.basename img_name in 
  let prefix = Filename.remove_extension base in
  let temp_img = Filename.temp_file prefix ".ppm"in
  temp_img

let convert_img_to_ppm w h img_name = 
  let temp_img = _convert img_name in
  let _ = Sys.command ("convert -resize " ^ (string_of_int w) ^ 
    "x" ^ (string_of_int h) ^"\\! " ^ 
    img_name ^ " " ^ temp_img) in
  temp_img

let convert_arr_to_img d3array output_name = 
  let temp_img = _convert output_name in
  let output = d3array in
  ImageUtils.save_ppm_from_arr output temp_img;
  let _ = Sys.command ("convert " ^ temp_img ^ " " ^ output_name) in
  ()

let get_img_shape img_name = 
  let temp_img = _convert img_name in
  let _ = Sys.command ("convert " ^ img_name ^ " " ^ temp_img) in
  let _, w, h, _ = ImageUtils._read_ppm temp_img in
  w, h

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

(* FST service function *)
let run ?(style=0) content_img output_img =
  
  let w, h = get_img_shape content_img in
  let content_img = convert_img_to_ppm w h content_img in
  let content_img = ImageUtils.(load_ppm content_img |> extend_dim) in

  let nn = make_network h w in
  Graph.init nn;

  let style_file = 
    try Hashtbl.find style_htb style 
    with Not_found -> failwith "style does not exist; try to run `list_styles ()`"
  in
  Graph.load_weights nn (Owl_zoo_path.extend_zoo_path style_file);
  let result = Graph.model nn content_img in

  convert_arr_to_img result output_img
