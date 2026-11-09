open Owl
open Owl_types
open Neural.S
open Neural.S.Graph

(* book: conv2d-bn begin *)
let conv2d_bn ?(padding=SAME) kernel stride nn =
  conv2d ~padding kernel stride nn
  |> normalisation ~training:false ~axis:3
  |> activation Activation.Relu
(* book: conv2d-bn end *)

(* book: mix-typ1 begin *)
let mix_typ1 in_shape bp_size nn =
  let branch1x1 = conv2d_bn [|1;1;in_shape;64|] [|1;1|] nn in
  let branch5x5 = nn
    |> conv2d_bn [|1;1;in_shape;48|] [|1;1|]
    |> conv2d_bn [|5;5;48;64|] [|1;1|]
  in
  let branch3x3dbl = nn
    |> conv2d_bn [|1;1;in_shape;64|] [|1;1|]
    |> conv2d_bn [|3;3;64;96|]  [|1;1|]
    |> conv2d_bn [|3;3;96;96|]  [|1;1|]
  in
  let branch_pool = nn
    |> avg_pool2d [|3;3|] [|1;1|]
    |> conv2d_bn [|1;1;in_shape; bp_size |] [|1;1|]
  in
  concatenate 3 [|branch1x1; branch5x5; branch3x3dbl; branch_pool|]
(* book: mix-typ1 end *)

(* book: mix-typ3 begin *)
let mix_typ3 nn =
  let branch3x3 = conv2d_bn [|3;3;288;384|] [|2;2|] ~padding:VALID nn in
  let branch3x3dbl = nn
    |> conv2d_bn [|1;1;288;64|] [|1;1|]
    |> conv2d_bn [|3;3;64;96|] [|1;1|]
    |> conv2d_bn [|3;3;96;96|] [|2;2|] ~padding:VALID
  in
  let branch_pool = max_pool2d [|3;3|] [|2;2|] ~padding:VALID nn in
  concatenate 3 [|branch3x3; branch3x3dbl; branch_pool|]
(* book: mix-typ3 end *)

(* book: mix-typ4 begin *)
let mix_typ4 size nn =
  let branch1x1 = conv2d_bn [|1;1;768;192|] [|1;1|] nn in
  let branch7x7 = nn
    |> conv2d_bn [|1;1;768;size|] [|1;1|]
    |> conv2d_bn [|1;7;size;size|] [|1;1|]
    |> conv2d_bn [|7;1;size;192|] [|1;1|]
  in
  let branch7x7dbl = nn
    |> conv2d_bn [|1;1;768;size|] [|1;1|]
    |> conv2d_bn [|7;1;size;size|] [|1;1|]
    |> conv2d_bn [|1;7;size;size|] [|1;1|]
    |> conv2d_bn [|7;1;size;size|] [|1;1|]
    |> conv2d_bn [|1;7;size;192|] [|1;1|]
  in
  let branch_pool = nn
    |> avg_pool2d [|3;3|] [|1;1|] (* padding = SAME *)
    |> conv2d_bn [|1;1; 768; 192|] [|1;1|]
  in
  concatenate 3 [|branch1x1; branch7x7; branch7x7dbl; branch_pool|]
(* book: mix-typ4 end *)

(* book: mix-typ8 begin *)
let mix_typ8 nn =
  let branch3x3 = nn
    |> conv2d_bn [|1;1;768;192|] [|1;1|]
    |> conv2d_bn [|3;3;192;320|] [|2;2|] ~padding:VALID
  in
  let branch7x7x3 = nn
    |> conv2d_bn [|1;1;768;192|] [|1;1|]
    |> conv2d_bn [|1;7;192;192|] [|1;1|]
    |> conv2d_bn [|7;1;192;192|] [|1;1|]
    |> conv2d_bn [|3;3;192;192|] [|2;2|] ~padding:VALID
  in
  let branch_pool = max_pool2d [|3;3|] [|2;2|] ~padding:VALID nn in
  concatenate 3 [|branch3x3; branch7x7x3; branch_pool|]
(* book: mix-typ8 end *)

(* book: mix-typ9 begin *)
let mix_typ9 input nn =
  let branch1x1 = conv2d_bn [|1;1;input;320|] [|1;1|] nn in
  let branch3x3 = conv2d_bn [|1;1;input;384|] [|1;1|] nn in
  let branch3x3_1 = branch3x3 |> conv2d_bn [|1;3;384;384|] [|1;1|] in
  let branch3x3_2 = branch3x3 |> conv2d_bn [|3;1;384;384|] [|1;1|] in
  let branch3x3 = concatenate 3 [| branch3x3_1; branch3x3_2 |] in
  let branch3x3dbl = nn |> conv2d_bn [|1;1;input;448|] [|1;1|] |> conv2d_bn [|3;3;448;384|] [|1;1|] in
  let branch3x3dbl_1 = branch3x3dbl |> conv2d_bn [|1;3;384;384|] [|1;1|]  in
  let branch3x3dbl_2 = branch3x3dbl |> conv2d_bn [|3;1;384;384|] [|1;1|]  in
  let branch3x3dbl = concatenate 3 [|branch3x3dbl_1; branch3x3dbl_2|] in
  let branch_pool = nn |> avg_pool2d [|3;3|] [|1;1|] |> conv2d_bn [|1;1;input;192|] [|1;1|] in
  concatenate 3 [|branch1x1; branch3x3; branch3x3dbl; branch_pool|]
(* book: mix-typ9 end *)

(* book: make-network begin *)
let make_network img_size =
  input [|img_size;img_size;3|]
  |> conv2d_bn [|3;3;3;32|] [|2;2|] ~padding:VALID
  |> conv2d_bn [|3;3;32;32|] [|1;1|] ~padding:VALID
  |> conv2d_bn [|3;3;32;64|] [|1;1|]
  |> max_pool2d [|3;3|] [|2;2|] ~padding:VALID
  |> conv2d_bn [|1;1;64;80|] [|1;1|] ~padding:VALID
  |> conv2d_bn [|3;3;80;192|] [|1;1|] ~padding:VALID
  |> max_pool2d [|3;3|] [|2;2|] ~padding:VALID
  |> mix_typ1 192 32 |> mix_typ1 256 64 |> mix_typ1 288 64
  |> mix_typ3
  |> mix_typ4 128 |> mix_typ4 160 |> mix_typ4 160 |> mix_typ4 192
  |> mix_typ8
  |> mix_typ9 1280 |> mix_typ9 2048
  |> global_avg_pool2d
  |> linear 1000 ~act_typ:Activation.(Softmax 1)
  |> get_network
(* book: make-network end *)

(* book: update-weights begin *)
let update_node_weights layers (node:Graph.node) =
  let arrays=Hashtbl.find layers node.name in
  Neuron.load_weights node.neuron (Array.map Algodiff.pack_arr arrays)
(* book: update-weights end *)

(* book: portable-weights begin *)
let load_portable_weights (network:Graph.network) directory =
  let table=Hashtbl.create network.size in
  Owl_io.iteri_lines_of_file (fun _ line ->
    match String.split_on_char '\t' line with
    | name::files ->
      if Hashtbl.mem table name then failwith "Duplicate portable node";
      Hashtbl.add table name (Array.of_list files)
    | [] -> failwith "Empty portable node") (Filename.concat directory "weights.tsv");
  if Hashtbl.length table<>network.size then failwith "Weight/network node count differs";
  Array.iter (fun node ->
    let files=Hashtbl.find table node.name in
    let arrays=Array.map (fun file -> Dense.Ndarray.S.load_npy (Filename.concat directory file)) files in
    let layers=Hashtbl.create 1 in Hashtbl.add layers node.name arrays;
    update_node_weights layers node;
    let loaded=Neuron.save_weights node.neuron in
    if Array.length loaded<>Array.length arrays then failwith "Weight count differs";
    Array.iteri (fun i value ->
      let actual=Algodiff.unpack_arr value in
      if Dense.Ndarray.S.shape actual<>Dense.Ndarray.S.shape arrays.(i) ||
         Dense.Ndarray.S.to_array actual<>Dense.Ndarray.S.to_array arrays.(i)
      then failwith "Portable parameter changed during loading") loaded) network.topo
(* book: portable-weights end *)

let load_network weights =
  let network=make_network 299 in
  load_portable_weights network weights;
  network

(* book: infer begin *)
let infer_ppm ~weights image =
  let network=load_network weights in
  let pixels=ImageUtils.(load_ppm image |> extend_dim |> normalise) in
  if Dense.Ndarray.S.shape pixels <> [|1;299;299;3|] then invalid_arg "Expected 299 x 299 RGB image";
  Graph.model network pixels

let infer ~weights image =
  let temporary=Filename.temp_file "book-inception-" ".ppm" in
  Fun.protect ~finally:(fun ()->Sys.remove temporary) (fun ()->
    let pid=Unix.create_process "convert" [|"convert";image;"-resize";"299x299!";"-depth";"8";temporary|]
      Unix.stdin Unix.stdout Unix.stderr in
    let _,status=Unix.waitpid [] pid in
    if status<>Unix.WEXITED 0 then failwith "Image conversion failed";
    infer_ppm ~weights temporary)
(* book: infer end *)

let json_string text =
  let b=Buffer.create (String.length text+2) in
  Buffer.add_char b '"';
  String.iter (fun ch -> match ch with
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | ch when Char.code ch<32 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code ch))
    | ch -> Buffer.add_char b ch) text;
  Buffer.add_char b '"';Buffer.contents b

let to_tuples ?(top=5) ~labels predictions =
  if top<0 || Dense.Ndarray.S.shape predictions<>[|1;1000|] then invalid_arg "to_tuples";
  let dictionary:(int,string) Hashtbl.t=Owl_io.marshal_from_file labels in
  Dense.Matrix.S.top predictions (min top 1000)
  |> Array.map (fun position ->
    Hashtbl.find dictionary position.(1),Dense.Matrix.S.get predictions position.(0) position.(1))

let to_json ?(top=5) ~labels predictions =
  to_tuples ~top ~labels predictions
  |> Array.map (fun (name,probability)->
    "{\"class\":"^json_string name^",\"prop\":"^Printf.sprintf "%.9g" probability^"}")
  |> Array.to_list |> String.concat "," |> fun body -> "["^body^"]"
