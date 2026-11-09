
open Owl
open Owl_types
open Optimise
open Neural.S
open Neural.S.Graph
open Neural.S.Algodiff
open Neural.S.Graph.Neuron


let init_target = Dense.Ndarray.S.zeros [|1|]
let style_nodes_idx = [|2;7;12;21;30|]
let weight_directory = Filename.concat (Sys.getenv "HOME") ".owl/dataset/style/vgg19"

let get_shape x = 
  let s = x |> primal' |> unpack_arr |> Dense.Ndarray.S.shape in 
  assert (Array.length s = 4);
  assert (s.(0) = 1);
  s.(0), s.(1), s.(2), s.(3)


(* book: run-prefix begin *)
let run' topo x =
  let last_node_output = ref (F 0.) in
  Array.iteri (fun i n ->
    let input  = if i = 0 then x else !last_node_output in 
    let output = run [|input|] n.neuron in
    last_node_output := output;
  ) topo;
  !last_node_output
(* book: run-prefix end *)

(* book: gram begin *)
let gram x = 
  let _, h, w, feature = get_shape x in 
  let new_shape = [|h * w; feature|] in 
  let ff = Maths.(reshape x new_shape) in 
  let size = F (float_of_int (feature * h * w)) in
  Maths.((transpose ff) *@ ff / size)
(* book: gram end *)

(* book: content-loss begin *)
let c_loss response target = 
  let loss = Maths.((pow (response - target) (F 2.)) |> sum') in 
  let _, h, w, feature = get_shape target in 
  let c = float_of_int ( feature * h * w ) in
  Maths.(loss / (F c))
(* book: content-loss end *)

(* book: style-loss begin *)
let s_loss response_gram target_gram = 
  let loss = Maths.((pow (response_gram - target_gram) (F 2.)) |> sum') in 
  let s = Algodiff.shape target_gram in
  let c = float_of_int ( s.(0) * s.(1)) in
  Maths.(loss / (F c))
(* book: style-loss end *)



(* book: content-target begin *)
let fill_content_targets ~idx x net =
  let selected_topo = Array.sub net.topo 0 (idx + 1) in
  run' selected_topo x
(* book: content-target end *)

(* Each objective captures its own frozen targets; optimising pixels does not train VGG. *)
(* book: content-objective begin *)
let content_objective ~idx net pixels =
  let topo = Array.sub net.topo 0 (idx + 1) in
  let target = fill_content_targets ~idx (Arr pixels) net |> unpack_arr in
  fun x -> c_loss (run' topo x) (Arr target)
(* book: content-objective end *)

(* book: style-objective begin *)
let style_objective ~indices net pixels =
  let topo = Array.sub net.topo 0 (Array.fold_left Stdlib.max 0 indices + 1) in
  let targets = Array.make (Array.length indices) init_target in
  let last = ref (Arr pixels) in
  Array.iteri (fun i n ->
    last := Neuron.run [| !last |] n.neuron;
    Array.iteri (fun j idx -> if i = idx then targets.(j) <- gram !last |> unpack_arr) indices) topo;
  fun x ->
    let total = ref (F 0.) and last = ref x in
    Array.iteri (fun i n ->
      last := Neuron.run [| !last |] n.neuron;
      Array.iteri (fun j idx -> if i = idx then
        total := Maths.(!total + s_loss (gram !last) (Arr targets.(j)))) indices) topo;
    !total
(* book: style-objective end *)

(* book: adam begin *)
let adam_params ~ckpt ~learning_rate iter =
  if ckpt < 1 || not (Float.is_finite iter) || iter < 1. || Float.floor iter <> iter then
    invalid_arg "Positive integral iterations and checkpoint interval required";
  let chkpt state =
    if Checkpoint.(state.current_batch mod ckpt = 0) then
      Checkpoint.(state.stop <- true)
  in
  Params.config ~verbosity:false ~batch:Batch.Full
    ~learning_rate:(Learning_Rate.Adam (learning_rate, 0.9, 0.999))
    ~checkpoint:(Checkpoint.Custom chkpt) iter
(* book: adam end *)


(* book: resume begin *)
let minimise_with_checkpoints ?(checkpoint=(fun _ _ -> ())) params g input_img =
  let state, img = Optimise.minimise_fun params g (Arr input_img) in
  let x' = ref img in
  while Checkpoint.(state.current_batch <= state.batches) do
    checkpoint Checkpoint.(state.current_batch - 1) (unpack_arr !x');
    Checkpoint.(state.stop <- false);
    let _, img = Optimise.minimise_fun ~state params g !x' in
    x' := img
  done;
  state, unpack_arr !x'
(* book: resume end *)

let optimise ?(ckpt=50) ?(learning_rate=10.) ?checkpoint iter g input_img =
  let params = adam_params ~ckpt ~learning_rate iter in
  minimise_with_checkpoints ?checkpoint params g input_img

(* book: noise begin *)
let noise_image content_img =
  let input_shape = Dense.Ndarray.S.shape content_img in
  Dense.Ndarray.S.(gaussian input_shape |> scalar_mul 0.256)
(* book: noise end *)

let load_network ?(weights=weight_directory) h w =
  let nn = Vgg19_short.model h w in
  Portable_weights.load_portable_weights nn weights;
  nn

let objective ?(content_idx=23) ?(indices=style_nodes_idx) ?(alpha=5.) ?(beta=100.) nn content style =
  let content_topo = Array.sub nn.topo 0 (content_idx + 1) in
  let content_target = run' content_topo (Arr content) |> unpack_arr in
  let last_idx = Array.fold_left Stdlib.max content_idx indices in
  let topo = Array.sub nn.topo 0 (last_idx + 1) in
  let style_targets = Array.make (Array.length indices) init_target in
  let last = ref (Arr style) in
  Array.iteri (fun i n ->
    last := Neuron.run [| !last |] n.neuron;
    Array.iteri (fun j idx -> if i = idx then
      style_targets.(j) <- gram !last |> unpack_arr) indices) topo;
  fun x ->
    let total = ref (F 0.) and last = ref x in
    Array.iteri (fun i n ->
      last := Neuron.run [| !last |] n.neuron;
      if i = content_idx then
        total := Maths.(!total + F alpha * c_loss !last (Arr content_target));
      Array.iteri (fun j idx -> if i = idx then
        total := Maths.(!total + F beta * s_loss (gram !last) (Arr style_targets.(j)))) indices) topo;
    !total

let run ?(weights=weight_directory) ?(ckpt=50) ?(init=true)
    ~src ~style ~dst iter =
  let content = Style_io.load src |> ImageUtils.preprocess in
  let input_shape = Dense.Ndarray.S.shape content in
  let style = Style_io.load ~size:(input_shape.(2),input_shape.(1)) style |> ImageUtils.preprocess in
  let input_img = if init then Dense.Ndarray.S.copy content
    else noise_image content in
  let nn = load_network ~weights input_shape.(1) input_shape.(2) in
  let g = objective nn content style in
  let checkpoint step pixels =
    let filename = Printf.sprintf "%s_%04d%s" (Filename.remove_extension dst) step (Filename.extension dst) in
    Style_io.save ~preprocessed:true pixels filename
  in
  let _, pixels = optimise ~ckpt ~checkpoint iter g input_img in
  Style_io.save ~preprocessed:true pixels dst
