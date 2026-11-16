module A = Owl.Dense.Ndarray.S
module C = Owl_computation_cpu_engine.Make (Owl_algodiff_primal_ops.S)

(* Fixed-shape graphs share scratch space across all 24 layers. Inputs are
   non-reusable variable nodes. Rebinding their public value blocks avoids
   copying weights, or writing the next layer's weights into the previous one. *)
let bind variable value =
  let node = C.arr_to_node variable in
  if not (C.is_var node) || C.get_reuse node || C.node_shape node <> A.shape value then
    invalid_arg "CGraph binding requires a non-reusable variable of matching shape";
  C.make_value_block (C.arr_to_value value) node;
  C.invalidate_graph node

let scalar = C.pack_elt
let cols x start count = C.get_slice [ []; [ start; start + count - 1 ] ] x
let silu x = C.mul x (C.sigmoid x)

let rms_norm weight x =
  let variance = C.div_scalar (C.sum ~axis:1 ~keep_dims:true (C.sqr x)) (scalar 896.) in
  C.mul (C.div x (C.sqrt (C.add_scalar variance (scalar 1e-6)))) weight

let rope ~heads cos sin x =
  Array.init heads (fun h ->
      let a = cols x (h * 64) 32 and b = cols x ((h * 64) + 32) 32 in
      C.concatenate ~axis:1
        [| C.sub (C.mul a cos) (C.mul b sin); C.add (C.mul b cos) (C.mul a sin) |])
  |> C.concatenate ~axis:1

type graph = {
  graph : C.graph;
  inputs : (string * C.arr) list;
  weights : (string * C.arr) list;
  outputs : C.arr array;
}

let build ~name ~rows ~capacity w make =
  let inputs = ref [] and weights = ref [] in
  let input name shape =
    let v = C.var_arr ~shape name in
    inputs := (name, v) :: !inputs;
    v
  in
  let weight suffix =
    let value = Weights.get w ("qwen.layers.0." ^ suffix) in
    let v = C.var_arr ~shape:(A.shape value) suffix in
    weights := (suffix, v) :: !weights;
    v
  in
  let outputs = make input weight rows capacity in
  let nodes bindings = Array.of_list (List.map (fun (_, v) -> C.arr_to_node v) bindings) in
  let graph = C.make_graph ~input:(nodes (!inputs @ !weights))
      ~output:(Array.map C.arr_to_node outputs) name in
  C.optimise graph;
  { graph; inputs = !inputs; weights = !weights; outputs }

let run graph w layer values =
  List.iter (fun (name, variable) -> bind variable (List.assoc name values)) graph.inputs;
  List.iter (fun (suffix, variable) ->
      bind variable (Weights.get w (Printf.sprintf "qwen.layers.%d.%s" layer suffix))) graph.weights;
  C.eval_graph graph.graph;
  Array.map C.unpack_arr graph.outputs

let linear weight name x =
  let y = C.dot x (weight (name ^ ".weight")) in
  if List.mem name [ "self_attn.q_proj"; "self_attn.k_proj"; "self_attn.v_proj" ] then
    C.add y (weight (name ^ ".bias"))
  else y

let projections ~rows ~capacity w =
  build ~name:"qwen-qkv" ~rows ~capacity w (fun input weight n _ ->
      let x = input "x" [| n; 896 |] in
      let cos = input "cos" [| n; 32 |] and sin = input "sin" [| n; 32 |] in
      let z = rms_norm (weight "input_layernorm.weight") x in
      [| linear weight "self_attn.q_proj" z |> rope ~heads:14 cos sin;
         linear weight "self_attn.k_proj" z |> rope ~heads:2 cos sin;
         linear weight "self_attn.v_proj" z |])

let attention_mlp ~rows ~capacity w =
  build ~name:"qwen-attention-mlp" ~rows ~capacity w (fun input weight n capacity ->
      let x = input "x" [| n; 896 |] and q = input "q" [| n; 896 |] in
      let k = input "k" [| capacity; 128 |] and v = input "v" [| capacity; 128 |] in
      let mask = input "mask" [| n; capacity |] in
      let heads = Array.init 14 (fun h ->
          let qh = cols q (h * 64) 64 in
          let kh = cols k ((h / 7) * 64) 64 and vh = cols v ((h / 7) * 64) 64 in
          let scores = C.add (C.dot (C.div_scalar qh (scalar 8.)) (C.transpose kh)) mask in
          let exp = C.exp (C.sub scores (C.max ~axis:1 ~keep_dims:true scores)) in
          let probabilities = C.div exp (C.sum ~axis:1 ~keep_dims:true exp) in
          C.dot probabilities vh) in
      let attn = C.concatenate ~axis:1 heads |> linear weight "self_attn.o_proj" in
      let residual = C.add x attn in
      let z = rms_norm (weight "post_attention_layernorm.weight") residual in
      let gate = linear weight "mlp.gate_proj" z |> silu in
      let up = linear weight "mlp.up_proj" z in
      [| C.add residual (linear weight "mlp.down_proj" (C.mul gate up)) |])
