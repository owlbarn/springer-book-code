module A = Owl.Dense.Ndarray.S

type layer = { keys : A.arr; values : A.arr }
type t = { weights : Weights.t; layers : layer array; capacity : int; mutable length : int }

let create ~capacity weights =
  if capacity < 1 then invalid_arg "KV cache capacity must be positive";
  let layers =
    Array.init 24 (fun _ ->
        { keys = A.empty [| capacity; 128 |]; values = A.empty [| capacity; 128 |] })
  in
  { weights; layers; capacity; length = 0 }

let length cache = cache.length
let capacity cache = cache.capacity
let clear cache = cache.length <- 0

let forward ?(tap = fun _ _ -> ()) cache input =
  let shape = A.shape input in
  if Array.length shape <> 2 || shape.(1) <> 896 || shape.(0) < 1 then
    invalid_arg "KV input must have shape [n,896] with n > 0";
  let n = shape.(0) and past = cache.length in
  if n > cache.capacity - past then invalid_arg "KV cache capacity exceeded";
  let total = past + n in
  let mask =
    if n = 1 then None
    else Some (A.init [| n; total |] (fun i ->
        if i mod total > past + (i / total) then neg_infinity else 0.))
  in
  let angles = A.init [| n; 32 |] (fun i ->
      float (past + (i / 32)) /. (1e6 ** (float (2 * (i mod 32)) /. 64.))) in
  let tables = A.cos angles, A.sin angles in
  let w = cache.weights and x = ref input in
  for i = 0 to 23 do
    let p = Printf.sprintf "qwen.layers.%d" i in
    let z = Nn.rms_norm w (p ^ ".input_layernorm") !x in
    let q = Nn.linear w (p ^ ".self_attn.q_proj") z |> Qwen2.rope tables in
    let k = Nn.linear w (p ^ ".self_attn.k_proj") z |> Qwen2.rope tables in
    let v = Nn.linear w (p ^ ".self_attn.v_proj") z in
    let layer = cache.layers.(i) in
    A.set_slice [ [ past; total - 1 ]; [] ] layer.keys k;
    A.set_slice [ [ past; total - 1 ]; [] ] layer.values v;
    let keys = Nn.rows layer.keys 0 total and values = Nn.rows layer.values 0 total in
    let attn = Nn.attention ?mask ~heads:14 ~kv_heads:2 q keys values
               |> Nn.linear w (p ^ ".self_attn.o_proj") in
    x := A.add !x attn;
    let z = Nn.rms_norm w (p ^ ".post_attention_layernorm") !x in
    let gate = Nn.linear w (p ^ ".mlp.gate_proj") z |> Nn.silu in
    let up = Nn.linear w (p ^ ".mlp.up_proj") z in
    x := A.add !x (Nn.linear w (p ^ ".mlp.down_proj") (A.mul gate up));
    if i = 0 then tap "qwen_layer1" !x
  done;
  let hidden = Nn.rms_norm w "qwen.norm" !x in
  (* Commit only after all layers succeed; an interrupted append can be retried. *)
  cache.length <- total;
  hidden
