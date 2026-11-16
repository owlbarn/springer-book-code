module A = Owl.Dense.Ndarray.S

type layer = { keys : A.arr; values : A.arr }
type program = { rows : int; qkv : Cgraph_block.graph; block : Cgraph_block.graph }
type t = {
  weights : Weights.t;
  layers : layer array;
  capacity : int;
  mutable length : int;
  mutable program : program option;
}

let create ~capacity weights =
  if capacity < 1 then invalid_arg "KV cache capacity must be positive";
  (* Attention uses the fixed capacity. Unused rows are finite zeros, masked
     with -infinity before softmax; uninitialised values could contain NaNs. *)
  let layers = Array.init 24 (fun _ ->
      { keys = A.zeros [| capacity; 128 |]; values = A.zeros [| capacity; 128 |] }) in
  { weights; layers; capacity; length = 0; program = None }

let length cache = cache.length
let capacity cache = cache.capacity
let clear cache =
  Array.iter (fun layer -> A.reset layer.keys; A.reset layer.values) cache.layers;
  cache.length <- 0

let forward ?(tap = fun _ _ -> ()) cache input =
  let shape = A.shape input in
  if Array.length shape <> 2 || shape.(1) <> 896 || shape.(0) < 1 then
    invalid_arg "KV input must have shape [n,896] with n > 0";
  let n = shape.(0) and past = cache.length in
  if n > cache.capacity - past then invalid_arg "KV cache capacity exceeded";
  let program = match cache.program with
    | Some p when p.rows = n -> p
    | _ ->
        let qkv = Cgraph_block.projections ~rows:n ~capacity:cache.capacity cache.weights in
        let block = Cgraph_block.attention_mlp ~rows:n ~capacity:cache.capacity cache.weights in
        let p = { rows = n; qkv; block } in
        cache.program <- Some p;
        p
  in
  let mask = A.init [| n; cache.capacity |] (fun i ->
      if i mod cache.capacity > past + (i / cache.capacity) then neg_infinity else 0.) in
  let angles = A.init [| n; 32 |] (fun i ->
      float (past + (i / 32)) /. (1e6 ** (float (2 * (i mod 32)) /. 64.))) in
  let cos = A.cos angles and sin = A.sin angles in
  let x = ref input in
  for i = 0 to 23 do
    let qkv = Cgraph_block.run program.qkv cache.weights i [ "x", !x; "cos", cos; "sin", sin ] in
    let layer = cache.layers.(i) in
    A.set_slice [ [ past; past + n - 1 ]; [] ] layer.keys qkv.(1);
    A.set_slice [ [ past; past + n - 1 ]; [] ] layer.values qkv.(2);
    let output = Cgraph_block.run program.block cache.weights i
        [ "x", !x; "q", qkv.(0); "k", layer.keys; "v", layer.values; "mask", mask ] in
    (* Graph outputs are scratch buffers. Own the residual before evaluating
       this graph for another layer, and before exposing it to a tap/caller. *)
    x := A.copy output.(0);
    if i = 0 then tap "qwen_layer1" !x
  done;
  let hidden = Nn.rms_norm cache.weights "qwen.norm" !x in
  cache.length <- past + n;
  hidden
