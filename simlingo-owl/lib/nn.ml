module A = Owl.Dense.Ndarray.S

let linear w name x =
  let shape = A.shape x in
  let width = shape.(Array.length shape - 1) in
  let weight = Weights.get w (name ^ ".weight") in
  let y = A.dot (A.reshape x [| A.numel x / width; width |]) weight in
  let y = if Weights.has w (name ^ ".bias") then A.add y (Weights.get w (name ^ ".bias")) else y in
  let output_shape = Array.copy shape in
  output_shape.(Array.length shape - 1) <- (A.shape weight).(1);
  A.reshape y output_shape

let gelu x = A.mul_scalar (A.mul x (A.add_scalar (A.erf (A.div_scalar x (sqrt 2.))) 1.)) 0.5
let silu x = A.mul x (A.sigmoid x)

let layer_norm w name x =
  let axis = Array.length (A.shape x) - 1 in
  let centered = A.sub x (A.mean ~axis ~keep_dims:true x) in
  (* Explicit population variance: independent of sample-variance conventions. *)
  let variance = A.mean ~axis ~keep_dims:true (A.sqr centered) in
  let normalized = A.div centered (A.sqrt (A.add_scalar variance 1e-6)) in
  A.add (A.mul normalized (Weights.get w (name ^ ".weight"))) (Weights.get w (name ^ ".bias"))

let rms_norm w name x =
  let axis = Array.length (A.shape x) - 1 in
  let variance = A.mean ~axis ~keep_dims:true (A.sqr x) in
  A.mul (A.div x (A.sqrt (A.add_scalar variance 1e-6))) (Weights.get w (name ^ ".weight"))

let rows x start length = A.get_slice [ [ start; start + length - 1 ]; [] ] x
let cols x start length = A.get_slice [ []; [ start; start + length - 1 ] ] x
let cat xs = A.concatenate ~axis:0 xs

let attention ?mask ~heads ~kv_heads q k v =
  let dim = (A.shape q).(1) / heads in
  if heads mod kv_heads <> 0 then invalid_arg "GQA head ratio";
  Array.init heads (fun h ->
      let kh = h / (heads / kv_heads) in
      let qh = cols q (h * dim) dim in
      let kh_ = cols k (kh * dim) dim in
      let vh = cols v (kh * dim) dim in
      let scores = A.dot (A.div_scalar qh (sqrt (float dim))) (A.transpose kh_) in
      let scores = match mask with None -> scores | Some m -> A.add scores m in
      A.dot (A.softmax ~axis:1 scores) vh)
  |> A.concatenate ~axis:1

let argmax x =
  let values = A.to_array x in
  let best = ref 0 in
  Array.iteri
    (fun i v ->
      if not (Float.is_finite v) then failwith "non-finite logits";
      if v > values.(!best) then best := i)
    values;
  !best
