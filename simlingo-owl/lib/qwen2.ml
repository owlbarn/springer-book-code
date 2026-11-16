module A = Owl.Dense.Ndarray.S

let rope_tables n =
  let angles =
    A.init [| n; 32 |] (fun i -> float (i / 32) /. (1e6 ** (float (2 * (i mod 32)) /. 64.)))
  in
  (A.cos angles, A.sin angles)

let rope (cos, sin) x =
  let n = (A.shape x).(0) in
  let heads = (A.shape x).(1) / 64 in
  Array.init heads (fun h ->
      let a = Nn.cols x (h * 64) 32 and b = Nn.cols x ((h * 64) + 32) 32 in
      A.concatenate ~axis:1
        [| A.sub (A.mul a cos) (A.mul b sin); A.add (A.mul b cos) (A.mul a sin) |])
  |> A.concatenate ~axis:1
  |> fun x -> A.reshape x [| n; heads * 64 |]

let forward ?(tap = fun _ _ -> ()) w input =
  let n = (A.shape input).(0) in
  let mask = A.init [| n; n |] (fun i -> if i mod n > i / n then neg_infinity else 0.) in
  let tables = rope_tables n in
  let x = ref input in
  for i = 0 to 23 do
    let p = Printf.sprintf "qwen.layers.%d" i in
    let z = Nn.rms_norm w (p ^ ".input_layernorm") !x in
    let q = Nn.linear w (p ^ ".self_attn.q_proj") z |> rope tables in
    let k = Nn.linear w (p ^ ".self_attn.k_proj") z |> rope tables in
    let v = Nn.linear w (p ^ ".self_attn.v_proj") z in
    let attn =
      Nn.attention ~mask ~heads:14 ~kv_heads:2 q k v |> Nn.linear w (p ^ ".self_attn.o_proj")
    in
    x := A.add !x attn;
    let z = Nn.rms_norm w (p ^ ".post_attention_layernorm") !x in
    let gate = Nn.linear w (p ^ ".mlp.gate_proj") z |> Nn.silu in
    let up = Nn.linear w (p ^ ".mlp.up_proj") z in
    x := A.add !x (Nn.linear w (p ^ ".mlp.down_proj") (A.mul gate up));
    if i = 0 then tap "qwen_layer1" !x
  done;
  Nn.rms_norm w "qwen.norm" !x

let logits w hidden = Nn.rows hidden ((A.shape hidden).(0) - 1) 1 |> Nn.linear w "qwen.lm_head"
