module A = Owl.Dense.Ndarray.S

let pixel_shuffle x =
  (* InternVL ps_version=v2: both spatial ordering and channel order matter. *)
  A.reshape x [| 32; 16; 2048 |] |> A.transpose ~axis:[| 1; 0; 2 |] |> fun x ->
  A.reshape x [| 16; 16; 4096 |] |> A.transpose ~axis:[| 1; 0; 2 |] |> fun x ->
  A.reshape x [| 256; 4096 |]

let tile ?(tap = fun _ _ -> ()) w image =
  let kernel = Weights.expect w "vit.embeddings.patch_embedding.weight" [| 14; 14; 3; 1024 |] in
  let patches =
    A.conv2d ~padding:Owl_types.VALID image kernel [| 14; 14 |] |> fun x ->
    A.reshape x [| 1024; 1024 |] |> fun x ->
    A.add x (Weights.get w "vit.embeddings.patch_embedding.bias")
  in
  let cls = Weights.get w "vit.embeddings.class_embedding" |> fun x -> A.reshape x [| 1; 1024 |] in
  let pos =
    Weights.get w "vit.embeddings.position_embedding" |> fun x -> A.reshape x [| 1025; 1024 |]
  in
  let x = ref (A.add (Nn.cat [| cls; patches |]) pos) in
  for i = 0 to 23 do
    let p = Printf.sprintf "vit.encoder.layers.%d" i in
    let qkv = Nn.layer_norm w (p ^ ".norm1") !x |> Nn.linear w (p ^ ".attn.qkv") in
    let attn =
      Nn.attention ~heads:16 ~kv_heads:16 (Nn.cols qkv 0 1024) (Nn.cols qkv 1024 1024)
        (Nn.cols qkv 2048 1024)
      |> Nn.linear w (p ^ ".attn.proj")
    in
    x := A.add !x (A.mul attn (Weights.get w (p ^ ".ls1")));
    let mlp =
      Nn.layer_norm w (p ^ ".norm2") !x
      |> Nn.linear w (p ^ ".mlp.fc1")
      |> Nn.gelu
      |> Nn.linear w (p ^ ".mlp.fc2")
    in
    x := A.add !x (A.mul mlp (Weights.get w (p ^ ".ls2")));
    if i = 0 then tap "vit_layer1" !x
  done;
  Nn.rows !x 1 1024 |> pixel_shuffle |> Nn.layer_norm w "mlp1.0" |> Nn.linear w "mlp1.1" |> Nn.gelu
  |> Nn.linear w "mlp1.3"

let forward ?(tap = fun _ _ -> ()) w pixels =
  if A.shape pixels <> [| 2; 448; 448; 3 |] then invalid_arg "expected pixels [2,448,448,3]";
  let output =
    Array.init 2 (fun i ->
        Printf.eprintf "vision tile %d/2\n%!" (i + 1);
        let image =
          A.get_slice [ [ i ]; []; []; [] ] pixels |> fun x -> A.reshape x [| 1; 448; 448; 3 |]
        in
        tile ~tap:(fun name x -> tap (Printf.sprintf "%s_tile%d" name i) x) w image)
    |> Nn.cat
  in
  tap "image_features" output;
  output
