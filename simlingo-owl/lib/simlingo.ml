module A = Owl.Dense.Ndarray.S

let prompt ~speed ~question =
  if (not (Float.is_finite speed)) || speed < 0. then invalid_arg "invalid speed";
  if String.contains question '<' then invalid_arg "question must not contain special-token syntax";
  "<|im_start|>user\n<img>"
  ^ String.concat "" (List.init 512 (fun _ -> "<IMG_CONTEXT>"))
  ^ "</img>\n"
  ^ Printf.sprintf "Current speed: %.1f m/s. Target waypoint: <TARGET_POINT><TARGET_POINT>. %s"
      speed question
  ^ "<|im_end|><|im_start|>assistant\n"

let embeddings w ids =
  A.rows
    (Weights.expect w "qwen.embed_tokens.weight" [| 151655; 896 |])
    (Array.map (fun id -> max 0 (min 151654 id)) ids)

let merge w ids image_features targets =
  if A.shape image_features <> [| 512; 896 |] then invalid_arg "image feature shape";
  if A.shape targets <> [| 2; 2 |] then invalid_arg "two target points required";
  let x = embeddings w ids in
  let points =
    Nn.linear w "wp_encoder.mlp.0" targets
    |> A.relu |> Nn.linear w "wp_encoder.mlp.2" |> A.relu |> Nn.linear w "wp_encoder.mlp.4"
  in
  let image_index = ref 0 and target_index = ref 0 in
  Array.iteri
    (fun pos id ->
      let replace values index = A.set_slice [ [ pos ]; [] ] x (Nn.rows values index 1) in
      if id = 151648 then (
        if !image_index >= 512 then invalid_arg "too many image tokens";
        replace image_features !image_index;
        incr image_index)
      else if id = 151662 then (
        if !target_index >= 2 then invalid_arg "too many target tokens";
        replace points !target_index;
        incr target_index)
      else if id >= 151655 then invalid_arg "unsupported placeholder in prompt")
    ids;
  if !image_index <> 512 || !target_index <> 2 then invalid_arg "prompt placeholder count";
  x

let driving w input =
  let route_queries =
    Weights.expect w "driving.query_embeds_wps" [| 1; 20; 896 |] |> fun x ->
    A.reshape x [| 20; 896 |]
  in
  let speed_queries =
    Weights.expect w "driving.query_embeds_speed" [| 1; 10; 896 |] |> fun x ->
    A.reshape x [| 10; 896 |]
  in
  let hidden = Qwen2.forward w (Nn.cat [| input; route_queries; speed_queries |]) in
  let start = (A.shape hidden).(0) - 30 in
  let route =
    Nn.rows hidden start 20
    |> Nn.linear w "driving.route_head.0"
    |> Nn.silu
    |> Nn.linear w "driving.route_head.2"
    |> Nn.silu
    |> Nn.linear w "driving.route_head.4"
    |> A.cumsum ~axis:0
  in
  let speed =
    Nn.rows hidden (start + 20) 10
    |> Nn.linear w "driving.speed_wps_head.0"
    |> Nn.silu
    |> Nn.linear w "driving.speed_wps_head.2"
    |> A.cumsum ~axis:0
  in
  (route, speed)

let generate ?(tap = fun _ _ -> ()) ~max_new_tokens w input =
  if max_new_tokens < 1 || max_new_tokens > 100 then invalid_arg "max-new-tokens must be 1..100";
  let rec loop step input ids =
    if step = max_new_tokens then (Array.of_list (List.rev ids), input)
    else (
      Printf.eprintf "language token %d/%d (sequence %d)\n%!" (step + 1) max_new_tokens
        (A.shape input).(0);
      let hidden = Qwen2.forward ~tap:(if step = 0 then tap else fun _ _ -> ()) w input in
      let logits = Qwen2.logits w hidden in
      if step = 0 then tap "first_logits" logits;
      let id = Nn.argmax logits in
      let input = Nn.cat [| input; embeddings w [| id |] |] in
      if id = 151645 then (Array.of_list (List.rev (id :: ids)), input)
      else loop (step + 1) input (id :: ids))
  in
  let ids, input = loop 0 input [] in
  Printf.eprintf "driving head\n%!";
  let route, speed = driving w input in
  (ids, route, speed)
