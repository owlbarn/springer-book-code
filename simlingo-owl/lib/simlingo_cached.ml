module A = Owl.Dense.Ndarray.S

(* Keep the full-sequence implementation in Simlingo as an independent baseline. *)
let driving w forward last_token =
  let route_queries = Weights.expect w "driving.query_embeds_wps" [| 1; 20; 896 |]
                      |> fun x -> A.reshape x [| 20; 896 |] in
  let speed_queries = Weights.expect w "driving.query_embeds_speed" [| 1; 10; 896 |]
                      |> fun x -> A.reshape x [| 10; 896 |] in
  (* The selected final token (EOS or a token-limit stop) is not cached yet. *)
  let hidden = forward (Nn.cat [| last_token; route_queries; speed_queries |]) in
  let route = Nn.rows hidden 1 20
              |> Nn.linear w "driving.route_head.0" |> Nn.silu
              |> Nn.linear w "driving.route_head.2" |> Nn.silu
              |> Nn.linear w "driving.route_head.4" |> A.cumsum ~axis:0 in
  let speed = Nn.rows hidden 21 10
              |> Nn.linear w "driving.speed_wps_head.0" |> Nn.silu
              |> Nn.linear w "driving.speed_wps_head.2" |> A.cumsum ~axis:0 in
  route, speed

let generate ?(cgraph = false) ?(tap = fun _ _ -> ()) ?(on_logits = fun _ _ -> ()) ~max_new_tokens w input =
  if max_new_tokens < 1 || max_new_tokens > 100 then invalid_arg "max-new-tokens must be 1..100";
  let shape = A.shape input in
  if Array.length shape <> 2 || shape.(1) <> 896 || shape.(0) < 1 then
    invalid_arg "input must have shape [n,896] with n > 0";
  let capacity = shape.(0) + max_new_tokens + 30 in
  let forward, length =
    if cgraph then
      let cache = Qwen2_cgraph.create ~capacity w in
      (fun ~tap x -> Qwen2_cgraph.forward ~tap cache x), (fun () -> Qwen2_cgraph.length cache)
    else
      let cache = Qwen2_cached.create ~capacity w in
      (fun ~tap x -> Qwen2_cached.forward ~tap cache x), (fun () -> Qwen2_cached.length cache)
  in
  let rec loop step chunk ids =
    Printf.eprintf "language token %d/%d (cached prefix %d, new rows %d)\n%!"
      (step + 1) max_new_tokens (length ()) (A.shape chunk).(0);
    let hidden = forward ~tap:(if step = 0 then tap else fun _ _ -> ()) chunk in
    let logits = Qwen2.logits w hidden in
    if step = 0 then tap "first_logits" logits;
    on_logits step logits;
    let id = Nn.argmax logits in
    let last_token = Simlingo.embeddings w [| id |] in
    let ids = id :: ids in
    if id = 151645 || step + 1 = max_new_tokens then (
      Printf.eprintf "driving head (cached prefix %d, new rows 31)\n%!" (length ());
      let route, speed = driving w (fun x -> forward ~tap:(fun _ _ -> ()) x) last_token in
      Array.of_list (List.rev ids), route, speed)
    else loop (step + 1) last_token ids
  in
  loop 0 input []
