open Simlingo_owl
open Yojson.Basic.Util
module A = Owl.Dense.Ndarray.S

module type CACHE = sig
  type t
  val create : capacity:int -> Weights.t -> t
  val length : t -> int
  val capacity : t -> int
  val clear : t -> unit
  val forward : ?tap:(string -> A.arr -> unit) -> t -> A.arr -> A.arr
end

(* Uses real weights, and the independent full-sequence path as the oracle.
   Usage: cache.exe WEIGHTS INPUT_EMBEDDINGS BASELINE_OUTPUT REPORT [--cgraph] *)
let () =
  let cgraph = Array.exists ((=) "--cgraph") Sys.argv in
  let module Cache = (val (if cgraph then (module Qwen2_cgraph : CACHE)
                          else (module Qwen2_cached : CACHE))) in
  let w = Weights.load Sys.argv.(1) in
  let input = A.load_npy Sys.argv.(2) in
  let saved_input = A.copy input in
  let saved_weights = List.map (fun name -> name, A.copy (Weights.get w name))
      [ "qwen.layers.0.self_attn.q_proj.weight"; "qwen.layers.0.mlp.gate_proj.weight";
        "qwen.layers.23.mlp.down_proj.weight" ] in
  let baseline = Yojson.Basic.from_file Sys.argv.(3) in
  let errors = ref [] in
  let check name expected actual =
    if A.shape expected <> A.shape actual then failwith ("shape: " ^ name);
    let error = A.max' (A.abs (A.sub expected actual)) in
    if not (Float.is_finite error) || error > 1e-3 then
      failwith (Printf.sprintf "%s: max absolute error %g" name error);
    errors := (name, `Float error) :: !errors;
    Printf.printf "%s: max absolute error %.8g\n%!" name error
  in
  let rejected name f =
    match f () with
    | _ -> failwith ("accepted invalid " ^ name)
    | exception Invalid_argument _ -> ()
  in
  let short = Nn.rows input 0 7 in
  let full = Qwen2.forward w short in
  let cache = Cache.create ~capacity:7 w in
  let part start count = Cache.forward cache (Nn.rows short start count) in
  let first = part 0 3 in
  let saved_first = A.copy first in
  check "prefill" (Nn.rows full 0 3) first;
  check "single_token" (Nn.rows full 3 1) (part 3 1);
  (* Three appended rows must not see their future neighbours. *)
  check "chunk_mask_and_positions" (Nn.rows full 4 3) (part 4 3);
  if Cache.length cache <> 7 || Cache.capacity cache <> 7 then
    failwith "cache length/capacity";
  rejected "overflow" (fun () -> part 0 1);
  rejected "shape" (fun () -> Cache.forward cache (A.zeros [| 1; 64 |]));
  rejected "empty chunk" (fun () -> Cache.forward cache (A.empty [| 0; 896 |]));
  rejected "capacity" (fun () -> Cache.create ~capacity:0 w);
  if Cache.length cache <> 7 then failwith "rejection changed length";
  Cache.clear cache;
  check "reset" first (part 0 3);
  (* A failed append must leave the existing prefix usable. *)
  let interrupted =
    try
      ignore (Cache.forward ~tap:(fun _ _ -> raise Exit) cache (Nn.rows short 3 1));
      false
    with Exit -> true
  in
  if not interrupted || Cache.length cache <> 3 then failwith "failed append committed";
  check "retry_append" (Nn.rows full 3 1) (part 3 1);
  let other = Cache.create ~capacity:7 w in
  let different = A.mul_scalar short 0.5 in
  check "separate_request" (Qwen2.forward w different) (Cache.forward other different);
  check "original_request" (Nn.rows full 4 3) (part 4 3);
  (* A token-limit stop still includes the last selected token in the head. *)
  List.iter (fun limit ->
      let ids, route, speed = Simlingo.generate ~max_new_tokens:limit w short in
      let cached_ids, cached_route, cached_speed =
        Simlingo_cached.generate ~cgraph ~max_new_tokens:limit w short in
      if ids <> cached_ids then failwith "token-limit IDs differ";
      if Array.length ids <> limit || ids.(limit - 1) = 151645 then
        failwith "fixture did not exercise a token-limit stop";
      check (Printf.sprintf "limit_%d_route" limit) route cached_route;
      check (Printf.sprintf "limit_%d_speed" limit) speed cached_speed)
    [ 1; 3 ];
  check "retained_output" saved_first first;
  let prefix = ref input and steps = ref 0 in
  let on_logits step actual =
    let expected = Qwen2.forward w !prefix |> Qwen2.logits w in
    check (Printf.sprintf "logits_%02d" step) expected actual;
    let id = Nn.argmax expected in
    if id <> Nn.argmax actual then failwith "greedy token differs";
    prefix := Nn.cat [| !prefix; Simlingo.embeddings w [| id |] |];
    incr steps
  in
  let ids, route, speed = Simlingo_cached.generate ~cgraph ~on_logits ~max_new_tokens:100 w input in
  let expected_ids = baseline |> member "token_ids" |> to_list |> List.map to_int |> Array.of_list in
  if ids <> expected_ids || ids.(Array.length ids - 1) <> 151645 then
    failwith "full answer/EOS differs from retained baseline";
  let expected_route, expected_speed = Simlingo.driving w !prefix in
  check "eos_route" expected_route route;
  check "eos_speed" expected_speed speed;
  if A.max' (A.abs (A.sub input saved_input)) <> 0. then failwith "input was overwritten";
  List.iter (fun (name, before) ->
      if A.max' (A.abs (A.sub before (Weights.get w name))) <> 0. then
        failwith ("weight was overwritten: " ^ name)) saved_weights;
  Yojson.Basic.to_file Sys.argv.(4)
    (`Assoc [ "passed", `Bool true; "cgraph", `Bool cgraph; "decoded_steps", `Int !steps;
              "absolute_tolerance", `Float 1e-3;
              "max_absolute_errors", `Assoc (List.rev !errors) ]);
  Printf.printf "KV cache checks passed (%d full-prefix logit comparisons).\n%!" !steps
