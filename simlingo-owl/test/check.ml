open Simlingo_owl
open Yojson.Basic.Util
module A = Owl.Dense.Ndarray.S

let () =
  let root = Sys.argv.(1) in
  let tokenizer = Tokenizer.load Sys.argv.(2) in
  let cases = Yojson.Basic.from_file (Filename.concat root "tokenizer.json") |> to_list in
  List.iteri
    (fun i case ->
      let text = case |> member "text" |> to_string in
      let expected = case |> member "ids" |> to_list |> List.map to_int |> Array.of_list in
      let actual = Tokenizer.encode tokenizer text in
      if expected <> actual then failwith (Printf.sprintf "tokenizer case %d: %S" i text);
      let decoded = case |> member "decoded" |> to_string in
      if Tokenizer.decode tokenizer actual <> decoded then
        failwith
          (Printf.sprintf "decode case %d: expected %S, got %S" i decoded
             (Tokenizer.decode tokenizer actual)))
    cases;
  let decode_cases = Yojson.Basic.from_file (Filename.concat root "decode.json") |> to_list in
  List.iter
    (fun case ->
      let ids = case |> member "ids" |> to_list |> List.map to_int |> Array.of_list in
      if Tokenizer.decode tokenizer ids <> (case |> member "decoded" |> to_string) then
        failwith "UTF-8/truncated decode mismatch")
    decode_cases;
  let load name = A.load_npy (Filename.concat root (name ^ ".npy")) in
  let check name result =
    let expected = load name in
    if A.shape expected <> A.shape result then failwith ("shape: " ^ name);
    let error = A.max' (A.abs (A.sub expected result)) in
    if (not (Float.is_finite error)) || error > 2e-5 then
      failwith (Printf.sprintf "%s max error %g" name error)
  in
  check "gelu" (Nn.gelu (load "activation_input"));
  check "silu" (Nn.silu (load "activation_input"));
  check "rope" (Qwen2.rope (Qwen2.rope_tables 7) (load "rope_input"));
  let mask = A.init [| 7; 7 |] (fun i -> if i mod 7 > i / 7 then neg_infinity else 0.) in
  check "attention" (Nn.attention ~mask ~heads:14 ~kv_heads:2 (load "q") (load "k") (load "v"));
  check "shuffle" (Vit.pixel_shuffle (load "shuffle_input"));
  (* Raster begins with whitespace bytes: never skip them as PPM header space. *)
  let width, height, bytes = Preprocess.ppm (Filename.concat root "whitespace.ppm") in
  if width <> 1 || height <> 1 || bytes <> "\n\r " then failwith "PPM whitespace pixel";
  let rejected =
    try
      ignore (Tokenizer.encode tokenizer "你好");
      false
    with Invalid_argument _ -> true
  in
  if not rejected then failwith "non-ASCII question silently accepted";
  let load_weight name =
    Weights.load (Filename.concat root name) |> fun w -> Weights.get w "weight"
  in
  if A.shape (load_weight "valid") <> [| 2; 2 |] then failwith "valid weight load";
  List.iter
    (fun name ->
      let rejected =
        try
          ignore (load_weight name);
          false
        with Failure _ -> true
      in
      if not rejected then failwith ("accepted corrupt weight: " ^ name))
    [ "bad-shape"; "bad-hash" ];
  Printf.printf
    "%d tokenizer cases, %d UTF-8 decode cases; activation, RoPE, GQA, shuffle, PPM, \
     weight-integrity checks passed\n"
    (List.length cases) (List.length decode_cases)
