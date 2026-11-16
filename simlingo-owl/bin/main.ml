open Simlingo_owl
module A = Owl.Dense.Ndarray.S
open Yojson.Basic.Util

let () =
  let weights = ref "" and image = ref "" and pixels = ref "" and targets = ref "" in
  let question = ref "What should the ego do next?" and speed = ref nan in
  let output = ref "output.json" and max_tokens = ref 100 and crop = ref 0. in
  let dump = ref "" and text = ref "" and tokenize_only = ref false in
  let input_embeddings = ref "" and forward_only = ref false in
  Arg.parse
    [
      ("--weights", Arg.Set_string weights, "DIR converted weights");
      ("--image", Arg.Set_string image, "FILE binary RGB PPM");
      ("--pixels", Arg.Set_string pixels, "FILE preprocessed fp32 NHWC npy (debug)");
      ("--targets", Arg.Set_string targets, "FILE JSON array [[x1,y1],[x2,y2]] in metres");
      ("--speed", Arg.Set_float speed, "MPS current ego speed");
      ("--question", Arg.Set_string question, "TEXT ASCII question");
      ("--output", Arg.Set_string output, "FILE output JSON");
      ("--max-new-tokens", Arg.Set_int max_tokens, "N 1..100 (default 100)");
      ("--crop-bottom", Arg.Set_float crop, "FRACTION optional crop before resize (default 0)");
      ("--dump", Arg.Set_string dump, "DIR save intermediate npy files (must exist)");
      ( "--tokenize",
        Arg.String
          (fun s ->
            text := s;
            tokenize_only := true),
        "TEXT only encode/decode text" );
      ("--input-embeddings", Arg.Set_string input_embeddings, "FILE replay merged input npy (debug)");
      ("--forward-only", Arg.Set forward_only, " only one Qwen2 forward (debug)");
    ]
    (fun s -> raise (Arg.Bad ("unexpected argument: " ^ s)))
    "SimLingo inference with Owl";
  if !weights = "" then failwith "--weights is required";
  if !max_tokens < 1 || !max_tokens > 100 then failwith "--max-new-tokens must be 1..100";
  if !dump <> "" && ((not (Sys.file_exists !dump)) || not (Sys.is_directory !dump)) then
    failwith "--dump directory must already exist";
  if not (Sys.file_exists (Filename.dirname !output)) then
    failwith "output parent directory does not exist";
  let tokenizer = Tokenizer.load (Filename.concat !weights "tokenizer") in
  let write json = Yojson.Basic.to_file !output json in
  let ints ids = `List (Array.to_list (Array.map (fun i -> `Int i) ids)) in
  if !tokenize_only then
    let ids = Tokenizer.encode tokenizer !text in
    write (`Assoc [ ("ids", ints ids); ("decoded", `String (Tokenizer.decode tokenizer ids)) ])
  else
    let w = Weights.load !weights in
    let tap name value =
      if !dump <> "" then A.save_npy ~out:(Filename.concat !dump (name ^ ".npy")) value
    in
    let start = Unix.gettimeofday () in
    let input =
      if !input_embeddings <> "" then A.load_npy !input_embeddings
      else (
        if !targets = "" || not (Float.is_finite !speed) then
          failwith "--targets and --speed are required";
        if !image = "" = (!pixels = "") then failwith "supply exactly one of --image or --pixels";
        let target_json = Yojson.Basic.from_file !targets |> to_list in
        let number = function
          | `Int n -> float n
          | `Float f -> f
          | _ -> failwith "invalid target coordinate"
        in
        let coordinates =
          List.map
            (fun row ->
              let values = to_list row in
              if List.length values <> 2 then failwith "target must have two coordinates";
              List.map number values)
            target_json
        in
        if List.length coordinates <> 2 then failwith "supply two target points";
        let values = List.concat coordinates |> Array.of_list in
        if not (Array.for_all Float.is_finite values) then failwith "non-finite target";
        let targets = A.of_array values [| 2; 2 |] in
        let ids = Simlingo.prompt ~speed:!speed ~question:!question |> Tokenizer.encode tokenizer in
        if !dump <> "" then
          Yojson.Basic.to_file (Filename.concat !dump "prompt_ids.json") (ints ids);
        let pixels =
          if !pixels <> "" then A.load_npy !pixels else Preprocess.load ~crop_bottom:!crop !image
        in
        tap "pixels" pixels;
        let features = Vit.forward ~tap w pixels in
        Simlingo.merge w ids features targets)
    in
    if Array.length (A.shape input) <> 2 || (A.shape input).(1) <> 896 then
      failwith "input shape must be [sequence,896]";
    tap "input_embeddings" input;
    if !forward_only then (
      let logits = Qwen2.forward ~tap w input |> Qwen2.logits w in
      tap "first_logits" logits;
      write (`Assoc [ ("argmax", `Int (Nn.argmax logits)) ]))
    else
      let ids, route, speed = Simlingo.generate ~tap ~max_new_tokens:!max_tokens w input in
      let points array =
        let shape = A.shape array in
        `List
          (List.init shape.(0) (fun i ->
               `List
                 (List.init 2 (fun j ->
                      let v = A.get array [| i; j |] in
                      if not (Float.is_finite v) then failwith "non-finite trajectory";
                      `Float v))))
      in
      write
        (`Assoc
           [
             ("answer", `String (Tokenizer.decode tokenizer ids));
             ("token_ids", ints ids);
             ("eos_reached", `Bool (ids.(Array.length ids - 1) = 151645));
             ("route", points route);
             ("speed_waypoints", points speed);
             ("elapsed_seconds", `Float (Unix.gettimeofday () -. start));
           ])
