(* Qwen2 byte BPE. Encoding deliberately supports ASCII prompt text only;
   decoding supports every UTF-8 byte sequence in the vocabulary. *)
open Yojson.Basic.Util

type t = {
  vocab : (string, int) Hashtbl.t;
  inverse : (int, string) Hashtbl.t;
  ranks : (string * string, int) Hashtbl.t;
  special : (string * int) list;
  bytes : string array;
  unbytes : (string, char) Hashtbl.t;
}

let utf8 n =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int n);
  Buffer.contents b

let glyphs s =
  let rec loop i acc =
    if i = String.length s then List.rev acc
    else
      let c = Char.code s.[i] in
      let n = if c < 128 then 1 else if c < 224 then 2 else if c < 240 then 3 else 4 in
      if i + n > String.length s then failwith "malformed UTF-8 vocabulary";
      loop (i + n) (String.sub s i n :: acc)
  in
  loop 0 []

let load root =
  let vocab = Hashtbl.create 160000 and inverse = Hashtbl.create 160000 in
  let add (token, id) =
    Hashtbl.replace vocab token id;
    Hashtbl.replace inverse id token
  in
  Yojson.Basic.from_file (Filename.concat root "vocab.json")
  |> to_assoc
  |> List.iter (fun (s, j) -> add (s, to_int j));
  let special =
    Yojson.Basic.from_file (Filename.concat root "added_tokens.json")
    |> to_assoc
    |> List.map (fun (s, j) -> (s, to_int j))
  in
  List.iter add special;
  let ranks = Hashtbl.create 160000 in
  let ic = open_in (Filename.concat root "merges.txt") in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let rank = ref 0 in
      try
        while true do
          let line = input_line ic in
          if line <> "" && not (String.starts_with ~prefix:"#version" line) then
            match String.split_on_char ' ' line with
            | [ a; b ] ->
                Hashtbl.add ranks (a, b) !rank;
                incr rank
            | _ -> failwith "invalid BPE merge"
        done
      with End_of_file -> ());
  let extra = ref 256 in
  let bytes =
    Array.init 256 (fun b ->
        if (b >= 33 && b <= 126) || (b >= 161 && b <= 172) || b >= 174 then utf8 b
        else
          let s = utf8 !extra in
          incr extra;
          s)
  in
  let unbytes = Hashtbl.create 256 in
  Array.iteri (fun i s -> Hashtbl.add unbytes s (Char.chr i)) bytes;
  { vocab; inverse; ranks; special; bytes; unbytes }

let letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let digit = function '0' .. '9' -> true | _ -> false
let space = function ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true | _ -> false
let newline c = c = '\r' || c = '\n'

let pretokenize s =
  String.iter
    (fun c ->
      if Char.code c >= 128 then invalid_arg "first version supports ASCII questions/prompts only")
    s;
  let n = String.length s in
  let matches i word =
    i + String.length word <= n
    && String.lowercase_ascii (String.sub s i (String.length word)) = word
  in
  let scan pred i =
    let j = ref i in
    while !j < n && pred s.[!j] do
      incr j
    done;
    !j
  in
  let rec loop i acc =
    if i = n then List.rev acc
    else
      let contraction =
        if s.[i] = '\'' then
          List.find_opt (matches i) [ "'s"; "'t"; "'re"; "'ve"; "'m"; "'ll"; "'d" ]
        else None
      in
      let j =
        match contraction with
        | Some c -> i + String.length c
        | None ->
            if letter s.[i] then scan letter i
            else if (not (newline s.[i] || digit s.[i])) && i + 1 < n && letter s.[i + 1] then
              scan letter (i + 1)
            else if digit s.[i] then i + 1
            else
              let punct c = not (space c || letter c || digit c) in
              let start = if s.[i] = ' ' then i + 1 else i in
              if start < n && punct s.[start] then scan newline (scan punct start)
              else
                let stop = scan space i in
                let last_nl = ref (-1) in
                for k = i to stop - 1 do
                  if newline s.[k] then last_nl := k
                done;
                if !last_nl >= i then !last_nl + 1
                else if stop = n then stop
                else if stop - i > 1 then stop - 1
                else stop
      in
      if j <= i then failwith "pretokenizer made no progress";
      loop j (String.sub s i (j - i) :: acc)
  in
  loop 0 []

let bpe t s =
  let initial = List.init (String.length s) (fun i -> t.bytes.(Char.code s.[i])) in
  let rec merge parts =
    let best = ref None in
    let rec pairs = function
      | a :: (b :: _ as rest) ->
          (match (Hashtbl.find_opt t.ranks (a, b), !best) with
          | Some rank, None -> best := Some (rank, a, b)
          | Some rank, Some (old, _, _) when rank < old -> best := Some (rank, a, b)
          | _ -> ());
          pairs rest
      | _ -> ()
    in
    pairs parts;
    match !best with
    | None -> parts
    | Some (_, a, b) ->
        let rec replace = function
          | x :: y :: rest when x = a && y = b -> (x ^ y) :: replace rest
          | x :: rest -> x :: replace rest
          | [] -> []
        in
        merge (replace parts)
  in
  List.map (fun s -> Hashtbl.find t.vocab s) (merge initial)

let encode t text =
  let n = String.length text in
  let starts i token =
    let len = String.length token in
    i + len <= n && String.sub text i len = token
  in
  let specials =
    List.sort (fun (a, _) (b, _) -> compare (String.length b) (String.length a)) t.special
  in
  let ordinary start stop =
    String.sub text start (stop - start) |> pretokenize |> List.concat_map (bpe t)
  in
  let rec loop start i acc =
    if i = n then List.rev_append acc (ordinary start i)
    else
      match List.find_opt (fun (s, _) -> starts i s) specials with
      | None -> loop start (i + 1) acc
      | Some (s, id) ->
          let acc = id :: List.rev_append (ordinary start i) acc in
          loop (i + String.length s) (i + String.length s) acc
  in
  Array.of_list (loop 0 0 [])

let decode t ids =
  let out = Buffer.create 256 in
  Array.iter
    (fun id ->
      (* Adding SimLingo's special tokens replaces the tokenizer's previous
       additional-special list. The image/box tokens remain added tokens,
       but upstream skip_special_tokens no longer removes them. *)
      if not (id = 151643 || id = 151645 || id >= 151655) then
        let token = Hashtbl.find t.inverse id in
        if List.mem_assoc token t.special then Buffer.add_string out token
        else glyphs token |> List.iter (fun s -> Buffer.add_char out (Hashtbl.find t.unbytes s)))
    ids;
  (* A token-limited answer can end midway through a UTF-8 code point.
     Match the upstream decoder's replacement behavior and keep JSON valid. *)
  let raw = Buffer.contents out in
  Buffer.clear out;
  let rec repair i =
    if i < String.length raw then (
      let d = String.get_utf_8_uchar raw i in
      Buffer.add_utf_8_uchar out (Uchar.utf_decode_uchar d);
      repair (i + Uchar.utf_decode_length d))
  in
  repair 0;
  Buffer.contents out
