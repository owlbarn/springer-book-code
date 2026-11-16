module A = Owl.Dense.Ndarray.S
open Yojson.Basic.Util

type t = {
  root : string;
  entries : (string * Yojson.Basic.t) list;
  cache : (string, A.arr) Hashtbl.t;
}

let load root =
  let manifest = Yojson.Basic.from_file (Filename.concat root "manifest.json") in
  if manifest |> member "format_version" |> to_int <> 1 then failwith "unsupported weights format";
  { root; entries = manifest |> member "tensors" |> to_assoc; cache = Hashtbl.create 700 }

let has t name = List.mem_assoc name t.entries

let get t name =
  match Hashtbl.find_opt t.cache name with
  | Some a -> a
  | None ->
      let entry =
        try List.assoc name t.entries with Not_found -> failwith ("missing weight: " ^ name)
      in
      let file = entry |> member "file" |> to_string in
      if Filename.basename file <> file then failwith "invalid weight filename";
      let path = Filename.concat t.root file in
      let expected = entry |> member "sha256" |> to_string in
      if Sha256.(to_hex (file_fast path)) <> expected then failwith ("checksum mismatch: " ^ name);
      if entry |> member "dtype" |> to_string <> "float32" then
        failwith ("unsupported dtype: " ^ name);
      let a = A.load_npy path in
      let shape = entry |> member "shape" |> to_list |> List.map to_int |> Array.of_list in
      if A.shape a <> shape then failwith ("shape mismatch: " ^ name);
      Hashtbl.add t.cache name a;
      a

let expect t name shape =
  let a = get t name in
  if A.shape a <> shape then failwith ("model shape mismatch: " ^ name);
  a
