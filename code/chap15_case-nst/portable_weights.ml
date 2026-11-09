open Owl
open Neural.S
open Graph
let update_node_weights layers (node:Graph.node) =
  let arrays=Hashtbl.find layers node.name in
  Neuron.load_weights node.neuron (Array.map Algodiff.pack_arr arrays)



let load_portable_weights (network:Graph.network) directory =
  let table=Hashtbl.create network.size in
  Owl_io.iteri_lines_of_file (fun _ line ->
    match String.split_on_char '\t' line with
    | name::files ->
      if Hashtbl.mem table name then failwith "Duplicate portable node";
      Hashtbl.add table name (Array.of_list files)
    | [] -> failwith "Empty portable node") (Filename.concat directory "weights.tsv");
  if Hashtbl.length table<>network.size then failwith "Weight/network node count differs";
  Array.iter (fun node ->
    let files=Hashtbl.find table node.name in
    let arrays=Array.map (fun file -> Dense.Ndarray.S.load_npy (Filename.concat directory file)) files in
    let layers=Hashtbl.create 1 in Hashtbl.add layers node.name arrays;
    update_node_weights layers node;
    let loaded=Neuron.save_weights node.neuron in
    if Array.length loaded<>Array.length arrays then failwith "Weight count differs";
    Array.iteri (fun i value ->
      let actual=Algodiff.unpack_arr value in
      if Dense.Ndarray.S.shape actual<>Dense.Ndarray.S.shape arrays.(i) ||
         Dense.Ndarray.S.to_array actual<>Dense.Ndarray.S.to_array arrays.(i)
      then failwith "Portable parameter changed during loading") loaded) network.topo


