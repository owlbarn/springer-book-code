open Owl
open CGraph.Graph
module N=Dense.Ndarray.S
let load (network:network) directory =
  let used=Hashtbl.create 700 in
  let tensors=ref 0 and parameters=ref 0 in
  let get name suffix =
    let key=name^suffix in Hashtbl.replace used key ();
    let array=N.load_npy (Filename.concat directory (key^".npy")) in
    incr tensors;parameters:= !parameters+N.numel array;array in
  Array.iter (fun node ->
    let kind=Neuron.to_name node.neuron in
    let len=String.length node.name in
    let name=if len>=6 && String.sub node.name 0 3="rpn" && String.sub node.name (len-3) 2="_p"
      then String.sub node.name 0 (len-3) else node.name in
    let arrays=match kind with
      | "conv2d" | "transpose_conv2d" | "dilated_conv2d" -> [|get name "kernel:0";get name "bias:0"|]
      | "normalisation" ->
        Array.map (fun suffix -> let a=get name suffix in N.reshape a [|1;1;1;N.numel a|])
          [|"beta:0";"gamma:0";"moving_mean:0";"moving_variance:0"|]
      | "linear" ->
        let weights=get name "kernel:0" and bias=get name "bias:0" in
        [|weights;N.reshape bias [|1;N.numel bias|]|]
      | _ ->
        if Array.length (Neuron.save_weights node.neuron)>0 then failwith ("Unsupported parameter node: "^kind);
        [||] in
    if Array.length arrays>0 then begin
      Neuron.load_weights node.neuron (Array.map MrcnnUtil.pack arrays);
      let loaded=Neuron.save_weights node.neuron in
      if Array.length arrays<>Array.length loaded then failwith "Parameter count differs";
      Array.iteri (fun i value ->
        let value=MrcnnUtil.unpack value in
        if N.shape value<>N.shape arrays.(i) || N.to_array value<>N.to_array arrays.(i)
        then failwith ("Parameter mismatch in "^node.name)) loaded
    end) network.topo;
  if Hashtbl.length used<>690 then failwith (Printf.sprintf "Expected all 690 original datasets, loaded %d" (Hashtbl.length used));
  !tensors,!parameters
