
open Owl
open Owl_types
open Algodiff.S
open Neural 
open Neural.S 
open Neural.S.Graph

let model h w = 
  let nn = input [|h; w; 3|]
    (* block 1 *)
    |> conv2d [|3;3;3;64|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;64;64|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    (* block 2 *)
    |> conv2d [|3;3;64;128|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;128;128|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    (* block 3 *)
    |> conv2d [|3;3;128;256|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;256;256|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;256;256|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME
    |> conv2d [|3;3;256;256|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    (* block 4 *)
    |> conv2d [|3;3;256;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    (* block 5 *)
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME 
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME
    |> conv2d [|3;3;512;512|] [|1;1|] ~act_typ:Activation.Relu ~padding:SAME
    |> max_pool2d [|2;2|] [|2;2|] ~padding:VALID
    (* classification block *)
    (*
    |> flatten 
    |> fully_connected ~act_typ:Activation.Relu 4096
    |> fully_connected ~act_typ:Activation.Relu 4096
    |> fully_connected ~act_typ:Activation.Softmax classes *)
    |> global_max_pool2d (* ~act_typ:Activation.Softmax *)
    |> get_network
  in
  nn
