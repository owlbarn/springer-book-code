open Owl
(* book: black-scholes begin *)
let cdf x = Stats.gaussian_cdf ~mu:0. ~sigma:1. x
let black_scholes r s k t sigma =
  if s<=0. || k<=0. || t<0. || sigma<0. then invalid_arg "Invalid option inputs";
  if t=0. then max 0. (s-.k)
  else if sigma=0. then max 0. (s-.k*.exp(-.r*.t))
  else
    let d1 = (log(s/.k) +. (r +. 0.5*.sigma*.sigma)*.t) /. (sigma*.sqrt t) in
    let d2 = d1 -. sigma*.sqrt t in
    cdf d1 *. s -. cdf d2 *. k *. exp(-.r*.t)
(* book: black-scholes end *)

(* book: network begin *)
open Neural.S
open Neural.S.Graph
open Neural.S.Algodiff
let make_network input_shape =
  input input_shape
  |> lambda (fun x -> Maths.(x / F 256.))
  |> conv2d [|5;5;1;32|] [|1;1|] ~act_typ:Activation.Relu
  |> max_pool2d [|2;2|] [|2;2|]
  |> dropout 0.1
  |> fully_connected 1024 ~act_typ:Activation.Relu
  |> linear 10 ~act_typ:Activation.(Softmax 1)
  |> get_network
(* book: network end *)
