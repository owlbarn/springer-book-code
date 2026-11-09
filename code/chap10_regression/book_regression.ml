open Owl

(* book: gradient-descent begin *)
let gradient_desc x y =
  let alpha = 0.01 in
  let theta0 = ref 10. and theta1 = ref 10. in
  for _ = 0 to 500 do
    let residual = Mat.(map (fun x -> x *. !theta1 +. !theta0) x - y) in
    let d0 = Mat.mean' residual in
    let d1 = Mat.(mean' (residual * x)) in
    theta0 := !theta0 -. alpha *. d0;
    theta1 := !theta1 -. alpha *. d1
  done;
  theta0, theta1
(* book: gradient-descent end *)

(* book: binary-logistic begin *)
let logistic ?(i=false) x y =
  let open Optimise.D in
  let open Algodiff in
  let binary_loss y z =
    Maths.(sum' (max2 z (F 0.) - y * z +
                 log (F 1. + exp (neg (abs z))))) in
  let params = Params.config
    ~batch:Batch.Full ~gradient:Gradient.GD
    ~learning_rate:(Learning_Rate.Adagrad 1.)
    ~loss:(Loss.Custom binary_loss) ~verbosity:false
    ~stopping:(Stopping.Const 1e-8) 1000. in
  Regression.D._linear_reg i params x y
(* book: binary-logistic end *)

(* book: polynomial-ridge begin *)
let poly_ridge ~alpha x y n =
  let open Optimise.D in
  let open Algodiff in
  if alpha < 0. || n < 1 then invalid_arg "poly_ridge";
  let z = Array.init (n + 1) (fun i -> A.pow_scalar x (float_of_int i))
          |> A.concatenate ~axis:1 in
  let objective parameters =
    let w = Maths.transpose parameters in
    let slopes = Maths.get_slice [[1;n];[]] w in
    Maths.((sum' (sqr (Arr z *@ w - Arr y)) +
            F alpha * sum' (sqr slopes)) /
           F (2. *. float_of_int (A.row_num x))) in
  let params = Params.config ~batch:Batch.Full
    ~gradient:Gradient.Newton ~learning_rate:(Learning_Rate.Const 1.)
    ~verbosity:false ~stopping:(Stopping.Const 1e-16) 100. in
  let w = Arr (A.zeros [|1;n+1|]) in
  minimise_fun params objective w |> snd |> unpack_arr |> A.transpose
(* book: polynomial-ridge end *)
