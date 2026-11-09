#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
open Owl;;
open Owl_plplot;;
Owl_stats_prng.init 81;;
Random.init 81;;
let book_checks=ref 0;;
let book_assert label value=if not value then failwith label;incr book_checks;;
let book_matrix_printer formatter value=Owl_pretty.print_dsnda ~max_row:4 ~max_col:10 ~formatter value;;
#install_printer book_matrix_printer;;

(* book: mnist-shapes begin *)
let x, _, y = Dataset.load_mnist_train_data_arr ();;
let x_shape, y_shape =
   Dense.Ndarray.S.shape x, Dense.Ndarray.S.shape y;;
(* book: mnist-shapes end *)

(* book: mnist-labels begin *)
y;;
(* book: mnist-labels end *)

(* book: two-layer begin *)
open Algodiff.D
module N = Dense.Ndarray.D

let input_size = 28 * 28
let hidden_size = 25
let classes = 10

let theta0 = Arr (N.uniform ~a:(-0.075) ~b:0.075 [|input_size; hidden_size|])
let theta1 = Arr (N.uniform ~a:(-0.075) ~b:0.075 [|hidden_size; classes|])

let h theta0 theta1 x =
  let t = Arr.dot x theta0 |> Maths.sigmoid in
  Arr.dot t theta1 |> Maths.sigmoid;;
(* book: two-layer end *)

(* book: binary-loss begin *)
let c t0 t1 x y =
  let z = h t0 t1 x in
  Maths.add
    (Maths.cross_entropy y z)
    (Maths.cross_entropy Arr.(sub (ones (shape y)) y)
       Arr.(sub (ones (shape z)) z))
  |> fun loss -> Maths.(loss / F (float_of_int (Mat.row_num y)));;
(* book: binary-loss end *)

(* book: first-backprop begin *)
let images,_,labels=Dataset.load_mnist_train_data ();;
let x',y'=Dataset.draw_samples images labels 1;;
let x'=Dense.Ndarray.Generic.cast_s2d x' |> fun x -> N.div_scalar x 255.;;
let y'=Dense.Ndarray.Generic.cast_s2d y';;
let t=tag ();;
let t0=make_reverse theta0 t and t1=make_reverse theta1 t;;
let cost=c t0 t1 (Arr x') (Arr y');;
reverse_prop (F 1.) cost;;
let theta0'=adjval t0 |> unpack_arr;;
let theta1'=adjval t1 |> unpack_arr;;
(* book: first-backprop end *)

(* book: first-checks begin *)
book_assert "two-layer mean loss finite" (Float.is_finite (unpack_flt cost));;
book_assert "first weight gradient shape" (N.shape theta0'=[|784;25|]);;
book_assert "second weight gradient shape" (N.shape theta1'=[|25;10|]);;
book_assert "first weight gradient finite" (Array.for_all Float.is_finite (N.to_array theta0'));;
(* book: first-checks end *)

(* book: layer-types begin *)
open Algodiff.S

type layer = {
  mutable w : t;
  mutable b : t;
  mutable a : t -> t;
}

type network = { layers : layer array };;
(* book: layer-types end *)

(* book: layer-run begin *)
let run_layer x l = Maths.((x *@ l.w) + l.b) |> l.a
let run_network x nn = Array.fold_left run_layer x nn.layers;;
(* book: layer-run end *)

(* book: initial-layers begin *)
let l0 = {
  w = Maths.(Mat.uniform 784 40 * F 0.15 - F 0.075);
  b = Mat.zeros 1 40;
  a = Maths.tanh;
}
let l1 = {
  w = Maths.(Mat.uniform 40 10 * F 0.15 - F 0.075);
  b = Mat.zeros 1 10;
  a = Maths.softmax ~axis:1;
}
let nn = {layers = [|l0; l1|]};;
let handwritten_nn=nn;;
(* book: initial-layers end *)

(* book: layer-loss begin *)
let loss_fun nn x y =
  let t = tag () in
  Array.iter (fun l ->
    l.w <- make_reverse (primal' l.w) t;
    l.b <- make_reverse (primal' l.b) t;
  ) nn.layers;
  Maths.(cross_entropy y (run_network x nn) /
    (F (Mat.row_num y |> float_of_int)));;
(* book: layer-loss end *)

(* book: layer-backprop begin *)
let backprop nn eta x y =
  let loss = loss_fun nn x y in
  reverse_prop (F 1.) loss;
  Array.iter (fun l ->
    l.w <- Maths.((primal l.w) -
      (eta * (adjval l.w))) |> primal;
    l.b <- Maths.((primal l.b) -
      (eta * (adjval l.b))) |> primal;
  ) nn.layers;
  loss |> unpack_flt;;
(* book: layer-backprop end *)


(* book: backprop-regression begin *)
let tiny_layer={w=Arr(Dense.Matrix.S.of_array [|0.1;-.0.2;0.3;0.1|] 2 2);b=Mat.zeros 1 2;a=Maths.softmax ~axis:1};;
let tiny_nn={layers=[|tiny_layer|]};;
let fixture_x=Arr(Dense.Matrix.S.of_array [|0.2;0.7;0.9;0.1|] 2 2);;
let fixture_y=Arr(Dense.Matrix.S.of_array [|1.;0.;0.;1.|] 2 2);;
let objective ()=Maths.(cross_entropy fixture_y (run_network fixture_x tiny_nn) / F 2.) |> unpack_flt;;
let before=objective ();;
let loss=loss_fun tiny_nn fixture_x fixture_y;;
reverse_prop (F 1.) loss;;
let analytic=adjval tiny_layer.w |> unpack_arr |> fun a -> Dense.Matrix.S.get a 0 0;;
tiny_layer.w<-primal' tiny_layer.w;; tiny_layer.b<-primal' tiny_layer.b;;
let w=unpack_arr tiny_layer.w;;
let old=Dense.Matrix.S.get w 0 0;;
Dense.Matrix.S.set w 0 0 (old+.0.001);;
let plus=objective ();;
Dense.Matrix.S.set w 0 0 (old-.0.001);;
let minus=objective ();;
Dense.Matrix.S.set w 0 0 old;;
book_assert "backprop agrees with finite difference" (abs_float (analytic-.(plus-.minus)/.0.002)<5e-4);;
for _=1 to 5 do ignore(backprop tiny_nn (F 0.1) fixture_x fixture_y) done;;
book_assert "backprop lowers fixture loss" (objective () < before);;
(* book: backprop-regression end *)

(* book: layer-test begin *)
let test nn x y =
  Dense.Matrix.S.iter2_rows (fun u v ->
    let p = run_network (Arr u) nn |> unpack_arr in
    Dense.Matrix.Generic.print p;
    let _, prediction = Dense.Matrix.Generic.max_i p in
    let _, label = Dense.Matrix.Generic.max_i v in
    Printf.printf "prediction: %i label: %i correct: %b\n"
      prediction.(1) label.(1) (prediction.(1) = label.(1))
  ) (unpack_arr x) (unpack_arr y);;
(* book: layer-test end *)

(* book: handwritten-training begin *)
let main () =
  let x, _, y = Dataset.load_mnist_train_data () in
  let x=Dense.Matrix.S.div_scalar x 255. in
  for i = 1 to 999 do
    let x', y' = Dataset.draw_samples x y 100 in
    backprop nn (F 0.01) (Arr x') (Arr y')
    |> Owl_log.info "#%03i : loss = %g" i
  done;
  let x, _, y = Dataset.load_mnist_test_data () in
  let x=Dense.Matrix.S.div_scalar x 255. in
  let x, y = Dataset.draw_samples x y 10 in
  test nn (Arr x) (Arr y);;
main ();;
(* book: handwritten-training end *)

(* book: handwritten-checks begin *)
let images,_,labels=Dataset.load_mnist_test_data ();;
let images=Dense.Matrix.S.div_scalar images 255.;;
let predicted=run_network (Arr images) handwritten_nn |> unpack_arr;;
let label_of_row x=Dense.Matrix.S.max_rows x |> Array.map (fun (_,_,i)->i);;
let predicted_labels=label_of_row predicted and true_labels=label_of_row labels;;
let correct=ref 0;;
Array.iteri (fun i p -> if p=true_labels.(i) then incr correct) predicted_labels;;
let handwritten_accuracy=float_of_int !correct /. 10000.;;
book_assert "handwritten test accuracy beats random" (handwritten_accuracy>0.7);;




(* book: handwritten-checks end *)

(* book: activation-plot begin *)
let h=Plot.create ~m:1 ~n:3 "activations.png" in
Plot.set_page_size h 1200 350;
List.iteri (fun i (name,f) -> Plot.subplot h 0 i;Plot.set_title h name;
 Plot.plot_fun ~h ~spec:[Plot.RGB(0,100,220);Plot.LineWidth 2.] f (-10.) 10.)
 ["tanh",Owl_maths.tanh;"relu",(fun x->Stdlib.max 0. x);"softsign",(fun x->x/.(1.+.abs_float x))];
Plot.output h;;
(* book: activation-plot end *)

(* book: prediction-plot begin *)
let first_image=Dense.Matrix.S.row images 0 |> fun a -> Dense.Ndarray.S.reshape a [|28;28|] |> Dense.Ndarray.Generic.cast_s2d;;
let first_probabilities=Dense.Matrix.S.row predicted 0 |> Dense.Matrix.Generic.cast_s2d;;
let h=Plot.create ~m:1 ~n:2 "prediction.png" in
Plot.set_page_size h 1000 450;
Plot.subplot h 0 0;Plot.set_title h (Printf.sprintf "MNIST label %d" true_labels.(0));
Plot.image ~h (Dense.Matrix.D.mul_scalar first_image 255.);
Plot.set_page_size h 1000 450;
Plot.subplot h 0 1;Plot.set_title h (Printf.sprintf "Prediction %d" predicted_labels.(0));
Plot.set_xlabel h "class";Plot.set_ylabel h "probability";
Plot.set_yrange h 0. 1.;
Plot.stem ~h (Dense.Matrix.D.sequential 1 10) first_probabilities;Plot.output h;;
(* book: prediction-plot end *)

(* book: neuron-context begin *)
open Neural.S;;
open Neural.S.Graph;;
open Neural.S.Graph.Neuron;;
open Neural.S.Algodiff;;
module FullyConnected=Neural.S.Graph.Neuron.FullyConnected;;
open FullyConnected;;
(* book: neuron-context end *)

(* book: neuron-type begin *)
#show_type FullyConnected.neuron_typ;;
(* book: neuron-type end *)

(* book: neuron-create begin *)
let create ?inputs o init_typ =
  let in_shape =
    match inputs with
    | Some i -> [| i |]
    | None   -> [| 0 |]
  in
  { w = Mat.empty 0 o; b = Mat.empty 1 o;
    init_typ; in_shape; out_shape = [| o |] };;
(* book: neuron-create end *)

(* book: neuron-connect begin *)
let connect out_shape l =
  assert (Array.length out_shape > 0);
  l.in_shape <- Array.copy out_shape;;
(* book: neuron-connect end *)

(* book: neuron-init begin *)
let init l =
  let m = Array.fold_left (fun a b -> a * b) 1 l.in_shape in
  let n = l.out_shape.(0) in
  l.w <- Init.run l.init_typ [| m; n |] l.w;
  l.b <- Mat.zeros 1 n;;
(* book: neuron-init end *)

(* book: neuron-run begin *)
let run x l =
  let m = Mat.row_num l.w in
  let n = Arr.numel x / m in
  let x = Maths.reshape x [| n; m |] in
  let y = Maths.((x *@ l.w) + l.b) in
  y;;
(* book: neuron-run end *)

(* book: neuron-parameters begin *)
let mktag t l =
  l.w <- make_reverse l.w t;
  l.b <- make_reverse l.b t

let mkpri l = [| primal l.w; primal l.b |]

let mkadj l = [| adjval l.w; adjval l.b |]

let update l u =
  l.w <- u.(0) |> primal';
  l.b <- u.(1) |> primal';;
(* book: neuron-parameters end *)

(* book: neuron-checks begin *)
let layer=create ~inputs:3 2 Init.Standard;;
connect [|3|] layer;;
init layer;;
let result=run (Mat.ones 4 3) layer;;
book_assert "neuron output shape" (Arr.shape result=[|4;2|]);;
mktag (tag ()) layer;;
let objective=run (Mat.ones 4 3) layer |> Maths.sum';;
reverse_prop (F 1.) objective;;
let gradients=mkadj layer;;
book_assert "neuron bias gradient" (let g=unpack_arr gradients.(1) in abs_float (Dense.Matrix.S.get g 0 0 -. 4.)<1e-5);;
update layer (mkpri layer);;
open Neural.S.Graph;;
(* book: neuron-checks end *)

(* book: node-type begin *)
#show_type Neural.S.Graph.node;;
(* book: node-type end *)

(* book: graph-layer begin *)
let fully_connected ?name ?(init_typ = Init.Standard) ?act_typ
  outputs input_node =
  let neuron = FullyConnected
    (FullyConnected.create outputs init_typ) in
  let nn = get_network input_node in
  let n = make_node ?name [||] [||] neuron None nn in
  add_node ?act_typ nn [| input_node |] n;;
(* book: graph-layer end *)

(* book: graph-network begin *)
open Neural.S
open Neural.S.Graph
open Neural.S.Algodiff

let make_network () =
  input [|28; 28; 1|]
  |> fully_connected 40 ~act_typ:Activation.Tanh
  |> linear 10 ~act_typ:Activation.(Softmax 1)
  |> get_network;;
(* book: graph-network end *)



(* book: batch begin *)
let network=make_network ();;
Graph.init network;;
let raw_x,_,raw_y=Dataset.load_mnist_train_data ();;
let small_x=Dense.Matrix.S.get_slice [[0;99];[]] raw_x |> fun x -> Dense.Matrix.S.div_scalar x 255.;;
let small_y=Dense.Matrix.S.get_slice [[0;99];[]] raw_y;;
let x=Arr (Dense.Ndarray.S.reshape small_x [|100;28;28;1|]) and y=Arr small_y;;
let params=Params.config ~batch:(Batch.Mini 10) ~regularisation:(Regularisation.L2norm 0.001) 1.;;
let batch_fun=Optimise.Batch.run params.batch;;
let i=0;;
let forward=Graph.forward network;;
let loss_fun=Optimise.Loss.run params.loss;;
let regl_fun=Optimise.Regularisation.run params.regularisation;;
module Regularisation=Optimise.Regularisation;;

let xt, yt = batch_fun x y i;;
(* book: batch end *)

(* book: graph-forward begin *)
let yt', ws = forward xt;;
(* book: graph-forward end *)

(* book: graph-loss begin *)
let loss = loss_fun yt yt'
let loss = Maths.(loss / _f (Mat.row_num yt |> float_of_int));;
(* book: graph-loss end *)

(* book: graph-regularisation begin *)
let reg =
  match params.regularisation <> Regularisation.None with
  | true  -> Owl_utils.aarr_fold
    (fun a w -> Maths.(a + regl_fun w)) (F 0.) ws
  | false -> _f 0.
let loss = Maths.(loss + reg);;
(* book: graph-regularisation end *)

(* book: optimiser-checks begin *)
book_assert "graph loss finite" (Float.is_finite (unpack_flt loss));;
let _=Graph.backward network loss;;
(* book: optimiser-checks end *)

(* book: graph-train begin *)
let train () =
  let x, _, y = Dataset.load_mnist_train_data_arr () in
  let x=Dense.Ndarray.S.div_scalar x 255. in
  let network = make_network () in
  let params = Params.config
    ~batch:(Batch.Mini 100)
    ~learning_rate:(Learning_Rate.Adagrad 0.005) 0.1
  in
  Graph.train ~params network x y |> ignore;
  network;;
let trained_network=train ();;
(* book: graph-train end *)

(* book: graph-test begin *)
let test network =
  let imgs, _, labels = Dataset.load_mnist_test_data () in
  let imgs=Dense.Matrix.S.div_scalar imgs 255. in
  let m = Dense.Matrix.S.row_num imgs in
  let imgs = Dense.Ndarray.S.reshape imgs [|m;28;28;1|] in

  let mat2num x = Dense.Matrix.S.of_array (
      x |> Dense.Matrix.Generic.max_rows
        |> Array.map (fun (_,_,num) -> float_of_int num)
    ) 1 m
  in

  let pred = mat2num (Graph.model network imgs) in
  let fact = mat2num labels in
  let accu = Dense.Matrix.S.(elt_equal pred fact |> sum') in
  Owl_log.info "Accuracy on test set: %f"
    (accu /. (float_of_int m));
  accu /. float_of_int m;;
let graph_accuracy=test trained_network;;
(* book: graph-test end *)

(* book: cnn begin *)
let make_network input_shape =
  input input_shape
  |> lambda (fun x -> Maths.(x / F 256.))
  |> conv2d [|5;5;1;32|] [|1;1|] ~act_typ:Activation.Relu
  |> max_pool2d [|2;2|] [|2;2|]
  |> dropout 0.1
  |> fully_connected 1024 ~act_typ:Activation.Relu
  |> linear 10 ~act_typ:Activation.(Softmax 1)
  |> get_network;;
(* book: cnn end *)

(* book: cnn-checks begin *)
let cnn=make_network [|28;28;1|];;
Graph.init cnn;;
let pixels=Dense.Ndarray.S.get_slice [[0;1];[]] raw_x |> fun x -> Dense.Ndarray.S.reshape x [|2;28;28;1|];;
let prediction=Graph.model cnn pixels;;
book_assert "CNN output shape" (Dense.Ndarray.S.shape prediction=[|2;10|]);;
book_assert "CNN probabilities sum to one" (Dense.Ndarray.S.sum ~axis:1 prediction |> Dense.Ndarray.S.to_array |> Array.for_all (fun p -> abs_float(p-.1.)<1e-5));;
book_assert "CNN finite output" (Array.for_all Float.is_finite (Dense.Ndarray.S.to_array prediction));;
(* book: cnn-checks end *)

(* book: rnn begin *)
open Neural.S
open Neural.S.Graph

let make_network wndsz vocabsz =
  input [|wndsz|]
  |> embedding vocabsz 40
  |> lstm 128
  |> linear 512 ~act_typ:Activation.Relu
  |> linear vocabsz ~act_typ:Activation.(Softmax 1)
  |> get_network;;
(* book: rnn end *)

(* book: rnn-checks begin *)
let rnn=make_network 5 20;;
Graph.init rnn;;
let tokens=Dense.Ndarray.S.of_array [|0.;1.;2.;3.;4.;5.;6.;7.;8.;9.|] [|2;5|];;
let prediction=Graph.model rnn tokens;;
book_assert "RNN output shape" (Dense.Ndarray.S.shape prediction=[|2;20|]);;
book_assert "RNN probabilities sum to one" (Dense.Ndarray.S.sum ~axis:1 prediction |> Dense.Ndarray.S.to_array |> Array.for_all (fun p -> abs_float(p-.1.)<1e-5));;
book_assert "RNN finite output" (Array.for_all Float.is_finite (Dense.Ndarray.S.to_array prediction));;
(* book: rnn-checks end *)

(* book: generator begin *)
open Neural.S
open Neural.S.Graph
open Neural.S.Algodiff

let make_generator input_shape =
  let out_size = Owl_utils_array.fold_left ( * ) 1
    input_shape in
  input input_shape
  |> fully_connected 256 ~act_typ:(Activation.LeakyRelu 0.2)
  |> normalisation ~decay:0.8
  |> linear 512 ~act_typ:(Activation.LeakyRelu 0.2)
  |> normalisation ~decay:0.8
  |> linear 1024 ~act_typ:(Activation.LeakyRelu 0.2)
  |> normalisation ~decay:0.8
  |> linear out_size ~act_typ:Activation.Tanh
  |> reshape input_shape
  |> get_network;;
(* book: generator end *)

(* book: discriminator begin *)
let make_discriminator input_shape =
  input input_shape
  |> fully_connected 512 ~act_typ:(Activation.LeakyRelu 0.2)
  |> linear 256 ~act_typ:(Activation.LeakyRelu 0.2)
  |> linear 1 ~act_typ:Activation.Sigmoid
  |> get_network;;
(* book: discriminator end *)

(* book: gan-checks begin *)
let generator=make_generator [|28;28;1|] and discriminator=make_discriminator [|28;28;1|];;
Graph.init generator;; Graph.init discriminator;;
let noise=Dense.Ndarray.S.uniform ~a:(-1.) ~b:1. [|2;28;28;1|];;
let generated=Graph.model generator noise;;
let prediction=Graph.model discriminator generated;;
book_assert "generator shape" (Dense.Ndarray.S.shape generated=[|2;28;28;1|]);;
book_assert "discriminator shape" (Dense.Ndarray.S.shape prediction=[|2;1|]);;
book_assert "generator tanh range" (Dense.Ndarray.S.to_array generated |> Array.for_all (fun p -> p>= -.1.00001 && p<=1.00001));;
book_assert "discriminator probability range" (Dense.Ndarray.S.to_array prediction |> Array.for_all (fun p -> p>=0. && p<=1.));;
book_assert "GAN finite output" (Array.for_all Float.is_finite (Dense.Ndarray.S.to_array prediction));;
Printf.printf "PASS: %d neural-network property checks\n" !book_checks;;
(* book: gan-checks end *)

(* book: metrics begin *)
let channel=open_out "metrics.json" in
Printf.fprintf channel "{\"handwritten_accuracy\":%.17g,\"graph_accuracy\":%.17g,\"sample_label\":%d,\"sample_prediction\":%d,\"sample_confidence\":%.17g}\n"
 handwritten_accuracy graph_accuracy true_labels.(0) predicted_labels.(0)
 (Dense.Matrix.D.get first_probabilities 0 predicted_labels.(0));
close_out channel;;
(* book: metrics end *)
