#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
open Owl;;
open Owl_plplot;;
#mod_use "book_regression.ml";;
Owl_stats_prng.init 80;;
Random.init 80;;
let book_checks=ref 0;;
let book_assert label value=if not value then failwith label;incr book_checks;;
let book_near label expected actual tol=book_assert label (Float.is_finite actual && abs_float(actual-.expected)<tol);;
let book_matrix_printer formatter value=Owl_pretty.print_dsnda ~max_row:6 ~max_col:6 ~formatter value;;
#install_printer book_matrix_printer;;

(* book: extract-data begin *)
let extract_data csv_file =
  let data = Owl_io.read_csv ~sep:',' csv_file in
  let data = Array.map (fun x ->
    Array.map float_of_string x) data
    |> Mat.of_arrays in
  let x = Mat.get_slice [[];[1]] data in
  let y = Mat.get_slice [[];[0]] data in
  x, y;;
(* book: extract-data end *)

(* book: linear-inputs begin *)
let x,y = extract_data "data_reg.csv";;
let h=Plot.create "regdata.png" in
Plot.scatter ~h ~spec:[ Marker "o"; MarkerSize 4. ] x y; Plot.set_xlabel h "population"; Plot.set_ylabel h "profit";Plot.output h;;
let h=Plot.create ~m:1 ~n:3 "reg_options.png" in
Plot.set_page_size h 1200 400;
Plot.set_font_size h 3.;
List.iteri (fun i f -> Plot.subplot h 0 i; Plot.set_font_size h 3.; Plot.scatter ~h ~spec:[ Marker "o"; MarkerSize 4. ] x y; Plot.plot_fun ~h f 0. 22.)
 [ (fun a -> a +. 2.); (fun a -> -.a +. 15.); (fun a -> 0.5 *. a +. 10.) ];
Plot.output h;;
(* book: linear-inputs end *)

(* book: linear-cost begin *)
let c x_data y_data theta0 theta1 =
  let f x = x *. theta1 +. theta0 in
  Mat.(pow_scalar (map f x_data - y_data) 2. |> mean') *. 0.5;;
(* book: linear-cost end *)

(* book: cost-surface begin *)
let plot_surface x_data y_data =
  let x, y = Mat.meshgrid (-20.) 10. (-20.) 10. 100 100 in
  let z = Mat.(map2 (c x_data y_data) x y) in
  let h = Plot.create ~m:1 ~n:2 "reg_cost.png" in
  Plot.subplot h 0 0;
  Plot.(mesh ~h ~spec:[ NoMagColor ] x y z);
  Plot.set_zlabel h "cost";
  Plot.subplot h 0 1;
  Plot.contour ~h x y z;
  Plot.output h;;
(* book: cost-surface end *)

(* book: cost-plot begin *)
plot_surface x y;;
(* book: cost-plot end *)

(* book: gradient-descent begin *)
let gradient_desc = Book_regression.gradient_desc;;
(* book: gradient-descent end *)

(* book: gradient-checks begin *)
let theta0,theta1 = gradient_desc x y;;
book_assert "gradient descent lowers cost" (c x y !theta0 !theta1 < c x y 10. 10.);;
let h=Plot.create "reg_gd.png" in
Plot.scatter ~h ~spec:[ Marker "o"; MarkerSize 4. ] x y;
Plot.plot_fun ~h (fun x -> !theta0 +. !theta1 *. x) 0. 22.;Plot.output h;;
(* book: gradient-checks end *)

(* book: ols-interface begin *)
#show Regression.D.ols;;
(* book: ols-interface end *)

(* book: ols-example begin *)
let theta = Regression.D.ols ~i:true x y;;
(* book: ols-example end *)

(* book: linear-checks begin *)
let design=Mat.concat_horizontal x (Mat.ones (Mat.row_num x) 1);;
let coefficients=Mat.(Linalg.D.pinv design *@ y);;
let owl_coefficients=Mat.concat_vertical theta.(0) theta.(1);;
book_assert "OLS residual near analytic reference" (Mat.(l2norm' (design *@ owl_coefficients - y)) < 1.001 *. Mat.(l2norm' (design *@ coefficients - y)));;
(* book: linear-checks end *)

(* book: multiple-regression begin *)
let multi_regression csv_file =
  let data = Owl_io.read_csv ~sep:',' csv_file in
  let data = Array.map
    (fun x -> Array.map float_of_string x) data
    |> Mat.of_arrays in
  let x = Mat.get_slice [[];[0; 1]] data in
  let y = Mat.get_slice [[];[2]] data in
  Regression.D.ols ~i:true x y;;
(* book: multiple-regression end *)

(* book: multiple-inputs begin *)
let data=Owl_io.read_csv ~sep:',' "data_mul.csv" |> Array.map (Array.map float_of_string) |> Mat.of_arrays;;
let x=Mat.get_slice [[];[0;1]] data and y=Mat.get_slice [[];[2]] data;;
let raw_theta=multi_regression "data_mul.csv";;
(* book: multiple-inputs end *)

(* book: normalise begin *)
let norm_ols data =
  let m = Arr.mean ~axis:0 data in
  let r = Arr.(sub (max ~axis:0 data) (min ~axis:0 data)) in
  let r = Arr.map (fun a -> if a = 0. then 1. else a) r in
  let data' = Arr.((data - m) / r) in
  let x' = Mat.get_slice [[];[0; 1]] data' in
  let y' = Mat.get_slice [[];[2]] data' in
  Regression.D.ols ~i:true x' y';;
(* book: normalise end *)

(* book: normalised-inputs begin *)
let data_mean=Arr.mean ~axis:0 data;;
let range=Arr.(max ~axis:0 data - min ~axis:0 data) |> Arr.map (fun r -> if r=0. then 1. else r);;
let data'=Arr.((data-data_mean)/range);;
let x'=Mat.get_slice [[];[0;1]] data' and y'=Mat.get_slice [[];[2]] data';;
let normalised_theta=norm_ols data;;
(* book: normalised-inputs end *)

(* book: normalised-data begin *)
data';;
(* book: normalised-data end *)

(* book: normal-equations begin *)
let o = Arr.ones [|(Arr.shape x).(0); 1|];;
let z = Arr.concatenate ~axis:1 [|o; x'|];;
let solution = Mat.dot (Mat.dot
    (Linalg.D.inv Mat.(dot (transpose z) z)) 
    (Mat.transpose z)) y';;
(* book: normal-equations end *)

(* book: normal-solution begin *)
solution;;
(* book: normal-solution end *)

(* book: normal-checks begin *)
let reference=Mat.(Linalg.D.pinv z *@ y');;
book_assert "normal equations match least squares" (Mat.(l2norm' (solution-reference)) < 1e-9);;
book_assert "normalised columns centered" (Arr.l2norm' (Arr.mean ~axis:0 data') < 1e-12);;
(* book: normal-checks end *)

(* book: boston-data begin *)
let f ?(csv_file="boston.csv") () =
  let data = Owl_io.read_csv ~sep:' ' csv_file in
  let data = Array.map
    (fun x -> Array.map float_of_string x) data
    |> Mat.of_arrays in
  let lstat = Mat.get_slice [[];[12]] data in
  let medv = Mat.get_slice [[];[13]] data in
  lstat, medv;;
(* book: boston-data end *)

(* book: boston-inputs begin *)
let data=Owl_io.read_csv ~sep:' ' "boston.csv" |> Array.map (Array.map float_of_string) |> Mat.of_arrays;;
let lstat,medv=f ();;
let h=Plot.create "boston.png" in Plot.scatter ~h lstat medv;Plot.output h;;
(* book: boston-inputs end *)

(* book: poly-interface begin *)
#show Regression.D.poly;;
(* book: poly-interface end *)

(* book: quadratic-poly begin *)
let poly lstat medv =
  let a = Regression.D.poly lstat medv 2 in
  let a0 = Mat.get a 0 0 in
  let a1 = Mat.get a 1 0 in
  let a2 = Mat.get a 2 0 in
  fun x -> a0 +. a1 *. x +. a2 *. x *. x;;
(* book: quadratic-poly end *)

(* book: poly-coefficients begin *)
Regression.D.poly lstat medv 2;;
(* book: poly-coefficients end *)

(* book: quadratic-plot begin *)
let h=Plot.create "reg_poly.png" in Plot.scatter ~h lstat medv;Plot.plot_fun ~h (poly lstat medv) 2. 38.;Plot.output h;;
(* book: quadratic-plot end *)

(* book: subset begin *)
let subdata, _ = Mat.draw_rows ~replacement:false data 50
let slstat=Mat.get_slice [[];[12]] subdata and smedv=Mat.get_slice [[];[13]] subdata
let subset_coefficients=Regression.D.poly slstat smedv 4;;
(* book: subset end *)

(* book: subset-inputs begin *)
let evaluate_poly a x = let result=ref 0. in for i=Mat.row_num a-1 downto 0 do result:= !result *. x +. Mat.get a i 0 done;!result;;
let h=Plot.create "reg_poly4s.png" in Plot.scatter ~h slstat smedv;Plot.plot_fun ~h (evaluate_poly subset_coefficients) 2. 36.;Plot.output h;;
(* book: subset-inputs end *)

(* book: polynomial-ridge begin *)
let poly_ridge = Book_regression.poly_ridge;;
(* book: polynomial-ridge end *)

(* book: ridge-checks begin *)
let regularised_coefficients=poly_ridge ~alpha:20. slstat smedv 4;;
let h=Plot.create "reg_poly4s_reg.png" in Plot.scatter ~h slstat smedv;Plot.plot_fun ~h (evaluate_poly regularised_coefficients) 2. 36.;Plot.output h;;
let fixture_x=Mat.of_array [|-2.;-1.;0.;1.;2.|] 5 1;;
let fixture_y=Mat.(fixture_x *$ 2. +$ 1.);;
let fitted=poly_ridge ~alpha:3. fixture_x fixture_y 1;;
book_near "ridge unpenalised intercept" 1. (Mat.get fitted 0 0) 1e-9;;
book_near "ridge analytic slope" (20. /. 13.) (Mat.get fitted 1 0) 1e-9;;
book_assert "ridge finite coefficients" (Array.for_all Float.is_finite (Mat.to_array regularised_coefficients));;
(* book: ridge-checks end *)

(* book: regularised-interfaces begin *)
#show Regression.D.ridge;;
#show Regression.D.lasso;;
#show Regression.D.elastic_net;;
(* book: regularised-interfaces end *)

(* book: classification-data begin *)
let generate_data () =
  let open Mat in
  let c = 500 in
  let x1 = (gaussian c 2 *$ 2.) in
  let a, b = float_of_int (Random.int 5),
    float_of_int (Random.int 5) in
  let x1 = map_at_col (fun x -> x +. a) x1 0 in
  let x1 = map_at_col (fun x -> x +. b) x1 1 in
  let x2 = (gaussian c 2 *$ 1.) in
  let a, b = float_of_int (Random.int 5),
    float_of_int (Random.int 5) in
  let x2 = map_at_col (fun x -> x -. a) x2 0 in
  let x2 = map_at_col (fun x -> x -. b) x2 1 in
  let y1 = create c 1 ( 1.) in
  let y2 = create c 1 ( 0.)in
  let x = concat_vertical x1 x2 in
  let y = concat_vertical y1 y2 in
  x, y

let x, y = generate_data ();;
(* book: classification-data end *)

(* book: logistic-fit begin *)
let theta =
    Book_regression.logistic ~i:true x y;;
(* book: logistic-fit end *)

(* book: logistic-checks begin *)
let scores=Mat.(x *@ theta.(0) +$ (get theta.(1) 0 0));;
let probability=Mat.sigmoid scores;;
let predicted=Mat.map (fun p -> if p>0.5 then 1. else 0.) probability;;
let training_accuracy=1. -. Mat.(mean' (abs (predicted-y)));;
book_assert "logistic exceeds balanced-class baseline by ten points" (training_accuracy > 0.6);;
let training_log_loss = Mat.(mean' (neg (y * log probability + (scalar_sub 1. y) * log (scalar_sub 1. probability))));;
book_assert "binary cross entropy beats constant model" (training_log_loss < log 2.);;
book_assert "logistic probabilities finite" (Array.for_all Float.is_finite (Mat.to_array probability));;
let h=Plot.create "sigmoid.pdf" in Plot.plot_fun ~h Owl_maths.sigmoid (-6.) 6.;Plot.output h;;
(* book: logistic-checks end *)

(* book: logistic-accuracy-function begin *)
let test_log p' x y =
  let p = Mat.(p'.(0) @= p'.(1)) in
  let x = Mat.(concat_horizontal x (ones (row_num x) 1)) in
  let y' = Mat.(sigmoid (x *@ p)) in
  let y' = Mat.map (fun x -> if x > 0.5 then 1. else 0.) y' in
  let e = Mat.((mean' (abs (y - y')))) in
  Printf.printf "training accuracy: %.4f\n" (1. -. e);;
(* book: logistic-accuracy-function end *)

(* book: logistic-accuracy begin *)
test_log theta x y;;
(* book: logistic-accuracy end *)

(* book: logistic-plot begin *)
open Owl

let data = Mat.concat_horizontal x y
let f x =
  let w0=Mat.get theta.(0) 0 0 and w1=Mat.get theta.(0) 1 0 in
  let b=Mat.get theta.(1) 0 0 in
  -. (w0 *. x +. b) /. w1

let plot_logistic data =
  let neg_idx = Mat.filter_rows
    (fun m -> Mat.get m 0 2 = 0.) data in
  let neg_data = Mat.get_fancy
    [ L (Array.to_list neg_idx); R [] ] data in
  let pos_idx = Mat.filter_rows
    (fun m -> Mat.get m 0 2 = 1.) data in
  let pos_data = Mat.get_fancy
    [ L (Array.to_list pos_idx); R [] ] data in
  let h = Plot.create "reg_logistic.png" in
  Plot.(scatter ~h ~spec:[ Marker "#[0x2217]"; MarkerSize 5. ]
    (Mat.get_slice [[];[0]] neg_data)
    (Mat.get_slice [[];[1]] neg_data));
  Plot.(scatter ~h ~spec:[ Marker "#[0x2295]"; MarkerSize 5. ]
    (Mat.get_slice [[];[0]] pos_data)
    (Mat.get_slice [[];[1]] pos_data));
  Plot.plot_fun ~h ~spec:[ RGB (0,0,255); LineWidth 2. ] 
    f (-5.) 5.;
  Plot.output h;;
plot_logistic data;;
(* book: logistic-plot end *)

(* book: svm-fit begin *)
let theta =
  let svm_y = Mat.(y *$ 2. -$ 1.) in
  Regression.D.svm ~i:true x svm_y;;
(* book: svm-fit end *)

(* book: svm-checks begin *)
let scores=Mat.(x *@ theta.(0) +$ (get theta.(1) 0 0));;
let predicted=Mat.map (fun p -> if p>0. then 1. else 0.) scores;;
book_assert "SVM exceeds balanced-class baseline by ten points" (1. -. Mat.(mean' (abs (predicted-y))) > 0.6);;
let h=Plot.create "reg_svm.png" in
let labelled=Mat.concat_horizontal x y in
let group label = let idx=Mat.filter_rows (fun row -> Mat.get row 0 2 = label) labelled in
  Mat.get_fancy [L (Array.to_list idx);R []] labelled in
let negative=group 0. and positive=group 1. in
Plot.scatter ~h ~spec:[Marker "#[0x2217]";MarkerSize 5.] (Mat.col negative 0) (Mat.col negative 1);
Plot.scatter ~h ~spec:[Marker "#[0x2295]";MarkerSize 5.] (Mat.col positive 0) (Mat.col positive 1);
Plot.plot_fun ~h (fun a -> -. (Mat.get theta.(1) 0 0 +. Mat.get theta.(0) 0 0 *. a) /. Mat.get theta.(0) 1 0) (-5.) 5.;Plot.output h;;
Printf.printf "PASS: %d regression property checks\n" !book_checks;;
(* book: svm-checks end *)
