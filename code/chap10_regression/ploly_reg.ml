(**
  * URL: https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html
  * The Boston house-price data of Harrison, D. and Rubinfeld, D.L. 'Hedonic
 prices and the demand for clean air', J. Environ. Economics & Management,
 vol.5, 81-102, 1978.   Used in Belsley, Kuh & Welsch, 'Regression diagnostics
 ...', Wiley, 1980.   N.B. Various transformations are used in the table on
 pages 244-261 of the latter.

 Variables in order:
 CRIM     per capita crime rate by town
 ZN       proportion of residential land zoned for lots over 25,000 sq.ft.
 INDUS    proportion of non-retail business acres per town
 CHAS     Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
 NOX      nitric oxides concentration (parts per 10 million)
 RM       average number of rooms per dwelling
 AGE      proportion of owner-occupied units built prior to 1940
 DIS      weighted distances to five Boston employment centres
 RAD      index of accessibility to radial highways
 TAX      full-value property-tax rate per $10,000
 PTRATIO  pupil-teacher ratio by town
 B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
 LSTAT    % lower status of the population
 MEDV     Median value of owner-occupied homes in $1000's
  *)

let data = Owl_io.read_csv ~sep:' ' "boston.csv"
let data = Array.map (fun x -> Array.map float_of_string x) data |> Mat.of_arrays

let lstat = Mat.get_slice [[];[12]] data
let medv = Mat.get_slice [[];[13]] data


let plot_boston () =
  let h = Plot.create "boston.png" in
  Plot.scatter ~h ~spec:[ MarkerSize 5.] lstat medv;
  Plot.set_xlabel h "lstat";
  Plot.set_ylabel h "medv";
  Plot.output h


let poly () = 
  let a = Regression.D.poly lstat medv 2 in 
  let a0 = Mat.get a 0 0 in 
  let a1 = Mat.get a 1 0 in 
  let a2 = Mat.get a 2 0 in 
  fun x -> a0 +. a1 *. x +. a2 *. x *. x 


let poly4 lstat medv = 
  let a = Regression.D.poly lstat medv 4 in 
  let a0 = Mat.get a 0 0 in 
  let a1 = Mat.get a 1 0 in 
  let a2 = Mat.get a 2 0 in 
  let a3 = Mat.get a 3 0 in 
  let a4 = Mat.get a 4 0 in 
  fun x -> a0 +. a1 *. x +. a2 *. x *. x
    +. a3 *. x *. x *. x +. a4 *. x *. x *. x *. x 


let poly6 lstat medv = 
  let a = Regression.D.poly lstat medv 6 in 
  let a0 = Mat.get a 0 0 in 
  let a1 = Mat.get a 1 0 in 
  let a2 = Mat.get a 2 0 in 
  let a3 = Mat.get a 3 0 in 
  let a4 = Mat.get a 4 0 in 
  let a5 = Mat.get a 5 0 in 
  let a6 = Mat.get a 6 0 in 
  fun x -> a0 +. a1 *. x +. a2 *. x *. x
    +. a3 *. x *. x *. x +. a4 *. x *. x *. x *. x 
    +. a5 *. x *. x *. x *. x *. x 
    +. a6 *. x *. x *. x *. x *. x *. x 


let plot_poly () = 
  let h = Plot.create "reg_poly.png" in
  Plot.scatter ~h ~spec:[ MarkerSize 5.] lstat medv;
  Plot.set_xlabel h "lstat";
  Plot.set_ylabel h "medv";
  Plot.plot_fun ~h ~spec:[ LineWidth 3.] (poly ()) 2. 38.;
  Plot.output h


let plot_poly4 () = 
  let h = Plot.create "reg_poly4.png" in
  Plot.scatter ~h lstat medv;
  Plot.plot_fun ~h (poly4 ()) 2. 36.;
  Plot.output h


let plot_poly6 () = 
  let h = Plot.create "reg_poly6.png" in
  Plot.scatter ~h lstat medv;
  Plot.plot_fun ~h (poly6 ()) 2. 36.;
  Plot.output h


(** regularistion on subset *)

let data = Owl_io.read_csv ~sep:' ' "boston.csv"
let data = Array.map (fun x -> Array.map float_of_string x) data |> Mat.of_arrays

let subdata, _ = Mat.draw_rows ~replacement:false data 50

let slstat = Mat.get_slice [[];[12]] subdata
let smedv = Mat.get_slice [[];[13]] subdata


let plot_sub () =
  let h = Plot.create "boston_sub.png" in
  Plot.scatter ~h slstat smedv;
  Plot.output h

let plot_poly4s () = 
  let h = Plot.create "reg_poly4s.png" in
  Plot.scatter ~h slstat smedv;
  Plot.plot_fun ~h (poly4 ()) 2. 36.;
  Plot.output h

let plot_poly6s () = 
  let h = Plot.create "reg_poly6s.png" in
  Plot.scatter ~h slstat smedv;
  Plot.plot_fun ~h (poly6 ()) 2. 36.;
  Plot.output h



open Optimise.D
open Optimise.D.Algodiff

let poly_ridge ~alpha x y n =
    let z =
      Array.init (n + 1) (fun i -> A.(pow_scalar x (float_of_int i |> float_to_elt)))
    in
    let x = A.concatenate ~axis:1 z in
    let params =
      Params.config
        ~batch:Batch.Full
        ~learning_rate:(Learning_Rate.Const 1.)
        ~gradient:Gradient.Newton
        ~loss:Loss.Quadratic
        ~regularisation:(Regularisation.L2norm alpha)
        ~verbosity:false
        ~stopping:(Stopping.Const 1e-16)
        100.
    in
    (Regression.D._linear_reg false params x y).(0)

module M = Dense.Matrix.D

let poly4_reg lstat medv = 
  let a = poly_ridge ~alpha:20. lstat medv 4 in 
  let a0 = M.get a 0 0 in 
  let a1 = M.get a 1 0 in 
  let a2 = M.get a 2 0 in 
  let a3 = M.get a 3 0 in 
  let a4 = M.get a 4 0 in 
  fun x -> a0 +. a1 *. x +. a2 *. x *. x
    +. a3 *. x *. x *. x +. a4 *. x *. x *. x *. x 

let plot_poly4s_reg () = 
  let h = Plot.create "reg_poly4s_reg.png" in
  Plot.scatter ~h slstat smedv;
  Plot.plot_fun ~h (poly4_reg slstat smedv) 2. 36.;
  Plot.output h

let plot_exp ()  =
  let a, b, e = Regression.D.exponential lstat medv in 
  let f1 x = a *. Maths.exp ((-1.) *. b *. x) +. e in  
  let f2 x = a *. Maths.exp ((-1.) *. e *. x) +. b in  
  let f3 x = b *. Maths.exp ((-1.) *. a *. x) +. e in  
  let f4 x = b *. Maths.exp ((-1.) *. e *. x) +. a in  
  let f5 x = e *. Maths.exp ((-1.) *. a *. x) +. b in  
  let f6 x = e *. Maths.exp ((-1.) *. b *. x) +. a in  
  let h = Plot.create "fit_exp.png" in
  Plot.scatter ~h lstat medv;
  Plot.plot_fun ~h f1 2. 36.;
  Plot.plot_fun ~h f2 2. 36.;
  Plot.plot_fun ~h f3 2. 36.;
  Plot.plot_fun ~h f4 2. 36.;
  Plot.plot_fun ~h f5 2. 36.;
  Plot.plot_fun ~h f6 2. 36.;
  Plot.output h