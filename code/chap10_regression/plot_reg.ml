let data = Owl_io.read_csv ~sep:',' "data_reg.csv"
let data = Array.map (fun x -> Array.map float_of_string x) data |> Mat.of_arrays

let x = Mat.get_slice [[];[1]] data
let y = Mat.get_slice [[];[0]] data

let plot_01 () =
  let h = Plot.create "regdata.pdf" in
  Plot.scatter ~h ~spec:[ MarkerSize 6.] x y;
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 2.;
  Plot.set_xlabel h "population";
  Plot.set_ylabel h "profit";
  Plot.output h


let plot_02 () =
  let h = Plot.create ~m:1 ~n:3 "reg_options.pdf" in
  Plot.subplot h 0 0;
  Plot.scatter ~h x y;
  Plot.plot_fun ~h ~spec:[ RGB (0,0,255) ] (fun a -> a +. 2.) 3. 20.;
  Plot.set_xlabel h "population";
  Plot.set_ylabel h "profit";
  Plot.subplot h 0 1;
  Plot.scatter ~h x y;
  Plot.plot_fun ~h ~spec:[ RGB (0,0,255) ] (fun a -> a *. (-1.) +. 15.) 0. 12.;
  Plot.set_xlabel h "population";
  Plot.set_ylabel h "profit";
  Plot.subplot h 0 2;
  Plot.scatter ~h x y;
  Plot.plot_fun ~h ~spec:[ RGB (0,0,255) ] (fun a -> a *. (0.5) +. 10.) 0. 20.;
  Plot.set_xlabel h "population";
  Plot.set_ylabel h "profit";
  Plot.output h 



let plot_03 () =
  let h = Plot.create "reg_gd.pdf" in
  Plot.scatter ~h ~spec:[ MarkerSize 6.] x y;
  Plot.set_xlabel h "population";
  Plot.set_ylabel h "profit";
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 2.;
  Plot.plot_fun ~h ~spec:[ RGB (0,0,255); LineWidth 2. ] (fun a -> a *. 0.57 +. 5.01) 0. 22.;
  Plot.output h 

let j theta0 theta1 = 
  let f x = x *. theta1 +. theta0 in
  Mat.(pow_scalar (map f x - y) 2. |> mean') *. 0.5


let plot_04 () = 
  let x, y = Mat.meshgrid (-20.) 10. (-20.) 10. 100 100 in
  let z = Mat.(map2 j x y) in
  let h = Plot.create ~m:1 ~n:2 "reg_cost_1.pdf" in
  Plot.subplot h 0 0;
  Plot.(mesh ~h ~spec:[ NoMagColor ] x y z);
  Plot.set_xlabel h "theta0";
  Plot.set_ylabel h "theta1";
  Plot.set_zlabel h "cost";
  Plot.subplot h 0 1;
  Plot.contour ~h x y z;
  Plot.set_xlabel h "theta0";
  Plot.set_ylabel h "theta1";
  Plot.output h

let cost x_data y_data theta0 theta1 =
    let f x = x *. theta1 +. theta0 in
    Mat.(pow_scalar (map f x_data - y_data) 2. |> mean') *. 0.5

let plot_surface x_data y_data =
    let x, y = Mat.meshgrid (-20.) 15. (-20.) 15. 50 50 in 
    let z = Mat.(map2 (cost x_data y_data) x y) in
    let h = Plot.create ~m:1 ~n:2 "reg_cost.pdf" in 
    Plot.subplot h 0 0;
    Plot.(mesh ~h ~spec:[ NoMagColor ] x y z); 
    Plot.set_zlabel h "cost";
    Plot.subplot h 0 1;
    Plot.contour ~h x y z;
    Plot.output h

let generate_data () =
    let x = Mat.uniform 500 1 in
    let p = Mat.uniform 1 1 in
    let y = Mat.(x *@ p + gaussian ~sigma:0.05 500 1) in x, y

let plot_rand () =
  let x, y = generate_data () in
  let h = Plot.create "plot.pdf" in
  let a, b = Linalg.D.linreg x y in
  let y' = Mat.(x *$ b +$ a) in
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 2.;
  Plot.scatter ~h x y;
  Plot.plot ~h ~spec:[ RGB (0,255,0); LineWidth 2. ] x y'; 
  Plot.output h