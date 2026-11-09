open Owl
let count=ref 0
let check name ok=incr count;if not ok then failwith name
let near name expected actual tol=check name (Float.is_finite actual && abs_float(expected-.actual)<tol)
let () =
  Owl_stats_prng.init 180;
  let x=Mat.of_array [|-2.;-1.;0.;1.;2.|] 5 1 in
  let y=Mat.(x *$ 2. +$ 1.) in
  let a,b=Book_regression.gradient_desc x y in
  near "GD intercept analytic recurrence" (1.+.9.*.0.99**501.) !a 1e-12;
  near "GD slope analytic recurrence" (2.+.8.*.0.98**501.) !b 1e-12;
  let ux=Mat.of_array [|0.;1.;2.|] 3 1 in
  let uy=Mat.(ux *$ 2. +$ 1.) in
  let a,b=Book_regression.gradient_desc ux uy in
  let lambda1=(4.+.sqrt 10.)/.3. and lambda2=(4.-.sqrt 10.)/.3. in
  let m1=1.-.0.01*.lambda1 and m2=1.-.0.01*.lambda2 in
  let aa=(m1**501.-.m2**501.)/.(m1-.m2) in
  let bb=(m1*.m2**501.-.m2*.m1**501.)/.(m1-.m2) in
  near "uncentered simultaneous GD intercept" (1.+.aa*.8.83+.bb*.9.) !a 1e-10;
  near "uncentered simultaneous GD slope" (2.+.aa*.(8.*.(1.-.0.01*.5./.3.)-.0.09)+.bb*.8.) !b 1e-10;
  let w=Book_regression.poly_ridge ~alpha:3. x y 1 in
  near "ridge exact intercept" 1. (Mat.get w 0 0) 1e-10;
  near "ridge exact slope" (20./.13.) (Mat.get w 1 0) 1e-10;
  let w=Book_regression.poly_ridge ~alpha:100. x (Mat.create 5 1 7.) 2 in
  near "ridge constant target intercept" 7. (Mat.get w 0 0) 1e-9;
  near "ridge constant slopes vanish" 0. (Mat.get w 1 0) 1e-9;
  near "ridge constant curvature vanishes" 0. (Mat.get w 2 0) 1e-9;
  let constant_probability labels =
    let x=Mat.zeros (Array.length labels) 1 in
    let y=Mat.of_array labels (Array.length labels) 1 in
    let w=Book_regression.logistic ~i:true x y in
    Owl_maths.sigmoid (Mat.get w.(1) 0 0) in
  near "balanced logistic optimum" 0.5 (constant_probability [|0.;0.;1.;1.|]) 1e-7;
  near "class prevalence logistic optimum" 0.25 (constant_probability [|0.;0.;0.;1.|]) 1e-7;
  let x=Mat.of_array [|-3.;-2.;-1.;1.;2.;3.|] 6 1 in
  let w=Book_regression.logistic ~i:true x (Mat.of_array [|0.;0.;0.;1.;1.;1.|] 6 1) in
  let z=Mat.(x *@ w.(0) +$ (get w.(1) 0 0)) |> Mat.to_array in
  Array.iteri (fun i v -> check "separable fixture classification" ((v>0.)=(i>=3))) z;
  Printf.printf "PASS: %d native regression analytic checks\n" !count
