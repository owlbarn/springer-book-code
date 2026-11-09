open Owl
open Owl_ode
open Owl_ode.Types
let count = ref 0
let check label condition = incr count; if not condition then failwith label
let near label expected actual tolerance = check label (Float.is_finite actual && abs_float (expected -. actual) < tolerance)
let last matrix = let r,c = Mat.shape matrix in Mat.get matrix (r-1) (c-1)
let () =
  let exact = Book_ode.f' 1. in
  near "Euler endpoint" exact (Book_ode.euler 0.001 1.) 0.003;
  near "midpoint endpoint" exact (Book_ode.midpoint 0.001 1.) 3e-6;
  near "zero interval" 0. (Book_ode.euler 0.1 0.) 1e-15;
  near "partial final step" (Book_ode.f' 1.23) (Book_ode.midpoint 0.001 1.23) 1e-5;
  let error method_ h = abs_float (exact -. method_ h 1.) in
  check "Euler first-order refinement" (error Book_ode.euler 0.05 /. error Book_ode.euler 0.025 > 1.8);
  check "midpoint second-order refinement" (error Book_ode.midpoint 0.05 /. error Book_ode.midpoint 0.025 > 3.5);
  check "invalid step rejected" (try ignore (Book_ode.euler 0. 1.); false with Invalid_argument _ -> true);
  let a = Mat.of_arrays [|[|1.;-1.|];[|2.;-3.|]|] in
  let f y _ = Mat.(a *@ y) in
  let y0 = Mat.of_array [|-1.;1.|] 2 1 in
  let ts,ys = Ode.odeint Native.D.rk4 f y0 (T1 {t0=0.;duration=2.;dt=0.01}) () in
  near "RK4 final time" 2. (last ts) 1e-10;
  let r = sqrt 2. in
  near "RK4 linear system x" (exp (-2.) *. (-.cosh (2.*.r) -. 3. /. r *. sinh (2.*.r))) (Mat.get ys 0 (Mat.col_num ys-1)) 1e-8;
  near "RK4 linear system y" (exp (-2.) *. (cosh (2.*.r) -. 4. /. r *. sinh (2.*.r))) (Mat.get ys 1 (Mat.col_num ys-1)) 1e-8;
  let f y t = Mat.((2. $* y *$ t) +$ t) in
  let ts,ys = Ode.odeint (Native.D.rk45 ~tol:1e-9 ~dtmax:10.) f (Mat.zeros 1 1) (T1 {t0=0.;duration=1.;dt=0.001}) () in
  near "adaptive endpoint time" 1. (last ts) 1e-12;
  near "adaptive analytic solution" exact (last ys) 1e-7;
  let oscillator y _ = let m=Mat.of_arrays [|[|0.;1.|];[|-1.;0.|]|] in Mat.(m *@ y) in
  let ts,ys = Ode.odeint Native.D.rk4 oscillator (Mat.of_array [|1.;0.|] 2 1) (T1 {t0=0.;duration=10.;dt=0.01}) () in
  near "oscillator final time" 10. (last ts) 1e-10;
  near "oscillator x" (cos 10.) (Mat.get ys 0 (Mat.col_num ys-1)) 1e-8;
  near "oscillator y" (-.sin 10.) (Mat.get ys 1 (Mat.col_num ys-1)) 1e-8;
  let force (x,p) _ = Mat.((x *$ (-1.)) + (p *$ (-1.))) in
  let position = Mat.of_array [|-0.25|] 1 1 and momentum = Mat.of_array [|0.75|] 1 1 in
  let _,xs,ps = Ode.odeint Symplectic.D.ruth3 force (position,momentum) (T1 {t0=0.;duration=15.;dt=0.01}) () in
  check "damped position approaches zero" (abs_float (last xs) < 0.002);
  check "damped momentum approaches zero" (abs_float (last ps) < 0.002);
  let decay y _ = Mat.neg y in
  let ts,ys = Ode.odeint (Owl_ode_odepack.lsoda ~relative_tol:1e-9 ~abs_tol:1e-11) decay (Mat.ones 1 1) (T1 {t0=0.;duration=2.;dt=0.01}) () in
  near "LSODA final time" 2. (last ts) 1e-10;
  near "LSODA analytic decay" (exp (-2.)) (last ys) 1e-7;
  let custom=Native.D.rk45 ~tol:1e-7 ~dtmax:0.1 in
  let ts,ys=Ode.odeint custom oscillator (Mat.of_array [|1.;0.|] 2 1) (T1 {t0=0.;duration=10.;dt=0.01}) () in
  near "configured RK45 final time" 10. (last ts) 1e-10;
  near "configured RK45 oscillator position" (cos 10.) (Mat.get ys 0 (Mat.col_num ys-1)) 1e-5;
  near "configured RK45 oscillator velocity" (-.sin 10.) (Mat.get ys 1 (Mat.col_num ys-1)) 1e-5;
  let f y _ = let a=Mat.get y 0 0 and b=Mat.get y 0 1 in
    Mat.of_array [|b;(1.-.a*.a)*.b-.a|] 1 2 in
  let initial=Mat.of_array [|0.02;0.03|] 1 2 in
  let ts,ys=Ode.odeint (Native.D.rk45 ~tol:1e-8 ~dtmax:0.05) f initial (T1 {t0=0.;duration=30.;dt=0.01}) () in
  near "nonstiff Van der Pol initial time" 0. (Mat.get ts 0 0) 1e-12;
  near "nonstiff Van der Pol endpoint" 30. (last ts) 1e-10;
  check "nonstiff state and time shapes" (Mat.col_num ys=2 && Mat.row_num ys=Mat.row_num ts);
  check "nonstiff trajectory finite" (Array.for_all Float.is_finite (Mat.to_array ys));
  near "nonstiff initial position" 0.02 (Mat.get ys 0 0) 1e-12;
  near "nonstiff initial velocity" 0.03 (Mat.get ys 0 1) 1e-12;
  (* Independent scalar RK4 reference for the two textbook differential equations. *)
  let h=0.001 and q=ref 0.02 and v=ref 0.03 in
  let rhs q v = v,(1.-.q*.q)*.v-.q in
  for _=1 to 30000 do
    let a,b=rhs !q !v in
    let c,d=rhs (!q+.h*.a/.2.) (!v+.h*.b/.2.) in
    let e,g=rhs (!q+.h*.c/.2.) (!v+.h*.d/.2.) in
    let j,k=rhs (!q+.h*.e) (!v+.h*.g) in
    q:= !q+.h*.(a+.2.*.c+.2.*.e+.j)/.6.;
    v:= !v+.h*.(b+.2.*.d+.2.*.g+.k)/.6.
  done;
  let lastrow=Mat.row_num ys-1 in
  near "nonstiff RK45 position versus independent RK4" !q (Mat.get ys lastrow 0) 2e-5;
  near "nonstiff RK45 velocity versus independent RK4" !v (Mat.get ys lastrow 1) 2e-5;
  Printf.printf "PASS: %d ODE analytic and property checks\n" !count
