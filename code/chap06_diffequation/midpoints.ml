let f x y = 2. *. x *. y +. x
let f' x = 0.5 *. (Maths.exp (x *. x) -. 1.)

let euler step target =
  let x = ref 0. in
  let y = ref 0. in
  while !x <= target do
    y := !y +. step *. (f !x !y);
    x := !x +. step
  done;
  !y

let midpoint step target =
  let x = ref 0. in
  let y = ref 0. in
  while !x <= target do
    let s1 = f !x !y in
    let s2 = f (!x +. step /. 2.) (!y +. step /. 2. *. s1) in
    y := !y +. step *. (s1 +. s2) /. 2.;
    x := !x +. step
  done;
  !y

let _ =
  let target = 2.6 in
  let h = Plot.create "plot_rk01.png" in
  Plot.(plot_fun ~h ~spec:[ RGB (66,133,244); LineStyle 1; 
    LineWidth 2.; Marker "*" ] f' 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (219,68,55); LineStyle 2; 
    LineWidth 2.; Marker "+" ] (euler 0.01) 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (219,68,55); LineStyle 2;
    LineWidth 2.; Marker "." ] (euler 0.001) 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (244,180,0); LineStyle 3; 
    LineWidth 2.; Marker "+" ] (midpoint 0.01) 2. target);
  Plot.(plot_fun ~h ~spec:[ RGB (244,180,0); LineStyle 3; 
    LineWidth 2.; Marker "." ] (midpoint 0.001) 2. target);
  Plot.(legend_on h ~position:NorthWest 
    [|"Close-Form Solution"; "Euler (step = 0.01)";
    "Euler (step = 0.001)"; "Midpoint (step = 0.01)"; 
    "Midpoint (step = 0.001)"|]);
  Plot.output h