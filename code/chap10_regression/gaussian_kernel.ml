
let gaussian sigma mux muy x y =
  let p = Maths.(pow (x -. mux) 2. +. pow (y -. muy) 2.) in
  let r = Maths.exp ((-1.) *. p /. (2. *. sigma *. sigma)) in 
  r /. (2. *. Owl_const.pi *. sigma *. sigma)

let plot () =
  let x, y = Mat.meshgrid (-1.) 5. (-1.) 5. 50 50 in
  let f x y = 
    (gaussian 0.6 1. 3.) x y +. 
    (gaussian 1.7 2. 1.) x y +.
    (gaussian 0.8 3. 3.) x y
  in
  let z = Mat.(map2 f x y) in
  let h = Plot.create ~m:1 ~n:2 "kernel.png" in
  Plot.(mesh ~h ~spec:[ NoMagColor ] x y z);
  Plot.set_xlabel h "";
  Plot.set_ylabel h "";
  Plot.set_zlabel h "";
  Plot.subplot h 0 1;
  Plot.contour ~h x y z;
  Plot.text ~h ~spec:[ RGB (66,133,244) ] 1. (3. -. 0.0) "p1";
  Plot.text ~h ~spec:[ RGB (66,133,244) ] 2. (1. -. 0.0) "p2";
  Plot.text ~h ~spec:[ RGB (66,133,244) ] 3. (3. -. 0.0) "p3"; 
  Plot.set_xlabel h "";
  Plot.set_ylabel h "";
  Plot.output h
