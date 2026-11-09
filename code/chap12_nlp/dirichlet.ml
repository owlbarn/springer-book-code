open Owl
open Owl_plplot
let () = Owl_stats_prng.init 83

let alpha01 = [|0.1; 0.1|]
let alpha02 = [|3.; 3.|]

let n = 100

let generate_data alpha =
  let x = Mat.zeros 1 n in
  let y = Mat.zeros 1 n in
  for i = 0 to n - 1 do
    let a = Stats.dirichlet_rvs ~alpha in
    Mat.set x 0 i (a.(0));
    Mat.set y 0 i (a.(1));
  done;
  x, y

let _ =
  let h = Plot.create ~m:1 ~n:2 "dirichlet.png" in
  Plot.set_page_size h 1200 450;
  Plot.set_font_size h 3.;
  Plot.subplot h 0 0;
  let x, y = generate_data alpha01 in
  Plot.set_title h "alpha=0.1";
  Plot.(scatter ~h ~spec:[ Marker "o"; MarkerSize 4. ] x y);
  Plot.set_font_size h 3.;
  Plot.set_xlabel h "";
  Plot.set_ylabel h "";
  Plot.subplot h 0 1;
  Plot.set_title h "alpha=3";
  let x, y = generate_data alpha02 in
  Plot.(scatter ~h ~spec:[ Marker "o"; MarkerSize 4. ] x y);
  Plot.set_font_size h 3.;
  Plot.set_xlabel h "";
  Plot.set_ylabel h "";
  Plot.output h

let () =
  Array.iter (fun alpha ->
    let x,y=generate_data alpha in
    Mat.iteri (fun i value ->
      let other=Mat.get y 0 i in
      if value<0. || value>1. || abs_float(value+.other-.1.)>1e-12 then failwith "Dirichlet simplex") x) [|alpha01;alpha02|];
  print_endline "PASS: 200 Dirichlet simplex samples"
