open Owl
open Owl_plplot

module N = Dense.Ndarray.D

let _ =
  let x = N.linspace 0. 16. 100 in

  let f1 x = Owl_stats.gamma_pdf x ~shape:1. ~scale:2. in
  let f2 x = Owl_stats.gamma_pdf x ~shape:2. ~scale:2. in
  let f3 x = Owl_stats.gamma_pdf x ~shape:5. ~scale:1. in
  let f4 x = Owl_stats.gamma_pdf x ~shape:7.5 ~scale:1. in

  let y1 = N.map f1 x in
  let y2 = N.map f2 x in
  let y3 = N.map f3 x in
  let y4 = N.map f4 x in

  let h = Plot.create "gamma_pdf.png" in
  let open Plot in
  set_xlabel h "";
  set_ylabel h "";
  set_title h "Gamma distribution probablility density functions";
  plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2. ] x y1;
  plot ~h ~spec:[ RGB (219, 68,  55); LineStyle 1; LineWidth 2. ] x y2;
  plot ~h ~spec:[ RGB (244, 180,  0); LineStyle 1; LineWidth 2. ] x y3;
  plot ~h ~spec:[ RGB (15, 157,  88); LineStyle 1; LineWidth 2. ] x y4;

  Plot.(legend_on h ~position:NorthEast [|"k=1, theta=2"; "k=2, theta=2"; "k=5, theta=1"; "k=7.5, theta=1"|]);

  output h