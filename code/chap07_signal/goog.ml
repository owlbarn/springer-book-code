open Owl 
open Owl_plplot 

let plot_goog data y y' =
  let n = (Arr.shape data).(0) in
  let x = Mat.sequential n 1 in
  let h = Plot.create "plot_goog.png" in
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.;
  Plot.set_xlabel h "date";
  Plot.set_ylabel h "Google stock price ($)";
  Plot.plot ~h ~spec:[ RGB (255,0,0); LineStyle 1] x y;
  let filtered_rows = Mat.row_num y' in
  let offset = float_of_int (n-filtered_rows) /. 2. in
  let filtered_x = Mat.sequential ~a:offset filtered_rows 1 in
  Plot.plot ~h ~spec:[ RGB (0,0,255); LineStyle 2] filtered_x y';
  Plot.(legend_on h ~position:NorthWest [|"original"; "smooth"|]);
  Plot.output h