open Owl 
open Owl_plplot

let plot_sine_1 () =
  let f1 x = Maths.sin (50. *. 2. *. Owl_const.pi *. x) in 
  let f2 x = 0.5 *. (Maths.sin (80. *. 2. *. Owl_const.pi *. x)) in 

  let h = Plot.create "plot_sine_1.pdf" in
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.;
  Plot.set_xlabel h "";
  Plot.set_ylabel h "";

  Plot.plot_fun ~h ~spec:[ RGB (66,133,234); LineStyle 1] f1 0. 0.05;
  Plot.plot_fun ~h ~spec:[ RGB (251,188,5); LineStyle 2] f2 0. 0.05;
  (* Plot.(legend_on h ~position:NorthWest [|"y1"; "y2"|]); *) 
  Plot.output h


let plot_sine_2 () =
  let f1 x = Maths.sin (50. *. 2. *. Owl_const.pi *. x) in 
  let f2 x = 0.5 *. (Maths.sin (80. *. 2. *. Owl_const.pi *. x)) in 
  let f x = f1 x +. f2 x in 

  let h = Plot.create "plot_sine_2.pdf" in
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.;
  Plot.set_xlabel h "";
  Plot.set_ylabel h "";

  Plot.plot_fun ~h ~spec:[ RGB (52,168,83); LineStyle 1] f 0. 0.05;
  Plot.output h