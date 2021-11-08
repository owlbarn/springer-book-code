
let plot_cost () = 
  let f0 x = Maths.(-1. *. log (1. /. (1. +. exp (-1. *. x) ))) in 
  let f1 x = Maths.(-1. *. log (1. -. 1. /. (1. +. exp (-1. *. x) ))) in 
  let g0 x = if x >= 1. then 0. else 0.625 *. (1. -. x) in 
  let g1 x = if x <= (-1.) then 0. else 0.625 *. (1. +. x) in 
  let h = Plot.create ~m:1 ~n:2 "svm_cost.png" in
  Plot.subplot h 0 0;
  Plot.plot_fun ~h ~spec:[ LineStyle 1; RGB (66, 133, 244); LineWidth 3. ] f0 (-3.) 3.;
  Plot.plot_fun ~h ~spec:[ LineStyle 2; RGB (219, 68, 55);  LineWidth 3. ] g0 (-3.) 3.;
  Plot.legend_on h [|"f0"; "g0"|];
  Plot.subplot h 0 1;
  Plot.plot_fun ~h ~spec:[ LineStyle 1; RGB (66, 133, 244); LineWidth 3. ] f1 (-3.) 3.;
  Plot.plot_fun ~h ~spec:[ LineStyle 2; RGB (219, 68, 55);  LineWidth 3. ] g1 (-3.) 3.;
  Plot.legend_on h ~position:NorthWest [|"f1"; "g1"|];
  Plot.output h