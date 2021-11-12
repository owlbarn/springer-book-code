open Owl
open Owl_plplot

let plot_histogram x y = 
  (* convert arrays to matrices *)

  let x' = Mat.of_array x 1 999 in
  let y' = Mat.of_array y 1 999 in

  (* plot the figures *)

  let h = Plot.create ~m:1 ~n:2 "plot_02.png" in

  Plot.subplot h 0 0;
  Plot.set_ylabel h "frequency";
  Plot.histogram ~bin:30 ~h x';
  Plot.histogram ~bin:30 ~h y';

  Plot.subplot h 0 1; 
  Plot.set_ylabel h "PDF p(x)";
  Plot.plot_fun ~h (fun x -> Stats.gaussian_pdf ~mu:1. ~sigma:1. x) (-2.) 6.;
  Plot.plot_fun ~h (fun x -> Stats.gaussian_pdf ~mu:12. ~sigma:3. x) 0. 25.; 

  Plot.output h


let plot_cdf () = 
  let h = Plot.create "plot_gaussian_cdf.png" in
  Plot.set_ylabel h "CDF";
  Plot.plot_fun ~h ~spec:[ RGB (66,133,244); LineStyle 1; LineWidth 2.; Marker "*" ] (fun x -> Stats.gaussian_cdf ~mu:1. ~sigma:1. x) (-2.) 6.;
  Plot.plot_fun ~h ~spec:[ RGB (219,68,55);  LineStyle 2; LineWidth 2.; Marker "+" ] (fun x -> Stats.gaussian_cdf ~mu:12. ~sigma:3. x) 0. 25.;
  Plot.(legend_on h ~position:SouthEast [|"mu=1,sigma=1"; "mu=12, sigma=3"|]);
  Plot.output h


let plot_scatter x y z = 
  let x' = Mat.of_array x 1 50 in
  let y' = Mat.of_array y 1 50 in
  let z' = Mat.of_array z 1 50 in

  let h = Plot.create ~m:1 ~n:2 "plot_01.png" in
  Plot.subplot h 0 0;
  Plot.set_xlabel h "x";
  Plot.set_ylabel h "y (sigma = 1)";
  Plot.scatter ~h x' y';
  Plot.subplot h 0 1;
  Plot.set_xlabel h "x";
  Plot.set_ylabel h "z (sigma = 8)";
  Plot.scatter ~h x' z';
  Plot.output h