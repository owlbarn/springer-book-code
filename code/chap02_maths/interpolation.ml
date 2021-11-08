open Owl_plplot

let a = -5. 
let b = 5. 
let xs1 = Mat.linspace a b 11
let g x = 1. /. (1. +. x *. x)
let ys1 = Mat.map g xs1

let f1 x = 
    let y, _err = Owl_maths_interpolate.polint (Mat.to_array xs1) (Mat.to_array ys1) x in
    y

let xs2 = Mat.linspace 1. 10. 11
let xs2 = Mat.scalar_add
    ((a +. b) /. 2.)
    (Mat.scalar_mul
      ((b -. a) /. 2.)
      (Mat.cos (Mat.scalar_mul (Owl_const.pi /. 10.) (Mat.sub_scalar xs2 0.5)))) |> Mat.reverse
let ys2 = Mat.map g xs2

let f2 x = 
    let y, _err = Owl_maths_interpolate.polint (Mat.to_array xs2) (Mat.to_array ys2) x in
    y


let plot1 () = 
    let h = Plot.create ~m:1 ~n:2 "fig_inter_poly.pdf" in 
    Plot.subplot h 0 0;
    Plot.(plot_fun ~h ~spec: [RGB (66,133,244); LineStyle 1; LineWidth 2.; Marker "*"] g (-5.) 5.); 
    Plot.(plot_fun ~h ~spec: [RGB (219,68,55); LineStyle 2; LineWidth 2.; Marker "+"] f1 (-5.) 5.);
    Plot.(scatter ~h ~spec: [Marker "o"; MarkerSize 3.] xs1 ys1);
    Plot.legend_on h [|"Original"; "Interpolated"; "Points"|];
    Plot.set_title h "Interpolation with Equal Step";
    Plot.set_xlabel h "";
    Plot.set_ylabel h "";

    Plot.subplot h 0 1;
    Plot.(plot_fun ~h ~spec: [RGB (66,133,244); LineStyle 1; LineWidth 2.; Marker "*"] g (-5.) 5.); 
    Plot.(plot_fun ~h ~spec: [RGB (219,68,55); LineStyle 2; LineWidth 2.; Marker "+"] f2 (-5.) 5.); 
    Plot.(scatter ~h ~spec: [Marker "o"; MarkerSize 3.] xs2 ys2);
    Plot.set_title h "Interpolation with Chebyshev Step";
    Plot.set_xlabel h "";
    Plot.set_ylabel h "";
    Plot.output h

(** Test the impl. of ratint *)

let xs3 = [| -2.;-1.;0.;1.;2. |]
let ys3 = [| 1.;-2.;-1.;0.;1. |]
let f3 x = 
    let y, _err = Owl_maths_interpolate.ratint xs3 ys3 x in
    y 

let plot2 () = 
    let h = Plot.create ~m:1 ~n:2 "fig_inter_rat.pdf" in 
    Plot.subplot h 0 0;
    Plot.(plot_fun ~h ~spec: [RGB (66,133,244); LineStyle 1; LineWidth 2.; Marker "*"] g (-5.) 5.); 
    Plot.(plot_fun ~h ~spec: [RGB (219,68,55); LineStyle 2; LineWidth 2.; Marker "+"] f3 (-5.) 5.);
    Plot.(scatter ~h ~spec: [Marker "o"; MarkerSize 3.] xs1 ys1);
    Plot.legend_on h [|"Original"; "Interpolated"; "Points"|];
    Plot.set_title h "Interpolation with Equal Step";
    Plot.set_xlabel h "";
    Plot.set_ylabel h "";

    Plot.subplot h 0 1;
    Plot.(plot_fun ~h ~spec: [RGB (66,133,244); LineStyle 1; LineWidth 2.; Marker "*"] g (-5.) 5.); 
    Plot.(plot_fun ~h ~spec: [RGB (219,68,55); LineStyle 2; LineWidth 2.; Marker "+"] f2 (-5.) 5.); 
    Plot.(scatter ~h ~spec: [Marker "o"; MarkerSize 3.] xs2 ys2);
    (* Plot.legend_on h [|"Original"; "Interpolated"; "Points"|]; *)
    Plot.set_title h "Interpolation with Chebyshev Step";
    Plot.set_xlabel h "";
    Plot.set_ylabel h "";
    Plot.output h