open Owl
open Owl_plplot

let plot_01 () =
  let h = Plot.create ~m:1 ~n:2 "example_integrals.png" in
  Plot.subplot h 0 0;
  Plot.(plot_fun ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2.] Maths.dawsn (-5.) 5.);
  Plot.set_ylabel h "dawsn(x)";
  Plot.subplot h 0 1;
  Plot.(plot_fun ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2.] (fun x -> let s, _ = Maths.fresnel x in s) 0. 5.);
  Plot.(plot_fun ~h ~spec:[ RGB (219, 68,  55); LineStyle 2; LineWidth 2.] (fun x -> let _, c = Maths.fresnel x in c) 0. 5.);
  Plot.(legend_on h ~position:SouthEast [|"S(x)"; "C(x)"|]);
  Plot.set_ylabel h "fresnel(x)";
  Plot.output h