
let plot_airy () =
  let x = Mat.linspace (-15.) 5. 200 in 
  let y0 = Mat.map (fun x ->
    let ai, _, _, _ = Maths.airy x in ai
  ) x
  in 
  let y1 = Mat.map (fun x ->
    let _, _, bi, _ = Maths.airy x in bi
  ) x
  in
  let h = Plot.create "special_airy.png" in
  Plot.(plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2. ] x y0);
  Plot.(plot ~h ~spec:[ RGB (219, 68,  55); LineStyle 2; LineWidth 2. ] x y1);
  Plot.(set_yrange h (-0.5) 1.);
  Plot.(legend_on h ~position:SouthEast [|"Ai"; "Bi"|]);
  Plot.output h

let plot_bessel () =
  let x = Mat.linspace (0.) 20. 200 in
  let y0 = Mat.map Maths.j0 x in
  let y1 = Mat.map Maths.j1 x in
  let y2 = Mat.map (Maths.jv 2.) x in
  let h = Plot.create "example_bessel.png" in
  Plot.(plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2. ] x y0);
  Plot.(plot ~h ~spec:[ RGB (219, 68,  55); LineStyle 2; LineWidth 2. ] x y1);
  Plot.(plot ~h ~spec:[ RGB (244, 180,  0); LineStyle 3; LineWidth 2. ] x y2);
  Plot.(legend_on h ~position:NorthEast [|"j0"; "j1"; "j2"|]);
  Plot.output h

let plot_gamma () = 
  let x = Mat.linspace (-3.5) 5. 2000 in
  let y = Mat.map Maths.gamma x in 
  let h = Plot.create "example_gamma.png" in
  Plot.(plot ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2. ] x y);
  Plot.(set_yrange h (-10.) 20.);
  Plot.output h

let plot_struve () = 
  let h = Plot.create "example_struve.png" in
  Plot.(plot_fun ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2.] (Maths.struve 0.) (-12.) 12.);
  Plot.(plot_fun ~h ~spec:[ RGB (219, 68,  55); LineStyle 2; LineWidth 2.] (Maths.struve 1.) (-12.) 12.);
  Plot.(plot_fun ~h ~spec:[ RGB (244, 180,  0); LineStyle 3; LineWidth 2.] (Maths.struve 2.) (-12.) 12.);
  Plot.(plot_fun ~h ~spec:[ RGB (77,  81,  57); LineStyle 1; LineWidth 2.] (Maths.struve 3.) (-12.) 12.);
  Plot.(plot_fun ~h ~spec:[ RGB (111, 51, 129); LineStyle 2; LineWidth 2.] (Maths.struve 4.) (-12.) 12.);
  Plot.(set_yrange h (-3.) 5.);
  Plot.(legend_on h ~position:SouthEast [|"H0"; "H1"; "H2"; "H3"; "H4"|]);
  Plot.output h

let plot_erf () = 
  let h = Plot.create "example_erf.png" in
  Plot.(plot_fun ~h ~spec:[ RGB (66, 133, 244); LineStyle 1; LineWidth 2.] Maths.erf (-3.) 3.);
  Plot.output h

let plot_integrals () = 
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