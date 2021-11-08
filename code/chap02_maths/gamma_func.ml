let x = [|2; 3; 4; 5; 6|]
let y = Array.map (fun x -> Maths.fact (x - 1)) x
let x = Array.map float_of_int x

let f a =
  let v, _ = Owl_maths_interpolate.polint x y a in
  v

let xm = Mat.of_array x 1 5
let ym = Mat.of_array y 1 5

let _ =
  let h = Plot.create "interp.png" in
  Plot.(plot_fun ~h ~spec:[ RGB (66, 133, 244); 
    LineStyle 1; LineWidth 2.] f 2. 6.5);
  Plot.(plot_fun ~h ~spec:[ RGB (219, 68,  55); 
    LineStyle 2; LineWidth 2.] Maths.gamma 2. 6.5);
  Plot.(scatter ~h ~spec:[ Marker "#[0x229a]"; 
    MarkerSize 5. ] xm ym);
  Plot.(legend_on h ~position:NorthWest [|"Interpolation"; 
    "Gamma function"; "Given values"|]);
  Plot.output h