open Owl 
open Owl_plplot

let data = Owl_io.read_csv ~sep:';' "sunspot_full.csv"
let data = Array.map (fun x -> Array.map float_of_string x) 
  data |> Mat.of_arrays

let x = Mat.get_slice [[];[0]] data
let y = Mat.get_slice [[];[1]] data

let plot_sunspot x y =
  let h = Plot.create "plot_sunspot.png" in
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.;
  Plot.set_xlabel h "Date";
  Plot.set_ylabel h "Sunspot number";
  Plot.plot ~h ~spec:[ RGB (255,0,0); LineStyle 1] x y;
  Plot.output h

let get_frequency = Book_signal.get_frequency

let plot_sunspot_freq p = Book_signal.plot_sunspot_freq ~samples:(Mat.row_num y) p

let () = plot_sunspot x y; plot_sunspot_freq (get_frequency y)
