open Owl
open Owl_plplot
let read filename separator =
  Owl_io.read_csv ~sep:separator filename
  |> Array.map (Array.map float_of_string) |> Mat.of_arrays
let () =
  let tone=read "touchtone.csv" ',' |> fun x -> Mat.div_scalar x 128. in
  Dialtone.plot_pair tone "tone.png";
  Dialtone.plot_pair (Mat.get_slice [[];[0;4999]] tone) "tone2.png";
  let rows=Owl_io.read_csv ~sep:',' "goog.csv" in
  let prices=Array.sub rows 1 (Array.length rows-1)
    |> Array.map (fun row -> Array.sub row 1 6 |> Array.map float_of_string) |> Mat.of_arrays in
  let y=Mat.col prices 3 in
  Goog.plot_goog prices y (Book_signal.moving_average 10 y);
  let kernel=Book_signal.gaussian_kernel 3. in
  let width=Mat.numel kernel in
  let smooth=Mat.init (Mat.row_num y-width+1) 1 (fun i ->
    Mat.dot kernel (Mat.get_slice [[i;i+width-1];[]] y) |> Mat.sum') in
  Goog.plot_goog ~filename:"plot_goog_gauss.png" prices y smooth;
  Goog.plot_goog ~filename:"plot_goog2.png" prices y (Book_signal.lowpass ~keep:5 y);
  print_endline "PASS: generated-input dial tone and price figures actually rendered"
