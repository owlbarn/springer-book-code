open Owl
open Owl_plplot

(* book: sunspot-spectrum begin *)
let get_frequency y =
  let spectrum = Owl_fft.D.rfft ~axis:0 y in
  let bins = (Dense.Ndarray.Z.shape spectrum).(0) in
  let selected = Dense.Ndarray.Z.get_slice [[1;bins-1];[]] spectrum in
  Dense.Ndarray.Z.(abs selected |> re)
(* book: sunspot-spectrum end *)

(* book: sunspot-plot begin *)
let plot_sunspot_freq ~samples p =
  let n = Mat.row_num p in
  let frequency = Mat.of_array
    (Array.init n (fun k -> float_of_int (k+1) /. float_of_int samples)) n 1 in
  let h = Plot.create ~m:1 ~n:2 "plot_sunspot_freq.png" in
  Plot.subplot h 0 0;
  Plot.set_xlabel h "cycles/year";
  Plot.set_ylabel h "FFT magnitude";
  Plot.plot ~h frequency p;
  Plot.subplot h 0 1;
  Plot.set_xlabel h "years/cycle";
  Plot.plot ~h (Mat.scalar_div 1. frequency) p;
  Plot.set_xrange h 0. 40.;
  Plot.output h
(* book: sunspot-plot end *)

(* book: gaussian-filter begin *)
let gaussian_kernel sigma =
  if not (Float.is_finite sigma && sigma > 0.) then invalid_arg "positive finite sigma";
  let radius = int_of_float (4. *. sigma +. 0.5) in
  let x = Mat.linspace (-.float_of_int radius) (float_of_int radius) (2*radius+1) in
  let kernel = Mat.map (fun a -> exp (-0.5 *. a *. a /. (sigma *. sigma))) x in
  Mat.div_scalar kernel (Mat.sum' kernel)
(* book: gaussian-filter end *)

let moving_average width y =
  let n = Mat.row_num y in
  if width <= 0 || width > n then invalid_arg "moving average width";
  let filter = Mat.create 1 width (1. /. float_of_int width) in
  Mat.init (n-width+1) 1 (fun i ->
    Mat.dot filter (Mat.get_slice [[i;i+width-1];[]] y) |> Mat.sum')

(* Book helper: current Owl's native irfft guard only accepts even lengths. *)
let irfft_exact ~axis ~n spectrum =
  let module Z = Dense.Ndarray.Z in
  let input_shape = Z.shape spectrum in
  let axis = if axis < 0 then Array.length input_shape + axis else axis in
  if n < 1 || axis < 0 || axis >= Array.length input_shape
     || input_shape.(axis) <> n / 2 + 1 then invalid_arg "real inverse FFT dimensions";
  let output_shape = Array.copy input_shape in
  output_shape.(axis) <- n;
  let full = Z.zeros output_shape in
  let slice first last = List.init (Array.length input_shape)
    (fun dim -> if dim = axis then [first;last] else []) in
  Z.set_slice (slice 0 (input_shape.(axis)-1)) full spectrum;
  for k = 1 to (n-1)/2 do
    let positive = Z.get_slice (slice k k) spectrum |> Z.conj in
    Z.set_slice (slice (n-k) (n-k)) full positive
  done;
  Owl_fft.D.ifft ~axis full |> Z.re

let lowpass ~keep y =
  let spectrum = Owl_fft.D.rfft ~axis:0 y in
  let bins = (Dense.Ndarray.Z.shape spectrum).(0) in
  if keep < 1 || keep > bins then invalid_arg "retained FFT bins";
  if keep < bins then Dense.Ndarray.Z.set_slice [[keep;bins-1];[]] spectrum (Dense.Ndarray.Z.zeros [|bins-keep;1|]);
  irfft_exact ~axis:0 ~n:(Mat.row_num y) spectrum

let visualise_img img_fft filename =
  let shape = Dense.Ndarray.Z.shape img_fft in
  let rows,columns = shape.(0),shape.(1) in
  let magnitude = Dense.Ndarray.Z.(abs img_fft |> re) in
  let x,y = Mat.meshgrid 0. (float_of_int (columns-1)) 0. (float_of_int (rows-1)) columns rows in
  let h = Plot.create filename in
  Plot.mesh ~h ~spec:[Plot.ZLine Plot.Y; Plot.NoMagColor] x y magnitude;
  Plot.set_zrange h 0. 400000.;
  Plot.output h

