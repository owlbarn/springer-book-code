#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
open Owl;;
open Owl_plplot;;
#use "book_signal.ml";;
let book_matrix_printer formatter value =
  Owl_pretty.print_dsnda ~max_row:6 ~max_col:6 ~formatter value;;
#install_printer book_matrix_printer;;
Owl_stats_prng.init 77;;

(* book: fft-input begin *)
let a = [|1.;2.;1.;-1.;1.5;1.0|];;
let b = Arr.of_array a [|6|]
  |> Dense.Ndarray.Generic.cast_d2z;;
(* book: fft-input end *)

(* book: fft begin *)
let c = Owl_fft.D.fft b;;
(* book: fft end *)

(* book: inverse-fft begin *)
let d = Owl_fft.D.ifft c;;
(* book: inverse-fft end *)

(* book: sample-times begin *)
module G = Dense.Ndarray.Generic;;
let n = 600.;;
let t = 1. /. 800.;;
let x = Arr.mul_scalar (Arr.sequential [|int_of_float n|]) t;;
(* book: sample-times end *)

(* book: sine-components begin *)
let y1 = Arr.((50. *. 2. *. Owl_const.pi) $* x |> sin);;
let y2 = Arr.(0.5 $* ((80. *. 2. *. Owl_const.pi)
  $* x |> sin));;
(* book: sine-components end *)

(* book: combined-signal begin *)
let y = Arr.(y1 + y2) |> G.cast_d2z;;
(* book: combined-signal end *)

(* book: sine-transform begin *)
let yf = Owl_fft.D.fft y;;
(* book: sine-transform end *)

(* book: magnitude begin *)
let z = Dense.Ndarray.Z.(abs yf |> re);;
(* book: magnitude end *)

(* book: spectrum-plot begin *)
let h = Plot.create "plot_sine_3.pdf" in
let frequency = Arr.mul_scalar (Arr.sequential [|int_of_float n|]) (1. /. (n *. t)) in
Plot.set_xlabel h "frequency (Hz)";
Plot.set_ylabel h "FFT magnitude";
Plot.plot ~h frequency z;
Plot.output h;;
(* book: spectrum-plot end *)

(* book: sunspot-input begin *)
let data = Owl_io.read_csv ~sep:';' "sunspot_full.csv";;
let data = Array.map (fun x -> Array.map float_of_string x)
  data |> Mat.of_arrays;;
let x = Mat.get_slice [[];[0]] data;;
let y = Mat.get_slice [[];[1]] data;;
(* book: sunspot-input end *)

(* book: sunspot-spectrum begin *)
#use "book_signal.ml";;
let get_frequency = get_frequency;;
(* book: sunspot-spectrum end *)

(* book: sunspot-plot begin *)
let p = get_frequency y;;
plot_sunspot_freq ~samples:(Mat.row_num y) p;;
(* book: sunspot-plot end *)

(* book: tone-input begin *)
let fs = 8192.;;
let data = Owl_io.read_csv ~sep:',' "touchtone.csv";;
let data = Array.map (fun x -> Array.map float_of_string x)
           data |> Mat.of_arrays;;
let data = Mat.div_scalar data 128.;;
(* book: tone-input end *)

(* book: tone-times begin *)
let tone_times = Mat.div_scalar (Mat.sequential 1 (Arr.shape data).(1)) fs;;
(* book: tone-times end *)

(* book: tone-spectrum begin *)
let yf = Owl_fft.D.rfft data;;
let y' = Dense.Ndarray.Z.(abs yf |> re);;
let n = (Arr.shape y').(1);;
let x' = Mat.linspace 0. (fs /. 2.) n;;
(* book: tone-spectrum end *)

(* book: first-tone begin *)
let data2 = Arr.get_slice [[];[0; 4999]] data;;
(* book: first-tone end *)

(* book: image-helper begin *)
#use "imageUtils.ml";;
module N = Dense.Ndarray.D;;
module C = Dense.Ndarray.Z;;
(* book: image-helper end *)

(* book: image-input begin *)
let img_arr = load_ppm "cat_noise.ppm" |> Dense.Ndarray.Generic.cast_s2d
  |> N.get_slice [[];[];[0]];;
let shp = N.shape img_arr;;
let h, w = shp.(0), shp.(1);;
let img = N.reshape img_arr [|h; w|]
  |> Dense.Ndarray.Generic.cast_d2z;;
(* book: image-input end *)

(* book: image-fft begin *)
let img_fft = Owl_fft.D.fft2 img;;
visualise_img img_fft "img_fft1.pdf";;
(* book: image-fft end *)

(* book: image-mask begin *)
let sub_length x frac = 
  (float_of_int x) *. frac |> int_of_float;;

let h1 = sub_length h 0.1;;
let h2 = sub_length h 0.9;;
let w1 = sub_length w 0.1;;
let w2 = sub_length w 0.9;;
let index_0 = [ R [h1; h2]; R []];;
let index_1 = [ R [0; h1]; R [w1; w2] ];;
let index_2 = [ R [h2; h-1]; R [w1; w2] ];;
let slice_0 = C.get_fancy index_0 img_fft;;
let slice_1 = C.get_fancy index_1 img_fft;;
let slice_2 = C.get_fancy index_2 img_fft;;

let _ = C.set_fancy index_0 img_fft 
  (C.shape slice_0 |> C.zeros);;
let _ = C.set_fancy index_1 img_fft 
  (C.shape slice_1 |> C.zeros);;
let _ = C.set_fancy index_2 img_fft 
  (C.shape slice_2 |> C.zeros);;
visualise_img img_fft "img_fft2.pdf";;
(* book: image-mask end *)

(* book: image-inverse begin *)
let img = Owl_fft.D.ifft2 img_fft |> C.re;;
let image = N.stack ~axis:2 [|img; img; img|];;
let image = N.expand image 4 |> Dense.Ndarray.Generic.cast_d2s;;
let _ = save_ppm_from_arr image "cat_denoise.ppm";;
(* book: image-inverse end *)

(* book: stock-input begin *)
let data = Owl_io.read_csv ~sep:',' "goog.csv";;
let data = Array.map (fun x ->
    Array.map float_of_string (Array.sub x 1 6))
    (Array.sub data 1 (Array.length data-1)) |> Mat.of_arrays;;
(* book: stock-input end *)

(* book: stock-close begin *)
let y = Mat.get_slice [[];[3]] data;;
(* book: stock-close end *)

(* book: moving-average-filter begin *)
let filter = Mat.of_array (Array.make 10 0.1 ) 1 10;;
let moving_average_filter = filter;;
(* book: moving-average-filter end *)

(* book: moving-average begin *)
let y' = Mat.mapi (fun i _ ->
    let r = Mat.get_fancy [R [i; i+9]; R []] y in
    Mat.dot filter r |> Mat.sum'
  ) (Mat.get_slice [[0; (Arr.shape y).(0) - 10]; []] y);;
(* book: moving-average end *)

(* book: gaussian-filter begin *)
let gaussian_kernel = gaussian_kernel;;
let filter = gaussian_kernel 3.;;
(* book: gaussian-filter end *)

(* book: stock-transform begin *)
let yf = Owl_fft.D.rfft ~axis:0 y;;
(* book: stock-transform end *)

(* book: stock-spectrum begin *)
yf;;
(* book: stock-spectrum end *)

(* book: stock-mask begin *)
let n = (Dense.Ndarray.Z.shape yf).(0);;
let z = Dense.Ndarray.Z.zeros [|n-5; 1|];;
let _ = Dense.Ndarray.Z.set_slice [[5;n-1];[]] yf z;;
(* book: stock-mask end *)

(* book: stock-inverse begin *)
let y2 = irfft_exact ~axis:0 ~n:(Mat.row_num y) yf;;
(* book: stock-inverse end *)

(* book: convolution begin *)
let y3 = Arr.reshape y [|1; Mat.row_num y; 1|];;
let f3 = Arr.reshape moving_average_filter [|Arr.numel moving_average_filter;1;1|];;
let y3' = Arr.conv1d ~padding:VALID y3 f3 [|1|];;
(* book: convolution end *)
