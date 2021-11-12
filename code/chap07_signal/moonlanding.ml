open Owl
open Owl_plplot
open ImageUtils

module N = Dense.Ndarray.S 
module C = Dense.Ndarray.C

let img_arr = load_ppm "moonlanding.ppm" |> N.get_slice [[];[];[0]]
let shp = N.shape img_arr
let h, w = shp.(0), shp.(1)
let img = N.reshape img_arr [|h; w|]
    |> Dense.Ndarray.Generic.cast_s2c

let img_fft = Owl_fft.S.fft2 img

(* visualise img_fft *)

let visualise_img img_fft filename = 
    let z = Dense.Ndarray.C.(abs img_fft |> re) 
        |> Dense.Ndarray.Generic.cast_s2d in
    let x, y = Mat.meshgrid 0. 314. 0. 236. 315 237 in
    let h = Plot.create ~m:1 ~n:1 filename in
    Plot.(mesh ~h  ~spec:[ ZLine Y; NoMagColor ]  x y z);
    Plot.set_zrange h 0. 400000.;
    Plot.output h

let _ = visualise_img img_fft "img_fft1.pdf"

let sub_length x frac = (float_of_int x) *. frac |> int_of_float

let h1 = sub_length h 0.1 
let h2 = sub_length h 0.9 
let w1 = sub_length w 0.1 
let w2 = sub_length w 0.9

let index_0 = [ R [h1; h2]; R []]
let index_1 = [ R [0; h1]; R [w1; w2] ]
let index_2 = [ R [h2; h-1]; R [w1; w2] ]

let slice_0 = C.get_fancy index_0 img_fft 
let slice_1 = C.get_fancy index_1 img_fft 
let slice_2 = C.get_fancy index_2 img_fft

let _ = C.set_fancy index_0 img_fft (C.shape slice_0 |> C.zeros) 
let _ = C.set_fancy index_1 img_fft (C.shape slice_1 |> C.zeros) 
let _ = C.set_fancy index_2 img_fft (C.shape slice_2 |> C.zeros)

let _ = visualise_img img_fft "img_fft2.pdf"