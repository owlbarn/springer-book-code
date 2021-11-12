open Owl
open ImageUtils
(* #use "imageUtils.ml" *)

module N = Dense.Ndarray.S 
module M = Dense.Matrix.S

let img_arr = load_ppm "lena.ppm" |> N.get_slice [[];[];[0]]
let shp = N.shape img_arr
let h, w = shp.(0), shp.(1)
let img = N.reshape img_arr [|h; w|]

let u, s, vt = Linalg.S.svd img

let l = (N.shape s).(1)
let n = 50
let s' = N.copy s
let z = N.zeros [|1; l - n|]
let _ = N.set_slice [[];[n; l - 1]] s' z

let img' = M.(dot (dot u (diagm s')) vt)

let image = N.stack ~axis:2 [|img'; img'; img'|]
let image = N.expand image 4
let _ = save_ppm_from_arr image ("lena_" ^ (string_of_int n) ^ ".ppm")