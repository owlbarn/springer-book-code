(* [related dataset](https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/data) *)

let data = Owl_io.read_csv ~sep:',' "data_mul.csv"
let data = Array.map (fun x -> Array.map float_of_string x) data |> Mat.of_arrays

let x = Mat.get_slice [[];[0; 1]] data
let y = Mat.get_slice [[];[2]] data

let theta = Regression.D.ols ~i:true x y


let m = Arr.mean ~axis:0 data
let r = Arr.(sub (max ~axis:0 data) (min ~axis:0 data))
let data' = Arr.((data - m) / r)
let x' = Mat.get_slice [[];[0; 1]] data'
let y' = Mat.get_slice [[];[2]] data'
let theta' = Regression.D.ols ~i:true x' y'

let o = Arr.ones [|(Arr.shape x).(0); 1|]
let z = Arr.concatenate ~axis:1 [|o; x'|];;

let solution = Mat.dot (Mat.dot
    (Linalg.D.inv Mat.(dot (transpose z) z)) (Mat.transpose z)) y'