open Owl

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


let () =
  let reference=Mat.(Linalg.D.pinv z *@ y') in
  assert (Mat.(l2norm' (solution-reference)) < 1e-9);
  assert (Mat.(l2norm' (transpose z *@ (z *@ solution-y'))) < 1e-10);
  let raw_design=Mat.concat_horizontal x (Mat.ones (Mat.row_num x) 1) in
  let raw_solution=Mat.(Linalg.D.pinv raw_design *@ y) in
  let restored_prediction=Mat.((z *@ solution) *$ (get r 0 2) +$ (get m 0 2)) in
  assert (Mat.(l2norm' (raw_design *@ raw_solution-restored_prediction)) < 1e-6);
  Printf.printf "PASS: 3 native multiple-regression checks\n"
