module A = Owl.Dense.Ndarray.S

let print_matrix name x =
  Printf.printf "%s\n" name;
  for i = 0 to (A.shape x).(0) - 1 do
    for j = 0 to (A.shape x).(1) - 1 do
      Printf.printf "%8.3f" (A.get x [|i; j|])
    done;
    print_newline ()
  done

(* book: attention begin *)
let q = A.of_array [|1.; 0.; 0.; 1.; 1.; 1.|] [|3; 2|]
let k = A.copy q
let v = A.of_array [|10.; 0.; 0.; 20.; 6.; 8.|] [|3; 2|]
let scores = A.dot (A.div_scalar q (sqrt 2.)) (A.transpose k)
let weights = A.softmax ~axis:1 scores
let output = A.dot weights v
(* book: attention end *)

let () =
  print_matrix "Scores" scores;
  print_matrix "Weights" weights;
  print_matrix "Output" output;
  for i = 0 to 2 do
    let total = ref 0. in
    for j = 0 to 2 do total := !total +. A.get weights [|i; j|] done;
    assert (abs_float (!total -. 1.) < 1e-6)
  done;
  let a = exp (1. /. sqrt 2.) in
  assert (abs_float (A.get output [|0; 0|] -. 16. *. a /. (2. *. a +. 1.)) < 1e-5)
