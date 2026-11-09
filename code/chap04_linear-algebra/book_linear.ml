open Owl

(* book: pivot-permutation begin *)
let perm_vec_to_mat vec =
  let n = Array.length vec in
  let permutation = Mat.eye n in
  Array.iteri (fun i pivot ->
    let j = pivot - 1 in
    if j < 0 || j >= n then invalid_arg "one-based LU pivot";
    for column = 0 to n - 1 do
      let value = Mat.get permutation i column in
      Mat.set permutation i column (Mat.get permutation j column);
      Mat.set permutation j column value
    done
  ) vec;
  permutation
(* book: pivot-permutation end *)
