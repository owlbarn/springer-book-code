open Owl

(* book: fold-sum begin *)
let sum ?axis x = Arr.fold ?axis ( +. ) 0. x
(* book: fold-sum end *)

(* book: reverse begin *)
let reverse x = Mat.get_slice [ [-1;0]; [-1;0] ] x
(* book: reverse end *)

(* book: rotate begin *)
let rotate90 x = Mat.(transpose x |> get_slice [ [];[-1;0] ])
(* book: rotate end *)

(* book: circular-shift begin *)
let cshift x n =
  let columns = Mat.col_num x in
  if columns = 0 then Mat.copy x
  else
    let n = ((n mod columns) + columns) mod columns in
    let indices = List.init columns (fun i -> (i - n + columns) mod columns) in
    Mat.get_fancy [R []; L indices] x
(* book: circular-shift end *)
