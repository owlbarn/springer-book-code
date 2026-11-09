#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
open Owl;;
let book_matrix_printer formatter value =
  Owl_pretty.print_dsnda ~max_row:8 ~max_col:8 ~formatter value;;
#install_printer book_matrix_printer;;
Owl_stats_prng.init 75;;

(* book: ndarray-type begin *)
#show_type Dense.Ndarray.Generic.t;;
(* book: ndarray-type end *)

(* book: initialisation begin *)
let x = Arr.init [|6;8|] (fun i -> 2. *. (float_of_int i));;
(* book: initialisation end *)

(* book: mapping begin *)
let y = Arr.map (fun a -> a +. 1.) x;;
(* book: mapping end *)

(* book: fold-sum begin *)
#use "book_ndarray.ml";;
(* book: fold-sum end *)

(* book: threshold begin *)
let y = Arr.map (fun a -> if a > 0.5 then a else 0.) x;;
let z = Arr.((x >.$ 0.5) * x);;
(* book: threshold end *)

(* book: sequential begin *)
let x = Arr.sequential [|3;4|];;
(* book: sequential end *)

(* book: tiling begin *)
let y = Arr.tile x [|2;2|];;
(* book: tiling end *)

(* book: repetition begin *)
let z = Arr.repeat x [|2;1|];;
(* book: repetition end *)

(* book: index-type begin *)
#show_type Owl_types.index;;
(* book: index-type end *)

(* book: slicing begin *)
let x = Arr.sequential [|10; 10; 10|];;
Arr.get_slice [ []; [0;8]; [3;9;2] ] x;;
Arr.get_fancy [ R[]; R[0;8]; R[3;9;2] ] x;;
(* book: slicing end *)

(* book: element-index begin *)
open Arr;;
let x = sequential [|10; 10; 10|] in
let a = x.%{2; 3; 4} in
x.%{2; 3; 4} <- 111.;;
(* book: element-index end *)

(* book: fancy-index begin *)
open Arr;;
let x = sequential [|10; 10; 10|] in
let a = x.!{L [2;2;1]; R [6;-1]; I 5} in
let b = zeros (shape a) in
x.!{L [2;2;1]; R [6;-1]; I 5} <- b;;
(* book: fancy-index end *)

(* book: column-slice begin *)
let x = Arr.sequential [|8;8|];;
Arr.get_slice [ []; [2]  ] x;;
Arr.get_fancy [ R[]; I 2 ] x;;
(* book: column-slice end *)

(* book: row-slice begin *)
Arr.get_slice [ [2]; [4; 6] ] x;;
Arr.get_fancy [ I 2; R [4; 6] ] x;;
(* book: row-slice end *)

(* book: selected-slice begin *)
Arr.get_fancy [ L [3; 5]; R [1; 7; 2] ] x;;
(* book: selected-slice end *)

(* book: negative-index begin *)
Arr.get_fancy [ L [-2; -1]; R [-3; -2] ] x;;
(* book: negative-index end *)

(* book: matrix-input begin *)
let x = Mat.sequential 5 5;;
(* book: matrix-input end *)

(* book: reverse begin *)
reverse x;;
(* book: reverse end *)

(* book: rotate begin *)
rotate90 x;;
(* book: rotate end *)

(* book: circular-shift begin *)
let _ = cshift x 0;;
(* book: circular-shift end *)

(* book: shift-example begin *)
cshift x 2;;
(* book: shift-example end *)

(* book: bias-tile begin *)
let x = Mat.uniform 1000 500;;
let v = Mat.uniform 1 500;;
let u = Mat.tile v [|1000;1|];;
Mat.(x + u);;
(* book: bias-tile end *)

(* book: expand begin *)
let y = Arr.sequential [|4;5|];;
let y' = Arr.expand y 4;;
Arr.shape y';;
(* book: expand end *)

(* book: scalar-product begin *)
let a = Arr.sequential [|1;3|];;
Arr.mul_scalar a 3.;;
(* book: scalar-product end *)

(* book: row-broadcast begin *)
let b0 = Arr.sequential [|3;3|];;
let b1 = Arr.sequential ~a:1. [|1;3|];;
Arr.add b0 b1;;
(* book: row-broadcast end *)

(* book: outer-broadcast begin *)
let c0 = Arr.sequential [|3;1|];;
let c1 = Arr.copy b1;;
Arr.add c0 c1;;
(* book: outer-broadcast end *)

(* book: matrix-contraction begin *)
let x = Mat.uniform 3 4;;
let y = Mat.uniform 4 5;;

let z1 = Mat.dot x y;;
let z2 = Arr.contract2 [|(1,0)|] x y;;
(* book: matrix-contraction end *)

(* book: tensor-contraction begin *)
let x = Arr.sequential [|3;4;5|];;
let y = Arr.sequential [|4;3;2|];;
let z1 = Arr.contract2 [|(0, 1); (1, 0)|] x y;;
(* book: tensor-contraction end *)

(* book: manual-contraction begin *)
let z2 = Arr.zeros [|5;2|];;

let _ =
  for h = 0 to 2 do
    for k = 0 to 3 do
      for i = 0 to 4 do
        for j = 0 to 1 do
          let r = (Arr.get x [|h;k;i|]) *.
            (Arr.get y [|k;h;j|]) in
          Arr.set z2 [|i;j|] ((Arr.get z2 [|i;j|]) +. r)
        done
      done
    done
  done;;
(* book: manual-contraction end *)

(* book: trace-contraction begin *)
let x = Arr.sequential [|2;2;3|];;
let y = Arr.contract1 [|(0,1)|] x;;
(* book: trace-contraction end *)

(* book: contraction-check begin *)
assert (Arr.(approx_equal z1 z2));;
(* book: contraction-check end *)
