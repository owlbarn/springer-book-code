#use "topfind";;
#require "owl";;
open Owl;;
Owl_stats_prng.init 103;;

(* book: add-type begin *)
#show Arr.add_;;
(* book: add-type end *)

(* book: sum-array begin *)
let x = Arr.sequential [|3;3;3|];;
let a = Arr.sum x;;
(* book: sum-array end *)

(* book: sum-scalar begin *)
let b = Arr.get a [|0|] in
b +. 10.;;
(* book: sum-scalar end *)

(* book: generic-sum begin *)
open Owl;;

let x = Dense.Matrix.S.eye 5 in
Dense.Matrix.Generic.sum x;;

let x = Dense.Matrix.D.eye 5 in
Dense.Matrix.Generic.sum x;;

let x = Dense.Matrix.C.eye 5 in
Dense.Matrix.Generic.sum x;;

let x = Dense.Matrix.Z.eye 5 in
Dense.Matrix.Generic.sum x;;
(* book: generic-sum end *)

(* book: ext-interoperation begin *)
open Owl.Ext;;

let x = Dense.Matrix.S.uniform 5 5;;
let y = Dense.Matrix.C.uniform 5 5;;
let z = Dense.Matrix.D.uniform 5 5;;

x + F 5.;;
x * C Complex.({re = 2.; im = 3.});;
x - y;;
x / y;;
x *@ y;;

x > z;;
x >. z;;
(x >. z) * x;;
(x >. F 0.5) * x;;
(F 10. * x) + y *@ z;;

round (F 10. * (x *@ z));;
sin (F 5.) * cos (x + z);;
(* book: ext-interoperation end *)
