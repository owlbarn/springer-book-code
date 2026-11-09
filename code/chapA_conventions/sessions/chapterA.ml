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

(* book: explicit-cast begin *)
let x32 = Dense.Matrix.S.eye 3;;
let x64 = Dense.Matrix.Generic.cast_s2d x32;;
let z = Mat.(x64 + ones 3 3);;
(* book: explicit-cast end *)
