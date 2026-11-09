#use "topfind";;
#require "owl";;
#mod_use "book_introduction.ml";;
open Owl;;
(* book: black-scholes begin *)
let cdf=Book_introduction.cdf;;
let black_scholes=Book_introduction.black_scholes;;
assert(abs_float(black_scholes 0.05 100. 100. 0.25 0.2 -.4.614997129602855)<1e-9);;
(* book: black-scholes end *)
(* book: network begin *)
let make_network=Book_introduction.make_network;;
Owl_stats_prng.init 101;;
let network=make_network [|28;28;1|];;
Neural.S.Graph.init network;;
let probabilities=Neural.S.Graph.model network (Dense.Ndarray.S.zeros [|1;28;28;1|]);;
assert(Dense.Ndarray.S.shape probabilities=[|1;10|]);;
(* book: network end *)
