#use "topfind";;
#require "owl";;
#use "book-dependencies.ml";;
open Owl;;
open Book_recommender;;
module Mat = Dense.Matrix.D;;
Owl_stats_prng.init 96;;

(* book: sparse-matrices begin *)
let make_projection_matrix = Sparse_projection.make_projection_matrix;;
let make_projected_matrix = Sparse_projection.make_projected_matrix;;
(* book: sparse-matrices end *)

(* book: project begin *)
let project = Sparse_projection.project;;
let projected = make_projected_matrix 2 1;;
project 0 0 [|0,2.;1,3.|] [|[|1.;0.|];[|2.;1.|]|] projected;;
assert(projected.(0).(0)=8.);;
(* book: project end *)

(* book: sparse-random begin *)
let sparse_random = Sparse_projection.random;;
let corpus=Nlp.Corpus.build ~stopwords:(Hashtbl.create 1) ~lo:0. ~hi:1. ~minlen:1 "fixture.txt";;
let tfidf=Nlp.Tfidf.build ~norm:true ~sort:true corpus;;
let _,_,sparse=sparse_random 96 2. tfidf;;
assert(Array.length sparse=2);;
(* book: sparse-random end *)

(* book: dense-random begin *)
let dense_random = Dense_projection.random;;
let data=Mat.of_arrays [|[|1.;0.|];[|0.;1.|];[|2.;1.|];[|1.;2.|]|];;
let _,levels,matrix,projection=dense_random 96 1. data;;
assert(levels=2 && Mat.shape matrix=(2,4));;
(* book: dense-random end *)

(* book: tree-type begin *)
#show_type Rp_tree.t;;
(* book: tree-type end *)

(* book: median begin *)
let split_space_median=Rp_tree.split_space_median;;
let median,_,_=split_space_median [|0,2.;1,4.;2,6.;3,8.|];;
assert(median=5.);;
(* book: median end *)

(* book: filter begin *)
let filter_projected_space=Rp_tree.filter_projected_space;;
assert(filter_projected_space 1 [|[|2.;4.|];[|6.;8.|]|] [|1,4.|]=[|1,8.|]);;
(* book: filter end *)

(* book: subtree begin *)
let make_subtree=Rp_tree.make_subtree;;
(* book: subtree end *)

(* book: grow begin *)
let grow=Rp_tree.grow;;
let tree=grow (Mat.to_arrays matrix);;
(* book: grow end *)

(* book: traverse begin *)
let traverse=Rp_tree.traverse;;
let leaf=traverse tree 0 (Mat.col matrix 0 |> Mat.to_array);;
assert(Array.mem 0 leaf);;
(* book: traverse end *)

(* book: leaves begin *)
let iter_leaves=Rp_tree.iter_leaves;;
let search_leaves=Rp_tree.search_leaves;;
assert(search_leaves tree 0=leaf);;
(* book: leaves end *)
