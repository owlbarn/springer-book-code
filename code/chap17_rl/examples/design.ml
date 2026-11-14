(* Display-only functions, compiled against the installed library. *)
open Owl_rllib
(* book: modules begin *)
module N = Neural.Make (Owl.Neural.S.Graph)
module AD = N.Algodiff
module Stepper = N.Stepper
(* book: modules end *)
(* book: update begin *)
let make_optimiser network =
  Stepper.init (N.adam 0.001) (N.Graph.mkpri network)

let train_batch network observations targets optimiser =
  let predictions, _ = N.Graph.forward network observations in
  let loss = AD.Maths.(mean (sqr (predictions - targets))) in
  N.update network optimiser loss
(* book: update end *)
(* book: clipped begin *)
let clipped_loss ~clip ~log_prob ~old_log_prob ~advantages =
  let open AD in
  let ratio = Maths.exp Maths.(log_prob - old_log_prob) in
  let bounded = Maths.(min2 (max2 ratio (F (1. -. clip))) (F (1. +. clip))) in
  Maths.(neg (mean (min2 (ratio * advantages) (bounded * advantages))))
(* book: clipped end *)
(* book: target begin *)
let target_network online = N.Graph.copy online
(* book: target end *)
