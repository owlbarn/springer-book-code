open Owl
module Mat = Dense.Matrix.D
(* book: dense-random begin *)
let make_projection_matrix seed m n =
  Owl_stats_prng.init seed;
  Mat.gaussian m n

let random seed cluster data =
  let m = Mat.row_num data in
  let n = Mat.col_num data in
  let level = Projection_depth.levels m cluster in
  let projection = make_projection_matrix seed n level in
  let projected = if level=0 then Mat.zeros 0 m
    else Mat.dot data projection |> Mat.transpose in
  n, level, projected, projection
(* book: dense-random end *)
