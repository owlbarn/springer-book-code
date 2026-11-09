open Owl
module Mat = Dense.Matrix.D
(* book: sparse-matrices begin *)
let make_projection_matrix seed m n =
  Owl_stats_prng.init seed;
  Mat.gaussian m n |> Mat.to_arrays

let make_projected_matrix m n =
  Array.init m (fun _ -> Array.make n 0.)
(* book: sparse-matrices end *)

(* book: project begin *)
let project i j s projection projected =
  let r = ref 0. in
  Array.iter (fun (w, a) ->
    r := !r +. a *. projection.(w).(j)) s;
  projected.(j).(i) <- !r
(* book: project end *)

(* book: sparse-random begin *)
let random seed cluster tfidf =
  let num_doc = Nlp.Tfidf.length tfidf in
  let vocab_len = Nlp.Tfidf.vocab_len tfidf in
  let level = Projection_depth.levels num_doc cluster in
  let projection = make_projection_matrix seed vocab_len level in
  let projected = make_projected_matrix level num_doc in
  Nlp.Tfidf.reset_iterators tfidf;
  Nlp.Tfidf.iteri (fun i s ->
    for j = 0 to level - 1 do
      project i j s projection projected
    done) tfidf;
  vocab_len, level, projected
(* book: sparse-random end *)
