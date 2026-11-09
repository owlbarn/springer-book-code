open Owl
open Book_recommender
module M = Dense.Matrix.D
let checks = ref 0
let check name ok = incr checks; if not ok then failwith name
let close x y = abs_float(x-.y)<1e-10
let () =
  let input=M.of_arrays [|[|1.;0.;2.|];[|0.;3.;0.|];[|4.;5.;6.|];[|1.;1.;1.|];[|2.;3.;4.|];[|0.;0.;0.|];[|-1.;2.;1.|];[|3.;1.;2.|]|] in
  let n,levels,projected,projection=Dense_projection.random 96 2. input in
  check "dense projection dimensions" (n=3 && levels=2 && M.shape projected=(2,8) && M.shape projection=(3,2));
  let _,_,again,_=Dense_projection.random 96 2. input in
  check "seed reproducibility" (M.to_array projected=M.to_array again);
  let _,_,other,_=Dense_projection.random 97 2. input in
  check "different seed changes projection" (M.to_array projected<>M.to_array other);
  let _,depth,small,_=Dense_projection.random 96 10. input in
  check "actual small dense corpus has zero projection depth" (depth=0 && M.shape small=(0,8));
  check "zero-depth dense result retains all identifiers" (Rp_tree.search_leaves (Rp_tree.grow ~documents:8 (M.to_arrays small)) 7=Array.init 8 Fun.id);
  let _,depth,empty,_=Dense_projection.random 96 2. (M.zeros 0 3) in
  check "empty dense projection" (depth=0 && M.shape empty=(0,0));
  let sparse_projection=Sparse_projection.make_projection_matrix 96 3 levels in
  let sparse_result=Sparse_projection.make_projected_matrix levels 8 in
  Array.iteri (fun i row ->
    let entries=Array.mapi (fun j value->j,value) row |> Array.to_list |> List.filter(fun (_,v)->v<>0.) |> Array.of_list in
    for j=0 to levels-1 do Sparse_projection.project i j entries sparse_projection sparse_result done) (M.to_arrays input);
  check "every sparse dot product equals dense BLAS result" (Array.for_all2 (Array.for_all2 close) sparse_result (M.to_arrays projected));
  check "projected rows do not alias" (sparse_result.(0)!=sparse_result.(1));
  let tree=Rp_tree.grow (M.to_arrays projected) in
  let leaves=ref [] in Rp_tree.iter_leaves (fun leaf->leaves:=leaf::!leaves) tree;
  let ids=Array.concat !leaves |> Array.to_list |> List.sort compare in
  check "each source point occurs exactly once" (ids=List.init 8 Fun.id);
  check "balanced requested leaf size" (List.length !leaves=4 && List.for_all(fun leaf->Array.length leaf=2) !leaves);
  for id=0 to 7 do
    let point=M.col projected id |> M.to_array in
    let routed=Rp_tree.traverse tree 0 point in
    check "query reaches its own leaf on distinct projections" (Array.mem id routed);
    check "leaf lookup agrees with geometric routing" (Rp_tree.search_leaves tree id=routed)
  done;
  let found=Rp_tree.search_leaves tree 0 in found.(0)<-999;
  check "returned leaf mutation cannot alter tree" (not(Array.mem 999 (Rp_tree.search_leaves tree 0)));
  check "missing identifier" (Rp_tree.search_leaves tree 999=[||]);
  let median,left,right=Rp_tree.split_space_median [|0,8.;1,2.;2,4.;3,6.|] in
  check "even median and rank split" (median=5. && Array.map snd left=[|2.;4.|] && Array.map snd right=[|6.;8.|]);
  let median,_,_=Rp_tree.split_space_median [|0,3.;1,1.;2,2.|] in check "odd median" (median=2.);
  let median,_,_=Rp_tree.split_space_median [|0,Float.max_float;1,Float.max_float|] in
  check "median does not overflow finite equal maxima" (median=Float.max_float);
  let singleton=Rp_tree.grow [|[|1.|];[|2.|];[|3.|]|] in
  check "singleton stops before empty splitting" (Rp_tree.search_leaves singleton 0=[|0|]);
  check "zero-depth tree keeps every document" (Rp_tree.search_leaves (Rp_tree.grow ~documents:3 [||]) 2=[|0;1;2|]);
  check "empty tree" (Rp_tree.traverse (Rp_tree.grow [||]) 0 [||]=[||]);
  check "invalid cluster rejected" (try ignore(Projection_depth.levels 8 0.);false with Invalid_argument _->true);
  check "no negative projection depth for small corpus" (Projection_depth.levels 3 10.=0);
  check "ragged projections rejected" (try ignore(Rp_tree.grow [|[|1.;2.|];[|1.|]|]);false with Invalid_argument _->true);
  check "nonfinite projections rejected" (try ignore(Rp_tree.grow [|[|nan|]|]);false with Invalid_argument _->true);
  let repeated=Rp_tree.grow [|Array.make 8 1.;Array.make 8 1.|] in
  let count=ref 0 in Rp_tree.iter_leaves (fun leaf -> count:= !count+Array.length leaf) repeated;
  check "duplicate coordinates retained" (!count=8);
  check "duplicate query still returns equivalent points" (Array.length(Rp_tree.traverse repeated 0 [|1.;1.|])=2);
  let lines=[|"ocaml owl matrix numerical algebra";"matrix algebra vectors projection index";"neural network image style feature";"text corpus document query index";"ocaml functional program numerical computing";"tree vector random projection leaf"|] in
  Owl_io.write_file "fixture.txt" (String.concat "\n" (Array.to_list lines)^"\n");
  let corpus=Nlp.Corpus.build ~stopwords:(Hashtbl.create 1) ~lo:0. ~hi:1. ~minlen:1 "fixture.txt" in
  let tfidf=Nlp.Tfidf.build ~norm:true ~sort:true corpus in
  let docs=Nlp.Tfidf.length tfidf and vocabulary=Nlp.Tfidf.vocab_len tfidf in
  check "real TFIDF fixture retained documents" (docs=6 && vocabulary>0);
  let dense=M.zeros docs vocabulary in
  Nlp.Tfidf.reset_iterators tfidf;
  Nlp.Tfidf.iteri(fun i entries->Array.iter(fun(j,value)->M.set dense i j value) entries) tfidf;
  let _,depth,sparse=Sparse_projection.random 96 2. tfidf in
  let _,_,dense_projected,_=Dense_projection.random 96 2. dense in
  check "full sparse TFIDF projector equals dense projector" (Array.for_all2 (Array.for_all2 close) sparse (M.to_arrays dense_projected));
  let _,_,repeat=Sparse_projection.random 96 2. tfidf in
  check "sparse iterator reset makes repeat reproducible" (repeat=sparse);
  check "real TFIDF dimensions" (depth=2);
  Owl_io.write_file "native-metrics.json" (Printf.sprintf "{\"checks\":%d,\"fixture_documents\":%d,\"fixture_vocabulary\":%d,\"depth\":%d,\"scope\":\"Original OCaml projection and RP-tree excerpts; deterministic verification fixtures, not historical Sofia service or corpus benchmarks\"}\n" !checks docs vocabulary depth);
  Printf.printf "PASS: %d dense/sparse projection and RP-tree property checks\n%!" !checks
