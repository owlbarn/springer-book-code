open Owl

(* book: preprocess begin *)
let simple_process s =
  Str.split Owl_nlp_utils.regexp_split s
  |> List.filter (fun x -> String.length x > 1)
  |> String.concat " "
  |> String.lowercase_ascii
  |> Bytes.of_string

let preprocess input_file =
  let output_file=input_file^".output" in
  Nlp.Corpus.preprocess simple_process input_file output_file;
  output_file
(* book: preprocess end *)

(* book: vocabulary begin *)
let build_vocabulary input_file =
  let vocab=Nlp.Vocabulary.build input_file in
  Nlp.Vocabulary.save vocab (input_file^".vocab");
  vocab
(* book: vocabulary end *)

(* book: print-freq begin *)
let print_freq vocab =
  Nlp.Vocabulary.top vocab 10 |>
  Owl.Utils.Array.to_string ~sep:", " fst
(* book: print-freq end *)

(* book: trim-vocabulary begin *)
let trim_vocabulary vocab = Nlp.Vocabulary.trim_percent ~lo:0.01 ~hi:0.99 vocab
(* book: trim-vocabulary end *)

(* book: tokenise begin *)
let tokenise vocab text =
  Str.split Owl_nlp_utils.regexp_split text
  |> List.filter (Nlp.Vocabulary.exits_w vocab)
  |> List.map (Nlp.Vocabulary.word2index vocab)
(* book: tokenise end *)

(* book: tokenise-all begin *)
let tokenise_all vocab input_file =
  let docs=Owl_utils.Stack.make () in
  Owl_io.iteri_lines_of_file (fun _i line ->
    let tokens=tokenise vocab line |> Array.of_list in
    Owl_utils.Stack.push docs tokens) input_file;
  docs
(* book: tokenise-all end *)

(* book: corpus begin *)
let prepare_corpus input_file output_file =
  let removed=Nlp.Corpus.unique input_file output_file in
  Printf.printf "removed %d duplicates.\n%!" (Array.length removed);
  let corpus=Nlp.Corpus.build ~lo:0.01 ~hi:0.99 output_file in
  Nlp.Corpus.print corpus;
  corpus
(* book: corpus end *)

(* book: term-count begin *)
let term_count table doc =
  Array.iter (fun word ->
    let old=match Hashtbl.find_opt table word with Some value->value | None->0. in
    Hashtbl.replace table word (old+.1.)) doc
(* book: term-count end *)

(* book: bag-of-words begin *)
let build_bow corpus =
  Nlp.Corpus.mapi_tok (fun _ doc ->
    let table=Hashtbl.create 128 in
    term_count table doc;table) corpus
(* book: bag-of-words end *)

(* book: tfidf begin *)
let build_tfidf corpus =
  let tf=Nlp.Tfidf.Count and df=Nlp.Tfidf.Idf in
  let model=Nlp.Tfidf.build ~tf ~df corpus in
  Nlp.Tfidf.save model "news.tfidf";
  model
(* book: tfidf end *)

(* book: tfidf-query begin *)
let sparse_unit vector =
  let length=Array.fold_left (fun sum (_,x)->sum+.x*.x) 0. vector |> sqrt in
  if length=0. then [||] else Array.map (fun (id,x)->id,x/.length) vector

let query_tfidf model text k =
  if k<0 then invalid_arg "query_tfidf k";
  let corpus=Nlp.Tfidf.get_corpus model in
  let vocab=Nlp.Corpus.get_vocab corpus in
  let tokens=tokenise vocab text |> Array.of_list in
  let counts=Hashtbl.create 128 in
  term_count counts tokens;
  let documents=float_of_int (Nlp.Corpus.length corpus) in
  let vector=Hashtbl.fold (fun id count acc ->
    let word=Nlp.Vocabulary.index2word vocab id in
    let df=Nlp.Tfidf.doc_count_of model word in
    let weight=Nlp.Tfidf.term_freq Nlp.Tfidf.Count count (float_of_int (Array.length tokens))
               *. Nlp.Tfidf.doc_freq Nlp.Tfidf.Idf df documents in
    (id,weight)::acc) counts [] |> Array.of_list |> sparse_unit in
  if Array.length vector=0 then [||] else
  let distances=Nlp.Tfidf.mapi (fun i candidate ->
    i,Owl_nlp_similarity.cosine_distance vector (sparse_unit candidate)) model in
  Array.sort (fun (i,a) (j,b)->let c=Float.compare a b in if c=0 then Int.compare i j else c) distances;
  Array.sub distances 0 (min k (Array.length distances))
(* book: tfidf-query end *)

(* book: lda begin *)
let build_lda ?(iter=1000) corpus topics =
  let model=Book_lda.init ~iter topics corpus in
  Book_lda.train Book_lda.SimpleLDA model;
  model
(* book: lda end *)

(* book: get-topics begin *)
let get_topics vocab weights =
  Mat.map_cols (fun column ->
    Mat.top column (min 10 (Mat.row_num column))
    |> Array.map (fun position->Nlp.Vocabulary.index2word vocab position.(0))) weights
(* book: get-topics end *)

(* book: all-distances begin *)
let unit_vector x =
  let norm=Array.fold_left (fun sum x->sum+.x*.x) 0. x |> sqrt in
  if norm=0. then Array.copy x else Array.map (fun value->value/.norm) x

let distance typ x y =
  if Array.length x<>Array.length y then invalid_arg "distance dimensions";
  match typ with
  | Owl_nlp_similarity.Cosine ->
    let x=unit_vector x and y=unit_vector y in
    let dot=ref 0. in Array.iteri (fun i a->dot:= !dot+.a*.y.(i)) x;
    -. (max (-1.) (min 1. !dot))
  | Owl_nlp_similarity.Euclidean ->
    let sum=ref 0. in Array.iteri (fun i a->let d=a-.y.(i) in sum:= !sum+.d*.d) x;
    sqrt !sum
  | Owl_nlp_similarity.KL_D -> invalid_arg "KL_D is not implemented in the upstream helper"

let all_pairwise_distance typ corpus doc =
  let result=Array.mapi (fun i other->i,distance typ doc other) corpus in
  Array.sort (fun (i,a) (j,b)->let c=Float.compare a b in if c=0 then Int.compare i j else c) result;
  result
(* book: all-distances end *)

(* book: linear-query begin *)
let query corpus doc k =
  if k<0 then invalid_arg "query k";
  let result=all_pairwise_distance Owl_nlp_similarity.Cosine corpus doc in
  Array.sub result 0 (min k (Array.length result))
(* book: linear-query end *)

(* book: matrix-query begin *)
let query_matrix corpus doc k =
  if k<0 || Mat.numel doc<>Mat.col_num corpus then invalid_arg "query_matrix";
  if k=0 then [||] else
  let corpus=Mat.to_arrays corpus |> Array.map unit_vector |> Mat.of_arrays in
  let doc=Mat.to_array doc |> unit_vector |> fun x->Mat.of_array x (Array.length x) 1 in
  let scores=Mat.dot corpus doc in
  let result=Mat.top scores (min k (Mat.row_num corpus))
    |> Array.map (fun position->position.(0), -. (Mat.get scores position.(0) 0)) in
  Array.sort (fun (i,a) (j,b)->let c=Float.compare a b in if c=0 then Int.compare i j else c) result;
  result
(* book: matrix-query end *)
