#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "str";;
open Owl;;
Owl_stats_prng.init 82;;
#mod_use "book_lda.ml";;
#mod_use "book_nlp.ml";;
let book_matrix_printer formatter value = Owl_pretty.print_dsnda ~max_row:4 ~max_col:8 ~formatter value;;
#install_printer book_matrix_printer;;

(* book: preprocess begin *)
let simple_process = Book_nlp.simple_process
let preprocess = Book_nlp.preprocess;;
(* book: preprocess end *)

(* book: vocabulary begin *)
let build_vocabulary = Book_nlp.build_vocabulary;;
let vocab = Nlp.Vocabulary.load "news.basic.vocab";;
(* book: vocabulary end *)

(* book: print-freq begin *)
let print_freq = Book_nlp.print_freq;;
(* book: print-freq end *)

(* book: frequent begin *)
print_freq vocab;;
(* book: frequent end *)

(* book: rare begin *)
Nlp.Vocabulary.bottom vocab 10 |> Owl.Utils.Array.to_string ~sep:", " fst;;
(* book: rare end *)

(* book: trim-vocabulary begin *)
let trim_vocabulary = Book_nlp.trim_vocabulary;;
(* book: trim-vocabulary end *)

(* book: tokenise begin *)
let tokenise = Book_nlp.tokenise;;
(* book: tokenise end *)

(* book: tokens begin *)
tokenise vocab "this is an owl book";;
(* book: tokens end *)

(* book: tokenise-all begin *)
let tokenise_all = Book_nlp.tokenise_all;;
(* book: tokenise-all end *)

(* book: corpus begin *)
let prepare_corpus = Book_nlp.prepare_corpus;;
let corpus = Nlp.Corpus.load "clean.txt.mdl";;
(* book: corpus end *)

(* book: term-count begin *)
let term_count = Book_nlp.term_count;;
(* book: term-count end *)

(* book: bag-of-words begin *)
let build_bow = Book_nlp.build_bow;;
(* book: bag-of-words end *)

(* book: tf-types begin *)
#show_type Nlp.Tfidf.tf_typ;;
(* book: tf-types end *)

(* book: df-types begin *)
#show_type Nlp.Tfidf.df_typ;;
(* book: df-types end *)

(* book: tfidf begin *)
let build_tfidf = Book_nlp.build_tfidf;;
let model = Nlp.Tfidf.load "news.tfidf";;
(* book: tfidf end *)

(* book: tfidf-query begin *)
let query_tfidf = Book_nlp.query_tfidf;;
(* book: tfidf-query end *)

(* book: knn begin *)
let knn = query_tfidf model (Nlp.Corpus.get corpus 0) 10;;
(* book: knn end *)

(* book: lda begin *)
let build_lda = Book_nlp.build_lda;;
(* book: lda end *)

(* book: document-topics begin *)
let dk = Mat.load "lda-dk.bin";;
(* book: document-topics end *)

(* book: word-topics begin *)
let wk = Mat.load "lda-wk.bin";;
(* book: word-topics end *)

(* book: get-topics begin *)
let get_topics = Book_nlp.get_topics;;
let topic_words = get_topics (Nlp.Corpus.get_vocab corpus) wk;;
(* book: get-topics end *)

(* book: all-distances begin *)
let distance = Book_nlp.distance
let all_pairwise_distance = Book_nlp.all_pairwise_distance;;
(* book: all-distances end *)

(* book: linear-query begin *)
let query = Book_nlp.query;;
(* book: linear-query end *)

(* book: matrix-query begin *)
let query_matrix = Book_nlp.query_matrix;;
(* book: matrix-query end *)

(* book: query-checks begin *)
let a = [|[|1.;0.|];[|0.;2.|];[|3.;0.|]|];;
let by_array = query a [|1.;0.|] 3;;
let by_matrix = query_matrix (Mat.of_arrays a) (Mat.of_array [|1.;0.|] 1 2) 3;;
assert (Array.map snd by_array=Array.map snd by_matrix);;
assert (query_tfidf model "__book_unknown_token__" 10=[||]);;
(* book: query-checks end *)

