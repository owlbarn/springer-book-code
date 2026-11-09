open Owl
let require name condition = if not condition then failwith name
let () =
  Owl_stats_prng.init 82;
  let corpus = Nlp.Corpus.build ~lo:0. ~hi:1. "quick-news.txt" in
  let documents = Nlp.Corpus.length corpus in
  require "small corpus retained" (documents >= 20 && documents <= 200);
  let lengths = Nlp.Corpus.mapi_tok (fun _ words -> Array.length words) corpus in
  let tokens = Array.fold_left (+) 0 lengths in
  let tfidf = Book_nlp.build_tfidf corpus in
  require "TFIDF rows" (Nlp.Tfidf.length tfidf = documents);
  let query = Book_nlp.query_tfidf tfidf (Nlp.Corpus.get corpus 0) 5 in
  require "TFIDF actual query" (Array.length query = 5 && Array.for_all (fun (_,x) -> Float.is_finite x) query);
  let lda = Book_nlp.build_lda ~iter:3 corpus 4 in
  let dk = Book_lda.document_topics lda and wk = Book_lda.word_topics lda in
  require "document topics" (Array.length dk = documents && Array.for_all (fun row -> Array.length row = 4) dk);
  require "word topics" (Array.length wk = Nlp.Vocabulary.length (Nlp.Corpus.get_vocab corpus));
  Array.iteri (fun i row ->
    require "finite nonnegative counts" (Array.for_all (fun x -> Float.is_finite x && x >= 0.) row);
    require "document token conservation" (Array.fold_left (+.) 0. row = float_of_int lengths.(i))) dk;
  let sum = Array.fold_left (fun total row -> total +. Array.fold_left (+.) 0. row) 0. wk in
  require "word token conservation" (sum = float_of_int tokens);
  require "model token conservation" (Book_lda.token_count lda = float_of_int tokens);
  Printf.printf "PASS: QUICK NLP: %d documents, %d conserved tokens, 3 real LDA iterations and TFIDF queries\n%!" documents tokens
