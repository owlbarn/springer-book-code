open Owl
let checks=ref 0
let check label ok=incr checks;if not ok then failwith label
let near label expected value tol=check label (Float.is_finite value && abs_float(expected-.value)<tol)
let count_lines fname=let count=ref 0 in Owl_io.iteri_lines_of_file (fun _ _->incr count) fname;!count
let () =
  Owl_stats_prng.init 82;
  let input=if Array.length Sys.argv>1 then Sys.argv.(1) else "news.txt" in
  let iterations=if Array.length Sys.argv>2 then int_of_string Sys.argv.(2) else 1000 in
  check "preprocessing string" (Bytes.to_string (Book_nlp.simple_process "A NEW, Owl! book.")="new owl book");
  let processed=Book_nlp.preprocess input in
  let basic_vocab=Book_nlp.build_vocabulary processed in
  Nlp.Vocabulary.save basic_vocab "news.basic.vocab";
  let trimmed=Book_nlp.trim_vocabulary basic_vocab in
  check "frequency trim actually reduces vocabulary" (Nlp.Vocabulary.length trimmed<Nlp.Vocabulary.length basic_vocab);
  let tokens=Book_nlp.tokenise basic_vocab "this is an owl book" in
  check "known phrase token count" (List.length tokens=5);
  check "vocabulary roundtrip" (List.map (Nlp.Vocabulary.index2word basic_vocab) tokens=["this";"is";"an";"owl";"book"]);
  check "unknown words filtered" (Book_nlp.tokenise basic_vocab "__book_unknown_token__"=[]);
  let tokenised_count=Book_nlp.tokenise_all basic_vocab processed |> Owl_utils.Stack.to_array |> Array.length in
  check "all documents tokenised" (tokenised_count=count_lines input);
  print_endline "BOOK_CORPUS_BEGIN";
  let corpus=Book_nlp.prepare_corpus input "clean.txt" in
  print_endline "BOOK_CORPUS_END";
  Nlp.Corpus.save corpus "clean.txt.mdl";
  let documents=Nlp.Corpus.length corpus in
  let vocab=Nlp.Corpus.get_vocab corpus in
  let vocabulary=Nlp.Vocabulary.length vocab in
  let lengths=Nlp.Corpus.mapi_tok (fun _ words->Array.length words) corpus in
  let tokens=Array.fold_left (+) 0 lengths in
  check "every retained document meets minlen" (Array.for_all (fun n->n>=10) lengths);
  check "corpus mapping count" (Array.length (Nlp.Corpus.get_docid corpus)=documents);
  check "retained documents within full input" (documents>100000 && documents<=tokenised_count);
  Hashtbl.iter (fun id word ->
    if id<0 || id>=vocabulary || Nlp.Vocabulary.word2index vocab word<>id then failwith "vocabulary index") (Nlp.Vocabulary.get_i2w vocab);
  check "vocabulary indices contiguous" true;
  let bow_count=Book_nlp.build_bow corpus |> Array.length in
  check "one bag per document" (bow_count=documents);
  let histogram=Hashtbl.create 3 in Book_nlp.term_count histogram [|1;2;1|];
  near "term repeated count" 2. (Hashtbl.find histogram 1) 0.000001;
  let model=Book_nlp.build_tfidf corpus in
  check "TFIDF document count" (Nlp.Tfidf.length model=documents);
  let restored=Nlp.Tfidf.load "news.tfidf" in
  let self=Book_nlp.query_tfidf restored (Nlp.Corpus.get corpus 0) 10 in
  check "self query first document" (Array.length self=10 && fst self.(0)=0);
  near "self negative cosine" (-1.) (snd self.(0)) 1e-12;
  check "unknown TFIDF query empty" (Book_nlp.query_tfidf model "__book_unknown_token__" 10=[||]);
  check "zero k empty" (Book_nlp.query_tfidf model "owl book" 0=[||]);
  near "negative cosine scale invariant" (-1.) (Book_nlp.distance Owl_nlp_similarity.Cosine [|1.;2.|] [|10.;20.|]) 1e-12;
  near "orthogonal cosine" 0. (Book_nlp.distance Owl_nlp_similarity.Cosine [|1.;0.|] [|0.;1.|]) 1e-12;
  near "zero vector finite" 0. (Book_nlp.distance Owl_nlp_similarity.Cosine [|0.;0.|] [|1.;2.|]) 1e-12;
  near "Euclidean known vector" 5. (Book_nlp.distance Owl_nlp_similarity.Euclidean [|0.;0.|] [|3.;4.|]) 1e-12;
  Owl_stats_prng.init 82;
  let started=Unix.gettimeofday () in
  let lda=Book_nlp.build_lda ~iter:iterations corpus 8 in
  let lda_seconds=Unix.gettimeofday ()-.started in
  let dk=Book_lda.document_topics lda and wk=Book_lda.word_topics lda in
  check "document topic shape" (Array.length dk=documents && Array.for_all (fun row->Array.length row=8) dk);
  check "word topic shape" (Array.length wk=vocabulary && Array.for_all (fun row->Array.length row=8) wk);
  check "document tokens conserved" (Array.for_all Fun.id (Array.mapi (fun i row->Array.fold_left (+.) 0. row=float_of_int lengths.(i)) dk));
  let total a=Array.fold_left (fun sum row->sum+.Array.fold_left (+.) 0. row) 0. a in
  near "word tokens conserved" (float_of_int tokens) (total wk) 0.0001;
  near "model tokens conserved" (float_of_int tokens) (Book_lda.token_count lda) 0.0001;
  check "nonnegative document counts" (Array.for_all (Array.for_all (fun x->Float.is_finite x && x>=0.)) dk);
  check "nonnegative word counts" (Array.for_all (Array.for_all (fun x->Float.is_finite x && x>=0.)) wk);
  let dk=Mat.of_arrays dk and wk=Mat.of_arrays wk in
  Mat.save ~out:"lda-dk.bin" dk;Mat.save ~out:"lda-wk.bin" wk;
  let topics=Book_nlp.get_topics vocab wk in
  check "eight topic word lists" (Array.length topics=8 && Array.for_all (fun words->Array.length words=10) topics);
  check "nonempty topic names" (Array.for_all (Array.for_all (fun w->String.length w>0)) topics);
  let dense=Mat.to_arrays dk in
  let by_array=Book_nlp.query dense dense.(0) 10 in
  let by_matrix=Book_nlp.query_matrix dk (Mat.row dk 0) 10 in
  check "dense k result count" (Array.length by_matrix=10);
  Array.iteri (fun i (_,score)->near "array/matrix cosine ranking" (snd by_array.(i)) score 1e-12) by_matrix;
  check "oversized k clamped" (Array.length (Book_nlp.query [|[|1.;0.|]|] [|1.;0.|] 5)=1);
  let channel=open_out "native-metrics.json" in
  Printf.fprintf channel "{\"iterations\":%d,\"documents\":%d,\"vocabulary\":%d,\"tokens\":%d,\"checks\":%d,\"lda_seconds\":%.9g}\n"
    iterations documents vocabulary tokens !checks lda_seconds;
  close_out channel;
  Printf.printf "PASS: %d full-input NLP checks; %d LDA iterations in %.3f seconds\n%!" !checks iterations lda_seconds
