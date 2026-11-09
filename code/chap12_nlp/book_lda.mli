type lda_typ = SimpleLDA
type model
val init : ?iter:int -> int -> Owl.Nlp.Corpus.t -> model
val train : lda_typ -> model -> unit
val document_topics : model -> float array array
val word_topics : model -> float array array
val token_count : model -> float
