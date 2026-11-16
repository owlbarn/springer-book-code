(** Greedy decoding with a fresh KV cache for this request, followed by the
    route and speed heads. Includes EOS (or the last token at the limit) before
    the driving queries. Returns token IDs, [20,2] route and [10,2] speed points.
    [cgraph] selects the CPU computation-graph Transformer blocks (default false).
    [tap] exports the same first-step tensors as [Simlingo.generate];
    [on_logits] observes every decoding step for numerical verification. *)
val generate :
  ?cgraph:bool ->
  ?tap:(string -> Owl.Dense.Ndarray.S.arr -> unit) ->
  ?on_logits:(int -> Owl.Dense.Ndarray.S.arr -> unit) ->
  max_new_tokens:int ->
  Weights.t ->
  Owl.Dense.Ndarray.S.arr ->
  int array * Owl.Dense.Ndarray.S.arr * Owl.Dense.Ndarray.S.arr
