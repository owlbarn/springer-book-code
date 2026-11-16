(** Qwen2 Transformer blocks evaluated with Owl's CPU computation graph and a
    per-request KV cache. The final RMSNorm and logits use the ndarray helpers. *)
type t
val create : capacity:int -> Weights.t -> t
val length : t -> int
val capacity : t -> int
val clear : t -> unit
val forward :
  ?tap:(string -> Owl.Dense.Ndarray.S.arr -> unit) ->
  t -> Owl.Dense.Ndarray.S.arr -> Owl.Dense.Ndarray.S.arr
