(** One request's append-only KV cache, bound to its model weights. *)
type t

val create : capacity:int -> Weights.t -> t
val length : t -> int
val capacity : t -> int
val clear : t -> unit

(** Append a nonempty [n,896] chunk and return its [n,896] hidden states.
    Positions start at the current cache length. Rows in a chunk attend to
    the cached prefix and preceding/current rows of that chunk. *)
val forward :
  ?tap:(string -> Owl.Dense.Ndarray.S.arr -> unit) ->
  t -> Owl.Dense.Ndarray.S.arr -> Owl.Dense.Ndarray.S.arr
