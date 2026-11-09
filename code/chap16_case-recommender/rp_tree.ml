(* book: tree-type begin *)
type t =
  | Node of float * t * t
  | Leaf of int array
(* book: tree-type end *)

(* book: median begin *)
let split_space_median space =
  let space_size = Array.length space in
  if space_size < 2 then invalid_arg "At least two points required for splitting";
  let size_of_l = space_size / 2 in
  let size_of_r = space_size - size_of_l in
  let space = Array.copy space in
  Array.sort (fun x y -> Stdlib.compare (snd x) (snd y)) space;
  let median =
    if size_of_l < size_of_r then snd space.(size_of_l)
    else snd space.(size_of_l-1) /. 2. +. snd space.(size_of_l) /. 2.
  in
  let l_subspace = Array.sub space 0 size_of_l in
  let r_subspace = Array.sub space size_of_l size_of_r in
  median, l_subspace, r_subspace
(* book: median end *)

(* book: filter begin *)
let filter_projected_space level projected subspace =
  let plevel = projected.(level) in
  Array.map (fun (doc_id, _) -> doc_id, plevel.(doc_id)) subspace
(* book: filter end *)

(* book: subtree begin *)
let rec make_subtree level projected subspace =
  let num_levels = Array.length projected in
  if level = num_levels || Array.length subspace <= 1 then
    Leaf (Array.map fst subspace)
  else
    let median, l_space, r_space = split_space_median subspace in
    let next space =
      if level < num_levels - 1 then filter_projected_space (level+1) projected space
      else space
    in
    let l_subtree = make_subtree (level+1) projected (next l_space) in
    let r_subtree = make_subtree (level+1) projected (next r_space) in
    Node (median, l_subtree, r_subtree)
(* book: subtree end *)

(* book: grow begin *)
let grow ?documents projected =
  let levels = Array.length projected in
  let count = if levels=0 then Option.value ~default:0 documents
    else Array.length projected.(0) in
  if count < 0 || (match documents with Some n -> n<>count | None -> false) ||
     Array.exists (fun row -> Array.length row<>count || not(Array.for_all Float.is_finite row)) projected then
    invalid_arg "Projection rows must be finite and have the declared document count";
  if levels=0 then Leaf (Array.init count Fun.id)
  else
    let subspace = Array.mapi (fun doc_id x -> doc_id, x) projected.(0) in
    make_subtree 0 projected subspace
(* book: grow end *)

(* book: traverse begin *)
let rec traverse node level x =
  match node with
  | Leaf n -> Array.copy n
  | Node (s,l,r) ->
    if x.(level) < s then traverse l (level+1) x
    else traverse r (level+1) x
(* book: traverse end *)

(* book: leaves begin *)
let rec iter_leaves f = function
  | Leaf n -> f n
  | Node (_,l,r) -> iter_leaves f l; iter_leaves f r

let search_leaves node id =
  let exception Found of int array in
  try
    iter_leaves (fun leaf -> if Array.mem id leaf then raise (Found leaf)) node;
    [||]
  with Found leaf -> Array.copy leaf
(* book: leaves end *)
