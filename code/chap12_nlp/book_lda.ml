(*
 * OWL - OCaml Scientific Computing
 * Copyright (c) 2016-2022
 *   Ben Catterall <bpwc2@cam.ac.uk>
 *   Liang Wang <liang@ocaml.xyz>
 *)

[@@@warning "-6"]

(** NLP: LDA module *)

type lda_typ = SimpleLDA

type model =
  { mutable n_d : int
  ; (* number of documents *)
    mutable n_k : int
  ; (* number of topics *)
    mutable n_v : int
  ; (* number of vocabulary *)
    mutable alpha : float
  ; (* model hyper-parameters *)
    mutable beta : float
  ; (* model hyper-parameters *)
    mutable alpha_k : float
  ; (* model hyper-parameters *)
    mutable beta_v : float
  ; (* model hyper-parameters *)
    mutable t_dk : float array array
  ; (* document-topic table: num of tokens assigned to each topic in each doc *)
    mutable t_wk : float array array
  ; (* word-topic table: num of tokens assigned to each topic for each word *)
    mutable t__k : float array
  ; (* number of tokens assigned to a topic: k = sum_w t_wk = sum_d t_dk *)
    mutable t__z : int array array
  ; (* table of topic assignment of each token in each document *)
    mutable iter : int
  ; (* number of iterations *)
    mutable data : Owl_nlp_corpus.t
  ; (* training data, tokenised*)
    mutable vocb : (string, int) Hashtbl.t (* vocabulary, or dictionary if you prefer *)
  } [@@warning "-69"]

let include_token m w d k =
  m.t__k.(k) <- m.t__k.(k) +. 1.;
  m.t_wk.(w).(k) <- m.t_wk.(w).(k) +. 1.;
  m.t_dk.(d).(k) <- m.t_dk.(d).(k) +. 1.


let exclude_token m w d k =
  m.t__k.(k) <- m.t__k.(k) -. 1.;
  m.t_wk.(w).(k) <- m.t_wk.(w).(k) -. 1.;
  m.t_dk.(d).(k) <- m.t_dk.(d).(k) -. 1.


let show_info m i t = Owl_log.info "iter#%i t(s):%.3f tokens:%.0f" i t (Array.fold_left (+.) 0. m.t__k)

(* implement several LDA with specific samplings *)

module SimpleLDA = struct
  let init _m = ()

  let sampling m d doc =
    let p = Array.make m.n_k 0. in
    Array.iteri
      (fun i w ->
        let k = m.t__z.(d).(i) in
        exclude_token m w d k;
        (* make cdf function *)
        let x = ref 0. in
        for j = 0 to m.n_k - 1 do
          x
            := !x
               +. ((m.t_dk.(d).(j) +. m.alpha_k)
                  *. (m.t_wk.(w).(j) +. m.beta)
                  /. (m.t__k.(j) +. m.beta_v));
          p.(j) <- !x
        done;
        (* draw a sample *)
        let u = Owl_stats.std_uniform_rvs () *. !x in
        let k = ref 0 in
        while p.(!k) < u do
          k := !k + 1
        done;
        include_token m w d !k;
        m.t__z.(d).(i) <- !k)
      doc
end

let init ?(iter = 100) k d =
  let vocab = Owl_nlp_corpus.get_vocab d in
  let v = Owl_nlp_vocabulary.get_w2i vocab in
  Owl_log.info "init the model";
  (* set basic model stats *)
  let n_d = Owl_nlp_corpus.length d in
  let n_v = Hashtbl.length v in
  let n_k = k in
  (* set model hyper-parameters *)
  let alpha = 50. in
  let beta = 0.1 in
  let alpha_k = alpha /. float_of_int n_k in
  let beta_v = float_of_int n_v *. beta in
  (* init model parameters *)
  let t_dk = Array.init n_d (fun _ -> Array.make n_k 0.) in
  let t_wk = Array.init n_v (fun _ -> Array.make n_k 0.) in
  let t__k = Array.make n_k 0. in
  (* set document data and vocabulary *)
  let data = d in
  let vocb = v in
  (* init a partial model *)
  let m =
    { n_d
    ; n_k
    ; n_v
    ; alpha
    ; beta
    ; alpha_k
    ; beta_v
    ; t_dk
    ; t_wk
    ; t__k
    ; t__z = [||]
    ; iter
    ; data
    ; vocb
    }
  in
  (* randomise the topic assignment for each token *)
  m.t__z
    <- Owl_nlp_corpus.mapi_tok
         (fun i s ->
           Array.init (Array.length s) (fun j ->
               let k' = Owl_stats.uniform_int_rvs ~a:0 ~b:(k - 1) in
               include_token m s.(j) i k';
               k'))
         d;
  m


let train SimpleLDA m =
  SimpleLDA.init m;
  for i=0 to m.iter-1 do
    let started=Unix.gettimeofday () in
    Owl_nlp_corpus.iteri_tok (fun d tokens -> SimpleLDA.sampling m d tokens) m.data;
    show_info m i (Unix.gettimeofday () -. started)
  done

let document_topics m = Array.map Array.copy m.t_dk
let word_topics m = Array.map Array.copy m.t_wk
let token_count m = Array.fold_left (+.) 0. m.t__k
