(* book: run-nst begin *)
let run_nst ~src ~style ~dst =
  Neural_transfer.run ~ckpt:50 ~src ~style ~dst 250.
(* book: run-nst end *)
(* book: run-fst begin *)
let run_fst ~src ~dst =
  Fst.list_styles ();
  Fst.run ~style:1 src dst
(* book: run-fst end *)
