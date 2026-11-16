module A = Owl.Dense.Ndarray.S

(* book: merge begin *)
let grid = A.init [|4; 4; 1|] float_of_int
let merged =
  A.reshape grid [|4; 2; 2|]
  |> A.transpose ~axis:[|1; 0; 2|]
  |> fun x -> A.reshape x [|2; 2; 4|]
  |> A.transpose ~axis:[|1; 0; 2|]
  |> fun x -> A.reshape x [|4; 4|]
(* book: merge end *)

let () =
  let expected = [|0.; 1.; 4.; 5.; 2.; 3.; 6.; 7.;
                    8.; 9.; 12.; 13.; 10.; 11.; 14.; 15.|] in
  assert (A.to_array merged = expected);
  for i = 0 to 3 do
    Printf.printf "Block %d:" i;
    for j = 0 to 3 do Printf.printf " %2.0f" (A.get merged [|i; j|]) done;
    print_newline ()
  done
