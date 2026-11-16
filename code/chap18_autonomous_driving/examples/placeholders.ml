module A = Owl.Dense.Ndarray.S

(* book: replace begin *)
let ids = [|0; 99; 99; 1; 98; 98; 2|]
let table = A.of_array [|1.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 1.|] [|3; 3|]
let images = A.of_array [|10.; 11.; 12.; 20.; 21.; 22.|] [|2; 3|]
let points = A.of_array [|30.; 31.; 32.; 40.; 41.; 42.|] [|2; 3|]
let sequence = A.rows table (Array.map (fun id -> min id 2) ids)

let () =
  let image_index = ref 0 and point_index = ref 0 in
  Array.iteri (fun pos id ->
    let replace values index =
      let row = A.get_slice [[!index]; []] values in
      A.set_slice [[pos]; []] sequence row;
      incr index
    in
    if id = 99 then replace images image_index
    else if id = 98 then replace points point_index) ids
(* book: replace end *)

let () =
  assert (A.shape sequence = [|7; 3|]);
  assert (A.to_array sequence =
    [|1.; 0.; 0.; 10.; 11.; 12.; 20.; 21.; 22.; 0.; 1.; 0.;
      30.; 31.; 32.; 40.; 41.; 42.; 0.; 0.; 1.|]);
  Array.iteri (fun pos id ->
    Printf.printf "id %2d:" id;
    for j = 0 to 2 do Printf.printf " %2.0f" (A.get sequence [|pos; j|]) done;
    print_newline ()) ids
