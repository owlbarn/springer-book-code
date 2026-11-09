(* Reader for the pinned first-edition primal-weight schema, run only by the
   isolated OCaml 4.08.1 migration tool. The reader image uses current OCaml. *)
open Bigarray
type array32 = (float,float32_elt,c_layout) Genarray.t
type primal = F of float | Arr of array32
let output_u32 channel bits =
  for shift=0 to 3 do
    output_byte channel Int32.(to_int (logand (shift_right_logical bits (8*shift)) 255l))
  done
let write_npy filename array =
  if Genarray.kind array<>Float32 || Genarray.layout array<>C_layout then failwith "Unexpected weight dtype/layout";
  let dims=Genarray.dims array in
  let shape=Array.to_list dims |> List.map string_of_int |> String.concat ", " in
  let header="{'descr': '<f4', 'fortran_order': False, 'shape': ("^shape^",), }" in
  let padding=(16-((10+String.length header+1) mod 16)) mod 16 in
  let header=header^String.make padding ' '^"\n" in
  let channel=open_out_bin filename in
  output_string channel "\x93NUMPY\x01\x00";
  output_byte channel (String.length header land 255);output_byte channel (String.length header lsr 8);
  output_string channel header;
  let size=Array.fold_left ( * ) 1 dims in
  let flat=reshape_1 array size in
  for i=0 to size-1 do
    let value=Array1.get flat i in
    (match classify_float value with FP_nan|FP_infinite->failwith "Nonfinite model weight"|_->());
    output_u32 channel (Int32.bits_of_float value)
  done;
  close_out channel;size
let () =
  let input=open_in_bin Sys.argv.(1) in
  let table:(string,primal array) Hashtbl.t=input_value input in
  close_in input;
  let directory=Sys.argv.(2) in
  let entries=Hashtbl.fold (fun name arrays acc->(name,arrays)::acc) table [] |> List.sort (fun (a,_) (b,_)->String.compare a b) in
  let output=open_out (Filename.concat directory "weights.tsv") in
  let total=ref 0 and tensors=ref 0 in
  List.iteri (fun node (name,arrays)->
    if String.contains name '\t' || String.contains name '\n' then failwith "Invalid node name";
    output_string output name;
    Array.iteri (fun parameter -> function
      | F _ -> failwith "Unexpected scalar parameter"
      | Arr array ->
        let filename=Printf.sprintf "node-%03d-param-%d.npy" node parameter in
        total:= !total+write_npy (Filename.concat directory filename) array;
        incr tensors;output_char output '\t';output_string output filename) arrays;
    output_char output '\n') entries;
  close_out output;
  Printf.printf "PASS: migrated %d nodes, %d float32 tensors, %d finite parameters\n%!" (List.length entries) !tensors !total
