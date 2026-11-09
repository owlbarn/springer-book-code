(* #!/usr/bin/env owl *)

open Owl

module N = Dense.Ndarray.S

let preprocess img = 
  let img = N.copy img in (*!!!*)
  let r = N.get_slice [[];[];[];[0]] img in 
  let r = N.sub_scalar r 123.68 in 
  N.set_slice [[];[];[];[0]] img r;

  let g = N.get_slice [[];[];[];[1]] img in 
  let g = N.sub_scalar g 116.779 in 
  N.set_slice [[];[];[];[1]] img g;

  let b = N.get_slice [[];[];[];[2]] img in 
  let b = N.sub_scalar b 103.939 in 
  N.set_slice [[];[];[];[2]] img b;
  img
  
let postprocess img = 
  let img = N.copy img in (*!!!*)
  let r = N.get_slice [[];[];[];[0]] img in 
  let r = N.add_scalar r 123.68 in 
  N.set_slice [[];[];[];[0]] img r;

  let g = N.get_slice [[];[];[];[1]] img in 
  let g = N.add_scalar g 116.779 in 
  N.set_slice [[];[];[];[1]] img g;

  let b = N.get_slice [[];[];[];[2]] img in 
  let b = N.add_scalar b 103.939 in 
  N.set_slice [[];[];[];[2]] img b;
  img

let normalise img = 
  let img = N.div_scalar img 255. in 
  let img = N.sub_scalar img 0.5  in
  let img = N.mul_scalar img 2.   in
  img

let normalise_to_range a b img=
  let max_val = N.max' img in
  let min_val = N.min' img in
  let img = N.sub_scalar img min_val in
  let img = N.div_scalar img (max_val -. min_val) in
  let img = N.mul_scalar img (b -. a) in
  let img = N.add_scalar img a in
  let img = N.round img in 
  img

let save_image_to_file img outname =
  let shape = N.shape img in
  if Array.length shape <> 3 || shape.(2) <> 3 then
    invalid_arg "PPM expects height x width x 3";
  let height, width = shape.(0), shape.(1) in
  let values = N.to_array img in
  let payload = Bytes.init (height * width * 3) (fun i ->
    let value = max 0. (min 255. (Float.round values.(i))) in
    char_of_int (int_of_float value)) in
  let channel = open_out_bin outname in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
    Printf.fprintf channel "P6\n%d %d\n255\n" width height;
    output_bytes channel payload)

let save_ppm fname outfname = 
  let img = N.load fname in 
  let s = N.shape img in
  assert (Array.length s = 4);
  let img = N.reshape img [|s.(1);s.(2);s.(3)|] in
  let img = N.(clip_by_value ~amin:0. ~amax:255. img |> round) in
  save_image_to_file img outfname

let save_ppm_from_arr img outfname =  
  let s = N.shape img in
  assert (Array.length s = 4);
  let img = N.reshape img [|s.(1);s.(2);s.(3)|] in
  let img = N.(clip_by_value ~amin:0. ~amax:255. img |> round) in
  save_image_to_file img outfname

let _read_ppm fname =
  let channel = open_in_bin fname in
  Fun.protect ~finally:(fun () -> close_in channel) (fun () ->
    if input_line channel <> "P6" then invalid_arg "Expected binary PPM";
    let rec header_line () =
      let line = input_line channel |> String.trim in
      if line = "" || line.[0] = '#' then header_line () else line
    in
    let width, height = Scanf.sscanf (header_line ()) "%d %d" (fun w h -> w,h) in
    let maximum = int_of_string (header_line ()) in
    if width <= 0 || height <= 0 || maximum <> 255 then invalid_arg "Unsupported PPM header";
    let bytes = really_input_string channel (width * height * 3) in
    bytes, width, height)

let load_ppm fname =
  let bytes, width, height = _read_ppm fname in
  N.of_array (Array.init (String.length bytes) (fun i -> float_of_int (Char.code bytes.[i])))
    [|height; width; 3|]

let extend_dim img = 
  let shape = N.shape img in
  let shape = Array.append [|1|] shape in
  N.reshape img shape

let _ = ()
