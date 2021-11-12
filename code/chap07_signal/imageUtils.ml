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
  (* let img = N.load arrname in *)
  (* Assume img is 3d-array of range [0, 255] *)

  (* metadata *)
  let shape = N.shape img in
  assert (Array.length(shape) = 3);
  let h = shape.(0) in
  let w = shape.(1) in
  let num_col = 255 in

  (* divide *)
  let r = N.get_slice [[];[];[1]] img in 
  let r = N.reshape r [|h; w|] in
  let g = N.get_slice [[];[];[2]] img  in
  let g = N.reshape g [|h; w|] in
  let b = N.get_slice [[];[];[0]] img in 
  let b = N.reshape b [|h; w|] in

  (* merge r, g, b to one [|h; 3*w|] matrix *)
  let img_mat = Dense.Matrix.S.zeros h (3 * w) in
  Dense.Matrix.S.set_slice [[];[0;-1;3]] img_mat r;
  Dense.Matrix.S.set_slice [[];[1;-1;3]] img_mat g;
  Dense.Matrix.S.set_slice [[];[2;-1;3]] img_mat b;

  (* rotate *)
  let img_mat = Dense.Matrix.S.rotate img_mat 90 in 
  let img_arr = Dense.Matrix.S.to_arrays img_mat in 

  (* change to line *)
  let img_str = Bytes.make (w * h * 3) ' ' in 
  let ww = 3 * w in 

  for i = 0 to ww - 1 do
    for j = 0 to h - 1 do
      let ch = img_arr.(i).(j) |> int_of_float |> char_of_int in
      Bytes.set img_str ((h - 1 -j) * ww + i) ch;
    done
  done;

  let header = "P6\n" ^ string_of_int(h) ^ " " ^ string_of_int(w) ^ "\n" ^ string_of_int(num_col) ^ "\n" in 
  let img_final = Bytes.concat (Bytes.of_string " ") [header |> Bytes.of_string; img_str] in 
  Owl_io.write_file outname (Bytes.to_string img_final)

let save_ppm fname outfname = 
  let img = N.load fname in 
  let s = N.shape img in
  assert (Array.length s = 4);
  let img = N.reshape img [|s.(2);s.(1);s.(3)|] in
  let img = N.(clip_by_value ~amin:0. ~amax:255. img |> round) in
  save_image_to_file img outfname

let save_ppm_from_arr img outfname =  
  let s = N.shape img in
  assert (Array.length s = 4);
  let img = N.reshape img [|s.(2);s.(1);s.(3)|] in
  let img = N.(clip_by_value ~amin:0. ~amax:255. img |> round) in
  save_image_to_file img outfname

let _read_ppm fname =
  (* grayscale binary ppm reading *)
  let fp  = open_in fname in
  let ver = input_line fp in (* version *)

  if ver <> "P6" then (* expect a .ppm file *)
    raise (Invalid_argument ("Unable to read image: " ^ fname))
  else
    ();

  (* This will skip comments. *)
  let rec ignore_comments_then_get_w_h () =
    let maybe_comment = input_line fp in
    if maybe_comment.[0] = '#' then
      ignore_comments_then_get_w_h ()
    else
      (* This line has the width and height in it *)
      maybe_comment
  in

  (* width, height num colors *)
  let w_h_line = ignore_comments_then_get_w_h () in
  let num_col_line = input_line fp in
  let w, h = Scanf.sscanf w_h_line "%d %d" (fun w h -> w, h) in
  let num_col = Scanf.sscanf num_col_line "%d" (fun n -> n) in

  let img   = Bytes.make (w * h * 3) ' ' in
  let imf_o = Array.make_matrix (w * 3) h 0.0 in

  (* Note that under 32bit OCaml, this will only work when reading strings up
     to ~16 megabytes. *)
  really_input fp img 0 (w * h * 3); (* end of file *)

  close_in fp;

  let ww = 3 * w  in
  for i = 0 to ww - 1 do
    for j = 0 to h - 1 do
      imf_o.(i).(j) <- float_of_int (int_of_char (Bytes.get img ((h - 1 - j ) * ww + i)));
    done
  done;

  imf_o, w, h, num_col
 
let load_ppm fname = 
  let img, w, h, _num_col = _read_ppm fname in 
  let m = Dense.Matrix.S.of_arrays img in
  let m = Dense.Matrix.S.rotate m 270 in
  (* r,g, b: Mat of size h * w *)
  let r = N.get_slice [[];[0;-1;3]] m in
  let g = N.get_slice [[];[1;-1;3]] m in
  let b = N.get_slice [[];[2;-1;3]] m in

  let r' = N.reshape r [|h;w;1|] in
  let g' = N.reshape g [|h;w;1|] in
  let b' = N.reshape b [|h;w;1|] in

  let img = N.zeros [|h;w;3|] in
  N.set_fancy  [R []; R []; I 0] img r';
  N.set_fancy  [R []; R []; I 1] img g';
  N.set_fancy  [R []; R []; I 2] img b';
  img

let extend_dim img = 
  let shape = N.shape img in
  let shape = Array.append [|1|] shape in
  N.reshape img shape

let _ = ()
