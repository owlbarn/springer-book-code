open Owl
module N = Dense.Ndarray.S
let convert arguments =
  let command = String.concat " " (List.map Filename.quote ("convert" :: arguments)) in
  if Sys.command command <> 0 then failwith "ImageMagick conversion failed"
let with_ppm f =
  let path = Filename.temp_file "book-style-" ".ppm" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)
let load ?size path = with_ppm (fun temporary ->
  let resize = match size with None -> [] | Some (width,height) ->
    ["-resize";Printf.sprintf "%dx%d!" width height] in
  convert ([path] @ resize @ ["-strip";"-depth";"8";"PPM:" ^ temporary]);
  ImageUtils.(load_ppm temporary |> extend_dim))
let save ?(preprocessed=false) array path = with_ppm (fun temporary ->
  let array = if preprocessed then ImageUtils.postprocess array else array in
  ImageUtils.save_ppm_from_arr array temporary;
  convert [temporary;path])
