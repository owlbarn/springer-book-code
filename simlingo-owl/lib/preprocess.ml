module A = Owl.Dense.Ndarray.S

let ppm path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let data = really_input_string ic (in_channel_length ic) in
      let pos = ref 0 and n = String.length data in
      let whitespace = Tokenizer.space in
      let rec skip () =
        if !pos < n && whitespace data.[!pos] then (
          incr pos;
          skip ())
        else if !pos < n && data.[!pos] = '#' then (
          while !pos < n && data.[!pos] <> '\n' do
            incr pos
          done;
          skip ())
      in
      let token () =
        skip ();
        let first = !pos in
        while !pos < n && not (whitespace data.[!pos]) do
          incr pos
        done;
        String.sub data first (!pos - first)
      in
      if token () <> "P6" then invalid_arg "expected binary RGB PPM (P6)";
      let width = int_of_string (token ()) in
      let height = int_of_string (token ()) in
      if width <= 0 || height <= 0 || token () <> "255" then
        invalid_arg "invalid PPM dimensions/maxval";
      if !pos >= n || not (whitespace data.[!pos]) then invalid_arg "missing PPM raster separator";
      (* Consume exactly one separator, preserving pixels equal to whitespace. *)
      if data.[!pos] = '\r' && !pos + 1 < n && data.[!pos + 1] = '\n' then pos := !pos + 2
      else incr pos;
      if n - !pos <> width * height * 3 then invalid_arg "PPM raster length mismatch";
      (width, height, String.sub data !pos (n - !pos)))

let load ?(crop_bottom = 0.) path =
  if crop_bottom < 0. || crop_bottom >= 1. then invalid_arg "crop fraction must be in [0,1)";
  let width, height, data = ppm path in
  let height = max 1 (int_of_float (float height *. (1. -. crop_bottom))) in
  let mean = [| 0.485; 0.456; 0.406 |] and std = [| 0.229; 0.224; 0.225 |] in
  let pixel x y c = float (Char.code data.[(((y * width) + x) * 3) + c]) /. 255. in
  A.init [| 2; 448; 448; 3 |] (fun i ->
      let c = i mod 3 and x = i / 3 mod 448 and y = i / (3 * 448) mod 448 in
      let tile = i / (3 * 448 * 448) in
      let sx =
        max 0.
          (min
             (float (width - 1))
             (((float ((tile * 448) + x) +. 0.5) *. float width /. 896.) -. 0.5))
      in
      let sy =
        max 0. (min (float (height - 1)) (((float y +. 0.5) *. float height /. 448.) -. 0.5))
      in
      let x0 = int_of_float sx and y0 = int_of_float sy in
      let x1 = min (width - 1) (x0 + 1) and y1 = min (height - 1) (y0 + 1) in
      let dx = sx -. float x0 and dy = sy -. float y0 in
      let value =
        ((1. -. dy) *. (((1. -. dx) *. pixel x0 y0 c) +. (dx *. pixel x1 y0 c)))
        +. (dy *. (((1. -. dx) *. pixel x0 y1 c) +. (dx *. pixel x1 y1 c)))
      in
      (value -. mean.(c)) /. std.(c))
