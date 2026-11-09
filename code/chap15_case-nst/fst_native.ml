open Owl
open Book_style
module N = Dense.Ndarray.S
let checks = ref 0
let check name ok = incr checks; if not ok then failwith name
let () =
  let weights = Sys.argv.(1) and inputs = Sys.argv.(2) in
  let quick = Array.length Sys.argv > 3 && Sys.argv.(3) = "--quick" in
  let size = if quick then 32 else 256 in
  let sample = Style_io.load ~size:(size,size) (Filename.concat inputs "IMG_6754.png") in
  let sums = Array.mapi (fun style name ->
    let before = Unix.gettimeofday () in
    let result = Fst.infer ~weights ~style sample in
    check "sample FST shape" (N.shape result = [|1;size;size;3|]);
    check "sample FST finite" (Array.for_all Float.is_finite (N.to_array result));
    Style_io.save result ("fst-" ^ name ^ ".png");
    let sum = N.sum' result in
    Printf.printf "FST %s sum=%.9g seconds=%.3f\n%!" name sum (Unix.gettimeofday () -. before);
    sum) Fst.styles in
  Array.iteri (fun i sum -> Array.iteri (fun j other -> if i<>j then check "Different style results" (abs_float(sum-.other)>=1.)) sums) sums;
  let odd = Fst.infer ~weights ~style:0 (N.ones [|1;25;31;3|]) in
  check "non-multiple input shape" (N.shape odd = [|1;25;31;3|]);
  check "non-multiple finite" (Array.for_all Float.is_finite (N.to_array odd));
  if quick then (Printf.printf "PASS: QUICK FST: six original models, odd dimensions, finite/distinct outputs (%d checks)\n%!" !checks; exit 0);
  Fst.run ~weights ~style:1 (Filename.concat inputs "hojo.png") "fst-public.png";
  check "public FST dimensions" (N.shape (Style_io.load "fst-public.png")=[|1;556;556;3|]);
  let invalid = try ignore(Fst.infer ~weights ~style:6 sample);false with Invalid_argument _->true in
  check "invalid style rejected" invalid;
  let original = Style_io.load (Filename.concat inputs "IMG_6754.png") in
  for style = 0 to 1 do
    let before = Unix.gettimeofday () in
    let result = Fst.infer ~weights ~style original in
    check "book FST shape" (N.shape result = N.shape original);
    check "book FST finite" (Array.for_all Float.is_finite (N.to_array result));
    Style_io.save result (Printf.sprintf "6754_%d.png" style);
    Printf.printf "FST original style=%d seconds=%.3f\n%!" style (Unix.gettimeofday ()-.before)
  done;
  Owl_io.write_file "fst-metrics.json" (Printf.sprintf "{\"models\":6,\"sample_shape\":[1,256,256,3],\"book_shape\":[1,1500,1500,3],\"odd_shape\":[1,25,31,3],\"distinct_style_checks\":30,\"checks\":%d,\"sums\":[%s]}\n" !checks (String.concat "," (Array.to_list (Array.map (Printf.sprintf "%.17g") sums))));
  print_endline "PASS: six original FST models and two original-size book images"
