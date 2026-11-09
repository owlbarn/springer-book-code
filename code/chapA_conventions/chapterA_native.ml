open Owl
let checks=ref 0
let check name ok=incr checks;if not ok then failwith name
let () =
  let x=Arr.sequential [|3;3;3|] in
  let a=Arr.sum x in
  check "sum ndarray shape" (Arr.shape a=[|1|]);
  check "sum scalar value" (Arr.get a [|0|]=351. && Arr.sum' x=351.);
  check "scalar arithmetic after extraction" (Arr.get a [|0|]+.10.=361.);
  let x=Arr.zeros [|2;3|] and y=Arr.ones [|1;3|] in
  Arr.add_ x y;
  check "default in-place destination broadcasts y" (Arr.to_array x=Array.make 6 1.);
  check "second input preserved" (Arr.to_array y=Array.make 3 1.);
  let out=Arr.zeros [|2;3|] in Arr.add_ ~out x y;
  check "explicit destination" (Arr.to_array out=Array.make 6 2.);
  check "explicit destination preserves first input" (Arr.to_array x=Array.make 6 1.);
  let pure=Arr.add x y in
  check "pure addition returns fresh values" (Arr.to_array pure=Array.make 6 2. && Arr.to_array x=Array.make 6 1.);
  Arr.add_scalar_ x 2.;
  check "scalar underscore mutates input" (Arr.to_array x=Array.make 6 3.);
  check "generic float32 sum" (Dense.Matrix.Generic.sum (Dense.Matrix.S.eye 5) |> Dense.Ndarray.S.to_array = [|5.|]);
  check "generic float64 sum" (Dense.Matrix.Generic.sum (Dense.Matrix.D.eye 5) |> Dense.Ndarray.D.to_array = [|5.|]);
  check "generic complex32 sum" (Dense.Matrix.Generic.sum (Dense.Matrix.C.eye 5) |> Dense.Ndarray.C.to_array = [|Complex.{re=5.;im=0.}|]);
  check "generic complex64 sum" (Dense.Matrix.Generic.sum (Dense.Matrix.Z.eye 5) |> Dense.Ndarray.Z.to_array = [|Complex.{re=5.;im=0.}|]);
  check "16 dimensions accepted" (Arr.num_dims (Arr.ones (Array.make 16 1))=16);
  check "17 dimensions rejected by current Bigarray" (try ignore(Arr.ones (Array.make 17 1));false with Invalid_argument _->true);
  let x32=Dense.Matrix.S.eye 3 in
  let x64=Dense.Matrix.Generic.cast_s2d x32 in
  check "explicit conversion preserves shape" (Mat.shape x64=(3,3));
  check "explicit conversion preserves values" (Mat.to_array x64=Mat.to_array (Mat.eye 3));
  let z=Mat.(x64 + ones 3 3) in
  check "same-precision addition after cast" (Mat.get z 0 0=2. && Mat.get z 0 1=1.);
  Mat.set x64 0 0 7.;
  check "cast storage does not alias original" (Dense.Matrix.S.get x32 0 0=1.);
  Owl_io.write_file "native-metrics.json" (Printf.sprintf "{\"checks\":%d,\"sum\":351,\"bigarray_maximum_dimensions\":16,\"explicit_conversion\":\"passed\"}\n" !checks);
  Printf.printf "PASS: %d mutation, broadcasting, reduction, precision and Bigarray checks\n%!" !checks
