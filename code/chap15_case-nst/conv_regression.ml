open Owl
module N=Dense.Ndarray.S
let checks=ref 0
let check label array expected =
 let dimensions=N.shape array and values=N.to_array array in
 let max_error=ref 0. and bad=ref 0 in
 Array.iteri (fun index value ->
   let residual=ref index in
   let coordinate=Array.make (Array.length dimensions) 0 in
   for axis=Array.length dimensions-1 downto 0 do
    coordinate.(axis) <- !residual mod dimensions.(axis);residual := !residual/dimensions.(axis)
   done;
   let error=abs_float(value-.expected coordinate) in
   max_error:=max !max_error error;
   if not(Float.is_finite value) || error>1e-3 then incr bad) values;
 incr checks;
 if !bad<>0 then failwith (Printf.sprintf "%s: %d incorrect elements" label !bad);
 Printf.printf "%s bad=%d of %d max_error=%.9g\n%!" label !bad (Array.length values) !max_error
let () =
 let size=28 and channels=512 in
 let x=N.ones [|1;size;size;channels|] and kernel=N.ones [|3;3;channels;channels|] and dy=N.ones [|1;size;size;channels|] in
 let overlap index=if index=0 || index=size-1 then 2 else 3 in
 let expected p=float_of_int (overlap p.(1)*overlap p.(2)*channels) in
 for repeat=1 to 3 do
  check (Printf.sprintf "forward-%d" repeat) (N.conv2d ~padding:Owl_types.SAME x kernel [|1;1|]) expected;
  check (Printf.sprintf "input-gradient-%d" repeat) (N.conv2d_backward_input x kernel [|1;1|] dy) expected;
  check (Printf.sprintf "kernel-gradient-%d" repeat) (N.conv2d_backward_kernel x kernel [|1;1|] dy)
   (fun p -> float_of_int ((if p.(0)=1 then size else size-1)*(if p.(1)=1 then size else size-1)))
 done;
 let kernel=N.of_array (Array.init (3*3*channels*channels) (fun i -> float_of_int(i mod channels mod 7+1))) [|3;3;channels;channels|] in
 let dy=N.of_array (Array.init (size*size*channels) (fun i -> float_of_int(i mod channels mod 5+1))) [|1;size;size;channels|] in
 let total=Array.init channels (fun j -> (j mod 7+1)*(j mod 5+1)) |> Array.fold_left (+) 0 in
 for repeat=1 to 3 do
  check (Printf.sprintf "weighted-forward-%d" repeat) (N.conv2d ~padding:Owl_types.SAME x kernel [|1;1|])
   (fun p -> float_of_int(overlap p.(1)*overlap p.(2)*channels*(p.(3) mod 7+1)));
  check (Printf.sprintf "weighted-input-gradient-%d" repeat) (N.conv2d_backward_input x kernel [|1;1|] dy)
   (fun p -> float_of_int(overlap p.(1)*overlap p.(2)*total));
  check (Printf.sprintf "weighted-kernel-gradient-%d" repeat) (N.conv2d_backward_kernel x kernel [|1;1|] dy)
   (fun p -> float_of_int((if p.(0)=1 then size else size-1)*(if p.(1)=1 then size else size-1)*(p.(3) mod 5+1)))
 done;
 Printf.printf "PASS: %d exact analytical convolution tensor checks\n%!" !checks
