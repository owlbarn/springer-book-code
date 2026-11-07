let checks = ref 0

let close name tolerance expected actual =
  incr checks;
  if not (Float.is_finite actual) || abs_float (expected -. actual) > tolerance then
    failwith (Printf.sprintf "%s: expected %.16g, got %.16g" name expected actual)

let () =
  let open Book_algorithms in
  let xs = [|0.; 1.; 2.|] in
  let ys = Array.map (fun x -> x *. x +. 2. *. x +. 1.) xs in
  List.iter (fun x ->
    close "polynomial" 1e-12 (x *. x +. 2. *. x +. 1.) (fst (polint xs ys x)))
    [-0.5; 0.; 0.5; 1.; 2.; 2.5];
  List.iter (fun x ->
    close "rational off/on node" 1e-12 (1. /. (1. +. x))
      (fst (ratint [|0.; 1.|] [|1.; 0.5|] x))) [0.; 0.25; 0.5; 1.; 1.5];
  let rational_xs = [|-2.; -0.5; 1.; 3.|] in
  let rational_f x = (2. *. x +. 3.) /. (x *. x +. 4.) in
  let rational_ys = Array.map rational_f rational_xs in
  List.iter (fun x -> close "rational four-point" 1e-12 (rational_f x)
    (fst (ratint rational_xs rational_ys x))) [-1.; 0.; 0.5; 2.];
  for stage = 1 to 8 do
    let inside x = if x < 0. || x > 1. then failwith "out-of-interval sample" else 1. in
    close "samples remain in interval" 1e-12 1. (trapzd inside 0. 1. stage);
    close "constant trapezoid" 1e-12 1. (trapzd (fun _ -> 1.) 0. 1. stage);
    close "linear trapezoid" 1e-12 0.5 (trapzd Fun.id 0. 1. stage)
  done;
  close "trapezoid x squared" 1e-7 21. (trapz ~eps:1e-10 (fun x -> x *. x) 1. 4.);
  close "Simpson x squared" 1e-12 21. (simpson ~n:2 (fun x -> x *. x) 1. 4.);
  close "Simpson cubic" 1e-12 0.25 (simpson ~n:2 (fun x -> x ** 3.) 0. 1.);
  close "Romberg x fourth" 1e-12 0.2 (romberg (fun x -> x ** 4.) 0. 1.);
  List.iter (fun (name, integrate) ->
    close (name ^ " sine") 1e-6 2. (integrate sin 0. Float.pi);
    close (name ^ " odd") 1e-12 0. (integrate (fun x -> x ** 3.) (-1.) 1.);
    close (name ^ " zero interval") 1e-12 0. (integrate exp 1. 1.);
    close (name ^ " reversed") 1e-6 (-1.) (integrate exp (log 2.) 0.))
    ["trapz", (fun f a b -> trapz f a b);
     "simpson", (fun f a b -> simpson f a b);
     "romberg", (fun f a b -> romberg f a b)];
  (* A failing integrand must not be swallowed as numerical convergence. *)
  let propagated =
    try ignore (trapz (fun _ -> failwith "integrand") 0. 1.); false
    with Failure s when s = "integrand" -> true
  in
  incr checks;
  if not propagated then failwith "integrand exception was swallowed";
  Printf.printf "PASS: %d teaching-algorithm checks\n" !checks
