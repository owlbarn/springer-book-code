open Owl

(* book: factorial_data begin *)
let xi = [|2; 3; 4; 5; 6|]
let y = Array.map (fun x -> Maths.fact (x - 1)) xi
let x = Array.map float_of_int xi
let f a = fst (Book_algorithms.polint x y a)
(* book: factorial_data end *)

let () =
  Array.iteri (fun i a ->
    if abs_float (f a -. y.(i)) > 1e-10 then failwith "factorial interpolation") x;
  Printf.printf "polint Gamma(3.5): %.12g (Gamma: %.12g)\n" (f 3.5) (Maths.gamma 3.5);
  let cube x = x *. x in
  Printf.printf "teaching trapz x^2 [1,4]: %.12g\n" (Book_algorithms.trapz cube 1. 4.);
  Printf.printf "atan(1)-atan(-1): %.12g\n" (atan 1. -. atan (-1.));
  let nodes, weights = Book_gaussian.gauss_legendre 10 in
  Array.iteri (fun i a -> Printf.printf "Legendre %d %.12g %.12g\n" i a weights.(i)) nodes;
  let sinc x = if x = 0. then 1. else sin x /. x in
  let integral = Book_gaussian.gaussian sinc 0. 4. in
  if abs_float (integral -. Maths.si 4.) > 1e-6 then failwith "sine integral";
  Printf.printf "Gaussian sinc [0,4]: %.12g (si: %.12g)\n" integral (Maths.si 4.);
  let beta = Maths.beta 3. 4. in
  let gamma_ratio = Maths.gamma 3. *. Maths.gamma 4. /. Maths.gamma 7. in
  if abs_float (beta -. gamma_ratio) > 1e-12 then failwith "beta identity";
  Printf.printf "beta(3,4): %.12g; symmetric: %b\n" beta
    (abs_float (beta -. Maths.beta 4. 3.) < 1e-12);
  let a, b = 4., 3. in
  Printf.printf "ellipse perimeter: %.12g\n" (4. *. a *. Maths.ellipe (1. -. (b /. a) ** 2.));
  for n = 1 to 10 do
    for degree = 0 to 2 * n - 1 do
      let expected = if degree mod 2 = 0 then 2. /. float_of_int (degree + 1) else 0. in
      let actual = Book_gaussian.gaussian_fixed ~n (fun x -> x ** float_of_int degree) (-1.) 1. in
      if abs_float (expected -. actual) > 1e-8 then failwith "Gauss polynomial exactness"
    done
  done;
  Printf.printf "PASS: library examples and Gauss polynomial checks\n"
