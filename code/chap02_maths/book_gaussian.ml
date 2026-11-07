(* The chapter delegates generation of nodes/weights to Owl. *)
let gauss_legendre = Owl_maths_quadrature.gauss_legendre
let gauss_legendre_cache = Array.init 50 gauss_legendre

(* book: gaussian begin *)
let gaussian_fixed ?(n = 10) f a b =
  if n < 1 then invalid_arg "gaussian_fixed: n must be positive";
  let x, w =
    if n < Array.length gauss_legendre_cache then gauss_legendre_cache.(n)
    else gauss_legendre n
  in
  let xr = 0.5 *. (b -. a) in
  let s = ref 0. in
  for i = 0 to n - 1 do
    let c = xr *. (x.(i) +. 1.) +. a in
    s := !s +. w.(i) *. f c
  done;
  !s *. xr

let gaussian ?(n = 50) ?(eps = 1e-6) f a b =
  if n < 1 || eps <= 0. then invalid_arg "gaussian: n or eps";
  let rec refine i old =
    let current = gaussian_fixed ~n:i f a b in
    if i = n || (i > 1 && abs_float (current -. old) <= eps) then current
    else refine (i + 1) current
  in
  refine 1 infinity
(* book: gaussian end *)
