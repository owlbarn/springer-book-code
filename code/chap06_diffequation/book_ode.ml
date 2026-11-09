open Owl

(* book: euler-midpoint begin *)
let f x y = 2. *. x *. y +. x
let f' x = 0.5 *. (Maths.exp (x *. x) -. 1.)

let integrate advance step target =
  if not (Float.is_finite step && step > 0. && Float.is_finite target && target >= 0.)
  then invalid_arg "finite positive step and nonnegative target required";
  let steps = int_of_float (ceil (target /. step)) in
  let y = ref 0. in
  for i = 0 to steps - 1 do
    let x = float_of_int i *. step in
    let h = min step (target -. x) in
    y := advance x !y h
  done;
  !y

let euler step target =
  integrate (fun x y h -> y +. h *. f x y) step target

let midpoint step target =
  integrate (fun x y h ->
    let slope = f x y in
    y +. h *. f (x +. h /. 2.) (y +. h *. slope /. 2.)
  ) step target
(* book: euler-midpoint end *)
