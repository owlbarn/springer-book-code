(* Teaching implementations from Chapter 2; not replacements for Owl internals. *)

(* book: polint begin *)
let polint xs ys x =
  let n = Array.length xs in
  if n = 0 || Array.length ys <> n then invalid_arg "polint: array lengths";
  let c = Array.copy ys in
  let d = Array.copy ys in
  let ns = ref 0 in
  let dy = ref 0. in
  let dif = ref (abs_float (x -. xs.(0))) in

  for i = 0 to n - 1 do
    let dift = abs_float (x -. xs.(i)) in
    if dift < !dif
    then (
      ns := i;
      dif := dift)
  done;

  let y = ref ys.(!ns) in
  ns := !ns - 1;

  for m = 0 to n - 2 do
    for i = 0 to n - m - 2 do
      let ho = xs.(i) -. x in
      let hp = xs.(i + m + 1) -. x in
      let w = c.(i + 1) -. d.(i) in
      let den = ho -. hp in
      assert (den <> 0.);
      let den = w /. den in
      c.(i) <- ho *. den;
      d.(i) <- hp *. den
    done;
    if (2 * !ns) + 2 < n - m - 1
    then dy := c.(!ns + 1)
    else (
      dy := d.(!ns);
      ns := !ns - 1);
    y := !y +. !dy
  done;

  !y, !dy
(* book: polint end *)

(* book: ratint begin *)
let ratint xs ys x =
  let n = Array.length xs in
  if n = 0 || Array.length ys <> n then invalid_arg "ratint: array lengths";
  match Array.find_index (fun xi -> xi = x) xs with
  | Some i -> ys.(i), 0.
  | None ->
    let c = Array.copy ys in
    let d = Array.map (fun y -> y +. 1e-25) ys in
    let ns = ref 0 in
    for i = 1 to n - 1 do
      if abs_float (x -. xs.(i)) < abs_float (x -. xs.(!ns)) then ns := i
    done;
    let y = ref ys.(!ns) in
    let dy = ref 0. in
    decr ns;
    for m = 1 to n - 1 do
      for i = 0 to n - m - 1 do
        let w = c.(i + 1) -. d.(i) in
        let h = xs.(i + m) -. x in
        let t = (xs.(i) -. x) *. d.(i) /. h in
        let den = t -. c.(i + 1) in
        if den = 0. then failwith "ratint: singular interpolation table";
        let den = w /. den in
        d.(i) <- c.(i + 1) *. den;
        c.(i) <- t *. den
      done;
      if 2 * (!ns + 1) < n - m then dy := c.(!ns + 1)
      else (dy := d.(!ns); decr ns);
      y := !y +. !dy
    done;
    !y, !dy
(* book: ratint end *)

(* book: trapzd begin *)
let trapzd f a b n =
  if n < 1 || n > 25 then invalid_arg "trapzd: stage must be in [1,25]";
  let m = 1 lsl (n - 1) in
  let h = (b -. a) /. float_of_int m in
  let s = ref (0.5 *. (f a +. f b)) in
  for i = 1 to m - 1 do
    s := !s +. f (a +. float_of_int i *. h)
  done;
  h *. !s
(* book: trapzd end *)

let check_options n eps =
  if n < 1 || n > 25 || eps <= 0. then invalid_arg "quadrature: n or eps"

let converged eps old current =
  abs_float (current -. old) <= eps *. max 1. (abs_float current)

(* book: trapz begin *)
let trapz ?(n = 20) ?(eps = 1e-6) f a b =
  check_options n eps;
  let rec refine stage old =
    let current = trapzd f a b stage in
    if stage = n || (stage > 5 && converged eps old current) then current
    else refine (stage + 1) current
  in
  refine 1 infinity
(* book: trapz end *)

(* book: simpson begin *)
let simpson ?(n = 20) ?(eps = 1e-6) f a b =
  check_options n eps;
  if n < 2 then invalid_arg "simpson: at least two stages";
  let rec refine stage previous_trapezoid previous_simpson =
    let trapezoid = trapzd f a b stage in
    let current = (4. *. trapezoid -. previous_trapezoid) /. 3. in
    if stage = n || (stage > 5 && converged eps previous_simpson current) then current
    else refine (stage + 1) trapezoid current
  in
  refine 2 (trapzd f a b 1) infinity
(* book: simpson end *)

(* book: romberg begin *)
let romberg ?(n = 20) ?(eps = 1e-6) f a b =
  check_options n eps;
  let k = 5 in
  if n < k then invalid_arg "romberg: at least five stages";
  let s = Array.make n 0. in
  let h = Array.init n (fun i -> 0.25 ** float_of_int i) in
  let rec refine i =
    s.(i) <- trapzd f a b (i + 1);
    if i + 1 < k then refine (i + 1)
    else (
      let hs = Array.sub h (i - k + 1) k in
      let ss = Array.sub s (i - k + 1) k in
      let result, correction = polint hs ss 0. in
      if i + 1 = n || abs_float correction <= eps *. max 1. (abs_float result)
      then result
      else refine (i + 1))
  in
  refine 0
(* book: romberg end *)
