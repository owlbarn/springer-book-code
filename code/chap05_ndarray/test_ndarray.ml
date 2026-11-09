open Owl
let count = ref 0
let check label condition = incr count; if not condition then failwith label
let same label a b = check label (Arr.shape a = Arr.shape b && Arr.approx_equal a b)
let () =
  let x = Arr.init [|6;8|] (fun i -> 2. *. float_of_int i) in
  check "creation shape" (Arr.shape x = [|6;8|]);
  check "creation values" (Arr.get x [|5;7|] = 94.);
  let mapped = Arr.map (fun a -> a +. 1.) x in
  check "map value" (Arr.get mapped [|5;7|] = 95.);
  check "map leaves source intact" (Arr.get x [|5;7|] = 94.);
  same "fold matches sum" (Book_ndarray.sum ~axis:1 x) (Arr.sum ~axis:1 x);
  let threshold = Arr.map (fun a -> if a > 0.5 then a else 0.) x in
  same "mask threshold" threshold Arr.((x >.$ 0.5) * x);
  let x = Arr.sequential [|3;4|] in
  let tiled = Arr.tile x [|2;2|] in
  check "tile shape" (Arr.shape tiled = [|6;8|]);
  check "tile value" (Arr.get tiled [|4;6|] = 6.);
  let repeated = Arr.repeat x [|2;1|] in
  check "repeat shape" (Arr.shape repeated = [|6;4|]);
  check "repeat value" (Arr.get repeated [|1;2|] = 2.);
  let x = Arr.sequential [|10;10;10|] in
  same "basic and fancy slicing" (Arr.get_slice [[];[0;8];[3;9;2]] x)
    (Arr.get_fancy [R [];R [0;8];R [3;9;2]] x);
  let x = Arr.sequential [|8;8|] in
  let col = Arr.get_slice [[];[2]] x in
  check "column shape retained" (Arr.shape col = [|8;1|]);
  check "column value" (Arr.get col [|7;0|] = 58.);
  same "column basic/fancy" col (Arr.get_fancy [R [];I 2] x);
  let selection = Arr.get_fancy [L [3;5];R [1;7;2]] x in
  check "fancy selection shape" (Arr.shape selection = [|2;4|]);
  check "fancy selection value" (Arr.get selection [|1;3|] = 47.);
  let negative = Arr.get_fancy [L [-2;-1];R [-3;-2]] x in
  check "negative index" (Arr.get negative [|1;1|] = 62.);
  let original = Mat.sequential 5 5 in
  same "double reverse identity" original (Book_ndarray.reverse (Book_ndarray.reverse original));
  let rotate = Book_ndarray.rotate90 in
  same "four rotations identity" original (rotate (rotate (rotate (rotate original))));
  List.iter (fun n -> same "circular shift inverse" original (Book_ndarray.cshift (Book_ndarray.cshift original n) (-n))) [-7;-5;-1;0;1;2;5;7];
  check "shift sample" (Mat.get (Book_ndarray.cshift original 2) 0 0 = 3.);
  let x = Mat.sequential 5 4 and bias = Mat.sequential 1 4 in
  same "broadcast equals tiled addition" Mat.(x + Mat.tile bias [|5;1|]) Mat.(x + bias);
  let valid = [[|1;1;1|];[|2;1;1|];[|2;3;1|]] in
  List.iter (fun shape -> check "illustrated compatible shapes" (Owl_utils_ndarray.broadcastable [|2;1;3|] shape)) valid;
  List.iter (fun shape -> check "illustrated incompatible shapes" (not (Owl_utils_ndarray.broadcastable [|2;1;3|] shape))) [[|1;1;2|];[|3;1;1|]];
  let x = Arr.sequential [|4;5|] in
  check "expand prepends ones" (Arr.shape (Arr.expand x 4) = [|1;1;4;5|]);
  let a = Mat.sequential 3 4 and b = Mat.sequential 4 5 in
  same "matrix dot contraction" (Mat.dot a b) (Arr.contract2 [|1,0|] a b);
  let a = Arr.sequential [|3;4;5|] and b = Arr.sequential [|4;3;2|] in
  let contracted = Arr.contract2 [|0,1;1,0|] a b in
  let manual = Arr.zeros [|5;2|] in
  for h=0 to 2 do for k=0 to 3 do for i=0 to 4 do for j=0 to 1 do
    Arr.set manual [|i;j|] (Arr.get manual [|i;j|] +. Arr.get a [|h;k;i|] *. Arr.get b [|k;h;j|])
  done done done done;
  same "multi-axis contraction vs loops" contracted manual;
  let traced = Arr.contract1 [|0,1|] (Arr.sequential [|2;2;3|]) in
  check "trace contraction values" (Arr.to_array traced = [|9.;11.;13.|]);
  Printf.printf "PASS: %d ndarray property checks\n" !count
