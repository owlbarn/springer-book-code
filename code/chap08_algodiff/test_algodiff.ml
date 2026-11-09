open Owl
module AD = Algodiff.D
let count = ref 0
let check name condition = incr count; if not condition then failwith name
let near name expected actual = check name (Float.is_finite actual && abs_float (expected -. actual) < 1e-9 *. max 1. (abs_float expected))
let scalar f x = f (AD.F x) |> AD.unpack_flt
let () =
  let open AD in
  let f x = Maths.tanh x in
  let f1 = diff f in
  let f2 = diff f1 in
  let f3 = diff f2 in
  let f4 = diff f3 in
  List.iter (fun x ->
    let y = tanh x in
    near "tanh first" (1. -. y *. y) (scalar f1 x);
    near "tanh second" (-2. *. y *. (1. -. y *. y)) (scalar f2 x);
    near "tanh third" (-2. +. 8. *. y *. y -. 6. *. y ** 4.) (scalar f3 x);
    near "tanh fourth" (16. *. y -. 40. *. y ** 3. +. 24. *. y ** 5.) (scalar f4 x)
  ) [-1.;0.;0.1;2.];
  let logistic x =
    let a=Mat.get x 0 0 and b=Mat.get x 0 1 in
    Maths.(F 1. / (F 1. + exp (sin a + a*b))) in
  let x=Mat.ones 1 2 in
  let p=logistic x |> unpack_flt in
  let g=grad logistic x |> unpack_arr in
  near "gradient x0" (-.p *. (1. -. p) *. (cos 1. +. 1.)) (Owl.Mat.get g 0 0);
  near "gradient x1" (-.p *. (1. -. p)) (Owl.Mat.get g 0 1);
  let directional=logistic (make_forward x (Mat.ones 1 2) (tag ())) |> tangent |> unpack_flt in
  near "forward directional derivative" (Owl.Mat.sum' g) directional;
  let q x=Maths.(sum' (sqr x)) in
  let g=grad q x |> unpack_arr and h=hessian q x |> unpack_arr in
  near "quadratic gradient" 0. Owl.Mat.(l2norm' (g - of_array [|2.;2.|] 1 2));
  near "quadratic Hessian" 0. Owl.Mat.(l2norm' (h - (eye 2 *$ 2.)));
  let two_body y =
    let a=Mat.get y 0 0 and b=Mat.get y 0 1 in
    let r=Maths.(sqrt (sqr a+sqr b)) in
    let z=Mat.zeros 1 4 in
    let z=Mat.set z 0 0 (Mat.get y 0 2) in
    let z=Mat.set z 0 1 (Mat.get y 0 3) in
    let z=Mat.set z 0 2 Maths.(neg a / pow r (F 3.)) in
    Mat.set z 0 3 Maths.(neg b / pow r (F 3.)) in
  let j=jacobian two_body (Mat.ones 1 4) |> unpack_arr |> Owl.Mat.transpose in
  let a=1. /. (4. *. sqrt 2.) in
  let expected=Owl.Mat.of_array [|0.;0.;1.;0.;0.;0.;0.;1.;a;3.*.a;0.;0.;3.*.a;a;0.;0.|] 4 4 in
  near "two body Jacobian" 0. Owl.Mat.(l2norm' (j-expected));
  let eig=Owl_linalg.D.eigvals j in
  let lambda=2. ** (-0.25) and omega=2. ** (-0.75) in
  List.iter (fun target ->
    check "Jacobian eigenvalue" (Array.exists (fun z -> Complex.norm (Complex.sub z target)<1e-9) (Dense.Matrix.Z.to_array eig)))
    [Complex.{re=lambda;im=0.};Complex.{re= -.lambda;im=0.};Complex.{re=0.;im=omega};Complex.{re=0.;im= -.omega}];
  let nested x=Maths.(x * diff (fun y -> x+y) (F 2.)) in
  near "nested derivative tags" 1. (diff nested (F 2.) |> unpack_flt);
  let shared x=let u=Maths.(x*x) in Maths.(u+u) in
  near "upstream shared scalar graph" 8. (diff shared (F 2.) |> unpack_flt);
  Printf.printf "PASS: %d native analytic AD checks\n" !count
