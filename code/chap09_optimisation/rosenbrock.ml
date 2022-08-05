open Owl
open Algodiff.D
module N = Dense.Ndarray.D 
open Owl_plplot

let rosenbrock a = 
  let x = Mat.get a 0 0 in 
  let y = Mat.get a 0 1 in
  Maths.(( F 100. ) * (y - (x ** (F 2.))) ** (F 2.) + 
    (F 1. - x) ** (F 2.) |> sum')

let v = N.of_array [|2.; -0.5|] [|1;2|]
let traj = ref (N.copy v)
let a = ref v
let eta = 0.0001
let n= 200

let _ = 
  for _ = 1 to n - 1 do 
    let u = grad rosenbrock (Arr !a) |> unpack_arr in 
    a := N.(sub !a (scalar_mul eta u));
    traj := N.concatenate [|!traj; (N.copy !a)|]
  done

let plot () =
    let a, b = Dense.Matrix.D.meshgrid (-2.) 2. (-1.) 3. 50 50 in
    let c = N.(scalar_mul 100.
      (pow_scalar (sub b (pow_scalar a 2.)) 2.) +
      (pow_scalar (scalar_sub 1. a) 2.)) in
  
    let h = Plot.create ~m:1 ~n:2 "plot_gradients.pdf" in
    Plot.subplot h 0 0;
    Plot.(mesh ~h ~spec:[ NoMagColor ] a b c);
  
    Plot.subplot h 0 1;
    Plot.contour ~h a b c;
    let vx = N.get_slice [[]; [0]] !traj in
    let vy = N.get_slice [[]; [1]] !traj in
    Plot.plot ~h vx vy;
    Plot.output h

let _ = plot ()