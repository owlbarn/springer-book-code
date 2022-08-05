open Owl
open Algodiff.D
module N = Dense.Ndarray.D 
open Owl_plplot

let himmelblau a = 
  let x = Mat.get a 0 0 in 
  let y = Mat.get a 0 1 in
  Maths.(x ** (F 2.) + y - (F 11.) ** (F 2.) + 
    (x + y ** (F 2.) - (F 7.)) ** (F 2.) |> sum')
let v = N.of_array [|-2.; 0.|] [|1;2|]
let traj = ref (N.copy v)
let a = ref v
let eta = 0.0001
let n= 2000

let _ = 
  for _ = 1 to n - 1 do 
    let u = grad himmelblau (Arr !a) |> unpack_arr in 
    a := N.(sub !a (scalar_mul eta u));
    traj := N.concatenate [|!traj; (N.copy !a)|]
  done

let plot () =
    let a, b = Dense.Matrix.D.meshgrid (-4.) 4. (-4.) 4. 50 50 in
    let c = N.(add
      (sub_scalar (add (pow_scalar a 2.) b) 11.)
      (pow_scalar (sub_scalar (add a (pow_scalar b 2.)) 7.) 2.)
    ) in
    let h = Plot.create ~m:1 ~n:2 "plot_himm.pdf" in
    Plot.subplot h 0 0;
    Plot.(mesh ~h ~spec:[ NoMagColor ] a b c);
  
    Plot.subplot h 0 1;
    Plot.contour ~h a b c;
    let vx = N.get_slice [[]; [0]] !traj in
    let vy = N.get_slice [[]; [1]] !traj in
    Plot.plot ~h vx vy;
    Plot.output h

let _ = plot ()