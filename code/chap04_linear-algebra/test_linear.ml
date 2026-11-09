open Owl
let count = ref 0
let check label condition = incr count; if not condition then failwith label
let near label a b tolerance = check label (Float.is_finite b && abs_float (a -. b) < tolerance)
let residual label a b = near label 0. Mat.(l2norm' (a - b)) 1e-9
let matrix a rows cols = Mat.of_array a rows cols
let () =
  let a = matrix [|2.;2.;2.;2.;2.;3.;3.;4.;5.|] 3 3 in
  let l,u,p = Linalg.D.lu a in
  let pivots = Dense.Matrix.Generic.to_array p |> Array.map Int32.to_int in
  let permutation = Book_linear.perm_vec_to_mat pivots in
  residual "PA=LU" Mat.(permutation *@ a) Mat.(l *@ u);
  let cycle = Book_linear.perm_vec_to_mat [|2;3;3|] in
  residual "pivot swaps are forward and one-based" cycle
    (matrix [|0.;1.;0.;0.;0.;1.;1.;0.;0.|] 3 3);
  residual "permutation orthogonal" Mat.(cycle *@ transpose cycle) (Mat.eye 3);
  let q,r,_ = Linalg.D.qr a in
  residual "QR reconstructs input" Mat.(q *@ r) a;
  residual "Q orthogonal" Mat.(transpose q *@ q) (Mat.eye 3);
  let positive = matrix [|4.;12.;-16.;12.;37.;-43.;-16.;-43.;98.|] 3 3 in
  let chol = Linalg.D.chol positive in
  residual "Cholesky upper triangular convention" Mat.(transpose chol *@ chol) positive;
  check "positive definite" (Linalg.D.is_posdef positive);
  let rank_two = matrix [|1.;5.;-1.;-1.;1.;-2.;1.;3.;3.;8.;-1.;1.;1.;-9.;3.;7.|] 4 4 in
  check "rank two" (Linalg.D.rank rank_two = 2);
  let null = Linalg.D.null rank_two in
  check "nullity two" (Mat.col_num null = 2);
  residual "null space residual" Mat.(rank_two *@ null) (Mat.zeros 4 2);
  let a = matrix [|2.;3.;1.;1.;-2.;4.;3.;8.;-2.;4.;-1.;9.|] 4 3 in
  let b = matrix [|4.;-5.;13.;-6.|] 4 1 in
  let x = Mat.dot (Linalg.D.pinv a) b in
  residual "rank-deficient solve residual" Mat.(a *@ x) b;
  let null = Linalg.D.null a in
  residual "solution family residual" Mat.(a *@ (x + null)) b;
  let ill = matrix [|4.1;2.8;9.7;6.6|] 2 2 in
  check "large condition number" (Linalg.D.cond ill > 1600.);
  residual "inverse" Mat.(ill *@ Linalg.D.inv ill) (Mat.eye 2);
  let m = Mat.magic 5 in
  near "magic determinant" 5070000. (Linalg.D.det m) 1e-6;
  near "logdet" (log (Linalg.D.det m)) (Linalg.D.logdet m) 1e-10;
  let u,s,vt = Linalg.D.svd a in
  residual "SVD reconstructs rectangular input" Mat.(u *@ diagm s *@ vt) a;
  let singular = matrix [|1.;2.;3.;0.;0.;1.;0.;0.;2.|] 3 3 in
  check "singular LU rejected" (try ignore (Linalg.D.lu singular); false with Failure _ -> true);
  let module Z = Dense.Matrix.Z in
  let c re im = Complex.{re;im} in
  let hermitian = Z.of_array [|c 1. 0.;c 2. (-1.);c 2. 1.;c 3. 0.|] 2 2 in
  check "Hermitian matrix" (Linalg.Generic.is_hermitian hermitian);
  let values = Linalg.Z.eigvals hermitian |> Z.to_array in
  Array.sort (fun a b -> compare a.Complex.re b.Complex.re) values;
  near "Hermitian eigenvalue lower" (2. -. sqrt 6.) values.(0).Complex.re 1e-10;
  near "Hermitian eigenvalue upper" (2. +. sqrt 6.) values.(1).Complex.re 1e-10;
  Array.iter (fun x -> near "Hermitian eigenvalue imaginary part" 0. x.Complex.im 1e-10) values;
  let a = Z.of_array [|c 1. 0.;c 1. 0.;c (-2.) 0.;c 3. 0.|] 2 2 in
  let t,u,_ = Linalg.Z.schur a in
  let uh = Z.(transpose (conj u)) in
  near "Schur reconstruction" 0. (Z.(l2norm' ((u *@ t *@ uh) - a))).Complex.re 1e-10;
  near "Schur unitarity" 0. (Z.(l2norm' ((u *@ uh) - eye 2))).Complex.re 1e-10;
  let module N = Dense.Ndarray.S in
  let samples = Array.init 18 (fun i -> float_of_int (i * 11)) in
  let image = N.of_array samples [|2;3;3|] in
  let path = Filename.temp_file "book-ppm-" ".ppm" in
  ImageUtils.save_image_to_file image path;
  let loaded = ImageUtils.load_ppm path in
  Sys.remove path;
  check "non-square PPM dimensions" (N.shape loaded = [|2;3;3|]);
  check "PPM pixel and RGB order" (N.to_array loaded = samples);
  Printf.printf "PASS: %d linear algebra and image checks\n" !count
