#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
open Owl;;
let book_matrix_printer formatter value =
  Owl_pretty.print_dsnda ~max_row:6 ~max_col:6 ~formatter value;;
#install_printer book_matrix_printer;;
Owl_stats_prng.init 74;;

(* book: matrix-access begin *)
let x = Mat.uniform 5 5;;
Mat.set x 1 2 0.;;
let a = Mat.get x 0 3;;
(* book: matrix-access end *)

(* book: indexing begin *)
Mat.(x.%{1;2} <- 0.);;
let a = Mat.(x.%{0;3});;
(* book: indexing end *)

(* book: lu-input begin *)
let a = [|
    2.;2.;2.;
    2.;2.;3.;
    3.;4.;5.;|];;
let a = Mat.of_array a 3 3;;
(* book: lu-input end *)

(* book: lu-factorisation begin *)
let l, u, p = Linalg.D.lu a;;
let lu_pivots = p;;
(* book: lu-factorisation end *)

(* book: lu-product begin *)
let a' = Mat.dot l u;;
(* book: lu-product end *)

(* book: manual-permutation begin *)
let p = Mat.of_array [|
    0.;0.;1.;
    0.;1.;0.;
    1.;0.;0.;
  |] 3 3;;
Mat.(dot p a =~ dot l u);;
(* book: manual-permutation end *)

(* book: pivot-permutation begin *)
#use "book_linear.ml";;
let pivot_vector = Dense.Matrix.Generic.to_array lu_pivots
  |> Array.map Int32.to_int;;
let permutation = perm_vec_to_mat pivot_vector;;
Mat.(dot permutation a =~ dot l u);;
(* book: pivot-permutation end *)

(* book: diagonal begin *)
Mat.eye 5 |> Linalg.D.is_diag;;
(* book: diagonal end *)

(* book: inverse begin *)
let x = Mat.semidef 5 in
  let y = Linalg.D.inv x in
  Mat.(x *@ y =~ eye 5);;
(* book: inverse end *)

(* book: transpose-product begin *)
let flag =
    let a = Mat.uniform 4 4 in
    let b = Mat.uniform 4 4 in
    let m1 = Mat.(dot a b |> transpose) in
    let m2 = Mat.(dot (transpose b) (transpose a)) in
    Mat.(m1 =~ m2);;
(* book: transpose-product end *)

(* book: singular-lu begin *)
let x = Mat.of_array [|
  1.; 2.; 3.;
  0.; 0.; 1.;
  0.; 0.; 2.;
|] 3 3;;
let singular_lu_rejected =
  try ignore (Linalg.D.lu x); false
  with Failure _ -> true;;
(* book: singular-lu end *)

(* book: null-space begin *)
let a = Mat.magic 4;;
let x = Linalg.D.null a;;
Mat.dot a x |> Mat.l2norm';;
(* book: null-space end *)

(* book: qr begin *)
let a = Mat.of_array [|
    12.; -51.;   4.;
     6.; 167.; -68.;
    -4.;  24.; -41.;
  |] 3 3;;
let q, r, _ = Linalg.D.qr a;;
(* book: qr end *)

(* book: rank begin *)
let a = Mat.of_array [|
    1.; 5.;-1.;-1.;
    1.;-2.; 1.; 3.;
    3.; 8.;-1.; 1.;
    1.;-9.; 3.; 7.;
  |] 4 4;;
Linalg.D.rank a;;
(* book: rank end *)

(* book: rank-null begin *)
Linalg.D.null a;;
(* book: rank-null end *)

(* book: solve begin *)
let a = Mat.of_array [|
     2.; 3.; 1.;
     1.;-2.; 4.;
     3.; 8.;-2.;
     4.;-1.; 9.;
  |] 4 3;;
let b = Mat.of_array [|4.;-5.;13.;-6.|] 4 1;;
let x0 = Mat.dot (Linalg.D.pinv a) b;;
(* book: solve end *)

(* book: solution-null begin *)
let x1 = Linalg.D.null a;;
(* book: solution-null end *)

(* book: condition-number begin *)
let a = Mat.of_array [|
    4.1; 2.8;
    9.7; 6.6;
  |] 2 2;;
let c = Linalg.D.cond a;;
(* book: condition-number end *)

(* book: ill-conditioned-inverse begin *)
let a' = Linalg.D.inv a;;
(* book: ill-conditioned-inverse end *)

(* book: perturbed-inverse begin *)
let a2 = Mat.of_array [|
     4.1;   2.8;
    9.67; 6.607;
  |] 2 2;;
let a2' = Linalg.D.inv a2;;
(* book: perturbed-inverse end *)

(* book: determinant begin *)
let x = Mat.magic 5;;
Linalg.D.det x;;
Linalg.D.logdet x;;
(* book: determinant end *)

(* book: eigenspace begin *)
let basis =
    let ia = Mat.((eye 3) - (of_array
      [|3.; 1.;0.;
       -4.;-1.;0.;
        4.;-8.;2.|] 3 3))
    in
    Linalg.D.null ia;;
(* book: eigenspace end *)

(* book: eigenvectors begin *)
let eigvec, eigval =
    let a = Mat.of_array [|
        3.; 1.;0.;
       -4.;-1.;0.;
        4.;-8.;2.;
    |] 3 3 in
    Linalg.D.eig a;;
(* book: eigenvectors end *)

(* book: hermitian begin *)
let a = Dense.Matrix.Z.of_array [|
    {re=1.; im=0.}; {re=2.; im=(-1.)};
    {re=2.; im=1.}; {re=3.; im=0.};
  |] 2 2;;
Linalg.Generic.is_hermitian a;;
(* book: hermitian end *)

(* book: conjugate-transpose begin *)
Dense.Matrix.Z.(conj a |> transpose);;
(* book: conjugate-transpose end *)

(* book: hermitian-eigenvalues begin *)
Linalg.Z.eigvals a;;
(* book: hermitian-eigenvalues end *)

(* book: schur begin *)
let a = Dense.Matrix.Z.of_array [|
    {re=1.; im=0.};    {re=1.; im=0.};
    {re=(-2.); im=0.}; {re=3.; im=0.};
  |] 2 2;;
let t, u, eigvals = Linalg.Z.schur a;;
(* book: schur end *)

(* book: unitary begin *)
Dense.Matrix.Z.(dot u (conj u |> transpose));;
(* book: unitary end *)

(* book: cholesky begin *)
let a = Mat.of_array [|
     4.; 12.;-16.;
    12.; 37.;-43.;
   -16.;-43.; 98.;
   |] 3 3;;
let l = Linalg.D.chol a;;
Mat.(dot (transpose l) l =~ a);;
(* book: cholesky end *)

(* book: positive-definite begin *)
Linalg.D.is_posdef a;;
(* book: positive-definite end *)

(* book: load-image begin *)
#use "imageUtils.ml";;
module N = Dense.Ndarray.S;;
module M = Dense.Matrix.S;;
let img_arr = load_ppm "cat_bw.ppm" |> N.get_slice [[];[];[0]];;
let shp = N.shape img_arr;;
let h, w = shp.(0), shp.(1);;
let img = N.reshape img_arr [|h; w|];;
(* book: load-image end *)

(* book: svd begin *)
let u, s, vt = Linalg.S.svd img;;
(* book: svd end *)

(* book: truncate begin *)
let l = (N.shape s).(1);;
let n = 10;;
let s' = N.copy s;;
let z = N.zeros [|1; l - n|];;
let _ = N.set_slice [[];[n; l - 1]] s' z;;
(* book: truncate end *)

(* book: reconstruct begin *)
let img' = M.(dot (dot u (diagm s')) vt);;
(* book: reconstruct end *)

(* book: save-image begin *)
let image = N.stack ~axis:2 [|img'; img'; img'|];;
let image = N.expand image 4;;
let fname = "cat_" ^ (string_of_int n) ^ ".ppm";;
let _ = save_ppm_from_arr image fname;;
(* book: save-image end *)

(* book: additional-images begin *)
List.iter (fun n ->
  let s' = N.copy s in
  N.set_slice [[];[n; l - 1]] s' (N.zeros [|1; l-n|]);
  let img' = M.(dot (dot u (diagm s')) vt) in
  let image = N.stack ~axis:2 [|img';img';img'|] |> fun x -> N.expand x 4 in
  save_ppm_from_arr image ("cat_" ^ string_of_int n ^ ".ppm")
) [20;50];;
let full_reconstruction = M.(dot (dot u (diagm s)) vt);;
let relative_error image = M.(l2norm' (image - img) /. l2norm' img);;
assert (relative_error full_reconstruction < 1e-5);;
let errors = List.map (fun n ->
  let s' = N.copy s in
  N.set_slice [[];[n; l-1]] s' (N.zeros [|1;l-n|]);
  relative_error M.(dot (dot u (diagm s')) vt)
) [10;20;50];;
assert (match errors with [a;b;c] -> a > b && b > c | _ -> false);;
Printf.printf "PASS: SVD full reconstruction and decreasing rank errors: %.6f, %.6f, %.6f\n"
  (List.nth errors 0) (List.nth errors 1) (List.nth errors 2);;
(* book: additional-images end *)
