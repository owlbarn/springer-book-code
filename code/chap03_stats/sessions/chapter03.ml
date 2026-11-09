(* Canonical Chapter 3 inputs; execute sequentially in the book container. *)
#use "topfind";;
Topfind.log := (fun _ -> ());;
#require "owl";;
#require "owl-plplot";;
open Owl;;
open Owl_plplot;;
Owl_stats_prng.init 73;;

(* book: uniform-data begin *)
let data = Array.init 100 (fun _ -> Stats.uniform_rvs 0. 10.);;
(* book: uniform-data end *)

(* book: descriptive begin *)
Stats.mean data;;
Stats.std data;;
Stats.var data;;
Stats.skew data;;
Stats.kurtosis data;;
(* book: descriptive end *)

(* book: central-moment begin *)
Stats.central_moment 2 data;;
(* book: central-moment end *)

(* book: coin-tosses begin *)
let _ =
  let toss = Array.make 10 0 in
  Array.map (fun _ -> Stats.binomial_rvs 0.3 1) toss;;
(* book: coin-tosses end *)

(* book: binomial-pmf begin *)
let x = [|0; 1; 2; 3|];;
let p = Array.map (Stats.binomial_pdf ~p:0.3 ~n:3) x;;
Array.fold_left (+.) 0. p;;
(* book: binomial-pmf end *)

(* book: binomial-cdf begin *)
let x = [|0; 1; 2; 3|];;
let p = Array.map (Stats.binomial_cdf ~p:0.3 ~n:3) x;;
(* book: binomial-cdf end *)

(* book: gaussian-data begin *)
let x = Array.init 999
  (fun _ -> Stats.gaussian_rvs ~mu:1. ~sigma:1.);;
let y = Array.init 999
  (fun _ -> Stats.gaussian_rvs ~mu:12. ~sigma:3.);;
(* book: gaussian-data end *)

(* book: gaussian-histogram begin *)
let x' = Mat.of_array x 1 999;;
let y' = Mat.of_array y 1 999;;

let h = Plot.create ~m:1 ~n:2 "plot_02.png" in
Plot.subplot h 0 0;
Plot.set_ylabel h "frequency";
Plot.histogram ~bin:30 ~h x';
Plot.histogram ~bin:30 ~h y';
Plot.subplot h 0 1;
Plot.set_ylabel h "PDF p(x)";
Plot.plot_fun ~h (fun x -> Stats.gaussian_pdf
  ~mu:1. ~sigma:1. x) (-2.) 6.;
Plot.plot_fun ~h (fun x -> Stats.gaussian_pdf
  ~mu:12. ~sigma:3. x) 0. 25.;
Plot.output h;;
(* book: gaussian-histogram end *)

(* book: correlated-data begin *)
let noise sigma = Stats.gaussian_rvs ~mu:0. ~sigma;;
let x = Array.init 50 (fun _ -> Stats.uniform_rvs 0. 10.);;
let y = Array.map (fun a -> 2.5 *. a +. noise 1.) x;;
let z = Array.map (fun a -> 2.5 *. a +. noise 8.) x;;
(* book: correlated-data end *)

(* book: correlation begin *)
Stats.corrcoef x y;;
Stats.corrcoef x z;;
(* book: correlation end *)

(* book: normal-interval begin *)
let f = Stats.gaussian_cdf ~mu:0. ~sigma:1. in
f 1.96 -. f (-1.96);;
(* book: normal-interval end *)

(* book: student-interval begin *)
let f x = Stats.t_cdf x ~df:16. ~loc:0. ~scale:1. in
f 2.12 -. f (-2.12);;
(* book: student-interval end *)

(* book: hypothesis-data begin *)
let data_0 = Array.init 10
  (fun _ -> Stats.gaussian_rvs ~mu:0. ~sigma:1.);;
let data_1 = Array.init 10
  (fun _ -> Stats.gaussian_rvs ~mu:3. ~sigma:1.);;
(* book: hypothesis-data end *)

(* book: z-null begin *)
Stats.z_test ~mu:0. ~sigma:1. data_0;;
(* book: z-null end *)

(* book: hypothesis-type begin *)
#show_type Stats.hypothesis;;
(* book: hypothesis-type end *)

(* book: z-shift begin *)
Stats.z_test ~mu:0. ~sigma:1. data_1;;
(* book: z-shift end *)

(* book: two-sample begin *)
Stats.t_test_unpaired ~alpha:0.05 ~side:Stats.BothSide
  ~equal_var:false data_0 data_1;;
(* book: two-sample end *)

(* book: additional-plots begin *)
let h = Plot.create "plot_gaussian_cdf.pdf" in
Plot.set_ylabel h "CDF";
Plot.plot_fun ~h (Stats.gaussian_cdf ~mu:1. ~sigma:1.) (-2.) 25.;
Plot.plot_fun ~h (Stats.gaussian_cdf ~mu:12. ~sigma:3.) (-2.) 25.;
Plot.legend_on h ~position:Plot.SouthEast [|"mu=1,sigma=1"; "mu=12,sigma=3"|];
Plot.output h;;

let h = Plot.create ~m:1 ~n:2 "plot_01.png" in
let x' = Mat.of_array x 1 50 in
Plot.subplot h 0 0;
Plot.set_xlabel h "x";
Plot.set_ylabel h "y (sigma = 1)";
Plot.scatter ~h x' (Mat.of_array y 1 50);
Plot.subplot h 0 1;
Plot.set_xlabel h "x";
Plot.set_ylabel h "z (sigma = 8)";
Plot.scatter ~h x' (Mat.of_array z 1 50);
Plot.output h;;
(* book: additional-plots end *)
