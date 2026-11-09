open Owl

let checks = ref 0
let check name condition =
  incr checks;
  if not condition then failwith name
let close name expected actual tolerance =
  check name (Float.is_finite actual && abs_float (expected -. actual) <= tolerance)

let () =
  let a = [|1.; 2.; 3.; 4.|] in
  close "mean" 2.5 (Stats.mean a) 1e-12;
  close "unbiased sample variance" (5. /. 3.) (Stats.var a) 1e-12;
  close "population central moment" 1.25 (Stats.central_moment 2 a) 1e-12;
  close "variance vs standard deviation" (Stats.var a) ((Stats.std a) ** 2.) 1e-12;
  close "positive affine correlation" 1. (Stats.corrcoef a (Array.map (fun v -> 3. *. v +. 2.) a)) 1e-12;
  close "negative affine correlation" (-1.) (Stats.corrcoef a (Array.map (~-.) a)) 1e-12;
  let pmf = [|0.343; 0.441; 0.189; 0.027|] in
  let cumulative = ref 0. in
  Array.iteri (fun k expected ->
    close "binomial PMF" expected (Stats.binomial_pdf ~p:0.3 ~n:3 k) 1e-12;
    cumulative := !cumulative +. expected;
    close "binomial CDF sum" !cumulative (Stats.binomial_cdf ~p:0.3 ~n:3 k) 1e-12) pmf;
  let normal = Stats.gaussian_cdf ~mu:0. ~sigma:1. in
  close "normal 95 percent" 0.95 (normal 1.96 -. normal (-1.96)) 1e-4;
  let student v = Stats.t_cdf v ~df:16. ~loc:0. ~scale:1. in
  close "Student 95 percent" 0.95 (student 2.12 -. student (-2.12)) 1e-4;
  List.iter (fun shape ->
    let scale = 2. in
    close "Gamma CDF complementary SF" 1.
      (Stats.gamma_cdf ~shape ~scale 3. +. Stats.gamma_sf ~shape ~scale 3.) 1e-12;
    check "Gamma PDF nonnegative" (Stats.gamma_pdf ~shape ~scale 3. >= 0.)) [1.;2.;5.;7.5];
  let null = Stats.z_test ~mu:2.5 ~sigma:1. a in
  close "null z-score" 0. null.score 1e-12;
  close "null p-value" 1. null.p_value 1e-12;
  check "null decision" (not null.reject);
  let shifted = Stats.z_test ~mu:0. ~sigma:1. [|3.;3.;3.;3.|] in
  close "known z-score" 6. shifted.score 1e-12;
  check "shift rejected" (shifted.reject && shifted.p_value < 1e-8);
  let b = [|2.;4.;6.;8.;10.|] in
  let welch = Stats.t_test_unpaired ~equal_var:false a b in
  close "Welch statistic" ((2.5 -. 6.) /. sqrt ((5. /. 3.) /. 4. +. 10. /. 5.)) welch.score 1e-12;
  let paired = Stats.t_test_paired a [|2.;2.;4.;5.|] in
  let differences = [|-1.;0.;-1.;-1.|] in
  close "paired statistic uses differences" (Stats.t_test ~mu:0. differences).score paired.score 1e-12;
  Owl_stats_prng.init 917;
  let uniform = Array.init 30000 (fun _ -> Stats.uniform_rvs 0. 10.) in
  check "uniform support" (Array.for_all (fun x -> x >= 0. && x <= 10.) uniform);
  close "uniform mean within six standard errors" 5. (Stats.mean uniform) 0.11;
  close "uniform variance" (100. /. 12.) (Stats.var uniform) 0.25;
  Owl_stats_prng.init 918;
  let gaussian = Array.init 30000 (fun _ -> Stats.gaussian_rvs ~mu:12. ~sigma:3.) in
  close "Gaussian mean within six standard errors" 12. (Stats.mean gaussian) 0.11;
  close "Gaussian variance" 9. (Stats.var gaussian) 0.5;
  Owl_stats_prng.init 919;
  let binomial = Array.init 30000 (fun _ -> Stats.binomial_rvs 0.3 3) in
  check "binomial support" (Array.for_all (fun x -> x >= 0 && x <= 3) binomial);
  close "binomial mean" 0.9 (Stats.mean (Array.map float_of_int binomial)) 0.03;
  Printf.printf "PASS: %d statistical property checks\n" !checks
