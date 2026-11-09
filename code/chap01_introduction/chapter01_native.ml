open Owl
let checks=ref 0
let check name ok=incr checks;if not ok then failwith name
let near x y=abs_float(x-.y)<1e-9
let () =
  let call=Book_introduction.black_scholes in
  check "standard normal symmetry" (near (Book_introduction.cdf 0.) 0.5);
  check "one-year analytic fixture" (near (call 0.05 100. 100. 1. 0.2) 10.450583572185565);
  check "quarter-year analytic fixture exposes misplaced square root" (near (call 0.05 100. 100. 0.25 0.2) 4.614997129602855);
  check "expiry payoff" (call 0.05 120. 100. 0. 0.2=20.);
  check "zero-volatility discounted payoff" (near (call 0.05 100. 100. 1. 0.) (100.-.100.*.exp(-0.05)));
  let value=call 0.05 100. 110. 2. 0.3 in
  check "no-arbitrage call bounds" (value>=max 0. (100.-.110.*.exp(-0.1)) && value<=100.);
  check "call increases with stock price" (call 0.05 110. 100. 0.25 0.2 > call 0.05 100. 100. 0.25 0.2);
  check "call increases with volatility" (call 0.05 100. 100. 0.25 0.3 > call 0.05 100. 100. 0.25 0.2);
  Owl_stats_prng.init 101;
  let network=Book_introduction.make_network [|28;28;1|] in
  Neural.S.Graph.init network;
  let probabilities=Neural.S.Graph.model network (Dense.Ndarray.S.zeros [|1;28;28;1|]) in
  check "real CNN forward output" (Dense.Ndarray.S.shape probabilities=[|1;10|]);
  check "CNN finite probabilities" (Array.for_all(fun x->Float.is_finite x && x>=0. && x<=1.) (Dense.Ndarray.S.to_array probabilities));
  check "CNN probabilities sum to one" (abs_float(Dense.Ndarray.S.sum' probabilities-.1.)<1e-5);
  Owl_io.write_file "native-metrics.json" (Printf.sprintf "{\"checks\":%d,\"quarter_year_call\":%.17g,\"cnn_shape\":[1,10]}\n" !checks (call 0.05 100. 100. 0.25 0.2));
  Printf.printf "PASS: %d introductory formula and real CNN checks\n%!" !checks
