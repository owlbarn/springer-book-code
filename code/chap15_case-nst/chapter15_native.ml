open Owl
open Book_style
open Neural.S
module N = Dense.Ndarray.S
module A = Neural.S.Algodiff
let checks = ref 0
let check name ok = incr checks; if not ok then failwith name
let close a b = abs_float (a -. b) < 1e-4
let () =
  Owl_stats_prng.init 95;
  let weights = Sys.argv.(1) and inputs = Sys.argv.(2) in
  let x = N.of_array (Array.init 8 (fun i -> float_of_int (i+1))) [|1;2;2;2|] in
  let gram = Neural_transfer.gram (A.Arr x) |> A.unpack_arr in
  check "analytic Gram" (Array.for_all2 close (N.to_array gram) [|10.5;12.5;12.5;15.|]);
  let c = Neural_transfer.c_loss (A.Arr x) (A.Arr (N.zeros (N.shape x))) |> A.unpack_flt in
  check "analytic content loss" (close c 25.5);
  let g = A.grad (fun v -> Neural_transfer.c_loss v (A.Arr (N.zeros (N.shape x)))) (A.Arr x) |> A.unpack_arr in
  check "analytic content gradient" (Array.for_all2 close (N.to_array g) (Array.init 8 (fun i -> float_of_int(i+1)/.4.)));
  let style v = Neural_transfer.s_loss (Neural_transfer.gram v) (A.Arr (N.zeros [|2;2|])) in
  check "analytic style loss" (close (A.unpack_flt (style (A.Arr x))) 161.9375);
  let gradient = A.grad style (A.Arr x) |> A.unpack_arr |> N.to_array in
  check "analytic style gradient" (Array.for_all2 close gradient [|4.4375;5.3125;10.1875;12.1875;15.9375;19.0625;21.6875;25.9375|]);
  let quadratic v = A.Maths.(sum' (v * v)) in
  let start = N.of_array [|2.;-3.|] [|2|] in
  let old_params = Neural_transfer.adam_params ~ckpt:4 ~learning_rate:0.1 5. in
  let old_state, old_img = Optimise.minimise_fun old_params quadratic (A.Arr start) in
  let old_x = ref old_img in
  while Optimise.Checkpoint.(old_state.current_batch < old_state.batches) do
    Optimise.Checkpoint.(old_state.stop <- false);
    let _, img = Optimise.minimise_fun ~state:old_state old_params quadratic !old_x in old_x := img
  done;
  check "original strict condition reproduces missing fifth update" (old_state.current_batch=5);
  let state, resumed = Neural_transfer.optimise ~ckpt:4 ~learning_rate:0.1 5. quadratic start in
  let _, whole = Neural_transfer.optimise ~ckpt:100 ~learning_rate:0.1 5. quadratic start in
  check "resume executes exact final update after penultimate checkpoint" (state.current_batch=6);
  check "resume preserves all Adam state" (Array.for_all2 close (N.to_array resumed) (N.to_array whole));
  let small = Style_io.load ~size:(32,32) (Filename.concat inputs "hojo.png") in
  Style_io.save small "test input.png";
  Neural_transfer.run ~weights:(Filename.concat weights "vgg19") ~ckpt:1
    ~src:"test input.png" ~style:"test input.png" ~dst:"test output.png" 2.;
  check "public NST wrapper and quoted image paths" (N.shape (Style_io.load "test output.png")=[|1;32;32;3|]);
  check "actual checkpoint file after one update" (Sys.file_exists "test output_0001.png");
  let pixels = N.of_array (Array.init 18 (fun i -> float_of_int i *. 15.)) [|1;2;3;3|] in
  check "RGB preprocessing inverse" (Array.for_all2 close (N.to_array pixels) (N.to_array (ImageUtils.(preprocess pixels |> postprocess))));
  let content = Style_io.load ~size:(224,224) (Filename.concat inputs "hojo.png") |> ImageUtils.preprocess in
  let style = Style_io.load ~size:(224,224) (Filename.concat inputs "hokusai.png") |> ImageUtils.preprocess in
  let nn = Neural_transfer.load_network ~weights:(Filename.concat weights "vgg19") 224 224 in
  check "VGG complete nodes" (Array.length nn.topo=39);
  let deep = Neural_transfer.content_objective ~idx:30 nn content in
  let probe = N.gaussian [|1;224;224;3|] in
  let _, derivative = A.grad' deep (A.Arr probe) in
  let derivative = A.unpack_arr derivative in
  let direction = N.div_scalar derivative (N.max' (N.abs derivative)) in
  let predicted = N.sum' (N.mul derivative direction) in
  let evaluate pixels = A.unpack_flt (deep (A.Arr pixels)) in
  let measured = (evaluate (N.add probe (N.mul_scalar direction 0.01)) -.
                  evaluate (N.sub probe (N.mul_scalar direction 0.01))) /. 0.02 in
  check "real 224 VGG directional gradient agrees with central difference" (abs_float(predicted-.measured) < 0.1 *. max 1. (abs_float predicted));
  Printf.printf "DEEP_GRADIENT predicted=%.9g central_difference=%.9g\n%!" predicted measured;
  let objective = Neural_transfer.objective nn content style in
  let initial = A.unpack_flt (objective (A.Arr content)) in
  Printf.printf "INITIAL_LOSS %.9g\n%!" initial;
  let started = Unix.gettimeofday () in
  let state, result = Neural_transfer.optimise ~ckpt:50 ~checkpoint:(fun step _ -> Printf.printf "NST step %d\n%!" step) 250. objective content in
  let final = A.unpack_flt (objective (A.Arr result)) in
  check "exact 250 NST updates" (state.current_batch=251);
  Printf.printf "FINAL_NST initial=%.9g final=%.9g\n%!" initial final;
  check "real NST objective improves" (Float.is_finite final && final < initial);
  check "finite NST pixels" (Array.for_all Float.is_finite (N.to_array result));
  Style_io.save ~preprocessed:true result "hojo-wave.png";
  let metrics = Printf.sprintf "{\"checks\":%d,\"steps\":%d,\"initial_loss\":%.17g,\"final_loss\":%.17g,\"seconds\":%.6f,\"old_resume_steps\":4,\"fixed_resume_steps\":5}\n" !checks (state.current_batch-1) initial final (Unix.gettimeofday ()-.started) in
  Owl_io.write_file "native-metrics.json" metrics;
  Printf.printf "NST_RESULT initial=%.9g final=%.9g seconds=%.3f checks=%d\n%!" initial final (Unix.gettimeofday ()-.started) !checks
