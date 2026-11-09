open Owl
let count = ref 0
let check name condition = incr count; if not condition then failwith name
let near name expected actual tol = check name (Float.is_finite actual && abs_float (expected -. actual)<tol)
let () =
  let module Z = Dense.Ndarray.Z in
  let real = Arr.of_array [|1.;2.;1.;-1.;1.5;1.|] [|6|] in
  let complex = Dense.Ndarray.Generic.cast_d2z real in
  let spectrum = Owl_fft.D.fft complex in
  let inverse = Owl_fft.D.ifft spectrum in
  Array.iter2 (fun a b -> near "complex FFT roundtrip" 0. (Complex.norm (Complex.sub a b)) 1e-10) (Z.to_array complex) (Z.to_array inverse);
  let expected = [|5.5;2.25;-2.75;1.5;-2.75;2.25|] in
  Array.iter2 (fun value reference -> near "known DFT real parts" reference value.Complex.re 1e-10) (Z.to_array spectrum) expected;
  List.iter (fun n ->
    let input = Arr.init [|n;1|] (fun i -> sin (float_of_int i) +. 0.2 *. float_of_int i) in
    let transformed = Owl_fft.D.rfft ~axis:0 input in
    let restored = Book_signal.irfft_exact ~axis:0 ~n transformed in
    check "real FFT shape preserved for even and odd lengths" (Arr.shape input = Arr.shape restored);
    near "real FFT roundtrip" 0. Arr.(l2norm' (input-restored)) 1e-10
  ) [6;7;250;251];
  let nsamples = 330 in
  let signal = Mat.init nsamples 1 (fun i -> 100. +. sin (2. *. Float.pi *. float_of_int i /. 11.)) in
  let magnitude = Book_signal.get_frequency signal |> Mat.to_array in
  let peak = ref 0 in
  Array.iteri (fun i v -> if v > magnitude.(!peak) then peak:=i) magnitude;
  near "11-year period uses true FFT bin indices" 11. (float_of_int nsamples /. float_of_int (!peak+1)) 1e-12;
  let y = Mat.sequential ~a:1. 21 1 in
  let average = Book_signal.moving_average 10 y in
  check "valid average length" (Mat.shape average = (12,1));
  near "first average" 5.5 (Mat.get average 0 0) 1e-12;
  near "last average" 16.5 (Mat.get average 11 0) 1e-12;
  let convolution = Arr.conv1d ~padding:VALID (Arr.reshape y [|1;21;1|]) (Arr.create [|10;1;1|] 0.1) [|1|] |> fun x -> Arr.reshape x [|12;1|] in
  near "convolution equals manual average" 0. Arr.(l2norm' (convolution-average)) 1e-10;
  let kernel = Book_signal.gaussian_kernel 3. |> Mat.to_array in
  near "Gaussian normalized" 1. (Array.fold_left (+.) 0. kernel) 1e-12;
  check "Gaussian odd kernel length" (Array.length kernel = 25);
  Array.iteri (fun i v -> near "Gaussian symmetric" v kernel.(Array.length kernel-1-i) 1e-12) kernel;
  check "invalid sigma rejected" (try ignore (Book_signal.gaussian_kernel 0.);false with Invalid_argument _->true);
  let constant = Mat.create 251 1 4. in
  let filtered = Book_signal.lowpass ~keep:5 constant in
  check "odd-length filtered signal retains shape" (Mat.shape filtered = (251,1));
  near "DC preserved by lowpass" 0. Mat.(l2norm' (filtered-constant)) 1e-10;
  let module S = Dense.Ndarray.S in
  let module C = Dense.Ndarray.C in
  let image = S.sequential [|5;7|] |> Dense.Ndarray.Generic.cast_s2c in
  let restored = Owl_fft.S.fft2 image |> Owl_fft.S.ifft2 in
  near "2D FFT single-precision roundtrip" 0. C.(l2norm' (restored-image)).Complex.re 1e-3;
  Printf.printf "PASS: %d signal/FFT property checks\n" !count
