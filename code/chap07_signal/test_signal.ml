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
  let csv filename separator = Owl_io.read_csv ~sep:separator filename
    |> Array.map (Array.map float_of_string) |> Mat.of_arrays in
  let sunspots=csv "sunspot_full.csv" ';' in
  check "synthetic solar input shape" (Mat.shape sunspots=(321,2));
  check "synthetic solar values finite and nonnegative"
    (Array.for_all Float.is_finite (Mat.to_array sunspots) && Mat.min' (Mat.col sunspots 1)>=0.);
  check "one-year sampling interval" (Array.for_all (fun i -> Mat.get sunspots i 0=1700.+.float_of_int i) (Array.init 321 Fun.id));
  let magnitudes=Book_signal.get_frequency (Mat.col sunspots 1) |> Mat.to_array in
  let peak=ref 0 in Array.iteri (fun i x -> if x>magnitudes.(!peak) then peak:=i) magnitudes;
  let period=321. /. float_of_int (!peak+1) in
  near "generated cycle recovered by actual FFT" 11. period 0.2;
  let tone=csv "touchtone.csv" ',' |> fun x -> Mat.div_scalar x 128. in
  check "eleven synthetic tone segments" (Mat.shape tone=(1,66000));
  check "normalized tone amplitude" (Mat.max' (Mat.abs tone)<1.);
  let decoded=Bytes.make 11 '?' in
  let rows=[|697.;770.;852.;941.|] and cols=[|1209.;1336.;1477.|] in
  let nearest values target =
    let found=ref 0 in Array.iteri (fun i x -> if abs_float(x-.target)<abs_float(values.(!found)-.target) then found:=i) values;!found in
  for digit=0 to 10 do
    let offset=digit*6000 in
    let segment=Mat.get_slice [[];[offset;offset+4999]] tone in
    let spectrum=Owl_fft.D.rfft ~axis:1 segment |> Z.abs |> Z.re |> Mat.to_array in
    let strongest lower upper =
      let first=int_of_float(ceil(lower*.5000./.8192.)) and last=int_of_float(floor(upper*.5000./.8192.)) in
      let k=ref first in for i=first+1 to last do if spectrum.(i)>spectrum.(!k) then k:=i done;
      float_of_int !k *.8192./.5000. in
    let low=strongest 600. 1000. and high=strongest 1100. 1600. in
    let row=nearest rows low and col=nearest cols high in
    near "DTMF row peak" rows.(row) low 2.;
    near "DTMF column peak" cols.(col) high 2.;
    Bytes.set decoded digit [|"123";"456";"789";"*0#"|].(row).[col];
    check "tone gaps are silent" (Mat.max' (Mat.abs (Mat.get_slice [[];[offset+5000;offset+5999]] tone))=0.)
  done;
  let decoded=Bytes.to_string decoded in
  check "all generated digits decoded from actual spectrum" (decoded="12345678901");
  let raw=Owl_io.read_csv ~sep:',' "goog.csv" in
  let prices=Array.sub raw 1 (Array.length raw-1) |> Array.map(fun row -> Array.sub row 1 6 |> Array.map float_of_string) |> Mat.of_arrays in
  check "251 generated OHLCV observations" (Mat.shape prices=(251,6));
  check "finite positive synthetic prices" (Array.for_all (fun x -> Float.is_finite x && x>0.) (Mat.to_array prices));
  check "generated high and low bracket open and close" (Array.for_all(fun i ->
    let o=Mat.get prices i 0 and h=Mat.get prices i 1 and l=Mat.get prices i 2 and c=Mat.get prices i 3 in
    l<=min o c && h>=max o c) (Array.init 251 Fun.id));
  let y=Mat.col prices 3 in
  let average=Book_signal.moving_average 10 y in
  let convolution=Arr.conv1d ~padding:VALID (Arr.reshape y [|1;251;1|]) (Arr.create [|10;1;1|] 0.1) [|1|] |> fun x -> Arr.reshape x [|242;1|] in
  near "actual CSV convolution agrees with moving average" 0. Mat.(l2norm' (average-convolution)) 1e-10;
  let roughness y = let a=Mat.to_array y in
    let sum=ref 0. in for i=1 to Array.length a-1 do sum:= !sum+.((a.(i)-.a.(i-1))**2.) done;
    !sum/.float_of_int(Array.length a-1) in
  check "moving average reduces high-frequency variation" (roughness average<roughness y);
  let smooth=Book_signal.lowpass ~keep:5 y in
  check "FFT filter retains odd input length" (Mat.shape smooth=(251,1));
  near "FFT filter preserves actual CSV mean" (Mat.mean' y) (Mat.mean' smooth) 1e-10;
  check "FFT filter reduces high-frequency variation" (roughness smooth<roughness y);
  Owl_io.write_file "input-checks.json" (Printf.sprintf
    "{\"checks\":%d,\"synthetic\":true,\"recovered_period_years\":%.9g,\"decoded_digits\":\"%s\",\"stock_rows\":251}\n" !count period decoded);
  Printf.printf "PASS: %d signal/FFT property checks\n" !count
