open Owl 
open Owl_plplot 

let fs = 8192.

let plot_tone data filename =
  let x = Mat.div_scalar (Mat.sequential 1 (Arr.shape data).(1)) fs in
  let h = Plot.create filename in
  Plot.set_font_size h 8.;
  Plot.set_pen_size h 3.;
  Plot.set_xlabel h "time(s)";
  Plot.set_ylabel h "signal magnitude";
  Plot.plot ~h ~spec:[ RGB (0, 0, 255); LineStyle 1] x data;
  Plot.output h
let plot_pair data filename =
  let n=Mat.col_num data in
  let times=Mat.sequential 1 n |> fun x -> Mat.div_scalar x fs in
  let magnitude=Owl_fft.D.rfft ~axis:1 data |> Dense.Ndarray.Z.abs |> Dense.Ndarray.Z.re in
  let frequencies=Mat.linspace 0. (fs/.2.) (Mat.col_num magnitude) in
  let h=Plot.create ~m:1 ~n:2 filename in
  Plot.subplot h 0 0;
  Plot.set_xlabel h "time (s)"; Plot.set_ylabel h "synthetic signal";
  Plot.plot ~h times data;
  Plot.subplot h 0 1;
  Plot.set_xlabel h "frequency (Hz)"; Plot.set_ylabel h "FFT magnitude";
  Plot.plot ~h frequencies magnitude;
  Plot.set_xrange h 0. 2000.;
  Plot.output h
