# Chapter 7: signal processing (partial input coverage)

`listings.json` maps all 31 original blocks; `sessions/chapter07.ml` is the full
sequential source. Seventeen blocks can currently execute; fourteen depending
on the missing original CSVs are explicitly NOT RUN. The old stock-spectrum
output is preserved as historical, never presented as newly executed output.
The input investigation is in `input-sources.json`; external files fetched for
inspection remain in `/tmp`, outside version control and the reference image.

The original `cat_noise.ppm` is copied unchanged from the manuscript repository's
`code/signal/cat_noise.ppm`. `imageUtils.ml` uses the corrected PPM implementation
also used by Chapter 4. The old moonlanding input is retained historically, but
the program now uses the cat and derives mesh dimensions from the actual image.

`book_signal.ml` provides the corrected frequency-bin/period axes, normalized
Gaussian kernel, valid moving average, odd-length-safe spectral lowpass, and mesh
plotting. `irfft_exact` reconstructs the conjugate complex spectrum for the
requested real length, because the current Owl native irfft guard rejects odd
lengths even with `~n`. The image example uses double precision: current Owl's
complex32 bytecode FFT is bound to the float64 trampoline and segfaulted in the
actual interactive example. Native single-precision numerical tests are recorded
separately and do not imply that bytecode entry works. Tests use explicit mathematical fixtures; those fixtures are not
substitutes for the original book CSV datasets. Sampling times use k/fs, not an
endpoint-inclusive linspace. Inverse real FFT specifies the original n, and the
conv1d comparison selects VALID padding and actual input/kernel dimensions.

Run `python3 scripts/run_chapter07.py` in the book container. It compiles all
chapter programs, executes the FFT/property tests and both real plot entrypoints,
then runs each unblocked canonical region. Seven book figures are generated.
The stock, touch-tone and annual-sunspot examples remain separate unresolved
input cases and their old book figures are not claimed as regenerated.
