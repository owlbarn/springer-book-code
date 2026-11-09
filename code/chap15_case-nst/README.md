# Chapter 15: neural and fast style transfer

Run inside the book image, from `/opt/book`:

```sh
python3 scripts/run_chapter15.py --output /tmp/chapter15
```

This is a full native computation: 250 Adam updates for combined NST; five content
and five cumulative style reconstructions with 100 updates each; all six original
FST models, and two 1500×1500 outputs from the book author's original Budapest photo.
Expect minutes of CPU work. Actual initial/final losses, update counts and timings
are recorded; no old output is used as a current result. The short separate OCaml
session checks the same loaded modules and analytic shapes without repeating training.
After running the command, load the built bytecode library as shown in the book.

The fixed original VGG19 and six FST weights are downloaded and verified in a
build-only OCaml 4.08.1 reader, then losslessly exported to portable NPY. Readers
use only current OCaml/Owl; the legacy reader and original Marshal weights are absent
from the final image. `prepare_style.py` verifies every cached file. Every loaded
parameter is compared element by element. Provenance and exact inputs are in
`upstream/`, `inputs.json`, `portable-manifest.json` and `data/IMAGE-NOTICE.md`.

The VGG network has 39 nodes including input (38 non-input nodes). Layer indices
are zero based, including input. Content uses node 23; style uses nodes 2, 7, 12,
21 and 30. The original normalisations are retained: Gram = XᵀX/(FHW), content
MSE = sum squared feature differences/(FHW), style = sum squared Gram differences/F².
The combined objective uses content weight 5 and each of five style weights 100,
as in the original source; the ratio of each style weight to content is 20.

Restored executable excerpts fix unbound original `nn`/`idx`, malformed `pow`, and
unbound `response`/`target`. Resuming Adam retains the same state and executes the
last update even when the preceding update was a checkpoint (`current_batch` is the
next update, so continue while `<= batches`). Five-update analytic regression
reproduces the original missing fifth step and compares resumed versus continuous
Adam. FST's SAME stride rounds arbitrary dimensions up to multiples of four; only
the extra bottom/right border is cropped to preserve the original image dimensions.
Native and interactive checks include a 25×31 nonsquare input. PPM handling is shared
with Chapter 4; ImageMagick arguments are quoted and temporary files are removed.

`upstream/*.ml` are original reference snapshots, excluded from compilation.
The Gist's different sample pictures are not substituted for the book's Hojo,
Great Wave and Budapest inputs. Diagram `fst.pdf` is explanatory and is not generated
by model inference. Original attribution is retained; no extra licence is asserted.

The reference Owl main also needs the recorded five-line blocked-convolution
accuracy patch: absolute matrix offsets are recomputed for every channel/kernel
block instead of accumulating. This was verified with repeated full-tensor exact
uniform and nonuniform convolution checks and an actual 224-pixel VGG directional
gradient comparison. Without the correction, deep NST/reconstruction can diverge;
small 32-pixel gradient tests alone missed it. The patch and original/patched file
hashes are recorded under `docker/patches/`; image validation rejects missing or
changed correction. Full Inception and London Mask R-CNN inference have also been
rerun under the patched environment.
