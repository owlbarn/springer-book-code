# Chapter 13: Image recognition

Run `python3 scripts/run_chapter13.py --output /tmp/chapter13` in the book image.
All 23 original blocks are mapped. The original InceptionV3 graph, panda image,
ImageNet dictionary and 2019 weights are fixed by raw URL, Git object, size and
SHA256 in `inputs.json`. Native and real interactive examples perform complete
pretrained inference; the native runner checks every loaded parameter and shape,
modern weight save/reload, probability bounds/sum, all RGB pixels of a nonsquare
PPM fixture, factorized-convolution shapes/parameter counts and result wrappers.
ImageMagick is installed and receives paths as separate process arguments.

The original weights contain an old Bigarray Marshal format refused by current
OCaml. `docker/legacy/` builds an isolated OCaml 4.08.1 reader from a source archive
with a fixed SHA256. It reads only the pinned old file's primal-weight schema and
exports 314 nodes, 566 standard little-endian float32 NPY tensors and 23,886,216
finite parameters. `portable-manifest.json` fixes all output hashes. The final
book image includes the portable cache, dictionary and panda, with the current
OCaml compiler; the historical compiler and old weight file are excluded. This is
one data migration stage, not another reader platform or compatibility matrix.
The current loader compares every loaded tensor shape/value before inference.
The original panda gets 96.196544%, consistent with the printed first-edition 96.20%.

`ImageUtils` is built from the restored Chapter 4 source via Dune copy, avoiding
another drifting hand-maintained copy. Pixel snippets use RGB channels 0/1/2,
height/width in row-major order and an exact binary P6 header/data boundary; no
extra space or compensating rotations are inserted. They run on a nonsquare
fixture and check reconstruction. The actual panda remains in the verified reader cache; the originally commented
book illustration remains inactive.

Three original partial Python/TensorFlow/Keras/HDF5 fragments containing ellipses
remain explicitly non-executable illustrations of weight exchange. They are not
included in execution counts and do not add TensorFlow/HDF5 to the reader runtime.
The prepared original weights provide the fully executed prediction path. The old
Zoo run is replaced by the tested local runner; remote gist execution is not a
runtime dependency. `gists.md` retains historical architecture references.
