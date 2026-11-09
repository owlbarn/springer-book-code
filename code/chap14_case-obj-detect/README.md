# Chapter 14: Instance segmentation

Run `python3 scripts/run_chapter14.py --output /tmp/chapter14` in the book image.
All eight original blocks are mapped. The first seven concatenate to the complete
`Model.mrcnn` constructor, rather than being independently valid REPL phrases;
`book_mrcnn.ml` contains the actual run/render entry. Native tests perform complete
1024 inference, every parameter comparison and per-instance masks/boxes; the real
interpreter checks architecture stage shapes separately. Run the reader command
before using the printed library-loading setup, since it builds the bytecode CMAs.

The original project is frozen at `owlbarn/owl_mask_rcnn` commit
`838d921401676271f96e867452e21b829aff29ec` with original file/archive hashes in
`upstream.json`. No separate LICENSE was present in that tree; its original
attribution and provenance are retained without asserting a new license. All
runtime source is here; old weight-conversion and video examples remain as
historical references outside the executed book scope.

The original author HDF5 exchange file is fixed by URL, exact size and SHA256 in
`portable-manifest.json`. All 690 datasets / 64,158,584 unique parameters are
exactly representable as float32. The build-only Python/HDF5 stage writes a fixed
standard NPY format and verifies every byte hash; only the 248 MB portable cache
enters the reader image. No TensorFlow, h5py or original 490 MB exchange file is
needed at runtime. RPN sharing loads 714 tensors / 68,916,160 values; current CGraph
compares each loaded shape and value, and uses all 690 original datasets. We do
not attempt unsafe decoding of the old serialized computation graph.

The actual book photo is Mark Pegrum's CC BY 2.0 Regent Street image, not the
upstream file called buildings.jpg (which shows Toronto). The original Flickr
2048×1536 JPEG, exact hash and bottom 2048×916 crop are retained with
`IMAGE-NOTICE.md`. The native runner regenerates London masks, boxes and labels;
the figure is exported from that actual execution. Sheep/Toronto were temporary
probes and are not the book's data. Random seeds are fixed; inference timings and
model results are recorded, not fabricated historical values.

Restoration fixes current ndarray index syntax, current PNG/JPEG CamlImages
library names and public stage aliases. A numerical regression found the original
fixed +1e-5 IoU denominator made a tiny identical box have self IoU ~0.0909. The
helper now guards only a nonpositive union, preserving positive-area IoU scale
invariance; explicit self/scale/zero-union and NMS cases cover it. Other tests check
zero deltas, clipping, normalized-coordinate roundtrips, hand-computable bilinear
interpolation, RGB pre/postprocessing, metadata, empty detections, original-image
bounds, binary masks and expected London bus/person classes.

CamlImages5.0.5 is pinned with an explicit Dune `:standard` C-flags repair; otherwise
current shared-stub linking loses -fPIC. The source's seven patched file hashes
are checked before image validation. Base/StdIO <v0.17 are retained for this
release's configurator. The book switch remains the independently resolved latest
stable OCaml and Owl main; no compiler downgrade or historical reader matrix is
introduced. The font and original source attribution are preserved.
