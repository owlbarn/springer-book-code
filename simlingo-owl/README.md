# SimLingo inference with Owl

Independent OCaml implementation of the `epoch=013.ckpt/pytorch_model.pt`
SimLingo / InternVL2-1B model described in [owl-plan.md](../owl-plan.md).
All neural computation runs through `Owl.Dense.Ndarray.S`. No Owl source changes,
Python inference bridge, training, GPU, or network access are required at runtime.

The CLI takes an RGB PPM frame, an ASCII question, current speed in m/s, and two
target points in the model's ego coordinate system. It produces an English
answer, a 20×2 route, and 10×2 speed waypoints in JSON. These are raw model
predictions, before SimLingo's controller or route resampling.

Model sources, checkpoint identity and licence terms are recorded in
[MODEL-LICENSES.md](MODEL-LICENSES.md).

## Build

Install OCaml >= 4.14, Dune >= 3, Owl, Yojson and Sha, then:

```sh
cd simlingo-owl
opam install owl yojson sha dune
dune build
```

For a locally built Owl, expose its existing installation without modifying it:

```sh
export OCAMLPATH=/path/to/owl/_build/install/default/lib${OCAMLPATH:+:$OCAMLPATH}
dune build
```

## Convert weights once

Use a Python environment containing PyTorch and NumPy. `BASE` is the local
InternVL2-1B Hugging Face snapshot directory, including tokenizer assets.
The converter expects the 992-tensor SimLingo checkpoint and rank-32/alpha-64
LoRA. Output must be a new directory. It verifies duplicate weights, merges
168 LoRA pairs, converts to fp32, transposes linear/patch weights, and writes a
SHA-256/shape manifest. A manifest is written only after successful conversion.

```sh
python tools/convert.py --checkpoint "$CHECKPOINT" --base "$BASE" --out artifacts/weights
```

Converted weights contain 939,599,104 parameters (~3.76 GB). We recommend 16 GB of system
RAM for the workflow. A complete straight-road run reached 4.41 GB peak resident
memory; the separate conversion reached 3.06 GB (Linux `ru_maxrss`, measured on
2026-11-16). The inference ran to EOS in a container limited to 16 GiB with swap
disabled. These figures include the processes' intermediate arrays and tokenizer. Large generated data stays in
the ignored `artifacts/` directory. The loader checks each tensor's checksum and
shape on first use and keeps it in memory for subsequent decoding steps.

## Run a frame

Convert an image to binary RGB PPM with Pillow (this step only decodes pixels):

```sh
python -c 'from PIL import Image; Image.open("frame.png").convert("RGB").save("frame.ppm")'
printf '[[20.0, 0.0], [40.0, 0.0]]\n' > targets.json
OPENBLAS_NUM_THREADS=4 OMP_NUM_THREADS=4 \
  _build/default/bin/main.exe \
  --weights artifacts/weights --image frame.ppm \
  --speed 9.5 --targets targets.json \
  --question 'What should the ego do next?' --output output.json
```

The target points above are an illustrative straight-ahead navigation input;
use the actual recorded targets for a real frame. OCaml resizes to 896×448,
splits two tiles, and applies ImageNet normalization. `--crop-bottom 0.3`
reproduces the bottom crop used by the selected OverDrive camera captures;
default 0 preserves the supplied frame. JPEG round-tripping and PIL-exact
interpolation are intentionally omitted, as allowed by the plan.

The upstream chat template has no system message. Two `<TARGET_POINT>` tokens
are replaced by the waypoint encoder output, and 512 `<IMG_CONTEXT>` tokens
by projected image features. Generation is greedy, with EOS 151645 and at most
100 new tokens; EOS is included before appending the 30 driving queries.
Both output heads use cumulative sums along the point sequence.

The default follows the book's full-sequence implementation: each generated
token reruns all 24 Qwen2 layers over the complete sequence. Add `--kv-cache`
to reuse previous keys and values. The first pass processes the prompt; later
passes process one new token each. The final selected token and 30 driving
queries form one causal chunk, including EOS when it was generated.

Each request owns its cache. It stores the two KV heads per layer, with RoPE
positions continuing from the cached prefix. Storage is allocated once for
the prompt, token limit and driving queries; a 546-row prompt with a limit of
100 reserves 15.84 MiB of FP32 keys and values. Model weights and prefill arrays
still account for most memory. `--max-new-tokens 3` is useful
for debugging but truncates the answer and changes the driving-head context;
check `eos_reached` in the output. A truncated run is not a complete demo.

## Verification

The reference exporter uses the original checkpoint and upstream InternViT /
Transformers Qwen2 modules on CPU fp32. It independently merges LoRA in PyTorch
layout, without reading the converted Owl weights. It consumes the exact pixel
tensor exported by Owl so image interpolation differences do not obscure model
arithmetic. This checks the fp32 port, not bitwise equivalence with deployment
in mixed precision or CUDA.

Use the existing SimLingo Python environment (tested with Transformers 4.46.3,
PyTorch 2.2.0,
NumPy, timm and einops) for reference export and fixture generation:

```sh
python tools/test_fixtures.py --base "$BASE" --out artifacts/tests
dune build test/check.exe
OPENBLAS_NUM_THREADS=2 OMP_NUM_THREADS=2 \
  _build/default/test/check.exe artifacts/tests artifacts/weights/tokenizer

mkdir -p artifacts/frame/owl
OPENBLAS_NUM_THREADS=4 OMP_NUM_THREADS=4 \
  _build/default/bin/main.exe --weights artifacts/weights \
  --image frame.ppm --speed 9.5 --targets targets.json \
  --dump artifacts/frame/owl --output artifacts/frame/owl/output.json

python tools/reference.py --checkpoint "$CHECKPOINT" --base "$BASE" \
  --pixels artifacts/frame/owl/pixels.npy --targets targets.json --speed 9.5 \
  --out artifacts/frame/reference
python tools/compare.py artifacts/frame/owl artifacts/frame/reference \
  --out artifacts/frame/comparison.json
```

Reference and Owl must use the same speed, targets, question and token limit.
`--dump` saves ViT layer 1, projected image features, merged input embeddings,
Qwen2 layer 1, first-step logits and prompt IDs. `--input-embeddings FILE`
replays an exported merged sequence; `--forward-only` checks one Qwen2 forward.
`--pixels FILE` bypasses image preprocessing using a `[2,448,448,3]` fp32 NPY.

Tests compare 258 English/ASCII tokenization and decoding cases to the upstream
tokenizer, including whitespace, contractions and special tokens. Independent
PyTorch fixtures cover exact GELU, SiLU, RoPE, causal GQA and pixel-shuffle order.
A PPM regression checks that whitespace-valued first pixels are not lost.
The loader tests reject incorrect shapes and corrupted checksums. Another 17
cases check UTF-8 decoding, including token-limited partial characters.
Non-ASCII questions fail explicitly; decoding handles UTF-8 vocabulary bytes.

The cache regression uses real weights and a previously exported straight-road
input. It checks chunked and single-token passes, causal masking, position
offsets, capacity, reset, failed-append recovery and request isolation. It also
checks token-limit stops and compares every decoding step's logits and the
final driving outputs against the full-sequence implementation:

```sh
dune build test/cache.exe
OPENBLAS_NUM_THREADS=4 OMP_NUM_THREADS=4 \
  _build/default/test/cache.exe artifacts/weights \
  artifacts/frame/owl/input_embeddings.npy artifacts/frame/owl/output.json \
  artifacts/frame/cache-checks.json
```

Use an untruncated straight-road run for these input/output files. Greedy IDs
and EOS must match exactly; arrays are compared with an absolute tolerance of
0.001 (metres for driving outputs). FP32 results can differ slightly when
matrix operations process different numbers of rows.

## Recorded frames

To prepare an OverDrive recording, select only rows with available images:

```sh
python tools/prepare_frame.py --frames /path/to/frames.json --out artifacts/frame
python tools/run_frame.py --frame artifacts/frame --weights artifacts/weights
python tools/plot_demo.py artifacts/frame --out artifacts/figures
```

`input.json` records speed, targets, question, crop and source image hash.
The plotter exports PNG/PDF figures and a compact JSON record for later use in
the book. Four selected source frames, inputs and compact results are retained
in `demo/`; full recordings and checkpoints remain external inputs. Replay a
retained frame with:

```sh
python tools/run_frame.py --frame demo/straight --weights artifacts/weights \
  --out artifacts/replay-straight
```

For cached inference, append `--kv-cache` and choose a new output directory:

```sh
python tools/run_frame.py --frame demo/straight --weights artifacts/weights \
  --kv-cache --out artifacts/replay-straight-cached
```

`tools/book_run.py` also accepts `--kv-cache`. Use `--measure-memory` to record
each complete inference process's elapsed time and peak RSS in the reference
container. Run the two modes sequentially with separate `--out` directories.

```sh
python tools/book_run.py --out artifacts/kv-baseline --measure-memory
python tools/book_run.py --out artifacts/kv-cached --measure-memory --kv-cache
python tools/compare_cache.py artifacts/kv-baseline artifacts/kv-cached \
  --out artifacts/kv-comparison.json
```

The comparison checks identical source/weight hashes, complete token sequences,
EOS and all coordinates. Measurements include image processing, first-use weight
loading and checksum verification, text generation and the driving heads.

Measured on 2026-11-16 with the reference image, an i9-13900KF and four BLAS
threads (one sequential replay per mode):

| Frame | Full sequence | KV cache | Speedup |
| --- | ---: | ---: | ---: |
| straight | 174.77 s | 42.32 s | 4.13× |
| curve | 116.27 s | 40.01 s | 2.91× |
| close_follow | 147.21 s | 40.98 s | 3.59× |
| brake | 131.32 s | 39.72 s | 3.31× |

Peak RSS remained about 4.10–4.12 GiB in both modes. All four answers and token
sequences, including EOS, matched exactly; the largest trajectory difference
was 0.00000954 m. Source hashes, per-scene measurements and the 44 numerical
cache checks are recorded in [kv-cache-verification.json](demo/kv-cache-verification.json).

The wrapper decodes the retained PNG to PPM and runs the OCaml executable,
streaming progress and saving it in `run.log`. Its `--threads` option defaults
to 4 and explicitly sets both BLAS and OpenMP thread counts.
See [demo/README.md](demo/README.md) for measured results and the retained
braking-frame counterexample.

## CGraph with KV cache

Add `--cgraph` to evaluate Qwen2's 24 Transformer blocks with Owl's CPU
computation graph. This option includes KV caching:

```sh
python tools/run_frame.py --frame demo/straight --weights artifacts/weights \
  --cgraph --out artifacts/replay-straight-cgraph
```

Each block uses two optimised graphs: RMSNorm, Q/K/V projections and RoPE;
then attention, the output projection, residuals and the gated MLP. The graphs
share scratch buffers across layers and are reused while the chunk shape stays
the same. A shape change, such as moving from prompt processing to one-token
decoding, replaces the graphs. Weights bind to non-reusable input nodes without
copying their arrays. Returned hidden states own their storage.

The KV buffers and their append positions remain OCaml state. Attention uses
the allocated cache capacity, with unused positions filled with zeros and
masked before softmax. This keeps the decoding graph's shape fixed as the
prefix grows. Vision, embedding lookup, the final RMSNorm, language projection
and trajectory heads use the ndarray implementation. Vision retains its exact
GELU; the current CGraph operator set does not expose `erf`.

Run the same numerical regression against the full-sequence implementation:

```sh
_build/default/test/cache.exe artifacts/weights \
  artifacts/frame/owl/input_embeddings.npy artifacts/frame/owl/output.json \
  artifacts/frame/cgraph-checks.json --cgraph
```

To compare complete frames against the ndarray KV path in the reference image:

```sh
python tools/book_run.py --kv-cache --measure-memory --out artifacts/cgraph-kv
python tools/book_run.py --cgraph --measure-memory --out artifacts/cgraph-graph
python tools/compare_cache.py artifacts/cgraph-kv artifacts/cgraph-graph \
  --cgraph --out artifacts/cgraph-comparison.json
```

Measured on 2026-11-16 in the same four-thread reference environment, including
graph construction and optimisation:

| Frame | Ndarray KV cache | CGraph + KV cache | Speedup |
| --- | ---: | ---: | ---: |
| straight | 42.07 s | 35.13 s | 1.20× |
| curve | 39.11 s | 34.00 s | 1.15× |
| close_follow | 40.95 s | 34.66 s | 1.18× |
| brake | 39.24 s | 34.27 s | 1.15× |

CGraph peak RSS was 4.150–4.153 GiB, compared with 4.103–4.150 GiB for the
ndarray KV path. All four answers and token sequences matched exactly; the
largest trajectory difference was 0.00000763 m. The 45 numerical comparisons,
input/weight ownership checks and measurements are recorded in
[cgraph-verification.json](demo/cgraph-verification.json).

## Code map

| Module | Responsibility |
| --- | --- |
| `Weights` | Manifest validation and lazy cached NPY loading |
| `Nn` | Dense-array normalization, activations, linear and per-head attention |
| `Vit` | InternViT, pixel-shuffle v2 and image projection |
| `Qwen2` | RoPE, GQA, causal attention and SwiGLU |
| `Qwen2_cached` | Per-layer KV buffers, absolute positions and causal chunk append |
| `Cgraph_block` | Reusable CPU graphs and read-only weight bindings for a Qwen2 layer |
| `Qwen2_cgraph` | CGraph layer execution with fixed-capacity KV attention |
| `Tokenizer` | ASCII pretokenization, byte BPE and UTF-8 byte decoding |
| `Preprocess` | P6 PPM parsing, optional crop, bilinear resize and normalization |
| `Simlingo` | Prompt, placeholder replacement, greedy decode and driving heads |
| `Simlingo_cached` | Cached decoding and the final driving-query chunk |

This directory is self-contained and can be moved into its own repository.
It is intentionally outside the book's automatically executed chapter examples:
verification requires the external multi-gigabyte checkpoint and recorded frames.
