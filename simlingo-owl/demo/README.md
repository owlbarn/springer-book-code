# Recorded demonstration

These are offline single-frame predictions from the epoch-013 SimLingo
checkpoint. The PNGs, speed, two target points and question are retained so the
examples can be replayed without the full OverDrive recordings. Model weights
remain in the ignored `artifacts/weights/` directory.

All four inputs use the same question, `What should the ego do next?`, and a
30% bottom crop before the two-tile resize. `input.json` records the original
source path and image SHA-256. `output.json` records the Owl prediction;
`comparison.json` compares it with the independent PyTorch fp32 reference.

Verified on 2026-11-14: all four prompt sequences and complete generated token
sequences match the reference exactly, including EOS. All output shapes are
20×2 / 10×2 and all coordinates are finite.

| Frame | Generated tokens (including EOS) | CPU wall time | Max route difference (m) | Max speed-waypoint difference (m) |
| --- | ---: | ---: | ---: | ---: |
| straight | 31 | 319 s | 1.91e-6 | 3.81e-6 |
| curve | 17 | 243 s | 1.91e-6 | 7.63e-6 |
| brake | 21 | 263 s | 1.91e-6 | 1.53e-5 |
| close_follow | 24 | 199 s | 2.64e-6 | 7.63e-6 |

The close-follow output says “Decelerate”; its first three speed-waypoint
segment lengths are approximately 1.99, 1.61 and 1.85 m, below the 2.35 m
constant-speed distance over 0.25 s at the supplied 9.4 m/s. Segment lengths
are not monotonically decreasing. The separate `brake` frame remains a
counterexample rather than a successful braking demonstration.

[verification.json](verification.json) records the checkpoint hash, source
hashes, environment, 258 tokenizer cases, 17 UTF-8 decode cases, operator
fixtures and weight-integrity checks. [trajectories.png](trajectories.png) and
[trajectories.pdf](trajectories.pdf) contain the visual results.

## Selection and interpretation

| Input | Selection | Interpretation |
| --- | --- | --- |
| `straight` | Middle recorded image of `pilot/town01_route_00_normal`, frame 680 | Straight route, a visible lead car, and reduced-speed following language |
| `curve` | Middle available image of `curve_normal_gap26`, frame 299 | Route and speed waypoints bend with the road / navigation targets |
| `brake` | Middle recorded image of `town04_v2_05_brake`, frame 4586 | Retained counterexample: the lead vehicle is stopped in the recording, but both implementations say to accelerate |
| `close_follow` | Same braking recording, frame 4603; selected as the frame with recorded gap nearest 6 m, constrained to gap ≥5 m | Follow-up to inspect the model when the lead vehicle occupies more of the image |

The recording's gap annotation is used only to select the follow-up frame. It
is not supplied to either model. The model receives a single image and cannot
directly observe the lead vehicle's velocity. This is a limitation of the input;
it does not by itself establish the cause of the counterexample.

Keep the counterexample when reusing these results in a chapter. Agreement
with PyTorch establishes that the port reproduces the chosen fp32 reference;
it does not establish that every driving instruction is appropriate. The
generated distances and vehicle colors are model statements, not verified
measurements. The close-follow reference, for example, calls the dark car
“maroon”.

## Replay and figures

From `simlingo-owl/`, after converting the weights and building:

```sh
python tools/run_frame.py --frame demo/straight --weights artifacts/weights \
  --out artifacts/replay-straight
python tools/plot_demo.py demo/straight demo/curve demo/brake demo/close_follow \
  --out artifacts/recreated-figures
```

The retained `trajectories.png` / `trajectories.pdf` show the cropped input,
generated answer and raw coordinate predictions. `results.json` also records
the distances between consecutive speed waypoints. Route and speed heads
have different sampling meanings; their points should not be paired by index.
Timing is elapsed wall-clock time on CPU, including first-use weight loading
and checksum verification. Some runs overlapped, so these timings are examples,
not a benchmark.

The CPU reference uses upstream InternViT and Transformers Qwen2 with eager
attention and merged LoRA in fp32. It consumes Owl's preprocessed pixel tensor.
The comparison therefore isolates the model port from interpolation differences
and does not claim equality with the original mixed-precision deployment.
