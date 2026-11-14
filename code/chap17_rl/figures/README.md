# Fixed case-study illustrations

These are editorial figures, separate from the automatically exported short examples.
The manuscript uses `book/figures/rl/`. Each case directory records CSV returns,
selected original simulator frames, the exact source hash, seed, training budget,
immutable local image ID and evaluation procedure in `provenance.json`.
Full logs and trained weights remain in the manuscript's ignored `.book-evidence/rl/`.

`plot_lunar.py` rebuilds the LunarLander figures using Python, NumPy, Matplotlib
and Pillow. Run from this directory:

```sh
python3 plot_lunar.py lunar-lander /path/to/springer/book/figures/rl
python3 plot_doom.py vizdoom-dqn /path/to/springer/book/figures/rl
```

The experiment used an unpublished local image built from the book environment.
`preview.Dockerfile` documents that build; its tar input is the owl-rllib checkout
at the recorded base commit plus `owl-rllib-frame-export.patch`, excluding `.git`
and `_build`. The first LunarLander run used only the Gym renderer part of the
patch; its immutable image ID is retained separately. These are preview inputs,
not a claim that the public reader image already contains the renderer changes.

The author should commit the owl-rllib extension first, then the companion
examples and existing runner changes together, and rebuild the reference image
through the normal validation workflow. Re-export manuscript snapshots from the
committed source before running the pinned-source publication gate. Do not repeat
unchanged full case training merely to regenerate a PDF or check a short example.

The final short-example image is
`sha256:b7ba7089e475554ff5d98dfc77ca773fe27bf720b5206626f1c96f1a71db22c2`.
It corrects inherited environment metadata only: the installed runtime is identical
to the image used for the accepted Doom run. The earlier experiment images recorded
the actual local owl-rllib source identity in `sources.owl-rllib`; their inherited
`rllib.commit` still referred to the parent image. The final preview puts the actual
base commit and dirty source-tree digest together under `rllib`, and the book
exporter retains this block, including Python dependency versions.

Fig. 12.11 now uses real RGB24 renderings. The policy still observes GRAY8.
`vizdoom-dqn/record_actions.ml` reconstructs the initial network and reloads the
saved checkpoint, recording only the first evaluation episode for each.
`replay_colour.py` replays those actions with seed 1000 in GRAY8 and RGB24,
checks every reward and episode boundary, and verifies that the replayed gray
frames equal the original figure inputs. `run-colour.sh` gives the container
commands (mount the working files at `/results` and saved weights at `/checkpoint`).
The recorded action CSVs also allow replay without loading the model.
Run `python3 plot_doom.py vizdoom-dqn /path/to/springer/book/figures/rl --frames-only`
to rebuild just the colour panel. No training or learning-curve regeneration is needed.

Fig. 12.9 uses the same crop in all four LunarLander views, with equal-size
100-by-100-pixel detail windows around the two final lander positions. The raw
frames and training curve are unchanged. Rebuild only the image panel with:

```sh
python3 plot_lunar.py lunar-lander /path/to/springer/book/figures/rl --frames-only
```

The `source_sha256` in each case provenance remains the source of the original
full run. `current_example` separately records later excerpt/timing edits and the
short-run checks; it does not relabel the historical full run as a fresh run.
Supplementary token, baseline and reference-implementation results are recorded
in [the historical experiment notes](../notes/historical-experiments.md).
