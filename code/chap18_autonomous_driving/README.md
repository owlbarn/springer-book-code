# Chapter 18: Autonomous Driving

The chapter follows the complete OCaml inference implementation in
[`simlingo-owl/`](../../simlingo-owl/). Four small programs isolate the array
operations used in that implementation:

| Program | What to follow |
| --- | --- |
| `examples/attention.ml` | Scores, row-wise softmax and weighted values |
| `examples/patch-merge.ml` | A 4×4 grid becomes four 2×2 neighbourhood vectors |
| `examples/placeholders.ml` | Image and coordinate rows replace reserved token positions |
| `examples/waypoints.ml` | Displacements accumulate into positions; writes `waypoints.pdf` |

In the book's reference environment, from `examples/`:

```sh
ocamlfind ocamlopt -package owl -linkpkg attention.ml -o attention
./attention
ocamlfind ocamlopt -package owl,owl-plplot -linkpkg waypoints.ml -o waypoints
./waypoints
```

From the manuscript repository, `make examples CHAPTER=18` executes and exports
all four programs through the existing example workflow. Named source regions
select the printed excerpts; the complete programs remain here.

The model uses a separate local extension of the book image. From the companion
repository root:

```sh
docker build -f simlingo-owl/Dockerfile.book -t springer-simlingo:chapter18 .
cd simlingo-owl
python tools/book_run.py --weights artifacts/weights --out artifacts/book-final
```

`book_run.py` needs Pillow on the host, a prepared weights directory (see the
model README), and Docker. It builds the OCaml executable in the container,
executes all four retained scenes to their end-of-answer tokens, validates the
coordinates and records source hashes, the image identity and output hashes.
The output directory must be new. All numerical inference runs in OCaml.

`figures.py` draws the chapter's input, pipeline, tile layout, sequence and
trajectory figures using Matplotlib, NumPy and Pillow. From the manuscript
repository, export these together with exact model excerpts:

```sh
python3 tools/sync_chapter18.py \
  --companion ../springer-book-code \
  --run ../springer-book-code/simlingo-owl/artifacts/book-final
make check
```

Full execution records stay in ignored artifact/evidence directories. The
manuscript tracks compact outputs, figures, source excerpts and their manifest.
Uncommitted sources can be rendered as local previews; commit the companion
inputs and export again before the manuscript's pinned publication check.
