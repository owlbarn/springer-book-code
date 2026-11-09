# Chapter 3: statistics

`sessions/chapter03.ml` is the canonical, sequential source of all 17 original
book listings and the histogram/CDF/scatter figures. It initialises both Owl RNGs
with seed 73; outputs are reproducible within the recorded image, not promised
identical across future compiler/library releases. `listings.json` records the
first-edition correspondence. Do not hand-edit the generated book transcripts.

Run `python3 scripts/run_chapter03.py` in the book container. This compiles the
chapter, executes `test_statistics.exe`, produces `gamma_pdf.png` from the
existing complete program, then executes every canonical phrase with explicit
Owl and plotting imports. Errors printed by the OCaml toplevel are failures even
if its process returns zero. All four book figures are regenerated, checked for
file signatures and hashed. Numerical tests use known values and statistical
properties with tolerances, rather than matching one random sequence exactly.

`Stats.var` uses sample normalization n-1; `central_moment 2` uses n. The displayed
two-sample example supplies actual samples, module names and optional arguments,
using Welch's test (`equal_var:false`). The old generic `plots.ml` functions are
retained as historical helpers; their definitions alone are not claimed as
execution coverage. The canonical session supplies the book's real plot calls.

Known upstream risk: Owl's equal-variance unpaired statistic does not use the
pooled-variance expression printed in the book when sample sizes differ. This
chapter's executable example uses the separately checked Welch path. The issue
is recorded in the manuscript review; no silent upstream patch is applied.
