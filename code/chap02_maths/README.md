# Chapter 2 numerical algorithms pilot

The full source and the 19 original listings are mapped in `listings.json`.
`book_algorithms.ml` contains the teaching algorithms; it is deliberately separate
from Owl's implementation. `book_gaussian.ml` uses Owl to generate Legendre nodes
and weights. The existing four plot programs are retained and now have entry points.

From the repository root, in the book container:

```sh
python3 scripts/run_chapter02.py --output /tmp/chapter02-results
```

The runner builds this chapter only, executes 56 teaching-algorithm checks,
checks Gaussian polynomial exactness and the sine-integral/beta identities, then
runs all four plot programs. Eight expected outputs must be newly generated and
have PNG/PDF signatures. `execution.json` stores commands and diagnostics;
`results.json` stores versions, source hashes and output hashes. Visual review
is separate. The runner sets Qt's offscreen platform for headless PNG output.

## Corrections to the original examples

- Rational interpolation indexed beyond the end of the input arrays and omitted
  the final corrections. The restored implementation tests both nodes and
  intermediate points, including a four-point rational function.
- The trapezoid helper sampled beyond the interval and used incorrect weights.
  For example, its stage-2 integral of the constant 1 on [0,1] was 1.5. The
  corrected composite rule returns 1 and never samples outside the interval.
- Simpson's method must update each preceding trapezoid and return the
  extrapolated estimate, not the most recent trapezoid.
- Romberg extrapolation includes the current refinement. Integrand exceptions
  propagate; they are not treated as successful convergence.
- `eps` for trapezoid/Simpson convergence and Romberg correction uses a scale
  floor of 1 for near-zero results. Reaching the stage limit returns the latest
  estimate; this is not a guarantee of accuracy for arbitrary integrands.
- Chebyshev sampling uses eleven indices and divides by eleven. The previous
  `linspace 1. 10. 11` construction was not the eleven stated Chebyshev roots.
- The factorial example binds its input/output arrays in usable scope and
  converts abscissas to floats. Beta symmetry uses a tolerance, not OCaml's
  physical equality operator. The sinc integrand is defined at zero.

The manuscript now imports generated snapshots of these implementations and
actual OCaml transcripts. Regenerate them from the manuscript repository with
`python3 tools/sync_chapter02.py --companion ../springer-book-code`; use `--check`
to check snapshots, adding `--companion ../springer-book-code` to detect changed
sources. The source mapping records the original listing hashes and exported paths. The original `plot2`
rational diagnostic in `interpolation.ml` is historical extra code, not a book
figure, and is not called by the pilot. The restored rational implementation is
tested separately; running that old diagnostic is not claimed as passing.

Known upstream issue: the inspected Owl main source contains the original
rational/trapezoid/Simpson routines too. Passing the teaching checks does not
prove those Owl routines correct. The printed integration example explicitly
uses `Book_algorithms.trapz`; upstream fixes require a separate change.
