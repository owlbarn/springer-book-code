# Chapter 4: linear algebra

`sessions/chapter04.ml` is the sequential source for all 35 book listings and the
three SVD reconstructions. `listings.json` records the original correspondence.
`book_linear.ml` provides the tested one-based LAPACK pivot-vector conversion.
The book uses approximate matrix equality; pivot row exchanges are applied in
forward order. The rank-deficient example uses the pseudoinverse and checks both
a particular solution and its null-space family. Singular LU failure is caught
explicitly as part of the lesson and tested as an expected rejection.

`cat_bw.ppm` is the original manuscript-repository image copied byte-for-byte
from `code/linear-algebra/cat_bw.ppm`; its actual dimensions are 350 by 350.
The old Lena files remain as historical inputs. `svd.ml` now uses the book's cat
input. `imageUtils.ml` preserves RGB pixel order and non-square dimensions,
covered by a binary PPM round-trip test; no inserted separator byte is written
between its PPM header and pixels. The image helper is used by both compiled and
interactive SVD examples.

Run `python3 scripts/run_chapter04.py` in the book container. The runner compiles
the chapter, runs the numerical/image checks, executes the compiled SVD program
and every canonical phrase, verifies full-image SVD reconstruction and decreasing
truncation errors, and generates four PNG snapshots from the actual PPM pixels.
The PNG writer uses Python's standard library; no image conversion service or
external photograph is needed. Logs preserve all toplevel output, even where the
printed book displays only the singular-value vector. The compact printer shows
at most six rows and columns; output formatting is separate from residual tests.

For interactive use, start utop and change to
`/opt/book/code/chap04_linear-algebra` before loading image/helper files. The
book setup includes this command. Random matrix examples use seed 74 in the
recorded environment. Residual checks tolerate rounding; vector signs, basis
choices and repeated-eigenvalue order are not golden output tests.
