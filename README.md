# springer-book-code
The code included in the Springer Owl book.

## Second edition environment

- [Build, run and validate the Ubuntu book container](docker/README.md)
- All 398 original display blocks now have source mappings across Chapters
  [1](code/chap01_introduction/README.md), [2](code/chap02_maths/README.md),
  [3](code/chap03_stats/README.md), [4](code/chap04_linear-algebra/README.md),
  [5](code/chap05_ndarray/README.md), [6](code/chap06_diffequation/README.md),
  [7](code/chap07_signal/README.md), [8](code/chap08_algodiff/README.md),
  [9](code/chap09_optimisation/README.md), [10](code/chap10_regression/README.md),
  [11](code/chap11_neural_network/README.md), [12](code/chap12_nlp/README.md),
  [13](code/chap13_case-image-inception/README.md), [14](code/chap14_case-obj-detect/README.md),
  [15](code/chap15_case-nst/README.md), [16](code/chap16_case-recommender/README.md),
  [A](code/chapA_conventions/README.md) and [B](code/chapB_visualisation/README.md).

Restoration includes the original datasets/model inputs where recovered, native
runners and actual OCaml sessions. Chapter 12 performs all 1000 LDA iterations;
Chapter 15 performs 250 NST and ten 100-update reconstruction runs, and verifies
all six FST models. Some printed shapes, quotations, protocol and incomplete
weight-exchange fragments are explicitly illustrative rather than executable.

There are **18 explicitly unexecuted regions**: three Chapter 6 SUNDIALS examples
incompatible with the current compiler, fourteen Chapter 7 regions depending on
missing original CSV files, and Appendix A's removed historical Owl.Ext API.
`scripts/run_book.py` and image validation report these gaps. Complete source
mapping and a passing declared scope do not mean complete book execution.
The exact latest-compiler/Owl environment includes recorded RK45 and blocked
convolution accuracy corrections plus CamlImages build compatibility changes.
The local whole-book candidate passes build-time and fresh reader/PTY checks for
its declared executable scope. All 398 blocks are exported; both code styles and
the independent publisher source package compile. Consult the manuscript
repository's `STATUS.md` for the immutable image and remaining author-input boundary.

The last published `matrixanger/springer-book-code:pilot` contains Chapter 2 only.
The newer local work is uncommitted and unpublished. `:latest` remains reserved
for complete book checks; reproduce an edition using its recorded image digest.
Local candidate tags are not Docker Hub release tags.
