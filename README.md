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

The author approved second-edition replacements for the former 18 blocked regions:
Chapter 6 uses native RK45 and ODEPACK; Chapter 7 ships clearly labelled synthetic
CSV inputs with a fixed generator; Appendix A uses explicit precision conversion
in place of Owl.Ext. All 398 original IDs remain traceable (389 code/interface
blocks and 9 illustrative blocks); no blocked region remains in the current map.
The updated local image passes complete build-time and independent reader/PTY
validation. Illustrations are not counted as executed programs.
The exact latest-compiler/Owl environment includes recorded RK45 and blocked
convolution accuracy corrections plus CamlImages build compatibility changes.
All 18 publication snapshots, both code styles and the independent publisher
package pass. Consult the manuscript repository's `STATUS.md` for the immutable
image, exact source evidence and next review steps.

CI publishes verified `matrixanger/springer-book-code:pilot` tags after the
author commits and pushes. This local revision is not yet committed or published;
no `:latest` promotion was performed. Reproduce an edition using its recorded digest.
Local candidate tags are not Docker Hub release tags.
