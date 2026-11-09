# Chapter 10: Regression

`sessions/chapter10.ml` restores all 24 active book listings and their data context.
The original `data_reg.csv`, `data_mul.csv` and `boston.csv` are retained and hashed;
no replacement observations are invented. Owl PRNG and OCaml Random seeds are both 80.

`book_regression.ml` contains the book's simultaneous gradient update, correct binary
logistic loss and squared polynomial ridge objective (intercept unpenalised).
Current Owl's generic Cross_entropy only implements -sum(y*log(prediction)), and its
linear regression path does not apply sigmoid. This is insufficient for the binary
formula in the chapter. The explicit Book_regression implementation uses stable
binary loss on logits and the same Owl optimizer. Existing API signatures remain
real #show output, rather than implying the upstream functions were patched.

Similarly, current Regularisation.L2norm is an unsquared Euclidean norm; the book's
ridge helper spells out the squared penalty and tests an analytic coefficient and
constant response. Optimizer Hessians take row vectors, with coefficients reshaped
for the polynomial objective. Known balanced/imbalanced Bernoulli optima verify that
negative labels contribute to logistic training.

The random Gaussian classes overlap: this fixed-seed example's actual training
accuracy is about 89.8%, not the historical 99.1%. Checks compare with a balanced-class
baseline and constant-model binary loss; a separate separable fixture verifies exact
classification. No fixture replaces the book's random example. The displayed
boundary uses the fitted weights, and accuracy reuses the fitted model.

Run `python3 scripts/run_chapter10.py --output /tmp/chapter10` in the book image.
It compiles/runs all five original programs and the analytic checks, then executes
the canonical session and book plots. Subset polynomial curves now fit the actual
subset. Unused exploratory helpers (e.g. fit_exp and sixth-order plots) remain
available but are not claimed as active book validation. Conceptual diagrams remain
as originally supplied; native duplicate images are supplementary artifacts.
