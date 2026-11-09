# Chapter 11: Neural networks

All 32 active first-edition listings are restored in `sessions/chapter11.ml`.
The Dune rule generates a native executable from that same source, removing only
package/printer/introspection directives. It does not keep another handwritten
copy. Both native and interactive programs execute the full stated training loops.

The six original Owl-format MNIST files are fixed to commit
`2dc4d388f7d0f0cec1597a06f2782b89a6814c8d` in `mnist.json`: Git objects,
compressed/decoded SHA256 and byte sizes are verified. `MNIST-NOTICE.md` preserves
attribution and the dataset publisher's MIT declaration. The image includes this
222.6 MB cache (18 MB download) in its own reusable layer. To prepare/check it
outside the image, run `python3 scripts/prepare_mnist.py [--check]`.

Run `python3 scripts/run_chapter11.py --output /tmp/chapter11` in the book image.
The runner rejects missing/changed data and compares native/interpreter metrics.
It performs 999 handwritten SGD batches of 100, tests on all 10,000 test images,
and runs the Owl Graph 0.1 epoch (60 batches), again testing all 10,000 images.
CNN, embedding/LSTM and GAN constructors are initialized and actually run for
shape, finite-value and probability/range checks; these are forward checks, not
claims of full RNN/GAN training. A finite-difference fixture checks backpropagation.

Recovered context includes packed reverse-mode weights, float32/float64 conversion,
flat versus [batch;28;28;1] shapes, pixel normalization, actual one-hot test labels,
and the optimizer excerpts' batch/forward/params bindings. Neuron and graph types
come from real #show_type output; no ellipsis is compiled. Public neuron helper
snippets are executed and checked alongside the current graph interface.

The two book figures are regenerated: activation curves and a real first-test-image
prediction. Plot.image resets the page size and expects pixel intensities; the
example restores the canvas after that call and supplies 0–255 values. Probability
stems use true classes 0–9. Training excerpts require all 999 finite log lines before
showing the first eight; full logs remain available. The original 90.14%-for-digit-3
screenshot is not presented as a newly reproduced result.
