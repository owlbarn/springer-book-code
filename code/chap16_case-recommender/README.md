# Chapter 16: original random projection and RP-tree excerpts

Run `python3 scripts/run_chapter16.py --output /tmp/chapter16` inside the book image.
The sparse and dense projection implementations occupy separate modules, since
the original printed functions reuse the same names with different representations.
The interpreter uses the same compiled library after the native property checks.

All eleven original OCaml excerpts execute. Deterministic numerical and small text
fixtures test sparse versus dense equality, reproducible seeds, actual TFIDF input,
coverage and routing, duplicate coordinates, empty/singleton/zero-depth cases and
finite median values. They are verification fixtures, not a substitute for the
historical Wikipedia/news corpus or published Sofia benchmarks.

`projection_depth.ml` handles a corpus smaller than the requested leaf size without
negative depths. For zero levels call `Rp_tree.grow ~documents:count projected` so
all document identifiers can be retained when there are no projected coordinates.
Tree recursion stops at a singleton. Equal projection coordinates are split by
rank and can occur on either side of a threshold; query routing is approximate
and need not return a particular identifier among coincident points. Every point
is still stored once. `search_leaves` catches only its own local found exception
and returns a copy. `Pervasives.compare` becomes `Stdlib.compare`. Half-sums avoid
overflow in the median of large finite values.

The Gaussian directions are not unit-normalised; stored values are dot-product
coordinates. Positive rescaling of a level's direction preserves its split order
and threshold routing. Unit directions are needed if interpreting them as literal
Euclidean projected lengths.

The REST request/JSON response in the chapter is schematic, not a running public
endpoint. This reconstruction does not claim to recreate the original Chrome,
Spark, web crawler or distributed backend. PANNS is the cited Python reference
(<https://github.com/ryanrhymes/panns>); the original book's OCaml excerpts are the
source restored here. Historical architecture and benchmark figures remain as
explanatory first-edition figures, not newly measured outputs.
