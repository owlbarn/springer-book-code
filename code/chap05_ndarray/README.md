# Chapter 5: N-dimensional arrays

`sessions/chapter05.ml` is the canonical sequential source for the chapter's 30
executable/interface blocks. `listings.json` additionally retains the two
non-executable broadcast-shape illustrations (32 original display blocks total);
these illustrations are not claimed as executed OCaml. The validity of their
concrete shapes is checked separately by numerical tests.

`book_ndarray.ml` is the source of the displayed fold, reverse, rotate and
circular-shift functions. Shift counts are normalized, including zero, negative
counts and whole rotations. The slicing examples restore the missing 8 by 8
input context; sequential shapes use OCaml arrays, not lists. The generic ndarray
and index type displays come from actual `#show_type` commands.

Run `python3 scripts/run_chapter05.py` in the book container. It compiles and runs
38 property checks, then executes all canonical regions (plus a contraction
assertion), failing on printed toplevel errors even when OCaml exits zero.
Coverage includes mapping without mutation, masked thresholding, fold, tile vs
repeat, basic/fancy/negative indexing, rotation and shift identities, broadcast
vs explicit tiling, dimensional expansion, matrix dot vs contraction, multi-axis
contraction vs the book's explicit loops, and trace contraction. Static conceptual
diagrams are retained from the manuscript; this chapter generates no book plot.
