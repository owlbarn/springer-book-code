# Appendix A: current conventions and explicit precision conversion

Run `python3 scripts/run_chapterA.py --output /tmp/chapterA` in the book image.
All five canonical examples execute. Nineteen native checks cover mutation,
output destinations, broadcasting, reductions, four generic precisions, Bigarray
dimensionality, and explicit float32-to-float64 conversion with independent storage.

The author approved removing the obsolete Operator Extension section. Its original
block ID/hash is retained in the map, now linked to `explicit-cast`, showing current
`Dense.Matrix.Generic.cast_s2d` followed by ordinary same-precision arithmetic.
Owl.Ext, removed upstream in 2023, is not reintroduced or emulated. There are no
blocked examples in this appendix.
