# Appendix A: current conventions and historical Ext

Run `python3 scripts/run_chapterA.py --output /tmp/chapterA` in the book image.
Current mutation, output destination, broadcasting, reduction, precision-generic
APIs and Bigarray dimensionality are actually checked. Four original examples
execute in the interpreter. `listings.json` explicitly blocks `ext-interoperation`:
Owl.Ext was removed by upstream commit
`f882ec0f137be3c0f4910427551c75881efbeccd` on 2023-01-29. The current Owl module
has no Ext or Sparse exports. The book's historical Ext example is preserved and
clearly marked as unavailable in the reader environment. No old compiler or
misleading Owl.Ext compatibility layer is installed. A later author review should
decide whether to retain or replace that historical section.
