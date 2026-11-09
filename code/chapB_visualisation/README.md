# Appendix B: visualisation

Run `python3 scripts/run_chapterB.py --output /tmp/chapterB` in the book image.
The separate `owl-plplot` package must be required and `Owl_plplot` opened alongside
Owl. All 24 original blocks are covered: two real API/type inspections and 22
actual native and interpreter plotting examples, producing all 22 original figures.
The first example now calls `Plot.output h` to close and write its handle. The
uniform sample's autocorrelation is labelled Uniform to match its actual input.
Source formulae and plotting data otherwise retain the original examples.
Every PNG is decoded, dimensions checked and nonblank before export. This is not
an automatic judgement of a figure's scientific interpretation; generated output
is visually inspected in the manuscript workflow. The book's screen/print code
style is switchable independently of these colour plotting examples.
