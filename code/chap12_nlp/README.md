# Chapter 12: Vector space modelling

All 27 original listings are mapped in `listings.json`; two are textual quotations.
`book_nlp.ml` contains the printed helper functions and `sessions/chapter12.ml`
checks their actual interfaces and outputs. Run in the book image:

```
python3 scripts/run_chapter12.py --output /tmp/chapter12
```

The runner downloads the original 130,000-document news file pinned in `news.json`,
verifies its compressed and decoded SHA256, Git object and byte sizes, builds the
full corpus, and runs all 1,000 native SimpleLDA iterations with eight topics.
This takes about eleven minutes on the validation machine. Network access and
roughly 2 GB of temporary disk space are required. `--news /path/to/news.txt`
reuses a matching local input; `--keep-work` retains corpus products in
`/tmp/chapter12/work` for reader exploration. Otherwise raw news and reversible
corpus indices are cleaned before Docker commits its layer. The public image
contains code, compact execution evidence, metrics and the regenerated figure.

To explore the prepared data interactively, first run with `--keep-work`, start
utop in that work directory, then load the two modules from
`/opt/book/code/chap12_nlp` before the session. The manuscript setup shows these
module loads. Full training uses the native executable; the interactive session
inspects count tables produced by that same training run, rather than repeating
1,000 iterations in the bytecode interpreter.

The current public LDA model is abstract. `book_lda.ml` keeps only the upstream
SimpleLDA algorithm, with read-only copies of document/word count tables; its
source commit/hash and limited adaptation are in `upstream.json`, with the MIT
notice in `OWL-LICENSE.md`. No unsafe representation casts or changes to Owl's
working tree are used. Log counters report real tokens instead of placeholder
zeros. Native checks enforce document/word token conservation and nonnegativity.

Restored code fixes `hi=0.99` for the upper word-frequency quantile, token arrays
rather than line numbers on the stack, non-exported TF-IDF query helpers using
public Count/Idf operations, and normalized sparse/dense negative-cosine ranking.
Empty queries/vectors and k bounds are checked. The dense matrix search selects
maximum similarities and returns their negatives, matching ascending array ranks.
`dirichlet.ml` restores the formerly manuscript-only figure and checks 200 simplex
samples. The first-edition topic words from the separate headline corpus remain
explicitly historical; they are not claimed as results of the pinned news run.
