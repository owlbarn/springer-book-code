# Chapter 8: Algorithmic differentiation

`sessions/chapter08.ml` restores all 53 active first-edition listings in order.
Commented historical blocks are excluded. It contains forward, reverse and combined
teaching implementations, current Owl API calls, Newton iteration, the intentional
perturbation-confusion counterexample and the custom/lazy operator builders.

`listings.json` records original block identities. The extra session checks exercise
shared intermediate nodes, repeated differentiation and analytic derivatives. Reverse
propagation forwards each incoming increment rather than an already accumulated
adjoint; each differentiation resets the reachable graph. The omitted Unary/Binary
signatures and remaining logistic operators are supplied explicitly. `Book_naive`
captures the earlier untagged type/functions before opening Owl's tagged AD module;
the counterexample intentionally returns 4, while Owl's tagged version returns 1.

Forward tangents for arrays have the input shape. `Stdlib.Lazy.force` avoids Owl's
Lazy module shadowing the standard lazy-value API. Newton has a finite iteration
limit and rejects non-finite gradients; its gradient stopping criterion finds a
stationary point, not necessarily a minimum.

Run `python3 scripts/run_chapter08.py --output /tmp/chapter08` in the book image.
It compiles and runs the native analytic checks, actually calls the existing
higher-order derivative plot, and executes the complete marked teaching/API session.
The extra derivative plot remains a verification artifact because the original book
commented that figure out; the chapter's conceptual diagrams remain unchanged.
