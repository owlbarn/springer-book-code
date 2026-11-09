# Chapter 9: Optimisation

`sessions/chapter09.ml` restores all 13 active book listings, including seven
Newton root iterates and the ten real optimizer log entries. The canonical runner
actually calls the hump and Rosenbrock trajectory plots. No synthetic or historical
output is substituted. The learning rate is explicitly 0.001 for the ten-step Owl
example; the initial Rosenbrock value is 2026 at (2,-0.5).

The session checks roots, stationary derivatives and curvature signs, the Rosenbrock
minimum/trajectory, finite decreasing optimizer output, Newton's analytic quadratic
case, gradient termination and iteration limit. A small Newton gradient establishes
a stationary point, not necessarily a minimum; the cosine example can reach a maximum.

The existing native Rosenbrock and Himmelblau programs are compiled and run too.
Himmelblau's previously missing first square is restored in both AD and plot formulas;
each native program checks a known minimum/gradient, trajectory shape and lower loss.
Their PDF plots remain supplementary validation artifacts. The manuscript's two
program figures come from the marked session; other conceptual diagrams stay as supplied.

Run `python3 scripts/run_chapter09.py --output /tmp/chapter09` in the book image.
