# Chapter 6: ODEs with native solvers and ODEPACK

`listings.json` retains all 26 original block IDs and their first-edition hashes.
The second edition replaces the former custom-CVODE example with configured
native RK45 (`tol=1e-7`, `dtmax=0.1`), and the non-stiff Van der Pol solve/plot
with native RK45 (`tol=1e-8`, `dtmax=0.05`). Equations, initial conditions and the
30-unit non-stiff interval are preserved. The stiff case remains ODEPACK LSODA
with mu=1000 and duration 3000. SUNDIALS is background, not a book dependency.

Run `python3 scripts/run_chapter06.py`. All 26 canonical regions execute and eight
book plots are generated. Thirty native checks cover convergence, endpoints,
analytic linear/oscillator solutions, configured RK45, damping and LSODA decay.
The new Van der Pol trajectory is compared with an independent scalar RK4 loop
at step 0.001, including both final state components, dimensions and finiteness.

`book_ode.ml` contains the corrected Euler/midpoint teaching functions.
Lorenz perturbations use a common fixed RK4 time grid. The environment records
OwlDE's upstream commit plus `docker/patches/owl-ode-rk45-step.patch`; path pins
ensure the actual patched working tree is installed. The patch separates the
current step size from the proposed next step and remains guarded by analytic
accuracy checks. RK45's `tol` is not presented as CVODE's separate relative and
absolute tolerances.

ODEPACK 0.7.1 is compiled with Ubuntu gfortran. The unsupported SUNDIALS OCaml
binding is not installed, its safety constraints are preserved, and OCaml is not
downgraded. No historical CVODE results are used as second-edition output.
