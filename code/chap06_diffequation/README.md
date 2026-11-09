# Chapter 6: ODEs (partial coverage on OCaml 5)

`listings.json` maps all 26 original book blocks. The full sequential inputs are
in `sessions/chapter06.ml`. `book_ode.ml` contains the corrected Euler/midpoint
teaching functions, with final-step clamping and true midpoint updates. The
compiled midpoint/Lorenz programs are also executed. Lorenz's state has three
components; the perturbation comparison uses a common fixed RK4 time grid rather
than comparing unrelated adaptive sample indices.

Run `python3 scripts/run_chapter06.py` in the book container. Nineteen independent
analytic/property checks cover Euler/midpoint convergence, non-integral endpoints,
RK4 linear systems and oscillators, adaptive RK45 accuracy, damping, and LSODA
against exponential decay. Seven book plots are generated. SUNDIALS blocks cvode,
vdp-cvode and vdp-cvode-plot are restored as source but explicitly NOT RUN.
Their original manuscript figure remains historical and unverified.

The current official sundialsml package conflicts with base-domains. Upstream
master 27ae63cf9e6e45e4b820232d99366751e30a6b9f also records OCaml 5 example
segfaults in NEWS.md. This environment does not remove that protection or downgrade
OCaml. This chapter must not be represented as fully validated.

The environment uses pinned OwlDE master plus the explicitly recorded patch
`docker/patches/owl-ode-rk45-step.patch`. Upstream RK45 overwrote the current step
size before advancing the state/time; keep the proposed next step separate.
The original y'=2*t*y+t example returned 0.859078699 at t=1, versus analytic
0.859140914, even with tol=1e-9. The corrected solver passes the analytic check.
The Dockerfile uses path pins so opam installs the patched working tree rather
than its unmodified Git commit. Both upstream SHA and patch/diff hashes are
recorded; this is not described as an unmodified upstream installation.

ODEPACK 0.7.1 is compiled with Ubuntu gfortran; no SUNDIALS/MPI/PETSc development
stack is included in the reference image. Test executables use Dune's native
executable mode, avoiding an unused custom bytecode runtime link target.
