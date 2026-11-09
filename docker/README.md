# Book pilot container

The Dockerfile and scripts live in the companion code repository. The validation
manifest covers Chapters 1–16 and Appendices A/B. Source mappings cover all 398
original blocks; execution still explicitly excludes three SUNDIALS regions,
fourteen missing-original-CSV regions and the removed historical Owl.Ext example.
Illustrative protocol/shape/quotation/weight-exchange blocks are labelled separately.
The local whole-book candidate passes declared-scope build and fresh reader/PTY
validation. All book snapshots, both layouts and the independent publisher package
pass. The 18 gaps remain: do not describe the book as fully executed or promote
`latest`. The last public pilot still contains Chapter 2 only.

Build and validate a local candidate from the repository root:

```sh
BOOK_IMAGE=matrixanger/springer-book-code:candidate
python3 docker/build.py --tag "$BOOK_IMAGE" --output /tmp/book-build.json
BOOK_IMAGE_ID=$(docker image inspect --format '{{.Id}}' "$BOOK_IMAGE")
python3 docker/validate.py --image "$BOOK_IMAGE_ID" \
  --resolved /tmp/book-build.json --output /tmp/book-validation.json
```

The script resolves the latest non-prerelease OCaml GitHub release, Owl main and
owl-plplot main before building. Ubuntu 24.04 is the single distribution. It
refreshes the opam repository instead of relying on the base image's frozen
package snapshot. A future OCaml series requires the corresponding official
Ubuntu base tag to exist; failure is reported, never silently downgraded.

The default local tag is `matrixanger/springer-book-code:candidate`. Docker Hub is
not required for local builds. The Dockerfile records the actual compiler,
packages, source commits and chapter-source hashes in `/opt/book/environment.json`.
The final build step runs every restored chapter in `docker/chapters.json`.
Chapter 2 results remain in `/opt/book/verification`; subsequent chapters use
its `chapterNN`, `chapterA` and `chapterB` subdirectories. Per-chapter runners are also available.
Declared blocked regions are shown separately from successful execution; an
unexpected skipped region fails validation.

Pull the published **Chapter 2 pilot** and enter its environment:

```sh
docker pull matrixanger/springer-book-code:pilot
docker run --rm -it matrixanger/springer-book-code:pilot utop
```

To inspect the results generated during the build, copy them from a stopped
container (no Docker Hub upload is needed):

```sh
mkdir -p results/chapter02
docker create --name springer-book-results matrixanger/springer-book-code:pilot true
docker cp springer-book-results:/opt/book/verification/. results/chapter02/
docker rm springer-book-results
```

To run the complete declared checks again in your local candidate (the public pilot has only its published subset):

```sh
docker run --rm "$BOOK_IMAGE" \
  python3 scripts/run_book.py --output /tmp/book-results
```

## Automatic builds and publication

`.github/workflows/book-image.yml` checks pull requests, main-branch pushes,
manual runs and `owl-updated` repository notifications. A separate nightly
workflow calls the same pipeline daily at 04:23 UTC to refresh compiler, Owl,
plotting and base-image inputs. The nightly workflow is separate so its possible
inactivity suspension does not disable push/manual/upstream-triggered builds.
GitHub may disable scheduled workflows after 60 days without repository activity;
check Actions periodically and re-enable the nightly workflow when required.

Resolution selects the latest stable OCaml release, exact Owl/owl-plplot main
commits and a registry digest for the Ubuntu OCaml base. Missing future base tags
or incompatible dependencies fail visibly. The build's environment record stores
those inputs plus the source revision, dirty state and source hashes. Metadata
changes are placed after dependency installation to reuse cached compiler builds.
Owl uses explicit generic x86-64 C flags rather than `-march=native`, so the
distributed image does not require the build runner's optional CPU extensions.
The actual flags are recorded and checked before publication.

PRs build/test only. Publications from main require the repository Actions secret
`DOCKERHUB_TOKEN`, a matrixanger personal token with **Read & Write** repository
permission. The username is fixed to `matrixanger`; no username secret is needed.
Tokens are passed only to the login action, not Docker build arguments.

The pipeline first loads the image into Docker, validates the exact image ID and
executes the reader's command, including canonical REPL inputs for all restored chapters and the declared
partial Chapter 6/7 and Appendix A scope. It also starts
`utop` through a real terminal and checks the default Owl/plot imports and matrix
arithmetic. Keep `-it` for interactive use: utop's nonterminal fallback follows a
different initialization path and is not used to validate the interactive entry.
The canonical automated sessions use `ocaml -noinit` with explicit imports. It pushes a unique
`pilot-<code revision>-<run ID>` tag, pulls that registry digest and validates it.
Only then does it tag and push the same image as `:pilot`. Publication jobs are
serialised in this repository. Failed builds/checks preserve the last passing
pilot. This workflow never publishes `latest`, which remains reserved for full
book coverage. Downloaded-image identity and registry digest are recorded as CI
artifacts; no rebuild occurs between testing and pushing.

Local equivalents (after `docker login --username matrixanger`):

```sh
python3 docker/build.py --resolve-only --output /tmp/book-build.json
python3 docker/build.py --resolved /tmp/book-build.json --tag matrixanger/springer-book-code:candidate
python3 docker/validate.py --image matrixanger/springer-book-code:candidate \
  --resolved /tmp/book-build.json --output /tmp/book-validation.json
python3 docker/publish.py --image matrixanger/springer-book-code:candidate \
  --resolved /tmp/book-build.json --output /tmp/book-published.json
```

## Immediate Owl notifications

Immediate notifications are deferred; the daily workflow is the active update
mechanism. A draft `.github/workflows/notify-book.yml` in the local Owl checkout
can send a notification to this repository on main-branch pushes once enabled. It needs `BOOK_BUILD_DISPATCH_TOKEN` configured **in the Owl
repository**: a fine-grained GitHub token for resource owner `owlbarn`, limited to
`springer-book-code`, with Contents read/write (required by repository_dispatch).
No Docker credentials belong in the notification. Without that secret, the hook
emits a notice and skips; the companion nightly check is independent.

Notifications trigger a fresh resolution of current main. Their payload SHA is
recorded by GitHub but is not executed as an arbitrary checkout, so a delayed
notification does not intentionally rebuild and promote an older source revision.
Commit and push both workflow files before configuring/testing the notification.

## ODE backend boundary and recorded accuracy patch

The image tracks an exact `owlbarn/owl_ode` master commit alongside Owl and
owl-plplot. Ubuntu gfortran supports ODEPACK. The SUNDIALS binding is deliberately
not installed: sundialsml 6.1.1p1 declares a base-domains conflict, and upstream
master NEWS records OCaml 5 examples segfaulting. No compiler downgrade or removal
of that protection is used. The native/ODEPACK runners explicitly report the
three blocked regions; they do not claim full Chapter 6 validation.

`docker/patches/owl-ode-rk45-step.patch` corrects a reproduced RK45 step-size error:
the current state/time must advance with the size used for the stages, keeping
the proposed next step separate. The compiler/commit record additionally stores
the patch and upstream source-diff hashes. Path pins ensure opam installs the
patched working tree, rather than the unmodified local Git commit. Numerical
regression checks cover the analytic problem that failed before this fix. If
upstream changes make the patch stop applying, builds fail visibly until the
patch is reviewed; this is not advertised as an unmodified OwlDE installation.

## Pinned MNIST data

The image prepares only the six existing MNIST files used in Chapter 11, pinned
to an Owl dataset commit and checked before decompression/use. The 222.6 MB cache
is built in a separate layer, so ordinary code edits reuse it. Attribution and
the publisher's license declaration accompany `mnist.json` and MNIST-NOTICE.md
in `/opt/book-data/`. No all-datasets downloader or arbitrary new model weights
is run. Dataset hashes are recorded in the environment and per-chapter results;
missing or stale dataset evidence blocks publication. The reader run is offline.

Compiled Dune artifacts are removed after successful build-time verification.
The image retains source and compact verification outputs; each reader check
compiles the selected programs again. This keeps repeated source updates from
storing a full set of generated executables in each image layer.


Chapter 12 validates all 130,000 original news documents and 1,000 native LDA
iterations. That reader validation requires network access to download the pinned
96.9 MB gzip; source objects, compressed/decoded hashes and byte counts are checked.
The news and reversible corpus products live in a temporary directory cleaned
before the image layer is committed. `--keep-work` is a reader opt-in for exploration,
never used in the build. The compact verification receipt records the input hashes.
Other chapters continue to work from the installed data; image inspection and PTY
startup checks are offline. Allow roughly 2 GB temporary disk and eleven minutes
per Chapter 12 execution; CI permits 180 minutes for build and registry revalidation.
All later chapters are included in the declared scope; complete execution still
awaits the 18 explicitly reported Chapter 6/7 and Appendix A gaps.


Chapter 13 includes a pinned portable Inception cache. A separate `legacy-reader`
build stage compiles OCaml 4.08.1 solely to read the old Bigarray Marshal format;
`legacy-model` exports standard NPY and rejects any output hash drift. The final
`book-tools`/`book` stages use the independently resolved latest OCaml/Owl switch,
ImageMagick and copied portable data; no old compiler or original Marshal weight
file is copied in. The tool layer allows rapid chapter probes before the final
whole-scope build. Three incomplete original weight-exchange excerpts are declared
illustrative in the chapter manifest and validator; actual pretrained inference,
all loaded parameters, modern save/reload and image processing are executed.


Chapter 14 restores the complete Mask R-CNN constructor and runs the book's actual
credited London input at 1024. The separate mrcnn-model stage converts the author's
fixed 490 MB HDF5 exchange file to 690 exact float32 NPY arrays and verifies all
hashes. The final image includes only that portable cache; h5py/NumPy and the
exchange file remain build-only. CamlImages5.0.5 is pinned with the recorded Dune
C-flags patch and its original Base/StdIO compatibility bounds; actual patched
source hashes and model/photo inputs are checked. The target compiler is unchanged.


Chapter 15 uses an isolated `legacy-style-model` build stage to convert the original
VGG19 and all six FST weight files losslessly to NPY. No historical compiler or
Marshal weights enter the reader image. The chapter really executes 1250 Adam
updates across the combined NST and ten reconstructions, all six FST models and
two original 1500×1500 Budapest renders. Allow minutes of CPU computation; Chapter
12 also downloads a fixed full news corpus and executes all 1000 LDA iterations.
About 2 GB of additional transient disk is needed for the corpus, beyond compiler
and model build space. Repeated obsolete local image candidates can consume space;
keep the last passing image and remove only your own superseded build artifacts.

The Owl main source is pinned explicitly as a path after applying the recorded
five-line blocked-convolution correction. Exact uniform/nonuniform full-tensor
forward/input-gradient/kernel-gradient checks and a real 224-pixel VGG directional
gradient check guard it. The image records upstream SHA, patch SHA and patched-file
SHA; publication rejects missing or changed accuracy corrections. Inception and
Mask R-CNN are checked again under the corrected environment.

Appendix B requires `owl-plplot` and invokes/decodes all 22 original plots in native
and interpreter runs. Appendix A deliberately does not reinstall the removed
`Owl.Ext` API. Chapter 16 verifies the original OCaml projection/tree code, not the
historical external Spark/Chrome service or published corpus-scale benchmarks.

CI allows 180 minutes for the expanded workload. After an exact immutable image
has completed the reader and PTY checks, `publish.py --validation /path/to/record.json`
can reuse that pre-push result only if the image ID, complete baked source/environment
and scope evidence all match. Incomplete runs and stale/mutable identities are
rejected. Standalone publication without that argument performs its own full check;
registry-pulled content always receives another actual reader/PTY execution before
promotion. This avoids a duplicate pre-push long training run, not the registry check.
