# Book pilot container

The Dockerfile and scripts live in the companion code repository. This first
image validates Chapter 2 only; ODE, model weights and the later chapters have
not yet been incorporated. Do not promote this pilot to `latest` as a fully
validated book environment.

Build locally from the repository root:

```sh
python3 docker/build.py
```

The script resolves the latest non-prerelease OCaml GitHub release, Owl main and
owl-plplot main before building. Ubuntu 24.04 is the single distribution. It
refreshes the opam repository instead of relying on the base image's frozen
package snapshot. A future OCaml series requires the corresponding official
Ubuntu base tag to exist; failure is reported, never silently downgraded.

The default local tag is `matrixanger/springer-book-code:pilot`. Docker Hub is
not required for local builds. The Dockerfile records the actual compiler,
packages, source commits and chapter-source hashes in `/opt/book/environment.json`.
The final build step runs the chapter's numeric and plot checks.

Pull the published pilot and enter the environment:

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

To run the checks again inside the image:

```sh
docker run --rm matrixanger/springer-book-code:pilot \
  python3 scripts/run_chapter02.py --output /tmp/chapter02-results
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
executes the reader's command, including canonical REPL inputs. It also starts
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
