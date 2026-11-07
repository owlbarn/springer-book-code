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

Enter the environment:

```sh
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

The pilot is not pushed by the build script. Publishing and automatic update
workflows follow after the validation/promotion rules are implemented.
