# syntax=docker/dockerfile:1
# Book pilot environment. Resolve exact versions with docker/build.py.
ARG BASE_IMAGE=ocaml/opam:ubuntu-24.04-ocaml-5.5
# This isolated tool reads the author's 2019 Marshal data once and emits NPY.
# Neither the historical compiler nor old Marshal weights enter the book image.
FROM ${BASE_IMAGE} AS legacy-reader
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    build-essential python3 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY docker/legacy/build_reader.py docker/legacy/ocaml.json /opt/legacy/
RUN python3 /opt/legacy/build_reader.py

FROM legacy-reader AS legacy-model
COPY docker/legacy/export_weights.ml /opt/book/docker/legacy/export_weights.ml
COPY scripts/prepare_inception.py /opt/book/scripts/prepare_inception.py
COPY code/chap13_case-image-inception/inputs.json code/chap13_case-image-inception/portable-manifest.json /opt/book/code/chap13_case-image-inception/
RUN python3 /opt/book/scripts/prepare_inception.py --directory /opt/inception-portable --legacy-source /opt/legacy/ocaml-4.08.1

FROM legacy-reader AS legacy-style-model
COPY docker/legacy/export_weights.ml /opt/book/docker/legacy/export_weights.ml
COPY scripts/prepare_style.py /opt/book/scripts/prepare_style.py
COPY code/chap15_case-nst/portable-manifest.json /opt/book/code/chap15_case-nst/portable-manifest.json
RUN python3 /opt/book/scripts/prepare_style.py --directory /opt/style-portable --legacy-source /opt/legacy/ocaml-4.08.1

# Convert the author's original HDF5 arrays without installing Python model tools
# or retaining the large exchange file in the reader environment.
FROM ${BASE_IMAGE} AS mrcnn-model
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    python3-h5py python3-numpy ca-certificates && rm -rf /var/lib/apt/lists/*
COPY scripts/prepare_mrcnn.py /opt/book/scripts/prepare_mrcnn.py
COPY code/chap14_case-obj-detect/portable-manifest.json /opt/book/code/chap14_case-obj-detect/portable-manifest.json
RUN python3 /opt/book/scripts/prepare_mrcnn.py --prepare --directory /opt/mrcnn-portable

FROM ${BASE_IMAGE} AS book-tools
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    build-essential ca-certificates curl git m4 pkg-config python3 unzip wget \
    libopenblas-dev liblapacke-dev libplplot-dev plplot-driver-cairo \
    fonts-dejavu-core zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /opt/book-sources /opt/book && chown -R opam:opam /opt/book-sources /opt/book
USER opam
ARG OCAML_VERSION
RUN test -n "$OCAML_VERSION"
RUN opam repository set-url default https://opam.ocaml.org && opam update && opam switch create book "ocaml-base-compiler.${OCAML_VERSION}" --yes --jobs=2
ARG OWL_REF
ARG OWL_PLPLOT_REF
RUN test -n "$OWL_REF" && test -n "$OWL_PLPLOT_REF"
RUN git clone https://github.com/owlbarn/owl.git /opt/book-sources/owl \
    && git -C /opt/book-sources/owl checkout "$OWL_REF" \
    && git clone https://github.com/owlbarn/owl-plplot.git /opt/book-sources/owl-plplot \
    && git -C /opt/book-sources/owl-plplot checkout "$OWL_PLPLOT_REF"
COPY --chown=opam:opam docker/patches/owl-conv-block-offset.patch /tmp/owl-conv-block-offset.patch
RUN git -C /opt/book-sources/owl apply /tmp/owl-conv-block-offset.patch
ENV OWL_CFLAGS="-O3 -march=x86-64 -mtune=generic -mfpmath=sse -msse2 -DSFMT_MEXP=19937 -fno-strict-aliasing"
RUN opam pin add --switch=book --kind=path --no-action --yes owl-base /opt/book-sources/owl \
    && opam pin add --switch=book --kind=path --no-action --yes owl /opt/book-sources/owl \
    && opam pin add --switch=book --kind=path --no-action --yes owl-top /opt/book-sources/owl \
    && opam pin add --switch=book --no-action --yes owl-plplot /opt/book-sources/owl-plplot \
    && opam install --switch=book --yes --jobs=2 owl owl-top owl-plplot utop
RUN printf '%s\n' '#use "topfind";;' '#require "owl-top";;' '#require "owl-plplot";;' \
    'open Owl;;' 'open Owl_plplot;;' > /home/opam/.ocamlinit
COPY --chown=opam:opam docker/patches/owl-ode-rk45-step.patch /tmp/owl-ode-rk45-step.patch
ARG OWL_ODE_REF
RUN test -n "$OWL_ODE_REF" \
    && git clone https://github.com/owlbarn/owl_ode.git /opt/book-sources/owl-ode \
    && git -C /opt/book-sources/owl-ode checkout "$OWL_ODE_REF" \
    && git -C /opt/book-sources/owl-ode apply /tmp/owl-ode-rk45-step.patch \
    && opam pin add --switch=book --kind=path --no-action --yes owl-ode-base /opt/book-sources/owl-ode \
    && opam pin add --switch=book --kind=path --no-action --yes owl-ode /opt/book-sources/owl-ode \
    && opam install --switch=book --yes --jobs=2 owl-ode
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends gfortran && rm -rf /var/lib/apt/lists/*
USER opam
RUN opam pin add --switch=book --kind=path --no-action --yes owl-ode-odepack /opt/book-sources/owl-ode \
    && opam install --switch=book --yes --jobs=2 owl-ode-odepack
COPY --chown=opam:opam code/chap11_neural_network/mnist.json code/chap11_neural_network/MNIST-NOTICE.md /opt/book-data/
COPY --chown=opam:opam scripts/prepare_mnist.py /opt/book-data/prepare_mnist.py
RUN python3 /opt/book-data/prepare_mnist.py --manifest /opt/book-data/mnist.json
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    imagemagick libpng-dev libjpeg-dev libtiff-dev libxpm-dev libfreetype-dev libgif-dev && rm -rf /var/lib/apt/lists/*
USER opam
COPY --chown=opam:opam docker/patches/camlimages-fpic.patch /opt/book-camlimages.patch
RUN opam source camlimages.5.0.5 --dir=/opt/book-sources/camlimages \
    && cd /opt/book-sources/camlimages && git apply /opt/book-camlimages.patch \
    && opam pin add --switch=book --no-action --yes camlimages.5.0.5 /opt/book-sources/camlimages \
    && opam install --switch=book "base<v0.17" "stdio<v0.17" camlimages --yes --no-depexts --jobs=2
COPY --from=mrcnn-model --chown=opam:opam /opt/mrcnn-portable /home/opam/.owl/dataset/mrcnn
COPY --from=legacy-model --chown=opam:opam /opt/inception-portable /home/opam/.owl/dataset/inception
COPY --from=legacy-style-model --chown=opam:opam /opt/style-portable /home/opam/.owl/dataset/style
ENV QT_QPA_PLATFORM=offscreen
ENV OPENBLAS_NUM_THREADS=1
# owl-rllib and the Python environment its bridge runs (Gymnasium, MuJoCo, VizDoom).
# Ubuntu's Python 3.12 suits VizDoom 1.2.3; wheels only, nothing is compiled.
USER root
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    python3-venv && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /opt/book-python && chown opam:opam /opt/book-python
USER opam
COPY --chown=opam:opam docker/rllib-requirements.txt /opt/book-python/requirements.txt
RUN python3 -m venv /opt/book-python/venv \
    && /opt/book-python/venv/bin/pip install --no-cache-dir --only-binary=:all: -r /opt/book-python/requirements.txt \
    && /opt/book-python/venv/bin/python -I -c 'import gymnasium, Box2D, mujoco, imageio, vizdoom'
ENV OWL_RLLIB_PYTHON=/opt/book-python/venv/bin/python
RUN opam install --switch=book --yes --jobs=2 alcotest
ARG OWL_RLLIB_REF
# Built with dune rather than pinned: owl-rllib requires owl > 1.2, while the
# path-pinned Owl main branch still carries version 1.2. Its tests, including the
# bridge tests against the Python packages above, must pass in this environment.
RUN test -n "$OWL_RLLIB_REF" \
    && git clone https://github.com/owlbarn/owl-rllib.git /opt/book-sources/owl-rllib \
    && git -C /opt/book-sources/owl-rllib checkout "$OWL_RLLIB_REF" \
    && cd /opt/book-sources/owl-rllib \
    && opam exec --switch=book -- dune build \
    && opam exec --switch=book -- dune test --force \
    && opam exec --switch=book -- dune install \
    && rm -rf _build
FROM book-tools AS book
ARG BASE_IMAGE
ARG BOOK_VALIDATION_MODE=auto
ARG BOOK_VERIFY_NONCE=
ARG BOOK_CODE_REV=unknown
ARG BOOK_CODE_DIRTY=unknown
ENV BOOK_CODE_REV=${BOOK_CODE_REV}
ENV BOOK_CODE_DIRTY=${BOOK_CODE_DIRTY}
ENV BOOK_BASE_IMAGE=${BASE_IMAGE}
WORKDIR /opt/book
COPY --chown=opam:opam . /opt/book
RUN --mount=type=bind,from=verification_cache,target=/verified-cache \
    opam exec --switch=book -- python3 docker/record_environment.py \
    && opam exec --switch=book -- python3 scripts/run_book.py --output /opt/book/verification --mode "$BOOK_VALIDATION_MODE" --cache /verified-cache \
    && rm -rf /opt/book/code/_build
ENTRYPOINT ["opam", "exec", "--switch=book", "--"]
CMD ["bash"]
