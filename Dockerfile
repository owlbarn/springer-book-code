# syntax=docker/dockerfile:1
# Book pilot environment. Resolve exact versions with docker/build.py.
ARG BASE_IMAGE=ocaml/opam:ubuntu-24.04-ocaml-5.5
FROM ${BASE_IMAGE}
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
ENV OWL_CFLAGS="-O3 -march=x86-64 -mtune=generic -mfpmath=sse -msse2 -DSFMT_MEXP=19937 -fno-strict-aliasing"
RUN opam pin add --switch=book --no-action --yes owl-base /opt/book-sources/owl \
    && opam pin add --switch=book --no-action --yes owl /opt/book-sources/owl \
    && opam pin add --switch=book --no-action --yes owl-top /opt/book-sources/owl \
    && opam pin add --switch=book --no-action --yes owl-plplot /opt/book-sources/owl-plplot \
    && opam install --switch=book --yes --jobs=2 owl owl-top owl-plplot utop
RUN printf '%s\n' '#use "topfind";;' '#require "owl-top";;' '#require "owl-plplot";;' \
    'open Owl;;' 'open Owl_plplot;;' > /home/opam/.ocamlinit
ENV QT_QPA_PLATFORM=offscreen
ENV OPENBLAS_NUM_THREADS=1
ARG BASE_IMAGE
ARG BOOK_CODE_REV=unknown
ARG BOOK_CODE_DIRTY=unknown
ENV BOOK_CODE_REV=${BOOK_CODE_REV}
ENV BOOK_CODE_DIRTY=${BOOK_CODE_DIRTY}
ENV BOOK_BASE_IMAGE=${BASE_IMAGE}
WORKDIR /opt/book
COPY --chown=opam:opam . /opt/book
RUN opam exec --switch=book -- python3 docker/record_environment.py \
    && opam exec --switch=book -- python3 scripts/run_chapter02.py --output /opt/book/verification
ENTRYPOINT ["opam", "exec", "--switch=book", "--"]
CMD ["bash"]
