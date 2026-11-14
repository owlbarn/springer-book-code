FROM matrixanger/springer-book-code:rllib-quick
USER root
ADD --chown=opam:opam owl-rllib.tar /tmp/book-rllib
USER opam
WORKDIR /tmp/book-rllib
RUN opam exec --switch=book -- dune build @install test/test_bridges.exe && opam exec --switch=book -- dune install --prefix /home/opam/.opam/book
ENV SDL_VIDEODRIVER=dummy
ENV SDL_AUDIODRIVER=dummy
RUN cd test && OPENBLAS_NUM_THREADS=1 ../_build/default/test/test_bridges.exe
USER root
RUN python3 -c 'import hashlib,json,pathlib; p=pathlib.Path("/opt/book/environment.json"); d=json.loads(p.read_text()); fs=sorted(x for x in pathlib.Path("/tmp/book-rllib").rglob("*") if x.is_file() and "_build" not in x.parts); h=hashlib.sha256(); [(h.update(str(x.relative_to("/tmp/book-rllib")).encode()),h.update(x.read_bytes())) for x in fs]; d["rllib"].update(commit="a1aca6a8194fa6d1443a268159faa6801c6987e1", source_tree_sha256=h.hexdigest(), dirty=True); p.write_text(json.dumps(d,indent=2)+"\n")'
USER opam
WORKDIR /opt/book
