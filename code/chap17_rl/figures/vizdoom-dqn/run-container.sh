set -eu
cd /results
ocamlfind ocamlopt -package owl,owl-rllib,owl-rllib.bridges -linkpkg example.ml -o doom.exe
./doom.exe --steps 50000 --frames > training.log 2> training.stderr
