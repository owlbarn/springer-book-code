set -eu
cp /source/lunar-lander.ml /results/example.ml
cd /results
ocamlfind ocamlopt -package owl,owl-rllib,owl-rllib.bridges -linkpkg example.ml -o lunar.exe
./lunar.exe --steps 1000000 --frames > training.log 2> training.stderr
