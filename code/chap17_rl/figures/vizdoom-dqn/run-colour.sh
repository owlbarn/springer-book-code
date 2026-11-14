set -eu
cd /results
ocamlfind ocamlopt -package owl,owl-rllib,owl-rllib.bridges -linkpkg record_actions.ml -o record_actions.exe
./record_actions.exe /checkpoint/vizdoom-weights.bin
/opt/book-python/venv/bin/python replay_colour.py
