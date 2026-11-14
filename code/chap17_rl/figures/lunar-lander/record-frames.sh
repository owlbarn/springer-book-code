# Repeat the full run, saving every evaluation step, for the web animation.
# From the companion repository root, mount code/ at /source and an empty
# directory at /results:
#   docker run --rm --network none -u "$(id -u):$(id -g)" -v "$PWD/code:/source:ro" \
#     -v /tmp/lunar-frames:/results matrixanger/springer-book-code:latest \
#     bash /source/chap17_rl/figures/lunar-lander/record-frames.sh
# The run must reproduce training.log and lunar-lander.csv exactly; then
#   python3 make_gifs.py lunar /tmp/lunar-frames /path/to/springer/book/figures/rl
set -eu
sed 's/if List.mem t \[0; 50; 100; 200; 400\] then/if true then/' \
  /source/chap17_rl/examples/lunar-lander.ml > /results/example.ml
cd /results
ocamlfind ocamlopt -package owl,owl-rllib,owl-rllib.bridges -linkpkg example.ml -o lunar.exe
./lunar.exe --steps 1000000 --frames > training.log 2> training.stderr
