# Chapter 17: Reinforcement learning

New chapter of the second edition. It appears in the book after Chapter 11
(Neural networks) and before Chapter 12; the directory number is a stable
identifier, so the existing chapter directories keep their first-edition
numbers.

All code is in self-contained examples under `examples/`, built on
`owl-rllib`. An example that uses it lists `owl-rllib` (or one of its
sub-libraries) in `packages` of its `name.json`; its verification then
depends on the owl-rllib commit, so an owl-rllib update reruns only these
examples. Display-only code sets `"run": false`: it is compiled, but neither
executed nor given an output.

## Examples

| Name | Topic | Default run |
| --- | --- | --- |
| gridworld | MDP and Bellman residual | Finite-model checks |
| planning | Value/policy iteration, slippery FrozenLake | Value/policy plot |
| tabular | MC, TD, SARSA, Q-learning, Taxi | CliffWalking learning curves |
| linear | Tile-coded MountainCar | 500 episodes |
| dqn | CartPole target-network comparison | 50000 steps per setting |
| reinforce | CartPole learned-baseline comparison | 1000 episodes per setting |
| baseline-variance | Frozen-policy paired gradients | 1000 training episodes, 64 held-out trajectories |
| actor-critic | A2C and PPO on CartPole | 51200 steps per method |
| pendulum | Gaussian-action PPO | 102400 steps |
| llm-grpo | Addition SFT followed by GRPO | 500 SFT steps, 250 RL iterations |
| lunar-lander | Gymnasium PPO | 16384 steps (one rollout) |
| vizdoom-dqn | Pixel-input DQN | 2048 steps |
| design | Graph, Algodiff, Stepper and clipping | Compile only |

From the manuscript checkout use `make example EXAMPLE=17/gridworld`, or
`make examples CHAPTER=17`. Within the reference container run, for example:

```sh
python3 scripts/book_examples.py --example 17/llm-grpo --output /tmp/llm-grpo
```

All ordinary programs have a 60-second execution timeout. The case programs
use short defaults and a 180-second timeout; these runs verify the pipeline,
not convergence. To compile and run a full case directly in the container:

```sh
ocamlfind ocamlopt -package owl,owl-rllib,owl-rllib.bridges -linkpkg \
  code/chap17_rl/examples/lunar-lander.ml -o /tmp/lunar.exe
mkdir -p /tmp/lunar-results
cd /tmp/lunar-results
/tmp/lunar.exe --steps 1000000 --frames
```

The image sets `OWL_RLLIB_PYTHON` to the isolated simulator environment and
`OPENBLAS_NUM_THREADS=1`. Headless frame export uses `SDL_VIDEODRIVER=dummy`
and `SDL_AUDIODRIVER=dummy`. Figure provenance and regeneration scripts live
in [figures/](figures/README.md). Short case outputs are intentionally not printed
in the manuscript. Every displayed output in other sections comes from execution.
