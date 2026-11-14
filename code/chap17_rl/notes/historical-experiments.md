# Historical experiments cited in the Reinforcement Learning chapter

These results come from the earlier owl-rl development experiments recorded in
M4, M6 and ACCEPTANCE. They are distinct from the chapter's short examples and its later VizDoom
DQN run. `historical-sources.json` records the source revision and exact document,
program and CSV hashes. No experiment was rerun to prepare this note.

## Token termination and baselines (M4)

The task is two-digit addition with an end marker. The initial model receives
500 SFT steps. The token-environment comparison uses process rewards and a
learned critic for 32,768 episodes. The recorded test accuracies are:

| Setting | Sampled | Greedy |
| --- | ---: | ---: |
| Initial policy | 0.134 | 0.406 |
| Process rewards and critic; length limit terminates | 0.236 | 0.396 |
| Same setting; length limit incorrectly truncates | 0.000 | 0.000 |

M4 describes completions losing their end marker, for example `85+26=1009`.
It does not tabulate a separate end-marker failure rate; zero correctness should
not be interpreted as a measured end-marker failure percentage.

The variance experiment freezes each policy and reuses 32 batches of 512
completions across estimators. Values below divide the trace of the gradient
covariance by the no-baseline value for that same policy:

| SFT steps | Mean reward | Batch-mean baseline | RLOO | Optimal constant baseline |
| --- | ---: | ---: | ---: | ---: |
| 500 | 0.14 | 0.97 | 1.14 | 0.95 (b*=0.062) |
| 1500 | 0.82 | 1.67 | 1.63 | 0.99 (b*=0.132) |

These are batch-mean, leave-one-out and constant baselines, not the learned
CartPole critic in the chapter's `baseline-variance` example.

Current corresponding programs in owl-rllib (base revision
`a1aca6a8194fa6d1443a268159faa6801c6987e1`):

- `examples/llm/b1_formulation.ml`: token formulation and truncation example.
- `examples/llm/b2_reinforce.ml`: frozen-policy variance and training comparisons.
- `examples/llm/b3_rloo.ml`: leave-one-out estimator.
- `examples/llm/common.ml`: shared initial models and variance measurements.

From an owl-rllib checkout with its dependencies installed:

```sh
OPENBLAS_NUM_THREADS=1 dune exec examples/llm/b1_formulation.exe -- --process --critic --truncate
OPENBLAS_NUM_THREADS=1 dune exec examples/llm/b1_formulation.exe -- --process --critic
OPENBLAS_NUM_THREADS=1 dune exec examples/llm/b2_reinforce.exe
```

The historical programs used `tutorials/llm/` in owl-rl. The paths above identify
their current equivalents; the historical figures are not presented as fresh
measurements of every subsequent library revision.

## Reference implementations

### CartPole DQN and CleanRL (ACCEPTANCE)

The earlier five-seed run used 500,000 steps, learning rate 2.5e-4, replay capacity
10,000, batch size 128, training every 10 steps after a 10,000-step warm-up,
target copies every 500 steps, epsilon falling from 1 to 0.05 over half the run,
and a 120–84 ReLU network. For each seed, the last 100 training episode returns
were averaged; the reported standard deviation is across the five seed means.

- owl-rl: **488.7 ± 17.9**.
- CleanRL's published result: **488.69 ± 16.11**, three seeds.

The CleanRL result was taken from its public documentation, not a local run of
CleanRL under the chapter's environment. Its exact aggregation was not specified
in that documentation. This also differs from the chapter's 50,000-step DQN
example and its greedy evaluation.

### LunarLander PPO and SB3 (M6)

Both implementations ran locally with the same 999,424-step budget and
hyperparameters. Each of five training seeds was evaluated for 20 episodes using
deterministic actions; the mean and sample standard deviation below are across
those five evaluation means. Initial weights and random streams were different.

- owl-rl: **266.9 ± 17.7**; lowest seed **235.8**.
- SB3: **283.3 ± 6.3**; lowest seed **273.1**.

The chapter's single-seed result, **271.11**, is a separate later run. Timing
comparisons are omitted here because the recorded simultaneous and standalone
runs used different host loads. The original M6 VizDoom experiments used PPO,
not the chapter's DQN.
