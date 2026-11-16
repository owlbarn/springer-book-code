# Model sources and licences

Checked on 16 November 2026 for Chapter 18 of *OCaml Scientific Computing*.

| Component | Published licence | Source |
| --- | --- | --- |
| SimLingo checkpoint | Apache-2.0 | [Official weights repository](https://huggingface.co/RenzKa/simlingo) |
| SimLingo upstream code | Apache-2.0 | [Repository licence](https://github.com/RenzKa/simlingo/blob/main/LICENSE) |
| InternVL2-1B configuration, tokenizer and base model | MIT | [Model card and licence section](https://huggingface.co/OpenGVLab/InternVL2-1B#license) |
| Qwen2-0.5B-Instruct language component | Apache-2.0 | Identified in the InternVL2-1B licence section above |

SimLingo is by Katrin Renz, Long Chen, Elahe Arani and Oleg Sinavski (CVPR 2025).
InternVL2-1B is published by OpenGVLab; Qwen2 is published by the Qwen team.
The chapter cites these sources alongside its explanation and results.

Apache-2.0 permits use, modification and distribution, including commercial
use, subject to its terms. Its grants cover using the released checkpoint to
compute the book's examples. The book and this repository provide OCaml code,
conversion instructions and inference results; the multi-gigabyte checkpoint
and converted weights remain external inputs. If distributing the weights or
modified upstream files, include the applicable licence, retain attribution
and any relevant NOTICE contents, and identify modifications as required by
[sections 2–4 of Apache-2.0](https://www.apache.org/licenses/LICENSE-2.0).
Keep the MIT copyright and permission notice with redistributed InternVL assets.

The example uses:

- SimLingo `simlingo/checkpoints/epoch=013.ckpt/pytorch_model.pt`, SHA-256
  `ec8943723d266ee9f5f56f45d153a163b22616960bfccb741965ea5daa700d28`.
- InternVL2-1B snapshot `0d75ccd166b1d0b79446ae6c5d1a4a667f1e6187`.
- Upstream SimLingo source commit `743b243afd6cf5ff51b9fa1f8cac86f22d569684`.

## Camera frames

The camera frames in `demo/` were rendered with the
[CARLA simulator](https://github.com/carla-simulator/carla) 0.9.15; each
`input.json` names the map and frame. CARLA's assets (maps, vehicles and
buildings shown in the frames) are distributed under the CC-BY licence, and its
code under MIT ([CARLA licences](https://github.com/carla-simulator/carla/blob/0.9.15/README.md#licenses)).
Attribution: CARLA, Dosovitskiy et al., *CARLA: An Open Urban Driving
Simulator*, CoRL 2017. The frames are separate from the SimLingo training
dataset hosted on Hugging Face; the model licences above describe the model assets.
