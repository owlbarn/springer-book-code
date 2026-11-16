"""Export CPU fp32 references using upstream InternViT and Transformers Qwen2.

Loads the original checkpoint, independently merges LoRA in PyTorch layout,
and never reads the converted Owl weights. No training or remote downloads.
"""
import argparse
import importlib
import importlib.util
import json
import os
from pathlib import Path
import sys
import types

os.environ['HF_HUB_OFFLINE'] = '1'
os.environ['TRANSFORMERS_OFFLINE'] = '1'
os.environ['TOKENIZERS_PARALLELISM'] = 'false'
import numpy as np
import torch
from torch import nn
from transformers import AutoTokenizer, Qwen2Config, Qwen2Model

from convert import SPECIAL, LM, VISION, digest


def local_module(base, name):
    if 'local_internvl' not in sys.modules:
        package = types.ModuleType('local_internvl')
        package.__path__ = [str(base)]
        sys.modules['local_internvl'] = package
    return importlib.import_module('local_internvl.' + name)


def prompt(base, speed, question):
    conv = local_module(base, 'conversation').get_conv_template('internlm2-chat')
    text = 'Current speed: %.1f m/s. Target waypoint: <TARGET_POINT><TARGET_POINT>. %s' % (speed, question)
    conv.append_message(conv.roles[0], '<image>\n' + text)
    conv.append_message(conv.roles[1], None)
    query = conv.get_prompt()
    system = conv.system_template.replace('{system_message}', conv.system_message) + conv.sep
    return query.replace(system, '').replace('<image>', '<img>' + '<IMG_CONTEXT>'*512 + '</img>', 1)


def tokenizer(base):
    tok = AutoTokenizer.from_pretrained(str(base), trust_remote_code=True, local_files_only=True, use_fast=False)
    tok.add_special_tokens({'additional_special_tokens': SPECIAL})
    assert tok.convert_tokens_to_ids(SPECIAL) == list(range(151655, 151663))
    return tok


def build_models(state, base):
    config = json.loads((base/'config.json').read_text())
    vc = local_module(base, 'configuration_intern_vit').InternVisionConfig(**config['vision_config'])
    vc.use_flash_attn = False
    vit = local_module(base, 'modeling_intern_vit').InternVisionModel(vc).eval()
    vit.load_state_dict({k[len(VISION+'vision_model.'):]: v for k,v in state.items()
                         if k.startswith(VISION+'vision_model.')}, strict=True)
    projector = nn.Sequential(nn.LayerNorm(4096, eps=1e-6), nn.Linear(4096,896), nn.GELU(), nn.Linear(896,896)).eval()
    projector.load_state_dict({k[len(VISION+'mlp1.'):]: v for k,v in state.items() if k.startswith(VISION+'mlp1.')}, strict=True)
    cfg = Qwen2Config(**config['llm_config'])
    cfg._attn_implementation = 'eager'
    qwen = Qwen2Model(cfg).eval()
    merged = {}
    for key, value in state.items():
        if not key.startswith(LM+'model.') or '.lora_' in key:
            continue
        value = value.detach().float()
        if '.base_layer.weight' in key:
            stem = key[:-len('.base_layer.weight')]
            value = value + (state[stem+'.lora_B.default.weight'].float() @
                             state[stem+'.lora_A.default.weight'].float()) * (64/32)
        merged[key[len(LM+'model.'):].replace('.base_layer.', '.')] = value
    qwen.load_state_dict(merged, strict=True)
    del merged
    head = nn.Linear(896,151655,bias=False).eval()
    head.weight.data.copy_(state[LM+'lm_head.weight'])
    wp = nn.Sequential(nn.Linear(2,256),nn.ReLU(),nn.Linear(256,512),nn.ReLU(),nn.Linear(512,896)).eval()
    wp.load_state_dict({k[len('wp_encoder.mlp.'):]:v for k,v in state.items() if k.startswith('wp_encoder.mlp.')})
    route = nn.Sequential(nn.Linear(896,512),nn.SiLU(),nn.Linear(512,256),nn.SiLU(),nn.Linear(256,2,bias=False)).eval()
    speed = nn.Sequential(nn.Linear(896,256),nn.SiLU(),nn.Linear(256,2,bias=False)).eval()
    for module, prefix in [(route,'adaptors.driving.route_head.'),(speed,'adaptors.driving.speed_wps_head.')]:
        module.load_state_dict({k[len(prefix):]:v for k,v in state.items() if k.startswith(prefix)})
    return vit, projector, qwen, head, wp, route, speed


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--checkpoint', type=Path, required=True)
    p.add_argument('--base', type=Path, required=True)
    p.add_argument('--pixels', type=Path, required=True, help='NHWC fp32 npy exported by Owl, to isolate model arithmetic')
    p.add_argument('--targets', type=Path, required=True)
    p.add_argument('--speed', type=float, required=True)
    p.add_argument('--question', default='What should the ego do next?')
    p.add_argument('--max-new-tokens', type=int, default=100)
    p.add_argument('--out', type=Path, required=True)
    args = p.parse_args()
    if not 1 <= args.max_new_tokens <= 100:
        p.error('max-new-tokens must be 1..100')
    args.out.mkdir(parents=True, exist_ok=False)
    torch.set_num_threads(4)
    def save(name, value):
        np.save(args.out/(name+'.npy'), value.detach().float().cpu().numpy(), allow_pickle=False)
    state = torch.load(str(args.checkpoint), map_location='cpu', weights_only=True, mmap=True)
    tok = tokenizer(args.base)
    query = prompt(args.base, args.speed, args.question)
    ids = tok.encode(query, add_special_tokens=False)
    (args.out/'prompt_ids.json').write_text(json.dumps(ids))
    (args.out/'prompt.txt').write_text(query)
    with torch.inference_mode():
        vit, projector, qwen, head, wp, route_head, speed_head = build_models(state, args.base)
        first = []
        handle = vit.encoder.layers[0].register_forward_hook(lambda m,a,o: first.append(o))
        pixels = torch.from_numpy(np.load(args.pixels)).permute(0,3,1,2).contiguous()
        print('reference vision', flush=True)
        hidden = vit(pixels).last_hidden_state
        handle.remove()
        for i in range(2): save('vit_layer1_tile%d' % i, first[0][i])
        # Call the upstream shuffle method without instantiating another 1B model.
        chat = local_module(args.base, 'modeling_internvl_chat').InternVLChatModel
        shim = types.SimpleNamespace(ps_version='v2')
        shuffled = chat.pixel_shuffle(shim, hidden[:,1:].reshape(2,32,32,1024), scale_factor=0.5)
        features = projector(shuffled.reshape(2,256,4096)).reshape(512,896)
        save('image_features', features)
        ids_tensor = torch.tensor(ids)
        inputs = qwen.embed_tokens(ids_tensor.clamp(0,151654)).unsqueeze(0)
        inputs[0,ids_tensor==151648] = features
        targets = torch.tensor(json.loads(args.targets.read_text()),dtype=torch.float32)
        inputs[0,ids_tensor==151662] = wp(targets)
        save('input_embeddings', inputs[0])
        generated = []
        handle = qwen.layers[0].register_forward_hook(lambda m,a,o: save('qwen_layer1',o[0][0]))
        for i in range(args.max_new_tokens):
            print('reference token %d' % (i+1), flush=True)
            hidden = qwen(inputs_embeds=inputs, use_cache=False).last_hidden_state
            logits = head(hidden[:,-1])
            if i == 0:
                save('first_logits',logits)
                handle.remove()
            token = int(logits.argmax(-1))
            generated.append(token)
            inputs = torch.cat([inputs,qwen.embed_tokens(torch.tensor([[token]]))],dim=1)
            if token == 151645: break
        inputs = torch.cat([inputs,state['adaptors.driving.query_embeds_wps'].float(),
                            state['adaptors.driving.query_embeds_speed'].float()],dim=1)
        hidden = qwen(inputs_embeds=inputs, use_cache=False).last_hidden_state[:,-30:]
        route = route_head(hidden[:,:20]).cumsum(1)[0]
        speed = speed_head(hidden[:,20:]).cumsum(1)[0]
        result = dict(answer=tok.decode(generated,skip_special_tokens=True),token_ids=generated,
                      route=route.tolist(),speed_waypoints=speed.tolist(),
                      eos_reached=generated[-1]==151645)
        (args.out/'output.json').write_text(json.dumps(result,indent=2)+'\n')
        (args.out/'provenance.json').write_text(json.dumps(dict(
            checkpoint_sha256=digest(args.checkpoint), torch=torch.__version__, precision='float32 CPU',
            implementation='upstream InternViT + Transformers Qwen2, merged LoRA, eager attention',
            source_sha256={name:digest(args.base/name) for name in ['modeling_intern_vit.py','modeling_internvl_chat.py','conversation.py']}
        ),indent=2)+'\n')
        print(json.dumps(result),flush=True)


if __name__ == '__main__':
    main()
