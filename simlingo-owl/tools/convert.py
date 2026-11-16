"""Offline, strict conversion of the SimLingo epoch=013 state dict for Owl."""
import argparse
import json
from pathlib import Path
import shutil

import numpy as np
import torch
from assets import digest

SPECIAL = ['<WAYPOINTS>', '<WAYPOINTS_DIFF>', '<ORG_WAYPOINTS_DIFF>',
           '<ORG_WAYPOINTS>', '<WAYPOINT_LAST>', '<ROUTE>', '<ROUTE_DIFF>', '<TARGET_POINT>']
LM = 'language_model.model.base_model.model.'
VISION = 'vision_model.image_encoder.model.'


def converted(state):
    aliases = {
        LM + 'embed_tokens.weight': LM + 'model.embed_tokens.weight',
        'adaptors.language.embed_tokens.weight': LM + 'model.embed_tokens.weight',
        'adaptors.language.lm_head.weight': LM + 'lm_head.weight',
    }
    for key, original in aliases.items():
        if not torch.equal(state[key], state[original]):
            raise ValueError(f'nonidentical alias: {key}')
    used, names, merges = set(aliases), set(), 0
    for key, tensor in state.items():
        if key in used or '.lora_' in key:
            continue
        value = tensor.detach().float()
        if '.base_layer.weight' in key:
            stem = key[:-len('.base_layer.weight')]
            ak, bk = stem + '.lora_A.default.weight', stem + '.lora_B.default.weight'
            a, b = state[ak].detach().float(), state[bk].detach().float()
            if a.shape[0] != 32 or b.shape[1] != 32:
                raise ValueError('expected rank-32 LoRA')
            value = value + 2.0 * (b @ a)
            used.update((ak, bk))
            merges += 1
        name = key.replace('.base_layer.', '.')
        if name.startswith(LM + 'model.'):
            name = 'qwen.' + name[len(LM + 'model.'):]
        elif name.startswith(LM):
            name = 'qwen.' + name[len(LM):]
        elif name.startswith(VISION + 'vision_model.'):
            name = 'vit.' + name[len(VISION + 'vision_model.'):]
        elif name.startswith(VISION + 'mlp1.'):
            name = name[len(VISION):]
        elif name.startswith('adaptors.driving.'):
            name = 'driving.' + name[len('adaptors.driving.'):]
        elif not name.startswith('wp_encoder.'):
            raise ValueError(f'unrecognized tensor: {key}')
        if name == 'vit.embeddings.patch_embedding.weight':
            value = value.permute(2, 3, 1, 0)
        elif name.endswith('.weight') and value.ndim == 2 and name != 'qwen.embed_tokens.weight':
            value = value.T
        if name in names:
            raise ValueError(f'duplicate name: {name}')
        names.add(name)
        used.add(key)
        yield name, np.ascontiguousarray(value.numpy(), dtype='<f4'), key
    if set(state) != used or merges != 168:
        raise ValueError(f'unconsumed tensors: {set(state) - used}; LoRA merges: {merges}')


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--checkpoint', type=Path, required=True)
    p.add_argument('--base', type=Path, required=True, help='local InternVL2-1B snapshot')
    p.add_argument('--out', type=Path, required=True)
    args = p.parse_args()
    torch.set_num_threads(4)
    config = json.loads((args.base / 'config.json').read_text())
    expected = {
        'vision_config': dict(hidden_size=1024, num_hidden_layers=24, num_attention_heads=16,
                              intermediate_size=4096, patch_size=14, image_size=448,
                              qk_normalization=False, norm_type='layer_norm',
                              hidden_act='gelu', layer_norm_eps=1e-6),
        'llm_config': dict(hidden_size=896, num_hidden_layers=24, num_attention_heads=14,
                           num_key_value_heads=2, intermediate_size=4864, vocab_size=151655,
                           rope_theta=1e6, rms_norm_eps=1e-6, use_sliding_window=False),
    }
    for section, fields in expected.items():
        for field, value in fields.items():
            if config[section].get(field) != value:
                raise ValueError('unsupported config: %s.%s' % (section, field))
    if config['ps_version'] != 'v2' or config['downsample_ratio'] != 0.5 or config['select_layer'] != -1:
        raise ValueError('unsupported visual projection config')
    args.out.mkdir(parents=True, exist_ok=False)
    state = torch.load(str(args.checkpoint), map_location='cpu', weights_only=True, mmap=True)
    if len(state) != 992:
        raise ValueError(f'expected 992 tensors, got {len(state)}')
    tensors = {}
    for name, value, source in converted(state):
        path = args.out / (name + '.npy')
        np.save(path, value, allow_pickle=False)
        tensors[name] = dict(file=path.name, shape=list(value.shape), dtype='float32',
                             sha256=digest(path), source=source)
    tok = args.out / 'tokenizer'
    tok.mkdir()
    for name in ('vocab.json', 'merges.txt', 'tokenizer_config.json'):
        shutil.copyfile(args.base / name, tok / name)
    added = json.loads((args.base / 'added_tokens.json').read_text())
    for i, token in enumerate(SPECIAL, 151655):
        added[token] = i
    (tok / 'added_tokens.json').write_text(json.dumps(added, indent=2) + '\n')
    shutil.copyfile(args.base / 'config.json', args.out / 'config.json')
    manifest = dict(format_version=1, checkpoint_sha256=digest(args.checkpoint),
                    lora_scale=2.0, lora_merges=168, tensors=tensors,
                    parameters=sum(np.prod(v['shape']).item() for v in tensors.values()))
    (args.out / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    print(json.dumps({k: v for k, v in manifest.items() if k != 'tensors'}, indent=2))


if __name__ == '__main__':
    main()
