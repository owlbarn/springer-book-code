"""Report intermediate differences and token equality; no perceptual acceptance threshold."""
import argparse
import json
from pathlib import Path
import numpy as np

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('owl',type=Path)
p.add_argument('reference',type=Path)
p.add_argument('--out',type=Path,required=True)
a = p.parse_args()
report = {}
for file in sorted(a.reference.glob('*.npy')):
    other = a.owl/file.name
    if not other.exists():
        raise FileNotFoundError(other)
    x,y = np.load(other),np.load(file)
    if x.shape != y.shape or not np.isfinite(x).all() or not np.isfinite(y).all():
        raise ValueError('shape/nonfinite failure: '+file.name)
    report[file.stem] = dict(shape=list(x.shape),max_abs=float(np.max(np.abs(x-y))),
                             mean_abs=float(np.mean(np.abs(x-y))))
    if file.stem == 'first_logits':
        report[file.stem].update(owl_argmax=int(x.argmax()),reference_argmax=int(y.argmax()))
for name in ['prompt_ids.json','output.json']:
    x,y = json.loads((a.owl/name).read_text()),json.loads((a.reference/name).read_text())
    if name == 'prompt_ids.json': report['prompt_ids_equal'] = x == y
    else:
        report['generated_tokens_equal'] = x['token_ids'] == y['token_ids']
        report['answers'] = dict(owl=x['answer'],reference=y['answer'])
        for field in ['route','speed_waypoints']:
            report[field+'_max_abs'] = float(np.max(np.abs(np.array(x[field])-np.array(y[field]))))
a.out.write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
