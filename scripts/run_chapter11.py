#!/usr/bin/env python3
"""Compile and execute the same Chapter 11 program and real MNIST training."""
import argparse
import hashlib
import json
import math
import subprocess
from pathlib import Path
from prepare_mnist import check, DEFAULT_DIRECTORY
from run_sessions import execute

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapter11-results'))
a=p.parse_args()
root=Path(__file__).resolve().parents[1]
chapter=root/'code/chap11_neural_network'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
for name in ('activations.png','prediction.png'):(output/name).unlink(missing_ok=True)
datasets={'mnist':check(DEFAULT_DIRECTORY)}
commands=[]
for command,cwd in [(['dune','build','--profile','release','@chap11_neural_network/all'],root/'code'),([str(root/'code/_build/default/chap11_neural_network/chapter11_compiled.exe')],output)]:
    r=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if r.returncode:raise ValueError(r.stdout+r.stderr)
    if 'PASS:' in r.stdout:print(r.stdout[r.stdout.rfind('PASS:'):].strip())
compiled_metrics=json.loads((output/'metrics.json').read_text())
execute(chapter/'sessions/chapter11.ml',output)
metrics=json.loads((output/'metrics.json').read_text())
if metrics.keys()!=compiled_metrics.keys() or any(not math.isclose(value,compiled_metrics[key],rel_tol=1e-6,abs_tol=1e-7) for key,value in metrics.items()):
    raise ValueError('Native and interactive model metrics disagree')
files=[]
for name in ('activations.png','prediction.png'):
    data=(output/name).read_bytes()
    if len(data)<100 or not data.startswith(b'\x89PNG\r\n\x1a\n'):raise ValueError('Invalid plot: '+name)
    files.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
record={'scope':'All 32 active Chapter 11 listings; full declared MNIST training/test loops; architecture inference only for CNN/RNN/GAN.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(q.relative_to(root)):hashlib.sha256(q.read_bytes()).hexdigest() for q in sorted(chapter.rglob('*')) if q.is_file()},
        'datasets':datasets,'metrics':metrics,'compiled_metrics':compiled_metrics,'files':files}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 11 original data, native and interactive training, architecture checks and two book figures')
