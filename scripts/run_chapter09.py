#!/usr/bin/env python3
"""Run the complete Chapter 9 optimization examples and their mathematical checks."""
import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from run_sessions import execute

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('/tmp/chapter09-results'))
a=parser.parse_args()
root=Path(__file__).resolve().parents[1]
chapter=root/'code/chap09_optimisation'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
for name in ('plot_hump.png','plot_gradients.png','plot_gradients.pdf','plot_himm.pdf'):
    (output/name).unlink(missing_ok=True)
commands=[]
for command,cwd in [(['dune','build','--profile','release','@chap09_optimisation/all'],root/'code')]+[([str(root/f'code/_build/default/chap09_optimisation/{name}.exe')],output) for name in ('rosenbrock','himm')]:
    r=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(r.stdout,end='')
    if r.returncode:raise ValueError(r.stdout+r.stderr)
execute(chapter/'sessions/chapter09.ml',output)
files=[]
supplementary=[]
for name in ('plot_hump.png','plot_gradients.png','plot_gradients.pdf','plot_himm.pdf'):
    data=(output/name).read_bytes()
    magic=b'%PDF-' if name.endswith('.pdf') else b'\x89PNG\r\n\x1a\n'
    if len(data)<100 or not data.startswith(magic):raise ValueError(f'Invalid plot: {name}')
    item={'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()}
    (supplementary if name.endswith('.pdf') else files).append(item)
record={'scope':'All 13 active Chapter 9 listings and existing native optimization programs.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(chapter.rglob('*')) if p.is_file()},
        'files':files,'supplementary_files':supplementary}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 9 roots, stationary points, gradient descent, Owl optimizer and Newton')
