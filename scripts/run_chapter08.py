#!/usr/bin/env python3
"""Run the complete Chapter 8 teaching session and native analytic AD checks."""
import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from run_sessions import execute

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('/tmp/chapter08-results'))
a=parser.parse_args()
root=Path(__file__).resolve().parents[1]
chapter=root/'code/chap08_algodiff'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
(output/'plot_00.png').unlink(missing_ok=True)
commands=[]
for command,cwd in [(['dune','build','--profile','release','@chap08_algodiff/all'],root/'code')]+[([str(root/f'code/_build/default/chap08_algodiff/{name}.exe')],output) for name in ('test_algodiff','high_order')]:
    r=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(r.stdout,end='')
    if r.returncode:raise ValueError(r.stdout+r.stderr)
execute(chapter/'sessions/chapter08.ml',output)
data=(output/'plot_00.png').read_bytes()
if len(data)<100 or not data.startswith(b'\x89PNG\r\n\x1a\n'):raise ValueError('Invalid derivative plot')
record={'scope':'All 53 active Chapter 8 listings; intentionally incorrect untagged counterexample separately asserted.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(chapter.rglob('*')) if p.is_file()},
        'files':[],
        'supplementary_files':[{'name':'plot_00.png','bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()}]}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 8 teaching, native analytic checks and higher-order plot')
