#!/usr/bin/env python3
"""Run the complete Chapter 10 regression examples and their mathematical checks."""
import argparse
import hashlib
import json
import subprocess
import shutil
from pathlib import Path
from run_sessions import execute

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('/tmp/chapter10-results'))
a=parser.parse_args()
root=Path(__file__).resolve().parents[1]
chapter=root/'code/chap10_regression'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
plots=['regdata.png', 'reg_options.png', 'reg_cost.png', 'reg_gd.png', 'boston.png', 'reg_poly.png', 'reg_poly4s.png', 'reg_poly4s_reg.png', 'reg_logistic.png', 'reg_svm.png', 'sigmoid.pdf', 'kernel.png', 'svm_cost.png']
for name in plots:
    (output/name).unlink(missing_ok=True)
for name in ("data_reg.csv","data_mul.csv","boston.csv"):
    shutil.copyfile(chapter/name,output/name)
commands=[]
for command,cwd in [(['dune','build','--profile','release','@chap10_regression/all'],root/'code')]+[([str(root/f'code/_build/default/chap10_regression/{name}.exe')],output) for name in ('test_regression','plot_reg','mul_reg','poly_reg','gaussian_kernel','svm')]:
    r=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(r.stdout,end='')
    if r.returncode:raise ValueError(r.stdout+r.stderr)
execute(chapter/'sessions/chapter10.ml',output)
files=[]
for name in plots:
    data=(output/name).read_bytes()
    magic=b'%PDF-' if name.endswith('.pdf') else b'\x89PNG\r\n\x1a\n'
    if len(data)<100 or not data.startswith(magic):raise ValueError(f'Invalid plot: {name}')
    files.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
record={'scope':'All 24 active Chapter 10 listings, supplied datasets, native programs and corrected book-specific objectives.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(chapter.rglob('*')) if p.is_file()},
        'files':files}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 10 regression, classification, datasets and thirteen book figures')
