#!/usr/bin/env python3
"""Run Chapter 7 FFT/image examples; missing original CSV cases stay explicit."""
import argparse
import hashlib
import json
import shutil
import subprocess
from pathlib import Path
from run_sessions import execute
from run_chapter04 import png_from_ppm

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('/tmp/chapter07-results'))
args=parser.parse_args()
root=Path(__file__).resolve().parents[1]
chapter=root/'code/chap07_signal'
output=args.output.resolve();output.mkdir(parents=True,exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
names=['plot_sine_1.pdf','plot_sine_2.pdf','plot_sine_3.pdf','cat_noise.png','cat_denoise.png','img_fft1.pdf','img_fft2.pdf']
for name in names:(output/name).unlink(missing_ok=True)
(output/'cat_denoise.ppm').unlink(missing_ok=True)
shutil.copyfile(chapter/'cat_noise.ppm',output/'cat_noise.ppm')
commands=[]
for command,cwd in [(['dune','build','--profile','release','@chap07_signal/all'],root/'code')]+[([str(root/f'code/_build/default/chap07_signal/{name}.exe')],output) for name in ('test_signal','sine','moonlanding')]:
    r=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':r.returncode,'stdout':r.stdout,'stderr':r.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(r.stdout,end='')
    if r.returncode:raise ValueError(r.stdout+r.stderr)
mapping=json.loads((chapter/'listings.json').read_text())
execute(chapter/'sessions/chapter07.ml',output,skip=mapping['blocked_regions'])
for name in ('cat_noise','cat_denoise'):png_from_ppm(output/(name+'.ppm'),output/(name+'.png'))
files=[]
for name in names:
    data=(output/name).read_bytes()
    magic=b'%PDF-' if name.endswith('.pdf') else b'\x89PNG\r\n\x1a\n'
    if len(data)<100 or not data.startswith(magic):raise ValueError(f'Invalid plot: {name}')
    files.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
record={'scope':'Chapter 7 FFT and original noisy-cat image; original historical CSV cases explicitly unexecuted.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(chapter.rglob('*')) if p.is_file()},
        'files':files,'blocked_regions':mapping['blocked_regions']}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 7 FFT/image checks; original CSV cases explicitly unexecuted')
