#!/usr/bin/env python3
"""Run all Chapter 7 examples with explicit synthetic second-edition inputs."""
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
names=['plot_sine_1.pdf','plot_sine_2.pdf','plot_sine_3.pdf','cat_noise.png','cat_denoise.png','img_fft1.pdf','img_fft2.pdf',
       'plot_sunspot.png','plot_sunspot_freq.png','tone.png','tone2.png','plot_goog.png','plot_goog_gauss.png','plot_goog2.png']
for name in names:(output/name).unlink(missing_ok=True)
(output/'cat_denoise.ppm').unlink(missing_ok=True)
for name in ['cat_noise.ppm','sunspot_full.csv','touchtone.csv','goog.csv']:shutil.copyfile(chapter/name,output/name)
commands=[]
entries=[(['python3',str(chapter/'generate_inputs.py'),'--check'],root),
         (['dune','build','--profile','release','@chap07_signal/all'],root/'code')]
entries += [([str(root/f'code/_build/default/chap07_signal/{name}.exe')],output)
            for name in ('test_signal','sine','moonlanding','sunspot','csv_examples')]
for command,cwd in entries:
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
    if name.endswith('.png'):
        decoded=subprocess.run(['convert',str(output/name),'-format','%[fx:mean>0 && mean<1]','info:'],capture_output=True,text=True,check=True)
        if decoded.stdout.strip()!='1':raise ValueError(f'Blank plot: {name}')
    files.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
record={'scope':mapping['scope'],'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(chapter.rglob('*')) if p.is_file()},
        'files':files,'blocked_regions':mapping['blocked_regions'],
        'synthetic_inputs':json.loads((chapter/'input-sources.json').read_text()),
        'input_checks':json.loads((output/'input-checks.json').read_text())}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 7 all 31 displays, synthetic input properties, actual tone decoding and 14 figures')
