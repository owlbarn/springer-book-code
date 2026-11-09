#!/usr/bin/env python3
"""Run original Inception weights, public wrappers and RGB/PPM examples."""
import argparse
import hashlib
import json
import math
import shutil
import subprocess
import tempfile
from pathlib import Path
from prepare_inception import check,DEFAULT_DIRECTORY
from run_sessions import execute
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapter13-results'))
a=p.parse_args();root=Path(__file__).resolve().parents[1];chapter=root/'code/chap13_case-image-inception'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True);(output/'results.json').unlink(missing_ok=True)
evidence=check(DEFAULT_DIRECTORY);commands=[]
def run(command,cwd,timeout=180):
    result=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=timeout)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if result.returncode:raise ValueError(result.stdout+result.stderr)
    if 'PASS:' in result.stdout:print(result.stdout[result.stdout.rfind('PASS:'):].strip(),flush=True)
run(['dune','build','--profile','release','@chap13_case-image-inception/all'],root/'code')
with tempfile.TemporaryDirectory(prefix='chapter13-') as temporary:
    work=Path(temporary);shutil.copy2(DEFAULT_DIRECTORY/'panda.png',work/'panda.png')
    shutil.copy2(DEFAULT_DIRECTORY/'panda.png',work/'panda input.png')
    run(['convert','panda.png','-resize','299x299!','-depth','8','panda.ppm'],work)
    run([str(root/'code/_build/default/chap13_case-image-inception/chapter13_native.exe'),str(DEFAULT_DIRECTORY),str(work/'panda input.png'),str(work/'panda.ppm')],work)
    metrics=json.loads((work/'native-metrics.json').read_text())
    predictions=json.loads((work/'native-predictions.json').read_text())
    if len(predictions)!=5 or not predictions[0]['class'].startswith('giant panda') or not math.isclose(predictions[0]['prop'],metrics['panda_probability'],rel_tol=1e-8):raise ValueError('Invalid native JSON result')
    (work/'book-dependencies.ml').write_text(f'#mod_use {json.dumps(str(root/"code/chap04_linear-algebra/imageUtils.ml"))};;\n#mod_use {json.dumps(str(chapter/"inception_v3.ml"))};;\n')
    execute(chapter/'sessions/chapter13.ml',work)
    for name in ('repl-execution.json','session-results.json','native-metrics.json','native-predictions.json'):
        shutil.copy2(work/name,output/name)
paths=[q for q in sorted(chapter.rglob('*')) if q.is_file()]
paths += [Path(__file__).resolve(),root/'scripts/run_sessions.py',root/'scripts/prepare_inception.py',root/'code/chap04_linear-algebra/imageUtils.ml'] + [root/name for name in ['docker/legacy/export_weights.ml', 'docker/legacy/ocaml.json', 'docker/legacy/build_reader.py']]
record={'scope':'23 original blocks; complete original Inception prediction, all migrated parameter values/shapes, RGB/PPM and wrappers. Three partial TensorFlow/Keras/HDF5 blocks remain explicitly non-executable illustrations.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(q.relative_to(root)):hashlib.sha256(q.read_bytes()).hexdigest() for q in paths},
        'datasets':{'inception':evidence},'metrics':metrics,
        'illustrations':['checkpoint-illustration','keras-illustration','hdf5-illustration'],
        'files':[]}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 13 original pretrained prediction, portable parameter equality, current-format reload, PPM and actual interactive wrappers')
