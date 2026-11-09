#!/usr/bin/env python3
"""Validate the actual London Mask R-CNN example and original architecture excerpts."""
import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
from pathlib import Path
from prepare_mrcnn import check,DEFAULT_DIRECTORY
from run_sessions import execute
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapter14-results'))
a=p.parse_args();root=Path(__file__).resolve().parents[1];chapter=root/'code/chap14_case-obj-detect'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True);(output/'results.json').unlink(missing_ok=True)
evidence=check(DEFAULT_DIRECTORY)
photo=json.loads((chapter/'data/london-photo.json').read_text());original=chapter/'data/london-original.jpg'
if original.stat().st_size!=photo['bytes'] or hashlib.sha256(original.read_bytes()).hexdigest()!=photo['sha256']:raise ValueError('Changed original London photo')
commands=[]
def run(command,cwd,timeout=180):
    result=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=timeout)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if result.returncode:raise ValueError(result.stdout+result.stderr)
    if 'PASS:' in result.stdout:print(result.stdout[result.stdout.rfind('PASS:'):].strip(),flush=True)
run(['dune','build','--profile','release','@chap14_case-obj-detect/all','chap14_case-obj-detect/src/mrcnn.cma','chap14_case-obj-detect/book_mrcnn.cma'],root/'code')
with tempfile.TemporaryDirectory(prefix='chapter14-') as temporary:
    work=Path(temporary);shutil.copy2(chapter/'FreeMonoBold.ttf',work/'FreeMonoBold.ttf')
    crop=photo['crop'];rectangle=f"{crop['width']}x{crop['height']}+{crop['x']}+{crop['y']}"
    run(['convert',str(original),'-crop',rectangle,'+repage','-strip','-depth','8','PNG24:london-input.png'],work)
    run([str(root/'code/_build/default/chap14_case-obj-detect/chapter14_native.exe'),str(DEFAULT_DIRECTORY),'london-input.png'],work,600)
    metrics=json.loads((work/'native-metrics.json').read_text())
    if metrics['instances']<=0 or metrics['model_size']!=1024 or metrics['loaded_tensors']!=714 or metrics['loaded_parameters']!=68916160:
        raise ValueError('Incomplete real London parameter/inference checks')
    build=root/'code/_build/default/chap14_case-obj-detect'
    dependencies=''.join(f'#require {json.dumps(package)};;\n' for package in ['owl','camlimages.png','camlimages.jpeg','camlimages.freetype'])
    dependencies+=''.join(f'#directory {json.dumps(str(path))};;\n' for path in [build/'src/.mrcnn.objs/byte',build/'src',build/'.book_mrcnn.objs/byte',build])
    dependencies+='#load "mrcnn.cma";;\n#load "book_mrcnn.cma";;\n'
    (work/'book-dependencies.ml').write_text(dependencies)
    execute(chapter/'sessions/chapter14.ml',work)
    for name in ('repl-execution.json','session-results.json','native-metrics.json','london.png'):
        shutil.copy2(work/name,output/name)
data=(output/'london.png').read_bytes()
if not data.startswith(b'\x89PNG\r\n\x1a\n'):raise ValueError('Invalid regenerated London figure')
paths=[q for q in sorted(chapter.rglob('*')) if q.is_file()]
paths += [Path(__file__).resolve(),root/'scripts/run_sessions.py',root/'scripts/prepare_mrcnn.py',root/'docker/patches/camlimages-fpic.patch',root/'docker/patches/camlimages-fpic.json']
record={'scope':'All eight original book excerpts; complete 1024 inference on the real credited London crop; all parameters and per-instance mask/box checks, geometry and interpreter stage shapes. Historical upstream weight/video examples are references outside this executed book scope.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(q.relative_to(root)):hashlib.sha256(q.read_bytes()).hexdigest() for q in paths},
        'datasets':{'mrcnn':evidence},'photo':photo,'metrics':metrics,
        'files':[{'name':'london.png','bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()}]}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 14 original full constructor, every weight, real credited London figure and native/interactive checks')
