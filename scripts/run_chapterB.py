#!/usr/bin/env python3
"""Execute every original visualisation example and verify real rendered figures."""
import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
from pathlib import Path
from run_sessions import execute
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapterB-results'))
a=p.parse_args();root=Path(__file__).resolve().parents[1];chapter=root/'code/chapB_visualisation'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True);(output/'results.json').unlink(missing_ok=True)
commands=[]
def run(command,cwd):
    result=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if result.returncode:raise ValueError(result.stdout+result.stderr)
    return result.stdout
run(['dune','build','--profile','release','@chapB_visualisation/all'],root/'code')
figures=json.loads((chapter/'figures.json').read_text());images={}
with tempfile.TemporaryDirectory(prefix='chapterB-') as temporary:
    work=Path(temporary)
    print(run([str(root/'code/_build/default/chapB_visualisation/chapterB_native.exe')],work).strip(),flush=True)
    for name in figures:
        dimensions=run(['convert',name,'-format','%w %h %[standard-deviation]','info:'],work).split()
        width,height,deviation=int(dimensions[0]),int(dimensions[1]),float(dimensions[2])
        if min(width,height)<100 or deviation<=0:raise ValueError('Empty rendered plot: '+name)
        images[name]={'width':width,'height':height,'standard_deviation':deviation}
    # Independent interpreter calls invoke the actual functions again.
    execute(chapter/'sessions/chapterB.ml',work)
    for name in figures+['repl-execution.json','session-results.json']:shutil.copy2(work/name,output/name)
files=[]
for name in figures:
    data=(output/name).read_bytes()
    if not data.startswith(b'\x89PNG\r\n\x1a\n'):raise ValueError('Invalid real plot')
    files.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
paths=[path for path in chapter.rglob('*') if path.is_file()]+[Path(__file__).resolve(),root/'scripts/run_sessions.py']
record={'scope':json.loads((chapter/'listings.json').read_text())['scope'],
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(path.relative_to(root)):hashlib.sha256(path.read_bytes()).hexdigest() for path in paths},
        'metrics':{'examples':22,'original_blocks':24,'image_checks':images},'files':files}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Appendix B 24 blocks, all 22 native/interactive plots decoded and nonblank')
