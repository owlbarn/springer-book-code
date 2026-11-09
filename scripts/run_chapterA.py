#!/usr/bin/env python3
"""Validate original sparse/dense projection and random partition tree examples."""
import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
from pathlib import Path
from run_sessions import execute
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapterA-results'))
a=p.parse_args();root=Path(__file__).resolve().parents[1];chapter=root/'code/chapA_conventions'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True);(output/'results.json').unlink(missing_ok=True)
commands=[]
def run(command,cwd):
    result=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if result.returncode:raise ValueError(result.stdout+result.stderr)
    if result.stdout:print(result.stdout.strip(),flush=True)
run(['dune','build','--profile','release','@chapA_conventions/all'],root/'code')
with tempfile.TemporaryDirectory(prefix='chapterA-') as temporary:
    work=Path(temporary);build=root/'code/_build/default/chapA_conventions'
    run([str(build/'chapterA_native.exe')],work)
    metrics=json.loads((work/'native-metrics.json').read_text())
    mapping=json.loads((chapter/'listings.json').read_text())
    execute(chapter/'sessions/chapterA.ml',work,skip=mapping['blocked_regions'])
    for name in ['native-metrics.json','repl-execution.json','session-results.json']:shutil.copy2(work/name,output/name)
paths=[path for path in chapter.rglob('*') if path.is_file()]+[Path(__file__).resolve(),root/'scripts/run_sessions.py']
mapping=json.loads((chapter/'listings.json').read_text())
record={'scope':mapping['scope'],'blocked_regions':mapping['blocked_regions'],
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(path.relative_to(root)):hashlib.sha256(path.read_bytes()).hexdigest() for path in paths},'metrics':metrics,'files':[]}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Appendix A current conventions checked; historical Ext explicitly not executed')
