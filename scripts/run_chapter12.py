#!/usr/bin/env python3
"""Run all original NLP examples with transient verified news and full native LDA."""
import argparse
import hashlib
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from prepare_news import check,prepare
from run_sessions import execute
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--output',type=Path,default=Path('/tmp/chapter12-results'))
p.add_argument('--news',type=Path,help='Reuse a local input only after pinned hash verification')
p.add_argument('--keep-work',action='store_true',help='Keep corpus in output/work for interactive reader sessions')
a=p.parse_args()
root=Path(__file__).resolve().parents[1];chapter=root/'code/chap12_nlp'
output=a.output.resolve();output.mkdir(parents=True,exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
commands=[]
def run(command,cwd,timeout):
    result=subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=timeout)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    if result.returncode:raise ValueError(result.stdout+result.stderr)
    if 'PASS:' in result.stdout:print(result.stdout[result.stdout.rfind('PASS:'):].strip(),flush=True)
run(['dune','build','--profile','release','@chap12_nlp/all'],root/'code',180)
# Work products contain the original text or reversible tokens; cleanup precedes image layer commit.
with tempfile.TemporaryDirectory(prefix='chapter12-') as temporary:
    work=output/'work' if a.keep_work else Path(temporary)
    work.mkdir(parents=True,exist_ok=True)
    if a.news:
        evidence=check(a.news.resolve())
        news=work/'news.txt'
        if news.exists():check(news)
        else:news.symlink_to(a.news.resolve())
    else:news,evidence=prepare(work)
    run([str(root/'code/_build/default/chap12_nlp/chapter12_native.exe'),str(news),'1000'],work,3600)
    iterations=re.findall(r'iter#(\d+) t\(s\):[0-9.]+ tokens:(\d+)',commands[1]['stdout'])
    if iterations!=[(str(i),'12796740') for i in range(1000)]:
        raise ValueError('Missing iterations or changing LDA token mass')
    metrics=json.loads((work/'native-metrics.json').read_text())
    if metrics['iterations']!=1000 or metrics['checks']!=42 or metrics['documents']!=121698 or metrics['tokens']!=12796740:
        raise ValueError('Incomplete full-input NLP verification')
    execute(chapter/'sessions/chapter12.ml',work)
    for name in ('native-metrics.json','repl-execution.json','session-results.json'):
        shutil.copy2(work/name,output/name)
run([str(root/'code/_build/default/chap12_nlp/dirichlet.exe')],output,180)
data=(output/'dirichlet.png').read_bytes()
if not data.startswith(b'\x89PNG\r\n\x1a\n') or len(data)<100:raise ValueError('Invalid Dirichlet plot')
paths=[q for q in sorted(chapter.rglob('*')) if q.is_file()]
paths += [Path(__file__).resolve(),root/'scripts/run_sessions.py',root/'scripts/prepare_news.py']
record={'scope':'27 original listings including two text quotations; all 130000 input lines, full 1000-iteration native SimpleLDA, interactive public types/queries and actual trained count tables.',
        'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
        'sources':{str(q.relative_to(root)):hashlib.sha256(q.read_bytes()).hexdigest() for q in paths},
        'datasets':{'news':evidence},'metrics':metrics,
        'files':[{'name':'dirichlet.png','bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()}]}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 12 full news input, 1000 native LDA iterations, actual interactive queries and Dirichlet samples')
