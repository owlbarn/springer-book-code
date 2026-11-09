#!/usr/bin/env python3
"""Run Chapter 5 numeric tests and every executable canonical input."""
import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from run_sessions import execute

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', type=Path, default=Path('/tmp/chapter05-results'))
args = parser.parse_args()
root = Path(__file__).resolve().parents[1]
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
commands = []
for command,cwd in [(['dune','build','--profile','release','@chap05_ndarray/all'],root/'code'),
                    ([str(root/'code/_build/default/chap05_ndarray/test_ndarray.exe')],output)]:
    result = subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(result.stdout,end='')
    if result.returncode:
        raise SystemExit(result.stdout+result.stderr)
execute(root/'code/chap05_ndarray/sessions/chapter05.ml',output)
record = {'scope':'Chapter 5 canonical inputs and numeric properties; two shape illustrations are not executed.',
          'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
          'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted((root/'code/chap05_ndarray').rglob('*')) if p.is_file()},
          'files':[]}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 5 ndarrays and all executable canonical examples')
