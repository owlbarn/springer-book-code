#!/usr/bin/env python3
"""Run Chapter 6 numeric tests, the full canonical session, and all book plots."""
import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from run_sessions import execute

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', type=Path, default=Path('/tmp/chapter06-results'))
args = parser.parse_args()
root = Path(__file__).resolve().parents[1]
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
files = ['plot_rk01.png','plot_circle.png','two_body.png','lorenz_01.png','lorenz_02.png','damped.png','vdp_odepack_stiff.png']
for name in files:
    (output/name).unlink(missing_ok=True)
commands = []
for command,cwd in [(['dune','build','--profile','release','@chap06_diffequation/all'],root/'code'),
                    ([str(root/'code/_build/default/chap06_diffequation/test_ode.exe')],output),
                    ([str(root/'code/_build/default/chap06_diffequation/midpoints.exe')],output),
                    ([str(root/'code/_build/default/chap06_diffequation/lorenz_01.exe')],output),
                    ([str(root/'code/_build/default/chap06_diffequation/lorenz_02.exe')],output)]:
    result = subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(result.stdout,end='')
    if result.returncode:
        raise SystemExit(result.stdout+result.stderr)
mapping=json.loads((root/'code/chap06_diffequation/listings.json').read_text())
execute(root/'code/chap06_diffequation/sessions/chapter06.ml',output,skip=mapping['blocked_regions'])
artifacts = []
for name in files:
    data = (output/name).read_bytes()
    magic = b'%PDF-' if name.endswith('.pdf') else b'\x89PNG\r\n\x1a\n'
    if len(data)<100 or not data.startswith(magic):
        raise ValueError(f'Invalid plot: {name}')
    artifacts.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
record = {'scope':'Chapter 6 native and ODEPACK inputs, numeric properties and seven book plots; SUNDIALS regions are explicitly unexecuted.',
          'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
          'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted((root/'code/chap06_diffequation').rglob('*')) if p.is_file()},
          'files':artifacts,'blocked_regions':mapping['blocked_regions']}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 6 native/ODEPACK checks; SUNDIALS explicitly unexecuted')
