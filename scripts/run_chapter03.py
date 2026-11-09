#!/usr/bin/env python3
"""Run Chapter 3 numeric tests, the full canonical session, and all book plots."""
import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from run_sessions import execute

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', type=Path, default=Path('/tmp/chapter03-results'))
args = parser.parse_args()
root = Path(__file__).resolve().parents[1]
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
files = ['plot_02.png', 'plot_gaussian_cdf.pdf', 'plot_01.png', 'gamma_pdf.png']
for name in files:
    (output/name).unlink(missing_ok=True)
commands = []
for command,cwd in [(['dune','build','--profile','release','@chap03_stats/all'],root/'code'),
                    ([str(root/'code/_build/default/chap03_stats/test_statistics.exe')],output),
                    ([str(root/'code/_build/default/chap03_stats/gamma_pdf.exe')],output)]:
    result = subprocess.run(command,cwd=cwd,text=True,capture_output=True,timeout=180)
    commands.append({'command':command,'cwd':str(cwd),'returncode':result.returncode,'stdout':result.stdout,'stderr':result.stderr})
    (output/'execution.json').write_text(json.dumps(commands,indent=2)+'\n')
    print(result.stdout,end='')
    if result.returncode:
        raise SystemExit(result.stdout+result.stderr)
execute(root/'code/chap03_stats/sessions/chapter03.ml',output)
artifacts = []
for name in files:
    data = (output/name).read_bytes()
    magic = b'%PDF-' if name.endswith('.pdf') else b'\x89PNG\r\n\x1a\n'
    if len(data)<100 or not data.startswith(magic):
        raise ValueError(f'Invalid plot: {name}')
    artifacts.append({'name':name,'bytes':len(data),'sha256':hashlib.sha256(data).hexdigest()})
record = {'scope':'Chapter 3 canonical inputs, numeric properties and four book plots; file checks are not visual review.',
          'ocaml':subprocess.check_output(['ocamlc','-version'],text=True).strip(),
          'sources':{str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted((root/'code/chap03_stats').rglob('*')) if p.is_file()},
          'files':artifacts}
(output/'results.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: Chapter 3 statistics, all canonical examples, and four plot files')
