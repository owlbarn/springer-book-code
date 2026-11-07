#!/usr/bin/env python3
"""Build and run the Chapter 2 pilot, checking numeric results and plot files."""
import argparse
import hashlib
import json
import os
import subprocess
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', type=Path, default=Path('/tmp/chapter02-results'))
args = parser.parse_args()
root = Path(__file__).resolve().parents[1]
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=True)
(output/'results.json').unlink(missing_ok=True)
env = dict(os.environ, QT_QPA_PLATFORM='offscreen', OPENBLAS_NUM_THREADS='1')
commands = []

def run(cmd, cwd):
    result = subprocess.run(cmd, cwd=cwd, env=env, text=True, capture_output=True)
    commands.append({'command': [str(x) for x in cmd], 'cwd': str(cwd),
                     'returncode': result.returncode, 'stdout': result.stdout, 'stderr': result.stderr})
    (output/'execution.json').write_text(json.dumps(commands, indent=2)+'\n')
    print(result.stdout, end='')
    if result.returncode:
        print(result.stderr)
        raise SystemExit(result.returncode)

run(['dune', 'build', '--profile', 'release', '@chap02_maths/all'], root/'code')
binary = root/'code/_build/default/chap02_maths'
# Run tests directly so cached Dune test results cannot hide a skipped execution.
expected = ['interp.png', 'example_integrals.png', 'fig_inter_poly.pdf', 'special_airy.png',
            'example_bessel.png', 'example_gamma.png', 'example_struve.png', 'example_erf.png']
for name in expected:
    (output/name).unlink(missing_ok=True)
for name in ['test_algorithms', 'examples', 'gamma_func', 'integration', 'interpolation', 'special_func']:
    run([str(binary/(name+'.exe'))], output)
run(['python3', str(root/'scripts/check_chapter02_sessions.py')], output)

files = []
for name in expected:
    path = output/name
    data = path.read_bytes()
    signature = b'%PDF-' if path.suffix == '.pdf' else b'\x89PNG\r\n\x1a\n'
    if len(data) < 100 or not data.startswith(signature):
        raise SystemExit(f'Invalid image output: {path}')
    files.append({'name': name, 'bytes': len(data), 'sha256': hashlib.sha256(data).hexdigest()})
record = {'scope': 'Chapter 2 pilot only; file checks are not visual review.',
          'ocaml': subprocess.check_output(['ocamlc', '-version'], text=True).strip(),
          'sources': {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest()
                      for p in sorted((root/'code/chap02_maths').rglob('*.ml'))},
          'files': files}
environment = root/'environment.json'
if environment.exists():
    record['image_environment'] = json.loads(environment.read_text())
(output/'results.json').write_text(json.dumps(record, indent=2)+'\n')
print(f'PASS: numeric checks and {len(files)} plot files; results in {output}')
