#!/usr/bin/env python3
"""Resolve stable OCaml and main-branch commits, then build the book image."""
import argparse
import json
import re
import subprocess
import urllib.request
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--tag', default='matrixanger/springer-book-code:pilot')
args = parser.parse_args()
root = Path(__file__).resolve().parents[1]
request = urllib.request.Request('https://api.github.com/repos/ocaml/ocaml/releases/latest',
                                 headers={'User-Agent': 'springer-book-code'})
with urllib.request.urlopen(request, timeout=60) as response:
    release = json.load(response)
version = release['tag_name']
if release['prerelease'] or release['draft'] or not re.fullmatch(r'\d+\.\d+\.\d+', version):
    raise SystemExit(f'Not a stable OCaml release: {version}')
refs = {}
for name in ('owl', 'owl-plplot'):
    refs[name] = subprocess.check_output(
        ['git', 'ls-remote', f'https://github.com/owlbarn/{name}.git', 'refs/heads/main'],
        text=True).split()[0]
series = '.'.join(version.split('.')[:2])
resolved = {'ocaml': version, **refs, 'tag': args.tag}
print(json.dumps(resolved, indent=2), flush=True)
subprocess.run(['docker', 'build', '--pull', '--tag', args.tag,
                '--build-arg', f'BASE_IMAGE=ocaml/opam:ubuntu-24.04-ocaml-{series}',
                '--build-arg', f'OCAML_VERSION={version}',
                '--build-arg', 'BOOK_CODE_REV=' + subprocess.check_output(['git', '-C', str(root), 'rev-parse', 'HEAD'], text=True).strip(),
                '--build-arg', 'BOOK_CODE_DIRTY=' + str(bool(subprocess.check_output(['git', '-C', str(root), 'status', '--porcelain'], text=True))).lower(),
                '--build-arg', f'OWL_REF={refs["owl"]}',
                '--build-arg', f'OWL_PLPLOT_REF={refs["owl-plplot"]}', str(root)], check=True)
