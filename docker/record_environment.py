"""Record the actual installed environment inside the image."""
import json
import os
import hashlib
import subprocess
from pathlib import Path

def run(*cmd):
    return subprocess.check_output(cmd, text=True).strip()

data = {'code_revision': os.environ.get('BOOK_CODE_REV'),
        'code_dirty': os.environ.get('BOOK_CODE_DIRTY'),
        'base_image': os.environ.get('BOOK_BASE_IMAGE'),
        'owl_cflags': os.environ.get('OWL_CFLAGS'),
        'os': Path('/etc/os-release').read_text(),
        'source_hashes': {str(p.relative_to('/opt/book')): hashlib.sha256(p.read_bytes()).hexdigest()
                          for p in sorted(Path('/opt/book/code/chap02_maths').rglob('*')) if p.is_file() and (p.suffix in {'.ml', '.mli', '.json', '.md'} or p.name == 'dune')},
        'ocaml': run('ocamlc', '-version'),
        'packages': run('opam', 'list', '--switch=book', '--installed', '--columns=name,version', '--short'),
        'sources': {name: run('git', '-C', f'/opt/book-sources/{name}', 'rev-parse', 'HEAD')
                    for name in ('owl', 'owl-plplot')}}
Path('/opt/book/environment.json').write_text(json.dumps(data, indent=2) + '\n')
