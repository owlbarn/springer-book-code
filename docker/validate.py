#!/usr/bin/env python3
"""Check a built/pulled image against its resolved inputs and run the reader command."""
import argparse
import json
import subprocess
from pathlib import Path


def validate(image, resolved, execute=True):
    reader = ('import json; from pathlib import Path; '
              'print(json.dumps({"environment":json.loads(Path("/opt/book/environment.json").read_text()),'
              '"verification":json.loads(Path("/opt/book/verification/results.json").read_text())}))')
    raw = subprocess.check_output(['docker','run','--rm','--network','none',image,'python3','-c',reader],text=True)
    record = json.loads(raw)
    env = record['environment']
    expected = {'ocaml':resolved['ocaml'], 'code_revision':resolved['code_revision'],
                'code_dirty':str(resolved['code_dirty']).lower(), 'base_image':resolved['base_image']}
    for key,value in expected.items():
        if env.get(key) != value:
            raise ValueError(f'Image {key} does not match the resolved build input')
    for name in ('owl','owl-plplot'):
        if env['sources'].get(name) != resolved[name]:
            raise ValueError(f'Image uses a different {name} commit')
    if '-march=x86-64' not in env.get('owl_cflags', '').split() or '-march=native' in env.get('owl_cflags', '').split():
        raise ValueError('Image is not compiled for the generic x86-64 target')
    if len(record['verification']['files']) != 8 or record['verification']['ocaml'] != resolved['ocaml']:
        raise ValueError('Missing or inconsistent baked Chapter 2 verification')
    if execute:
        subprocess.run(['docker','run','--rm','--network','none',image,'python3',
                        'scripts/run_chapter02.py','--output','/tmp/chapter02-validation'],check=True)
    record['image'] = image
    record['runtime_check'] = 'passed' if execute else 'not-run'
    return record


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--image',required=True)
    p.add_argument('--resolved',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    a=p.parse_args()
    record=validate(a.image,json.loads(a.resolved.read_text()))
    a.output.parent.mkdir(parents=True,exist_ok=True)
    a.output.write_text(json.dumps(record,indent=2)+'\n')
    print('PASS: exact compiler, source commits, base image and reader execution')


if __name__=='__main__':main()
