#!/usr/bin/env python3
"""Run every restored chapter listed in the image's validation manifest."""
import argparse
import json
import subprocess
from pathlib import Path

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('/tmp/book-results'))
args=parser.parse_args()
root=Path(__file__).resolve().parents[1]
chapters=json.loads((root/'docker/chapters.json').read_text())
for number,spec in chapters.items():
    subprocess.run(['python3',str(root/f'scripts/run_chapter{number}.py'),
                    '--output',str(args.output.resolve()/spec['output'])],check=True)
from book_examples import run_all
run_all(root, args.output.resolve()/'examples')
print('PASS: restored checks for chapters '+', '.join(chapters))
for number,spec in chapters.items():
    if spec.get('blocked_regions'):
        print(f'PARTIAL COVERAGE: Chapter {number}: '+', '.join(spec['blocked_regions']))
