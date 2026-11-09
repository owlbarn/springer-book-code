#!/usr/bin/env python3
"""Run the book with full, incremental, quick or reader smoke verification."""
import argparse
import json
import subprocess
from pathlib import Path
from verification import run

parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output',type=Path,default=Path('/tmp/book-results'))
parser.add_argument('--mode',choices=['full','auto','quick','smoke','registry'],default='full')
parser.add_argument('--cache',type=Path)
args=parser.parse_args()
root=Path(__file__).resolve().parents[1]
output=args.output.resolve()
if args.mode in ('smoke','registry'):
    # Exercise the reader environment without repeating long training.
    chapters=['02'] if args.mode=='registry' else ['01','02','08','12','15']
    for number in chapters:
        subprocess.run(['python3',str(root/f'scripts/run_chapter{number}.py'),
                        '--output',str(output/('chapter'+number))]+
                       (['--quick'] if number in ('12','15') else []),check=True)
    output.mkdir(parents=True,exist_ok=True)
    (output/'reader-check.json').write_text(json.dumps({'mode':args.mode,'status':'passed','chapters':chapters,
        'scope':'Short reader checks; full training evidence is validated separately'})+'\n')
else:
    run(root,output,args.mode,args.cache.resolve() if args.cache else None)
