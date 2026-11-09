#!/usr/bin/env python3
"""Execute marked canonical OCaml sessions and preserve actual phrase outputs."""
import argparse
import hashlib
import json
import re
import subprocess
from pathlib import Path


def execute(source, output, skip=None):
    source = Path(source).resolve()
    output = Path(output).resolve()
    output.mkdir(parents=True, exist_ok=True)
    text = source.read_text()
    regions = re.findall(r'\(\* book: ([\w-]+) begin \*\)\n(.*?)\n\(\* book: \1 end \*\)', text, re.S)
    if not regions or len({name for name, _ in regions}) != len(regions):
        raise ValueError('Expected uniquely named canonical session regions')
    skip = skip or {}
    if set(skip) - {name for name,_ in regions}:
        raise ValueError('Unknown skipped session region')
    skipped = {name:reason for name,reason in skip.items()}
    regions = [(name,code) for name,code in regions if name not in skip]
    setup = text[:text.index('(* book:')]
    program = setup
    phrases = []
    for name, code in regions:
        for phrase in (p.strip() for p in code.split(';;')):
            if not phrase:
                continue
            index = len(phrases)
            phrases.append((name, phrase))
            program += f'\nprint_endline "BOOK_BEGIN_{index}";;\n{phrase};;\nprint_endline "BOOK_END_{index}";;\n'
    command = ['ocaml', '-noinit', '-noprompt', '-nopromptcont', '-I', str(source.parent.parent)]
    result = subprocess.run(command, input=program, cwd=output, text=True, capture_output=True, timeout=180)
    record = {'source': str(source), 'source_sha256': hashlib.sha256(source.read_bytes()).hexdigest(),
              'command': command, 'cwd': str(output), 'input': program,
              'returncode': result.returncode, 'stdout': result.stdout, 'stderr': result.stderr,
              'skipped_regions':skipped}
    (output/'repl-execution.json').write_text(json.dumps(record, indent=2)+'\n')
    if result.returncode or re.search(r'\bError:|\bException:', result.stdout + result.stderr):
        raise ValueError(result.stdout + result.stderr)
    outputs = {name: [] for name, _ in regions}
    for index, (name, phrase) in enumerate(phrases):
        value = result.stdout.split(f'BOOK_BEGIN_{index}\n', 1)[1].split(f'BOOK_END_{index}\n', 1)[0]
        if not value.startswith('- : unit = ()\n'):
            raise ValueError('Unexpected toplevel marker output')
        outputs[name].append({'input': phrase+';;', 'output': value.removeprefix('- : unit = ()\n').strip()+'\n'})
    (output/'session-results.json').write_text(json.dumps(outputs, indent=2)+'\n')
    print(f'PASS: {len(regions)} session regions, {len(phrases)} actual OCaml phrases')
    for name,reason in skipped.items():
        print(f'NOT RUN: {name}: {reason}')
    return outputs


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    execute(args.source, args.output)
