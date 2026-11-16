#!/usr/bin/env python3
"""Replay the book's four frames in a local extension of its reference image."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

from PIL import Image


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--image', default='springer-simlingo:chapter18')
    parser.add_argument('--weights', type=Path, default=Path('artifacts/weights'))
    parser.add_argument('--out', type=Path, default=Path('artifacts/book'))
    parser.add_argument('--kv-cache', action='store_true')
    parser.add_argument('--cgraph', action='store_true', help='CGraph Qwen2 blocks with KV cache')
    parser.add_argument('--measure-memory', action='store_true',
                        help='record each inference process wall time and peak RSS')
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=False)
    frames = ['straight', 'curve', 'close_follow', 'brake']
    sources = sorted([*root.glob('lib/*'), *root.glob('bin/*'),
                      root/'dune-project', root/'simlingo-owl.opam',
                      root/'Dockerfile.book', Path(__file__).resolve()])
    if args.measure_memory:
        sources.append(root/'tools/measure_memory.py')
    for name in frames:
        sources += [root/'demo'/name/f for f in ['frame.png', 'input.json', 'targets.json']]
        target = out/name
        target.mkdir()
        with Image.open(root/'demo'/name/'frame.png') as im:
            im.convert('RGB').save(target/'frame.ppm')
    recorded = {str(p.relative_to(root)): sha(p) for p in sources if p.is_file()}
    image_id = subprocess.check_output(
        ['docker', 'image', 'inspect', '--format', '{{.Id}}', args.image], text=True).strip()
    commands = [
        'mkdir /tmp/model',
        'cp -r /source/lib /source/bin /source/dune-project /source/simlingo-owl.opam /tmp/model/',
        'cd /tmp/model',
        'ocamlc -version > /results/ocaml-version.txt',
        'opam list --installed --short > /results/packages.txt',
        'cp /opt/book/environment.json /results/base-environment.json',
        'dune build bin/main.exe',
        "_build/default/bin/main.exe --weights /weights --tokenize 'Follow the route.' --output /results/tokenization.json",
    ]
    # Fixed retained inputs keep shell values small and under repository control.
    import shlex
    for name in frames:
        inp = json.loads((root/'demo'/name/'input.json').read_text())
        inference = [
            '_build/default/bin/main.exe', '--weights', '/weights',
            '--image', '/results/'+name+'/frame.ppm',
            '--speed', inp['speed'], '--crop-bottom', inp['crop_bottom'],
            '--targets', '/source/demo/'+name+'/targets.json',
            '--question', inp['question'], '--output', '/results/'+name+'/output.json']
        if args.kv_cache:
            inference.append('--kv-cache')
        if args.cgraph:
            inference.append('--cgraph')
        if args.measure_memory:
            inference = ['python3', '/source/tools/measure_memory.py', '--report',
                         '/results/'+name+'/measurement.json', '--'] + inference
        commands.append(' '.join(shlex.quote(str(v)) for v in inference))
    command = ['docker', 'run', '--rm', '--network', 'none',
               '--memory=16g', '--memory-swap=16g',
               '--user', str(os.getuid())+':'+str(os.getgid()),
               '-e', 'OPENBLAS_NUM_THREADS=4', '-e', 'OMP_NUM_THREADS=4',
               '-v', str(root)+':/source:ro',
               '-v', str(args.weights.resolve())+':/weights:ro',
               '-v', str(out)+':/results', image_id,
               'bash', '-lc', 'set -eu\n'+'\n'.join(commands)]
    start = time.time()
    with (out/'run.log').open('w') as log:
        process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                   text=True, bufsize=1)
        for line in process.stdout:
            print(line, end='', flush=True)
            log.write(line)
        status = process.wait()
    evidence = {'schema': 'simlingo-book-run-v1', 'image_id': image_id,
                'kv_cache': args.kv_cache or args.cgraph, 'cgraph': args.cgraph,
                'sources': recorded, 'command': command, 'exit_code': status,
                'elapsed_seconds': time.time()-start,
                'weights_manifest_sha256': sha(args.weights/'manifest.json')}
    if status == 0:
        import math
        for name in frames:
            result = json.loads((out/name/'output.json').read_text())
            assert result['eos_reached'] and result['token_ids'][-1] == 151645
            assert len(result['route']) == 20 and len(result['speed_waypoints']) == 10
            assert all(len(row) == 2 and all(math.isfinite(v) for v in row)
                       for key in ['route', 'speed_waypoints'] for row in result[key])
        evidence['validation'] = 'EOS, trajectory shapes and finite coordinates passed for all four frames'
    evidence['artifacts'] = {str(p.relative_to(out)): sha(p) for p in out.rglob('*') if p.is_file()}
    (out/'execution.json').write_text(json.dumps(evidence, indent=2)+'\n')
    if status:
        raise SystemExit(status)


if __name__ == '__main__':
    main()
