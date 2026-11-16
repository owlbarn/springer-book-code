#!/usr/bin/env python3
"""Compare two complete book_run.py replays, without and with --kv-cache."""
import argparse
import hashlib
import json
import math
from pathlib import Path


def read(path):
    return json.loads(path.read_text())


def verify(baseline, cached):
    before, after = [read(p/'execution.json') for p in (baseline, cached)]
    if before.get('kv_cache', False) or not after.get('kv_cache'):
        raise ValueError('expected an uncached run followed by a cached run')
    for run in (before, after):
        if run['exit_code'] != 0 or not run.get('validation'):
            raise ValueError('both runs must have completed validation')
    for field in ('image_id', 'weights_manifest_sha256', 'sources'):
        if before[field] != after[field]:
            raise ValueError('run inputs differ: '+field)
    for root, evidence in ((baseline, before), (cached, after)):
        for name, expected in evidence['artifacts'].items():
            if hashlib.sha256((root/name).read_bytes()).hexdigest() != expected:
                raise ValueError('execution artifact changed: '+str(root/name))
    scenes = {}
    for name in ('straight', 'curve', 'close_follow', 'brake'):
        a, b = [read(root/name/'output.json') for root in (baseline, cached)]
        for key in ('answer', 'token_ids', 'eos_reached'):
            if a[key] != b[key]:
                raise ValueError(name+': '+key+' differs')
        if not b['eos_reached'] or b['token_ids'][-1] != 151645:
            raise ValueError(name+': missing EOS')
        errors = {}
        for key, rows in (('route', 20), ('speed_waypoints', 10)):
            for output in (a, b):
                values = output[key]
                if len(values) != rows or any(len(row) != 2 for row in values):
                    raise ValueError(name+': invalid shape '+key)
                if not all(math.isfinite(v) for row in values for v in row):
                    raise ValueError(name+': nonfinite '+key)
            error = max(abs(x-y) for xs, ys in zip(a[key], b[key]) for x, y in zip(xs, ys))
            if error > 1e-3:
                raise ValueError(name+': '+key+' max error '+str(error))
            errors[key] = error
        result = {'tokens': len(b['token_ids']), 'max_absolute_error_metres': errors}
        for label, root in (('baseline', baseline), ('cached', cached)):
            path = root/name/'measurement.json'
            if path.exists():
                measurement = read(path)
                if measurement['exit_code'] != 0:
                    raise ValueError(name+': measurement failed')
                result[label] = {k: measurement[k] for k in
                                 ('elapsed_seconds', 'peak_rss_kib', 'peak_rss_gib')}
        if 'baseline' in result and 'cached' in result:
            result['speedup'] = result['baseline']['elapsed_seconds'] / result['cached']['elapsed_seconds']
        scenes[name] = result
    return {'passed': True, 'absolute_tolerance_metres': 1e-3,
            'image_id': before['image_id'],
            'weights_manifest_sha256': before['weights_manifest_sha256'],
            'sources': before['sources'],
            'execution_sha256': {
                label: hashlib.sha256((root/'execution.json').read_bytes()).hexdigest()
                for label, root in (('baseline', baseline), ('cached', cached))},
            'scenes': scenes}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('baseline', type=Path)
    parser.add_argument('cached', type=Path)
    parser.add_argument('--out', required=True, type=Path)
    args = parser.parse_args()
    result = verify(args.baseline, args.cached)
    args.out.write_text(json.dumps(result, indent=2)+'\n')
    print('PASS: all four answers, token sequences and EOS match; trajectories within 0.001 m')
    for name, scene in result['scenes'].items():
        if 'speedup' in scene:
            print('{}: {:.2f}s -> {:.2f}s ({:.2f}x); peak RSS {:.3f} -> {:.3f} GiB'.format(
                name, scene['baseline']['elapsed_seconds'], scene['cached']['elapsed_seconds'],
                scene['speedup'], scene['baseline']['peak_rss_gib'], scene['cached']['peak_rss_gib']))
