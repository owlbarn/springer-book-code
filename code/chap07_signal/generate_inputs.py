#!/usr/bin/env python3
"""Generate the book's explicitly synthetic second-edition CSV examples."""
import argparse
import csv
import hashlib
import io
import json
import math
import random
from pathlib import Path

SEED = 77
SAMPLE_RATE = 8192
TONE_SAMPLES = 5000
GAP_SAMPLES = 1000
DIGITS = '12345678901'
KEYPAD = {key: (row, column) for row, keys in zip((697, 770, 852, 941), ('123', '456', '789', '*0#'))
          for column, key in zip((1209, 1336, 1477), keys)}


def csv_bytes(rows, separator=','):
    stream = io.StringIO()
    csv.writer(stream, delimiter=separator, lineterminator='\n').writerows(rows)
    return stream.getvalue().encode()


def generated_inputs():
    rng = random.Random(SEED)
    sunspots = []
    for i in range(321):
        cycle = 0.5 + 0.5 * math.cos(2 * math.pi * i / 11)
        amplitude = 90 * (1 + 0.25 * math.sin(2 * math.pi * i / 88))
        sunspots.append([1700+i, f'{15 + amplitude*cycle*cycle + rng.uniform(-4, 4):.6f}'])
    samples = []
    for digit in DIGITS:
        low, high = KEYPAD[digit]
        for i in range(TONE_SAMPLES):
            fade = min(1., i/64., (TONE_SAMPLES-1-i)/64.)
            sample = 48 * fade * (math.sin(2*math.pi*low*i/SAMPLE_RATE) + math.sin(2*math.pi*high*i/SAMPLE_RATE))
            samples.append(f'{sample:.6f}')
        samples.extend(['0.000000'] * GAP_SAMPLES)
    prices = [['sample', 'open', 'high', 'low', 'close', 'adj_close', 'volume']]
    for i in range(251):
        close = 100 + 0.07*i + 7*math.sin(2*math.pi*3*i/251) + 1.5*math.sin(2*math.pi*22*i/251) + rng.uniform(-0.6, 0.6)
        opening = close + 0.3*math.sin(i)
        prices.append([i, f'{opening:.6f}', f'{max(opening, close)+0.8:.6f}',
                       f'{min(opening, close)-0.8:.6f}', f'{close:.6f}', f'{close:.6f}', 1000000+10000*(i % 13)])
    files = {'sunspot_full.csv': csv_bytes(sunspots, ';'),
             'touchtone.csv': csv_bytes([samples]), 'goog.csv': csv_bytes(prices)}
    details = {
        'sunspot_full.csv': {'shape': [321, 2], 'columns': ['year', 'synthetic_count'], 'injected_period_years': 11},
        'touchtone.csv': {'shape': [1, len(samples)], 'sample_rate_hz': SAMPLE_RATE, 'digits': DIGITS,
                         'tone_samples': TONE_SAMPLES, 'gap_samples': GAP_SAMPLES},
        'goog.csv': {'shape': [251, 7], 'header': prices[0], 'observations': 251, 'first_column': 'sample index'},
    }
    for name, data in files.items():
        details[name].update(bytes=len(data), sha256=hashlib.sha256(data).hexdigest())
    manifest = {'edition': 2, 'seed': SEED, 'generator': 'generate_inputs.py',
                'origin': 'Synthetic teaching data generated for this book; no third-party dataset is used.',
                'notice': 'The historical filenames are retained. These are not measured solar observations, a telephone recording or real GOOG quotes. The first-edition CSV files were lost; author approved replacement on 2026-11-09.',
                'files': details}
    files['input-sources.json'] = (json.dumps(manifest, indent=2)+'\n').encode()
    return files


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    if not args.check:
        args.output.mkdir(parents=True, exist_ok=True)
    for name, data in generated_inputs().items():
        target = args.output/name
        if args.check:
            if not target.is_file() or target.read_bytes() != data:
                raise ValueError('Synthetic input differs from the fixed generator: '+str(target))
        else:
            target.write_bytes(data)
    print('PASS: three reproducible, explicitly synthetic second-edition CSV inputs')


if __name__ == '__main__':
    main()
