#!/usr/bin/env python3
"""Measure one Linux child process, including its peak resident memory (KiB)."""
import argparse
import json
import platform
import resource
import subprocess
import time
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--report', required=True, type=Path)
parser.add_argument('command', nargs=argparse.REMAINDER)
args = parser.parse_args()
command = args.command[1:] if args.command[:1] == ['--'] else args.command
if not command:
    parser.error('supply a command after --')
if platform.system() != 'Linux':
    parser.error('this measurement records Linux ru_maxrss in KiB')
start = time.monotonic()
status = subprocess.call(command)
usage = resource.getrusage(resource.RUSAGE_CHILDREN)
args.report.write_text(json.dumps({
    'command': command, 'exit_code': status,
    'elapsed_seconds': time.monotonic() - start,
    'peak_rss_kib': usage.ru_maxrss,
    'peak_rss_gib': usage.ru_maxrss / (1024 ** 2),
    'metric': 'Linux ru_maxrss of the child process',
    'platform': platform.platform(),
}, indent=2) + '\n')
raise SystemExit(status)
