#!/usr/bin/env python3
"""Resolve release/compiler sources once; build locally or supply CI build args."""
import argparse
import time
import hashlib
import json
import os
import re
import subprocess
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def run(*cmd):
    return subprocess.check_output(cmd, text=True).strip()


def stable_version(release):
    version = release['tag_name']
    if release['prerelease'] or release['draft'] or not re.fullmatch(r'\d+\.\d+\.\d+', version):
        raise ValueError(f'Not a stable OCaml release: {version}')
    return version


def resolve():
    request = urllib.request.Request('https://api.github.com/repos/ocaml/ocaml/releases/latest',
                                     headers={'User-Agent': 'springer-book-code'})
    token = os.environ.get('GH_TOKEN')
    if token:
        request.add_header('Authorization', 'Bearer '+token)
    with urllib.request.urlopen(request, timeout=60) as response:
        version = stable_version(json.load(response))
    refs = {}
    for name, repository, branch in [('owl','owl','main'),('owl-plplot','owl-plplot','main'),('owl-ode','owl_ode','master'),
                                     ('owl-rllib','owl-rllib','main')]:
        sha = run('git', 'ls-remote', f'https://github.com/owlbarn/{repository}.git', 'refs/heads/'+branch).split()[0]
        if not re.fullmatch(r'[0-9a-f]{40}', sha):
            raise ValueError(f'Invalid {name} commit')
        refs[name] = sha
    series = '.'.join(version.split('.')[:2])
    base_tag = f'ocaml/opam:ubuntu-24.04-ocaml-{series}'
    digest = run('docker', 'buildx', 'imagetools', 'inspect', base_tag, '--format', '{{.Manifest.Digest}}')
    if not re.fullmatch(r'sha256:[0-9a-f]{64}', digest):
        raise ValueError('Could not resolve base-image digest')
    return {'ocaml': version, **refs, 'base_image': base_tag+'@'+digest,
            'owl-ode-patch':hashlib.sha256((ROOT/'docker/patches/owl-ode-rk45-step.patch').read_bytes()).hexdigest(),
            'owl-conv-patch':hashlib.sha256((ROOT/'docker/patches/owl-conv-block-offset.patch').read_bytes()).hexdigest(),
            'camlimages-patch':hashlib.sha256((ROOT/'docker/patches/camlimages-fpic.patch').read_bytes()).hexdigest(),
            'code_revision': run('git', '-C', str(ROOT), 'rev-parse', 'HEAD'),
            'code_dirty': bool(run('git', '-C', str(ROOT), 'status', '--porcelain'))}


def build_args(resolved):
    return {'BASE_IMAGE': resolved['base_image'], 'OCAML_VERSION': resolved['ocaml'],
            'BOOK_CODE_REV': resolved['code_revision'], 'BOOK_CODE_DIRTY': str(resolved['code_dirty']).lower(),
            'OWL_REF': resolved['owl'], 'OWL_PLPLOT_REF': resolved['owl-plplot'],
            'OWL_ODE_REF': resolved['owl-ode'], 'OWL_RLLIB_REF': resolved['owl-rllib']}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--mode',choices=['auto','full','quick'],default='auto')
    parser.add_argument('--tag', default='matrixanger/springer-book-code:candidate')
    parser.add_argument('--resolve-only', action='store_true')
    parser.add_argument('--resolved', type=Path, help='Reuse an already resolved build specification')
    parser.add_argument('--output', type=Path)
    parser.add_argument('--github-output', action='store_true')
    args = parser.parse_args()
    resolved = json.loads(args.resolved.read_text()) if args.resolved else resolve()
    print(json.dumps(resolved, indent=2), flush=True)
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(resolved, indent=2)+'\n')
    if args.github_output:
        with open(os.environ['GITHUB_OUTPUT'], 'a') as stream:
            stream.write('build_args<<BOOK_BUILD_ARGS\n')
            stream.write('\n'.join(k+'='+v for k,v in build_args(resolved).items())+'\nBOOK_BUILD_ARGS\n')
    if not args.resolve_only:
        cache=ROOT/'.book-cache';cache.mkdir(exist_ok=True)
        cmd = ['docker', 'build', '--platform', 'linux/amd64', '--pull', '--tag', args.tag,
               '--build-context','verification_cache='+str(cache),'--build-arg','BOOK_VALIDATION_MODE='+args.mode]
        if args.mode=='full':
            cmd += ['--build-arg','BOOK_VERIFY_NONCE='+str(time.time_ns())]
        for k,v in build_args(resolved).items():
            cmd.extend(['--build-arg', k+'='+v])
        subprocess.run(cmd+[str(ROOT)], check=True)


if __name__ == '__main__':
    main()
