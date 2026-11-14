#!/usr/bin/env python3
"""Discover and run self-contained book examples. No per-example registration."""
import argparse
import hashlib
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

ID = re.compile(r'(\d{2}|A|B)/([a-z][a-z0-9-]*)\Z')
MARKER = re.compile(r'^\(\* book: ([a-z][a-z0-9-]*) (begin|end) \*\)\s*$', re.M)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def discover(root):
    found = {}
    for source in sorted((root/'code').glob('chap*_*/examples/*.ml')):
        chapter = source.parent.parent.name.split('_', 1)[0][4:]
        identity = chapter+'/'+source.stem
        if not ID.fullmatch(identity) or identity in found:
            raise ValueError('Invalid or duplicate example: '+identity)
        found[identity] = source
    return found


def source_inputs(root, source):
    paths = [source, root/'scripts/book_examples.py']
    config = source.with_suffix('.json')
    if config.exists():
        paths.append(config)
    data = source.with_suffix('.data')
    if data.exists():
        paths.extend(p for p in data.rglob('*') if p.is_file() or p.is_symlink())
    result = {}
    for path in paths:
        if path.is_symlink() or not path.resolve().is_relative_to(root.resolve()):
            raise ValueError('Example inputs must be regular files inside the repository: '+str(path))
        result[str(path.relative_to(root))] = digest(path)
    return result


def regions(text):
    result, active, start = {}, None, 0
    for match in MARKER.finditer(text):
        name, kind = match.groups()
        if kind == 'begin':
            if active or name in result:
                raise ValueError('Nested or duplicate code region: '+name)
            active, start = name, match.end()
        else:
            if active != name:
                raise ValueError('Unmatched code region: '+name)
            result[name] = text[start:match.start()].strip('\n')+'\n'
            active = None
    if active:
        raise ValueError('Unclosed code region: '+active)
    return result


def configuration(source):
    path = source.with_suffix('.json')
    config = json.loads(path.read_text()) if path.exists() else {}
    if not isinstance(config, dict):
        raise ValueError('Example configuration must be a JSON object')
    if set(config)-{'packages', 'timeout', 'run'}:
        raise ValueError('Unknown example option; supported: packages, timeout, run')
    packages = config.get('packages', ['owl', 'owl-plplot'])
    if not isinstance(packages, list) or any(not isinstance(p, str) or not re.fullmatch(r'[A-Za-z0-9_.-]+', p) for p in packages):
        raise ValueError('packages must be a list of OCaml findlib package names')
    timeout = config.get('timeout', 180)
    if type(timeout) is not int or not 1 <= timeout <= 3600:
        raise ValueError('timeout must be an integer from 1 to 3600 seconds')
    run = config.get('run', True)
    if type(run) is not bool:
        raise ValueError('run must be true or false')
    return packages, timeout, run


def check_figure(path):
    data = path.read_bytes()
    if path.suffix == '.pdf':
        if not data.startswith(b'%PDF-') or b'%%EOF' not in data[-1024:]:
            raise ValueError('Invalid PDF figure: '+path.name)
    else:
        # Decode the pixels, rather than just checking a filename or signature.
        subprocess.run(['identify', '-regard-warnings', str(path)], check=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30)


def run_example(root, identity, output):
    source = discover(root).get(identity)
    if source is None:
        raise ValueError('Unknown example: '+identity)
    inputs = source_inputs(root, source)
    text = source.read_text()
    excerpts = regions(text)
    packages, timeout, execute = configuration(source)
    output.mkdir(parents=True, exist_ok=True)
    # Only export a complete successful run; old publication files live elsewhere.
    (output/'result.json').unlink(missing_ok=True)
    commands = []
    with tempfile.TemporaryDirectory() as temporary:
        work = Path(temporary)
        build, run = work/'build', work/'run'
        build.mkdir(); run.mkdir()
        local_source = build/'example.ml'
        local_source.write_text(text)
        data = source.with_suffix('.data')
        if data.exists():
            shutil.copytree(data, run/'data')
        compile_command = ['ocamlfind', 'ocamlopt']
        if packages:
            compile_command += ['-package', ','.join(packages)]
        compile_command += ['-linkpkg', '-o', str(build/'example.exe'), str(local_source)]
        # "run": false compiles display-only code without executing it.
        steps = [(compile_command, build)]+([([str(build/'example.exe')], run)] if execute else [])
        for command, cwd in steps:
            result = subprocess.run(command, cwd=cwd, capture_output=True, text=True, timeout=timeout)
            commands.append({'command':command, 'returncode':result.returncode,
                             'stdout':result.stdout, 'stderr':result.stderr})
            if result.returncode:
                raise ValueError('Example failed: '+identity+'\n'+result.stdout+result.stderr)
        publication = {'source.ml': text.encode()}
        if execute:
            publication['output.txt'] = commands[-1]['stdout'].encode()
        publication.update({'region-'+name+'.ml': body.encode() for name, body in excerpts.items()})
        figures = []
        for path in sorted(run.iterdir()):
            if path.is_file() and path.suffix.lower() in ('.png', '.pdf', '.jpg', '.jpeg'):
                if path.is_symlink() or not re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9_.-]*', path.name):
                    raise ValueError('Use a simple figure filename: '+path.name)
                if path.name in publication:
                    raise ValueError('Reserved output filename: '+path.name)
                check_figure(path)
                publication[path.name] = path.read_bytes()
                figures.append(path.name)
        if source_inputs(root, source) != inputs:
            raise ValueError('Example inputs changed during execution; run again')
        for name, content in publication.items():
            (output/name).write_bytes(content)
        evidence = {'commands':commands, 'sources':inputs, 'packages':packages,
                    'ocaml':subprocess.check_output(['ocamlc', '-version'], text=True).strip()}
        (output/'execution.json').write_text(json.dumps(evidence, indent=2)+'\n')
        report = {'id':identity, 'status':'passed', 'executed':execute, 'sources':inputs, 'figures':figures,
                  'regions':list(excerpts), 'ocaml':evidence['ocaml'],
                  'artifacts':{name:hashlib.sha256(content).hexdigest() for name, content in publication.items()},
                  'execution_sha256':digest(output/'execution.json')}
        (output/'result.json').write_text(json.dumps(report, indent=2)+'\n')
    print('PASS: example '+identity+f' ({len(figures)} figures)', flush=True)
    return report


def run_all(root, output):
    output.mkdir(parents=True, exist_ok=True)
    (output/'results.json').unlink(missing_ok=True)
    reports = {identity:run_example(root, identity, output/identity) for identity in discover(root)}
    output.mkdir(parents=True, exist_ok=True)
    (output/'results.json').write_text(json.dumps(reports, indent=2)+'\n')
    return reports


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument('--example')
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    try:
        if args.example:
            run_example(args.root.resolve(), args.example, args.output.resolve())
        else:
            run_all(args.root.resolve(), args.output.resolve())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        parser.exit(1, 'book example: '+str(error)+'\n')


if __name__ == '__main__':
    main()
