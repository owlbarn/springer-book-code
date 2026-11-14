"""Content-addressed verification policy: full, reused-full, and quick stay distinct."""
import hashlib
import json
import shutil
import subprocess
from pathlib import Path

SCHEMA = 1
HEAVY = {'12', '15'}


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, separators=(',', ':')).encode()).hexdigest()


def file_hash(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def runtime_file(path):
    return path.is_file() and not any(p in ('_build','__pycache__','.git') for p in path.parts) and path.suffix not in ('.md','.pyc','.cmi','.cmx','.cmo','.o')


def pipeline_inputs(root):
    paths = [root/'Dockerfile', root/'.dockerignore']
    for directory in ('code','scripts','docker','.github/workflows'):
        paths.extend((root/directory).rglob('*'))
    inputs = {str(p.relative_to(root)):file_hash(p) for p in sorted(set(paths)) if runtime_file(p)}
    from book_examples import discover, source_inputs
    for source in discover(root).values():inputs.update(source_inputs(root, source))
    return inputs


def environment_key(env):
    fields = ('ocaml','sources','base_image','packages','system_packages','dependency_patches','owl_cflags','build_inputs')
    return {name:env.get(name) for name in fields}


def chapter_key(root, number, spec, env):
    chapter = root/spec['directory']
    paths = [p for p in chapter.rglob('*') if 'examples' not in p.relative_to(chapter).parts]
    paths += [root/f'scripts/run_chapter{number}.py', root/'scripts/run_sessions.py',
              root/'scripts/verification.py', root/'docker/validate.py', root/'Dockerfile']
    mapping = chapter/'listings.json'
    if mapping.exists():
        paths += [root/name for name in json.loads(mapping.read_text()).get('support_scripts',[])]
    # Include input preparation and model conversion code conservatively.
    paths += list((root/'scripts').glob('prepare_*.py'))
    paths += list((root/'docker/legacy').glob('*'))
    sources = {str(p.relative_to(root)):file_hash(p) for p in sorted(set(paths)) if runtime_file(p)}
    return digest({'schema':SCHEMA,'environment':environment_key(env),'spec':spec,'sources':sources,
                   'datasets':{name:env.get('datasets',{}).get(name) for name in spec.get('datasets',[])}})


def uses_rllib(root, inputs):
    for name in inputs:
        path = Path(name)
        if path.suffix == '.json' and path.parent.name == 'examples':
            packages = json.loads((root/name).read_text()).get('packages', [])
            return any(p.split('.')[0] == 'owl-rllib' for p in packages)
    return False


def example_key(root, inputs, env):
    value = {'schema':SCHEMA,'environment':environment_key(env),'sources':inputs,
             'policy':file_hash(root/'scripts/verification.py')}
    # owl-rllib is kept out of environment_key; only examples using it depend on its commit.
    if uses_rllib(root, inputs):
        value['rllib'] = env.get('rllib',{}).get('commit')
    return digest(value)


def receipt(directory, key, action, origin=None):
    files = {p.name:file_hash(p) for p in sorted(directory.iterdir()) if p.is_file()}
    return {'key':key,'action':action,'origin':origin,'files':files}


def valid_receipt(directory, entry, key):
    if not entry or entry.get('key') != key or not entry.get('files'):
        return False
    for name, expected in entry['files'].items():
        if Path(name).name != name or not (directory/name).is_file() or (directory/name).is_symlink() or file_hash(directory/name) != expected:
            return False
    return 'results.json' in entry['files'] or 'result.json' in entry['files']


def reusable(directory, entry, key):
    return bool(entry and entry.get('action') in ('full','reused-full') and valid_receipt(directory, entry, key))


def copy_result(source, target, entry):
    target.mkdir(parents=True, exist_ok=True)
    for name in entry['files']:
        shutil.copyfile(source/name, target/name)


def validate_policy(root, output, env, chapters, examples, require_complete=True):
    policy = json.loads((output/'policy.json').read_text())
    if policy.get('schema') != SCHEMA:
        raise ValueError('Unknown verification policy schema')
    if set(policy['chapters']) != set(chapters) or set(policy['examples']) != set(examples):
        raise ValueError('Verification policy coverage differs')
    complete = True
    for number, spec in chapters.items():
        entry = policy['chapters'][number]
        key = chapter_key(root, number, spec, env)
        if entry.get('action') == 'quick':
            complete = False
            if number not in HEAVY or not valid_receipt(output/spec['output'], entry, key):
                raise ValueError('Missing, corrupt or stale quick verification: '+number)
        elif not reusable(output/spec['output'], entry, key):
            raise ValueError('Missing, corrupt or stale full verification: '+number)
    for identity, inputs in examples.items():
        if not reusable(output/'examples'/identity, policy['examples'][identity], example_key(root, inputs, env)):
            raise ValueError('Missing or stale example verification: '+identity)
    if policy.get('complete') != complete or require_complete and not complete:
        raise ValueError('Quick-only verification cannot publish a reader image')
    return policy


def run(root, output, mode='full', cache=None):
    from book_examples import discover, source_inputs, run_example
    env = json.loads((root/'environment.json').read_text())
    chapters = json.loads((root/'docker/chapters.json').read_text())
    output.mkdir(parents=True, exist_ok=True)
    (output/'policy.json').unlink(missing_ok=True)
    cached = {}
    if cache and (cache/'verification/policy.json').is_file():
        try:
            cached = json.loads((cache/'verification/policy.json').read_text())
            if cached.get('schema') != SCHEMA or not cached.get('complete'):
                cached = {}
        except (ValueError, OSError):
            cached = {}
    policy = {'schema':SCHEMA,'mode':mode,'complete':True,'chapters':{},'examples':{}}
    origin = json.loads((cache/'origin.json').read_text()) if cached else None
    for number, spec in chapters.items():
        target = output/spec['output'];key = chapter_key(root, number, spec, env)
        target.mkdir(parents=True, exist_ok=True)
        for old in target.iterdir():
            if old.is_file() or old.is_symlink():old.unlink()
        entry = cached.get('chapters',{}).get(number)
        source = cache/'verification'/spec['output'] if cached else None
        if mode != 'full' and cached and reusable(source, entry, key):
            copy_result(source, target, entry)
            policy['chapters'][number] = receipt(target, key, 'reused-full', origin)
            print('REUSED FULL: Chapter '+number, flush=True)
            continue
        quick = mode == 'quick' and number in HEAVY
        subprocess.run(['python3',str(root/f'scripts/run_chapter{number}.py'),
                        '--output',str(target)]+(['--quick'] if quick else []),check=True)
        policy['chapters'][number] = receipt(target, key, 'quick' if quick else 'full')
        if quick:
            policy['complete'] = False
    reports = {}
    for identity, source_path in discover(root).items():
        key = example_key(root, source_inputs(root, source_path), env)
        target = output/'examples'/identity
        if target.exists():shutil.rmtree(target)
        entry = cached.get('examples',{}).get(identity)
        source = cache/'verification/examples'/identity if cached else None
        if mode != 'full' and cached and reusable(source, entry, key):
            copy_result(source, target, entry)
            action = 'reused-full'
            reports[identity] = json.loads((target/'result.json').read_text())
        else:
            reports[identity] = run_example(root, identity, target)
            action = 'full'
        policy['examples'][identity] = receipt(target, key, action, origin if action=='reused-full' else None)
    (output/'examples').mkdir(exist_ok=True)
    (output/'examples/results.json').write_text(json.dumps(reports,indent=2)+'\n')
    (output/'policy.json').write_text(json.dumps(policy,indent=2)+'\n')
    print('Verification actions: '+json.dumps({n:e['action'] for n,e in policy['chapters'].items()}),flush=True)
    return policy
