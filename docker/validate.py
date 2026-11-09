#!/usr/bin/env python3
"""Check a built/pulled image against its resolved inputs and run the reader command."""
import argparse
import json
import subprocess
from pathlib import Path
from check_repl import check_repl


def validate(image, resolved, execute=True):
    reader = "import json\nfrom pathlib import Path\nroot=Path('/opt/book')\nchapters=json.loads((root/'docker/chapters.json').read_text())\nprint(json.dumps({'environment':json.loads((root/'environment.json').read_text()),\n                  'chapters':chapters,\n                  'verification':{number:json.loads((root/'verification'/spec['output']/'results.json').read_text()) for number,spec in chapters.items()}}))\n"
    raw = subprocess.check_output(['docker','run','--rm','--network','none',image,'python3','-c',reader],text=True)
    record = json.loads(raw)
    env = record['environment']
    expected = {'ocaml':resolved['ocaml'], 'code_revision':resolved['code_revision'],
                'code_dirty':str(resolved['code_dirty']).lower(), 'base_image':resolved['base_image']}
    for key,value in expected.items():
        if env.get(key) != value:
            raise ValueError(f'Image {key} does not match the resolved build input')
    for name in ('owl','owl-plplot','owl-ode'):
        if env['sources'].get(name) != resolved[name]:
            raise ValueError(f'Image uses a different {name} commit')
    if env.get('dependency_patches',{}).get('owl-ode',{}).get('sha256') != resolved['owl-ode-patch']:
        raise ValueError('Image does not record the expected ODE accuracy patch')
    conv=env.get('dependency_patches',{}).get('owl-conv',{})
    if not resolved.get('owl-conv-patch') or conv.get('sha256')!=resolved['owl-conv-patch'] or not conv.get('patched_file_sha256'):
        raise ValueError('Missing or different Owl blocked-convolution accuracy patch')
    if '14' in record['chapters']:
        patch=env.get('dependency_patches',{}).get('camlimages',{})
        if not resolved.get('camlimages-patch') or patch.get('sha256')!=resolved['camlimages-patch'] or not patch.get('patched_files'):
            raise ValueError('Missing or different CamlImages build compatibility patch')
    build_paths={'code/dune-project','code/dune','code/springer-book-code.opam',
                 'code/chap04_linear-algebra/imageUtils.ml'}
    if (set(env.get('build_inputs',{}))!=build_paths or
        any(env['source_hashes'].get(name)!=sha for name,sha in env.get('build_inputs',{}).items())):
        raise ValueError('Missing or inconsistent shared/build input evidence')
    if '-march=x86-64' not in env.get('owl_cflags', '').split() or '-march=native' in env.get('owl_cflags', '').split():
        raise ValueError('Image is not compiled for the generic x86-64 target')
    for number,spec in record['chapters'].items():
        verification = record['verification'][number]
        if {item['name'] for item in verification['files']} != set(spec['plots']) or verification['ocaml'] != resolved['ocaml']:
            raise ValueError(f'Missing or inconsistent baked Chapter {number} verification')
        if verification.get('blocked_regions',{}) != spec.get('blocked_regions',{}):
            raise ValueError(f'Chapter {number} skipped coverage differs from the declared scope')
        if verification.get('illustrations',[]) != spec.get('illustrations',[]):
            raise ValueError(f'Chapter {number} illustrative scope differs from the declared scope')
        for dataset in spec.get('datasets',[]):
            evidence=verification.get('datasets',{}).get(dataset)
            if not evidence or evidence != env.get('datasets',{}).get(dataset):
                raise ValueError(f'Chapter {number} dataset evidence differs from image inputs: {dataset}')
        if not verification['sources'] or any(env['source_hashes'].get(name) != sha for name,sha in verification['sources'].items()):
            raise ValueError(f'Baked Chapter {number} results do not match image sources')
    if execute:
        record['interactive_check'] = check_repl(image)
        network = [] if any(spec.get('external_network') for spec in record['chapters'].values()) else ['--network','none']
        subprocess.run(['docker','run','--rm',*network,image,'python3',
                        'scripts/run_book.py','--output','/tmp/book-validation'],check=True)
    record['image'] = image
    record['runtime_check'] = 'passed' if execute else 'not-run'
    return record


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--image',required=True)
    p.add_argument('--resolved',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    a=p.parse_args()
    record=validate(a.image,json.loads(a.resolved.read_text()))
    a.output.parent.mkdir(parents=True,exist_ok=True)
    a.output.write_text(json.dumps(record,indent=2)+'\n')
    print('PASS: exact compiler, source commits, base image and reader execution')


if __name__=='__main__':main()
