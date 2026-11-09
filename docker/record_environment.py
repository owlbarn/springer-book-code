"""Record the actual installed environment inside the image."""
import json
import os
import hashlib
import subprocess
from pathlib import Path

def run(*cmd):
    return subprocess.check_output(cmd, text=True).strip()

chapters = json.loads(Path('/opt/book/docker/chapters.json').read_text())
data = {'code_revision': os.environ.get('BOOK_CODE_REV'),
        'code_dirty': os.environ.get('BOOK_CODE_DIRTY'),
        'base_image': os.environ.get('BOOK_BASE_IMAGE'),
        'dependency_patches':{'owl-ode':{
            'sha256':hashlib.sha256(Path('/opt/book/docker/patches/owl-ode-rk45-step.patch').read_bytes()).hexdigest(),
            'source_diff_sha256':hashlib.sha256(subprocess.check_output(['git','-C','/opt/book-sources/owl-ode','diff'])).hexdigest()}},
        'owl_cflags': os.environ.get('OWL_CFLAGS'),
        'os': Path('/etc/os-release').read_text(),
        'source_hashes': {str(p.relative_to('/opt/book')): hashlib.sha256(p.read_bytes()).hexdigest()
                          for spec in chapters.values() for p in sorted((Path('/opt/book')/spec['directory']).rglob('*')) if p.is_file()},
        'ocaml': run('ocamlc', '-version'),
        'packages': run('opam', 'list', '--switch=book', '--installed', '--columns=name,version', '--short'),
        'sources': {name: run('git', '-C', f'/opt/book-sources/{name}', 'rev-parse', 'HEAD')
                    for name in ('owl', 'owl-plplot', 'owl-ode')}}
conv_patch=Path('/opt/book/docker/patches/owl-conv-block-offset.patch')
conv_metadata=conv_patch.with_suffix('.json')
conv=json.loads(conv_metadata.read_text())
actual=hashlib.sha256((Path('/opt/book-sources/owl')/conv['path']).read_bytes()).hexdigest()
if actual!=conv['patched_sha256']:
    raise ValueError('Owl source does not contain the verified blocked-convolution correction')
data['dependency_patches']['owl-conv']={'sha256':hashlib.sha256(conv_patch.read_bytes()).hexdigest(),
                                       'patched_file_sha256':actual,
                                       'source_diff_sha256':hashlib.sha256(subprocess.check_output(['git','-C','/opt/book-sources/owl','diff'])).hexdigest()}
for path in (conv_patch,conv_metadata):
    data['source_hashes'][str(path.relative_to('/opt/book'))]=hashlib.sha256(path.read_bytes()).hexdigest()
build_paths=['code/dune-project','code/dune','code/springer-book-code.opam',
             'code/chap04_linear-algebra/imageUtils.ml']
data['build_inputs']={name:hashlib.sha256((Path('/opt/book')/name).read_bytes()).hexdigest() for name in build_paths}
data['source_hashes'].update(data['build_inputs'])
if '11' in chapters:
    import sys
    sys.path.insert(0,'/opt/book/scripts')
    from prepare_mnist import check, DEFAULT_DIRECTORY
    data['datasets']={'mnist':check(DEFAULT_DIRECTORY)}
data['source_hashes'].update({str(p.relative_to('/opt/book')):hashlib.sha256(p.read_bytes()).hexdigest()
                              for p in Path('/opt/book/scripts').glob('*.py')})
if '12' in chapters:
    import sys
    sys.path.insert(0,'/opt/book/scripts')
    from prepare_news import receipt
    data.setdefault('datasets',{})['news']=receipt()
if '13' in chapters:
    import sys
    sys.path.insert(0,'/opt/book/scripts')
    from prepare_inception import check,DEFAULT_DIRECTORY
    data.setdefault('datasets',{})['inception']=check(DEFAULT_DIRECTORY)
    for p in Path('/opt/book/docker/legacy').iterdir():
        if p.suffix in ('.py','.ml','.json'):
            data['source_hashes'][str(p.relative_to('/opt/book'))]=hashlib.sha256(p.read_bytes()).hexdigest()
    data['model_migration']={'reader':json.loads(Path('/opt/book/docker/legacy/ocaml.json').read_text()),
                             'scope':'Historical Marshal to NPY in an isolated build stage; only current compiler in final image.'}
if '14' in chapters:
    import sys
    sys.path.insert(0,'/opt/book/scripts')
    from prepare_mrcnn import check,DEFAULT_DIRECTORY
    data.setdefault('datasets',{})['mrcnn']=check(DEFAULT_DIRECTORY)
    patch=Path('/opt/book/docker/patches/camlimages-fpic.patch')
    metadata_path=patch.with_suffix('.json')
    metadata=json.loads(metadata_path.read_text())
    actual={name:hashlib.sha256((Path('/opt/book-sources/camlimages')/name).read_bytes()).hexdigest()
            for name in metadata['patched_files']}
    if actual!=metadata['patched_files']:
        raise ValueError('CamlImages source does not contain the verified Dune C flags patch')
    data['dependency_patches']['camlimages']={'sha256':hashlib.sha256(patch.read_bytes()).hexdigest(),
                                             'patched_files':actual,'version':metadata['version']}
    for p in (patch,metadata_path):
        data['source_hashes'][str(p.relative_to('/opt/book'))]=hashlib.sha256(p.read_bytes()).hexdigest()
if '15' in chapters:
    import sys
    sys.path.insert(0,'/opt/book/scripts')
    from prepare_style import check,DEFAULT_DIRECTORY
    data.setdefault('datasets',{})['style']=check(DEFAULT_DIRECTORY)
import sys
sys.path.insert(0, '/opt/book/scripts')
from book_examples import discover, source_inputs
root=Path('/opt/book')
data['example_inputs']={identity:source_inputs(root,source) for identity,source in discover(root).items()}
for inputs in data['example_inputs'].values():
    data['source_hashes'].update(inputs)
from verification import pipeline_inputs
data['pipeline_inputs']=pipeline_inputs(root)
data['verification_schema']=1
data['system_packages']=run('dpkg-query','-W','-f=${Package}=${Version}\n')
Path('/opt/book/environment.json').write_text(json.dumps(data, indent=2) + '\n')
