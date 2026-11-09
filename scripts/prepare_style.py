#!/usr/bin/env python3
"""Verify seven fixed portable style models; old-weight migration is build-only."""
import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
import urllib.request
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
MANIFEST=ROOT/'code/chap15_case-nst/portable-manifest.json'
DEFAULT_DIRECTORY=Path.home()/'.owl/dataset/style'
def digest(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as stream:
        for data in iter(lambda:stream.read(1024*1024),b''):h.update(data)
    return h.hexdigest()
def check(directory):
    directory=Path(directory);manifest=json.loads(MANIFEST.read_text())
    models={}
    if not directory.is_dir() or {p.name for p in directory.iterdir()}!={m['name'] for m in manifest['models']}:
        raise ValueError('Missing or extra portable style model')
    for model in manifest['models']:
        folder=directory/model['name']
        if not folder.is_dir() or {p.name for p in folder.iterdir()}!={f['path'] for f in model['files']}:
            raise ValueError('Missing or extra portable style tensor')
        hashes={}
        for item in model['files']:
            path=folder/item['path']
            if not path.is_file() or path.stat().st_size!=item['bytes'] or digest(path)!=item['sha256']:
                raise ValueError('Changed style tensor: '+str(path))
            hashes[item['path']]=item['sha256']
        models[model['name']]={k:model[k] for k in ('nodes','tensors','parameters')}
        models[model['name']].update(source_weight_sha256=model['source']['sha256'],files=hashes)
    return {'manifest_sha256':digest(MANIFEST),'models':models}
def fetch(item,target):
    h=hashlib.sha256();git=hashlib.sha1(b'blob '+str(item['bytes']).encode()+b'\0');size=0
    with urllib.request.urlopen(item['url'],timeout=120) as response,target.open('wb') as stream:
        while data:=response.read(1024*1024):
            size+=len(data)
            if size>item['bytes']:raise ValueError('Oversized original style weight')
            h.update(data);git.update(data);stream.write(data)
    if size!=item['bytes'] or h.hexdigest()!=item['sha256'] or git.hexdigest()!=item['git_blob_sha1']:
        raise ValueError('Changed original style weight: '+item['name'])
def migrate(directory,legacy_source):
    directory=Path(directory);legacy_source=Path(legacy_source)
    if directory.exists():raise ValueError('Refusing to overwrite an existing style cache')
    with tempfile.TemporaryDirectory(prefix='style-migration-') as temporary:
        work=Path(temporary);portable=work/'portable';portable.mkdir()
        reader=legacy_source/'runtime/ocamlrun';compiler=legacy_source/'ocamlc'
        subprocess.run([str(reader),str(compiler),'-nostdlib','-I',str(legacy_source/'stdlib'),'-o',str(work/'export.byte'),str(ROOT/'docker/legacy/export_weights.ml')],cwd=legacy_source,check=True)
        for model in json.loads(MANIFEST.read_text())['models']:
            target=work/model['source']['name'];fetch(model['source'],target)
            output=portable/model['name'];output.mkdir()
            subprocess.run([str(reader),str(work/'export.byte'),str(target),str(output)],check=True)
            target.unlink()
        evidence=check(portable)
        directory.parent.mkdir(parents=True,exist_ok=True);shutil.copytree(portable,directory)
    return evidence
def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--directory',type=Path,default=DEFAULT_DIRECTORY)
    p.add_argument('--legacy-source',type=Path)
    a=p.parse_args()
    evidence=migrate(a.directory,a.legacy_source) if a.legacy_source else check(a.directory)
    print('PASS: seven original style models and every portable tensor hash')
if __name__=='__main__':main()
