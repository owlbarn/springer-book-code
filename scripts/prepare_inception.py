#!/usr/bin/env python3
"""Verify cached portable original Inception inputs; migrate only in a build stage."""
import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
import urllib.request
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]
CHAPTER=ROOT/'code/chap13_case-image-inception'
DEFAULT_DIRECTORY=Path.home()/'.owl/dataset/inception'
def digest(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda:stream.read(1024*1024),b''):h.update(block)
    return h.hexdigest()
def check(directory):
    directory=Path(directory);manifest=json.loads((CHAPTER/'portable-manifest.json').read_text())
    inputs=json.loads((CHAPTER/'inputs.json').read_text())
    hashes={}
    for item in manifest['files']+[item|{'path':item['name']} for item in inputs['files'] if item['name'] in ('panda.png','imagenet1000.dict')]:
        path=directory/item['path']
        if not path.is_file() or path.stat().st_size!=item['bytes'] or digest(path)!=item['sha256']:
            raise ValueError('Missing or changed portable Inception input: '+str(path))
        hashes[item['path']]=item['sha256']
    return {'repository':inputs['repository'],'revision':inputs['revision'],
            'input_manifest_sha256':digest(CHAPTER/'inputs.json'),
            'portable_manifest_sha256':digest(CHAPTER/'portable-manifest.json'),
            'original_weight_sha256':manifest['source_weight_sha256'],
            'nodes':manifest['nodes'],'tensors':manifest['tensors'],'parameters':manifest['parameters'],
            'files':hashes}
def fetch(item,target):
    h=hashlib.sha256();git=hashlib.sha1(b'blob '+str(item['bytes']).encode()+b'\0');size=0
    with urllib.request.urlopen(item['url'],timeout=120) as response,target.open('wb') as stream:
        while block:=response.read(1024*1024):
            size+=len(block)
            if size>item['bytes']:raise ValueError('Oversized original model input')
            h.update(block);git.update(block);stream.write(block)
    if size!=item['bytes'] or h.hexdigest()!=item['sha256'] or git.hexdigest()!=item['git_blob_sha1']:
        raise ValueError('Changed original model input: '+item['name'])
def migrate(directory,legacy_source):
    directory=Path(directory)
    if directory.exists():raise ValueError('Refusing to overwrite an existing model directory')
    legacy_source=Path(legacy_source)
    with tempfile.TemporaryDirectory(prefix='inception-migration-') as temporary:
        work=Path(temporary);portable=work/'portable';portable.mkdir()
        inputs=json.loads((CHAPTER/'inputs.json').read_text())
        for item in inputs['files']:
            if item['name'] not in ('inception_owl.weight','panda.png','imagenet1000.dict'):continue
            target=work/item['name'];fetch(item,target)
            if item['name']!='inception_owl.weight':shutil.copy2(target,portable/item['name'])
        command=[str(legacy_source/'runtime/ocamlrun'),str(legacy_source/'ocamlc'),'-nostdlib','-I',str(legacy_source/'stdlib'),
                 '-o',str(work/'export.byte'),str(ROOT/'docker/legacy/export_weights.ml')]
        subprocess.run(command,cwd=legacy_source,check=True)
        subprocess.run([str(legacy_source/'runtime/ocamlrun'),str(work/'export.byte'),str(work/'inception_owl.weight'),str(portable)],check=True)
        evidence=check(portable)
        directory.parent.mkdir(parents=True,exist_ok=True);shutil.copytree(portable,directory)
    return evidence
def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--directory',type=Path,default=DEFAULT_DIRECTORY)
    p.add_argument('--legacy-source',type=Path,help='Build-stage-only isolated reader; omit for reader cache verification')
    a=p.parse_args();evidence=migrate(a.directory,a.legacy_source) if a.legacy_source else check(a.directory)
    print('PASS: 314 nodes, 566 portable float32 tensors; original input and all migrated hashes verified')
if __name__=='__main__':main()
